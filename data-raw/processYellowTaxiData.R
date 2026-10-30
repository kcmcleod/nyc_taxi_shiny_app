start_time <- Sys.time()

offline_data_prep <- TRUE

source(paste0(getwd(), "/global.R"))
library(arrow)
library(data.table)
library(logger)
library(prettyunits)
library(tidyr)
library(readr)
library(tibble)

sourceRFilesFromFolder(paste0(getwd(), "/data-raw/yellow_taxi_helpers/"))

################################################################################
# SETUP LOGGING
log_name <- paste0(dataPath, "logs/", format(Sys.time(), "%Y%m%d_%H%M"), "_yellow.log")
log_appender(appender_tee(log_name))
log_threshold(INFO)
log_info("STARTING TO PROCESS YELLOW DATA")


################################################################################
# GETTING EXISTING PROCESSED DATA

log_info("Checking existing partitioned master directory for already processed months...")

partition_dir <- paste0(dataPath, "yellow_aggregation_partitioned")
processed_months <- character(0)

if (dir.exists(partition_dir)) {
  log_info(".. found existing partitioned master directory")

  # open_dataset maps the folder, collect() pulls it into memory for the join
  processed_months_df <- arrow::open_dataset(partition_dir) |>
    dplyr::select(Year, Month) |>
    dplyr::distinct() |>
    dplyr::collect()

  processed_months <- sprintf("%04d-%02d", processed_months_df$Year, processed_months_df$Month)
  processed_months <- sort(processed_months)
  log_info(".. already processed months: ", paste0(processed_months, collapse = ", "))
}


################################################################################
# CHECKING FOR FILES TO PROCESS
all_yellow_files <- list.files(
  path = paste0(dataPath, "src"), pattern = "yellow.*.parquet",
  recursive = TRUE, full.names = TRUE
)

new_files <- c()
for (file in all_yellow_files) {
  file_month <- str_match(file, pattern = "\\d{4}-\\d{2}")[1]
  if (is.na(file_month)) {
    log_error("Cannot process date from: ", file)
  } else if (!file_month %in% processed_months) {
    new_files <- c(new_files, file)
  }
}

if (length(new_files) == 0) {
  log_info("Cannot find any new files to process. Up to date. STOPPING!")

  total_time <- Sys.time() - start_time
  log_info("Finished! This took: ", pretty_ms(as.numeric(total_time, units = "secs") * 1000))
  message("GOOD BYE!")

  stop("Pipeline up to date", call. = FALSE)
}

log_info("Found ", length(new_files), " files to process...")


################################################################################
# CHECKING LOOKUPS EXIST

zone_lkup_file <- paste0(dataPath, "src/taxi_zone_lookup.csv")
rate_lkup_file <- paste0(dataPath, "src/rate_code_lookup.csv")
payment_lkup_file <- paste0(dataPath, "src/payment_type_lookup.csv")
vendor_lkup_file <- paste0(dataPath, "src/vendor_lookup.csv")

all_lkups <- c(zone_lkup_file, rate_lkup_file, payment_lkup_file, vendor_lkup_file)
missing_files <- all_lkups[!file.exists(all_lkups)]

if (length(missing_files) > 0) {
  log_error(paste0("Lookup files are missing: ", paste0(missing_files, collapse = ", ")))
  stop("Missing lookup files!")
}


################################################################################
# LOOP THRU DATA FILES
combinedDF <- NULL
expected_cols <- NULL

# groups files into batches
batch_size <- config$data_processing$max_num_of_parquet_to_process
file_batches <- split(new_files, ceiling(seq_along(new_files) / batch_size))

log_info(paste0(
  "Processing ", length(new_files), " files across ",
  length(file_batches), " in batches of up to ", batch_size, "files."
))

# set up & load lookups
sort_cols <- c("full_month_aggregation", "full_week_aggregation", "date")

partition_dir <- paste0(dataPath, "yellow_aggregation_partitioned")

zone_lookups <- read_csv(zone_lkup_file) |>
  select("LocationID", "Borough")
rate_lookups <- read_csv(rate_lkup_file, col_types = c("ic"))
payment_lookups <- read_csv(payment_lkup_file, col_types = c("ic"))
vendor_lookups <- read_csv(vendor_lkup_file, col_types = c("ic"))


# running batches
for (b_idx in seq_along(file_batches)) {
  log_info(paste0("--- Starting Batch ", b_idx, " of ", length(file_batches), " ---"))
  batch_files <- file_batches[[b_idx]]

  res_list <- vector("list", length(batch_files))
  row_count <- 0L # accumulating number of rows processed

  for (i in seq_along(batch_files)) {
    file <- batch_files[[i]]
    log_info("Processing file: ", file)
    tmp <- fn_process_yellow_file(file)

    if (!is.null(tmp) && is_tibble(tmp)) {
      # use for partitioning later
      file_month_str <- str_match(file, pattern = "\\d{4}-\\d{2}")[1]

      tmp <- tmp |>
        mutate(
          Year = as.integer(substr(file_month_str, 1, 4)),
          Month = as.integer(substr(file_month_str, 6, 7))
        )

      res_list[[i]] <- tmp
      row_count <- row_count + nrow(tmp)

      remove(tmp)
      log_info("size of combined data is now: ", row_count)
    }
  }

  # make combined DF Ignoring extra cols
  combinedDF <- as_tibble(rbindlist(res_list, fill = TRUE)) |>
    filter(!is.na(date))

  remove(res_list)

  if (is.null(combinedDF) || nrow(combinedDF) == 0) {
    log_warn(paste0("Batch ", b_idx, " produced 0 valid rows. Skipping write."))
    next
  }

  # SORT
  combinedDF <- arrange(combinedDF, across(all_of(sort_cols)))

  # LOOKUPS
  combinedDF <- fn_perform_joins(
    combinedDF, zone_lookups, rate_lookups,
    payment_lookups, vendor_lookups
  )

  # PARTITION FOR WRITING PARQUET
  combinedDF <- combinedDF |>
    group_by(Year, Month)

  log_info(paste0("Writing Batch ", b_idx, " to partitioned dataset..."))
  write_dataset(
    dataset = combinedDF,
    path = partition_dir,
    format = "parquet",
    # some of the Dec data is delayed till the Jan file from next year
    # ensure the data from the Dec file isnt overwritten when we process Jan data
    basename_template = paste0(
      "run_", format(Sys.time(), "%Y%m%d_%H%M%S"),
      "_b", b_idx, "-{i}.parquet"
    )
  )

  remove(combinedDF)
  gc()
  log_info(paste0("--- Finished Batch ", b_idx, " --- "))
}


################################################################################
# WRITE NEW ASSET FOR METADATA

log_info("Generating global metadata...")

Vendors <- sort(unique(vendor_lookups$Vendor))
payment_types <- sort(unique(payment_lookups$payment_class))
locations <- sort(unique(zone_lookups$Borough))

date_ranges <- arrow::open_dataset(partition_dir) |>
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE)
  ) |>
  collect()

daily_volumes <- arrow::open_dataset(partition_dir) |>
  dplyr::filter(full_month_aggregation == FALSE, full_week_aggregation == FALSE) |>
  dplyr::group_by(date) |>
  dplyr::summarise(total_trips = sum(total_number_trips, na.rm = TRUE)) |>
  dplyr::collect() |>
  dplyr::arrange(date)

yellow_taxi_meta_data <- list(
  "Vendor" = Vendors,
  "payment_type" = payment_types,
  "min_date" = date_ranges$min_date[1],
  "max_date" = date_ranges$max_date[1],
  "Location" = locations,
  "daily_volumes" = daily_volumes
)

# save metadate object used by shiny for faster loading
app_data_dir <- file.path(codePath, "app_data")
if (!dir.exists(app_data_dir)) dir.create(app_data_dir, recursive = TRUE)

old_meta <- list.files(app_data_dir, pattern = "_yellow_metadata\\.rds$", full.names = TRUE)
if (length(old_meta) > 0) file.remove(old_meta)

saveRDS(
  yellow_taxi_meta_data,
  file = file.path(app_data_dir, paste0(format(Sys.Date(), "%Y%m%d"), "_yellow_metadata.rds"))
)


################################################################################
# BYE TIME!
remove(yellow_taxi_meta_data)
gc()

total_time <- Sys.time() - start_time
pretty_duration <- pretty_ms(as.numeric(total_time, units = "secs") * 1000)
log_info("Finished! This took: ", pretty_duration)
message("Data written to: ", partition_dir)
message("Log: ", str_replace_all(string = log_name, pattern = " ", replacement = "\\\\ "))
message("GOOD BYE!")
