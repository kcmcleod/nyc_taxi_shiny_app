start_time <- Sys.time()

offline_data_prep <- TRUE

source(paste0(getwd(), "/global.R"))
library(arrow)
library(data.table)
library(logger)
library(prettyunits)

sourceRFilesFromFolder(paste0(getwd(), "/dataPrep/yellow_taxi_helpers/"))

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

  # Note: Use q() or stop() here depending on your pipeline runner, return(NULL) is fine if this is a function.
  stop("Pipeline up to date", call. = FALSE)
}

log_info("Found ", length(new_files), " files to process...")


################################################################################
# LOOP THRU DATA FILES
combinedDF <- NULL
expected_cols <- NULL

new_files <- new_files[1:15]

for (file in new_files) {
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

    if (is.null(combinedDF)) {
      combinedDF <- tmp
      expected_cols <- names(tmp)
    } else {
      # should ignore any extra cols
      combinedDF <- as_tibble(rbindlist(list(combinedDF, tmp), fill = TRUE))
    }
    remove(tmp)
    log_info("size of combined data is now: ", nrow(combinedDF))
  }
}

if (is.null(combinedDF) || nrow(combinedDF) == 0) {
  log_error("Failed to extract any valid data from the new files. STOPPING!")
  stop("No valid data processed", call. = FALSE)
}

################################################################################
# SORT
sort_cols <- c("full_month_aggregation", "full_week_aggregation", "date")
combinedDF <- arrange(combinedDF, across(all_of(sort_cols)))

################################################################################
# LOOKUPS
# todo need to check files exists

log_info("Starting to join lookups...")

zone_lookups <- read_csv(paste0(dataPath, "src/taxi_zone_lookup.csv")) |>
  select("LocationID", "Borough")
rate_lookups <- read_csv(paste0(dataPath, "src/rate_code_lookup.csv"), col_types = c("ic"))
payment_lookups <- read_csv(paste0(dataPath, "src/payment_type_lookup.csv"), col_types = c("ic"))
vendor_lookups <- read_csv(paste0(dataPath, "src/vendor_lookup.csv"), col_types = c("ic"))

combinedDF <- fn_perform_joins(
  combinedDF, zone_lookups, rate_lookups,
  payment_lookups, vendor_lookups
)


################################################################################
# WRITE NEW ASSET

log_info("Writing new partitioned data asset...")

# Create partition columns from the og file name
combinedDF <- combinedDF |>
  group_by(Year, Month)

# Define the folder path instead of a file path
partition_dir <- paste0(dataPath, "yellow_aggregation_partitioned")

# Write the partitioned dataset
write_dataset(
  dataset = combinedDF,
  path = partition_dir,
  format = "parquet",
  # Dec data is split between Dec and Jan file so to handle this we create 2 files
  basename_template = paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"), "-{i}.parquet")
)

log_info("Partitioned data successfully written to: ", partition_dir)

gc()


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

saveRDS(yellow_taxi_meta_data, file = paste0(
  paste0(getwd(), "/app_data/"),
  format(Sys.Date(), "%Y%m%d"), "_yellow_metadata.rds"
))


################################################################################
# BYE TIME!
remove(combinedDF)
gc()

total_time <- Sys.time() - start_time
pretty_duration <- pretty_ms(as.numeric(total_time, units = "secs") * 1000)
log_info("Finished! This took: ", pretty_duration)
message("Data written to: ", partition_dir)
message("Log: ", str_replace_all(string = log_name, pattern = " ", replacement = "\\\\ "))
message("GOOD BYE!")
