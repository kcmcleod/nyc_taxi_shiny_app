################################################################################
### FILE: data-raw/create_golden_baseline.R
################################################################################
# Purpose: Creates a frozen 500-row representative raw sample from existing
#          NYC Taxi Parquet data and generates its known-good enriched baseline
#          (aggregations + lookup joins) for automated diffdf regression testing.
################################################################################

library(arrow)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)

# 1. Load project environment & helper functions
source("global.R")
sourceRFilesFromFolder("dataPrep/yellow_taxi_helpers/")

# Ensure the testdata directory exists
testdata_dir <- file.path("tests", "testdata")
if (!dir.exists(testdata_dir)) {
  dir.create(testdata_dir, recursive = TRUE, showWarnings = FALSE)
}

# 2. Locate an existing processed source parquet file
all_src_files <- list.files(
  path = file.path(dataPath, "src"),
  pattern = "yellow.*\\.parquet$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(all_src_files) == 0) {
  stop("No raw yellow taxi parquet files found in dataPath/src/. Cannot generate sample.")
}

source_file <- all_src_files[1]
message("Building golden baseline from: ", source_file)

# Extract the YYYY-MM string so we can generate correct date boundaries
file_month_str <- stringr::str_match(source_file, pattern = "\\d{4}-\\d{2}")[1]

# 3. Read the raw data into memory
raw_df <- arrow::read_parquet(source_file)

# 4. Create a Stratified 500-Row Sample
set.seed(42) # Lock random seed so the sample is 100% reproducible

stratified_sample <- raw_df |>
  dplyr::filter(
    !is.na(tpep_pickup_datetime),
    !is.na(tpep_dropoff_datetime),
    !is.na(total_amount)
  ) |>
  dplyr::group_by(VendorID, payment_type) |>
  dplyr::slice_sample(n = 50, replace = FALSE) |>
  dplyr::ungroup() |>
  dplyr::slice_head(n = 480)

edge_case_sample <- raw_df |>
  dplyr::filter(
    lubridate::as_date(tpep_pickup_datetime) ==
      lubridate::ymd(paste0(file_month_str, "-01")) %m+% months(1) - lubridate::days(1)
  ) |>
  dplyr::slice_head(n = 20)

golden_raw_input <- dplyr::bind_rows(stratified_sample, edge_case_sample) |>
  dplyr::distinct() |>
  dplyr::slice_head(n = 500)

message("Generated raw sample with ", nrow(golden_raw_input), " rows.")

# 5. Save the Frozen Golden Input Parquet File (with date string in filename)
golden_input_path <- file.path(testdata_dir, paste0(file_month_str, "_golden_input_yellow.parquet"))
arrow::write_parquet(golden_raw_input, golden_input_path)
message("Saved frozen input: ", golden_input_path)

# 6. Step A: Process input through core temporal aggregations
message("Processing frozen input through fn_process_yellow_file()...")
combinedDF <- fn_process_yellow_file(golden_input_path)

if (is.null(combinedDF) || nrow(combinedDF) == 0) {
  stop("Pipeline processing failed! Output is empty or NULL.")
}

sort_cols <- c("full_month_aggregation", "full_week_aggregation", "date")
combinedDF <- dplyr::arrange(combinedDF, dplyr::across(dplyr::all_of(sort_cols)))

# 7. Step B: Perform lookup enrichment joins
message("Joining lookups to generate fully enriched baseline...")
zone_lookups <- readr::read_csv(file.path(dataPath, "src/taxi_zone_lookup.csv"),
  show_col_types = FALSE
) |>
  dplyr::select(LocationID, Borough)
rate_lookups <- readr::read_csv(file.path(dataPath, "src/rate_code_lookup.csv"),
  col_types = "ic", show_col_types = FALSE
)
payment_lookups <- readr::read_csv(file.path(dataPath, "src/payment_type_lookup.csv"),
  col_types = "ic", show_col_types = FALSE
)
vendor_lookups <- readr::read_csv(file.path(dataPath, "src/vendor_lookup.csv"),
  col_types = "ic", show_col_types = FALSE
)

golden_processed_output <- fn_perform_lookup(
  combinedDF, rate_lookups, "RatecodeID",
  "RatecodeID", "Rate", "rate"
) |>
  fn_perform_lookup(zone_lookups, c("PULocationID" = "LocationID"), "PULocationID",
    "PULocation", "PU",
    post_process_fn = function(df) dplyr::rename(df, PULocation = Borough)
  ) |>
  fn_perform_lookup(zone_lookups, c("DOLocationID" = "LocationID"), "DOLocationID",
    "DOLocation", "DO",
    post_process_fn = function(df) dplyr::rename(df, DOLocation = Borough)
  ) |>
  fn_perform_lookup(payment_lookups, "payment_type", "payment_type", "payment_type",
    "payment",
    post_process_fn = function(df) dplyr::mutate(df, payment_type = payment_class)
  ) |>
  dplyr::select(-c(payment_class)) |>
  fn_perform_lookup(vendor_lookups, "VendorID", "VendorID", "Vendor", "vendor")

# Verify Joins Passed
all_names <- names(golden_processed_output)
expected_names <- c("PULocation", "DOLocation", "Rate", "Vendor")
join_results <- sapply(expected_names, function(field) {
  fn_test_lookup_join(all_names, field)
})

if (any(!join_results)) {
  stop("Golden baseline generation STOPPED due to failed lookup joins!")
}

# 8. Save the Frozen Fully Enriched Output RDS File
golden_output_path <- file.path(testdata_dir, "golden_output_yellow.rds")
saveRDS(golden_processed_output, golden_output_path)
message("Saved fully enriched baseline output: ", golden_output_path)
message("End-to-end golden baseline generation complete!")
