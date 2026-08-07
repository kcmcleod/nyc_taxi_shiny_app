library(testthat)
library(dplyr)

# Prevent logger from crashing the covr sandbox
if (Sys.getenv("R_COVR") == "true") {
  environment(get_initial_taxi_data)$log_info <- function(...) invisible()
  environment(get_initial_taxi_data)$log_error <- function(...) invisible()
  environment(get_initial_taxi_data_aws)$log_info <- function(...) invisible()
  environment(get_initial_taxi_data_aws)$log_error <- function(...) invisible()
  environment(get_initial_taxi_metadata)$log_info <- function(...) invisible()
  environment(get_initial_taxi_metadata)$log_error <- function(...) invisible()
}

logger::log_threshold(logger::FATAL)

# --- SETUP: Ensure test data directory and files exist for testmode ---
# Because testthat runs inside tests/testthat/, we create the directory structure here if missing
if (!dir.exists("tests/testdata")) {
  dir.create("tests/testdata", recursive = TRUE, showWarnings = FALSE)
}

if (!file.exists("tests/testdata/mock_taxi_trips.rds")) {
  saveRDS(data.frame(trip_id = 1:3), "tests/testdata/mock_taxi_trips.rds")
}

if (!file.exists("tests/testdata/yellow_metadata.rds")) {
  saveRDS(list(zones = c("A", "B")), "tests/testdata/yellow_metadata.rds")
}


# ==============================================================================
# TESTS FOR get_initial_taxi_data
# ==============================================================================

test_that("get_initial_taxi_data loads mock data when shiny.testmode is TRUE", {
  withr::local_options(shiny.testmode = TRUE)
  
  result <- get_initial_taxi_data("dummy_path")
  expect_false(is.null(result))
})

test_that("get_initial_taxi_data successfully loads a parquet file from disk when testmode is FALSE", {
  withr::local_options(shiny.testmode = FALSE)
  
  temp_dir <- withr::local_tempdir()
  dummy_file <- file.path(temp_dir, "yellow_tripdata_2023-01.parquet")
  
  df <- data.frame(col1 = 1:5)
  arrow::write_parquet(df, dummy_file)
  
  result <- get_initial_taxi_data(temp_dir)
  expect_true(inherits(result, "Dataset") || inherits(result, "ArrowObject") || inherits(result, "data.frame"))
})

test_that("get_initial_taxi_data returns NULL when no file is found in directory", {
  withr::local_options(shiny.testmode = FALSE)
  empty_dir <- withr::local_tempdir()
  
  result <- get_initial_taxi_data(empty_dir)
  expect_null(result)
})


# ==============================================================================
# TESTS FOR get_initial_taxi_data_aws
# ==============================================================================

test_that("get_initial_taxi_data_aws loads mock data when shiny.testmode is TRUE", {
  withr::local_options(shiny.testmode = TRUE)
  
  result <- get_initial_taxi_data_aws()
  expect_false(is.null(result))
})

test_that("get_initial_taxi_data_aws stops if AWS environment variables are missing", {
  withr::local_options(shiny.testmode = FALSE)
  withr::local_envvar(TAXI_S3_BUCKET = "", TAXI_S3_OBJECT = "")
  
  expect_error(
    get_initial_taxi_data_aws(),
    regexp = "Missing required AWS configuration"
  )
})

test_that("get_initial_taxi_data_aws catches S3 connection errors gracefully", {
  withr::local_options(shiny.testmode = FALSE)
  withr::local_envvar(TAXI_S3_BUCKET = "fake-bucket-12345", TAXI_S3_OBJECT = "fake-object.parquet")
  
  result <- get_initial_taxi_data_aws()
  expect_null(result)
})


# ==============================================================================
# TESTS FOR get_initial_taxi_metadata
# ==============================================================================

test_that("get_initial_taxi_metadata loads mock metadata when shiny.testmode is TRUE", {
  withr::local_options(shiny.testmode = TRUE)
  
  result <- get_initial_taxi_metadata("dummy_path")
  expect_false(is.null(result))
})

test_that("get_initial_taxi_metadata successfully loads metadata rds file from disk", {
  withr::local_options(shiny.testmode = FALSE)
  
  temp_dir <- withr::local_tempdir()
  dummy_metadata_file <- file.path(temp_dir, "yellow_metadata_2023.rds")
  
  dummy_meta <- list(version = "1.0", zones = c("A", "B"))
  saveRDS(dummy_meta, dummy_metadata_file)
  
  result <- get_initial_taxi_metadata(temp_dir)
  expect_equal(result$version, "1.0")
})

test_that("get_initial_taxi_metadata returns NULL when no metadata file is found", {
  withr::local_options(shiny.testmode = FALSE)
  empty_dir <- withr::local_tempdir()
  
  result <- get_initial_taxi_metadata(empty_dir)
  expect_null(result)
})