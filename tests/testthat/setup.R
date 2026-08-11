offline_data_prep <- TRUE

# 2. Source global.R to initialise paths (dataPath, codePath, config) and libraries
global_path <- testthat::test_path("../../global.R")
if (file.exists(global_path)) {
  source(global_path, local = FALSE)
}

# stop covr creating issues with logger
if (Sys.getenv("R_COVR") == "true") {
  log_info <- function(...) invisible()
  log_warn <- function(...) invisible()
  log_error <- function(...) invisible()
  log_debug <- function(...) invisible()
  log_fatal <- function(...) invisible()
}

# Automatically source all dataPrep helper functions before running tests
helper_dir <- testthat::test_path("../../dataPrep/yellow_taxi_helpers")

if (dir.exists(helper_dir)) {
  helper_files <- list.files(helper_dir, pattern = "\\.R$", full.names = TRUE)
  for (file in helper_files) {
    source(file, local = FALSE)
  }
}
