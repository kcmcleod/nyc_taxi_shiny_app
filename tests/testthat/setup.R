offline_data_prep <- TRUE

codePath <- getwd()
while (!dir.exists(file.path(codePath, "config")) && codePath != dirname(codePath)) {
  codePath <- dirname(codePath)
}

# 2. Source global.R to attach {tidyverse}, {arrow}, etc.
global_path <- file.path(codePath, "global.R")
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
