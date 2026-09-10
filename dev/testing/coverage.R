app_sources <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
dp_sources <- list.files("data-raw/yellow_taxi_helpers", pattern = "\\.[Rr]$", full.names = TRUE)
app_sources <- c(app_sources, dp_sources)
app_tests <- list.files("tests/testthat", pattern = "\\.[Rr]$", full.names = TRUE)

# filter out shinytest2 etc
unit_tests <- grep("shinytest2|setup", app_tests, value = TRUE, invert = TRUE)
core_sources <- grep("_disable", app_sources, value = TRUE, invert = TRUE)

source("tests/testthat/setup.R")

cov_results <- covr::file_coverage(
  source_files = core_sources,
  test_files = unit_tests
)

covr::report(cov_results)
