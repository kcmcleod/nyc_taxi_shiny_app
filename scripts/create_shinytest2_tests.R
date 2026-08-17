library(shinytest2)

offline_data_prep <- TRUE

################################################################################
# SETUP

# Sets up the necessary infrastructure and 'tests/testthat/' folder
use_shinytest2()


################################################################################
# CREATE TESTS

# create tests
record_test()

# if snaps get really large, eg MBs, switch from app$expect_values() to app$get_value()
# plotly_data <- app$get_value(output = "po_tripVolumesOverTime")
# expect_true(length(plotly_data$x$data) > 0)


################################################################################
# RUN TESTS
test_app()


################################################################################
# UPDATE SNAPS
testthat::snapshot_review()


################################################################################
# COVERAGE

app_sources <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
dp_sources <- list.files("dataPrep/yellow_taxi_helpers", pattern = "\\.[Rr]$", full.names = TRUE)
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
covr::zero_coverage(cov_results)
