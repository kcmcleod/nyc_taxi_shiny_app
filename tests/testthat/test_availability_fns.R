library(testthat)
library(dplyr)
library(plotly)
library(lubridate)

if (Sys.getenv("R_COVR") == "true") {
  environment(fn_calculate_availability_data)$log_info <- function(...) invisible()
  environment(fn_calculate_availability_data)$log_error <- function(...) invisible()
  environment(fn_generate_availability_chart)$log_info <- function(...) invisible()
  environment(fn_generate_availability_chart)$log_error <- function(...) invisible()
}


# ==============================================================================
# MOCK DATA & CONFIGURATION
# ==============================================================================

mock_config <- list(
  colours = list(
    theme = list(primary_accent = "#00539F")
  ),
  ui_values = list(
    pi_level = c("Day", "Week", "Month")
  )
)

mock_daily_vols <- tibble::tibble(
  date = as.Date(c(
    "2023-01-02", "2023-01-05", "2023-01-10",
    "2023-02-05", "2023-02-15"
  )),
  total_trips = c(10, 20, 30, 40, 50)
)

# ==============================================================================
# DATA CALCULATION TESTS
# ==============================================================================

test_that("fn_calculate_availability_data executes all defensive blocks", {
  # 1. Bad base data
  expect_error(
    fn_calculate_availability_data(list(), "Month", mock_config, Sys.Date(), Sys.Date()),
    regexp = "System Error"
  )

  # 2. Missing cols
  expect_error(
    fn_calculate_availability_data(data.frame(a = 1), "Month", mock_config, Sys.Date(), Sys.Date()),
    regexp = "Data Error"
  )

  # 3. Invalid granularity
  expect_error(
    fn_calculate_availability_data(mock_daily_vols, "Banana", mock_config, Sys.Date(), Sys.Date()),
    regexp = "Invalid granularity"
  )
  expect_error(
    fn_calculate_availability_data(mock_daily_vols, "Year", mock_config, Sys.Date(), Sys.Date()),
    regexp = "Invalid granularity"
  )

  # 4. Invalid start date
  expect_error(
    fn_calculate_availability_data(mock_daily_vols, "Month", mock_config, "2023-01-01", Sys.Date()),
    regexp = "Invalid start date"
  )
  expect_error(
    fn_calculate_availability_data(mock_daily_vols, "Month", mock_config, as.Date(NA), Sys.Date()),
    regexp = "Invalid start date"
  )

  # 5. Invalid end date
  expect_error(
    fn_calculate_availability_data(mock_daily_vols, "Month", mock_config, Sys.Date(), "2023-01-01"),
    regexp = "Invalid end date"
  )
})

test_that("fn_calculate_availability_data aggregates and filters correctly", {
  # 1. Weekly aggregation & filtering
  res_week <- fn_calculate_availability_data(
    mock_daily_vols, "Week", mock_config,
    as.Date("2023-01-01"), as.Date("2023-01-31")
  )
  expect_s3_class(res_week, "data.frame")
  expect_equal(nrow(res_week), 2) # Two distinct weeks in Jan
  expect_equal(sum(res_week$total_trips), 60) # 10 + 20 + 30

  # 2. Monthly aggregation (Full Range)
  res_month <- fn_calculate_availability_data(
    mock_daily_vols, "Month", mock_config,
    as.Date("2023-01-01"), as.Date("2023-12-31")
  )
  expect_equal(nrow(res_month), 2) # Jan, Feb
  expect_equal(sum(res_month$total_trips), 150) # Total overall trips
})

# ==============================================================================
# CHART GENERATION TESTS
# ==============================================================================

test_that("fn_generate_availability_chart executes all defensive blocks", {
  # 1. Bad chart data
  expect_error(
    fn_generate_availability_chart(list(), "Month", mock_config),
    regexp = "System Error"
  )

  # 2. Invalid granularity
  expect_error(
    fn_generate_availability_chart(mock_daily_vols, "Invalid", mock_config),
    regexp = "Invalid granularity"
  )
})

test_that("fn_generate_availability_chart creates valid plotly objects with correct axes", {
  # 1. Monthly Chart
  p_month <- fn_generate_availability_chart(mock_daily_vols, "Month", mock_config)
  expect_s3_class(p_month, "plotly")

  # 2. Weekly Chart
  p_week <- fn_generate_availability_chart(mock_daily_vols, "Week", mock_config)
  expect_s3_class(p_week, "plotly")

  # 3. Simulate forcing the "Day" path to trigger 100% coverage
  # Even though your UI filters out "Day", testing it prevents dead-code flags in CI/CD.
  mock_config_day_override <- mock_config
  mock_config_day_override$ui_values$pi_level <- c("Day", "Week", "Month")

  p_day <- suppressWarnings(
    fn_generate_availability_chart(mock_daily_vols, "Day", mock_config_day_override)
  )
  expect_s3_class(p_day, "plotly")
})
