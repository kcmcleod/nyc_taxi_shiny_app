################################################################################
### FILE: tests/testthat/test-table.R
################################################################################

library(testthat)
library(DT)

# ==============================================================================
# MOCK DATA GENERATOR
# ==============================================================================

mock_raw_table_data <- tibble::tibble(
  date = as.Date(c("2023-01-01", "2023-01-01", "2023-02-01", "2023-03-01")),
  # The 4th row is deliberately set to fail the filter to test the exclusion logic
  full_month_aggregation = c(TRUE, TRUE, TRUE, FALSE),
  full_week_aggregation = c(FALSE, FALSE, FALSE, TRUE),
  total_number_trips = c(10, 20, 30, 40),
  total_distance = c(5.5, 4.5, 10.0, 20.0),
  total_passenger_count = c(1, 2, 3, 4),
  total_charges = c(15.0, 25.0, 50.0, 100.0),
  total_fare = c(10.0, 20.0, 40.0, 80.0),
  PULocation = c("EWR", "EWR", "EWR", "EWR"),
  DOLocation = c("EWR", "EWR", "EWR", "EWR")
)


# A helper to quickly generate valid mock taxi data of any length
make_mock_taxi_data <- function(n_monthly_rows = 5) {
  df <- data.frame(
    Date = paste("Month", 1:n_monthly_rows),
    `Trip Count` = rep(1000, n_monthly_rows),
    `Total Distance (miles)` = rep(500.5, n_monthly_rows),
    `Passenger Count` = rep(1200, n_monthly_rows),
    `Total Charge` = rep(15000, n_monthly_rows),
    `Total Fare` = rep(12000, n_monthly_rows),
    check.names = FALSE # Crucial for keeping spaces in column names!
  )

  # Add the required TOTALS row
  totals <- data.frame(
    Date = "TOTALS",
    `Trip Count` = 1000 * n_monthly_rows,
    `Total Distance (miles)` = 500.5 * n_monthly_rows,
    `Passenger Count` = 1200 * n_monthly_rows,
    `Total Charge` = 15000 * n_monthly_rows,
    `Total Fare` = 12000 * n_monthly_rows,
    check.names = FALSE
  )

  return(rbind(df, totals))
}

week_agg <- FALSE
month_agg <- TRUE
pu_locations <- c("EWR", "Manhattan", "Queens")
do_locations <- c("EWR", "Manhattan", "Queens")


# ==============================================================================
# DATA CALCULATION TESTS: fn_calculate_table_data
# ==============================================================================

test_that("fn_calculate_table_data aggregates, formats dates, and appends TOTALS row", {
  result <- fn_calculate_table_data(
    mock_raw_table_data, week_agg, month_agg,
    pu_locations, do_locations
  )

  # 1. Verify Structure & Column Names
  expect_s3_class(result, "data.frame")

  expected_cols <- c(
    "Date", "Trip Count", "Total Distance (miles)",
    "Passenger Count", "Total Fare", "Total Charge"
  )
  expect_equal(names(result), expected_cols)

  # 2. Verify Filtering & Grouping
  # Row 4 (March) should be excluded due to the aggregation flags.
  # Row 1 and 2 (Jan) should be summed together.
  # Row 3 (Feb) should remain standalone.
  # Plus the TOTALS row = 3 rows total.
  expect_equal(nrow(result), 3)

  # 3. Verify Date Formatting ("%b %Y")
  expect_equal(result$Date[1], "Jan 2023")
  expect_equal(result$Date[2], "Feb 2023")

  # 4. Verify Math & Totals Row
  expect_equal(result$Date[3], "TOTALS")

  # Jan trips (10+20) + Feb trips (30) = 60 total trips
  expect_equal(result$`Trip Count`[3], 60)

  # Jan distance (5.5+4.5) + Feb distance (10.0) = 20.0 total miles
  expect_equal(result$`Total Distance (miles)`[3], 20.0)
})

test_that("fn_calculate_table_data handles empty filtered sets gracefully", {
  # Create a dataset where nothing passes the filter
  empty_data <- mock_raw_table_data |>
    dplyr::mutate(full_month_aggregation = FALSE)

  result <- fn_calculate_table_data(
    empty_data, week_agg, month_agg,
    pu_locations, do_locations
  )

  # Should successfully return a 1-row dataframe (just the TOTALS row of zeroes)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$Date[1], "TOTALS")
})

test_that("fn_calculate_table_data defensive blocks execute", {
  # Catch invalid objects that do not inherit from data.frame or arrow datasets
  bad_data <- list(date = "2026-01-01", total_number_trips = 10)

  expect_error(
    fn_calculate_table_data(
      bad_data, week_agg, month_agg,
      pu_locations, do_locations
    ),
    regexp = "System Error: The underlying data source is disconnected or invalid."
  )

  # Invalid Granularity
  expect_error(
    fn_calculate_table_data(
      mock_raw_table_data, "week_agg", month_agg,
      pu_locations, do_locations
    ),
    regexp = "Input Error: Week aggregation selection is invalid."
  )

  expect_error(
    fn_calculate_table_data(
      mock_raw_table_data, week_agg, "month_agg",
      pu_locations, do_locations
    ),
    regexp = "Input Error: Month aggregation selection is invalid."
  )

  # Locations
  expect_error(
    fn_calculate_table_data(
      mock_raw_table_data, week_agg, month_agg,
      c(), do_locations
    ),
    regexp = "Input Error: Invalid choice for pick up location."
  )

  expect_error(
    fn_calculate_table_data(
      mock_raw_table_data, week_agg, month_agg,
      c(1, 2, 3), do_locations
    ),
    regexp = "Input Error: Invalid choice for pick up location."
  )

  expect_error(
    fn_calculate_table_data(
      mock_raw_table_data, week_agg, month_agg,
      pu_locations, c()
    ),
    regexp = "Input Error: Invalid choice for drop off location."
  )

  expect_error(
    fn_calculate_table_data(
      mock_raw_table_data, week_agg, month_agg,
      pu_locations, c(1, 2, 3)
    ),
    regexp = "Input Error: Invalid choice for drop off location."
  )
})


# ==============================================================================
# UI RENDERING & DEFENSIVE TESTS
# ==============================================================================

test_that("fn_generate_yellow_taxi_table generates successfully and hides pagination for < 9 rows", {
  # 10 data rows + 1 totals row = 11 rows total
  mock_data <- make_mock_taxi_data(3)

  expect_silent(
    res_table <- fn_generate_yellow_taxi_table(mock_data)
  )

  # Verify it returns a valid DT object
  expect_s3_class(res_table, "datatables")
  expect_s3_class(res_table, "htmlwidget")

  # Verify DOM string is "t" (Table only, no Pagination)
  expect_equal(res_table$x$options$dom, "t")
})

test_that("fn_generate_yellow_taxi_table enables pagination DOM string for > 20 rows", {
  # 25 data rows + 1 totals row = 26 rows total
  mock_data <- make_mock_taxi_data(25)

  res_table <- fn_generate_yellow_taxi_table(mock_data)

  # Verify DOM string is "tp" (Table + Pagination)
  expect_equal(res_table$x$options$dom, "pt")
})

test_that("fn_generate_yellow_taxi_table defensive blocks execute", {
  valid_mock_data <- make_mock_taxi_data(3)

  # 1. Catch non-dataframe objects
  expect_error(
    fn_generate_yellow_taxi_table(list(Date = "TOTALS")),
    "Input data must be a data.frame or tibble."
  )

  # 2. Catch empty dataframes
  expect_error(
    fn_generate_yellow_taxi_table(data.frame()),
    "Input data is empty."
  )

  # 3. Catch missing required columns
  mock_missing_cols <- valid_mock_data[, c("Date", "Trip Count")]

  expect_error(
    fn_generate_yellow_taxi_table(mock_missing_cols),
    "Missing required columns: Total Distance \\(miles\\), Passenger Count, Total Charge, Total Fare"
  )

  # 4. Catch missing TOTALS row
  mock_missing_totals <- valid_mock_data[valid_mock_data$Date != "TOTALS", ]

  expect_error(
    fn_generate_yellow_taxi_table(mock_missing_totals),
    "Input data must contain a summary row where Date is 'TOTALS'."
  )
})
