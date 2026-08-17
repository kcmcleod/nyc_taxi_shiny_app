library(testthat)
library(dplyr)


# 1. Create a minimal mock dataset mimicking your schema
mock_taxi_data <- tibble::tibble(
  date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-02")),
  Vendor = c("VTS", "CMT", "VTS", "CMT"),
  payment_type = c("Cash", "Credit", "Cash", "Cash"),
  full_month_aggregation = c(TRUE, TRUE, TRUE, FALSE),
  full_week_aggregation = c(FALSE, FALSE, FALSE, TRUE),
  total_distance = c(10, 15, NA, 5), # NA included to test na.rm = TRUE
  total_number_trips = c(2, 3, 1, 1)
)

test_that("fn_calculate_trip_volumes correctly filters and sums data", {
  # Action: Run the function with standard inputs
  result <- fn_calculate_trip_volumes(
    base_data = mock_taxi_data,
    vendor_list = c("VTS", "CMT"),
    payment_list = c("Cash", "Credit"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_distance"
  )

  # Assertions
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("date", "total_value"))

  # 2023-01-01 should sum VTS (10) and CMT (15) = 25
  expect_equal(result$total_value[result$date == as.Date("2023-01-01")], 25)

  # 2023-01-02 should just return 0 (VTS is NA, CMT on this date is week_agg = TRUE)
  expect_equal(result$total_value[result$date == as.Date("2023-01-02")], 0)
})

test_that("fn_calculate_trip_volumes handles strict filtering correctly", {
  # Action: Filter for only 'Cash' and 'VTS'
  result <- fn_calculate_trip_volumes(
    base_data = mock_taxi_data,
    vendor_list = c("VTS"),
    payment_list = c("Cash"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_number_trips"
  )

  # Assertions
  # 2023-01-01 VTS Cash has 2 trips
  expect_equal(result$total_value[result$date == as.Date("2023-01-01")], 2)
  # 2023-01-02 VTS Cash has 1 trip
  expect_equal(result$total_value[result$date == as.Date("2023-01-02")], 1)
})

test_that("fn_calculate_trip_volumes returns empty dataframe when no match is found", {
  # Action: Filter for a vendor that doesn't exist
  result <- fn_calculate_trip_volumes(
    base_data = mock_taxi_data,
    vendor_list = c("NonExistentVendor"),
    payment_list = c("Cash"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_distance"
  )

  # Assertions
  expect_equal(nrow(result), 0)
})

test_that("fn_calculate_trip_volumes handles 0-row dataframes gracefully", {
  # Create an intentionally empty dataframe with the correct schema
  empty_data <- mock_taxi_data |> dplyr::filter(Vendor == "ImpossibleMatch")

  result <- fn_calculate_trip_volumes(
    base_data = empty_data,
    vendor_list = c("VTS"),
    payment_list = c("Cash"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_distance"
  )

  # It should not crash; it should return a 0-row dataframe with the correct column names
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("date", "total_value"))
})

test_that("fn_calculate_trip_volumes safely returns 0 rows for empty filter lists", {
  # Action: Pass an empty character vector for vendors
  result <- fn_calculate_trip_volumes(
    base_data = mock_taxi_data,
    vendor_list = character(0),
    payment_list = c("Cash"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_distance"
  )

  expect_equal(nrow(result), 0)
})

test_that("fn_calculate_trip_volumes defensive blocks execute", {
  # BRUTE FORCE FIX: Neutralize the loggers in the test environment
  # so they cannot crash before the stop() line is reached.
  logger::log_threshold(logger::FATAL)

  expect_error(
    fn_calculate_trip_volumes(
      list(date = "2026-01-01"), c("VTS"), c("Cash"),
      TRUE, FALSE, "total_distance"
    ),
    regexp = "System Error"
  )

  expect_error(
    fn_calculate_trip_volumes(
      mock_taxi_data, c("VTS"), c("Cash"), "TRUE",
      FALSE, "total_distance"
    ),
    regexp = "Month aggregation selection is invalid"
  )

  expect_error(
    fn_calculate_trip_volumes(
      mock_taxi_data, c("VTS"), c("Cash"), TRUE,
      "FALSE", "total_distance"
    ),
    regexp = "Week aggregation selection is invalid"
  )

  expect_error(
    fn_calculate_trip_volumes(mock_taxi_data, c("VTS"), c("Cash"), TRUE, FALSE, 123),
    regexp = "Selected metric is invalid"
  )

  bad_data <- mock_taxi_data |> dplyr::select(-Vendor)
  expect_error(
    fn_calculate_trip_volumes(
      bad_data, c("VTS"), c("Cash"), TRUE, FALSE,
      "total_distance"
    ),
    regexp = "missing required columns"
  )
})


################################################################################