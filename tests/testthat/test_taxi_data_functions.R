library(testthat)
library(dplyr)

# 1. Create a minimal mock dataset mimicking your schema
mock_taxi_data <- tibble::tibble(
  date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-02")),
  Vendor = c("VTS", "CMT", "VTS", "CMT"),
  payment_type = c("Cash", "Credit", "Cash", "Cash"),
  full_month_aggregation = c(TRUE, TRUE, TRUE, FALSE),
  full_week_aggregation = c(FALSE, FALSE, FALSE, TRUE),
  total_distance = c(10, 15, NA, 5),          # NA included to test na.rm = TRUE
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

test_that("fn_calculate_trip_volumes throws an error if data_field is missing", {
  
  # The test passes if the function ABORTS with an error
  expect_error(
    fn_calculate_trip_volumes(
      base_data = mock_taxi_data,
      vendor_list = c("VTS", "CMT"),
      payment_list = c("Cash", "Credit"),
      month_agg = TRUE,
      week_agg = FALSE,
      data_field = "non_existent_column"
    )
  )
})

test_that("fn_calculate_trip_volumes throws an error if base_data is missing required columns", {
  
  # Remove the 'Vendor' column to simulate malformed input data
  bad_data <- mock_taxi_data |> dplyr::select(-Vendor)
  
  expect_error(
    fn_calculate_trip_volumes(
      base_data = bad_data,
      vendor_list = c("VTS"),
      payment_list = c("Cash"),
      month_agg = TRUE,
      week_agg = FALSE,
      data_field = "total_distance"
    )
  )
})

################################################################################

# --- MOCK DATA FOR HEATMAPS ---
mock_heatmap_data <- tibble::tibble(
  date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-15", "2023-02-01")),
  full_month_aggregation = c(TRUE, TRUE, FALSE, TRUE),
  full_week_aggregation = c(FALSE, FALSE, TRUE, FALSE),
  PULocation = c("Loc_A", "Loc_A", "Loc_B", "Loc_A"),
  DOLocation = c("Loc_X", "Loc_X", "Loc_Y", "Loc_Z")
)

# --- STANDARD BEHAVIOUR TESTS ---

test_that("fn_calculate_heatmap_data processes monthly aggregation correctly", {
  
  result <- fn_calculate_heatmap_data(
    base_data = mock_heatmap_data,
    month_agg = TRUE,
    week_agg = FALSE
  )
  
  # Assertions
  expect_s3_class(result, "data.frame")
  
  # Should have 4 columns: PULocation, DOLocation, date_month, trips
  expect_equal(ncol(result), 4)
  expect_true("date_month" %in% names(result))
  
  # January Loc_A to Loc_X should have 2 trips grouped together
  jan_group <- result |> 
    dplyr::filter(date_month == "2023 month:01", PULocation == "Loc_A", DOLocation == "Loc_X")
  
  expect_equal(jan_group$trips, 2)
  
  # February Loc_A to Loc_Z should have 1 trip
  feb_group <- result |> 
    dplyr::filter(date_month == "2023 month:02")
  
  expect_equal(feb_group$trips, 1)
})

test_that("fn_calculate_heatmap_data processes weekly aggregation correctly", {
  
  result <- fn_calculate_heatmap_data(
    base_data = mock_heatmap_data,
    month_agg = FALSE,
    week_agg = TRUE
  )
  
  # Assertions
  # Should have 4 columns, but with date_week instead of date_month
  expect_true("date_week" %in% names(result))
  
  # Only one row matches week_agg == TRUE in the mock data
  expect_equal(nrow(result), 1)
  expect_equal(result$trips, 1)
  expect_equal(result$PULocation, "Loc_B")
})

test_that("fn_calculate_heatmap_data handles no temporal aggregation", {
  
  result <- fn_calculate_heatmap_data(
    base_data = mock_heatmap_data,
    month_agg = FALSE,
    week_agg = FALSE
  )
  
  # Assertions
  # When both are FALSE, it should just group by PU and DO (3 columns total)
  expect_equal(ncol(result), 3)
  expect_false("date_month" %in% names(result))
  expect_false("date_week" %in% names(result))
})

# --- DEFENSIVE TESTS ---

test_that("fn_calculate_heatmap_data handles 0-row dataframes gracefully", {
  
  empty_data <- mock_heatmap_data |> dplyr::filter(PULocation == "Impossible")
  
  result <- fn_calculate_heatmap_data(
    base_data = empty_data,
    month_agg = TRUE,
    week_agg = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true("trips" %in% names(result))
})

test_that("fn_calculate_heatmap_data triggers UI validation on missing columns", {
  
  # Remove DOLocation
  bad_data <- mock_heatmap_data |> dplyr::select(-DOLocation)
  
  # shiny::validate() throws an error that testthat catches seamlessly
  expect_error(
    fn_calculate_heatmap_data(
      base_data = bad_data,
      month_agg = TRUE,
      week_agg = FALSE
    )
  )
})

test_that("fn_calculate_heatmap_data triggers UI validation on invalid argument types", {
  
  # Pass a character string instead of a logical value for month_agg
  expect_error(
    fn_calculate_heatmap_data(
      base_data = mock_heatmap_data,
      month_agg = "TRUE", 
      week_agg = FALSE
    )
  )
})