################################################################################
### FILE: tests/testthat/test-line_chart.R
################################################################################

library(testthat)
library(dplyr)
library(plotly)

# ==============================================================================
# MOCK DATA & CONFIGURATION
# ==============================================================================

# 1. RAW DATA (Used for testing fn_calculate_line_chart_data)
mock_taxi_data <- tibble::tibble(
  date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-02")),
  Vendor = c("VTS", "CMT", "VTS", "CMT"),
  payment_type = c("Cash", "Credit", "Cash", "Cash"),
  full_month_aggregation = c(TRUE, TRUE, TRUE, FALSE),
  full_week_aggregation = c(FALSE, FALSE, FALSE, TRUE),
  total_distance = c(10, 15, NA, 5), # NA included to test na.rm = TRUE
  total_number_trips = c(2, 3, 1, 1)
)

# 2. AGGREGATED DATA (Used for testing fn_generate_main_line_chart)
mock_chart_data <- data.frame(
  journey_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  total_volume = c(100, 150, 200)
)

# 3. CONFIGURATION
mock_config <- list(
  ui_values = list(
    pi_level = c("Day", "Week", "Month")
  )
)


# ==============================================================================
# DATA CALCULATION TESTS
# ==============================================================================

test_that("fn_calculate_line_chart_data correctly filters and sums data", {
  # Action: Run the function with standard inputs
  result <- fn_calculate_line_chart_data(
    base_data = mock_taxi_data,
    vendor_list = c("VTS", "CMT"),
    payment_list = c("Cash", "Credit"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_distance",
    split_by = "Total"
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

test_that("fn_calculate_line_chart_data handles strict filtering correctly", {
  # Action: Filter for only 'Cash' and 'VTS'
  result <- fn_calculate_line_chart_data(
    base_data = mock_taxi_data,
    vendor_list = c("VTS"),
    payment_list = c("Cash"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_number_trips",
    split_by = "Total"
  )

  # Assertions
  # 2023-01-01 VTS Cash has 2 trips
  expect_equal(result$total_value[result$date == as.Date("2023-01-01")], 2)
  # 2023-01-02 VTS Cash has 1 trip
  expect_equal(result$total_value[result$date == as.Date("2023-01-02")], 1)
})

test_that("fn_calculate_line_chart_data returns empty dataframe when no match is found", {
  # Action: Filter for a vendor that doesn't exist
  result <- fn_calculate_line_chart_data(
    base_data = mock_taxi_data,
    vendor_list = c("NonExistentVendor"),
    payment_list = c("Cash"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_distance",
    split_by = "Total"
  )

  # Assertions
  expect_equal(nrow(result), 0)
})

test_that("fn_calculate_line_chart_data handles 0-row dataframes gracefully", {
  # Create an intentionally empty dataframe with the correct schema
  empty_data <- mock_taxi_data |> dplyr::filter(Vendor == "ImpossibleMatch")

  result <- fn_calculate_line_chart_data(
    base_data = empty_data,
    vendor_list = c("VTS"),
    payment_list = c("Cash"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_distance",
    split_by = "Total"
  )

  # It should not crash; it should return a 0-row dataframe with the correct column names
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("date", "total_value"))
})

test_that("fn_calculate_line_chart_data safely returns 0 rows for empty filter lists", {
  # Action: Pass an empty character vector for vendors
  result <- fn_calculate_line_chart_data(
    base_data = mock_taxi_data,
    vendor_list = character(0),
    payment_list = c("Cash"),
    month_agg = TRUE,
    week_agg = FALSE,
    data_field = "total_distance",
    split_by = "Total"
  )

  expect_equal(nrow(result), 0)
})

test_that("fn_calculate_line_chart_data defensive blocks execute", {
  expect_error(
    fn_calculate_line_chart_data(
      list(date = "2026-01-01"), c("VTS"), c("Cash"),
      TRUE, FALSE, "total_distance", "Total"
    ),
    regexp = "System Error"
  )

  expect_error(
    fn_calculate_line_chart_data(
      mock_taxi_data, c("VTS"), c("Cash"), "TRUE",
      FALSE, "total_distance", "Total"
    ),
    regexp = "Month aggregation selection is invalid"
  )

  expect_error(
    fn_calculate_line_chart_data(
      mock_taxi_data, c("VTS"), c("Cash"), TRUE,
      "FALSE", "total_distance", "Total"
    ),
    regexp = "Week aggregation selection is invalid"
  )

  expect_error(
    fn_calculate_line_chart_data(
      mock_taxi_data, c("VTS"), c("Cash"), TRUE,
      FALSE, 123, "Total"
    ),
    regexp = "Selected metric is invalid"
  )

  bad_data <- mock_taxi_data |> dplyr::select(-Vendor)
  expect_error(
    fn_calculate_line_chart_data(
      bad_data, c("VTS"), c("Cash"), TRUE, FALSE,
      "total_distance", "Total"
    ),
    regexp = "missing required columns"
  )

  expect_error(
    fn_calculate_line_chart_data(
      base_data = mock_taxi_data,
      vendor_list = character(0),
      payment_list = c("Cash"),
      month_agg = TRUE,
      week_agg = FALSE,
      data_field = "total_distance",
      split_by = "banana"
    ),
    regexp = "line type is invalid"
  )
})


# ==============================================================================
# UI RENDERING TESTS
# ==============================================================================

test_that("fn_generate_main_line_chart generates a valid plotly object", {
  raw_fig <- fn_generate_main_line_chart(
    base_data = mock_chart_data,
    granularity = "Month",
    x_col = "journey_date",
    y_col = "total_volume",
    title = "Test Chart",
    y_lab_title = "Volume",
    split_by = "Total",
    config = mock_config
  )

  # Force Plotly to compile the lazy object into its final list structure
  built_fig <- suppressWarnings(plotly::plotly_build(raw_fig))

  # Assertions
  expect_s3_class(raw_fig, "plotly")
  expect_s3_class(raw_fig, "htmlwidget")

  # Verify the internal structure contains the data using the BUILT figure
  expect_true(!is.null(built_fig$x$data[[1]]$x))
  expect_true(!is.null(built_fig$x$data[[1]]$y))
})


test_that("fn_generate_main_line_chart defensive blocks execute", {
  # Non-dataframe validation
  bad_data <- list(journey_date = c("2023-01-01"), total_volume = c(100))
  expect_error(
    fn_generate_main_line_chart(
      bad_data, "Month", "journey_date", "total_volume",
      "Test Chart", "Volume", "Total", mock_config
    ),
    regexp = "System Error: The underlying data source"
  )

  # Invalid Title
  expect_error(
    fn_generate_main_line_chart(
      mock_chart_data, "Month", "journey_date",
      "total_volume", 12345, "Volume", "Total", mock_config
    ),
    regexp = "System Error: Please supply a valid title for the chart$"
  )

  # Invalid Y-axis label
  expect_error(
    fn_generate_main_line_chart(
      mock_chart_data, "Month", "journey_date",
      "total_volume", "Test Chart", NULL, "Total", mock_config
    ),
    regexp = "System Error: Please supply a valid title for the chart's y-axis"
  )

  # Invalid Granularity
  expect_error(
    fn_generate_main_line_chart(
      mock_chart_data, "Year", "journey_date",
      "total_volume", "Test Chart", "Volume", "Total", mock_config
    ),
    regexp = "System Error: Selected granularity is not permitted"
  )

  # Missing Columns
  expect_error(
    fn_generate_main_line_chart(
      mock_chart_data, "Month", "journey_date",
      "non_existent_column", "Test Chart", "Volume", "Total", mock_config
    ),
    regexp = "Data Error: The dataset is missing required columns"
  )

  expect_error(
    fn_generate_main_line_chart(
      base_data = mock_chart_data,
      granularity = "Month",
      x_col = "journey_date",
      y_col = "total_volume",
      title = "Test Chart",
      y_lab_title = "Volume",
      split_by = "banana",
      config = mock_config
    ),
    regexp = "line type is invalid"
  )
})
