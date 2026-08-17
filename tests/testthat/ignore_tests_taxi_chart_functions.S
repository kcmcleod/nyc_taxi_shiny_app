library(testthat)
library(plotly)
library(ggplot2)
library(dplyr)
library(logger)
library(DT)


# --- MOCK DATA FOR CHARTS ---
mock_chart_data <- data.frame(
  journey_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
  total_volume = c(100, 150, 200)
)

mock_config <- list(
  ui_values = list(
    pi_level = c("Day", "Week", "Month")
  )
)

mock_heatmap_config <- list(
  colours = list(
    theme = list(
      body_bg = "#FFFFFF",
      primary_accent = "#0000FF"
    ),
    icons = list(
      taxi = "#FFFF00"
    )
  )
)

mock_heatmap_base_data <- data.frame(
  PULocation = c("Loc_A", "Loc_A", "Loc_B", "Loc_B"),
  DOLocation = c("Loc_X", "Loc_Y", "Loc_X", "Loc_Y"),
  trips = c(10, 20, 30, 40)
)

# Add a date_month column to test the facet_wrap logic
mock_heatmap_facet_data <- mock_heatmap_base_data |>
  dplyr::mutate(date_month = c(
    "2023 month:01", "2023 month:01", "2023 month:02",
    "2023 month:02"
  ))


# ==============================================================================
# STANDARD BEHAVIOUR TESTS
# ==============================================================================

test_that("fn_generate_main_line_chart generates a valid plotly object", {
  raw_fig <- fn_generate_main_line_chart(
    base_data = mock_chart_data,
    granularity = "Month",
    x_col = "journey_date",
    y_col = "total_volume",
    title = "Test Chart",
    y_lab_title = "Volume",
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

test_that("fn_generate_heatmap_chart generates a valid ggplotly object (No Facets)", {
  fig <- fn_generate_heatmap_chart(
    base_data = mock_heatmap_base_data,
    config = mock_heatmap_config,
    title = "Base Heatmap Test"
  )

  # Assertions
  expect_s3_class(fig, "plotly")
  expect_s3_class(fig, "htmlwidget")
})

test_that("fn_generate_heatmap_chart handles dynamic faceting and height calculation", {
  fig <- fn_generate_heatmap_chart(
    base_data = mock_heatmap_facet_data,
    config = mock_heatmap_config,
    title = "Faceted Heatmap Test"
  )

  # Assertions
  expect_s3_class(fig, "plotly")

  # Because n_rows is ceiling(2 facets / 3) = 1,
  # height should resolve to
  # (1 row * 250px height) + (0 gaps) + (100px top margin) + (80px bottom margin)
  expect_equal(fig$height, 430)
})


# ==============================================================================
# DEFENSIVE TESTS
# ==============================================================================

test_that("fn_generate_main_line_chart defensive blocks execute", {
  # Non-dataframe validation
  bad_data <- list(journey_date = c("2023-01-01"), total_volume = c(100))
  expect_error(
    fn_generate_main_line_chart(
      bad_data, "Month", "journey_date", "total_volume",
      "Test Chart", "Volume", mock_config
    ),
    regexp = "System Error: The underlying data source"
  )

  # Invalid Title
  expect_error(
    fn_generate_main_line_chart(
      mock_chart_data, "Month", "journey_date",
      "total_volume", 12345, "Volume", mock_config
    ),
    regexp = "System Error: Please supply a valid title for the chart$"
  )

  # Invalid Y-axis label
  expect_error(
    fn_generate_main_line_chart(
      mock_chart_data, "Month", "journey_date",
      "total_volume", "Test Chart", NULL, mock_config
    ),
    regexp = "System Error: Please supply a valid title for the chart's y-axis"
  )

  # Invalid Granularity
  expect_error(
    fn_generate_main_line_chart(
      mock_chart_data, "Year", "journey_date",
      "total_volume", "Test Chart", "Volume", mock_config
    ),
    regexp = "System Error: Selected granularity is not permitted"
  )

  # Missing Columns
  expect_error(
    fn_generate_main_line_chart(
      mock_chart_data, "Month", "journey_date",
      "non_existent_column", "Test Chart", "Volume", mock_config
    ),
    regexp = "Data Error: The dataset is missing required columns"
  )
})

test_that("fn_generate_heatmap_chart defensive blocks execute", {
  # Prevent logger from crashing the temporary covr environment for this specific function
  logger::log_threshold(logger::FATAL)

  # Non-dataframe validation
  bad_data <- list(PULocation = c("A"), DOLocation = c("X"), trips = c(10))
  expect_error(
    fn_generate_heatmap_chart(bad_data, mock_heatmap_config, title = "Heatmap Test"),
    regexp = "System Error: The underlying data source must be collected"
  )

  # Invalid Title
  expect_error(
    fn_generate_heatmap_chart(mock_heatmap_base_data, mock_heatmap_config, title = 999),
    regexp = "System Error: Invalid chart title"
  )

  # Missing/Invalid Config
  expect_error(
    fn_generate_heatmap_chart(mock_heatmap_base_data, title = "Heatmap Test"),
    regexp = "System Error: Dashboard configuration \\(colours\\) is missing"
  )

  bad_config <- list(some_other_setting = TRUE)
  expect_error(
    fn_generate_heatmap_chart(mock_heatmap_base_data,
      config = bad_config,
      title = "Heatmap Test"
    ),
    regexp = "System Error: Dashboard configuration \\(colours\\) is missing"
  )

  # Invalid axis column data type
  expect_error(
    fn_generate_heatmap_chart(mock_heatmap_base_data, mock_heatmap_config,
      x_col = 123, title = "Heatmap Test"
    ),
    regexp = "System Error: Column names for the heatmap axes must be valid text strings"
  )

  # Missing columns
  expect_error(
    fn_generate_heatmap_chart(mock_heatmap_base_data, mock_heatmap_config,
      z_col = "non_existent_column", title = "Heatmap Test"
    ),
    regexp = "Data Error: The heatmap dataset is missing required columns"
  )
})

test_that("fn_generate_heatmap_chart handles weekly faceting correctly", {
  # Create mock data with a date_week column to hit that specific branch
  mock_heatmap_week_data <- mock_heatmap_base_data |>
    dplyr::mutate(date_week = c(
      "2026 week:12", "2026 week:12", "2026 week:13",
      "2026 week:13"
    ))

  fig <- fn_generate_heatmap_chart(
    base_data = mock_heatmap_week_data,
    config = mock_heatmap_config,
    title = "Weekly Faceted Heatmap Test"
  )

  # Assertions
  expect_s3_class(fig, "plotly")
  expect_equal(fig$height, 430)
})

################################################################################
# TABLE


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

# --- MOCK DATA FOR HEATMAPS ---
mock_heatmap_data <- tibble::tibble(
  date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-15", "2023-02-01")),
  full_month_aggregation = c(TRUE, TRUE, FALSE, TRUE),
  full_week_aggregation = c(FALSE, FALSE, TRUE, FALSE),
  PULocation = c("Loc_A", "Loc_A", "Loc_B", "Loc_A"),
  DOLocation = c("Loc_X", "Loc_X", "Loc_Y", "Loc_Z"),
  total_number_trips = 100
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
    dplyr::filter(
      date_month == "2023 month:01", PULocation == "Loc_A",
      DOLocation == "Loc_X"
    )
  
  expect_equal(jan_group$trips, 200)
  
  # February Loc_A to Loc_Z should have 1 trip
  feb_group <- result |>
    dplyr::filter(date_month == "2023 month:02")
  
  expect_equal(feb_group$trips, 100)
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
  expect_equal(result$trips, 100)
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

test_that("fn_calculate_heatmap_data defensive blocks execute", {
  logger::log_threshold(logger::FATAL)
  
  # base_data invalid type
  expect_error(
    fn_calculate_heatmap_data(list(PULocation = "Loc_A"), TRUE, FALSE),
    regexp = "System Error"
  )
  
  # month_agg invalid type
  expect_error(
    fn_calculate_heatmap_data(mock_heatmap_data, "TRUE", FALSE),
    regexp = "Month aggregation selection is invalid"
  )
  
  # week_agg invalid type
  expect_error(
    fn_calculate_heatmap_data(mock_heatmap_data, TRUE, "FALSE"),
    regexp = "Week aggregation selection is invalid"
  )
  
  # missing columns
  bad_data <- mock_heatmap_data |> dplyr::select(-DOLocation)
  expect_error(
    fn_calculate_heatmap_data(
      base_data = bad_data,
      month_agg = TRUE,
      week_agg = FALSE
    ),
    regexp = "missing required columns"
  )
})



# ------------------------------------------------------------------------------
# MOCK DATA GENERATOR
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# TESTS
# ------------------------------------------------------------------------------

test_that("Table generates successfully and hides pagination for <= 20 rows", {
  # 10 data rows + 1 totals row = 11 rows total
  mock_data <- make_mock_taxi_data(10)
  
  expect_silent(
    res_table <- fn_generate_yellow_taxi_table(mock_data)
  )
  
  # Verify it returns a valid DT object
  expect_s3_class(res_table, "datatables")
  expect_s3_class(res_table, "htmlwidget")
  
  # Verify DOM string is "t" (Table only, no Pagination)
  expect_equal(res_table$x$options$dom, "t")
})

test_that("Table enables pagination DOM string for > 20 rows", {
  # 25 data rows + 1 totals row = 26 rows total
  mock_data <- make_mock_taxi_data(25) 
  
  res_table <- fn_generate_yellow_taxi_table(mock_data)
  
  # Verify DOM string is "tp" (Table + Pagination)
  expect_equal(res_table$x$options$dom, "tp")
})

test_that("Validation traps catch malformed inputs cleanly", {
  
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

