################################################################################
### FILE: tests/testthat/test-heatmap.R
################################################################################

library(testthat)
library(plotly)
library(ggplot2)
library(dplyr)

# ==============================================================================
# MOCK DATA & CONFIGURATION
# ==============================================================================

# 1. RAW DATA (Used for testing fn_calculate_heatmap_data)
mock_heatmap_raw_data <- tibble::tibble(
  date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-15", "2023-02-01")),
  full_month_aggregation = c(TRUE, TRUE, FALSE, TRUE),
  full_week_aggregation = c(FALSE, FALSE, TRUE, FALSE),
  PULocation = c("Loc_A", "Loc_A", "Loc_B", "Loc_A"),
  DOLocation = c("Loc_X", "Loc_X", "Loc_Y", "Loc_Z"),
  total_number_trips = 100
)

# 2. AGGREGATED DATA (Used for testing fn_generate_heatmap_chart)
mock_heatmap_base_data <- data.frame(
  PULocation = c("Loc_A", "Loc_A", "Loc_B", "Loc_B"),
  DOLocation = c("Loc_X", "Loc_Y", "Loc_X", "Loc_Y"),
  trips = c(10, 20, 30, 40)
)

mock_heatmap_facet_data <- mock_heatmap_base_data |>
  dplyr::mutate(date_month = c(
    "2023 month:01", "2023 month:01", "2023 month:02", "2023 month:02"
  ))

# 3. CONFIGURATION
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


# ==============================================================================
# DATA CALCULATION TESTS
# ==============================================================================

test_that("fn_calculate_heatmap_data processes monthly aggregation correctly", {
  result <- fn_calculate_heatmap_data(
    base_data = mock_heatmap_raw_data,
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
    base_data = mock_heatmap_raw_data,
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
    base_data = mock_heatmap_raw_data,
    month_agg = FALSE,
    week_agg = FALSE
  )

  # Assertions
  # When both are FALSE, it should just group by PU and DO (3 columns total)
  expect_equal(ncol(result), 3)
  expect_false("date_month" %in% names(result))
  expect_false("date_week" %in% names(result))
})

test_that("fn_calculate_heatmap_data handles 0-row dataframes gracefully", {
  empty_data <- mock_heatmap_raw_data |> dplyr::filter(PULocation == "Impossible")

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
  # base_data invalid type
  expect_error(
    fn_calculate_heatmap_data(list(PULocation = "Loc_A"), TRUE, FALSE),
    regexp = "System Error"
  )

  # month_agg invalid type
  expect_error(
    fn_calculate_heatmap_data(mock_heatmap_raw_data, "TRUE", FALSE),
    regexp = "Month aggregation selection is invalid"
  )

  # week_agg invalid type
  expect_error(
    fn_calculate_heatmap_data(mock_heatmap_raw_data, TRUE, "FALSE"),
    regexp = "Week aggregation selection is invalid"
  )

  # missing columns
  bad_data <- mock_heatmap_raw_data |> dplyr::select(-DOLocation)
  expect_error(
    fn_calculate_heatmap_data(
      base_data = bad_data,
      month_agg = TRUE,
      week_agg = FALSE
    ),
    regexp = "missing required columns"
  )
})


# ==============================================================================
# UI RENDERING TESTS
# ==============================================================================

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

test_that("fn_generate_heatmap_chart handles weekly faceting correctly", {
  # Create mock data with a date_week column to hit that specific branch
  mock_heatmap_week_data <- mock_heatmap_base_data |>
    dplyr::mutate(date_week = c(
      "2026 week:12", "2026 week:12", "2026 week:13", "2026 week:13"
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

test_that("fn_generate_heatmap_chart defensive blocks execute", {
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
    fn_generate_heatmap_chart(
      mock_heatmap_base_data,
      config = bad_config,
      title = "Heatmap Test"
    ),
    regexp = "System Error: Dashboard configuration \\(colours\\) is missing"
  )

  # Invalid axis column data type
  expect_error(
    fn_generate_heatmap_chart(
      mock_heatmap_base_data, mock_heatmap_config,
      x_col = 123, title = "Heatmap Test"
    ),
    regexp = "System Error: Column names for the heatmap axes must be valid text strings"
  )

  # Missing columns
  expect_error(
    fn_generate_heatmap_chart(
      mock_heatmap_base_data, mock_heatmap_config,
      z_col = "non_existent_column", title = "Heatmap Test"
    ),
    regexp = "Data Error: The heatmap dataset is missing required columns"
  )
})
