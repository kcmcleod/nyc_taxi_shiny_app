library(testthat)
library(plotly)
library(ggplot2)
library(dplyr)
library(logger)

# 1. THE SAFE FIX: Inject dummy function for un-prefixed log_error calls in covr
if (!exists("log_error", mode = "function")) {
  log_error <- function(...) invisible()
}

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
