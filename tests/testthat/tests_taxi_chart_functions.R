library(testthat)
library(plotly)

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

# --- STANDARD BEHAVIOUR TESTS ---

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
  built_fig <- plotly::plotly_build(raw_fig)
  
  # Assertions
  expect_s3_class(raw_fig, "plotly")
  expect_s3_class(raw_fig, "htmlwidget")
  
  # Verify the internal structure contains the data using the BUILT figure
  expect_true(!is.null(built_fig$x$data[[1]]$x))
  expect_true(!is.null(built_fig$x$data[[1]]$y))
  
  # Optional: strictly verify the actual values made it into the chart
  expect_equal(as.numeric(built_fig$x$data[[1]]$y), c(100, 150, 200))
})

# --- DEFENSIVE TESTS ---

test_that("fn_generate_main_line_chart rejects non-dataframes (e.g. Arrow objects)", {
  
  # Simulate an object that is not an in-memory data.frame
  bad_data <- list(journey_date = c("2023-01-01"), total_volume = c(100))
  
  expect_error(
    fn_generate_main_line_chart(
      base_data = bad_data,
      granularity = "Month",
      x_col = "journey_date",
      y_col = "total_volume",
      title = "Test Chart",
      y_lab_title = "Volume",
      config = mock_config
    )
  )
})

test_that("fn_generate_main_line_chart validates character inputs (title, y_lab)", {
  
  # Pass a numeric value for the title
  expect_error(
    fn_generate_main_line_chart(
      base_data = mock_chart_data,
      granularity = "Month",
      x_col = "journey_date",
      y_col = "total_volume",
      title = 12345, 
      y_lab_title = "Volume",
      config = mock_config
    )
  )
  
  # Pass a NULL for the y_lab_title
  expect_error(
    fn_generate_main_line_chart(
      base_data = mock_chart_data,
      granularity = "Month",
      x_col = "journey_date",
      y_col = "total_volume",
      title = "Test Chart", 
      y_lab_title = NULL,
      config = mock_config
    )
  )
})

test_that("fn_generate_main_line_chart catches invalid granularity matching config", {
  
  # Pass "Year" which is not in our mock_config (Day, Week, Month)
  expect_error(
    fn_generate_main_line_chart(
      base_data = mock_chart_data,
      granularity = "Year",
      x_col = "journey_date",
      y_col = "total_volume",
      title = "Test Chart",
      y_lab_title = "Volume",
      config = mock_config
    )
  )
})

test_that("fn_generate_main_line_chart traps missing columns", {
  
  # Request 'non_existent_column' as the y-axis
  expect_error(
    fn_generate_main_line_chart(
      base_data = mock_chart_data,
      granularity = "Month",
      x_col = "journey_date",
      y_col = "non_existent_column",
      title = "Test Chart",
      y_lab_title = "Volume",
      config = mock_config
    )
  )
})


################################################################################


library(testthat)
library(plotly)
library(ggplot2)
library(dplyr)

# --- MOCK DATA FOR GGPLOT HEATMAP ---
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
  dplyr::mutate(date_month = c("2023 month:01", "2023 month:01", "2023 month:02", "2023 month:02"))


# --- STANDARD BEHAVIOUR TESTS ---

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
  
  # Because n_rows is ceiling(2 facets / 3) = 1, height should resolve to max(400, 1 * 300) = 400
  expect_equal(fig$height, 400)
})


# --- DEFENSIVE TESTS ---

test_that("fn_generate_heatmap_chart rejects non-dataframes (e.g. Arrow objects)", {
  
  bad_data <- list(PULocation = c("A"), DOLocation = c("X"), trips = c(10))
  
  expect_error(
    fn_generate_heatmap_chart(
      base_data = bad_data,
      config = mock_heatmap_config,
      title = "Heatmap Test"
    )
  )
})

test_that("fn_generate_heatmap_chart enforces config requirements", {
  
  # Missing config entirely
  expect_error(
    fn_generate_heatmap_chart(
      base_data = mock_heatmap_base_data,
      title = "Heatmap Test"
    )
  )
  
  # Malformed config (missing colours)
  bad_config <- list(some_other_setting = TRUE)
  expect_error(
    fn_generate_heatmap_chart(
      base_data = mock_heatmap_base_data,
      config = bad_config,
      title = "Heatmap Test"
    )
  )
})

test_that("fn_generate_heatmap_chart validates title and column arguments", {
  
  # Invalid Title
  expect_error(
    fn_generate_heatmap_chart(
      base_data = mock_heatmap_base_data,
      config = mock_heatmap_config,
      title = 999 
    )
  )
  
  # Invalid axis column data type
  expect_error(
    fn_generate_heatmap_chart(
      base_data = mock_heatmap_base_data,
      config = mock_heatmap_config,
      title = "Heatmap Test",
      x_col = 123
    )
  )
})

test_that("fn_generate_heatmap_chart traps missing columns dynamically", {
  
  # Request 'non_existent_column' as the z-axis
  expect_error(
    fn_generate_heatmap_chart(
      base_data = mock_heatmap_base_data,
      config = mock_heatmap_config,
      title = "Heatmap Test",
      z_col = "non_existent_column"
    )
  )
})