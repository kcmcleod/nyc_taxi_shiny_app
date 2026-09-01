library(testthat)
library(shiny)
library(dplyr)
library(lubridate)
library(plotly)

# ==============================================================================
# MOCK DATA & CONFIGURATION
# ==============================================================================

mock_daily_volumes <- tibble::tibble(
  date = seq.Date(as.Date("2023-01-01"), as.Date("2023-01-31"), by = "day"),
  total_trips = rep(10, 31)
)

mock_metadata <- list(
  min_date = as.Date("2023-01-01"),
  max_date = as.Date("2023-12-31"),
  daily_volumes = mock_daily_volumes
)

mock_config <- list(
  colours = list(
    theme = list(primary_accent = "#00539F"),
    icons = list(taxi = "#FFC107")
  ),
  ui_values = list(
    pi_level = c("Day", "Week", "Month")
  )
)

# ==============================================================================
# MODULE SERVER TESTS
# ==============================================================================

test_that("mod_yellow_taxis_availability_server processes reactive inputs and renders chart", {
  mock_data_version <- reactiveVal(1)
  mock_start_date <- reactiveVal(as.Date("2023-01-15"))
  mock_end_date <- reactiveVal(as.Date("2023-01-20"))

  testServer(
    app = mod_yellow_taxis_availability_server,
    args = list(
      app_metadata = mock_metadata,
      config = mock_config,
      data_version = mock_data_version,
      user_start_date = mock_start_date,
      user_end_date = mock_end_date
    ),
    expr = {
      # 1. Test "Full data range" path
      session$setInputs(pi_range = "Full data range", pi_level = "Month")
      session$elapse(800)

      res_full <- rv_availbility_chart_filtered_data_raw()
      expect_s3_class(res_full, "data.frame")
      expect_equal(nrow(res_full), 1)
      expect_equal(sum(res_full$total_trips), 310)

      p_full <- output$ply_data_vols
      expect_true(!is.null(p_full))

      # 2. Test "From above data input" path (uses user_start_date and user_end_date)
      session$setInputs(pi_range = "From above data input", pi_level = "Week")
      session$elapse(800)

      res_part <- rv_availbility_chart_filtered_data_raw()
      expect_s3_class(res_part, "data.frame")
      # Expect 70 trips because weekly floor_date pulls Saturday the 21st
      # into the Jan 15th week cohort
      expect_equal(sum(res_part$total_trips), 70)

      p_part <- output$ply_data_vols
      expect_true(!is.null(p_part))
    }
  )
})

test_that("mod_yellow_taxis_availability_server handles validations and tryCatch errors safely", {
  # Corrupted metadata missing mandatory columns
  bad_metadata <- list(
    min_date = as.Date("2023-01-01"),
    max_date = as.Date("2023-12-31"),
    daily_volumes = data.frame(invalid_col = 1)
  )

  testServer(
    app = mod_yellow_taxis_availability_server,
    args = list(
      app_metadata = bad_metadata,
      config = mock_config,
      data_version = reactiveVal(1),
      user_start_date = reactiveVal(as.Date("2023-01-01")),
      user_end_date = reactiveVal(as.Date("2023-01-31"))
    ),
    expr = {
      session$setInputs(pi_range = "Full data range", pi_level = "Month")
      session$elapse(800)

      # Output should catch the error and validate cleanly
      err <- expect_error(output$ply_data_vols)
      expect_match(err$message, "Chart unavailable due to data error")
    }
  )
})
