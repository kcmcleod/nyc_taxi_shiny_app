library(testthat)
library(shiny)
library(dplyr)
library(lubridate)

mock_parent_data <- tibble::tibble(
  id = 1:5,
  date = as.Date(c(
    NA, # Condition 1: Missing dates
    "2023-01-15", # Condition 2: Daily data (Middle of Jan)
    "2023-01-01", # Condition 3: Monthly data (1st of Jan)
    "2023-01-08", # Condition 4: Weekly data (Starts on a Sunday)
    "2022-01-01" # Condition 5: Out of bounds (Should be filtered out)
  )),
  full_month_aggregation = c(FALSE, FALSE, TRUE, FALSE, FALSE),
  full_week_aggregation = c(FALSE, FALSE, FALSE, TRUE, FALSE)
)

mock_metadata <- list(
  min_date = as.Date("2022-01-01"),
  max_date = as.Date("2023-12-31")
)


test_that("mod_yellow_taxis_parent_server processes complex date filtering correctly", {
  mock_taxi_data <- reactiveVal(mock_parent_data)
  mock_data_version <- reactiveVal(1)

  testServer(
    app = mod_yellow_taxis_parent_server,
    args = list(
      yellow_taxi_data = mock_taxi_data,
      app_metadata     = mock_metadata,
      data_version     = mock_data_version
    ),
    expr = {
      # module is programmed to return NULL if inputs equal Sys.Date()
      session$setInputs(
        di_start_date = Sys.Date(),
        di_end_date   = Sys.Date()
      )

      expect_null(rv_main_date_filtered_date())


      # We set the user start date to Jan 10th.
      # - Daily data on Jan 15th should PASS.
      # - Monthly data on Jan 1st should PASS (because floor_date(Jan 10) = Jan 1).
      # - Weekly data on Jan 8th should PASS (because floor_date(Jan 10, week=7) = Jan 8).
      # - Out of bounds 2022 data should FAIL.
      # - NA dates should PASS.
      session$setInputs(
        di_start_date = as.Date("2023-01-10"),
        di_end_date   = as.Date("2023-01-31")
      )

      result <- rv_main_date_filtered_date()

      # Assertions
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 4) # Rows 1, 2, 3, and 4 should be kept
      expect_false(5 %in% result$id) # Row 5 (2022) must be dropped


      # Push the start date to Feb 1st.
      # Now, the Jan 15th (daily), Jan 1st (monthly), and Jan 8th (weekly) rows
      # should all fail the filter. Only the NA row should remain.
      session$setInputs(
        di_start_date = as.Date("2023-02-01"),
        di_end_date   = as.Date("2023-02-28")
      )

      result_tight <- rv_main_date_filtered_date()

      expect_equal(nrow(result_tight), 1)
      expect_true(is.na(result_tight$date[1])) # Only the NA row survives
    }
  )
})
