library(testthat)
library(shiny)
library(dplyr)
library(lubridate)

mock_parent_data <- tibble::tibble(
  id = 1:4,
  Year = c(2023L, 2023L, 2023L, 2022L),
  date = as.Date(c(
    "2023-01-15", # Daily data (Middle of Jan)
    "2023-01-01", # Monthly data (1st of Jan)
    "2023-01-08", # Weekly data (Starts on a Sunday)
    "2022-01-01" # Out of bounds (Should be filtered out)
  )),
  full_month_aggregation = c(FALSE, TRUE, FALSE, FALSE),
  full_week_aggregation = c(FALSE, FALSE, TRUE, FALSE)
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
      # We set the user start date to Jan 10th.
      # - Daily data on Jan 15th should PASS.
      # - Monthly data on Jan 1st should PASS (because floor_date(Jan 10) = Jan 1).
      # - Weekly data on Jan 8th should PASS (because floor_date(Jan 10, week=7) = Jan 8).
      # - Out of bounds 2022 data should FAIL.
      # - NA dates should PASS.
      session$setInputs(
        di_date_range = c(as.Date("2023-01-10"), as.Date("2023-01-31"))
      )

      result <- rv_main_date_filtered_data()

      # Assertions
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 3) # Rows 1, 2, & 3 should be kept
      expect_false(4 %in% result$id) # Row 4 (2022) must be dropped


      # Push the start date to Feb 1st.
      # Now, the Jan 15th (daily), Jan 1st (monthly), and Jan 8th (weekly) rows
      # should all fail the filter.Nothing should remain
      session$setInputs(
        di_date_range = c(as.Date("2023-02-01"), as.Date("2023-02-28"))
      )

      result_tight <- rv_main_date_filtered_data()

      expect_equal(nrow(result_tight), 0) # No NAs
    }
  )
})
