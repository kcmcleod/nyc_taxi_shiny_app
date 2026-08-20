################################################################################
### FILE: tests/testthat/test-mod_heatmap.R
################################################################################

library(testthat)
library(shiny)
library(dplyr)

# ==============================================================================
# 1. MOCK DATA
# ==============================================================================
# We need distinct rows for each temporal aggregation tier to prove the UI switches
mock_heatmap_module_data <- tibble::tibble(
  date = as.Date(c(
    "2023-01-01", # Row 1: Monthly Aggregation
    "2023-01-08", # Row 2: Weekly Aggregation
    "2023-01-02", # Row 3: Daily (Combined) Data
    NA, # Row 4: Missing Date (Daily Tier)
    NA # Row 5: Missing Date (Daily Tier)
  )),
  full_month_aggregation = c(TRUE, FALSE, FALSE, FALSE, FALSE),
  full_week_aggregation = c(FALSE, TRUE, FALSE, FALSE, FALSE),
  PULocation = c("Loc_A", "Loc_A", "Loc_B", "Loc_A", "Loc_C"),
  DOLocation = c("Loc_X", "Loc_Y", "Loc_Z", "Loc_X", "Loc_Z"),
  total_number_trips = c(10, 20, 30, 5, 15)
)

# The heatmap module doesn't heavily use app_metadata for filtering,
# but it is passed down the server tree, so we supply a basic mock.
mock_metadata <- list(
  Location = c("Loc_A", "Loc_B", "Loc_C")
)

# ==============================================================================
# 2. TESTS
# ==============================================================================

test_that("mod_yellow_taxis_heatmap_server processes inputs and KPIs correctly", {
  # Mock the reactive dependencies passed from the parent
  mock_filtered_data <- reactiveVal(mock_heatmap_module_data)
  mock_data_version <- reactiveVal(1)
  mock_start_date <- reactiveVal(as.Date("2023-01-01"))
  mock_end_date <- reactiveVal(as.Date("2023-01-31"))

  testServer(
    app = mod_yellow_taxis_heatmap_server,
    args = list(
      filtered_data = mock_filtered_data,
      app_metadata  = mock_metadata,
      data_version  = mock_data_version,
      start_date    = mock_start_date,
      end_date      = mock_end_date
    ),
    expr = {
      # ------------------------------------------------------------------------
      # Test A: Monthly Aggregation Filter
      # ------------------------------------------------------------------------
      session$setInputs(ri_heatmap_period = "Monthly")

      heatmap_data_monthly <- rv_main_heatmap_data()

      # Should only grab Row 1 (full_month_aggregation == TRUE)
      expect_s3_class(heatmap_data_monthly, "data.frame")
      expect_equal(nrow(heatmap_data_monthly), 1)
      expect_equal(heatmap_data_monthly$trips[1], 10)
      expect_true("date_month" %in% names(heatmap_data_monthly))


      # ------------------------------------------------------------------------
      # Test B: Weekly Aggregation Filter
      # ------------------------------------------------------------------------
      session$setInputs(ri_heatmap_period = "Weekly")

      heatmap_data_weekly <- rv_main_heatmap_data()

      # Should only grab Row 2 (full_week_aggregation == TRUE)
      expect_equal(nrow(heatmap_data_weekly), 1)
      expect_equal(heatmap_data_weekly$trips[1], 20)
      expect_true("date_week" %in% names(heatmap_data_weekly))


      # ------------------------------------------------------------------------
      # Test C: Combined (Daily) Filter
      # ------------------------------------------------------------------------
      session$setInputs(ri_heatmap_period = "Combined")

      heatmap_data_combined <- rv_main_heatmap_data()

      # Should grab Rows 3, 4, and 5 (all daily/unaggregated rows, including NAs)
      # Because there are 3 distinct PU/DO combinations (B-Z, A-X, C-Z), we expect 3 rows
      expect_equal(nrow(heatmap_data_combined), 3)
      # Verify the sum of trips across the combined rows (30 + 5 + 15 = 50)
      expect_equal(sum(heatmap_data_combined$trips), 50)
      # Date columns should be completely absent in the "Combined" view
      expect_false("date_month" %in% names(heatmap_data_combined))
      expect_false("date_week" %in% names(heatmap_data_combined))


      # ------------------------------------------------------------------------
      # Test D: Missing Dates KPI Math
      # ------------------------------------------------------------------------
      # Our missing dates logic sums trips where `is.na(date)` is TRUE on the daily tier.
      # In the mock data, Row 4 (5 trips) and Row 5 (15 trips) = 20 total missing.

      # The UI element formats it using scales::comma, so we check for the string "20"
      expect_equal(output$vo_missing_dates, "20")
    }
  )
})
