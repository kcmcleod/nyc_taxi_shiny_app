library(testthat)
library(shiny)
library(dplyr)

mock_main_data <- tibble::tibble(
  date = as.Date(c(
    "2023-01-01", # Daily row 1 (VTS/Cash)
    "2023-01-01", # Daily row 2 (CMT/Credit) - Same day to test grouping!
    "2023-01-08", # Weekly row
    NA, # Missing date row (VTS/Cash)
    NA # Missing date row (CMT/Credit)
  )),
  Vendor = c("VTS", "CMT", "CMT", "VTS", "CMT"),
  payment_type = c("Cash", "Credit", "Credit", "Cash", "Credit"),
  full_month_aggregation = c(FALSE, FALSE, FALSE, FALSE, FALSE),
  full_week_aggregation = c(FALSE, FALSE, TRUE, FALSE, FALSE),
  total_distance = c(10, 15, 20, 5, 15),
  total_number_trips = c(2, 3, 4, 1, 3)
)

mock_metadata <- list(
  Vendor = c("VTS", "CMT"),
  payment_type = c("Cash", "Credit")
)

# ==============================================================================
# 2. TESTS
# ==============================================================================

test_that("mod_yellow_taxis_main_server processes inputs, splits, and KPIs correctly", {
  # Mock the reactive dependencies
  mock_filtered_data <- reactiveVal(mock_main_data)
  mock_data_version <- reactiveVal(1)
  mock_start_date <- reactiveVal(as.Date("2023-01-01"))
  mock_end_date <- reactiveVal(as.Date("2023-01-31"))

  testServer(
    app = mod_yellow_taxis_main_server,
    args = list(
      filtered_data = mock_filtered_data,
      app_metadata  = mock_metadata,
      data_version  = mock_data_version,
      start_date    = mock_start_date,
      end_date      = mock_end_date
    ),
    expr = {
      session$setInputs(
        pi_level = "Day",
        pi_vendor = c("VTS", "CMT"),
        pi_pay_type = c("Cash", "Credit"),
        rb_split_by = "Total",
        rb_journeys_or_passengers = "Journey Count"
      )

      chart_data_total <- rv_main_chart_filtered_data()
      # due to mock data we have NAs so filter them out
      chart_data_total_valid <- chart_data_total[!is.na(chart_data_total$date), ]

      # Both Jan 1st rows should be summed into a single row
      expect_equal(nrow(chart_data_total_valid), 1)
      expect_equal(chart_data_total_valid$total_value[1], 5) # 2 trips (VTS) + 3 trips (CMT) = 5
      expect_false("Vendor" %in% names(chart_data_total_valid)) # Grouping column shouldn't exist


      session$setInputs(rb_split_by = "Vendor")

      chart_data_vendor <- rv_main_chart_filtered_data()
      chart_data_vendor_valid <- chart_data_vendor[!is.na(chart_data_vendor$date), ]

      # Jan 1st should now be split into two separate rows
      expect_equal(nrow(chart_data_vendor_valid), 2)
      expect_true("Vendor" %in% names(chart_data_vendor_valid))
      expect_equal(chart_data_vendor_valid$total_value[chart_data_vendor_valid$Vendor == "VTS"], 2)
      expect_equal(chart_data_vendor_valid$total_value[chart_data_vendor_valid$Vendor == "CMT"], 3)


      session$setInputs(
        pi_vendor = c("VTS"),
        pi_pay_type = c("Cash")
      )

      # Test the KPI Title renders correctly
      expect_equal(output$vo_kpi_title, "Total Journeys with Unknown Dates")
      # Test the missing dates math sums the trips (should be "1")
      expect_equal(output$vo_missing_dates, "1")

      session$setInputs(
        rb_journeys_or_passengers = "Total Distance",
        pi_vendor = c("VTS", "CMT"),
        pi_pay_type = c("Cash", "Credit")
      )

      # Test the KPI Title switches correctly
      expect_equal(output$vo_kpi_title, "Total Distance with Unknown Dates (miles)")
      # Now both NA rows should be summed. Distance: 5 (VTS) + 15 (CMT) = 20
      expect_equal(output$vo_missing_dates, "20")
    }
  )
})
