library(testthat)
library(shiny)
library(dplyr)


mock_main_data <- tibble::tibble(
  date = as.Date(c(
    "2023-01-01", # Normal daily row
    "2023-01-08", # Normal weekly row
    NA, # Missing date row (VTS/Cash)
    NA # Missing date row (CMT/Credit)
  )),
  Vendor = c("VTS", "CMT", "VTS", "CMT"),
  payment_type = c("Cash", "Credit", "Cash", "Credit"),
  full_month_aggregation = c(FALSE, FALSE, FALSE, FALSE),
  full_week_aggregation = c(FALSE, TRUE, FALSE, FALSE),
  total_distance = c(10, 20, 5, 15),
  total_number_trips = c(2, 4, 1, 3)
)

mock_metadata <- list(
  Vendor = c("VTS", "CMT"),
  payment_type = c("Cash", "Credit")
)


test_that("mod_yellow_taxis_main_server processes inputs and KPIs correctly", {
  # Mock the reactive dependencies
  mock_filtered_data <- reactiveVal(mock_main_data)
  mock_data_version <- reactiveVal(1)
  mock_start_date <- as.Date("2023-01-01")
  mock_end_date <- as.Date("2023-01-31")

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
        pi_level = "Week",
        pi_vendor = c("CMT"),
        pi_pay_type = c("Credit"),
        rb_journeys_or_passengers = "Journey Count"
      )

      # Access the internal reactive
      chart_data <- rv_main_chart_filtered_data()

      # Verify it pulled the weekly CMT/Credit row
      expect_s3_class(chart_data, "data.frame")
      expect_equal(nrow(chart_data), 1)
      expect_equal(chart_data$total_value[1], 4) # 4 trips


      # We select VTS and Cash. The mock data has 1 NA row for this combo with 1 trip.
      session$setInputs(
        pi_level = "Day",
        pi_vendor = c("VTS"),
        pi_pay_type = c("Cash"),
        rb_journeys_or_passengers = "Journey Count" # Note: Your UI config uses "Journey Count"
      )

      # Test the KPI Title renders correctly
      expect_equal(output$vo_kpi_title, "Total Journeys with Unknown Dates")

      # Test the missing dates math sums the trips (should be "1")
      expect_equal(output$vo_missing_dates, "1")


      # Now we toggle the Y-axis to distance for the exact same filters
      session$setInputs(
        rb_journeys_or_passengers = "Total Distance"
      )

      # Test the KPI Title switches correctly
      expect_equal(output$vo_kpi_title, "Total Distance with Unknown Dates (miles)")

      # Test the missing dates math switches to sum distance (should be "5")
      expect_equal(output$vo_missing_dates, "5")


      session$setInputs(
        pi_vendor = c("VTS", "CMT"),
        pi_pay_type = c("Cash", "Credit")
      )

      # Now both NA rows should be summed.
      # Distance: 5 (VTS) + 15 (CMT) = 20
      expect_equal(output$vo_missing_dates, "20")
    }
  )
})
