library(testthat)
library(shiny)
library(dplyr)

mock_main_data <- tibble::tibble(
  date = as.Date(c(
    "2023-01-01", # Daily row 1 (VTS/Cash)
    "2023-01-01", # Daily row 2 (CMT/Credit) - Same day to test grouping!
    "2023-01-08" # Weekly row
  )),
  Vendor = c("VTS", "CMT", "CMT"),
  payment_type = c("Cash", "Credit", "Credit"),
  full_month_aggregation = c(FALSE, FALSE, FALSE),
  full_week_aggregation = c(FALSE, FALSE, TRUE),
  total_distance = c(10, 15, 20),
  total_number_trips = c(2, 3, 4)
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
      session$elapse(800)
      chart_data_vendor <- rv_main_chart_filtered_data()
      chart_data_vendor_valid <- chart_data_vendor[!is.na(chart_data_vendor$date), ]

      # Jan 1st should now be split into two separate rows
      expect_equal(nrow(chart_data_vendor_valid), 2)
      print(names(chart_data_vendor_valid))
      expect_true("Vendor" %in% names(chart_data_vendor_valid))
      expect_equal(chart_data_vendor_valid$total_value[chart_data_vendor_valid$Vendor == "VTS"], 2)
      expect_equal(chart_data_vendor_valid$total_value[chart_data_vendor_valid$Vendor == "CMT"], 3)
    }
  )
})


test_that("mod_yellow_taxis_main_server tryCatch safely handles data failures", {
  # 1. Create a sabotaged reactive that simulates a fatal pipeline error
  mock_crashing_data <- reactive({
    stop("Simulated Database Timeout")
  })

  mock_metadata <- list(
    Vendor = c("VTS", "CMT"),
    payment_type = c("Cash", "Credit")
  )

  testServer(
    app = mod_yellow_taxis_main_server,
    args = list(
      filtered_data = mock_crashing_data,
      app_metadata  = mock_metadata,
      data_version  = reactiveVal(1),
      start_date    = reactiveVal(as.Date("2023-01-01")),
      end_date      = reactiveVal(as.Date("2023-01-31"))
    ),
    expr = {
      # 2. Set required inputs to trigger the chart render
      session$setInputs(
        pi_level = "Day",
        pi_vendor = "VTS",
        pi_pay_type = "Cash",
        rb_split_by = "Total",
        rb_journeys_or_passengers = "Journey Count"
      )

      # 3. Evaluate the output.
      # The tryCatch will swallow the "Simulated Database Timeout",
      # fire the toastr message, and assign NULL to base_data.
      # The immediate validate() block then takes over and throws a safe Shiny silent error.
      err <- expect_error(output$po_tripVolumesOverTime)

      # 4. Verify the server did not crash and correctly surfaced the validation message
      expect_match(err$message, "Chart unavailable due to data error")
    }
  )
})
