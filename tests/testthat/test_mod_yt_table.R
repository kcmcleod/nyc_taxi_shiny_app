library(testthat)
library(shiny)

mock_raw_table_data <- tibble::tibble(
  date = as.Date(c("2023-01-01", "2023-01-01", "2023-02-01", "2023-03-01")),
  full_month_aggregation = c(TRUE, TRUE, TRUE, FALSE),
  full_week_aggregation = c(FALSE, FALSE, FALSE, TRUE),
  total_number_trips = c(10, 20, 30, 40),
  total_distance = c(5.5, 4.5, 10.0, 20.0),
  total_passenger_count = c(1, 2, 3, 4),
  total_charges = c(15.0, 25.0, 50.0, 100.0),
  total_fare = c(10.0, 20.0, 40.0, 80.0),
  PULocation = c("EWR", "EWR", "EWR", "EWR"),
  DOLocation = c("EWR", "EWR", "EWR", "EWR")
)

mock_metadata <- list(Location = c("EWR", "Manhattan", "Queens"))

test_that("mod_yellow_taxis_table_server processes reactive inputs correctly", {
  # 1. Mock the reactive dependencies passed from the parent module
  mock_filtered_data <- reactiveVal(mock_raw_table_data)
  mock_data_version <- reactiveVal(12345)
  mock_start_date <- reactiveVal(as.Date("2023-01-01"))
  mock_end_date <- reactiveVal(as.Date("2023-03-31"))

  # 2. Spin up the isolated module server environment
  testServer(
    app = mod_yellow_taxis_table_server,
    args = list(
      filtered_data = mock_filtered_data,
      app_metadata  = mock_metadata,
      data_version  = mock_data_version,
      start_date    = mock_start_date,
      end_date      = mock_end_date
    ),
    expr = {
      session$setInputs(
        pi_level = "Month",
        pi_pu_locations = "EWR",
        pi_do_locations = "EWR"
      )

      calculated_data <- rv_table_filtered_data()

      expect_s3_class(calculated_data, "data.frame")
      expect_equal(nrow(calculated_data), 3)
      expect_equal(calculated_data$Date[1], "Jan 2023")

      session$setInputs(pi_pu_locations = "Manhattan")
      session$elapse(800)
      updated_data <- rv_table_filtered_data()
      expect_equal(nrow(updated_data), 1) # Only the zeroes TOTALS row should remain

      session$setInputs(pi_pu_locations = "EWR")
      session$elapse(800)
      updated_data <- rv_table_filtered_data()
      expect_equal(nrow(updated_data), 3)
      expect_equal(pull(updated_data[3, 2]), 60)
    }
  )
})
