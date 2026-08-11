################################################################################
### FILE: tests/testthat/test_fn_enrich_yellow_data.R
################################################################################

library(testthat)
library(dplyr)
library(logger)

# Silence loggers during testthat / covr execution
if (Sys.getenv("R_COVR") == "true") {
  if (exists("log_info", mode = "function")) log_info <- function(...) invisible()
  if (exists("log_error", mode = "function")) log_error <- function(...) invisible()
}

# ------------------------------------------------------------------------------
# 1. SETUP: Tiny In-Memory Mock Lookups
# ------------------------------------------------------------------------------
mock_zone_lookups <- tibble::tibble(
  LocationID = c(1, 2),
  Borough    = c("Manhattan", "Queens")
)

mock_rate_lookups <- tibble::tibble(
  RatecodeID = c(1, 2),
  Rate       = c("Standard rate", "JFK")
)

mock_payment_lookups <- tibble::tibble(
  payment_type  = c("1", "2"),
  payment_class = c("Credit Card", "Cash")
)

mock_vendor_lookups <- tibble::tibble(
  VendorID = c("1", "2"),
  Vendor   = c("Creative Mobile Technologies", "VeriFone Inc")
)

# ------------------------------------------------------------------------------
# 2. SETUP: Tiny In-Memory Trip Dataset
# ------------------------------------------------------------------------------
mock_trips <- tibble::tibble(
  date               = as.Date("2023-01-01"),
  RatecodeID         = 1,
  PULocationID       = 1,
  DOLocationID       = 2,
  payment_type       = "1",
  VendorID           = "2",
  total_number_trips = 10
)

# ==============================================================================
# TESTS
# ==============================================================================

test_that("fn_perform_joins correctly joins lookups and maps new columns", {
  result <- fn_perform_joins(
    df              = mock_trips,
    zone_lookups    = mock_zone_lookups,
    rate_lookups    = mock_rate_lookups,
    payment_lookups = mock_payment_lookups,
    vendor_lookups  = mock_vendor_lookups
  )

  # 1. Verify structure and expected column additions
  expect_s3_class(result, "data.frame")
  expect_true(all(c("PULocation", "DOLocation", "Rate", "Vendor") %in% names(result)))

  # 2. Verify exact mapped values
  expect_equal(result$PULocation[1], "Manhattan")
  expect_equal(result$DOLocation[1], "Queens")
  expect_equal(result$Rate[1], "Standard rate")
  expect_equal(result$Vendor[1], "VeriFone Inc")

  # 3. Verify payment_type was overridden with payment_class text
  expect_equal(result$payment_type[1], "Credit Card")
  expect_false("payment_class" %in% names(result)) # Ensure temp column was dropped
})

test_that("fn_perform_joins returns NULL when passed an empty dataframe", {
  empty_df <- mock_trips[0, ]

  result <- fn_perform_joins(
    df              = empty_df,
    zone_lookups    = mock_zone_lookups,
    rate_lookups    = mock_rate_lookups,
    payment_lookups = mock_payment_lookups,
    vendor_lookups  = mock_vendor_lookups
  )

  expect_null(result)
})

test_that("fn_perform_joins halts when a join fails validation", {
  # Create a malformed lookup table missing the expected target column
  bad_vendor_lookups <- tibble::tibble(
    VendorID = c("1", "2"),
    WrongCol = c("A", "B")
  )

  expect_error(
    fn_perform_joins(
      df              = mock_trips,
      zone_lookups    = mock_zone_lookups,
      rate_lookups    = mock_rate_lookups,
      payment_lookups = mock_payment_lookups,
      vendor_lookups  = bad_vendor_lookups
    ),
    regexp = "Lookup join verification failed"
  )
})
