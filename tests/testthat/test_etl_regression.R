################################################################################
### FILE: tests/testthat/test_etl_regression.R
################################################################################

library(testthat)
library(diffdf)
library(dplyr)
library(readr)
library(logger)


test_that("End-to-End ETL pipeline (aggregations + lookups) matches frozen golden baseline", {
  # Locate frozen input and baseline files dynamically
  golden_files <- list.files(
    path = testthat::test_path("..", "testdata"),
    pattern = "\\d{4}-\\d{2}_golden_input_yellow\\.parquet$",
    full.names = TRUE
  )
  skip_if(length(golden_files) == 0, "Golden baseline parquet file not found in testdata/.")

  golden_input_file <- golden_files[1]
  golden_output_file <- testthat::test_path("..", "testdata", "golden_output_yellow.rds")
  skip_if_not(file.exists(golden_output_file), "Golden baseline RDS file not found in testdata/.")

  # Step A: Run core temporal aggregations
  combinedDF <- fn_process_yellow_file(golden_input_file)
  expect_false(is.null(combinedDF), label = "fn_process_yellow_file returned NULL on golden input")

  sort_cols <- c("full_month_aggregation", "full_week_aggregation", "date")
  combinedDF <- arrange(combinedDF, across(all_of(sort_cols)))

  # Step B: Perform lookup enrichment joins against live CSV lookup tables
  lookup_dir <- testthat::test_path("..", "testdata", "lookups")

  # Ensure CI skips gracefully if someone clones without test fixtures
  skip_if_not(
    dir.exists(lookup_dir),
    "Lookup CSV fixtures not found in tests/testdata/lookups/."
  )

  zone_lookups <- read_csv(file.path(lookup_dir, "taxi_zone_lookup.csv"),
    show_col_types = FALSE
  ) |>
    select(LocationID, Borough)
  rate_lookups <- read_csv(file.path(lookup_dir, "rate_code_lookup.csv"),
    col_types = "ic", show_col_types = FALSE
  )
  payment_lookups <- read_csv(file.path(lookup_dir, "payment_type_lookup.csv"),
    col_types = "ic", show_col_types = FALSE
  )
  vendor_lookups <- read_csv(file.path(lookup_dir, "vendor_lookup.csv"),
    col_types = "ic", show_col_types = FALSE
  )

  new_output <- fn_perform_lookup(
    combinedDF, rate_lookups, "RatecodeID",
    "RatecodeID", "Rate", "rate"
  ) |>
    fn_perform_lookup(zone_lookups, c("PULocationID" = "LocationID"), "PULocationID",
      "PULocation", "PU",
      post_process_fn = function(df) rename(df, PULocation = Borough)
    ) |>
    fn_perform_lookup(zone_lookups, c("DOLocationID" = "LocationID"), "DOLocationID",
      "DOLocation", "DO",
      post_process_fn = function(df) rename(df, DOLocation = Borough)
    ) |>
    fn_perform_lookup(payment_lookups, "payment_type", "payment_type", "payment_type",
      "payment",
      post_process_fn = function(df) mutate(df, payment_type = payment_class)
    ) |>
    select(-c(payment_class)) |>
    fn_perform_lookup(vendor_lookups, "VendorID", "VendorID", "Vendor", "vendor")

  # Step C: Assert lookup join integrity
  all_names <- names(new_output)
  expected_names <- c("PULocation", "DOLocation", "Rate", "Vendor")
  join_results <- sapply(expected_names, function(field) {
    fn_test_lookup_join(all_names, field)
  })
  expect_true(all(join_results), label = "One or more lookup joins failed during
              ETL regression testing")

  # Step D: Compare enriched output against frozen golden baseline using diffdf
  expected_output <- readRDS(golden_output_file)

  # Primary keys cover both raw IDs and enriched names across all temporal tiers
  key_cols <- c(
    "full_month_aggregation", "full_week_aggregation", "date",
    "VendorID", "payment_type", "RatecodeID", "PULocationID", "DOLocationID"
  )

  qc_diff <- diffdf::diffdf(
    base = expected_output,
    compare = new_output,
    keys = key_cols,
    suppress_warnings = TRUE
  )

  # Print detailed SAS-style audit report to console if a regression occurs
  if (diffdf::diffdf_has_issues(qc_diff)) {
    print(qc_diff)
  }

  expect_false(
    diffdf::diffdf_has_issues(qc_diff),
    label = "ETL pipeline regression detected: enriched output diverged from
    golden baseline"
  )
})
