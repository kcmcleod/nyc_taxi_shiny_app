library(testthat)
library(dplyr)
library(logger)

# Neutralize all logger levels inside the covr sandbox so namespace lookups don't crash
if (Sys.getenv("R_COVR") == "true") {
  log_info <- function(...) invisible()
  log_warn <- function(...) invisible()
  log_error <- function(...) invisible()
  log_debug <- function(...) invisible()
  log_fatal <- function(...) invisible()
}


test_that("fn_validate_columns triggers all defensive and warning branches", {
  df <- data.frame(col_a = 1, col_b = 2, col_c = 3)

  # 1. Non-dataframe input
  expect_false(fn_validate_columns("not_a_df", c("col_a"), "Test"))

  # 2. Missing expected_cols vector
  expect_false(fn_validate_columns(df, NULL, "Test"))

  # 3. Missing column (triggers error log and returns FALSE)
  expect_false(fn_validate_columns(df, c("col_a", "col_missing"), "Test"))

  # 4. Extra columns present (triggers log_warn for 'col_c' and returns TRUE)
  expect_true(fn_validate_columns(df, c("col_a", "col_b"), "Test"))
})


test_that("fn_perform_lookup catches non-dataframes and introduced NAs", {
  df <- data.frame(id = c(1, 2, 3), val = c("a", "b", "c"))
  lookup <- data.frame(id = c(1, 2), name = c("One", "Two")) # ID 3 is missing!

  # 1. Non-dataframe input
  expect_error(
    fn_perform_lookup("bad_input", lookup, "id", "id", "name", "Test"),
    regexp = "must be a data frame"
  )

  # 2. Trigger the log_error branch for new NAs (ID 3 won't match)
  result <- fn_perform_lookup(df, lookup, "id", "id", "name", "Test")
  expect_true(is.na(result$name[3]))
})


test_that("fn_monthly_aggregation defensive guardrails execute", {
  # 1. Non-dataframe & empty dataframe
  expect_null(fn_monthly_aggregation("not_a_df"))
  expect_null(fn_monthly_aggregation(data.frame()))

  # 2. Missing required financial/distance columns
  bad_df <- data.frame(VendorID = 1)
  expect_null(fn_monthly_aggregation(bad_df))

  # 3. Valid schema but missing requested grouping column
  #' ToDO
})


test_that("fn_process_yellow_file aborts on invalid filename or column validation failure", {
  # Filename without a YYYY-MM date string
  expect_null(fn_process_yellow_file("bad_file_name_without_date.parquet"))
})
