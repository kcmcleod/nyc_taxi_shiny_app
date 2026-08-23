#' Aggregate NYC Taxi Trip Data
#'
#' @description Aggregates cleaned NYC taxi trip data by calculating total volumes,
#' distances, and financial metrics across specified grouping variables. It dynamically
#' determines the temporal granularity (daily, weekly, or full month) based on the
#' presence of specific date columns in the grouping variables and applies standardised
#' aggregation flags.
#'
#' @param df A data frame or tibble containing the cleaned taxi trip data. Must
#'   include standard financial, distance, and passenger count columns.
#' @param grouping_values A character vector specifying the columns to group the
#'   data by (e.g., \code{c("VendorID", "payment_type", "trip_start_date")}).
#'   Defaults to an empty vector \code{c()}.
#'
#' @details
#' The function performs several internal checks before calculation:
#' \itemize{
#'   \item Validates that the input is a populated data frame.
#'   \item Ensures all required financial columns (e.g., \code{mta_tax}, \code{tolls_amount},
#'     \code{Airport_fee}) are present to securely calculate \code{total_other_charges}.
#'   \item Verifies that all requested \code{grouping_values} exist in the input data.
#' }
#'
#' The output date formatting and \code{full_month_aggregation} / \code{full_week_aggregation}
#' flags are dynamically assigned. If no temporal columns (\code{trip_start_date} or
#' \code{trip_start_week}) are passed in the grouping values, the function assumes a full
#' monthly aggregation and falls back to using the first value of \code{df$reporting_month}.
#'
#' @return A summarised tibble ordered by date. The output includes standard prefix columns
#'   (\code{full_month_aggregation}, \code{full_week_aggregation}, \code{date}), followed by
#'   the provided grouping variables and the calculated metric totals. Returns \code{NULL}
#'   if input validation fails.
#'
fn_monthly_aggregation <- function(df, grouping_values = c()) {
  # 1. Input validation
  if (!is.data.frame(df)) {
    log_error(".... Input 'df' must be a data frame or tibble.")
    return(NULL)
  }

  if (nrow(df) == 0) {
    log_error(".... Input 'df' is empty.")
    return(NULL)
  }

  # 2. Define required columns for calculation
  required_cols <- c(
    "extra", "mta_tax", "tolls_amount", "improvement_surcharge",
    "congestion_surcharge", "Airport_fee", "cbd_congestion_fee",
    "trip_distance", "total_amount", "fare_amount", "tip_amount",
    "passenger_count"
  )

  # Check if all required columns exist
  missing_required <- setdiff(required_cols, names(df))
  if (length(missing_required) > 0) {
    log_error(
      ".... missing required columns in input data frame: ",
      paste0(missing_required, collapse = ", ")
    )
    return(NULL)
  }

  # Check if grouping values exist
  missing_grouping <- setdiff(grouping_values, names(df))
  if (length(missing_grouping) > 0) {
    log_error(
      ".... grouping columns not found in data frame: ",
      paste0(missing_grouping, collapse = ", ")
    )
    return(NULL)
  }

  # 3. actual aggregation
  tmp <- group_by(df, across(all_of(grouping_values))) |>
    mutate(other_charges = extra + mta_tax + tolls_amount +
      improvement_surcharge + congestion_surcharge +
      Airport_fee + cbd_congestion_fee) |>
    summarise(
      total_number_trips = n(),
      total_distance = sum(trip_distance, na.rm = TRUE),
      total_charges = sum(total_amount, na.rm = TRUE),
      total_fare = sum(fare_amount, na.rm = TRUE),
      total_tip = sum(tip_amount, na.rm = TRUE),
      total_other_charges = sum(other_charges, na.rm = TRUE),
      total_passenger_count = sum(passenger_count, na.rm = TRUE),
      .groups = "drop"
    )

  if ("trip_start_date" %in% names(tmp)) {
    tmp |>
      rename(date = "trip_start_date") |>
      mutate(
        full_month_aggregation = FALSE,
        full_week_aggregation = FALSE
      ) |>
      select(full_month_aggregation, full_week_aggregation, date, everything()) |>
      arrange(date)
  } else if ("trip_start_week" %in% names(tmp)) {
    tmp |>
      rename(date = "trip_start_week") |>
      mutate(
        date = ymd(date),
        full_month_aggregation = FALSE,
        full_week_aggregation = TRUE
      ) |>
      select(full_month_aggregation, full_week_aggregation, date, everything()) |>
      arrange(date)
  } else {
    if (!"trip_start_date" %in% names(df)) {
      log_error(".... column 'trip_start_date' is required in the original
                data frame for monthly fallback aggregation.")
      return(NULL)
    }

    tmp |>
      mutate(
        date = ymd(df$reporting_month[1]),
        full_month_aggregation = TRUE,
        full_week_aggregation = FALSE
      ) |>
      select(full_month_aggregation, full_week_aggregation, date, everything())
  }
}
