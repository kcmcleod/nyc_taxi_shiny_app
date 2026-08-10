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
      total_distance = sum(trip_distance),
      total_charges = sum(total_amount),
      total_fare = sum(fare_amount),
      total_tip = sum(tip_amount),
      total_other_charges = sum(other_charges),
      total_passenger_count = sum(passenger_count),
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

    # date just needs to be the YM so grab that from any row
    currentDate <- format(df$trip_start_date[1], "%Y-%m")

    tmp |>
      mutate(
        date = ym(currentDate),
        full_month_aggregation = TRUE,
        full_week_aggregation = FALSE
      ) |>
      select(full_month_aggregation, full_week_aggregation, date, everything())
  }
}
