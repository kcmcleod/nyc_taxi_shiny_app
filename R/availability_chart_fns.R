fn_calculate_availability_data <- function(base_data, granularity, config, start_date, end_date) {
  if (!inherits(base_data, c("data.frame", "ArrowObject", "arrow_dplyr_query", "Dataset"))) {
    err_msg <- "System Error: The underlying data source is disconnected or invalid."
    log_error(err_msg)
    stop(err_msg)
  }

  # check cols
  required_cols <- c("date", "total_trips")
  missing_cols <- setdiff(required_cols, names(base_data))

  if (length(missing_cols) > 0) {
    err_msg <- sprintf(
      "Data Error: The dataset is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    )
    log_error(err_msg)
    stop(err_msg)
  }

  valid_granularity_levels <- config$ui_values$pi_level
  if (!granularity %in% valid_granularity_levels) {
    err_msg <- sprintf("Input Error: Invalid granularity: %s", granularity)
    log_error(err_msg)
    stop(err_msg)
  }

  if (!lubridate::is.Date(start_date) || is.na(start_date)) {
    err_msg <- sprintf("Input Error: Invalid start date supplied: %s", start_date)
    log_error(err_msg)
    stop(err_msg)
  }

  if (!lubridate::is.Date(end_date) || is.na(end_date)) {
    err_msg <- sprintf("Input Error: Invalid end date supplied: %s", end_date)
    log_error(err_msg)
    stop(err_msg)
  }

  ##########################################################################

  month_agg <- ifelse(granularity == "Month", TRUE, FALSE)
  week_agg <- ifelse(granularity == "Week", TRUE, FALSE)

  chart_data <- base_data

  if (week_agg) {
    chart_data$date <- floor_date(chart_data$date, unit = "week", week_start = 7)
  } else if (month_agg) {
    chart_data$date <- floor_date(chart_data$date, unit = "month")
  }

  # filter by dates... which may or may not be the user selected ones
  chart_data <- chart_data |>
    filter(date >= start_date & date <= end_date)

  if (week_agg || month_agg) {
    chart_data <- chart_data |>
      group_by(date) |>
      summarise(total_trips = sum(total_trips), .groups = "drop")
  }

  return(chart_data)
}


################################################################################

fn_generate_availability_chart <- function(chart_data, granularity, config) {
  if (!inherits(chart_data, "data.frame")) {
    err_msg <- "System Error: The underlying data source must be collected into memory before plotting."
    log_error(err_msg)
    stop(err_msg)
  }

  valid_granularity_levels <- config$ui_values$pi_level
  if (!granularity %in% valid_granularity_levels) {
    err_msg <- sprintf("Input Error: Invalid granularity: %s", granularity)
    log_error(err_msg)
    stop(err_msg)
  }

  x_lab_format <- switch(granularity,
    "Month" = "%b %Y",
    "%d %b %Y"
  )

  if (granularity == "Week") {
    # Isolate the first week of the month
    keep_labels <- lubridate::day(chart_data[["date"]]) <= 7
  } else if (granularity == "Day") {
    # Isolate the 1st of the month
    keep_labels <- lubridate::day(chart_data[["date"]]) == 1
  } else {
    # Keep everything for Monthly view
    keep_labels <- rep(TRUE, nrow(chart_data))
  }

  # Extract ONLY the values and text for those specific dates
  custom_tick_vals <- chart_data[["date"]][keep_labels]
  custom_tick_text <- format(custom_tick_vals, x_lab_format)

  plot_ly(chart_data,
    x = ~date, y = ~total_trips, type = "bar",
    marker = list(color = config$colours$theme$primary_accent)
  ) |>
    layout(
      title = "Data Availability",
      hovermode = "x unified",
      xaxis = list(
        title = "Journey date",
        tickmode = "array",
        tickvals = custom_tick_vals,
        ticktext = custom_tick_text,
        tickangle = 90
      ),
      yaxis = list(
        title = "Number of trips"
      )
    )
}
