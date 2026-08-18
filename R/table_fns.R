fn_calculate_table_data <- function(base_data, week_agg, month_agg,
                                    pu_locations, do_locations) {
  if (!inherits(base_data, c("data.frame", "ArrowObject", "arrow_dplyr_query", "Dataset"))) {
    err_msg <- "System Error: The underlying data source is disconnected or invalid."
    log_error(err_msg)
    stop(err_msg)
  }

  if (!is.logical(month_agg) || length(month_agg) != 1) {
    err_msg <- "Input Error: Month aggregation selection is invalid."
    log_error(err_msg)
    stop(err_msg)
  }

  if (!is.logical(week_agg) || length(week_agg) != 1) {
    err_msg <- "Input Error: Week aggregation selection is invalid."
    log_error(err_msg)
    stop(err_msg)
  }

  if (!length(pu_locations) > 0 || !is.character(pu_locations)) {
    err_msg <- "Input Error: Invalid choice for pick up location."
    log_error(err_msg)
    stop(err_msg)
  }

  if (!length(do_locations) > 0 || !is.character(do_locations)) {
    err_msg <- "Input Error: Invalid choice for drop off location."
    log_error(err_msg)
    stop(err_msg)
  }

  date_format_string <- ifelse(month_agg == TRUE, "%b %Y", "%d %b %Y")

  main_data <- base_data |>
    filter(
      full_month_aggregation == month_agg,
      full_week_aggregation == week_agg,
      PULocation %in% pu_locations,
      DOLocation %in% do_locations
    ) |>
    group_by(date) |>
    summarise(
      # not DRY cos arrow doesnt support across or anonymous
      total_number_trips = sum(total_number_trips, na.rm = TRUE),
      total_distance = sum(total_distance, na.rm = TRUE),
      total_passenger_count = sum(total_passenger_count, na.rm = TRUE),
      total_charges = sum(total_charges, na.rm = TRUE),
      total_fare = sum(total_fare, na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect() |>
    arrange(date) |>
    mutate(
      date = format(date, date_format_string),
      date = if_else(is.na(date), "Unknown", date)
    )

  # totals
  metrics <- c(
    "total_number_trips", "total_distance",
    "total_passenger_count", "total_charges", "total_fare"
  )

  totals <- main_data |>
    dplyr::summarise(
      dplyr::across(all_of(metrics), \(x) sum(x, na.rm = TRUE))
    ) |>
    dplyr::mutate(date = "TOTALS")

  renamed_metrics <- c(
    "Date", "Trip Count", "Total Distance (miles)",
    "Passenger Count", "Total Charge", "Total Fare"
  )
  presentation_names <- setNames(c("date", metrics), renamed_metrics)

  final_data <- dplyr::bind_rows(main_data, totals) |>
    rename(all_of(presentation_names))

  return(final_data)
}

################################################################################

#' Generate Yellow Taxi Data Table
#'
#' @description
#' Validates and formats aggregated yellow taxi data, generating an interactive
#' `{DT}` datatable. It automatically pins the "TOTALS" summary row to the HTML
#' table footer (`<tfoot>`), preventing it from being affected by user sorting
#' or pagination.
#'
#' @param base_data A `data.frame` or `tibble` containing the aggregated taxi data.
#'   Must include a summary row where the `Date` column equals `"TOTALS"`.
#'   Requires the following strict column names: `"Date"`, `"Trip Count"`,
#'   `"Total Distance (miles)"`, `"Passenger Count"`, `"Total Charge"`,
#'   and `"Total Fare"`.
#'
#' @return A `datatables` HTML widget ready for UI rendering.
#'
#' @examples
#' \dontrun{
#' my_table <- fn_generate_yellow_taxi_table(final_data)
#' }
fn_generate_yellow_taxi_table <- function(base_data) {
  if (!is.data.frame(base_data)) {
    err_msg <- "Input data must be a data.frame or tibble."
    log_error(err_msg)
    stop(err_msg)
  }

  if (nrow(base_data) == 0) {
    err_msg <- "Input data is empty."
    log_error(err_msg)
    stop(err_msg)
  }

  req_cols <- c(
    "Date", "Trip Count", "Total Distance (miles)",
    "Passenger Count", "Total Charge", "Total Fare"
  )
  missing_cols <- setdiff(req_cols, names(base_data))

  if (length(missing_cols) > 0) {
    err_msg <- (paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    log_error(err_msg)
    stop(err_msg)
  }

  if (!"TOTALS" %in% base_data$Date) {
    err_msg <- "Input data must contain a summary row where Date is 'TOTALS'."
    log_error(err_msg)
    stop(err_msg)
  }

  # BUILD CHART

  # totals will go into footer
  df_data <- base_data[!base_data$Date %in% "TOTALS", ]
  df_totals <- base_data[base_data$Date %in% "TOTALS", ]

  footer_vals <- c(
    "TOTALS",
    scales::comma(df_totals[["Trip Count"]]),
    scales::comma(df_totals[["Total Distance (miles)"]], accuracy = 0.01),
    scales::comma(df_totals[["Passenger Count"]]),
    scales::dollar(df_totals[["Total Charge"]]),
    scales::dollar(df_totals[["Total Fare"]])
  )

  # Create HTML container with footer
  sketch <- htmltools::withTags(table(
    tableHeader(names(df_data)),
    tableFooter(footer_vals)
  ))

  # only show pagination when more than 20 rows
  domString <- "t"
  maxLength <- 10
  if (nrow(df_data) > maxLength) {
    domString <- paste0("p", domString)
  }

  dt_options <- list(
    pageLength = maxLength, lengthMenu = c(5, 10, 15, 20),
    dom = domString
  )

  table <- DT::datatable(df_data,
    rownames = FALSE, options = dt_options,
    container = sketch
  ) |>
    DT::formatRound(columns = c("Trip Count", "Passenger Count"), digits = 0) |>
    DT::formatRound(columns = "Total Distance (miles)", digits = 2) |>
    DT::formatCurrency(columns = c("Total Charge", "Total Fare"), currency = "$")

  return(table)
}
