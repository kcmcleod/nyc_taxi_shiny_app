#' Calculate line chart data
#'
#' @description Filters and aggregates NYC taxi data based on user selections for
#' vendor, payment type, and temporal aggregation. It leverages Arrow for lazy
#' evaluation on disk before pulling the grouped data into memory to calculate the final sum.
#'
#' @param base_data An Arrow dataset or standard data frame containing the raw taxi data.
#' @param vendor_list A character vector of vendors to include in the filter.
#' @param payment_list A character vector of payment types to include in the filter.
#' @param month_agg Logical. If `TRUE`, filters for data pre-aggregated at the monthly level.
#' @param week_agg Logical. If `TRUE`, filters for data pre-aggregated at the weekly level.
#' @param data_field A character string of the metric to aggregate (e.g.,
#' "total_distance" or "total_number_trips").
#' @param split_by A character string of the metric to create lines for
#' (e.g., "Payment Type", "Vendor", "Total").
#'
#' @return A tibble with two columns: `date` and `total_value`, containing the aggregated totals.
#'
#' @export
fn_calculate_line_chart_data <- function(base_data, vendor_list, payment_list,
                                         month_agg, week_agg, data_field, split_by) {
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

  if (!is.character(data_field) || length(data_field) != 1) {
    err_msg <- "Input Error: Selected metric is invalid."
    log_error(err_msg)
    stop(err_msg)
  }

  if (!is.character(split_by) || length(split_by) != 1 ||
    !split_by %in% c("Payment Type", "Vendor", "Total")) {
    err_msg <- "Input Error: Selected line type is invalid."
    log_error(err_msg)
    stop(err_msg)
  }

  # check cols
  required_cols <- c(
    "date", "Vendor", "payment_type",
    "full_month_aggregation", "full_week_aggregation", data_field
  )
  missing_cols <- setdiff(required_cols, names(base_data))

  if (length(missing_cols) > 0) {
    err_msg <- sprintf(
      "Data Error: The dataset is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    )

    log_error(err_msg)
    stop(err_msg)
  }

  if (split_by == "Total") {
    split_col <- ""
  } else {
    split_col <- ifelse(split_by == "Vendor", "Vendor", "payment_type")
  }

  base_data |>
    filter(
      full_month_aggregation == month_agg,
      full_week_aggregation == week_agg,
      Vendor %in% vendor_list,
      payment_type %in% payment_list
    ) |>
    select(date, !!sym(split_col), all_of(data_field)) |>
    group_by(date, !!sym(split_col)) |>
    summarise(
      total_value = sum(!!sym(data_field), na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect() |>
    arrange(date)
}

################################################################################

#' Generate Main Line Chart
#'
#' @description Constructs an interactive plotly line chart to visualise time-series
#' trip data. Includes defensive traps to gracefully handle missing data, invalid
#' configurations, or incorrect inputs within a Shiny application without crashing the server.
#'
#' @param base_data An in-memory data.frame or tibble containing the aggregated taxi data.
#' @param granularity A character string indicating the time aggregation level
#' (e.g., "Day", "Week", "Month"). Must be permitted by `config$ui_values$pi_level`.
#' @param x_col A character string specifying the column for the x-axis (typically dates).
#' @param y_col A character string specifying the column for the y-axis (metric volumes).
#' @param title A character string for the chart's main title.
#' @param y_lab_title A character string for the y-axis label.
#' @param config A list containing application configuration settings, used to validate
#' the `granularity` input.
#'
#' @return A plotly htmlwidget object ready to be rendered in a Shiny UI.
fn_generate_main_line_chart <- function(base_data, granularity, x_col, y_col,
                                        title, y_lab_title, split_by, config) {
  if (!inherits(base_data, "data.frame")) {
    err_msg <- "System Error: The underlying data source is disconnected or invalid."
    log_error(err_msg)
    stop(err_msg)
  }

  if (!is.character(title) || length(title) != 1) {
    err_msg <- "System Error: Please supply a valid title for the chart"
    log_error(err_msg)
    stop(err_msg)
  }

  if (!is.character(y_lab_title) || length(y_lab_title) != 1) {
    err_msg <- "System Error: Please supply a valid title for the chart's y-axis"
    log_error(err_msg)
    stop(err_msg)
  }

  if (is.null(config$ui_values$pi_level) || !granularity %in% config$ui_values$pi_level) {
    err_msg <- "System Error: Selected granularity is not permitted by the configuration."
    log_error(err_msg)
    stop(err_msg)
  }

  if (!is.character(split_by) || length(split_by) != 1 ||
    !split_by %in% c("Payment Type", "Vendor", "Total")) {
    err_msg <- "Input Error: Selected line type is invalid."
    log_error(err_msg)
    stop(err_msg)
  }

  # check cols
  required_cols <- c(x_col, y_col)
  missing_cols <- setdiff(required_cols, colnames(base_data))

  if (length(missing_cols) > 0) {
    err_msg <- sprintf(
      "Data Error: The dataset is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    )

    log_error(err_msg)
    stop(err_msg)
  }

  # chart

  x_lab_format <- switch(granularity,
    "Month" = "%b %Y",
    "%d %b %Y"
  )

  if (split_by == "Total") {
    fig <- plot_ly(base_data,
      x = ~ get(x_col), y = ~ get(y_col), type = "scattergl", mode = "lines",
      line = list(color = config$colours$theme$primary_accent)
    )
  } else {
    colour_col <- ifelse(split_by == "Vendor", "Vendor", "payment_type")

    fig <- plot_ly(base_data,
      x = ~ get(x_col), y = ~ get(y_col),
      color = as.formula(paste0("~`", colour_col, "`")),
      type = "scattergl",
      mode = "lines"
    )
  }

  fig |>
    layout(
      title = title,
      xaxis = list(
        title = "Journey date",
        tickmode = "array",
        tickvals = base_data[[x_col]],
        ticktext = format(base_data[[x_col]], x_lab_format)
      ),
      yaxis = list(
        title = y_lab_title
      )
    )

  return(fig)
}
