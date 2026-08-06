
#' Calculate Trip Volumes
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
#' @param data_field A character string of the metric to aggregate (e.g., "total_distance" or "total_number_trips").
#'
#' @return A tibble with two columns: `date` and `total_value`, containing the aggregated totals.
#' 
#' @export
fn_calculate_trip_volumes <- function(base_data, vendor_list, payment_list, 
                                      month_agg, week_agg, data_field) {
  
  if (!inherits(base_data, c("data.frame", "ArrowObject", "arrow_dplyr_query", "Dataset"))) {
    err_msg <- "System Error: The underlying data source is disconnected or invalid."
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (!is.logical(month_agg) || length(month_agg) != 1) {
    err_msg <- "Input Error: Month aggregation selection is invalid."
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (!is.logical(week_agg) || length(week_agg) != 1) {
    err_msg <- "Input Error: Week aggregation selection is invalid."
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (!is.character(data_field) || length(data_field) != 1) {
    err_msg <- "Input Error: Selected metric is invalid."
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }

  # check cols
  required_cols <- c("date", "Vendor", "payment_type", 
                     "full_month_aggregation", "full_week_aggregation", data_field)
  missing_cols <- setdiff(required_cols, colnames(base_data))
  
  if (length(missing_cols) > 0) {
    err_msg <- sprintf("Data Error: The dataset is missing required columns: %s", 
                       paste(missing_cols, collapse = ", "))
    
    log_error(err_msg) 
    
    shiny::validate(
      shiny::need(FALSE, err_msg)
    )
  }
  
  base_data |> 
    filter(
      full_month_aggregation == month_agg,
      full_week_aggregation == week_agg,
      Vendor %in% vendor_list,
      payment_type %in% payment_list
    ) |> 
    select(date, all_of(data_field)) |>
    group_by(date) |> 
    collect()  |> 
    summarise(total_value = sum(.data[[data_field]], na.rm = TRUE), .groups = 'drop') 
}





#' Calculate Heatmap Data
#'
#' @description Filters NYC taxi data based on temporal aggregation settings, 
#' pulls the filtered data into memory, formats date columns if required, 
#' and aggregates trip counts by pickup and dropoff locations.
#'
#' @param base_data An Arrow dataset or standard data frame containing the raw taxi data.
#' @param month_agg Logical. If `TRUE`, filters for monthly aggregation and groups by month.
#' @param week_agg Logical. If `TRUE`, filters for weekly aggregation and groups by week.
#'
#' @return A tibble containing the grouped location columns and a `trips` count column.
#'
#' @export
fn_calculate_heatmap_data <- function(base_data, month_agg, week_agg) {
  
  if (!inherits(base_data, c("data.frame", "ArrowObject", "arrow_dplyr_query", "Dataset"))) {
    err_msg <- "System Error: The underlying data source is disconnected or invalid."
    logger::log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (!is.logical(month_agg) || length(month_agg) != 1) {
    err_msg <- "Input Error: Month aggregation selection is invalid."
    logger::log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (!is.logical(week_agg) || length(week_agg) != 1) {
    err_msg <- "Input Error: Week aggregation selection is invalid."
    logger::log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  required_cols <- c("date", "full_month_aggregation", "full_week_aggregation", "PULocation", "DOLocation")
  missing_cols <- setdiff(required_cols, colnames(base_data))
  
  if (length(missing_cols) > 0) {
    err_msg <- sprintf("Data Error: The dataset is missing required columns: %s", 
                       paste(missing_cols, collapse = ", "))
    logger::log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  filtered_data <- base_data |> 
    dplyr::filter(
      full_month_aggregation == month_agg,
      full_week_aggregation == week_agg
    ) |> 
    dplyr::collect()
  
  grouping_cols <- c("PULocation", "DOLocation")
  
  if(month_agg) {
    tDF <- filtered_data |> 
      dplyr::mutate(date_month = format(date, "%Y month:%m"))
    grouping_cols <- c(grouping_cols, "date_month")
    
  } else if(week_agg) {
    tDF <- filtered_data |> 
      dplyr::mutate(date_week = format(date, "%G week:%V"))
    grouping_cols <- c(grouping_cols, "date_week")
    
  } else {
    tDF <- filtered_data
  }
  
  tDF |> 
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
    dplyr::summarise(trips = dplyr::n(), .groups = 'drop')
}