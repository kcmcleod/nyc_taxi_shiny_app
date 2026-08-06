
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
fn_generate_main_line_chart <- function(base_data, granularity, x_col, y_col, title, y_lab_title, config) {
  
  if (!inherits(base_data, "data.frame")) {
    err_msg <- "System Error: The underlying data source is disconnected or invalid."
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }

  if (! is.character(title)  || length(title) != 1) {
    err_msg <- "System Error: Please supply a valid title for the chart"
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (! is.character(y_lab_title)  || length(y_lab_title) != 1) {
    err_msg <- "System Error: Please supply a valid title for the chart's y-axis"
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (is.null(config$ui_values$pi_level) || !granularity %in% config$ui_values$pi_level) {
    err_msg <- "System Error: Selected granularity is not permitted by the configuration."
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  
  # check cols
  required_cols <- c(x_col, y_col)
  missing_cols <- setdiff(required_cols, colnames(base_data))
  
  if (length(missing_cols) > 0) {
    err_msg <- sprintf("Data Error: The dataset is missing required columns: %s", 
                       paste(missing_cols, collapse = ", "))
    
    log_error(err_msg) 
    
    shiny::validate(
      shiny::need(FALSE, err_msg)
    )
  }
  
  # chart  
  
  x_lab_format <- switch (
    granularity,
    "Month" = "%b %Y",
    "%d %b %Y"
  )
  
  fig <- plot_ly(base_data, x = ~get(x_col), y = ~get(y_col), type = 'scatter', mode = 'lines') |> 
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



#' Generate Location Heatmap Chart
#'
#' @description Constructs an interactive plotly matrix grid heatmap to visualise 
#' trip volumes between pickup and dropoff locations. Includes defensive traps to 
#' gracefully handle missing data or incorrect inputs within a Shiny application without 
#' crashing the server.
#'
#' @param base_data An in-memory data.frame or tibble containing the aggregated taxi data.
#' @param x_col A character string specifying the column for the x-axis (default: "PULocation").
#' @param y_col A character string specifying the column for the y-axis (default: "DOLocation").
#' @param z_col A character string specifying the column for the heatmap colour intensity (default: "trips").
#' @param title A character string for the chart's main title.
#'
#' @return A plotly htmlwidget object ready to be rendered in a Shiny UI.
fn_generate_heatmap_chart <- function(base_data, config, x_col = "PULocation", y_col = "DOLocation", z_col = "trips", title) {
  
  if (!inherits(base_data, "data.frame")) {
    err_msg <- "System Error: The underlying data source must be collected into memory before plotting."
    logger::log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (! is.character(title) || length(title) != 1) {
    err_msg <- "System Error: Invalid chart title."
    logger::log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (missing(config) || is.null(config$colours)) {
    err_msg <- "System Error: Dashboard configuration (colours) is missing."
    logger::log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }

  if (!is.character(x_col) || length(x_col) != 1 || 
      !is.character(y_col) || length(y_col) != 1 || 
      !is.character(z_col) || length(z_col) != 1) {
    err_msg <- "System Error: Column names for the heatmap axes must be valid text strings."
    logger::log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  required_cols <- c(x_col, y_col, z_col)
  missing_cols <- setdiff(required_cols, names(base_data))
  
  if (length(missing_cols) > 0) {
    err_msg <- sprintf("Data Error: The heatmap dataset is missing required columns: %s", 
                       paste(missing_cols, collapse = ", "))
    logger::log_error(err_msg) 
    shiny::validate(shiny::need(FALSE, err_msg))
  }

  # CHART
  
  plot_data <- base_data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c(x_col, y_col)), as.factor)
    )
  
  if("date_week" %in% names(plot_data)) {
    split_variable <- "date_week"
  } else if("date_month" %in% names(plot_data)) {
    split_variable <- "date_month"
  }
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], 
                                               fill = .data[[z_col]])) +
    ggplot2::geom_tile()
  
  if(exists("split_variable")) {
    # as.formula avoids scoping issues in Shiny reactives
    p <- p + ggplot2::facet_wrap(as.formula(paste0("~", split_variable)), scales = "fixed", ncol = 3)
    
    n_facets <- dplyr::n_distinct(plot_data[[split_variable]])
    n_rows <- ceiling(n_facets / 3)
    calc_height <- max(400, n_rows * 300)
  } else {
    calc_height <- 400
  }
  
  p <- p +
    ggplot2::scale_fill_gradientn(
      colours = c(
        config$colours$theme$body_bg,
        config$colours$theme$primary_accent,
        config$colours$icons$taxi
      ),
      name = "Total Trips",
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
    ) +
    ggplot2::labs(
      title = "Pickup vs Dropoff Borough",
      x = "Pickup Borough",
      y = "Dropoff Borough"
    )
  
  plotly::ggplotly(p, tooltip = c("x", "y", "fill"), height = calc_height)
}