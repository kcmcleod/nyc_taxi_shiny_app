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
                                        title, y_lab_title, config) {
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

  fig <- plot_ly(base_data,
    x = ~ get(x_col), y = ~ get(y_col), type = "scattergl",
    mode = "lines"
  ) |>
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
#' @param config Application configuration list containing colour themes.
#' @param x_col A character string specifying the column for the x-axis (default: "PULocation").
#' @param y_col A character string specifying the column for the y-axis (default: "DOLocation").
#' @param z_col A character string specifying the column for the heatmap colour intensity (default: "trips").
#' @param title A character string for the chart's main title.
#'
#' @return A plotly htmlwidget object ready to be rendered in a Shiny UI.
fn_generate_heatmap_chart <- function(base_data, config, x_col = "PULocation",
                                      y_col = "DOLocation", z_col = "trips", title) {
  if (!inherits(base_data, "data.frame")) {
    err_msg <- "System Error: The underlying data source must be collected into memory before plotting."
    log_error(err_msg)
    stop(err_msg)
  }

  if (!is.character(title) || length(title) != 1) {
    err_msg <- "System Error: Invalid chart title."
    log_error(err_msg)
    stop(err_msg)
  }

  if (missing(config) || is.null(config$colours)) {
    err_msg <- "System Error: Dashboard configuration (colours) is missing."
    log_error(err_msg)
    stop(err_msg)
  }

  if (!is.character(x_col) || length(x_col) != 1 ||
    !is.character(y_col) || length(y_col) != 1 ||
    !is.character(z_col) || length(z_col) != 1) {
    err_msg <- "System Error: Column names for the heatmap axes must be valid text strings."
    log_error(err_msg)
    stop(err_msg)
  }
  required_cols <- c(x_col, y_col, z_col)
  missing_cols <- setdiff(required_cols, names(base_data))

  if (length(missing_cols) > 0) {
    err_msg <- sprintf(
      "Data Error: The heatmap dataset is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    )
    log_error(err_msg)
    stop(err_msg)
  }

  # CHART

  # Lock factor levels globally so axes align perfectly across facets even if sparse
  all_x <- sort(unique(as.character(base_data[[x_col]])))
  all_y <- sort(unique(as.character(base_data[[y_col]])))

  plot_data <- base_data |>
    dplyr::mutate(
      !!x_col := factor(.data[[x_col]], levels = all_x),
      !!y_col := factor(.data[[y_col]], levels = all_y)
    )

  # Determine if faceting is required
  if ("date_week" %in% names(plot_data)) {
    split_variable <- "date_week"
  } else if ("date_month" %in% names(plot_data)) {
    split_variable <- "date_month"
  } else {
    split_variable <- NULL
  }

  if (is.null(split_variable)) {
    calc_height <- 400
  } else {
    facets <- sort(unique(plot_data[[split_variable]]))
    n_facets <- length(facets)
    n_rows <- ceiling(n_facets / 3)
    calc_height <- max(400, n_rows * 300)
  }

  # Define the Plotly native colourscale
  custom_colours <- list(
    c(0, config$colours$theme$body_bg),
    c(0.5, config$colours$theme$primary_accent),
    c(1, config$colours$icons$taxi)
  )

  create_heatmap_trace <- function(df, show_legend = FALSE) {
    df_wide <- df |>
      dplyr::select(dplyr::all_of(c(x_col, y_col, z_col))) |>
      tidyr::pivot_wider(
        names_from = dplyr::all_of(x_col),
        values_from = dplyr::all_of(z_col),
        names_expand = TRUE,
        id_expand = TRUE,
        values_fn = sum
      )

    y_labels <- df_wide[[y_col]]
    z_matrix <- as.matrix(df_wide[, -1])
    x_labels <- colnames(z_matrix)

    plotly::plot_ly(
      x = x_labels,
      y = y_labels,
      z = z_matrix,
      type = "heatmap",
      height = calc_height,
      colorscale = custom_colours,
      colorbar = list(title = title),
      showscale = show_legend
    )
  }

  # Render Chart
  if (is.null(split_variable)) {
    # Standard single chart
    fig <- create_heatmap_trace(plot_data, show_legend = TRUE) |>
      plotly::layout(
        title = "Pickup vs Dropoff Borough",
        xaxis = list(title = "Pickup Borough", tickangle = 90),
        yaxis = list(title = "Dropoff Borough")
      )
    return(fig)
  } else {
    # Faceted (Subplot) Chart
    facets <- sort(unique(plot_data[[split_variable]]))
    n_facets <- length(facets)
    n_rows <- ceiling(n_facets / 3)

    # Define exactly how many pixels we want everything to take up
    plot_height_px <- 250
    gap_px <- 170 # Guaranteed space for 90-deg labels + the next title
    layout_top <- 100
    layout_bottom <- 80

    # 1. Total height scales perfectly with the number of rows
    calc_height <- (n_rows * plot_height_px) + ((n_rows - 1) * gap_px) +
      layout_top + layout_bottom

    # The exact fraction one gap represents
    gap_frac <- gap_px / calc_height

    plot_list <- lapply(seq_along(facets), function(i) {
      f <- facets[i]
      df_subset <- plot_data[plot_data[[split_variable]] == f, ]

      # Show the legend only on the first iteration to prevent duplicates
      p <- create_heatmap_trace(df_subset, show_legend = (i == 1)) |>
        plotly::layout(
          annotations = list(
            list(
              x = 0.5, y = 1.05, text = f,
              xref = "paper", yref = "paper",
              xanchor = "center", yanchor = "bottom",
              showarrow = FALSE, font = list(size = 13)
            )
          )
        )
      return(p)
    })

    # Apply the perfectly calculated gap fraction
    # Plotly's subplot margin adds top + bottom to create the vertical gap
    fig <- plotly::subplot(plot_list,
      nrows = n_rows, shareX = FALSE, shareY = TRUE,
      titleX = FALSE, titleY = FALSE,
      margin = c(0.02, 0.02, gap_frac / 2, gap_frac / 2)
    )

    layout_args <- list(
      p = fig,
      title = list(
        text = "Pickup vs Dropoff Borough",
        yref = "container", # Pin to the absolute HTML widget frame
        y = 1, # 100% to the top
        yanchor = "top",
        pad = list(t = 20) # Add 20 pixels of breathing room from the top edge
      ),
      margin = list(t = layout_top, b = layout_bottom)
    )

    # force 90 deg on every x axis
    for (i in 1:n_facets) {
      axis_name <- paste0("xaxis", if (i == 1) "" else i)
      layout_args[[axis_name]] <- list(tickangle = 90, showticklabels = TRUE)
    }

    fig <- do.call(plotly::layout, layout_args)

    return(fig)
  }
}


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
  df_data <- base_data[base_data$Date != "TOTALS", ]
  df_totals <- base_data[base_data$Date == "TOTALS", ]

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
  maxLength <- 20
  if (nrow(df_data) > maxLength) {
    domString <- paste0(domString, "p")
  }

  dt_options <- list(
    pageLength = maxLength, lengthMenu = c(5, 10, 15, 20),
    dom = domString
  )

  table <- DT::datatable(df_data, rownames = FALSE, options = dt_options, container = sketch) |>
    DT::formatRound(columns = c("Trip Count", "Passenger Count"), digits = 0) |>
    DT::formatRound(columns = "Total Distance (miles)", digits = 2) |>
    DT::formatCurrency(columns = c("Total Charge", "Total Fare"), currency = "$")

  return(table)
}
