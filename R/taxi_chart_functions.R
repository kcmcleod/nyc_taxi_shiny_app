

fn_generate_main_line_chart <- function(base_data, granularity, x_col, y_col, title, y_lab_title, config) {
  
  if (!inherits(base_data, c("data.frame", "ArrowObject", "arrow_dplyr_query", "Dataset"))) {
    err_msg <- "System Error: The underlying data source is disconnected or invalid."
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (class(title) != "character") {
    err_msg <- "System Error: Please supply a valid title for the chart"
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (class(y_lab_title) != "character") {
    err_msg <- "System Error: Please supply a valid title for the chart's y-axis"
    log_error(err_msg)
    shiny::validate(shiny::need(FALSE, err_msg))
  }
  
  if (!granularity %in% config$ui_values$pi_level) {
    err_msg <- "System Error: Please supply a valid title for the chart's y-axis"
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
  
  fig <- plot_ly(base_data, x = ~get(x_col), y = ~get(y_col), type = 'scatter', mode = 'line') |> 
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