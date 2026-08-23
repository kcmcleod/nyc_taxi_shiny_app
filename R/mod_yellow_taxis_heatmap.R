################################################################################
### FILE: R/mod_yellow_taxis_heatmap.R
################################################################################

#'
mod_yellow_taxis_heatmap_ui <- function(id, config) {
  ns <- NS(id)

  tagList(
    card(
      fill = FALSE,
      card_header("Options", class = "bg-light"),
      radioButtons(ns("ri_heatmap_period"),
        label = "Heatmaps period:",
        choices = config$ui_values$heatmap_level,
        selected = "Weekly",
        inline = TRUE
      )
    ),
    card(
      fill = FALSE,
      withSpinner(plotlyOutput(ns("po_heatmap"), height = "auto"),
        type = 2,
        color.background = config$colours$icons$taxi,
        color = config$colours$theme$primary_accent, size = 0.5
      )
    )
  )
}

#'
mod_yellow_taxis_heatmap_server <- function(id, filtered_data, app_metadata,
                                            data_version, start_date, end_date) {
  moduleServer(id, function(input, output, session) {
    ############################################################################
    # DATA
    rv_main_heatmap_data <- reactive({
      req(filtered_data())

      base_data <- filtered_data()
      time_period <- input$ri_heatmap_period
      month_agg <- ifelse(time_period == "Monthly", TRUE, FALSE)
      week_agg <- ifelse(time_period == "Weekly", TRUE, FALSE)

      data <- fn_calculate_heatmap_data(base_data, month_agg, week_agg)

      return(data)
    })


    ############################################################################
    # CHART
    output$po_heatmap <- renderPlotly({
      plot_data <- tryCatch(
        {
          rv_main_heatmap_data()
        },
        error = function(e) {
          toastr_error(e$message)
          return(NULL)
        }
      )

      # Validation safely lives here on the main thread now
      validate(
        need(!is.null(plot_data), "Chart unavailable due to data error")
      )

      validate(
        need(nrow(plot_data) > 0, "No data for that query"),
        need(!is.null(input$ri_heatmap_period), "Please select a time period")
      )

      chart <- tryCatch(
        {
          fn_generate_heatmap_chart(plot_data, config,
            x_col = "PULocation", y_col = "DOLocation",
            z_col = "trips",
            title = "Number of journeys (Log Scale)"
          )
        },
        error = function(e) {
          toastr_error(e$message)
          return(NULL)
        }
      )

      return(chart)
    }) |>
      bindCache(
        data_version(),
        start_date(),
        end_date(),
        input$ri_heatmap_period
      )


    ##############################################################################
    # EXPORT FOR TESTING
    exportTestValues(
      exported_heatmap_data = rv_main_heatmap_data()
    )
  })
}
