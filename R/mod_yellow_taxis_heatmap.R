################################################################################
### FILE: R/mod_yellow_taxis_heatmap.R
################################################################################

#'
mod_yellow_taxis_heatmap_ui <- function(id, config) {
  ns <- NS(id)

  tagList(
    card(
      fill = FALSE,
      card_header("Options"),
      radioButtons(ns("ri_heatmap_period"),
        label = "Heatmaps period:",
        choices = config$ui_values$heatmap_level,
        selected = "Weekly",
        inline = TRUE
      )
    ),
    conditionalPanel(
      condition = "input.ri_heatmap_period != 'Combined'",
      ns = ns,
      tags$div(
        class = "alert alert-secondary d-flex align-items-center mb-3",
        role = "alert",
        icon("circle-info",
          class = "fa-2x me-3",
          style = paste0("color: ", config$colours$theme$secondary_accent)
        ),
        tags$div(
          tags$strong("Total Journeys with Unknown Dates: "),
          tags$span(textOutput(ns("vo_missing_dates"), inline = TRUE), class = "fw-bold"),
          tags$br(),
          tags$small("These volumes are excluded from the faceted heatmaps
                     below due to meter date corruption.")
        )
      )
    ),
    card(
      style = "overflow-y: auto; max-height: 70vh; overflow-x: hidden;",
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
    # KPI VALUE BOXES

    output$vo_missing_dates <- renderText({
      req(filtered_data())

      # Isolate the NAs from the daily tier to prevent triple-counting
      na_val <- filtered_data() |>
        dplyr::filter(
          is.na(date),
          full_month_aggregation == FALSE,
          full_week_aggregation == FALSE
        ) |>
        dplyr::pull(total_number_trips) |>
        sum(na.rm = TRUE)

      # Format with commas
      scales::comma(na_val)
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
            title = "Number of journeys"
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
        start_date,
        end_date,
        input$ri_heatmap_period
      )


    ##############################################################################
    # EXPORT FOR TESTING
    exportTestValues(
      exported_heatmap_data = rv_main_heatmap_data()
    )
  })
}
