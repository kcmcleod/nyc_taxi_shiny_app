mod_yellow_taxis_availability_ui <- function(id, config, app_metadata) {
  ns <- NS(id)

  granularity_levels <- config$ui_values$pi_level[config$ui_values$pi_level != "Day"]

  tagList(
    card(
      fill = FALSE,
      card_header("Options", class = "bg-light"),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6, xl = 3),
        pickerInput(ns("pi_range"),
          label = "Data range:", multiple = FALSE,
          choices = c("From above data input", "Full data range"),
          selected = "Full data range",
          options = pickerOptions(container = "body")
        ),
        tags$div(
          id = ns("pi_level_wrapper"),
          pickerInput(ns("pi_level"),
            label = "Granularity:", multiple = FALSE,
            choices = granularity_levels, selected = "Month",
            options = pickerOptions(container = "body")
          )
        )
      )
    ),
    card(
      fill = FALSE,
      withSpinner(plotlyOutput(ns("ply_data_vols")),
        type = 2,
        color.background = config$colours$icons$taxi,
        color = config$colours$theme$primary_accent, size = 0.5
      )
    )
  )
}


mod_yellow_taxis_availability_server <- function(id, app_metadata, config,
                                                 data_version, user_start_date,
                                                 user_end_date) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$pi_range, {
      if (input$pi_range == "Full data range") {
        shinyjs::disable("pi_level_wrapper")
      } else {
        shinyjs::enable("pi_level_wrapper")
      }
    })


    rv_availbility_chart_filtered_data_raw <- reactive({
      req(app_metadata, input$pi_range)

      date_range <- input$pi_range
      granularity <- ifelse(date_range == "Full data range", "Month", input$pi_level)

      if (date_range == "Full data range") {
        start_date <- app_metadata$min_date
        end_date <- app_metadata$max_date
      } else {
        start_date <- user_start_date()
        end_date <- user_end_date()
      }

      chart_data <- app_metadata$daily_volumes

      data <- fn_calculate_availability_data(
        chart_data, granularity, config,
        start_date, end_date
      )

      return(data)
    })

    rv_availbility_chart_filtered_data <- debounce(rv_availbility_chart_filtered_data_raw, 800)

    ############################################################################

    output$ply_data_vols <- renderPlotly({
      validate(
        need(
          length(input$pi_level) == 1,
          "Please select exactly one time period."
        )
      )

      base_data <- tryCatch(
        {
          rv_availbility_chart_filtered_data()
        },
        error = function(e) {
          toastr_error(e$message)
          return(NULL)
        }
      )

      validate(
        need(!is.null(base_data), "Chart unavailable due to data error")
      )

      validate(
        need(nrow(base_data) > 0, "No data for that query")
      )

      date_range <- input$pi_range
      granularity <- ifelse(date_range == "Full data range", "Month", input$pi_level)

      chart <- tryCatch(
        {
          fn_generate_availability_chart(
            base_data, granularity, config
          )
        },
        error = function(e) {
          toastr_error(e$message)
          return(NULL)
        }
      )

      validate(
        need(!is.null(chart), "Chart unavailable due to data error"),
      )

      return(chart)
    })
  })
}
