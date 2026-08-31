################################################################################
### FILE: R/mod_yellow_taxis_table.R
################################################################################

#' Table tab
mod_yellow_taxis_table_ui <- function(id, config, app_metadata) {
  ns <- NS(id)

  tagList(
    card(
      fill = FALSE,
      card_header("Options", class = "bg-light"),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6, xl = 3),
        pickerInput(ns("pi_level"),
          label = "Granularity:", multiple = FALSE,
          choices = config$ui_values$pi_level, selected = "Week",
          options = pickerOptions(container = "body")
        ),
        pickerInput(ns("pi_pu_locations"),
          label = "Pick Up Locations:", multiple = TRUE,
          choices = app_metadata$Location,
          selected = app_metadata$Location,
          options = pickerOptions(
            `actions-box` = TRUE,
            container = "body"
          )
        ),
        pickerInput(ns("pi_do_locations"),
          label = "Drop Off Locations:", multiple = TRUE,
          choices = app_metadata$Location,
          selected = app_metadata$Location,
          options = pickerOptions(
            `actions-box` = TRUE,
            container = "body"
          )
        )
      )
    ),
    card(
      fill = FALSE,
      withSpinner(DTOutput(ns("dt_tripVolumes")),
        type = 2,
        color.background = config$colours$icons$taxi,
        color = config$colours$theme$primary_accent, size = 0.5
      )
    )
  )
}

#' Info Page Module Server
mod_yellow_taxis_table_server <- function(id, filtered_data, app_metadata,
                                          data_version, start_date, end_date) {
  moduleServer(id, function(input, output, session) {
    ################################################################################
    # DATA
    rv_table_filtered_data_raw <- reactive({
      req(filtered_data(), input$pi_level, input$pi_do_locations, input$pi_pu_locations)

      pu_locations <- input$pi_pu_locations
      do_locations <- input$pi_do_locations
      time_period <- input$pi_level
      month_agg <- ifelse(time_period == "Month", TRUE, FALSE)
      week_agg <- ifelse(time_period == "Week", TRUE, FALSE)

      base_data <- filtered_data()

      data <- fn_calculate_table_data(
        base_data, week_agg, month_agg,
        pu_locations, do_locations
      )

      return(data)
    }) |>
      bindCache(
        data_version(),
        start_date(),
        end_date(),
        input$pi_level,
        input$pi_do_locations,
        input$pi_pu_locations
      )

    if (isTRUE(getOption("shiny.testmode"))) {
      rv_table_filtered_data <- rv_table_filtered_data_raw
    } else {
      rv_table_filtered_data <- debounce(rv_table_filtered_data_raw, 800)
    }


    ############################################################################
    # TABLE
    output$dt_tripVolumes <- renderDT({
      validate(
        need(
          length(input$pi_pu_locations) > 0,
          "Please select at least one pick up location."
        ),
        need(
          length(input$pi_do_locations) > 0,
          "Please select at least one drop off location."
        ),
        need(length(input$pi_level) == 1, "
             Please select exactly one time period.")
      )

      base_data <- tryCatch(
        {
          rv_table_filtered_data()
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

      fn_generate_yellow_taxi_table(base_data)
    })

    ############################################################################
    # EXPORT FOR TESTING
    exportTestValues(
      exported_table_data = rv_table_filtered_data()
    )
  })
}
