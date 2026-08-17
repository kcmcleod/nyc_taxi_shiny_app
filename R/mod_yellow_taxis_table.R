################################################################################
### FILE: R/mod_yellow_taxis_table.R
################################################################################

#' Table tab
mod_yellow_taxis_table_ui <- function(id, config) {
  ns <- NS(id)

  tagList(
    card(
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
    rv_table_filtered_data <- reactive({
      req(filtered_data())

      base_data <- filtered_data()

      data <- fn_calculate_table_data(base_data)

      return(data)
    })


    ############################################################################
    # TABLE
    output$dt_tripVolumes <- renderDT({
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
