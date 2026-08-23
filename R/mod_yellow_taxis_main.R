################################################################################
### FILE: R/mod_yellow_taxis_main.R
################################################################################

#' Main tab for Taxi UI
mod_yellow_taxis_main_ui <- function(id, config, app_metadata) {
  ns <- NS(id)

  tagList(
    card(
      fill = FALSE,
      card_header("Options", class = "bg-light"),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6, xl = c(2, 3, 3, 2, 2)),
        pickerInput(ns("pi_level"),
          label = "Granularity:", multiple = FALSE, width = "100%",
          choices = config$ui_values$pi_level, selected = "Week",
          options = pickerOptions(container = "body")
        ),
        pickerInput(ns("pi_vendor"),
          label = "Vendors:", multiple = TRUE,
          choices = app_metadata$Vendor,
          selected = app_metadata$Vendor,
          width = "100%",
          options = pickerOptions(
            `actions-box` = TRUE,
            container = "body"
          )
        ),
        pickerInput(ns("pi_pay_type"),
          label = "Payment:", multiple = TRUE,
          choices = app_metadata$payment_type,
          selected = app_metadata$payment_type,
          width = "100%",
          options = pickerOptions(
            `actions-box` = TRUE,
            container = "body"
          )
        ),
        radioButtons(ns("rb_split_by"),
          label = "Lines:",
          choices = c("Payment Type", "Vendor", "Total")
        ),
        radioButtons(ns("rb_journeys_or_passengers"),
          label = "Y axis:",
          choices = config$ui_values$metric_type
        )
      )
    ),
    card(
      full_screen = TRUE, # Keeps the expand icon in the bottom right corner
      fill = FALSE,
      style = "min-height: 550px;",
      withSpinner(
        plotlyOutput(ns("po_tripVolumesOverTime"), height = "500px"), # Explicit height
        type = 2,
        color.background = config$colours$icons$taxi,
        color = config$colours$theme$primary_accent, size = 0.5
      )
    )
  )
}

#' Info Page Module Server
mod_yellow_taxis_main_server <- function(id, filtered_data, app_metadata,
                                         data_version, start_date, end_date) {
  moduleServer(id, function(input, output, session) {
    ################################################################################
    # DATA
    rv_main_chart_filtered_data <- reactive({
      req(
        filtered_data(), input$pi_vendor, input$pi_pay_type,
        input$pi_level, input$rb_split_by
      )

      base_data <- filtered_data()
      vendor_list <- input$pi_vendor
      payment_list <- input$pi_pay_type
      time_period <- input$pi_level
      month_agg <- ifelse(time_period == "Month", TRUE, FALSE)
      week_agg <- ifelse(time_period == "Week", TRUE, FALSE)
      data_field <- ifelse(input$rb_journeys_or_passengers == "Total Distance",
        "total_distance", "total_number_trips"
      )
      split_by <- input$rb_split_by

      data <- fn_calculate_line_chart_data(
        base_data, vendor_list, payment_list,
        month_agg, week_agg, data_field, split_by
      )

      return(data)
    })


    ############################################################################
    # CHART
    output$po_tripVolumesOverTime <- renderPlotly({
      # User-facing validations for interactive selections
      validate(
        need(
          !is.null(input$rb_journeys_or_passengers),
          "Please select journeys or passengers"
        ),
        need(
          length(input$pi_vendor) > 0,
          "Please select at least one vendor."
        ),
        need(
          length(input$pi_pay_type) > 0,
          "Please select at least one payement type."
        ),
        need(length(input$pi_level) == 1, "
             Please select exactly one time period.")
      )

      base_data <- tryCatch(
        {
          rv_main_chart_filtered_data()
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

      y_lab_title <- ifelse(input$rb_journeys_or_passengers == "Total Distance",
        "Distance in miles", "Total number of trips"
      )

      chart <- tryCatch(
        {
          fn_generate_main_line_chart(
            base_data, input$pi_level, "date",
            "total_value", "Yellow Taxi journeys",
            y_lab_title, input$rb_split_by, config
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
    }) |>
      bindCache(
        data_version(),
        start_date(),
        end_date(),
        input$pi_vendor,
        input$pi_pay_type,
        input$rb_journeys_or_passengers,
        input$pi_level,
        input$rb_split_by
      )

    ############################################################################
    # EXPORT FOR TESTING
    exportTestValues(
      exported_main_chart_data = rv_main_chart_filtered_data()
    )
  })
}
