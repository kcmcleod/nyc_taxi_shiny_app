################################################################################
### FILE: R/mod_yellow_taxis_main.R
################################################################################

#' Main tab for Taxi UI
mod_yellow_taxis_main_ui <- function(id, config) {
  ns <- NS(id)

  tagList(
    card(
      fill = FALSE,
      card_header("Options"),
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6, xl = 3),
        pickerInput(ns("pi_level"),
          label = "Granularity:", multiple = FALSE,
          choices = config$ui_values$pi_level, selected = "Week",
          options = pickerOptions(container = "body")
        ),
        disabled(pickerInput(ns("pi_vendor"),
          label = "Vendors:", multiple = TRUE, choices = c(),
          options = pickerOptions(
            `actions-box` = TRUE,
            container = "body"
          )
        )),
        disabled(pickerInput(ns("pi_pay_type"),
          label = "Payment:", multiple = TRUE, choices = c(),
          options = pickerOptions(
            `actions-box` = TRUE,
            container = "body"
          )
        )),
        radioButtons(ns("rb_journeys_or_passengers"),
          label = "Y axis:",
          choices = config$ui_values$metric_type
        )
      )
    ),
    conditionalPanel(
      condition = "input.pi_level != 'Month'",
      ns = ns,
      tags$div(
        class = "alert alert-secondary d-flex align-items-center mb-3", # mb-3 adds clean bottom spacing
        role = "alert",
        icon("circle-info", class = "fa-2x me-3", style = paste0("color: ", config$colours$theme$secondary_accent)),
        tags$div(
          tags$strong(textOutput(ns("vo_kpi_title"), inline = TRUE), ": "),
          tags$span(textOutput(ns("vo_missing_dates"), inline = TRUE), class = "fw-bold"),
          tags$br(),
          tags$small("These volumes are excluded from the timeline below due to meter date corruption.")
        )
      )
    ),
    card(
      withSpinner(plotlyOutput(ns("po_tripVolumesOverTime")),
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
    observe({
      # filters
      vendor_values <- app_metadata$Vendor
      payment_types <- app_metadata$payment_type

      updatePickerInput(
        session = session, "pi_vendor", choices = vendor_values,
        selected = vendor_values
      )
      updatePickerInput(
        session = session, "pi_pay_type", choices = payment_types,
        selected = payment_types
      )

      enable("pi_vendor")
      enable("pi_pay_type")
    })


    ################################################################################
    # DATA
    rv_main_chart_filtered_data <- reactive({
      req(filtered_data())

      base_data <- filtered_data()
      vendor_list <- input$pi_vendor
      payment_list <- input$pi_pay_type
      time_period <- input$pi_level
      month_agg <- ifelse(time_period == "Month", TRUE, FALSE)
      week_agg <- ifelse(time_period == "Week", TRUE, FALSE)
      data_field <- ifelse(input$rb_journeys_or_passengers == "Total Distance",
        "total_distance", "total_number_trips"
      )

      data <- fn_calculate_line_chart_data(
        base_data, vendor_list, payment_list,
        month_agg, week_agg, data_field
      )

      return(data)
    })


    ############################################################################
    # KPI VALUE BOXES

    output$vo_kpi_title <- renderText({
      if (input$rb_journeys_or_passengers == "Total Distance") {
        "Total Distance with Unknown Dates (miles)"
      } else {
        "Total Journeys with Unknown Dates"
      }
    })

    output$vo_missing_dates <- renderText({
      req(filtered_data(), input$pi_vendor, input$pi_pay_type)

      data_field <- ifelse(input$rb_journeys_or_passengers == "Total Distance",
        "total_distance", "total_number_trips"
      )

      # Isolate the NAs from the daily tier only to prevent triple-counting
      na_val <- filtered_data() |>
        filter(
          is.na(date),
          full_month_aggregation == FALSE,
          full_week_aggregation == FALSE,
          Vendor %in% input$pi_vendor,
          payment_type %in% input$pi_pay_type
        ) |>
        summarise(total = sum(!!sym(data_field), na.rm = TRUE)) |>
        collect() |>
        pull(total)

      # Format with commas for clean UI presentation
      scales::comma(na_val)
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
            y_lab_title, config
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
        start_date,
        end_date,
        input$pi_vendor,
        input$pi_pay_type,
        input$rb_journeys_or_passengers,
        input$pi_level
      )

    ############################################################################
    # EXPORT FOR TESTING
    exportTestValues(
      exported_main_chart_data = rv_main_chart_filtered_data()
    )
  })
}
