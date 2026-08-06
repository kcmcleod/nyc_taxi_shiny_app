################################################################################
# UI SETUP

observeEvent(yellow_taxi_data(), {
  
  tDF <- yellow_taxi_data()
  
  # filters
  vendor_values <- app_metadata$Vendor
  payment_types <- app_metadata$payment_type
  
  updatePickerInput(session = session, "pi_vendor", choices = vendor_values, 
                    selected = vendor_values)
  updatePickerInput(session = session, "pi_pay_type", choices = payment_types, 
                    selected = payment_types)
  
  # dates
  min_date <- app_metadata$min_date
  max_date <- app_metadata$max_date
  initial_window_date <- floor_date(max_date %m-% weeks(13), unit = 'month')
  
  updateDateInput(session = session, "di_start_date", min = min_date, 
                  max = max_date, value = initial_window_date)
  updateDateInput(session = session, "di_end_date", min = min_date, 
                  max = max_date, value = max_date)
  
  enable("di_start_date")
  enable("di_end_date")
  enable("pi_vendor")
  enable("pi_pay_type")
})


################################################################################
# DATA

rv_main_date_filtered_date <- reactive({
  req(yellow_taxi_data(), input$di_end_date, input$di_start_date)
  
  if(input$di_start_date == Sys.Date() || input$di_end_date == Sys.Date()) {
    return(NULL)
  }
  
  user_start_date <- input$di_start_date
  user_end_date <- input$di_end_date 
  
  tmpDF <- yellow_taxi_data() |> 
    filter(
      # 1. Daily Data: Strict exact-day matching
      (!full_month_aggregation & !full_week_aggregation & 
         date >= user_start_date & date <= user_end_date) |
        
        # 2. Monthly Data: Snap the user's start date to the 1st of the month
        # due to the data prep using the 1st of the month as the date
        (full_month_aggregation & 
           date >= floor_date(user_start_date, "month") & 
           date <= user_end_date) |
        
        # 3. Weekly Data: Snap the user's start date to the start of the week.
        # due to ata prep using week_start = 7 (Sunday)
        (full_week_aggregation & 
           date >= floor_date(user_start_date, "week", week_start = 7) & 
           date <= user_end_date)
    )
  
  return(tmpDF)
})


rv_main_table_filtered_data <- reactive({
  req(rv_main_date_filtered_date())
  
  base_data <- rv_main_date_filtered_date() 
  vendor_list <- input$pi_vendor
  payment_list <- input$pi_pay_type
  time_period <- input$pi_level 
  month_agg <- ifelse(time_period == "Month", TRUE, FALSE)
  week_agg <- ifelse(time_period == "Week", TRUE, FALSE)  
  data_field <- ifelse(input$rb_journeys_or_passengers == "Total Distance", 
                       "total_distance", "total_number_trips")
  
  data <- fn_calculate_trip_volumes(base_data, vendor_list, payment_list, 
                                    month_agg, week_agg, data_field)
  
  return(data)
})  


# main trip volumes chart
output$po_tripVolumesOverTime <- renderPlotly({
  
  # User-facing validations for interactive selections
  validate(
    need(! is.null(input$rb_journeys_or_passengers), "Please select journeys or passengers"),
    need(length(input$pi_vendor) > 0, "Please select at least one vendor."),
    need(length(input$pi_pay_type) > 0, "Please select at least one payement type."),
    need(length(input$pi_level) == 1, "Please select exactly one time period.")
  )
  
  base_data <- rv_main_table_filtered_data()
  
  validate(
    need(nrow(base_data) > 0, "No data for that query")
  )
  
  y_lab_title <- ifelse(input$rb_journeys_or_passengers == "Total Distance",
                        "Distance in miles", "Total number of trips"  
  )
  
  chart <- fn_generate_main_line_chart(base_data, input$pi_level, "date", 
                                       "total_value", "Yellow Taxi journeys", 
                                       y_lab_title, config)
  
  return(chart)
 
}) |>
  bindCache(
    yellow_data_version(),
    input$di_start_date,
    input$di_end_date,
    input$pi_vendor,
    input$pi_pay_type,
    input$rb_journeys_or_passengers,
    input$pi_level
  )


################################################################################
# EXPORT FOR TESTING
exportTestValues(
  exported_filtered_rows = nrow(rv_main_table_filtered_data())
)