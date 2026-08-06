# 
# 
# observeEvent(yellow_taxi_data(), {
#   
#   tDF <- yellow_taxi_data()
#   
#   # filters
#   vendor_values <- app_metadata$Vendor
#   payment_types <- app_metadata$payment_type
#   
#   updatePickerInput(session = session, "pi_vendor", choices = vendor_values, selected = vendor_values)
#   updatePickerInput(session = session, "pi_pay_type", choices = payment_types, selected = payment_types)
#   
#   # dates
#   min_date <- app_metadata$min_date
#   max_date <- app_metadata$max_date
#   initial_window_date <- floor_date(max_date %m-% weeks(13), unit = 'month')
#   
#   updateDateInput(session = session, "di_start_date", min = min_date, max = max_date, value = initial_window_date)
#   updateDateInput(session = session, "di_end_date", min = min_date, max = max_date, value = max_date)
#   
#   enable("di_start_date")
#   enable("di_end_date")
#   enable("pi_vendor")
#   enable("pi_pay_type")
# })
# 
# 
# rv_main_date_filtered_date <- reactive({
#   req(yellow_taxi_data(), input$di_end_date, input$di_start_date)
#   
#   if(input$di_start_date == Sys.Date() || input$di_end_date == Sys.Date()) {
#     return(NULL)
#   }
#   
#   user_start_date <- input$di_start_date
#   user_end_date <- input$di_end_date 
#   
#   tmpDF <- yellow_taxi_data() |> 
#     filter(
#       # 1. Daily Data: Strict exact-day matching
#       (!full_month_aggregation & !full_week_aggregation & 
#          date >= user_start_date & date <= user_end_date) |
#         
#         # 2. Monthly Data: Snap the user's start date to the 1st of the month
#         # due to the data prep using the 1st of the month as the date
#         (full_month_aggregation & 
#            date >= floor_date(user_start_date, "month") & 
#            date <= user_end_date) |
#         
#         # 3. Weekly Data: Snap the user's start date to the start of the week.
#         # due to ata prep using week_start = 7 (Sunday)
#         (full_week_aggregation & 
#            date >= floor_date(user_start_date, "week", week_start = 7) & 
#            date <= user_end_date)
#     )
#   
#   return(tmpDF)
# })
# 
# 
# rv_main_table_filtered_data <- reactive({
#   req(rv_main_date_filtered_date())
#   
#   base_data <- rv_main_date_filtered_date() 
#   vendor_list <- input$pi_vendor
#   payment_list <- input$pi_pay_type
#   time_period <- input$pi_level 
#   month_agg <- ifelse(time_period == "Month", TRUE, FALSE)
#   week_agg <- ifelse(time_period == "Week", TRUE, FALSE)  
#   data_field <- ifelse(input$rb_journeys_or_passengers == "Total Distance", 
#                        "total_distance", "total_number_trips")
#   
#   base_data |> 
#     filter(
#       full_month_aggregation == month_agg,
#       full_week_aggregation == week_agg,
#       Vendor %in% vendor_list,
#       payment_type %in% payment_list
#     ) |> 
#     select(date, all_of(data_field)) |>
#     group_by(date) |> 
#     collect()  |> 
#     summarise(total_value = sum(.data[[data_field]], na.rm = TRUE), .groups = 'drop') 
# })  
# 
# 
# # main trip volumes chart
# output$po_tripVolumesOverTime <- renderPlotly({
#   
#   # User-facing validations for interactive selections
#   validate(
#     need(! is.null(input$rb_journeys_or_passengers), "Please select journeys or passengers"),
#     need(length(input$pi_vendor) > 0, "Please select at least one vendor."),
#     need(length(input$pi_pay_type) > 0, "Please select at least one payement type."),
#     need(length(input$pi_level) == 1, "Please select exactly one time period.")
#   )
#   
#   x_lab_format <- switch (
#     input$pi_level,
#     "Month" = "%b %Y",
#     "%d %b %Y"
#   )
#   
#   y_lab_title <- ifelse(input$rb_journeys_or_passengers == "Total Distance",
#                         "Distance in miles", "Total number of trips"  
#   )
#   
#   tmpDF <- rv_main_table_filtered_data()
#   
#   validate(
#     need(nrow(tmpDF) > 0, "No data for that query")
#   )
#   
#   
#   fig <- plot_ly(tmpDF, x = ~date, y = ~total_value, type = 'scatter', mode = 'line') |> 
#     layout(
#       title = "Yellow Taxi journeys",
#       xaxis = list(
#         title = "Journey month",
#         tickmode = "array",
#         tickvals = tmpDF$date,
#         ticktext = format(tmpDF$date, x_lab_format)
#       ),
#       yaxis = list(
#         title = y_lab_title
#       )
#     )
#   
#   return(fig)    
# }) |> 
#   bindCache(
#     yellow_data_version(),
#     input$di_start_date, 
#     input$di_end_date, 
#     input$pi_vendor, 
#     input$pi_pay_type,
#     input$rb_journeys_or_passengers,
#     input$pi_level
#   )
# 
# 
# rv_main_heatmap_data <- reactive({
#   req(rv_main_date_filtered_date())
#   
#   base_data <- rv_main_date_filtered_date()
#   time_period <- input$ri_heatmap_period
#   month_agg <- ifelse(time_period == "Monthly", TRUE, FALSE)
#   week_agg <- ifelse(time_period == "Weekly", TRUE, FALSE)  
#   
#   grouping_cols <- c("PULocation", "DOLocation")
#   
#   if(month_agg) {
#     tDF <- base_data |> 
#       mutate(date_month = format(date, "%Y month:%m"))
#     
#     grouping_cols <- c(grouping_cols, "date_month")
#     
#   } else if(week_agg) {
#     tDF <- base_data |> 
#       mutate(date_week = format(date, "%G week:%V"))
#     
#     grouping_cols <- c(grouping_cols, "date_week")
#   } else {
#     tDF <- base_data
#   }
#   
#   tDF |> 
#     filter(
#       full_month_aggregation == month_agg,
#       full_week_aggregation == week_agg) |> 
#     group_by(across(all_of(grouping_cols))) %>%
#     summarise(trips = n(), .groups = 'drop') |> 
#     collect()
# })
# 
# 
# output$po_heatmap <- renderPlotly({
#   
#   plot_data <- rv_main_heatmap_data() 
#   
#   # Validation safely lives here on the main thread now
#   validate(
#     need(nrow(plot_data) > 0, "No data for that query"),
#     need(! is.null(input$ri_heatmap_period), "Please select a time period")
#   )
#   
#   plot_data <- plot_data  |>
#     mutate(
#       PULocation = as.factor(PULocation),
#       DOLocation = as.factor(DOLocation)
#     )
#   
#   if("date_week" %in% names(plot_data)) {
#     split_variable <- "date_week"
#   } else if("date_month" %in% names(plot_data)) {
#     split_variable <- "date_month"
#   }
#   
#   p <- ggplot(plot_data, aes(x = PULocation, y = DOLocation, fill = trips)) +
#     geom_tile()
#   
#   if(exists("split_variable")) {
#     p <- p + facet_wrap(~ get(split_variable), scales = "fixed", ncol = 3) 
#     
#     n_facets <- n_distinct(plot_data[[split_variable]])
#     n_rows <- ceiling(n_facets / 3)
#     calc_height <- max(400, n_rows * 300)
#   } else {
#     calc_height <- 400
#   }   
#   
#   p <- p + 
#     scale_fill_gradientn(
#       colours = c(
#         config$colours$theme$body_bg,
#         config$colours$theme$primary_accent,
#         config$colours$icons$taxi
#       ),
#       name = "Total Trips",
#       labels = label_number(scale_cut = cut_short_scale())
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
#     ) +
#     labs(
#       title = "Pickup vs Dropoff Borough",
#       x = "Pickup Borough",
#       y = "Dropoff Borough"
#     )
#   
#   ggplotly(p, tooltip = c("x", "y", "fill"), height = calc_height)
#   
#   return(p)
# }) |> 
#   bindCache(
#     yellow_data_version(),
#     input$di_start_date, 
#     input$di_end_date, 
#     input$ri_heatmap_period
#   )
# 
# 
# # --- EXPORT FOR TESTING ---
# # These values are invisible to the user but visible to shinytest2
# exportTestValues(
#   exported_filtered_rows = nrow(rv_main_table_filtered_data())
# )