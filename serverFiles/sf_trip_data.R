

observeEvent(yellow_taxi_data(), {
  
  if(is.null(yellow_taxi_data())) 
    return(NULL)
  
  tDF <- yellow_taxi_data()
  vendor_values = sort(unique(tDF$VendorID))
  payment_types = sort(unique(tDF$payment_type))
  
  updatePickerInput(
    session = session,
    "pi_vendor",
    choices = vendor_values,
    selected = vendor_values
  )
  
  updatePickerInput(
    session = session,
    "pi_pay_type",
    choices = payment_types,
    selected = payment_types
  )
})


# main trip volumes chart
output$po_tripVolumesOverTime <- renderPlotly({
  req(yellow_taxi_data(), input$pi_vendor, input$pi_pay_type, 
      input$rb_journeys_or_passengers)
  
  if(! is.null(yellow_taxi_data())) {
    
    month_agg <- ifelse(input$pi_level == "Month", TRUE, FALSE)
    week_agg <- ifelse(input$pi_level == "Week", TRUE, FALSE)  
    data_field <- ifelse(input$rb_journeys_or_passengers == "Total Distance", 
                         "total_distance", "total_number_trips")
    
    x_lab_format <- switch (
      input$pi_level,
      "Month" = "%b %Y",
      "%d %b %Y"
    )
    
    y_lab_title <- ifelse(input$rb_journeys_or_passengers == "Total Distance",
        "Distance in miles", "Total number of trips"  
    )

    tmpDF <- yellow_taxi_data() |> 
      filter(
        full_month_aggregation == month_agg,
        full_week_aggregation == week_agg,
        VendorID %in% input$pi_vendor,
        payment_type %in% input$pi_pay_type
        ) |> 
      select(date, data_field) |> 
      group_by(date) |> 
      summarise(total_value = sum(.data[[data_field]], na.rm = TRUE), .groups = 'drop') 
    
    fig <- plot_ly(tmpDF, x = ~date, y = ~total_value, type = 'scatter', mode = 'line') |> 
      layout(
        title = "Yellow Taxi journeys",
        xaxis = list(
          title = "Journey month",
          tickmode = "array",
          tickvals = tmpDF$date,
          ticktext = format(tmpDF$date, x_lab_format)
        ),
        yaxis = list(
          title = y_lab_title
        )
      )
    
    remove(tmpDF)
    
    return(fig)    
    
  }
})