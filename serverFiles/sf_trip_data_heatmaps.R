rv_main_heatmap_data <- reactive({
  req(rv_main_date_filtered_date())
  
  base_data <- rv_main_date_filtered_date()
  time_period <- input$ri_heatmap_period
  month_agg <- ifelse(time_period == "Monthly", TRUE, FALSE)
  week_agg <- ifelse(time_period == "Weekly", TRUE, FALSE)  
  
  data <- fn_calculate_heatmap_data(base_data, month_agg, week_agg)
  
  return(data)
})


output$po_heatmap <- renderPlotly({
  
  plot_data <- rv_main_heatmap_data()
  
  # Validation safely lives here on the main thread now
  validate(
    need(nrow(plot_data) > 0, "No data for that query"),
    need(! is.null(input$ri_heatmap_period), "Please select a time period")
  )
  
  plot_data <- plot_data  |>
    mutate(
      PULocation = as.factor(PULocation),
      DOLocation = as.factor(DOLocation)
    )
  
  if("date_week" %in% names(plot_data)) {
    split_variable <- "date_week"
  } else if("date_month" %in% names(plot_data)) {
    split_variable <- "date_month"
  }
  
  p <- ggplot(plot_data, aes(x = PULocation, y = DOLocation, fill = trips)) +
    geom_tile()
  
  if(exists("split_variable")) {
    p <- p + facet_wrap(~ get(split_variable), scales = "fixed", ncol = 3)
    
    n_facets <- n_distinct(plot_data[[split_variable]])
    n_rows <- ceiling(n_facets / 3)
    calc_height <- max(400, n_rows * 300)
  } else {
    calc_height <- 400
  }
  
  p <- p +
    scale_fill_gradientn(
      colours = c(
        config$colours$theme$body_bg,
        config$colours$theme$primary_accent,
        config$colours$icons$taxi
      ),
      name = "Total Trips",
      labels = label_number(scale_cut = cut_short_scale())
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    ) +
    labs(
      title = "Pickup vs Dropoff Borough",
      x = "Pickup Borough",
      y = "Dropoff Borough"
    )
  
  ggplotly(p, tooltip = c("x", "y", "fill"), height = calc_height)
  
  return(p)
}) |> 
  bindCache(
    yellow_data_version(),
    input$di_start_date,
    input$di_end_date,
    input$ri_heatmap_period
  )
