# Create a reactive or global expression representing the data version
# (e.g., using the file modification time of the latest parquet file)
yellow_data_version <- reactive({
  req(app_metadata, app_metadata$min_date, app_metadata$max_date)

  paste0(app_metadata$min_date, "_", app_metadata$max_date)
})
