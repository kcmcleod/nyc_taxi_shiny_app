# Create a reactive or global expression representing the data version
# (e.g., using the file modification time of the latest parquet file)
yellow_data_version <- reactive({
  all_yellow_files <- list.files(dataPath, pattern = "yellow.*parquet", full.names = TRUE)
  latest_file <- sort(all_yellow_files, decreasing = TRUE)[1]
  if (is.na(latest_file)) {
    return(0)
  }
  file.info(latest_file)$mtime
})
