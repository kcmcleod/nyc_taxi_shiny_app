
yellow_taxi_data <- reactiveVal(NULL)


observe({
  
  all_yellow_files <- list.files(dataPath, pattern="yellow.*parquet", 
                                 full.names = TRUE)
  
  latest_yellow_file <- sort(all_yellow_files, decreasing = T)[1]
  
  log_info("READING DATA FROM ", latest_yellow_file)
  
  tDF <- read_parquet(latest_yellow_file)
  log_info(".... ", nrow(tDF), " yellow rows read")
  
  yellow_taxi_data(tDF)
  
  remove(tDF)
})