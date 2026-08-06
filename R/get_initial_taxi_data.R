get_initial_taxi_data <- function(data_path) {
  
  if (isTRUE(getOption("shiny.testmode"))) {
    log_info(".... loading TEST data !")
    return(readRDS("tests/testdata/mock_taxi_trips.rds"))
  }
  
  all_yellow_files <- list.files(data_path, pattern="yellow.*parquet", full.names = TRUE)
  latest_yellow_file <- sort(all_yellow_files, decreasing = TRUE)[1]
  
  if(is.na(latest_yellow_file)) {
    log_error("No yellow taxi data found")  
    return(NULL)
  }
  
  log_info("READING DATA FROM ", latest_yellow_file)
  tDF <- arrow::open_dataset(latest_yellow_file)
  log_info(".... ", nrow(tDF), " yellow rows read")
  
  return(tDF)
}




get_initial_taxi_metadata <- function(data_path) {
  
  if (isTRUE(getOption("shiny.testmode"))) {
    log_info(".... loading TEST metadata !")
    return(readRDS("tests/testdata/yellow_metadata.rds"))
  }
  
  all_yellow_metadata <- list.files(data_path, pattern="yellow.*rds", full.names = TRUE)
  latest_yellow_metadata <- sort(all_yellow_metadata, decreasing = TRUE)[1]
  
  if(is.na(latest_yellow_metadata)) {
    log_error("No yellow taxi metadata found")  
    return(NULL)
  }
  
  log_info("READING DATA FROM ", latest_yellow_metadata)
  return(read_rds(latest_yellow_metadata))
}