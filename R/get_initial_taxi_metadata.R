get_initial_taxi_metadata <- function(data_path) {

  all_yellow_metadata <- list.files(data_path, pattern="yellow.*rds", full.names = TRUE)
  latest_yellow_metadata <- sort(all_yellow_metadata, decreasing = TRUE)[1]
  
  if(is.na(latest_yellow_metadata)) {
    log_error("No yellow taxi metadata found")  
    return(NULL)
  }
  
  log_info("READING DATA FROM ", latest_yellow_metadata)
  return(read_rds(latest_yellow_metadata))
}