source(paste0(getwd(), "/global.R"))
library(httr)

# Determine the target month to check (e.g., usually 2 months behind current time)
target_date <- Sys.Date() %m-% months(2)
year_month <- format(target_date, "%Y-%m")

file_url <- paste0("https://d37ci6vzurychx.cloudfront.net/trip-data/yellow_tripdata_", year_month, ".parquet")
destination_path <- paste0(Sys.getenv("HOME"), "/data/nyc_taxi/yellow_tripdata_", year_month, ".parquet")

# Check if we already have it locally
if (file.exists(destination_path)) {
  message("Data for ", year_month, " is already downloaded.")
} else {
  message("Polling NYC TLC for ", year_month, "...")
  
  # HEAD request to check if the file exists on their server yet without downloading the multi-GB payload
  response <- HEAD(file_url)
  
  if (status_code(response) == 200) {
    message("New data found! Downloading...")
    download.file(file_url, destfile = destination_path, mode = "wb")
    message("Download complete. Triggering aggregation pipeline...")
    
    # Automatically kick off your processing script
    source("dataPrep/processYellowTaxiData.R")
    
  } else {
    message("File not published by NYC TLC yet. Status code: ", status_code(response))
  }
}