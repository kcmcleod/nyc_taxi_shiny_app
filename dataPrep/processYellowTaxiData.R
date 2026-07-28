source(paste0(getwd(), "/global.R"))
library(arrow)
library(data.table)
library(logger)

sourceRFilesFromFolder(paste0(getwd(), "/dataPrep/yellow_taxi_helpers"))

# SETUP LOGGING
log_appender(appender_tee(paste0(dataPath, "logs/", format(Sys.time(), "%Y%m%d_%H%M"), "_yellow.log")))
log_threshold(INFO)
log_info("STARTING TO PROCESS YELLOW DATA")

yellowFiles <- list.files(path = paste0(dataPath, "src/"), pattern = "yellow.*.parquet", 
                          recursive = TRUE, full.names = TRUE)

if(length(yellowFiles) == 0) {
   log_error("Cannot find any files to process")
  return(NULL)
}

log_info("Found ", length(yellowFiles), " files to process...")

# objects built in the loop
combinedDF <- NULL
expected_cols <- NULL

for(file in yellowFiles) {
  log_info("Processing file: ", file)
  tmp <- fn_process_yellow_file(file)
  
  if(!is.null(tmp) && is_tibble(tmp)) 
  {
    if(is.null(combinedDF)) 
    {
      combinedDF <- tmp
      expected_cols <- names(tmp)
    } else if(identical(expected_cols, names(tmp))) {
      combinedDF <- as_tibble(rbindlist(list(combinedDF, tmp)))
    } else {
      log_error("Error adding ", file, " it's columns don't match what already exists")
    }
    remove(tmp)
    log_info("size of combined data is now: ", nrow(combinedDF))
  }
}

# write generated data to disk
sort_cols <- c("full_month_aggregation", "full_week_aggregation", "date")
combinedDF <- arrange(combinedDF, across(all_of(sort_cols)))

write_parquet(combinedDF, 
              sink = paste0(dataPath, format(Sys.Date(), "%Y%m%d"), "_yellow_aggregation.parquet"))

remove(combinedDF)
gc()