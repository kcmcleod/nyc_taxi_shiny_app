start_time = Sys.time()

source(paste0(getwd(), "/global.R"))
library(arrow)
library(data.table)
library(logger)
library(prettyunits)

sourceRFilesFromFolder(paste0(getwd(), "/dataPrep/yellow_taxi_helpers"))

################################################################################
# SETUP LOGGING
log_name <- paste0(dataPath, "logs/", format(Sys.time(), "%Y%m%d_%H%M"), "_yellow.log")
log_appender(appender_tee(log_name))
log_threshold(INFO)
log_info("STARTING TO PROCESS YELLOW DATA")

yellowFiles <- list.files(path = paste0(dataPath, "src"), pattern = "yellow.*.parquet", 
                          recursive = TRUE, full.names = TRUE)

if(length(yellowFiles) == 0) {
  log_error("Cannot find any files to process")
  return(NULL)
}

log_info("Found ", length(yellowFiles), " files to process...")

################################################################################
# LOOP THRU DATA FILES
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

################################################################################
# SORT
sort_cols <- c("full_month_aggregation", "full_week_aggregation", "date")
combinedDF <- arrange(combinedDF, across(all_of(sort_cols)))

################################################################################
# LOOKUPS
# todo need to check files exists 
zone_lookups <- read_csv(paste0(dataPath, "src/taxi_zone_lookup.csv")) |> 
  select("LocationID", "Borough")
rate_lookups <- read_csv(paste0(dataPath, "src/rate_code_lookup.csv"), col_types = c("ic"))
payment_lookups <- read_csv(paste0(dataPath, "src/payment_type_lookup.csv"), col_types = c("ic"))
vendor_lookups <- read_csv(paste0(dataPath, "src/vendor_lookup.csv"), col_types = c("ic"))

combinedDF <- fn_perform_lookup(combinedDF, rate_lookups, "RatecodeID", "RatecodeID", "Rate", "rate") |> 
  fn_perform_lookup(zone_lookups, c("PULocationID" = "LocationID"), "PULocationID", "PULocation", "PU",
                    post_process_fn = function(df) rename(df, PULocation = Borough)) |> 
  fn_perform_lookup(zone_lookups, c("DOLocationID" = "LocationID"), "DOLocationID", "DOLocation", "DO", 
                    post_process_fn = function(df) rename(df, DOLocation = Borough)) |> 
  fn_perform_lookup(payment_lookups, "payment_type", "payment_type", "payment_type", "payment", 
                    post_process_fn = function(df) mutate(df, payment_type = payment_class)) |> 
  fn_perform_lookup(vendor_lookups, "VendorID", "VendorID", "Vendor", "vendor")

# todo checks on joins

################################################################################
# WRITE ASSET
write_parquet(combinedDF, 
              sink = paste0(dataPath, format(Sys.Date(), "%Y%m%d"), "_yellow_aggregation.parquet"))

remove(combinedDF)
gc()

total_time = Sys.time() - start_time
pretty_duration <- pretty_ms(as.numeric(total_time, units = "secs") * 1000)
log_info("Finished! This took: ", pretty_duration)
message("Log: ", str_replace_all(string = log_name, pattern = " ", replacement = "\\\\ "))
message("GOOD BYE!")