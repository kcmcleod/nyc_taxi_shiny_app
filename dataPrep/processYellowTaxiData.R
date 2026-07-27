source(paste0(getwd(), "/global.R"))
library(arrow)

yellowFiles <- list.files(path = paste0(dataPath, "/src/"), pattern = "*.parquet", 
                                        recursive = TRUE, full.names = TRUE)

i <- 1


currentFile <- yellowFiles[i]
currentDate <- stringr::str_match(currentFile, pattern = "\\d{4}-\\d{2}")[1]

if( is.null(currentDate) || is.na(currentDate)) {
  stop("cannot read date from: ", currentFile)
}

tDF <- arrow::read_parquet(currentFile) |> 
  select(-c(store_and_fwd_flag))

# no need to drop fields ?


# clean up
tDF_clean <- filter(tDF, tpep_pickup_datetime >= ymd(paste0(currentDate, "-01")), 
                    tpep_dropoff_datetime < ymd(paste0(currentDate, "-01")) %m+% months(1))

count_removed_rows <- nrow(tDF) - nrow(tDF_clean)
message("Number of rows removed due to wrong dates: ", count_removed_rows)
remove(count_removed_rows, tDF)

# trip dates
tDF_clean$trip_start_date <- lubridate::as_date(tDF_clean$tpep_pickup_datetime)
tDF_clean$trip_start_month <- format.Date(tDF_clean$trip_start_date , format = "%Y-%m")
tDF_clean$trip_start_year <-  format.Date(tDF_clean$trip_start_date , format = "%Y")

# monthly aggregations
group_by(tDF_clean, tDF_clean$trip_start_month) |> 
  summarise(total_number_trips = n(), 
            total_distance = sum(trip_distance),
            total_charges = sum(total_amount),
            total_fare = sum(fare_amount),
            total_tip = sum(tip_amount),
            total_tolls = sum(tolls_amount),
            total_passenger_count = sum(passenger_count)
            ) 


grouping_values <- c("VendorID", "payment_type", "RatecodeID", "PULocationID", "DOLocationID")

fn_monthly_aggregatotion <- function(df, grouping_values = c()) {
 
  tmp <- group_by(df, across(all_of(grouping_values))) |> 
    mutate(other_charges = extra + mta_tax + tolls_amount + improvement_surcharge + congestion_surcharge + Airport_fee + cbd_congestion_fee) |> 
    summarise(total_number_trips = n(), 
              total_distance = sum(trip_distance),
              total_charges = sum(total_amount),
              total_fare = sum(fare_amount),
              total_tip = sum(tip_amount),
              total_other_charges = sum(other_charges), 
              total_passenger_count = sum(passenger_count),
              .groups = "drop"
    ) 
  
    if("trip_start_date" %in% names(tmp)) {
      tmp |> 
        rename(date = "trip_start_date") |> 
        mutate(full_month_aggregation = FALSE) |> 
        select(date, full_month_aggregation, everything()) |> 
        arrange(date)
        
    } else {
      # date just needs to be the YM so grab that from any row
      currentDate <- format(df$trip_start_date[1], "%Y-%m")
      
      tmp |> 
        mutate(date = ym(currentDate), full_month_aggregation = TRUE) |> 
        select(date, full_month_aggregation, everything())
    }
}

t2 <- fn_monthly_aggregatotion(tDF_clean, grouping_values)
t3 <- fn_monthly_aggregatotion(tDF_clean, append(grouping_values, "trip_start_date"))

combined_summary_df <- group_by(tDF_clean, across(all_of(grouping_values))) |> 
  mutate(other_charges = extra + mta_tax + tolls_amount + improvement_surcharge + congestion_surcharge + Airport_fee + cbd_congestion_fee) |> 
  summarise(total_number_trips = n(), 
            total_distance = sum(trip_distance),
            total_charges = sum(total_amount),
            total_fare = sum(fare_amount),
            total_tip = sum(tip_amount),
            total_other_charges = sum(other_charges), 
            total_passenger_count = sum(passenger_count),
            .groups = "drop"
  ) |> 
  mutate(date = ym(currentDate), full_month_aggregation = TRUE) |> 
  select(date, full_month_aggregation, everything())

grouping_values <- append(grouping_values, "trip_start_date")

t <- group_by(tDF_clean, across(all_of(grouping_values))) |> 
  mutate(other_charges = extra + mta_tax + tolls_amount + improvement_surcharge + congestion_surcharge + Airport_fee + cbd_congestion_fee) |> 
  summarise(total_number_trips = n(), 
            total_distance = sum(trip_distance),
            total_charges = sum(total_amount),
            total_fare = sum(fare_amount),
            total_tip = sum(tip_amount),
            total_other_charges = sum(other_charges), 
            total_passenger_count = sum(passenger_count),
            .groups = "drop"
  ) |> 
  rename(date = "trip_start_date") |> 
  mutate(full_month_aggregation = FALSE) |> 
  select(date, full_month_aggregation, everything()) |> 
  arrange(date)

combined_summary_df <- as_tibble(data.table::rbindlist(list(combined_summary_df, t)))
