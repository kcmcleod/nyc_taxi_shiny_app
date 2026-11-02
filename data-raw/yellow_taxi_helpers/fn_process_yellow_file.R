fn_process_yellow_file <- function(fullFileName) {
  currentDate <- str_match(fullFileName, pattern = "\\d{4}-\\d{2}")[1]

  if (is.na(currentDate) || is.na(fullFileName)) {
    log_error(".... cannot read date from: ", fullFileName)
    return(NULL)
  }

  tDF <- read_parquet(fullFileName) |>
    select(-c(store_and_fwd_flag))

  # clean up
  valid_start <- ymd(paste0(currentDate, "-01"))
  valid_end <- ymd(paste0(currentDate, "-01")) %m+% months(1)

  tDF_clean_dates <- mutate(
    tDF,
    tpep_pickup_datetime = if_else(
      tpep_pickup_datetime < valid_start | tpep_pickup_datetime >= valid_end,
      NA_POSIXct_,
      tpep_pickup_datetime
    ),
    tpep_dropoff_datetime = if_else(
      tpep_dropoff_datetime < valid_start | tpep_dropoff_datetime >= valid_end,
      NA_POSIXct_,
      tpep_dropoff_datetime
    )
  )

  count_mutated_rows <- sum(is.na(tDF_clean_dates$tpep_pickup_datetime) |
    is.na(tDF_clean_dates$tpep_dropoff_datetime))
  log_info(".... number of rows with wrong dates: ", count_mutated_rows)
  remove(count_mutated_rows, tDF)

  tDF_clean <- drop_na(tDF_clean_dates, c("tpep_pickup_datetime", "tpep_dropoff_datetime")) |>
    mutate(
      tmp_total = trip_distance + fare_amount,
      cpm = fare_amount / trip_distance,
      cpm = if_else(is.infinite(cpm), 0, cpm)
    ) |>
    filter(
      # removing ultra short duration
      tpep_pickup_datetime %m+% seconds(60) < tpep_dropoff_datetime,
      # removing long duration
      interval(tpep_pickup_datetime, tpep_dropoff_datetime) <= hours(6),
      # negative mile journeys
      trip_distance >= 0,
      # taxis have a limit of 8 passengers
      (passenger_count >= 0 & passenger_count < 9),
      # either distance or fare must be above 0
      tmp_total > 0,
      # removing ultra low cpm
      !(trip_distance > 5 & cpm < 0.5),
      # # can be super high cost if very low distance
      !(trip_distance < 1 & cpm > 100),
      # short distance capped at 20 per mile
      !(trip_distance >= 1 & trip_distance <= 10 & cpm > 20),
      # rest capped at 10 per mile
      !(trip_distance >= 10 & cpm > 10)
    ) |>
    select(-tmp_total)

  tDF_clean <- filter(
    tDF_clean,
    !is.na(tpep_pickup_datetime),
    !is.na(tpep_dropoff_datetime)
  )

  remove(tDF_clean_dates)

  # trip dates
  tDF_clean$trip_start_date <- lubridate::as_date(tDF_clean$tpep_pickup_datetime)
  tDF_clean$trip_start_week <- floor_date(tDF_clean$tpep_pickup_datetime,
    unit = "weeks", week_start = 7
  )
  tDF_clean$trip_start_week <- format(tDF_clean$trip_start_week, "%Y-%m-%d")
  tDF_clean$trip_start_month <- format.Date(tDF_clean$trip_start_date, format = "%Y-%m")
  tDF_clean$trip_start_year <- format.Date(tDF_clean$trip_start_date, format = "%Y")
  tDF_clean$reporting_month <- valid_start

  # cbd_congestion_fee is missing on older data
  if (!"cbd_congestion_fee" %in% names(tDF_clean)) {
    tDF_clean$cbd_congestion_fee <- 0.00
  }

  # Airport_fee might be missing or changed Capitalisation
  if ("airport_fee" %in% names(tDF_clean) && !"Airport_fee" %in% names(tDF_clean)) {
    tDF_clean <- dplyr::rename(tDF_clean, Airport_fee = airport_fee)
  }
  if (!"Airport_fee" %in% names(tDF_clean)) {
    tDF_clean$Airport_fee <- 0.00
  }

  # aggregations setup
  expected_cols <- c(
    "full_month_aggregation", "full_week_aggregation", "date", "VendorID", "payment_type",
    "RatecodeID", "PULocationID", "DOLocationID", "total_number_trips",
    "total_distance", "total_charges", "total_fare", "total_tip",
    "total_other_charges", "total_passenger_count"
  )

  grouping_values <- c(
    "VendorID", "payment_type", "RatecodeID",
    "PULocationID", "DOLocationID"
  )

  # t2 aggregates the full month and t3 is per day. t4 is per week (week starts on SUN)
  t2 <- fn_monthly_aggregation(tDF_clean, grouping_values)
  t3 <- fn_monthly_aggregation(tDF_clean, append(grouping_values, "trip_start_date"))
  t4 <- fn_monthly_aggregation(tDF_clean, append(grouping_values, "trip_start_week"))

  # validating
  continue_flag <- TRUE
  if (!fn_validate_columns(t2, expected_cols, "T2")) continue_flag <- FALSE
  if (!fn_validate_columns(t3, expected_cols, "T3")) continue_flag <- FALSE
  if (!fn_validate_columns(t4, expected_cols, "T4")) continue_flag <- FALSE

  if (!continue_flag) {
    log_error(".... STOPPING due to column issues in aggregated data")
    return(NULL)
  }

  combined_summary_df <- as_tibble(data.table::rbindlist(list(t2, t3, t4)))
  remove(t2, t3, t4)
  log_info(
    ".... returning comnbined dataset: ", ncol(combined_summary_df), " x ",
    nrow(combined_summary_df)
  )

  return(combined_summary_df)
}
