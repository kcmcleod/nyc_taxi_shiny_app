# generate sample of real data for tests

source(paste0(getwd(), "/global.R"))

cat("Sampling dataset...\n")
mock_data <- raw_taxi_data |>
  group_by(date, Vendor, payment_type, full_month_aggregation, full_week_aggregation) |>
  slice_sample(n = 1) |> # Take 1 random row per unique combination
  ungroup()

cat("Mock dataset size:", nrow(mock_data), "rows\n")

saveRDS(mock_data, "tests/testdata/mock_taxi_trips.rds")
