# install.packages("peakRAM")
library(peakRAM)
library(arrow)
library(dplyr)

# Point to your local partitioned dataset
dataPath <- "/Users/kcm/data/shiny_data/build/NYC Taxi/"
partitioned_parquet <- file.path(dataPath, "yellow_aggregation_partitioned")

# 1. The Optimised Arrow Pipeline (Your architecture)
# Filters and aggregates on disk, only pulling the tiny result into memory
run_arrow_pipeline <- function() {
  arrow::open_dataset(partitioned_parquet) |>
    dplyr::filter(
      full_month_aggregation == TRUE,
      full_week_aggregation == FALSE
    ) |>
    dplyr::group_by(PULocation, DOLocation) |>
    dplyr::summarise(trips = sum(total_number_trips, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect()
}

# 2. The Naive In-Memory Pipeline (Legacy approach)
# Pulls the full multi-GB dataset into R RAM first, then filters
run_naive_pipeline <- function() {
  df <- arrow::open_dataset(partitioned_parquet) |>
    dplyr::collect() # <--- The fatal memory spike happens here

  df |>
    dplyr::filter(
      full_month_aggregation == TRUE,
      full_week_aggregation == FALSE
    ) |>
    dplyr::group_by(PULocation, DOLocation) |>
    dplyr::summarise(trips = sum(total_number_trips, na.rm = TRUE), .groups = "drop")
}

# 3. Execute the Profiler
# Note: This will take a moment as it forces garbage collection between runs
memory_profile <- peakRAM::peakRAM(
  Arrow_Pipeline = run_arrow_pipeline(),
  Naive_Pipeline = run_naive_pipeline()
)

print(memory_profile)
