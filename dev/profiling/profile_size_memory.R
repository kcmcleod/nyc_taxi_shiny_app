offline_data_prep <- TRUE

source(paste0(getwd(), "/global.R"))

dataPath <- "/Users/kcm/data/shiny_data/build/NYC Taxi/"

# CHECKING FOR FILES TO PROCESS
all_yellow_files <- list.files(
  path = paste0(dataPath, "src"), pattern = "yellow.*.parquet",
  recursive = TRUE, full.names = TRUE
)

row_count <- 0
for (file in all_yellow_files) {
  tDF <- arrow::open_dataset(file)
  row_count <- row_count + nrow(tDF)
  remove(tDF)
}
message(scales::comma(row_count))


partitioned_parquet <- paste0(dataPath, "yellow_aggregation_partitioned")
tDF <- arrow::open_dataset(partitioned_parquet)
row_count <- nrow(tDF)
remove(tDF)
message(scales::comma(row_count))


tDF <- arrow::open_dataset(partitioned_parquet) |>
  dplyr::collect()

# 2. Measure actual memory size
ram_bytes <- object.size(tDF)

# 3. Format into human-readable units (MB / GB)
message("Total in-memory RAM: ", format(ram_bytes, units = "auto"))

# Clean up memory
rm(tDF_full)
gc()
