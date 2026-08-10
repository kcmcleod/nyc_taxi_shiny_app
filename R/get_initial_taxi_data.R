get_initial_taxi_data <- function(data_path) {
  if (isTRUE(getOption("shiny.testmode"))) {
    logger::log_info(".... loading TEST data !")
    return(base::readRDS("tests/testdata/mock_taxi_trips.rds"))
  }

  latest_yellow_file <- file.path(data_path, "yellow_aggregation_partitioned")

  if (!dir.exists(latest_yellow_file)) {
    logger::log_error("No yellow taxi data found")
    return(NULL)
  }

  logger::log_info("READING DATA FROM ", latest_yellow_file)
  tDF <- arrow::open_dataset(latest_yellow_file)
  logger::log_info(".... ", nrow(tDF), " yellow rows read")

  return(tDF)
}

get_initial_taxi_data_aws <- function() {
  # TEST MODE: Automatically triggered by shinytest2
  if (isTRUE(getOption("shiny.testmode"))) {
    logger::log_info(".... loading TEST data !")
    return(base::readRDS("tests/testdata/mock_taxi_trips.rds"))
  }

  # PROD MODE

  bucket_name <- Sys.getenv("TAXI_S3_BUCKET")
  object_key <- Sys.getenv("TAXI_S3_OBJECT")

  if (bucket_name == "" || object_key == "") {
    logger::log_error("S3 Bucket or Object Key missing from environment variables.")
    stop("Missing required AWS configuration.")
  }

  # Create the specific URI format Arrow requires
  s3_uri <- paste0("s3://", bucket_name, "/", object_key)
  logger::log_info("READING DATA FROM AWS S3: ", s3_uri)

  tDF <- tryCatch(
    {
      # Arrow natively connects to S3 using the keys in your .Renviron
      arrow::open_dataset(s3_uri)
    },
    error = function(e) {
      logger::log_error("Failed to fetch dataset from AWS S3: ", e$message)
      return(NULL)
    }
  )

  if (is.null(tDF)) {
    return(NULL)
  }

  logger::log_info(".... successfully mapped S3 dataset via Arrow")

  return(tDF)
}


get_initial_taxi_metadata <- function(data_path) {
  if (isTRUE(getOption("shiny.testmode"))) {
    logger::log_info(".... loading TEST metadata !")
    return(base::readRDS("tests/testdata/yellow_metadata.rds"))
  }

  all_yellow_metadata <- list.files(data_path, pattern = "yellow.*rds", full.names = TRUE)
  latest_yellow_metadata <- sort(all_yellow_metadata, decreasing = TRUE)[1]

  if (is.na(latest_yellow_metadata)) {
    logger::log_error("No yellow taxi metadata found")
    return(NULL)
  }

  logger::log_info("READING DATA FROM ", latest_yellow_metadata)
  return(base::readRDS(latest_yellow_metadata))
}
