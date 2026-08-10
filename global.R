################################################################################
# PACKAGES
suppressPackageStartupMessages(library("tidyverse"))

suppressPackageStartupMessages(library("shinyjs"))

suppressPackageStartupMessages(library("bslib"))
suppressPackageStartupMessages(library("shinydisconnect"))

suppressPackageStartupMessages(library("plotly"))
suppressPackageStartupMessages(library("logger"))
suppressPackageStartupMessages(library("arrow"))
suppressPackageStartupMessages(library("shinyWidgets"))

suppressPackageStartupMessages(library("cachem"))

suppressPackageStartupMessages(library("shinytoastr"))
suppressPackageStartupMessages(library("shinycssloaders"))
suppressPackageStartupMessages(library("scales"))


################################################################################
# PATHS
codePath <- getwd()
uiFilesPath <- paste0(codePath, "/uiFiles/")
serverFilesPath <- paste0(codePath, "/serverFiles/")
dataPath <- paste0(Sys.getenv("HOME"), "/data/shiny_data/")
cachePath <- paste0(dataPath, "shared_app_cache")


################################################################################
# CACHE

if (!exists("offline_data_prep")) {
  # no need to have cache if only doing local data processing


  if (Sys.getenv("DEPLOY_ENV") == "DEVELOPER") {
    shared_app_cache <- cachem::cache_mem()

    log_info("Using temporary memory cache")
  } else {
    shared_app_cache <- cachem::cache_disk(
      dir = cachePath,
      max_size = 1024 * 1024^2 # 1GB limit
    )

    log_info("Deployment detected: Persistent disk cache enabled")
  }

  shinyOptions(cache = shared_app_cache)
}


################################################################################
# APP INFO
appTitle <- "NYC Taxi"

isLiveVersion <- grepl("live", codePath)

if (isLiveVersion) {
  appVersion <- paste0(
    appTitle,
    system("git describe --tags --abbrev=0", intern = TRUE)
  )

  appVersion <- gsub("-v", "-", appVersion)

  folder <- "run"
} else {
  res <- stringr::str_match(codePath, "[0-9]+$")[1, 1]

  if (is.na(res)) {
    appVersion <- paste0(appTitle, "-Dev")
  } else {
    appVersion <- paste0(appTitle, "-Beta_", res)
  }

  folder <- "build"
}

dataPath <- paste0(dataPath, folder, "/", appTitle, "/")
if (!dir.exists(dataPath)) {
  dir.create(dataPath, recursive = TRUE)
}

logPath <- paste0(dataPath, "logs/")
if (!dir.exists(logPath)) {
  dir.create(logPath, recursive = TRUE)
}

################################################################################
# SOURCE FILES

listOfFiles <- function(folderPath, useRecursive = TRUE) {
  return(list.files(folderPath, full.names = TRUE, pattern = "\\.R$", recursive = useRecursive))
}

sourceRFilesFromFolder <- function(folderPath) {
  return(sapply(listOfFiles(folderPath), function(x) source(x)))
}

# disable autoload of R folder using: touch R/_disable_autoload.R
sourceRFilesFromFolder(paste0(codePath, "/R"))

config <- yaml::yaml.load_file(paste0(codePath, "/config/config.yml"), eval.expr = TRUE)

################################################################################
# DATA

# no need to load data if only doing local data processing
if (!exists("offline_data_prep")) {
  # Load the pre-computed metadata
  app_metadata <- get_initial_taxi_metadata(data_path = paste0(getwd(), "/app_data/"))

  # save aws bill if running locally
  if (Sys.getenv("DEPLOY_ENV") == "DEVELOPER") {
    raw_taxi_data <- get_initial_taxi_data(data_path = dataPath)
  } else {
    raw_taxi_data <- get_initial_taxi_data_aws()
  }
}

# # might move to database in future
# db_pool <- pool::dbPool(
#   drv = RPostgres::Postgres(), # Or whichever driver you use (e.g., odbc::odbc())
#   dbname = "taxi_db",
#   host = "localhost",
#   user = "your_user",
#   password = Sys.getenv("DB_PASSWORD"),
#
#   minSize = 2,  # Keep 2 connections open at all times
#   maxSize = 10, # Allow up to 10 simultaneous queries during a traffic spike
#   idleTimeout = 60000 # Close idle connections after 60 seconds
# )
#
# onStop(function() {
#   pool::poolClose(db_pool)
# })
