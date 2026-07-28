################################################################################
# PACKAGES
suppressPackageStartupMessages(library("tidyverse"))


suppressPackageStartupMessages(library("shinyjs"))

suppressPackageStartupMessages(library("shinydashboard"))
suppressPackageStartupMessages(library("bslib"))
suppressPackageStartupMessages(library("fresh"))
suppressPackageStartupMessages(library("shinydisconnect"))

suppressPackageStartupMessages(library("plotly"))
suppressPackageStartupMessages(library("logger"))
suppressPackageStartupMessages(library("arrow"))
suppressPackageStartupMessages(library("shinyWidgets"))



################################################################################
# PATHS
codePath <- getwd()
uiFilesPath <- paste0(codePath, "/uiFiles/")
serverFilesPath <- paste0(codePath, "/serverFiles/")
dataPath <- paste0(Sys.getenv("HOME"), "/data/shiny_data/")


################################################################################
# APP INFO
appTitle <- "NYC Taxi"

isLiveVersion <- grepl("live", codePath)

if(isLiveVersion) {
  appVersion <- paste0(
    appTitle,
    system("git describe --tags --abbrev=0", intern = TRUE)
  )
  
  appVersion <- gsub("-v", "-", appVersion)
  
  folder <- "run"
  
} else {
  res <- stringr::str_match(codePath, "[0-9]+$")[1,1]
  
  if(is.na(res)) {
    appVersion <- paste0(appTitle, "-Dev")
  } else {
    appVersion <- paste0(appTitle, "-Beta_", res)
  }
  
  folder <- "build"
}

dataPath <- paste0(dataPath, folder, "/", appTitle, "/")
if(!dir.exists(dataPath)) {
  dir.create(dataPath)
}

logPath <- paste0(dataPath, "logs/")
if(!dir.exists(logPath)) {
  dir.create(logPath)
}

################################################################################
# SOURCE FILES

listOfFiles <- function(folderPath, useRecursive = TRUE) {
  return(list.files(folderPath, full.names = TRUE, pattern = "\\.R$", recursive = useRecursive))
}

sourceRFilesFromFolder <- function(folderPath) {
  return(sapply(listOfFiles(folderPath), function(x) source(x)))
}

sourceRFilesFromFolder(paste(codePath, "/R"))

config <- yaml::yaml.load_file(paste0(codePath, "/config/config.yml"), eval.expr = TRUE)
