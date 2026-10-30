# Dynamically locate project root whether running from root, tests/, or data-raw/
codePath <- getwd()
while (!dir.exists(file.path(codePath, "config")) && codePath != dirname(codePath)) {
  codePath <- dirname(codePath)
}

app_sources <- list.files(paste0(codePath, "/R"), pattern = "\\.[Rr]$", full.names = TRUE)
dp_sources <- list.files(paste0(codePath, "/data-raw/yellow_taxi_helpers"), pattern = "\\.[Rr]$", full.names = TRUE)
app_sources <- c(app_sources, dp_sources)
app_tests <- list.files(paste0(codePath, "/tests/testthat"), pattern = "\\.[Rr]$", full.names = TRUE)

# filter out shinytest2 etc
unit_tests <- grep("shinytest2|setup", app_tests, value = TRUE, invert = TRUE)
core_sources <- grep("_disable", app_sources, value = TRUE, invert = TRUE)

source(paste0(codePath, "/tests/testthat/setup.R"))

cov_results <- covr::file_coverage(
  source_files = core_sources,
  test_files = unit_tests
)

# save report so it can be seen from make
report_path <- file.path(codePath, "/coverage_report.html")
covr::report(cov_results, file = report_path, browse = FALSE)

if (interactive()) {
  viewer <- getOption("viewer", utils::browseURL)
  viewer(report_path)
} else if (Sys.getenv("CI") == "") {
  sys_name <- Sys.info()[["sysname"]]
  if (sys_name == "Darwin") {
    system2("open", shQuote(report_path), wait = FALSE)
  } else if (sys_name == "Windows") {
    shell.exec(report_path)
  } else if (sys_name == "Linux" && nzchar(Sys.which("xdg-open"))) {
    system2("xdg-open", shQuote(report_path), wait = FALSE)
  }
}
