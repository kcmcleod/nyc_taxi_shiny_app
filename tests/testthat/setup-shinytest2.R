# Force test mode globally for all shinytest2 runs
options(shiny.testmode = TRUE)

# Load application support files into testing environment
shinytest2::load_app_support(test_path("../.."))
