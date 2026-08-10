################################################################################
# PROFILING
profvis::profvis({
  shiny::runApp()
})


################################################################################
# REACTIVE GRAPH
reactlog::reactlog_enable()

# use the app
shiny::runApp()

# view the graph
shiny::reactlogShow()


################################################################################
# MULTIPLE USERS
# library(shinyloadtest)
#
# # in Terminal run
# R -e 'shinyloadtest::record_session("http://127.0.0.1:6045")'
