library(shinytest2)

################################################################################
# SETUP

# Sets up the necessary infrastructure and 'tests/testthat/' folder
use_shinytest2()



################################################################################
# CREATE TESTS

# create tests
record_test()

# if snaps get really large, eg MBs, switch from app$expect_values() to app$get_value()
# plotly_data <- app$get_value(output = "po_tripVolumesOverTime")
# expect_true(length(plotly_data$x$data) > 0) 

# run tests
test_app()



################################################################################
# UPDATE SNAPS
testthat::snapshot_accept()