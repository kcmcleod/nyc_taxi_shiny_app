library(shinytest2)

test_that("sf_trip_data - main chart", {
  
  app <- AppDriver$new()
  
  app$wait_for_idle()
  
  # 1. Test the default state
  expect_equal(app$get_value(export = "exported_filtered_rows"), 151)

  # 2. not default
  app$set_inputs(pi_level = "Month")
  app$set_inputs(pi_vendor = "Myle Technologies Inc")
  app$set_inputs(pi_pay_type = "Flex Fare trip")
  
  expect_equal(app$get_value(export = "exported_filtered_rows"), 5)
})