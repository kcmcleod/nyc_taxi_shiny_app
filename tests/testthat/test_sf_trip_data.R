library(shinytest2)
library(testthat)

testthat::local_edition(3)

test_that("sf_trip_data - main chart", {
  
  app <- AppDriver$new()
  
  app$wait_for_idle()
  
  # 1. Test the default state
  app_data <- app$get_value(export = "exported_main_table_data")
  expect_equal(nrow(app_data), 14)
  expect_snapshot_value(app_data, style = "json2")

  # 2. not default
  app$set_inputs(pi_level = "Month", pi_vendor = "Myle Technologies Inc",
                 pi_pay_type = "Flex Fare trip")
  
  app$wait_for_idle()
  
  app_data <- app$get_value(export = "exported_main_table_data")
  expect_equal(nrow(app_data), 3)
  expect_snapshot_value(app_data, style = "json2")
})


test_that("sf_trip_data - heatmap chart", {
  
  app <- AppDriver$new()
  
  app$wait_for_idle()
  
  # 1. Test the default state
  app_data <- app$get_value(export = "exported_heatmap_data")
  expect_equal(nrow(app_data), 82)
  expect_snapshot_value(app_data, style = "json2")
  
  # 2. not default
  app$set_inputs(ri_heatmap_period = "Combined", di_start_date = "2026-01-01")
  
  app$wait_for_idle()
  
  print(app$get_logs())
  app_data <- app$get_value(export = "exported_heatmap_data")
  expect_snapshot_value(app_data, style = "json2")
})