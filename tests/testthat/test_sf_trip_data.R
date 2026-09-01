library(shinytest2)
library(testthat)

test_that("sf_trip_data - main chart", {
  testthat::local_edition(3)

  app <- AppDriver$new()

  app$wait_for_idle()

  # 1. Test the default state
  app_data <- app$get_value(export = "yt_parent-yt_main-exported_main_chart_data")
  expect_true(!is.null(app_data))
  expect_snapshot_value(app_data, style = "json2")

  # 2. not default
  app$set_inputs(
    "yt_parent-yt_main-pi_level" = "Month",
    "yt_parent-yt_main-pi_vendor" = "Myle Technologies Inc",
    "yt_parent-yt_main-pi_pay_type" = "Flex Fare trip"
  )
  Sys.sleep(1.5)
  app$wait_for_idle()

  app_data <- app$get_value(export = "yt_parent-yt_main-exported_main_chart_data")
  expect_true(!is.null(app_data))
  expect_snapshot_value(app_data, style = "json2")
})


test_that("sf_trip_data - heatmap chart", {
  testthat::local_edition(3)

  app <- AppDriver$new()

  app$wait_for_idle()

  # 1. Test the default state
  app_data <- app$get_value(export = "yt_parent-yt_heat-exported_heatmap_data")
  expect_true(!is.null(app_data))
  expect_snapshot_value(app_data, style = "json2")

  # 2. not default
  app$set_inputs(
    "yt_parent-yt_heat-ri_heatmap_period" = "Combined",
    "yt_parent-di_date_range" = c("2026-01-01", "2026-03-31")
  )
  Sys.sleep(1.5)
  app$wait_for_idle()

  print(app$get_logs())
  app_data <- app$get_value(export = "yt_parent-yt_heat-exported_heatmap_data")
  expect_true(!is.null(app_data))
  expect_snapshot_value(app_data, style = "json2")
})


test_that("sf_trip_data - table", {
  testthat::local_edition(3)

  app <- AppDriver$new()

  app$wait_for_idle()

  # 1. Test the default state
  app_data <- app$get_value(export = "yt_parent-yt_table-exported_table_data")
  expect_true(!is.null(app_data))
  expect_snapshot_value(app_data, style = "json2")

  # 2. not default
  app$set_inputs(
    "yt_parent-di_date_range" = c("2026-01-01", "2026-03-31")
  )
  Sys.sleep(1.5)
  app$wait_for_idle()

  print(app$get_logs())
  app_data <- app$get_value(export = "yt_parent-yt_table-exported_table_data")
  expect_true(!is.null(app_data))
  expect_snapshot_value(app_data, style = "json2")
})


test_that("sf_trip_data - 365 day guardrail snaps end date back", {
  testthat::local_edition(3)

  app <- AppDriver$new()
  app$wait_for_idle()

  app$set_inputs(
    "yt_parent-di_date_range" = c("2025-01-01", "2026-03-01")
  )
  Sys.sleep(1.5)
  app$wait_for_idle()

  updated_dates <- app$get_value(input = "yt_parent-di_date_range")

  expect_equal(as.character(updated_dates[1]), "2025-01-01")
  expect_equal(as.character(updated_dates[2]), "2026-01-01")
})
