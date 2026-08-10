library(shinytest2)

test_that("{shinytest2} recording: info_page", {
  app <- AppDriver$new(test_path("../.."),
    name = "info_page", height = 928,
    width = 1619
  )
  app$set_inputs(sidebar_id = "info_page")

  # Wait a fraction of a second for the UI to render the new tab
  app$wait_for_idle()

  app$expect_values(output = "ui_text_appName", screenshot_args = FALSE)
  app$expect_values(output = "ui_text_appVersion", screenshot_args = FALSE)
})


# test_that("{shinytest2} recording: heatmap_default", {
#   app <- AppDriver$new(test_path("../.."), name = "heatmap_default", height = 929,
#       width = 778)
#
#   app$set_inputs(di_start_date = "2026-03-01")
#   app$set_inputs(di_end_date = "2026-05-31")
#   app$set_inputs(yellow_data_panel = "JOURNEY PICKUP + DROP OFF")
#
#   app$wait_for_idle()
#
#   app$expect_values(screenshot_args = FALSE)
# })


# test_that("{shinytest2} recording: heatmap_feb_monthly", {
#   app <- AppDriver$new(test_path("../.."), name = "heatmap_feb_monthly", height = 929,
#       width = 1340)
#
#   app$set_inputs(di_end_date = "2026-05-31")
#   app$set_inputs(di_start_date = "2026-02-01")
#   app$set_inputs(yellow_data_panel = "JOURNEY PICKUP + DROP OFF")
#   app$set_inputs(ri_heatmap_period = "Monthly")
#
#   app$wait_for_idle()
#
#   app$expect_values(screenshot_args = FALSE)
# })
