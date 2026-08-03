library(shinytest2)

test_that("{shinytest2} recording: main_chart_month_vendor1_cash", {
  app <- AppDriver$new(test_path("../.."), name = "main_chart_month_vendor1_cash", 
      height = 928, width = 1619, variant = shinytest2::platform_variant())
  app$set_inputs(pi_vendor = "Creative Mobile Technologies LLC")
  app$set_inputs(pi_pay_type = "Cash")
  app$set_inputs(pi_level = "Month")
  app$expect_values(output = "po_tripVolumesOverTime", screenshot_args = FALSE)
})


test_that("{shinytest2} recording: main_chart_month_vendor6_flex_flare_distance", {
  app <- AppDriver$new(test_path("../.."), name = "main_chart_month_vendor6_flex_flare_distance", 
      height = 928, width = 1619, variant = shinytest2::platform_variant())
  app$set_inputs(pi_level = "Month")
  app$set_inputs(pi_vendor = "Myle Technologies Inc")
  app$set_inputs(pi_pay_type = "Flex Fare trip")
  app$set_inputs(rb_journeys_or_passengers = "Total Distance")
  app$expect_values(screenshot_args = FALSE)
})



test_that("{shinytest2} recording: info_page", {
  app <- AppDriver$new(test_path("../.."), name = "info_page", height = 928, 
                       width = 1619, variant = shinytest2::platform_variant())
  app$set_inputs(sidebar_id = "info_page")
  
  # Wait a fraction of a second for the UI to render the new tab
  app$wait_for_idle()
  
  app$expect_values(output = "ui_text_appName", screenshot_args = FALSE)
  app$expect_values(output = "ui_text_appVersion", screenshot_args = FALSE)
})
