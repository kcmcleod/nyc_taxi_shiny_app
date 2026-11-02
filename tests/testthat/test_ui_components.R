library(testthat)
library(shiny)
library(bslib)
library(shinycssloaders) # To ensure withSpinner resolves correctly

test_that("Module UI functions render without crashing", {
  # MOCK CONFIG
  mock_config <- list(
    colours = list(
      theme = list(primary_accent = "#000000"),
      icons = list(taxi = "#F7B731")
    ),
    ui_values = list(
      pi_level = c("Day", "Week", "Month"),
      metric_type = c("Journey Count", "Total Distance"),
      heatmap_level = c("Combined", "Weekly", "Monthly")
    )
  )

  #  MOCK METADATA
  mock_metadata <- list(
    Vendor = c("VTS", "CMT", "Unknown"),
    payment_type = c("Cash", "Credit card", "Dispute"),
    Location = c("Manhattan", "Brooklyn", "Queens"),
    min_date = as.Date("2023-01-01"),
    max_date = as.Date("2023-12-31")
  )

  # UI RENDERING TESTS
  expect_s3_class(
    mod_info_page_ui("info"),
    "shiny.tag"
  )

  expect_s3_class(
    mod_yellow_taxis_parent_ui("parent", mock_config, mock_metadata),
    "shiny.tag"
  )

  expect_s3_class(
    mod_yellow_taxis_main_ui("main", mock_config, mock_metadata),
    "shiny.tag.list"
  )

  expect_s3_class(
    mod_yellow_taxis_heatmap_ui("heatmap", mock_config),
    "shiny.tag.list"
  )

  expect_s3_class(
    mod_yellow_taxis_table_ui("table", mock_config, mock_metadata),
    "shiny.tag.list"
  )
})

test_that("mod_info_page_server initializes correctly", {
  testServer(mod_info_page_server, args = list(), {
    expect_true(TRUE)
  })
})
