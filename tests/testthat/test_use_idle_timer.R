library(testthat)
library(shiny)

test_that("use_idle_timer returns a valid head tag structure with correct children", {
  res <- use_idle_timer(15)
  
  # Assert it returns a shiny tag object representing a <head> tag
  expect_true(inherits(res, "shiny.tag"))
  expect_equal(res$name, "head")
  expect_length(res$children, 2)
  
  # Verify the external script reference in the second child
  external_script <- res$children[[2]]
  expect_equal(external_script$name, "script")
  expect_equal(external_script$attribs$src, "idle_time_out.js")
})

test_that("use_idle_timer correctly calculates timeout milliseconds", {
  
  # Helper to extract the inline script content cleanly
  get_script_content <- function(tag_obj) {
    paste(as.character(tag_obj$children[[1]]$children[[1]]), collapse = "")
  }
  
  # 1. Test default 15 minutes -> 900,000 ms
  res_default <- use_idle_timer(15)
  content_default <- get_script_content(res_default)
  expect_true(grepl("900000", content_default))
  
  # 2. Test custom input (e.g. 5 minutes -> 300,000 ms)
  res_custom <- use_idle_timer(5)
  content_custom <- get_script_content(res_custom)
  expect_true(grepl("300000", content_custom))
})

test_that("use_idle_timer rounds fractional minutes correctly into integers", {
  res <- use_idle_timer(0.5)
  content <- paste(as.character(res$children[[1]]$children[[1]]), collapse = "")
  
  expect_true(grepl("30000", content))
})