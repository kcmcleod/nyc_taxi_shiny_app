library(testthat)
library(shiny)

test_that("mod_info_page_server initializes correctly", {
  testServer(mod_info_page_server, args = list(), {
    # If the module has no reactive outputs, just expecting it to run without error is enough.
    expect_true(TRUE)
  })
})
