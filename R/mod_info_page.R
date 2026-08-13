################################################################################
### FILE: R/mod_info_page.R
################################################################################

#' Info Page Module UI
mod_info_page_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Info Page",
    icon = icon("list-alt"),
    card(
      card_header("App Info"),
      tags$p(tags$b("APP NAME:"), textOutput(ns("ui_text_appName"), inline = TRUE)),
      tags$p(tags$b("APP VERSION:"), textOutput(ns("ui_text_appVersion"), inline = TRUE)),
      tags$p(tags$b("DATA LOCATION:"), textOutput(ns("ui_text_dataPath"), inline = TRUE)),
      tags$p(tags$b("LIVE APP:"), textOutput(ns("ui_text_liveApp"), inline = TRUE)),
      hr(),
      tags$p(tags$b("PACKAGE CACHE:"), textOutput(ns("ui_text_renv"), inline = TRUE)),
      tags$p(tags$b("APP CACHE:"), textOutput(ns("ui_text_cache"), inline = TRUE))
    )
  )
}

#' Info Page Module Server
mod_info_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # These read directly from the global environment established in global.R
    output$ui_text_appName <- renderText({
      appTitle
    })
    output$ui_text_appVersion <- renderText({
      appVersion
    })
    output$ui_text_dataPath <- renderText({
      dataPath
    })
    output$ui_text_liveApp <- renderText({
      isLiveVersion
    })
    output$ui_text_renv <- renderText({
      renv::paths$cache()
    })
    output$ui_text_cache <- renderText({
      if (Sys.getenv("DEPLOY_ENV") == "DEVELOPER") {
        return("In memory cache")
      }
      cachePath
    })
  })
}
