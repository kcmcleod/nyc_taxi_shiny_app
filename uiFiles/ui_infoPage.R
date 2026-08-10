nav_panel(
  title = "Info Page",
  icon = icon("list-alt"),
  card(
    card_header("App Info"),
    tags$p(tags$b("APP NAME:"), textOutput("ui_text_appName", inline = TRUE)),
    tags$p(tags$b("APP VERSION:"), textOutput("ui_text_appVersion", inline = TRUE)),
    tags$p(tags$b("DATA LOCATION:"), textOutput("ui_text_dataPath", inline = TRUE)),
    tags$p(tags$b("LIVE APP:"), textOutput("ui_text_liveApp", inline = TRUE)),
    hr(),
    tags$p(tags$b("PACKAGE CACHE:"), textOutput("ui_text_renv", inline = TRUE)),
    tags$p(tags$b("APP CACHE:"), textOutput("ui_text_cache", inline = TRUE))
  )
)
