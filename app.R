if (interactive()) sink(stderr(), type = "output")

options(scipen = 999)

source("global.R")

server <- function(input, output, session) {
  listOfServerFiles <- listOfFiles(serverFilesPath)

  for (file in listOfServerFiles) {
    source(file, local = TRUE)
  }

  # Load modules
  mod_yellow_taxis_parent_server(
    "yt_parent", yellow_taxi_data, app_metadata,
    yellow_data_version
  )

  # mod_info_page_server("info_page")


  # Show the warning modal 60 seconds before timeout
  observeEvent(input$idle_warning, {
    showModal(
      modalDialog(
        title = "Session Timeout Warning",
        "Your session will expire in 60 seconds due to inactivity.
        Move your mouse or click anywhere to stay connected.",
        footer = modalButton("I'm still here!"),
        easyClose = TRUE
      )
    )
  })

  # Close the modal automatically if the user moves their mouse
  observeEvent(input$idle_active, {
    removeModal()
  })

  # 3. Kill the session if the final timer hits
  observeEvent(input$idle_timeout, {
    removeModal()
    logger::log_info("User session closed due to inactivity.")
    session$close()
  })
}

app_theme <- bs_theme(
  version = 5,
  bg = config$colours$theme$body_bg,
  fg = "#212529", # Standard dark grey text for high contrast
  primary = config$colours$theme$primary_accent,
  secondary = config$colours$theme$secondary_accent
)

ui <- page_navbar(
  title = appTitle,
  theme = app_theme,
  id = "main_nav",
  fillable = FALSE,
  bg = config$colours$theme$primary_accent,

  # Load Modules
  mod_yellow_taxis_parent_ui("yt_parent", config, app_metadata),

  # mod_info_page_ui("info_page"),

  # Global header components (invisible scripts and modals)
  header = tagList(
    tags$head(
      tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg")
    ),
    use_idle_timer(timeout_minutes = 15),
    useShinyjs(),
    disconnectMessage(
      text = "Your session has timed out due to inactivity.",
      refresh = "Reconnect to NYC Taxi Data",
      background = config$colours$disconnect$background,
      colour = config$colours$disconnect$text,
      overlayColour = config$colours$disconnect$overlay,
      overlayOpacity = 0.75,
      refreshColour = config$colours$disconnect$button
    )
  )
)

shinyApp(ui = ui, server = server, onStart = function() {
  logger::log_info("starting app... ")
})
