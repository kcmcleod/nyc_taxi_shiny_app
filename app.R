if (interactive()) sink(stderr(), type = "output")

source("global.R")

server <- function(input, output, session) {
  listOfServerFiles <- listOfFiles(serverFilesPath)
  
  for(file in listOfServerFiles) {
    source(file, local = TRUE)
  }
  
  # 1. Show the warning modal 60 seconds before timeout
  observeEvent(input$idle_warning, {
    showModal(
      modalDialog(
        title = "Session Timeout Warning",
        "Your session will expire in 60 seconds due to inactivity. Move your mouse or click anywhere to stay connected.",
        footer = modalButton("I'm still here!"),
        easyClose = TRUE
      )
    )
  })
  
  # 2. Close the modal automatically if the user moves their mouse
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

source(paste0(uiFilesPath, "sidebar.R"))
source(paste0(uiFilesPath, "header.R"))
source(paste0(uiFilesPath, "body.R"))

ui <- bootstrapPage(
  use_idle_timer(timeout_minutes = 15),                    
  useShinyjs(),
  tagList(
    tags$head(
      # tags$script(type = "text/javascript", src = "sidebar.js")
    ),
    tags$footer(
      align = "center",
      em("Kenneth McLeod")
    ),
    # includeCSS(paste0(getwd(), "/www/style.css")),
    dashboardPage(
      title = appTitle,
      sidebar = sidebar,
      header = header,
      body = body
    ),
    #add_busy_gif(),
    disconnectMessage(
      text = "Your session has timed out due to inactivity.",
      refresh = "Reconnect to NYC Taxi Data",
      background = "#FFC107",        # Classic Taxi Yellow
      colour = "#000000",            # Black text
      overlayColour = "#000000",     # Dark grey/black overlay to dim the frozen app
      overlayOpacity = 0.75,         # 75% opacity makes the app barely visible
      refreshColour = "#333333"
    )
  )
)

shinyApp(ui = ui, server = server, onStart = function() {
  message("starting app... ")
})

