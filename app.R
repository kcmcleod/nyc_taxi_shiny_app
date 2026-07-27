if (interactive()) sink(stderr(), type = "output")

source("global.R")

server <- function(input, output, session) {
  listOfServerFiles <- listOfFiles(serverFilesPath)
  
  for(file in listOfServerFiles) {
    source(file, local = TRUE)
  }
}

source(paste0(uiFilesPath, "sidebar.R"))
source(paste0(uiFilesPath, "header.R"))
source(paste0(uiFilesPath, "body.R"))

ui <- bootstrapPage(
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
      text = "Something went wrong. Please refresh the page",
      refresh = "REFRESH",
      background = '#6495ED',
      size = 26,
      width = "full",
      top = "center",
      colour = '#FFFFF7',
      overlayColour = '#D3D3D3',
      overlayOpacity = 0.3,
      refreshColour = '#8B0000',
      css = "padding: 15px !important; box-shadow: none !important;"
    )
  )
)

shinyApp(ui = ui, server = server, onStart = function() {
  message("starting app... ")
})

  