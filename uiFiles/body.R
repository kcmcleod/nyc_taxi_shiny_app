app_theme <- create_theme(
  adminlte_color(
    light_blue = config$colours$theme$primary_accent, 
    aqua = config$colours$theme$secondary_accent
  ),
  
  adminlte_global(
    content_bg = config$colours$theme$body_bg
  )
)

listOfUIFiles <- function(folderPath, useRecursive = TRUE) {
  fileList <- list.files(folderPath, full.names = TRUE, pattern = "\\.R$", 
                         recursive = useRecursive)
  
  return(fileList[!grepl(pattern = "/sidebar.R|body.R|header.R", fileList)])
}

sourceUIFilesFromFolder <- function(folderPath) {
  return(lapply(listOfUIFiles(folderPath), function(x) source(x, local = TRUE)$value))
}


getTabItems <- function() {
  return(
    do.call(
      tabItems,
      sourceUIFilesFromFolder(uiFilesPath)
    )
  )
}

body <- dashboardBody(
  use_theme(app_theme),
  tags$style(
    HTML(".main-sidebar { font-size: 12px!important; }
         .treeview-menu>li>a { font-size: 12px!important; }")
  ),
  getTabItems(),
  tags$footer(
    align = "center",
    em("Built by Kenneth McLeod using data from ",
       a(
         href = "https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page", 
         "NYC TLC", 
         target = "_blank")
    )
  )
)
