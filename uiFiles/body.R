app_theme <- create_theme(
  # 1. Map your primary and secondary colours
  # shinydashboard uses "light_blue" as the default primary accent 
  adminlte_color(
    light_blue = "#00539F", # Replaces bs_theme 'primary'
    aqua = "#6C757D"        # Replaces bs_theme 'secondary'
  ),
  
  # 2. Map your background and text colours
  adminlte_global(
    content_bg = "#F8F9FA", # Replaces bs_theme 'bg'
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
  getTabItems()
)
