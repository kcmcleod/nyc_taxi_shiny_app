sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar_id",
    menuItem("Info Page", tabName = "info_page", icon = icon("list-alt")),    
    menuItem("Yellow Taxis", tabName = "trip_data_page", icon = icon("list-taxi"), selected = TRUE)    
))