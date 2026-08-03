sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar_id",
    useToastr(), # Initialize the toastr script in the UI
    menuItem("Info Page", tabName = "info_page", icon = icon("list-alt")),    
    menuItem("Yellow Taxis", tabName = "trip_data_page", 
             icon = icon("taxi", style = config$colours$icons$taxi), selected = TRUE)    
))