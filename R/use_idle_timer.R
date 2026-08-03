#' Add Idle Timer to Shiny UI
#' @param timeout_minutes The number of minutes before the session times out
use_idle_timer <- function(timeout_minutes = 15) {
  
  # Convert minutes to milliseconds for JavaScript
  timeout_ms <- timeout_minutes * 60 * 1000
  
  shiny::tagList(
    # 1. Pass the custom timeout value from R to JavaScript
    shiny::tags$script(HTML(sprintf("window.shinyIdleTimeout = %d;", timeout_ms))),
    
    # 2. Load the external script from your www/ folder
    shiny::tags$script(src = "idle_time_out.js")
  )
}