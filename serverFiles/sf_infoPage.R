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
