file.rename("renv.lock", "renv_hidden.lock")

rsconnect::deployApp(
  appFiles = c(
    "app.R",
    "global.R",
    ".Renviron",
    ".Rprofile",
    "config",
    "app_data",
    "serverFiles",
    "R",
    "www",
    "data"
  )
)

file.rename("renv_hidden.lock", "renv.lock")
