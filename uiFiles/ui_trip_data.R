tabItem(
  tabName = "trip_data_page",
  fluidRow(
    column(
      width = 12,
      box(
        width = 12,
        title = "TRIP VOLUMES",
        status = "info",
        solidHeader = TRUE,
        fluidRow(
          # selectors
        ),
        fluidRow(
          plotlyOutput("po_tripVolumesOverTime")
        )
      )
    )
  )
)