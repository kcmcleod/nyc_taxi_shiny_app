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
          column(
            width = 3,
            pickerInput("pi_level", 
                        label = "Granularity:", 
                        multiple = FALSE, 
                        choices = c("Day", "Week", "Month"),
                        selected = "Day"
            )
          ),
          column(
            width = 3,
            pickerInput("pi_vendor", 
                        label = "Vendors:", 
                        multiple = TRUE, 
                        choices = c(), 
                        options = pickerOptions(
                          `actions-box` = TRUE
                        ))
          ),
          column(
            width = 3,
            pickerInput("pi_pay_type", 
                        label = "Payment:", 
                        multiple = TRUE, 
                        choices = c(), 
                        options = pickerOptions(
                          `actions-box` = TRUE
                        ))
          ),
          column(
            width = 3,
            radioButtons("rb_journeys_or_passengers", 
                         label = "Y axis:",
                         choices = c("Journey Count", "Total Distance")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            withSpinner(
              plotlyOutput("po_tripVolumesOverTime"),
              type = 2,
              color.background = config$colours$icons$taxi,
              color = config$colours$theme$primary_accent,
              size = 0.5
            )
          )
        )
      )
    )
  )
)