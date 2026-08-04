tabItem(
  tabName = "trip_data_page",
  fluidRow(
    column(
      width = 12,
      box(
        width = 12,
        title = "DATE RANGE",
        status = 'primary',
        solidHeader = TRUE,
        fluidRow(
          column(
            width = 12,
            tags$p("Controls the date range for all the charts/tables in this page")
          )
        ),
        fluidRow(
          column(
            width = 3,
            offset = 3,
            disabled(dateInput("di_start_date", label = "Start Date"))
          ),
          column(
            width = 3,
            disabled(dateInput("di_end_date", label = "End Date"))
          )
        )
      )
    )
  ),
  tabsetPanel(
    id = "yellow_data_panel",
    tabPanel(
      title = "VOLUMES",  
      fluidRow(
        column(
          width = 12,
          box(
            width = 12,
            title = "Options",
            status = "info",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 3,
                pickerInput("pi_level", 
                            label = "Granularity:", 
                            multiple = FALSE, 
                            choices = c("Day", "Week", "Month"),
                            selected = "Week"
                )
              ),
              column(
                width = 3,
                disabled(
                  pickerInput(
                    "pi_vendor", 
                    label = "Vendors:", 
                    multiple = TRUE, 
                    choices = c(), 
                    options = pickerOptions(
                      `actions-box` = TRUE
                    )))
              ),
              column(
                width = 3,
                disabled(
                  pickerInput(
                    "pi_pay_type", 
                    label = "Payment:", 
                    multiple = TRUE, 
                    choices = c(), 
                    options = pickerOptions(
                      `actions-box` = TRUE
                    )))
              ),
              column(
                width = 3,
                radioButtons("rb_journeys_or_passengers", 
                             label = "Y axis:",
                             choices = c("Journey Count", "Total Distance")
                )
              )
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
    ),
    tabPanel(
      title = "JOURNEY PICKUP + DROP OFF",  
      fluidRow(
        column(
          width = 12,
          box(
            width = 12,
            title = "Options",
            status = "info",
            solidHeader = TRUE,
            radioButtons(
              "ri_heatmap_period",
              label = "Heatmaps period:",
              choices = c("Combined", "Weekly", "Monthly"),
              selected = "Weekly", 
              inline = TRUE
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          br(),
          div(
            style = "overflow-y: auto; max-height: 70vh; overflow-x: hidden;",
            withSpinner(
              plotlyOutput("po_heatmap", height = 'auto'),
              type = 2,
              color.background = config$colours$icons$taxi,
              color = config$colours$theme$primary_accent,
              size = 0.5
            )
          )
        ) 
      )
    ),
    tabPanel(
      title = "TABLES",
      fluidRow(
        column(
          width = 12,
          box(
            width = 12,
            title = "Options",
            status = "info",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 12,
                tags$p("TBC")
              )
            )
          )
        )
      )
    )
  )
)