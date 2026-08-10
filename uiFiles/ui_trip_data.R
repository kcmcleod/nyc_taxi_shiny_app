nav_panel(
  title = "Yellow Taxis",
  icon = icon("taxi", style = paste0("color:", config$colours$icons$taxi)),

  # Top Date Range Card
  card(
    fill = FALSE,
    card_header("DATE RANGE"),
    tags$p("Controls the date range for all the charts/tables in this page"),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6),
      disabled(dateInput("di_start_date", label = "Start Date", width = "100%")),
      disabled(dateInput("di_end_date", label = "End Date", width = "100%"))
    )
  ),

  # Main Content Tabs wrapped in a modern card container
  navset_card_tab(
    id = "yellow_data_panel",
    full_screen = TRUE,
    nav_panel(
      title = "VOLUMES",
      card(
        fill = FALSE,
        card_header("Options"),
        layout_columns(
          col_widths = breakpoints(sm = 12, md = 6, xl = 3),
          pickerInput("pi_level",
            label = "Granularity:", multiple = FALSE,
            choices = config$ui_values$pi_level, selected = "Week"
          ),
          disabled(pickerInput("pi_vendor",
            label = "Vendors:", multiple = TRUE,
            choices = c(), options = pickerOptions(`actions-box` = TRUE)
          )),
          disabled(pickerInput("pi_pay_type",
            label = "Payment:", multiple = TRUE,
            choices = c(), options = pickerOptions(`actions-box` = TRUE)
          )),
          radioButtons("rb_journeys_or_passengers",
            label = "Y axis:",
            choices = config$ui_values$metric_type
          )
        )
      ),
      card(
        withSpinner(plotlyOutput("po_tripVolumesOverTime"),
          type = 2,
          color.background = config$colours$icons$taxi,
          color = config$colours$theme$primary_accent, size = 0.5
        )
      )
    ),
    nav_panel(
      title = "JOURNEY PICKUP + DROP OFF",
      card(
        fill = FALSE,
        card_header("Options"),
        radioButtons("ri_heatmap_period",
          label = "Heatmaps period:",
          choices = config$ui_values$heatmap_level, selected = "Weekly", inline = TRUE
        )
      ),
      card(
        style = "overflow-y: auto; max-height: 70vh; overflow-x: hidden;",
        withSpinner(plotlyOutput("po_heatmap", height = "auto"),
          type = 2,
          color.background = config$colours$icons$taxi,
          color = config$colours$theme$primary_accent, size = 0.5
        )
      )
    ),
    nav_panel(
      title = "TABLES",
      card(
        full_screen = TRUE,
        card_header("Options"),
        tags$p("TBC")
      )
    )
  )
)
