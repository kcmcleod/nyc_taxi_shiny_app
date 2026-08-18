################################################################################
### FILE: R/mod_yellow_taxis_parent.R
################################################################################

#' Module UI
mod_yellow_taxis_parent_ui <- function(id, config, app_metadata) {
  ns <- NS(id)

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
        disabled(dateInput(
          ns("di_start_date"),
          label = "Start Date",
          width = "100%",
          min = app_metadata$min_date,
          max = app_metadata$max_date,
          value = floor_date(app_metadata$max_date %m-% weeks(13), unit = "month")
        )),
        disabled(dateInput(
          ns("di_end_date"),
          label = "End Date",
          width = "100%",
          min = app_metadata$min_date,
          max = app_metadata$max_date,
          value = app_metadata$max_date
        ))
      )
    ),
    navset_card_tab(
      id = ns("yellow_data_panel"),
      full_screen = TRUE,
      nav_panel(
        title = "Volumes",
        icon = icon("list-alt"),
        mod_yellow_taxis_main_ui(ns("yt_main"), config)
      ),
      nav_panel(
        title = "JOURNEY PICKUP + DROP OFF",
        mod_yellow_taxis_heatmap_ui(ns("yt_heat"), config)
      ),
      nav_panel(
        title = "TABLES",
        mod_yellow_taxis_table_ui(ns("yt_table"), config, app_metadata)
      )
    )
  )
}

#' Parent level server
mod_yellow_taxis_parent_server <- function(id, yellow_taxi_data, app_metadata,
                                           data_version) {
  moduleServer(id, function(input, output, session) {
    observeEvent(yellow_taxi_data(), {
      # not sure this is needed...
      enable("di_start_date")
      enable("di_end_date")
    })

    ############################################################################
    # TOP LEVEL DATA PROCESSING
    rv_main_date_filtered_date <- reactive({
      req(yellow_taxi_data(), input$di_end_date, input$di_start_date)

      if (input$di_start_date == Sys.Date() || input$di_end_date == Sys.Date()) {
        return(NULL)
      }

      user_start_date <- input$di_start_date
      user_end_date <- input$di_end_date

      tmpDF <- yellow_taxi_data() |>
        filter(
          # allows dates with a missing value
          is.na(date) |

            (!full_month_aggregation & !full_week_aggregation &
              date >= user_start_date & date <= user_end_date) |

            # Monthly Data: Snap the user's start date to the 1st of the month
            # due to the data prep using the 1st of the month as the date
            (full_month_aggregation &
              date >= floor_date(user_start_date, "month") &
              date <= user_end_date) |

            # Weekly Data: Snap the user's start date to the start of the week.
            # due to ata prep using week_start = 7 (Sunday)
            (full_week_aggregation &
              date >= floor_date(user_start_date, "week", week_start = 7) &
              date <= user_end_date)
        )

      return(tmpDF)
    })

    mod_yellow_taxis_main_server(
      id = "yt_main",
      rv_main_date_filtered_date, app_metadata,
      data_version, input$di_start_date,
      input$di_end_date
    )

    mod_yellow_taxis_heatmap_server(
      id = "yt_heat",
      rv_main_date_filtered_date, app_metadata,
      data_version, input$di_start_date,
      input$di_end_date
    )

    mod_yellow_taxis_table_server(
      id = "yt_table",
      rv_main_date_filtered_date, app_metadata,
      data_version, input$di_start_date,
      input$di_end_date
    )
  })
}
