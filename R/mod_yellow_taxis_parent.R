################################################################################
### FILE: R/mod_yellow_taxis_parent.R
################################################################################

#' Module UI
mod_yellow_taxis_parent_ui <- function(id, config, app_metadata) {
  ns <- NS(id)

  nav_panel(
    title = "Yellow Taxis",
    icon = icon("taxi", style = paste0("color:", config$colours$icons$taxi)),
    card(
      fill = FALSE,
      card_header(
        "About this Dashboard",
        class = "bg-light"
      ),
      tags$p(
        "This application provides high-speed, interactive analytics for New York City's
        Yellow Taxi network. Powered by an Apache Arrow and Parquet backend,
        it rapidly queries partitioned data to visualise journey volumes,
        financial metrics, and geographic trip patterns without the overhead of
        a traditional database."
      ),
      tags$a(
        href = "https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page",
        "Data from the NYC Taxi and Limousine Commission (TLC). "
      )
    ),

    # Top Date Range Card
    card(
      fill = FALSE,
      card_header("DATE RANGE", class = "bg-light"),
      tags$p("Controls the date range for all the charts/tables in this page"),
      dateRangeInput(
        ns("di_date_range"),
        label = NULL, # Label hidden since the card header acts as the label
        start = floor_date(app_metadata$max_date %m-% weeks(13), unit = "month"),
        end = app_metadata$max_date,
        min = app_metadata$min_date,
        max = app_metadata$max_date,
        width = "100%",
        separator = " to "
      )
    ),
    navset_card_tab(
      id = ns("yellow_data_panel"),
      full_screen = TRUE,
      nav_panel(
        title = "TIME SERIES",
        icon = icon("list-alt"),
        mod_yellow_taxis_main_ui(ns("yt_main"), config, app_metadata)
      ),
      nav_panel(
        title = "ROUTE HEATMAPS",
        mod_yellow_taxis_heatmap_ui(ns("yt_heat"), config)
      ),
      nav_panel(
        title = "SUMMARY METRICS",
        mod_yellow_taxis_table_ui(ns("yt_table"), config, app_metadata)
      ),
      nav_panel(
        title = "DATA AVAILABILITY",
        icon = icon("database"),
        mod_yellow_taxis_availability_ui(ns("yt_avail"), config)
      ),
    )
  )
}

#' Parent level server
mod_yellow_taxis_parent_server <- function(id, yellow_taxi_data, app_metadata,
                                           data_version) {
  moduleServer(id, function(input, output, session) {
    ############################################################################
    # 1-YEAR GUARDRAIL
    observeEvent(input$di_date_range,
      {
        req(input$di_date_range[1], input$di_date_range[2])

        start_date <- as.Date(input$di_date_range[1])
        end_date <- as.Date(input$di_date_range[2])

        date_diff <- as.numeric(difftime(end_date, start_date, units = "days"))

        if (!is.na(date_diff) && date_diff > 365) {
          corrected_end <- start_date + lubridate::days(365)

          updateDateRangeInput(
            session,
            "di_date_range",
            end = corrected_end
          )

          toastr_warning(
            message = "Time period limited to a maximum of 1 year to ensure optimal performance.",
            position = "bottom-right",
            timeOut = 5000
          )
        }
      },
      ignoreInit = TRUE
    )


    ############################################################################
    # TOP LEVEL DATA PROCESSING
    rv_main_date_filtered_date <- reactive({
      req(yellow_taxi_data(), input$di_date_range[1], input$di_date_range[2])

      user_start_date <- as.Date(input$di_date_range[1])
      user_end_date <- as.Date(input$di_date_range[2])

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
      data_version, reactive(input$di_date_range[1]),
      reactive(input$di_date_range[2])
    )

    mod_yellow_taxis_heatmap_server(
      id = "yt_heat",
      rv_main_date_filtered_date, app_metadata,
      data_version, reactive(input$di_date_range[1]),
      reactive(input$di_date_range[2])
    )

    mod_yellow_taxis_table_server(
      id = "yt_table",
      rv_main_date_filtered_date, app_metadata,
      data_version, reactive(input$di_date_range[1]),
      reactive(input$di_date_range[2])
    )

    mod_yellow_taxis_availability_server(
      id = "yt_avail",
      app_metadata, config,
      data_version, reactive(input$di_date_range[1]),
      reactive(input$di_date_range[2])
    )
  })
}
