# ============================================================
# mod_activity.R  —  activity summaries + workbook downloads
# Renders one wide summary table (monitor x metric x grouping) from
# the group's precomputed `summaries`, downloadable as the matching
# .xlsx workbook (plug/middle/food/summed sheets).
# ============================================================

.metric_choices   <- c("Total activity"    = "activity_total",
                       "Activity per bout" = "bout_activity",
                       "Bout duration"     = "bout_duration")
.grouping_choices <- c("Light phase"        = "light_phase",
                       "Per day"            = "day",
                       "Day x light phase"  = "day_light_phase")
.monitor_choices  <- c("Summed" = "summed", "Plug" = "plug",
                       "Middle" = "middle", "Food" = "food")

mod_activity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE, title = "Selection",
          fluidRow(
            column(3, selectInput(ns("group"),    "Group",    choices = NULL)),
            column(3, selectInput(ns("monitor"),  "Monitor",  .monitor_choices)),
            column(3, selectInput(ns("metric"),   "Metric",   .metric_choices)),
            column(3, selectInput(ns("grouping"), "Grouping", .grouping_choices))
          ),
          downloadButton(ns("dl_xlsx"), "Download workbook (.xlsx)")
      )
    ),
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "Per-fly summary",
          DT::dataTableOutput(ns("tbl")))
    )
  )
}

mod_activity_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      ch <- group_choices(rv, only_done = TRUE)
      sel <- if (!is.null(rv$selected) && rv$selected %in% ch) rv$selected
             else if (length(ch)) ch[[1]] else NULL
      updateSelectInput(session, "group", choices = ch, selected = sel)
    })

    grp <- reactive({
      gid <- input$group
      validate(need(!is.null(gid) && !is.null(rv$groups[[gid]]$summaries),
                    "Run a group to see activity summaries."))
      rv$groups[[gid]]
    })

    tbl <- reactive({
      activity_summary_table(grp()$summaries, input$monitor,
                             input$metric, input$grouping)
    })

    output$tbl <- DT::renderDataTable({
      d <- as.data.frame(tbl())
      d$start_datetime <- as.character(d$start_datetime)
      d$end_datetime   <- as.character(d$end_datetime)
      DT::datatable(d, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE))
    })

    output$dl_xlsx <- downloadHandler(
      filename = function() {
        g <- grp()
        sprintf("%s_%s_%s.xlsx", gsub("[^A-Za-z0-9]+", "_", g$label),
                input$metric, input$grouping)
      },
      content = function(file) {
        g <- grp()
        settings <- activity_settings_df(
          g$dat, g$dead_summary, g$label,
          g$config$lights_on, g$config$lights_off, g$config$bout_window_min)
        write_activity_workbook(g$summaries, input$grouping, input$metric,
                                g$label, settings, file)
      }
    )
  })
}
