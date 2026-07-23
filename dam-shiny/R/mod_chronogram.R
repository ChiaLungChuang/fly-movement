# ============================================================
# mod_chronogram.R  —  COMBINED activity chronogram across groups
# Rebuilds each done group's chronogram data for the selected monitor
# + bin width, stacks by Labels, summarizes (mean ± 95% CI), and plots
# colored by group. Rtivity-style light/dark cycle shading.
# ============================================================

mod_chronogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "Chronogram controls",
          fluidRow(
            column(3, selectInput(ns("monitor"), "Signal",
                                  c("Summed" = "summed", "Plug" = "plug",
                                    "Middle" = "middle", "Food" = "food"))),
            column(3, numericInput(ns("bin"), "Bin (min)", 10, 1, 60)),
            column(3, checkboxInput(ns("day2"), "Single day (24–48 h)", FALSE)),
            column(3, br(), downloadButton(ns("dl_pdf"), "PDF"),
                      downloadButton(ns("dl_csv"), "CSV"))
          )
      )
    ),
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "Activity over time (all groups, mean ± 95% CI)",
          plotOutput(ns("chrono"), height = "520px"))
    )
  )
}

mod_chronogram_server <- function(id, rv, combined_chrono = NULL,
                                  combined_sleep = NULL) {
  moduleServer(id, function(input, output, session) {

    # Stack each done group's chronogram data for the current monitor/bin.
    combined <- reactive({
      gs <- done_groups(rv)
      validate(need(length(gs) > 0,
                    "Run at least one group to see the combined chronogram."))
      data.table::rbindlist(lapply(gs, function(g)
        make_activity_chronogram(g$dat, g$label,
                                 monitor = input$monitor, bin_min = input$bin)),
        fill = TRUE)
    })

    summ <- reactive({
      d <- combined()
      if (isTRUE(input$day2)) d <- filter_by_time(d, 86400, 172800)
      summarize_activity(d)
    })

    chrono_plot <- reactive({
      plot_activity_chronogram(
        summ(), group_colors(rv),
        title = sprintf("Activity Over Time (%s, %d-min bins)",
                        input$monitor, input$bin))
    })

    output$chrono <- renderPlot(chrono_plot())

    output$dl_pdf <- downloadHandler(
      filename = function() sprintf("chronogram_%s_%dmin.pdf",
                                    input$monitor, input$bin),
      content  = function(file) ggplot2::ggsave(file, chrono_plot(),
                                                width = 12, height = 6)
    )
    output$dl_csv <- downloadHandler(
      filename = function() sprintf("chronogram_%s_%dmin.csv",
                                    input$monitor, input$bin),
      content  = function(file) data.table::fwrite(summ(), file)
    )
  })
}
