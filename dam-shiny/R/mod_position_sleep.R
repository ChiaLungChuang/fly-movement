# ============================================================
# mod_position_sleep.R  —  timelines + sleep-zone plots (per group)
# Calls the R/04 plot functions with output_file = NULL (return-only)
# and renders the ggplot; download handlers ggsave PDFs.
# ============================================================

mod_position_sleep_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE, title = "Group & method",
          fluidRow(
            column(6, selectInput(ns("group"), "Group", choices = NULL)),
            column(6, radioButtons(ns("method"), "Zone method",
                                   c("Anchor" = "anchor", "State machine" = "sm"),
                                   inline = TRUE))
          )
      )
    ),
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "Individual movement timelines",
          downloadButton(ns("dl_timelines"), "PDF"),
          br(), br(),
          plotOutput(ns("timelines"), height = "700px"))
    ),
    fluidRow(
      box(width = 7, status = "primary", solidHeader = TRUE,
          title = "Sleep-zone heatmap (per fly)",
          downloadButton(ns("dl_heatmap"), "PDF"), br(), br(),
          plotOutput(ns("heatmap"), height = "420px")),
      box(width = 5, status = "primary", solidHeader = TRUE,
          title = "Sleep-zone preference (mean ± SEM)",
          downloadButton(ns("dl_summary"), "PDF"), br(), br(),
          plotOutput(ns("summary"), height = "420px"))
    )
  )
}

mod_position_sleep_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    observe({
      ch <- group_choices(rv, only_done = TRUE)
      sel <- if (!is.null(rv$selected) && rv$selected %in% ch) rv$selected
             else if (length(ch)) ch[[1]] else NULL
      updateSelectInput(session, "group", choices = ch, selected = sel)
    })

    grp <- reactive({
      gid <- input$group
      validate(need(!is.null(gid) && !is.null(rv$groups[[gid]]$dat),
                    "Run a group to see movement + sleep plots."))
      rv$groups[[gid]]
    })

    use_anchored <- reactive(identical(input$method, "anchor"))

    p_timelines <- reactive({
      g <- grp()
      plot_individual_timelines(
        g$dat, use_anchored = use_anchored(),
        lights_on = g$config$lights_on, lights_off = g$config$lights_off,
        output_file = NULL)
    })
    p_heatmap <- reactive({
      g <- grp()
      plot_sleep_zone_heatmap(g$dat, use_anchored = use_anchored(),
                              group_label = g$label, output_file = NULL)
    })
    p_summary <- reactive({
      g <- grp()
      plot_sleep_zone_summary(g$dat, use_anchored = use_anchored(),
                              group_label = g$label, output_file = NULL)
    })

    output$timelines <- renderPlot(p_timelines())
    output$heatmap   <- renderPlot(p_heatmap())
    output$summary   <- renderPlot(p_summary())

    slug <- function() gsub("[^A-Za-z0-9]+", "_", grp()$label)

    dl <- function(suffix, plot_fun, w, h) {
      downloadHandler(
        filename = function() sprintf("%s_%s.pdf", slug(), suffix),
        content  = function(file) ggplot2::ggsave(file, plot_fun(),
                                                  width = w, height = h, limitsize = FALSE))
    }
    output$dl_timelines <- dl("position_timelines", p_timelines, 20, 12)
    output$dl_heatmap   <- dl("sleep_zone_heatmap", p_heatmap, 14, 6)
    output$dl_summary   <- dl("sleep_zone_summary", p_summary, 7, 5)
  })
}
