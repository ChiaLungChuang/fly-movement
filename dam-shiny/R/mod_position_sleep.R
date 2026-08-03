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
            # Edits this group's stored output method -- what the bundle
            # exports, not just what is drawn below.
            column(6, radioButtons(ns("method"), "Zone method for outputs",
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

    # -- Zone method: the group's config is the single source of truth -----
    # Both zone methods run on every group regardless; use_anchored selects
    # which columns the OUTPUTS read. This radio therefore EDITS the group
    # rather than shadowing it, so the Downloads bundle -- which reads
    # config$use_anchored -- can never disagree with what is on screen. The
    # add-group checkbox supplies the initial value.

    # ignoreInit on the writer below matters in the live app but not under
    # testServer: the radio always holds a value in a browser, so without it
    # the session's first flush would write that default onto whichever group
    # is focused. testServer leaves input$method NULL until set, and
    # observeEvent's ignoreNULL default swallows it, so no test can tell the
    # difference. Kept because the browser behaves differently from the mock.

    # Show the focused group's own method. This is a sync, never a user edit.
    observeEvent(input$group, {
      g <- if (is.null(input$group)) NULL else rv$groups[[input$group]]
      req(g)
      updateRadioButtons(session, "method",
        selected = if (isTRUE(g$config$use_anchored)) "anchor" else "sm")
    })

    # What stops a group being restamped merely by being VIEWED is that this
    # observer keys on input$method ALONE. A group switch does not change
    # input$method, so it cannot trigger a write; the sync above pushes the new
    # group's own value, and the client echo that follows carries that same
    # value. Widening this to observeEvent(list(input$group, input$method))
    # reintroduces the bug, which is what the view-does-not-modify test pins.
    observeEvent(input$method, ignoreInit = TRUE, {
      gid <- input$group
      g <- if (is.null(gid)) NULL else rv$groups[[gid]]
      req(g)
      want <- identical(input$method, "anchor")
      # Not correctness -- the echo carries the value already stored, so
      # writing it would be a no-op. It avoids re-writing rv$groups on every
      # group switch, which would invalidate the table and every plot for
      # nothing. No test distinguishes this; it is a thrash guard.
      if (identical(isTRUE(g$config$use_anchored), want)) return()
      g$config$use_anchored <- want
      rv$groups[[gid]] <- g
    })

    # Read the stored value, not the control, so the rendered plots and the
    # exported ones are driven by the same field by construction.
    use_anchored <- reactive({
      g <- if (is.null(input$group)) NULL else rv$groups[[input$group]]
      if (is.null(g)) identical(input$method, "anchor")
      else isTRUE(g$config$use_anchored)
    })

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
