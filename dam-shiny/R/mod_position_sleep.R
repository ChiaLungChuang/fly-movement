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
            # Rendered per group, so the control carries the group's identity
            # in its input id. Edits this group's stored output method -- what
            # the bundle exports, not just what is drawn below.
            column(6, uiOutput(ns("method_ui")))
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
    # which columns the OUTPUTS read. This control EDITS the group rather than
    # shadowing it, so the Downloads bundle -- which reads config$use_anchored
    # -- cannot disagree with what is on screen. The add-group checkbox
    # supplies the initial value, and this is the ONLY way to change it
    # afterwards.
    #
    # Each group gets its own input id, method_<gid>. A single shared id
    # carried no group identity, so the writer had to read input$group when it
    # fired and assume the two referred to the same group. They are
    # independent inputs and that assumption does not hold: a value can arrive
    # after the selection has moved on, and the value check cannot catch it
    # because it fires precisely when the values differ. Binding the id to the
    # group removes the assumption instead of guarding it.
    output$method_ui <- renderUI({
      gid <- input$group
      g <- if (is.null(gid)) NULL else rv$groups[[gid]]
      if (is.null(g)) return(NULL)
      radioButtons(session$ns(paste0("method_", gid)), "Zone method for outputs",
                   c("Anchor" = "anchor", "State machine" = "sm"),
                   selected = if (isTRUE(g$config$use_anchored)) "anchor" else "sm",
                   inline = TRUE)
    })

    # Number of config writes this session has performed. Exposed so a test can
    # assert the writer runs ONCE per user edit: a dynamic UI invites observers
    # created per render, and duplicates would be invisible in the config value
    # because the writes agree. Counting is the only way to see them.
    method_writes <- reactiveVal(0L)

    # ONE writer for every per-group control, created once at module init --
    # deliberately a sibling of the renderUI above, not nested inside it.
    # Creating it in the render block would add an observer per group switch,
    # each writing the same value on every subsequent edit.
    #
    # Correctness no longer depends on WHEN this runs: method_<gid> can only
    # ever address <gid>, so a late or reordered value still lands on the group
    # it came from. rv$groups is read isolated so writing it here cannot
    # re-trigger this observer.
    observe({
      ins <- reactiveValuesToList(input)
      for (nm in names(ins)) {
        if (!startsWith(nm, "method_")) next
        val <- ins[[nm]]
        if (is.null(val) || !nzchar(val)) next
        gid <- sub("^method_", "", nm)
        # Shiny does not clear an input when the UI that created it goes away,
        # so method_<gid> outlives its group. A value arriving for a removed
        # group must be dropped, not used to resurrect a config entry.
        g <- isolate(rv$groups[[gid]])
        if (is.null(g)) next
        want <- identical(val, "anchor")
        # Skips the echo a redraw produces, which carries the value already
        # stored. Now a pure no-op guard: with the id bound to the group it can
        # no longer be the difference between a right and a wrong write.
        if (identical(isTRUE(g$config$use_anchored), want)) next
        g$config$use_anchored <- want
        rv$groups[[gid]] <- g
        method_writes(isolate(method_writes()) + 1L)
      }
    })

    # Read the stored value, not the control, so the rendered plots and the
    # exported ones are driven by the same field by construction.
    use_anchored <- reactive({
      g <- if (is.null(input$group)) NULL else rv$groups[[input$group]]
      if (is.null(g)) TRUE else isTRUE(g$config$use_anchored)
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
