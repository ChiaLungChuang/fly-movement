# ============================================================
# mod_qc.R  —  Data & QC (dead channels, UNKNOWN + transit + anchor)
# Per selected group: dead-channel table, UNKNOWN diagnostics,
# transit-type distribution and anchor-confidence summary.
# ============================================================

mod_qc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE, title = "Group",
          selectInput(ns("group"), "Show QC for group", choices = NULL, width = "50%"),
          uiOutput(ns("summary"))
      )
    ),
    fluidRow(
      tabBox(
        width = 12, title = "Quality control",
        tabPanel("Dead channels",   DT::dataTableOutput(ns("dead_tbl"))),
        tabPanel("UNKNOWN diag",    DT::dataTableOutput(ns("unknown_tbl"))),
        tabPanel("Transit types",   DT::dataTableOutput(ns("transit_tbl"))),
        tabPanel("Anchor confidence", DT::dataTableOutput(ns("anchor_tbl")))
      )
    )
  )
}

mod_qc_server <- function(id, rv) {
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
                    "Run a group to see QC output."))
      rv$groups[[gid]]
    })

    output$summary <- renderUI({
      g <- grp()
      n_ch   <- length(unique(g$dat$channel))
      n_dead <- sum(g$dead_summary$status == "DEAD")
      n_unk  <- sum(g$unknown_diag$N)
      n_incon <- sum(g$dat$transit_type == "TRANSIT_INCONSISTENT", na.rm = TRUE)
      tagList(
        tags$b(g$label),
        tags$ul(
          tags$li(sprintf("Live channels: %d", n_ch)),
          tags$li(sprintf("Dead channels: %d", n_dead)),
          tags$li(sprintf("Unresolved UNKNOWN rows: %d", n_unk)),
          tags$li(sprintf("TRANSIT_INCONSISTENT bins (review): %d", n_incon))
        )
      )
    })

    output$dead_tbl <- DT::renderDataTable({
      d <- as.data.frame(grp()$dead_summary)
      d$death_datetime <- as.character(d$death_datetime)
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10)) |>
        DT::formatStyle("status",
          backgroundColor = DT::styleEqual(c("DEAD", "ALIVE"),
                                           c("#f8d7da", "#d4edda")))
    })

    output$unknown_tbl <- DT::renderDataTable({
      d <- as.data.frame(grp()$unknown_diag)
      if (!nrow(d)) d <- data.frame(note = "All UNKNOWN rows resolved.")
      DT::datatable(d, rownames = FALSE, options = list(pageLength = 10))
    })

    output$transit_tbl <- DT::renderDataTable({
      dat <- grp()$dat
      tc <- as.data.frame(dat[!is.na(transit_type), .N, by = transit_type])
      names(tc) <- c("transit_type", "n_bins")
      tc <- tc[order(-tc$n_bins), ]
      DT::datatable(tc, rownames = FALSE, options = list(dom = "t"))
    })

    output$anchor_tbl <- DT::renderDataTable({
      anc <- grp()$anchors
      s <- as.data.frame(anc[, .(
        n_anchors   = .N,
        n_sleep     = sum(anchor_type == "SLEEP"),
        n_stop      = sum(anchor_type == "STOP"),
        n_confirmed = sum(confidence == "confirmed"),
        n_ambiguous = sum(confidence == "ambiguous")
      ), by = channel][order(channel)])
      DT::datatable(s, rownames = FALSE, options = list(pageLength = 10))
    })
  })
}
