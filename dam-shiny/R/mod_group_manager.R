# ============================================================
# mod_group_manager.R  —  add / label / configure / run groups
#
# Add groups (3 monitor files + label/color + per-group config),
# run the analysis engine (SHINY_APP_PLAN.md §11) per group or for
# all pending groups, and show a status table + engine log.
# ============================================================

# Parse a channel spec like "1:32" or "1,2,5,10" into an integer vector.
parse_channels <- function(txt) {
  txt <- trimws(txt)
  if (!nzchar(txt)) return(1:32)
  parts <- strsplit(txt, ",")[[1]]
  vals <- unlist(lapply(parts, function(p) {
    p <- trimws(p)
    if (grepl(":", p)) {
      rng <- as.integer(strsplit(p, ":")[[1]])
      seq(rng[1], rng[2])
    } else as.integer(p)
  }))
  vals <- vals[!is.na(vals) & vals >= 1 & vals <= 32]
  if (!length(vals)) 1:32 else sort(unique(vals))
}

mod_group_manager_ui <- function(id) {
  ns  <- NS(id)
  cfg <- app_config()   # config.yml defaults (with built-in fallback)
  tagList(
    fluidRow(
      box(
        title = "Add a group", width = 4, status = "primary", solidHeader = TRUE,
        textInput(ns("label"), "Label", value = "hTauV337M 50D"),
        textInput(ns("color"), "Color (hex)", value = "#0072B2"),
        fileInput(ns("plug"),   "Plug-end monitor (.txt)",   accept = ".txt"),
        fileInput(ns("middle"), "Middle monitor (.txt)",     accept = ".txt"),
        fileInput(ns("food"),   "Food-end monitor (.txt)",   accept = ".txt"),
        actionButton(ns("add"), "Add group", icon = icon("plus"),
                     class = "btn-primary"),
        actionButton(ns("demo"), "Add demo group", icon = icon("flask"))
      ),
      box(
        title = "Run settings (applied to new groups)", width = 4, status = "primary",
        solidHeader = TRUE,
        textInput(ns("start_dt"), "Start datetime", cfg$start_datetime),
        textInput(ns("end_dt"),   "End datetime",   cfg$end_datetime),
        textInput(ns("tz"),       "Timezone",       cfg$tz),
        splitLayout(
          textInput(ns("lights_on"),  "Lights on",  cfg$lights_on),
          textInput(ns("lights_off"), "Lights off", cfg$lights_off)
        ),
        textInput(ns("channels"), "Channels", cfg$channels),
        splitLayout(
          numericInput(ns("sleep_thr"), "Sleep thr (min)", cfg$sleep_threshold_min, 1, 60),
          numericInput(ns("death_thr"), "Death thr (h)",   cfg$death_threshold_hours, 1, 72),
          numericInput(ns("bout_win"),  "Bout win (min)",  cfg$bout_window_min, 1, 60)
        ),
        checkboxInput(ns("use_anchored"), "Use anchor method", isTRUE(cfg$use_anchored))
      ),
      box(
        title = "Groups", width = 4, status = "info", solidHeader = TRUE,
        DT::dataTableOutput(ns("group_tbl")),
        br(),
        actionButton(ns("remove"), "Remove selected", icon = icon("trash")),
        actionButton(ns("run"), "Run selected", icon = icon("play"),
                     class = "btn-success"),
        actionButton(ns("run_all"), "Run all pending", icon = icon("forward"),
                     class = "btn-success")
      )
    ),
    fluidRow(
      box(
        title = "Engine log", width = 12, status = "warning",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        actionButton(ns("clear_log"), "Clear log", icon = icon("eraser")),
        br(), br(),
        tags$pre(
          style = "max-height:300px; overflow:auto; font-size:11px;",
          textOutput(ns("log"))
        )
      )
    )
  )
}

mod_group_manager_server <- function(id, rv, parent = NULL) {
  moduleServer(id, function(input, output, session) {

    # -- Collect config from the "Run settings" form ----------------
    form_config <- function() {
      list(
        channels              = parse_channels(input$channels),
        start_datetime        = trimws(input$start_dt),
        end_datetime          = trimws(input$end_dt),
        tz                    = trimws(input$tz),
        lights_on             = trimws(input$lights_on),
        lights_off            = trimws(input$lights_off),
        death_threshold_hours = input$death_thr,
        sleep_threshold_min   = input$sleep_thr,
        look_window           = 5,
        bout_window_min       = input$bout_win,
        use_anchored          = input$use_anchored
      )
    }

    add_configured_group <- function(label, color, paths) {
      gid <- add_group(rv, label = label, color = color)
      g <- rv$groups[[gid]]
      g$monitor_paths <- paths
      g$config <- modifyList(g$config, form_config())
      rv$groups[[gid]] <- g
      gid
    }

    observeEvent(input$add, {
      if (!isTruthy(input$plug) || !isTruthy(input$middle) || !isTruthy(input$food)) {
        showNotification("Upload all three monitor files first.", type = "error")
        return()
      }
      add_configured_group(
        input$label, input$color,
        list(plug   = input$plug$datapath,
             middle = input$middle$datapath,
             food   = input$food$datapath)
      )
      showNotification(sprintf("Added group '%s'", input$label), type = "message")
    })

    # -- Demo group from bundled sample monitors --------------------
    observeEvent(input$demo, {
      ex <- system.file("extdata", package = "damshiny")
      if (!nzchar(ex)) ex <- file.path("inst", "extdata")   # running from source
      paths <- list(plug   = file.path(ex, "Monitor7.txt"),
                    middle = file.path(ex, "Monitor8.txt"),
                    food   = file.path(ex, "Monitor9.txt"))
      if (!all(file.exists(unlist(paths)))) {
        showNotification("Sample data not found in inst/extdata.", type = "error")
        return()
      }
      add_configured_group(
        if (nzchar(input$label)) input$label else "Demo group",
        input$color, paths)
      showNotification("Added demo group from bundled sample data.", type = "message")
    })

    output$group_tbl <- DT::renderDataTable({
      gs <- rv$groups
      if (!length(gs)) {
        return(DT::datatable(
          data.frame(Label = character(), Status = character(), Flies = integer()),
          selection = "single", rownames = FALSE,
          options = list(dom = "t")))
      }
      df <- data.frame(
        Label  = vapply(gs, `[[`, "", "label"),
        Status = vapply(gs, `[[`, "", "status"),
        Flies  = vapply(gs, function(g)
          if (is.null(g$dat)) NA_integer_ else length(unique(g$dat$channel)),
          integer(1)),
        stringsAsFactors = FALSE
      )
      DT::datatable(df, selection = "single", rownames = FALSE,
                    options = list(dom = "t")) |>
        DT::formatStyle("Status",
          backgroundColor = DT::styleEqual(
            c("configured", "queued", "running", "done", "error"),
            c("#eceff1", "#e2e3f3", "#fff3cd", "#d4edda", "#f8d7da")))
    })

    selected_gid <- reactive({
      sel <- input$group_tbl_rows_selected
      if (is.null(sel) || !length(rv$groups)) return(NULL)
      names(rv$groups)[[sel]]
    })

    # Keep the app-wide "focused group" in sync for the per-group tabs.
    observeEvent(selected_gid(), {
      if (!is.null(selected_gid())) rv$selected <- selected_gid()
    })

    observeEvent(input$remove, {
      gid <- selected_gid(); req(gid)
      remove_group(rv, gid)
    })

    # -- Async runner ------------------------------------------------
    # The pipeline runs in a background R process (future), so the UI stays
    # responsive. A single ExtendedTask queues invocations FIFO, so "Run all
    # pending" runs groups one after another without blocking the session.
    #
    # The worker gets only plain data (id, label, config, file paths) and
    # sources the engine itself — no functions or reactives cross the process
    # boundary. It returns the results plus the captured console log.
    app_root <- normalizePath(getwd())

    run_task <- ExtendedTask$new(function(gid, group, app_root) {
      promises::future_promise(
        {
          suppressPackageStartupMessages({
            library(data.table); library(lubridate); library(ggplot2)
            library(scales);     library(openxlsx);  library(readxl)
            library(dplyr);      library(rlang)
          })

          # Source the engine + state helpers (skip UI/module files).
          fs <- list.files(file.path(app_root, "R"), pattern = "\\.R$",
                           full.names = TRUE)
          fs <- fs[!grepl("^(mod_|app_ui|app_server)", basename(fs))]
          for (f in fs) source(f)

          # Collect the engine's console output via the pipeline's own log
          # callback (it already capture.output()s each step internally), so
          # it can be replayed into the app's log panel when the run returns.
          logbuf <- character(0)
          res <- tryCatch(
            run_group_pipeline(group, log = function(txt) logbuf <<- c(logbuf, txt)),
            error = function(e) {
              logbuf <<- c(logbuf, paste("ERROR:", conditionMessage(e)))
              list(status = "error", error = conditionMessage(e))
            })

          list(gid = gid, res = res, log = logbuf)
        },
        globals = list(gid = gid, group = group, app_root = app_root),
        seed    = TRUE
      )
    })

    # Groups handed to run_task, in invocation order. Only needed to attribute
    # a hard worker failure (where no payload comes back) to the right group.
    queue <- reactiveVal(character(0))

    run_one <- function(gid) {
      g <- rv$groups[[gid]]
      g$status <- "queued"; g$error <- NULL
      rv$groups[[gid]] <- g
      append_log(rv, sprintf("\n===== Queued group: %s =====", g$label))

      # Send only what the pipeline needs — never the (large) result fields.
      lean <- g[c("id", "label", "color", "monitor_paths", "config")]
      queue(c(queue(), gid))
      run_task$invoke(gid = gid, group = lean, app_root = app_root)
    }

    # -- damalignr pre-check + confirmation dialog ------------------
    # Before a run, diagnose each group's 3 monitor files (via the vendored
    # engine in R/00_dam_align.R). Files that are already aligned run
    # immediately; files that need alignment (staggered start/end, partial
    # final minute, or a mid-file dropped sub-row that a bare read would
    # truncate on) are held behind a confirmation dialog showing exactly what
    # will change. Original uploads are never modified -- alignment writes new
    # temp files used only for the run.
    pending_gids  <- reactiveVal(character(0))   # gids awaiting confirmation
    pending_plans <- reactiveVal(list())         # gid -> alignment plan

    request_run <- function(gids) {
      plans <- list(); need <- character(0)
      for (gid in gids) {
        g  <- rv$groups[[gid]]
        mf <- c(plug   = g$monitor_paths$plug,
                middle = g$monitor_paths$middle,
                food   = g$monitor_paths$food)
        if (any(vapply(mf, function(p) is.null(p) || !nzchar(p), logical(1)))) {
          run_one(gid); next                      # let the engine report missing files
        }
        plan <- tryCatch(dam_plan_alignment(mf), error = function(e) NULL)
        if (is.null(plan)) { run_one(gid); next }  # unreadable -> let the engine report
        if (isTRUE(plan$needs_alignment)) {
          plans[[gid]] <- plan; need <- c(need, gid)
        } else {
          run_one(gid)                             # already aligned: run straight away
        }
      }
      if (length(need)) {
        pending_gids(need); pending_plans(plans)
        showModal(dam_align_modal(need, plans))
      }
    }

    dam_align_modal <- function(need, plans) {
      blocks <- lapply(need, function(gid) {
        p <- plans[[gid]]
        tags$div(
          style = "margin-bottom:14px;",
          tags$b(sprintf("Group: %s", rv$groups[[gid]]$label)),
          tags$pre(
            style = paste("white-space:pre-wrap; font-size:12px; background:#f6f8fa;",
                          "padding:8px; border-radius:4px;"),
            paste(p$report, collapse = "\n"))
        )
      })
      modalDialog(
        title = "Monitor files need alignment before combining",
        tags$p(paste(
          "These monitors do not currently line up, so the combiner would",
          "otherwise stop with a row-count error. Review the planned fix, then",
          "choose whether to align and run. Your original files are NOT",
          "modified -- alignment writes new temporary files used only for this run.")),
        blocks,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("dam_confirm_align"), "Align & run",
                       class = "btn-success", icon = icon("check"))
        ),
        size = "l", easyClose = FALSE
      )
    }

    observeEvent(input$dam_confirm_align, {
      need  <- pending_gids(); plans <- pending_plans()
      removeModal()
      for (gid in need) {
        if (is.null(rv$groups[[gid]])) next        # removed while dialog open
        plan  <- plans[[gid]]
        paths <- tryCatch(dam_write_aligned(plan), error = function(e) NULL)
        if (is.null(paths)) {
          showNotification(sprintf("Alignment failed for '%s'.",
                                   rv$groups[[gid]]$label),
                           type = "error", duration = NULL)
          next
        }
        g <- rv$groups[[gid]]
        g$monitor_paths <- list(plug   = unname(paths["plug"]),
                                middle = unname(paths["middle"]),
                                food   = unname(paths["food"]))
        rv$groups[[gid]] <- g
        append_log(rv, c(sprintf("\n----- Aligned '%s' before run -----", g$label),
                         plan$report))
        showNotification(sprintf("Aligned '%s'; running.", g$label), type = "message")
        run_one(gid)
      }
      pending_gids(character(0)); pending_plans(list())
    })

    # -- Collect finished runs ---------------------------------------
    observeEvent(run_task$status(), {
      st <- run_task$status()

      if (identical(st, "running")) {
        # FIFO: the head of the queue is executing, the rest are still waiting.
        q <- queue()
        for (i in seq_along(q)) {
          gid <- q[[i]]
          if (!is.null(rv$groups[[gid]])) {
            rv$groups[[gid]]$status <- if (i == 1L) "running" else "queued"
          }
        }

      } else if (identical(st, "success")) {
        out <- run_task$result()
        gid <- out$gid
        queue(setdiff(queue(), gid))
        append_log(rv, out$log)

        if (is.null(rv$groups[[gid]])) return()   # removed while running
        set_group_results(rv, gid, out$res)
        lbl <- rv$groups[[gid]]$label

        if (identical(out$res$status, "done")) {
          showNotification(sprintf("Finished '%s'", lbl), type = "message")
        } else {
          showNotification(sprintf("'%s' failed: %s", lbl, out$res$error),
                           type = "error", duration = NULL)
        }

      } else if (identical(st, "error")) {
        # The future itself failed (no payload) — blame the oldest queued group.
        msg <- tryCatch({ run_task$result(); "unknown error" },
                        error = function(e) conditionMessage(e))
        q   <- queue()
        if (length(q)) {
          gid <- q[[1]]
          queue(q[-1])
          if (!is.null(rv$groups[[gid]])) {
            set_group_results(rv, gid, list(status = "error", error = msg))
          }
        }
        append_log(rv, paste("WORKER ERROR:", msg))
        showNotification(paste("Run failed:", msg), type = "error", duration = NULL)
      }
    })

    observeEvent(input$run, {
      gid <- selected_gid()
      if (is.null(gid)) {
        showNotification("Select a group in the table first.", type = "warning")
        return()
      }
      request_run(gid)
    })

    observeEvent(input$run_all, {
      pending <- names(Filter(function(g) g$status %in% c("configured", "error"),
                              rv$groups))
      if (!length(pending)) {
        showNotification("No pending groups to run.", type = "message")
        return()
      }
      request_run(pending)
    })

    observeEvent(input$clear_log, { rv$log <- character(0) })

    output$log <- renderText({
      if (!length(rv$log)) "No engine output yet. Add a group and press Run."
      else paste(rv$log, collapse = "\n")
    })
  })
}
