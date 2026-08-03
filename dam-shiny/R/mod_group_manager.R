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
        # Analysis period — auto-detected from the uploaded files' earliest and
        # latest timestamps; editable to narrow the window. No timezone: the
        # date/time shown is read straight from the files (parsed as UTC).
        uiOutput(ns("period_ui")),
        tags$hr(style = "margin:8px 0"),
        splitLayout(
          textInput(ns("lights_on"),  "Lights on",  cfg$lights_on),
          textInput(ns("lights_off"), "Lights off", cfg$lights_off)
        ),
        textInput(ns("channels"), "Channels", cfg$channels),
        splitLayout(
          numericInput(ns("sleep_thr"), "Sleep thr (min)", cfg$sleep_threshold_min, 1, 60),
          # Ceiling is one week, not 72 h: a three-day recording can show a
          # maximum inactivity above 72 (73.6 h observed), so the old cap made
          # "nothing is dead" inexpressible on exactly the data that needed it.
          # There is no server-side clamp, and `max` on a number input is a
          # hint rather than a block, so this control is the only supported
          # route to a higher threshold. bundle_divergence() records the value
          # per group, so a non-default threshold travels with the export.
          numericInput(ns("death_thr"), "Death thr (h)",   cfg$death_threshold_hours, 1, 168),
          numericInput(ns("bout_win"),  "Bout win (min)",  cfg$bout_window_min, 1, 60)
        ),
        # Output selector, not an analysis switch: both zone methods run on
        # every group regardless of this. Seeds each new group's config; the
        # Position & Sleep tab edits it per group afterwards.
        checkboxInput(ns("use_anchored"),
                      "Use anchor method for plots & exports",
                      isTRUE(cfg$use_anchored))
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

    ns <- session$ns

    # -- Detect the recording period from the uploaded files ---------
    # (all timestamps parsed as UTC / naive wall-clock, so what the user
    #  sees matches the files exactly).
    uploaded_paths <- reactive({
      if (isTruthy(input$plug) && isTruthy(input$middle) && isTruthy(input$food)) {
        c(plug   = input$plug$datapath,
          middle = input$middle$datapath,
          food   = input$food$datapath)
      } else NULL
    })

    detected_range <- reactiveVal(NULL)

    observeEvent(uploaded_paths(), {
      paths <- uploaded_paths(); req(paths)
      rng <- tryCatch(
        detect_datetime_range(paths, tz = "UTC"),
        error = function(e) {
          showNotification(paste("Could not read dates from the files:",
                                 conditionMessage(e)), type = "error")
          NULL
        })
      detected_range(rng)
    }, ignoreNULL = TRUE)

    # Period selector: appears once files are uploaded, prefilled to the full
    # detected range and editable to narrow the window.
    output$period_ui <- renderUI({
      rng <- detected_range()
      if (is.null(rng)) {
        return(helpText("Analysis period: upload the three monitor files to ",
                        "detect the recording start and end."))
      }
      d0 <- as.Date(format(rng$min, tz = "UTC"))
      d1 <- as.Date(format(rng$max, tz = "UTC"))
      tagList(
        div(style = "font-size:11px; color:#555; margin-bottom:6px;",
            tags$b("Recording detected: "),
            sprintf("%s  →  %s",
                    format(rng$min, "%Y-%m-%d %H:%M", tz = "UTC"),
                    format(rng$max, "%Y-%m-%d %H:%M", tz = "UTC"))),
        splitLayout(
          dateInput(ns("start_date"), "Start date", value = d0, min = d0, max = d1),
          textInput(ns("start_time"), "Start time (HH:MM)",
                    value = format(rng$min, "%H:%M", tz = "UTC"))
        ),
        splitLayout(
          dateInput(ns("finish_date"), "End date", value = d1, min = d0, max = d1),
          textInput(ns("finish_time"), "End time (HH:MM)",
                    value = format(rng$max, "%H:%M", tz = "UTC"))
        ),
        actionButton(ns("reset_period"), "Reset to full range",
                     icon = icon("rotate-left"), class = "btn-xs")
      )
    })

    observeEvent(input$reset_period, {
      rng <- detected_range(); req(rng)
      updateDateInput(session, "start_date",
                      value = as.Date(format(rng$min, tz = "UTC")))
      updateTextInput(session, "start_time",
                      value = format(rng$min, "%H:%M", tz = "UTC"))
      updateDateInput(session, "finish_date",
                      value = as.Date(format(rng$max, tz = "UTC")))
      updateTextInput(session, "finish_time",
                      value = format(rng$max, "%H:%M", tz = "UTC"))
    })

    # -- Collect config from the "Run settings" form ----------------
    # Build a "YYYY-MM-DD HH:MM:SS" string from a date input + HH:MM text.
    build_datetime <- function(d, t) {
      if (!isTruthy(d)) return(NULL)
      t <- trimws(if (is.null(t)) "" else t)
      if (!nzchar(t)) t <- "00:00"
      if (grepl("^\\d{1,2}:\\d{2}$", t)) t <- paste0(t, ":00")   # HH:MM -> HH:MM:00
      paste(as.character(d), t)
    }

    form_config <- function() {
      list(
        channels              = parse_channels(input$channels),
        start_datetime        = build_datetime(input$start_date,  input$start_time),
        end_datetime          = build_datetime(input$finish_date, input$finish_time),
        tz                    = "UTC",
        lights_on             = trimws(input$lights_on),
        lights_off            = trimws(input$lights_off),
        death_threshold_hours = input$death_thr,
        sleep_threshold_min   = input$sleep_thr,
        look_window           = 5,
        bout_window_min       = input$bout_win,
        use_anchored          = input$use_anchored
      )
    }

    add_configured_group <- function(label, color, paths, period = NULL,
                                     names = NULL) {
      gid <- add_group(rv, label = label, color = color)
      g <- rv$groups[[gid]]
      g$monitor_paths <- paths
      # Uploads arrive as generated datapaths; keep the names the user chose.
      g$monitor_names <- if (is.null(names))
        lapply(paths, function(p) if (is.null(p)) NULL else basename(p)) else names
      cfg <- form_config()
      if (!is.null(period)) {
        cfg$start_datetime <- period$start
        cfg$end_datetime   <- period$end
      }
      g$config <- modifyList(g$config, cfg)
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
             food   = input$food$datapath),
        names = list(plug   = input$plug$name,
                     middle = input$middle$name,
                     food   = input$food$name)
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
      # Detect the sample recording's own full range so the demo works no matter
      # what the (possibly empty) period inputs currently hold.
      rng <- tryCatch(detect_datetime_range(unlist(paths), tz = "UTC"),
                      error = function(e) NULL)
      period <- if (is.null(rng)) NULL else list(
        start = format(rng$min, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        end   = format(rng$max, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
      add_configured_group(
        if (nzchar(input$label)) input$label else "Demo group",
        input$color, paths, period = period)
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

    # -- Selection identity ------------------------------------------
    # The table reports a ROW INDEX. That index is only meaningful against the
    # list as it was rendered, so it is resolved to a gid the moment the
    # selection arrives and never stored. Keeping the index instead would
    # desync the first time the list changed under it: with [A, B, C], select
    # B (index 2) and remove it, and index 2 addresses C -- in range, no error,
    # and every "selected" action silently targets a group the user never
    # chose. Removing the LAST group is the same fault with the index out of
    # range, which threw inside a reactive and took the session down with it.
    selected_id <- reactiveVal(NULL)   # a gid, never an index

    observeEvent(input$group_tbl_rows_selected, ignoreNULL = FALSE, {
      sel <- input$group_tbl_rows_selected
      ids <- names(rv$groups)
      selected_id(
        if (length(sel) == 1L && !is.na(sel) && sel >= 1L && sel <= length(ids))
          ids[[sel]] else NULL
      )
    })

    # Total by construction: returns NULL rather than throwing, for a selection
    # that is absent, stale, or points at a group that has since been removed.
    selected_gid <- reactive({
      id <- selected_id()
      if (is.null(id) || is.null(rv$groups[[id]])) NULL else id
    })

    # Keep the app-wide "focused group" in sync for the per-group tabs. A NULL
    # selection is not propagated: remove_group() owns the fallback when the
    # focused group disappears.
    observeEvent(selected_gid(), ignoreNULL = FALSE, {
      gid <- selected_gid()
      if (!is.null(gid)) rv$selected <- gid
    })

    observeEvent(input$remove, {
      gid <- selected_gid(); req(gid)

      # Drop the selection before the list changes, and push the cleared
      # selection to the client explicitly rather than trusting it to redraw.
      # The race this closes: the client still believes row N is selected while
      # selected_gid() re-executes on the flush against a shorter list.
      selected_id(NULL)
      DT::selectRows(DT::dataTableProxy("group_tbl", session = session), NULL)

      dam_cache_drop(gid)          # don't strand the group's aligned temps
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

    # The three monitor roles, in the order the engine expects them.
    dam_roles <- c("plug", "middle", "food")

    # Build the payload handed to the worker. `aligned`, when supplied, is a
    # named character vector of temp paths that REPLACES monitor_paths for
    # this run only -- the group's own record keeps pointing at the user's
    # uploads. Split out from run_one() so the substitution can be asserted
    # directly; run_task$invoke() cannot be intercepted from a test.
    dam_lean_group <- function(g, aligned = NULL) {
      lean <- g[c("id", "label", "color", "monitor_paths", "monitor_names",
                  "config")]
      # Whether THIS run reads aligned copies, which is what lets the reader
      # label them without guessing from the filename.
      lean$aligned <- !is.null(aligned)
      if (!is.null(aligned)) {
        lean$monitor_paths <- as.list(aligned[dam_roles])
      }
      lean
    }

    run_one <- function(gid, aligned = NULL) {
      g <- rv$groups[[gid]]
      g$status <- "queued"; g$error <- NULL
      rv$groups[[gid]] <- g
      append_log(rv, sprintf("\n===== Queued group: %s =====", g$label))

      # Send only what the pipeline needs — never the (large) result fields.
      lean <- dam_lean_group(g, aligned)
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

    # Usable == exactly one non-empty, non-NA path. Checked on the LIST, never
    # on c(...): c() flattens to an atomic vector, so a NULL path vanishes
    # instead of becoming an element. The old guard tested the flattened
    # vector, which meant is.null(p) could never be TRUE (dead code) and a
    # group missing one file produced a length-2 vector that sailed past
    # any(...) into dam_plan_alignment() -- which diagnosed the two surviving
    # files, reported them fine, and skipped alignment silently. Three NULLs
    # produced length 0, where any(logical(0)) is also FALSE.
    dam_paths_usable <- function(monitor_paths) {
      vapply(monitor_paths[dam_roles], function(p) {
        length(p) == 1L && !is.na(p) && nzchar(p)
      }, logical(1))
    }

    # -- aligned-path cache ------------------------------------------
    # gid -> list(key = fingerprint of the ORIGINAL uploads, paths = temps).
    # Confirming alignment is a per-group decision, so remember the answer:
    # without this, every run of a misaligned group re-diagnoses the originals
    # and re-raises the dialog, which trains users to click through the one
    # dialog whose whole value is that it gets read.
    #
    # The group's own monitor_paths are never rewritten -- the temps live here
    # and are passed to run_one() per run, so a group keeps describing what
    # the user actually uploaded.
    aligned_cache <- reactiveVal(list())

    dam_path_key <- function(monitor_paths) {
      paste(vapply(monitor_paths[dam_roles], function(p) {
        if (length(p) == 1L && !is.na(p)) as.character(p) else ""
      }, character(1)), collapse = "\r")
    }

    dam_cache_drop <- function(gid) {
      cur <- aligned_cache()
      if (!is.null(cur[[gid]])) { cur[[gid]] <- NULL; aligned_cache(cur) }
      invisible()
    }

    dam_cache_put <- function(gid, monitor_paths, paths) {
      cur <- aligned_cache()
      cur[[gid]] <- list(key = dam_path_key(monitor_paths), paths = paths)
      aligned_cache(cur)
    }

    # Returns the cached temp paths, or NULL after dropping a stale entry.
    # Two ways an entry goes stale:
    #   - the originals changed, so the plan was computed against other files
    #     (not reachable from the UI today, which makes a new group per upload;
    #      cheap to check, and silently reusing a foreign plan would be ugly);
    #   - a temp file is gone. tempdir() is not guaranteed to survive a long
    #     session, and handing the worker a path that no longer exists would
    #     turn a stale cache into a run failure.
    # Either way the caller falls back to re-planning, which is always correct
    # -- just slower, and it re-asks.
    dam_cache_get <- function(gid, monitor_paths) {
      ent <- aligned_cache()[[gid]]
      if (is.null(ent)) return(NULL)
      if (!identical(ent$key, dam_path_key(monitor_paths))) {
        dam_cache_drop(gid); return(NULL)
      }
      if (!all(nzchar(ent$paths)) || !all(file.exists(ent$paths))) {
        dam_cache_drop(gid); return(NULL)
      }
      ent$paths
    }

    request_run <- function(gids) {
      plans <- list(); need <- character(0)
      for (gid in gids) {
        g  <- rv$groups[[gid]]
        if (!all(dam_paths_usable(g$monitor_paths))) {
          run_one(gid); next                      # let the engine report missing files
        }
        cached <- dam_cache_get(gid, g$monitor_paths)
        if (!is.null(cached)) {
          run_one(gid, aligned = cached); next    # already confirmed: don't re-ask
        }
        mf <- vapply(g$monitor_paths[dam_roles], as.character, character(1))
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
        # Deliberately NOT written back into g$monitor_paths: the group keeps
        # describing the user's uploads, and the temp paths travel with the
        # run instead (see dam_lean_group()). Cached so a re-run of this group
        # does not raise the dialog again.
        aligned <- c(plug   = unname(paths["plug"]),
                     middle = unname(paths["middle"]),
                     food   = unname(paths["food"]))
        dam_cache_put(gid, g$monitor_paths, aligned)
        # Record the alignment on the group itself. monitor_paths is still not
        # touched -- the group keeps describing the user's uploads -- but the
        # report has to outlive rv$log, which is shared, clearable, and gone at
        # session end while the repaired data travels on into figures.
        g$aligned          <- TRUE
        g$alignment_report <- plan$report
        rv$groups[[gid]]   <- g
        append_log(rv, c(sprintf("\n----- Aligned '%s' before run -----", g$label),
                         plan$report))
        showNotification(sprintf("Aligned '%s'; running.", g$label), type = "message")
        run_one(gid, aligned = aligned)
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
