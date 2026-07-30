# ============================================================
# utils_state.R  —  shared reactive state + helpers
#
# One store keyed by group id. Each group carries its own uploaded
# files, config, and (after a run) its engine results. Modules read
# and write this store; nothing else holds analysis state.
# ============================================================

# Create the app-wide reactive store.
init_state <- function() {
  reactiveValues(
    groups   = list(),        # named by group_id -> see new_group()
    selected = NULL,          # currently focused group_id (for per-group tabs)
    log      = character(0)   # engine console output (newest last)
  )
}

# Append a block of console text to the shared log panel.
append_log <- function(rv, text) {
  if (length(text) == 0) return(invisible())
  rv$log <- c(rv$log, text)
  # keep the panel bounded
  if (length(rv$log) > 4000) rv$log <- tail(rv$log, 4000)
  invisible()
}

# A blank group record. Results fields start NULL and are filled by a run.
new_group <- function(group_id,
                      label = "New group",
                      color = "#0072B2") {
  list(
    id            = group_id,
    label         = label,
    color         = color,
    monitor_paths = list(plug = NULL, middle = NULL, food = NULL),
    # Original upload names by role. Shiny's fileInput datapaths are generated
    # ("0.txt"), so these are the only record of which file is which.
    monitor_names = list(plug = NULL, middle = NULL, food = NULL),
    config = list(
      channels              = 1:32,
      start_datetime        = NULL,   # detected from files (app) or set by caller
      end_datetime          = NULL,
      tz                    = "UTC",   # parse timestamps as naive wall-clock
      lights_on             = "05:00:00",
      lights_off            = "17:00:00",
      death_threshold_hours = 12,
      sleep_threshold_min   = 5,
      look_window           = 5,
      bout_window_min       = 1,
      use_anchored          = TRUE
    ),
    status = "configured",             # configured | running | done | error
    error  = NULL,
    # results (filled after a successful run):
    monitors = NULL, dat = NULL, dead_summary = NULL, unknown_diag = NULL,
    anchors = NULL, summaries = NULL, chrono_long = NULL
  )
}

add_group <- function(rv, label = "New group", color = "#0072B2") {
  id <- paste0("g", as.integer(Sys.time()), "_",
               sample.int(1e4, 1))
  rv$groups[[id]] <- new_group(id, label, color)
  rv$selected <- id
  id
}

remove_group <- function(rv, group_id) {
  rv$groups[[group_id]] <- NULL
  if (identical(rv$selected, group_id)) {
    rv$selected <- if (length(rv$groups)) names(rv$groups)[[1]] else NULL
  }
  invisible(TRUE)
}

set_group_results <- function(rv, group_id, results) {
  for (nm in names(results)) rv$groups[[group_id]][[nm]] <- results[[nm]]
  invisible(TRUE)
}

# Groups that have completed a run (used by combined comparison reactives).
done_groups <- function(rv) {
  Filter(function(g) identical(g$status, "done"), rv$groups)
}

# Named choices (label -> id) for a per-group selectInput.
# only_done = TRUE restricts to groups with results.
group_choices <- function(rv, only_done = TRUE) {
  gs <- if (only_done) done_groups(rv) else rv$groups
  if (!length(gs)) return(character(0))
  setNames(names(gs), vapply(gs, `[[`, "", "label"))
}

# Named vector Labels -> color across all groups (for combined plots).
group_colors <- function(rv) {
  gs <- rv$groups
  if (!length(gs)) return(character(0))
  setNames(vapply(gs, `[[`, "", "color"),
           vapply(gs, `[[`, "", "label"))
}

# ============================================================
# run_group_pipeline() — the engine call order from SHINY_APP_PLAN.md §11,
# wrapped as one function so the module server stays thin. Returns a list
# of result fields to store via set_group_results(); throws on failure
# (caller wraps in tryCatch and sets status = "error").
#
# progress() is an optional callback (value in [0,1], message) used by the
# module to drive withProgress(); log() captures console output.
# ============================================================
run_group_pipeline <- function(g, progress = NULL, log = NULL) {

  say  <- function(msg) if (!is.null(log)) log(msg)
  step <- function(v, msg) { if (!is.null(progress)) progress(v, msg); say(msg) }

  cfg <- g$config

  paths <- g$monitor_paths[c("plug", "middle", "food")]
  ok <- vapply(paths, function(p) !is.null(p) && nzchar(p), logical(1))
  if (!all(ok)) {
    stop("All three monitor files (plug, middle, food) are required.")
  }
  mf <- c(plug = paths$plug, middle = paths$middle, food = paths$food)

  capture_engine <- function(expr) {
    out <- utils::capture.output(val <- eval.parent(substitute(expr)))
    if (!is.null(log)) log(out)
    val
  }

  step(0.05, "Reading monitor files")
  monitors <- capture_engine(
    read_position_monitors(mf, cfg$channels,
                           cfg$start_datetime, cfg$end_datetime, cfg$tz,
                           source_names = g$monitor_names))

  step(0.20, "Detecting dead channels")
  dead_summary <- capture_engine(
    detect_dead_channels(monitors, cfg$death_threshold_hours))
  monitors <- capture_engine(remove_dead_channels(monitors, dead_summary))

  # Stop at the step that knows what happened. assign_position_zones() derives
  # its channel list from the ch* columns of monitors$plug; with none left it
  # reshapes an empty table and data.table reports
  #   some columns are not in the data.table: [channel, datetime]
  # two steps downstream, naming neither the channels, the threshold, nor the
  # removal that emptied it. Checked against the same columns the consumer
  # reads, so the guard cannot disagree with what actually breaks.
  if (!length(grep("^ch\\d+$", names(monitors$plug), value = TRUE))) {
    n_dead  <- sum(dead_summary$status == "DEAD")
    n_total <- nrow(dead_summary)
    stop(sprintf(
      paste("all %d of %d channel(s) classified dead at threshold %.1f h;",
            "nothing left to analyse. Raise the death threshold, or check",
            "whether the recording really is flat."),
      n_dead, n_total, cfg$death_threshold_hours))
  }

  step(0.35, "Assigning position zones (state machine)")
  result <- capture_engine(assign_position_zones(monitors))

  step(0.50, "Backfilling + detecting sleep bouts")
  result <- capture_engine(
    backfill_and_detect_sleep(result, cfg$sleep_threshold_min))
  dat          <- result$dat
  unknown_diag <- result$unknown_diag

  step(0.65, "Anchor-based position assignment")
  anchor_result <- capture_engine(
    assign_position_anchored(dat, cfg$sleep_threshold_min, cfg$look_window))
  dat     <- anchor_result$dat
  anchors <- anchor_result$anchors

  step(0.85, "Building activity summaries")
  summaries <- capture_engine(
    build_activity_summaries(dat, g$label,
                             cfg$lights_on, cfg$lights_off, cfg$tz,
                             cfg$bout_window_min))

  step(0.95, "Building chronogram")
  chrono_long <- make_activity_chronogram(dat, g$label, monitor = "summed",
                                          bin_min = 10)

  step(1.0, "Done")

  list(
    monitors     = monitors,
    dat          = dat,
    dead_summary = dead_summary,
    unknown_diag = unknown_diag,
    anchors      = anchors,
    summaries    = summaries,
    chrono_long  = chrono_long,
    status       = "done",
    error        = NULL
  )
}
