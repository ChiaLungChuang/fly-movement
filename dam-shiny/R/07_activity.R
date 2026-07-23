# ============================================================
# R/07_activity.R  —  ACTIVITY PIPELINE (function definitions only)
#
# Extracts the activity-summary logic that used to live at top level
# in run_activity.R into reusable, side-effect-free functions so the
# Shiny app can call them directly. run_activity.R keeps its own inline
# copy for headless/batch runs, so nothing here changes that script.
#
# Provides:
#   add_time_metadata()          — day_night + experiment_day columns
#   build_monitor_activity_tables() — plug/middle/food/summed long tables
#   detect_activity_bouts()      — activity-bout flags per channel
#   build_activity_summaries()   — nested monitor x metric x grouping tables
#   activity_settings_df()       — the "Settings" sheet contents
#   write_activity_workbook()    — one .xlsx workbook (matches run_activity.R)
#   activity_summary_table()     — pull one wide table for on-screen display
#
# Requires (loaded by the app): data.table, lubridate, openxlsx
# ============================================================


# ============================================================
# Add day_night + experiment_day metadata (from run_activity.R Step 2)
# Returns a copy of dat with the two columns added.
# ============================================================
add_time_metadata <- function(dat,
                              lights_on  = "05:00:00",
                              lights_off = "17:00:00",
                              tz         = "America/Chicago") {

  dat <- data.table::copy(data.table::as.data.table(dat))

  lights_on_secs  <- as.numeric(lubridate::hms(lights_on))
  lights_off_secs <- as.numeric(lubridate::hms(lights_off))

  secs_from_midnight <- as.numeric(dat$datetime) -
    as.numeric(lubridate::floor_date(dat$datetime, "day"))

  dat[, day_night := secs_from_midnight >= lights_on_secs &
        secs_from_midnight < lights_off_secs]

  shifted_posix <- dat$datetime - lights_on_secs
  first_zt_day  <- as.Date(min(shifted_posix), tz = tz)
  dat[, experiment_day := as.integer(
    as.Date(datetime - lights_on_secs, tz = tz) - first_zt_day
  ) + 1L]

  dat
}


# ============================================================
# Build the four per-monitor activity tables (from run_activity.R Step 3).
# dat must already carry day_night + experiment_day.
# ============================================================
build_monitor_activity_tables <- function(dat, group_label) {

  meta_cols <- c("datetime", "channel", "day_night", "experiment_day")

  build_monitor_table <- function(activity_col, monitor_name) {
    dt <- dat[, .SD, .SDcols = c(meta_cols, activity_col)]
    data.table::setnames(dt, activity_col, "activity")
    dt[, monitor := monitor_name]
    dt
  }

  act_plug   <- build_monitor_table("B_plug",   "plug")
  act_middle <- build_monitor_table("B_middle", "middle")
  act_food   <- build_monitor_table("B_food",   "food")

  act_sum <- data.table::copy(act_plug)
  act_sum[, activity := dat$B_plug + dat$B_middle + dat$B_food]
  act_sum[, monitor  := "summed"]

  list(plug = act_plug, middle = act_middle, food = act_food, summed = act_sum)
}


# ============================================================
# Detect activity bouts per channel (from run_activity.R Step 4).
# Mutates and returns dt_monitor (activityBout / activityBoutTime /
# boutActivity columns).
# ============================================================
detect_activity_bouts <- function(dt_monitor, bout_window_min = 1) {

  channels <- sort(unique(dt_monitor$channel))

  dt_monitor[, activityBout     := FALSE]
  dt_monitor[, activityBoutTime := 0L]
  dt_monitor[, boutActivity     := 0.0]

  for (ch in channels) {

    idx      <- which(dt_monitor$channel == ch)
    activity <- dt_monitor$activity[idx]
    n        <- length(activity)

    is_active <- activity > 0

    if (bout_window_min > 1) {
      t <- 1
      while (t <= n) {
        if (!is_active[t]) {
          gap_end <- t
          while (gap_end < n && !is_active[gap_end + 1]) gap_end <- gap_end + 1
          gap_len <- gap_end - t + 1
          if (gap_len < bout_window_min) {
            is_active[t:gap_end] <- TRUE
          }
          t <- gap_end + 1
        } else {
          t <- t + 1
        }
      }
    }

    rle_result    <- rle(is_active)
    bout_ends     <- cumsum(rle_result$lengths)
    bout_starts   <- c(1L, bout_ends[-length(bout_ends)] + 1L)
    is_active_run <- rle_result$values

    for (b in which(is_active_run)) {
      s     <- bout_starts[b]
      e     <- bout_ends[b]
      row_s <- idx[s]
      row_e <- idx[e]

      dt_monitor$activityBout[row_s]     <- TRUE
      dt_monitor$activityBoutTime[row_s] <- e - s + 1L
      dt_monitor$boutActivity[row_s]     <- sum(
        dt_monitor$activity[row_s:row_e], na.rm = TRUE)
    }
  }

  dt_monitor
}


# -- Internal summary helpers (from run_activity.R Step 5) -----------
.act_pivot_wide <- function(long_dt, value_col) {
  data.table::dcast(long_dt, channel ~ period_label,
                    value.var = value_col, fill = NA_real_)
}

.act_add_meta <- function(wide_dt, dt_monitor, group_label) {
  meta <- dt_monitor[, .(
    File           = unique(monitor),
    Labels         = group_label,
    start_datetime = min(datetime),
    end_datetime   = max(datetime)
  ), by = channel]
  data.table::setnames(meta, "channel", "Channels")
  merge(wide_dt, meta, by.x = "channel", by.y = "Channels", sort = FALSE)
}

.act_compute_summary <- function(dt_monitor, metric, group_label) {

  make_long <- function(by_cols) {
    if (metric == "activity_total") {
      dt_monitor[, .(value = sum(activity, na.rm = TRUE)), by = by_cols]
    } else if (metric == "bout_activity") {
      dt_monitor[activityBout == TRUE & boutActivity > 0,
                 .(value = mean(boutActivity, na.rm = TRUE)), by = by_cols]
    } else {
      dt_monitor[activityBout == TRUE & activityBoutTime > 0,
                 .(value = mean(activityBoutTime, na.rm = TRUE)), by = by_cols]
    }
  }

  # grouping 1: light phase
  long <- make_long(c("channel", "day_night"))
  long[, period_label := ifelse(day_night, "Light", "Dark")]
  lp <- .act_add_meta(.act_pivot_wide(long[, .(channel, period_label, value)], "value"),
                      dt_monitor, group_label)

  # grouping 2: per day
  long <- make_long(c("channel", "experiment_day"))
  long[, period_label := paste0("Day_", experiment_day)]
  dp <- .act_add_meta(.act_pivot_wide(long[, .(channel, period_label, value)], "value"),
                      dt_monitor, group_label)

  # grouping 3: day x light phase
  long <- make_long(c("channel", "experiment_day", "day_night"))
  long[, period_label := paste0("Day_", experiment_day, "_",
                                ifelse(day_night, "Light", "Dark"))]
  dlp <- .act_add_meta(.act_pivot_wide(long[, .(channel, period_label, value)], "value"),
                       dt_monitor, group_label)

  list(light_phase = lp, day = dp, day_light_phase = dlp)
}


# ============================================================
# Build every activity summary: monitor x metric x grouping.
# Self-contained: adds time metadata, builds monitor tables, detects
# bouts, and computes the nested summary list the app renders and the
# workbook writer consumes.
# ============================================================
build_activity_summaries <- function(dat,
                                     group_label,
                                     lights_on       = "05:00:00",
                                     lights_off      = "17:00:00",
                                     tz              = "America/Chicago",
                                     bout_window_min = 1) {

  dat_meta      <- add_time_metadata(dat, lights_on, lights_off, tz)
  monitors_list <- build_monitor_activity_tables(dat_meta, group_label)
  monitors_list <- lapply(monitors_list, detect_activity_bouts,
                          bout_window_min = bout_window_min)

  summaries <- list()
  for (mon in names(monitors_list)) {
    dt_mon <- monitors_list[[mon]]
    summaries[[mon]] <- list(
      activity_total = .act_compute_summary(dt_mon, "activity_total", group_label),
      bout_activity  = .act_compute_summary(dt_mon, "bout_activity",  group_label),
      bout_duration  = .act_compute_summary(dt_mon, "bout_duration",  group_label)
    )
  }
  summaries
}


# -- Per-period descriptive statistics (from run_activity.R) ---------
summarise_activity_stats <- function(wide_dt, period_cols) {
  stats_list <- lapply(period_cols, function(col) {
    vals <- wide_dt[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) {
      return(data.table::data.table(
        Period = col, N = 0L,
        Mean = NA_real_, SEM = NA_real_, SD = NA_real_,
        Median = NA_real_, Q1 = NA_real_, Q3 = NA_real_
      ))
    }
    data.table::data.table(
      Period = col,
      N      = length(vals),
      Mean   = round(mean(vals, na.rm = TRUE), 4),
      SEM    = round(sd(vals, na.rm = TRUE) / sqrt(length(vals)), 4),
      SD     = round(sd(vals, na.rm = TRUE), 4),
      Median = round(median(vals, na.rm = TRUE), 4),
      Q1     = round(quantile(vals, 0.25, na.rm = TRUE), 4),
      Q3     = round(quantile(vals, 0.75, na.rm = TRUE), 4)
    )
  })
  data.table::rbindlist(stats_list)
}


# ============================================================
# One wide summary table (File/Labels/Channels/... + period columns)
# for on-screen display. monitor x metric x grouping selectors.
# ============================================================
activity_summary_table <- function(summaries, monitor, metric, grouping) {
  wide_dt <- summaries[[monitor]][[metric]][[grouping]]
  if (is.null(wide_dt)) return(NULL)

  period_cols <- setdiff(
    names(wide_dt),
    c("channel", "File", "Labels", "start_datetime", "end_datetime")
  )
  col_order <- c("File", "Labels", "channel",
                 "start_datetime", "end_datetime", period_cols)
  wide_out  <- wide_dt[, ..col_order]
  data.table::setnames(wide_out, "channel", "Channels")
  wide_out
}


# ============================================================
# Settings sheet contents (from run_activity.R Step 6).
# ============================================================
activity_settings_df <- function(dat, dead_summary, group_label,
                                 lights_on, lights_off, bout_window_min) {
  data.frame(
    Parameter = c(
      "Group label", "Lights on", "Lights off", "Bout window (min)",
      "Analysis start", "Analysis end", "Channels analyzed", "Dead channels"
    ),
    Value = c(
      group_label, lights_on, lights_off, as.character(bout_window_min),
      format(min(dat$datetime), "%Y-%m-%d %H:%M"),
      format(max(dat$datetime), "%Y-%m-%d %H:%M"),
      paste(sort(unique(dat$channel)), collapse = ", "),
      {
        dead <- dead_summary[status == "DEAD", channel]
        if (length(dead) == 0) "none" else paste(sort(dead), collapse = ", ")
      }
    ),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# Write one activity workbook (plug/middle/food/summed sheets) for a
# given grouping x metric. Mirrors run_activity.R's write_workbook().
# ============================================================
write_activity_workbook <- function(summaries, grouping_key, metric_key,
                                    group_label, settings_df, output_file) {

  wb            <- openxlsx::createWorkbook()
  monitor_names <- c("plug", "middle", "food", "summed")
  headerStyle   <- openxlsx::createStyle(textDecoration = "bold")

  for (i in seq_along(monitor_names)) {

    mon      <- monitor_names[i]
    wide_out <- activity_summary_table(summaries, mon, metric_key, grouping_key)
    period_cols <- setdiff(
      names(wide_out),
      c("File", "Labels", "Channels", "start_datetime", "end_datetime")
    )

    stats_dt <- summarise_activity_stats(wide_out, period_cols)

    if (i == 1) {
      openxlsx::addWorksheet(wb, "Settings")
      openxlsx::writeData(wb, "Settings", settings_df)
      openxlsx::addStyle(wb, "Settings", headerStyle, rows = 1,
                         cols = 1:ncol(settings_df), gridExpand = TRUE)
    }

    stats_sheet <- paste0("Statistics_", mon)
    openxlsx::addWorksheet(wb, stats_sheet)
    openxlsx::writeData(wb, stats_sheet, stats_dt)
    openxlsx::addStyle(wb, stats_sheet, headerStyle, rows = 1,
                       cols = 1:ncol(stats_dt), gridExpand = TRUE)

    data_sheet <- paste0("All_data_", mon)
    openxlsx::addWorksheet(wb, data_sheet)
    openxlsx::writeData(wb, data_sheet, wide_out)
    openxlsx::addStyle(wb, data_sheet, headerStyle, rows = 1,
                       cols = 1:ncol(wide_out), gridExpand = TRUE)

    meta_cols_export <- c("File", "Labels", "Channels",
                          "start_datetime", "end_datetime")
    for (pcol in period_cols) {
      sheet_name <- substr(paste0(mon, "_", pcol), 1, 31)
      openxlsx::addWorksheet(wb, sheet_name)

      period_export <- wide_out[, ..meta_cols_export]
      period_export[, (pcol) := wide_out[[pcol]]]

      openxlsx::writeData(wb, sheet_name, period_export)
      openxlsx::addStyle(wb, sheet_name, headerStyle, rows = 1,
                         cols = 1:ncol(period_export), gridExpand = TRUE)
    }
  }

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  invisible(output_file)
}


# The 9 workbook specs (grouping x metric) used by the Activity tab and
# the download bundle. filename() builds the per-group name.
activity_export_spec <- function(group_slug) {
  list(
    list(grouping = "light_phase",     metric = "activity_total",
         filename = paste0(group_slug, "_activity_per_light_phase.xlsx")),
    list(grouping = "day",             metric = "activity_total",
         filename = paste0(group_slug, "_activity_per_day.xlsx")),
    list(grouping = "day_light_phase", metric = "activity_total",
         filename = paste0(group_slug, "_activity_per_day_light_phase.xlsx")),
    list(grouping = "light_phase",     metric = "bout_activity",
         filename = paste0(group_slug, "_activity_per_bout_per_light_phase.xlsx")),
    list(grouping = "day",             metric = "bout_activity",
         filename = paste0(group_slug, "_activity_per_bout_per_day.xlsx")),
    list(grouping = "day_light_phase", metric = "bout_activity",
         filename = paste0(group_slug, "_activity_per_bout_per_day_light_phase.xlsx")),
    list(grouping = "light_phase",     metric = "bout_duration",
         filename = paste0(group_slug, "_bout_duration_per_light_phase.xlsx")),
    list(grouping = "day",             metric = "bout_duration",
         filename = paste0(group_slug, "_bout_duration_per_day.xlsx")),
    list(grouping = "day_light_phase", metric = "bout_duration",
         filename = paste0(group_slug, "_bout_duration_per_day_light_phase.xlsx"))
  )
}
