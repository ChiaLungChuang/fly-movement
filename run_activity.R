# ============================================================
# run_activity.R  —  DRIVER: DAM activity pipeline
#
# Companion to run_position.R. Uses `dat` and `dead_summary`.
# If they are not already in the environment, this script sources
# run_position.R to produce them.
#
# Writes 9 workbooks (3 metrics x 3 groupings) to output/Activity per monitor/
# Each workbook has plug / middle / food / summed sheets.
# ============================================================


# ============================================================
# CONFIG  —  edit this block only
# ============================================================

project_dir         <- "/Users/cchuang/Desktop/Fly movement"
position_run_script <- file.path(project_dir, "run_position.R")

# Group label (Rtivity "Labels" field). Keep consistent per genotype
# so workbooks from multiple groups can be stacked downstream.
group_label <- "hTauV337M 50D"

output_dir  <- file.path(project_dir, "output", "Activity per monitor")

lights_on  <- "05:00:00"   # ZT0
lights_off <- "17:00:00"   # ZT12

# Minutes of continuous inactivity that END an activity bout
bout_window_min <- 1


# ============================================================
# STEP 1: LOAD POSITION PIPELINE OUTPUT
# ============================================================

cat("=== Step 1: Load position pipeline output ===\n")

if (!exists("dat") || !exists("dead_summary")) {
  cat(sprintf("  Sourcing: %s\n", basename(position_run_script)))
  source(position_run_script)
  cat("  Sourced successfully.\n")
} else {
  cat("  Using dat and dead_summary from current environment.\n")
}

if (!exists("dat") || !exists("dead_summary")) {
  stop("dat and/or dead_summary not found. Run run_position.R first.")
}

library(data.table)
library(lubridate)
library(openxlsx)

group_slug <- gsub(" ", "_", group_label)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("  dat rows    : %d\n", nrow(dat)))
cat(sprintf("  Channels    : %d\n", length(unique(dat$channel))))
cat(sprintf("  Time range  : %s  to  %s\n",
            format(min(dat$datetime), "%Y-%m-%d %H:%M"),
            format(max(dat$datetime), "%Y-%m-%d %H:%M")))
cat("=== Done ===\n\n")


# ============================================================
# STEP 2: ADD TIME METADATA (day_night, experiment_day)
# ============================================================

cat("=== Step 2: Add time metadata ===\n")

tz <- "America/Chicago"

lights_on_secs  <- as.numeric(hms(lights_on))
lights_off_secs <- as.numeric(hms(lights_off))

secs_from_midnight <- as.numeric(dat$datetime) -
  as.numeric(floor_date(dat$datetime, "day"))

dat[, day_night := secs_from_midnight >= lights_on_secs &
      secs_from_midnight < lights_off_secs]

shifted_posix <- dat$datetime - lights_on_secs
first_zt_day  <- as.Date(min(shifted_posix), tz = tz)
dat[, experiment_day := as.integer(
  as.Date(datetime - lights_on_secs, tz = tz) - first_zt_day
) + 1L]

n_days <- max(dat$experiment_day)
cat(sprintf("  Lights on       : %s\n", lights_on))
cat(sprintf("  Lights off      : %s\n", lights_off))
cat(sprintf("  Experiment days : %d  (ZT-Day 1 starts %s at %s)\n",
            n_days, format(first_zt_day, "%Y-%m-%d"), lights_on))
cat(sprintf("  Light rows      : %d  /  Dark rows : %d\n",
            sum(dat$day_night), sum(!dat$day_night)))
cat("=== Done ===\n\n")


# ============================================================
# STEP 3: BUILD PER-MONITOR ACTIVITY TABLES
# ============================================================

cat("=== Step 3: Build per-monitor activity tables ===\n")

meta_cols <- c("datetime", "channel", "day_night", "experiment_day")

build_monitor_table <- function(activity_col, monitor_name) {
  dt <- dat[, .SD, .SDcols = c(meta_cols, activity_col)]
  setnames(dt, activity_col, "activity")
  dt[, monitor := monitor_name]
  return(dt)
}

act_plug   <- build_monitor_table("B_plug",   "plug")
act_middle <- build_monitor_table("B_middle", "middle")
act_food   <- build_monitor_table("B_food",   "food")

act_sum <- copy(act_plug)
act_sum[, activity := dat$B_plug + dat$B_middle + dat$B_food]
act_sum[, monitor  := "summed"]

monitors_list <- list(
  plug   = act_plug,
  middle = act_middle,
  food   = act_food,
  summed = act_sum
)

cat(sprintf("  Monitor tables built : plug, middle, food, summed\n"))
cat(sprintf("  Rows per table       : %d\n", nrow(act_plug)))
cat("=== Done ===\n\n")


# ============================================================
# STEP 4: DETECT ACTIVITY BOUTS
# ============================================================

cat(sprintf("=== Step 4: Detect activity bouts (window = %d min) ===\n",
            bout_window_min))

detect_bouts <- function(dt_monitor) {

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

  return(dt_monitor)
}

monitors_list <- lapply(monitors_list, detect_bouts)

total_bouts <- sapply(monitors_list, function(dt) sum(dt$activityBout))
cat(sprintf("  Total bouts detected per monitor:\n"))
for (nm in names(total_bouts)) {
  cat(sprintf("    %-8s : %d\n", nm, total_bouts[nm]))
}
cat("=== Done ===\n\n")


# ============================================================
# STEP 5: COMPUTE SUMMARIES
# ============================================================

cat("=== Step 5: Compute summaries ===\n")

pivot_wide <- function(long_dt, value_col) {
  dcast(long_dt, channel ~ period_label, value.var = value_col, fill = NA_real_)
}

add_meta <- function(wide_dt, dt_monitor) {
  meta <- dt_monitor[, .(
    File           = unique(monitor),
    Labels         = group_label,
    start_datetime = min(datetime),
    end_datetime   = max(datetime)
  ), by = channel]
  setnames(meta, "channel", "Channels")
  merge(wide_dt, meta, by.x = "channel", by.y = "Channels", sort = FALSE)
}

summarise_stats <- function(wide_dt, period_cols) {
  stats_list <- lapply(period_cols, function(col) {
    vals <- wide_dt[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) {
      return(data.table(
        Period = col, N = 0L,
        Mean = NA_real_, SEM = NA_real_, SD = NA_real_,
        Median = NA_real_, Q1 = NA_real_, Q3 = NA_real_
      ))
    }
    data.table(
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
  rbindlist(stats_list)
}

compute_summary <- function(dt_monitor, metric) {

  # grouping 1: light phase
  if (metric == "activity_total") {
    long <- dt_monitor[, .(value = sum(activity, na.rm = TRUE)),
                       by = .(channel, day_night)]
  } else if (metric == "bout_activity") {
    long <- dt_monitor[activityBout == TRUE & boutActivity > 0,
                       .(value = mean(boutActivity, na.rm = TRUE)),
                       by = .(channel, day_night)]
  } else {
    long <- dt_monitor[activityBout == TRUE & activityBoutTime > 0,
                       .(value = mean(activityBoutTime, na.rm = TRUE)),
                       by = .(channel, day_night)]
  }
  long[, period_label := ifelse(day_night, "Light", "Dark")]
  lp <- add_meta(pivot_wide(long[, .(channel, period_label, value)], "value"),
                 dt_monitor)

  # grouping 2: per day
  if (metric == "activity_total") {
    long <- dt_monitor[, .(value = sum(activity, na.rm = TRUE)),
                       by = .(channel, experiment_day)]
  } else if (metric == "bout_activity") {
    long <- dt_monitor[activityBout == TRUE & boutActivity > 0,
                       .(value = mean(boutActivity, na.rm = TRUE)),
                       by = .(channel, experiment_day)]
  } else {
    long <- dt_monitor[activityBout == TRUE & activityBoutTime > 0,
                       .(value = mean(activityBoutTime, na.rm = TRUE)),
                       by = .(channel, experiment_day)]
  }
  long[, period_label := paste0("Day_", experiment_day)]
  dp <- add_meta(pivot_wide(long[, .(channel, period_label, value)], "value"),
                 dt_monitor)

  # grouping 3: day x light phase
  if (metric == "activity_total") {
    long <- dt_monitor[, .(value = sum(activity, na.rm = TRUE)),
                       by = .(channel, experiment_day, day_night)]
  } else if (metric == "bout_activity") {
    long <- dt_monitor[activityBout == TRUE & boutActivity > 0,
                       .(value = mean(boutActivity, na.rm = TRUE)),
                       by = .(channel, experiment_day, day_night)]
  } else {
    long <- dt_monitor[activityBout == TRUE & activityBoutTime > 0,
                       .(value = mean(activityBoutTime, na.rm = TRUE)),
                       by = .(channel, experiment_day, day_night)]
  }
  long[, period_label := paste0("Day_", experiment_day, "_",
                                ifelse(day_night, "Light", "Dark"))]
  dlp <- add_meta(pivot_wide(long[, .(channel, period_label, value)], "value"),
                  dt_monitor)

  return(list(light_phase = lp, day = dp, day_light_phase = dlp))
}

summaries <- list()

for (mon in names(monitors_list)) {
  dt_mon <- monitors_list[[mon]]
  summaries[[mon]] <- list(
    activity_total = compute_summary(dt_mon, "activity_total"),
    bout_activity  = compute_summary(dt_mon, "bout_activity"),
    bout_duration  = compute_summary(dt_mon, "bout_duration")
  )
  cat(sprintf("  Summaries computed: %s\n", mon))
}

cat("=== Done ===\n\n")


# ============================================================
# STEP 6: EXPORT EXCEL (9 workbooks)
# ============================================================

cat("=== Step 6: Export Excel workbooks ===\n")

settings_df <- data.frame(
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

write_workbook <- function(grouping_key, metric_key, filename) {

  wb <- createWorkbook()
  monitor_names <- c("plug", "middle", "food", "summed")
  headerStyle   <- createStyle(textDecoration = "bold")

  for (i in seq_along(monitor_names)) {

    mon     <- monitor_names[i]
    wide_dt <- summaries[[mon]][[metric_key]][[grouping_key]]

    period_cols <- setdiff(
      names(wide_dt),
      c("channel", "File", "Labels", "start_datetime", "end_datetime")
    )

    col_order <- c("File", "Labels", "channel",
                   "start_datetime", "end_datetime", period_cols)
    wide_out  <- wide_dt[, ..col_order]
    setnames(wide_out, "channel", "Channels")

    stats_dt <- summarise_stats(wide_out, period_cols)

    if (i == 1) {
      addWorksheet(wb, "Settings")
      writeData(wb, "Settings", settings_df)
      addStyle(wb, "Settings", headerStyle, rows = 1,
               cols = 1:ncol(settings_df), gridExpand = TRUE)
    }

    stats_sheet <- paste0("Statistics_", mon)
    addWorksheet(wb, stats_sheet)
    writeData(wb, stats_sheet, stats_dt)
    addStyle(wb, stats_sheet, headerStyle, rows = 1,
             cols = 1:ncol(stats_dt), gridExpand = TRUE)

    data_sheet <- paste0("All_data_", mon)
    addWorksheet(wb, data_sheet)
    writeData(wb, data_sheet, wide_out)
    addStyle(wb, data_sheet, headerStyle, rows = 1,
             cols = 1:ncol(wide_out), gridExpand = TRUE)

    meta_cols_export <- c("File", "Labels", "Channels",
                          "start_datetime", "end_datetime")
    for (pcol in period_cols) {
      sheet_name    <- substr(paste0(mon, "_", pcol), 1, 31)
      addWorksheet(wb, sheet_name)

      period_export <- wide_out[, ..meta_cols_export]
      period_export[, (pcol) := wide_out[[pcol]]]

      writeData(wb, sheet_name, period_export)
      addStyle(wb, sheet_name, headerStyle, rows = 1,
               cols = 1:ncol(period_export), gridExpand = TRUE)
    }
  }

  out_path <- file.path(output_dir, filename)
  saveWorkbook(wb, out_path, overwrite = TRUE)
  cat(sprintf("  Saved: %s\n", basename(out_path)))
}

export_spec <- list(
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

for (spec in export_spec) {
  write_workbook(spec$grouping, spec$metric, spec$filename)
}

cat(sprintf("\n  Total workbooks written: %d\n", length(export_spec)))
cat("=== Done ===\n")
cat("=== Activity pipeline complete ===\n")
