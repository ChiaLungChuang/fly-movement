# ============================================================
# R/01_read_qc.R  —  READ + QC (function definitions only)
#
# Provides:
#   read_position_monitors()  — read plug/middle/food monitor files
#   detect_dead_channels()    — flag dead channels from summed signal
#   remove_dead_channels()    — drop dead channel columns
#
# Requires (loaded by the run script): data.table, lubridate
# No code executes when this file is sourced — definitions only.
# ============================================================


# ============================================================
# STEP 1: READ MONITOR FILES
#
# Returns a named list:
#   $plug, $middle, $food  — individual monitor data.tables
#                            columns: datetime, ch1 ... ch32
#                            time range: pre-window start -> end_datetime
#   $summed                — summed activity (dead detection only)
#   $analysis_start        — POSIXct: the original start_datetime,
#                            used to trim dat after state machine
# ============================================================

read_position_monitors <- function(
    monitor_files,         # Named vector: c(plug=, middle=, food=)
    channels       = 1:32,
    start_datetime = NULL,
    end_datetime   = NULL,
    tz             = "America/Chicago"
) {

  required_roles <- c("plug", "middle", "food")

  if (!all(required_roles %in% names(monitor_files))) {
    stop(paste(
      "monitor_files must be a named vector with names: plug, middle, food\n",
      "Example:\n",
      "  c(plug   = 'Monitor1.txt',\n",
      "    middle = 'Monitor2.txt',\n",
      "    food   = 'Monitor3.txt')"
    ))
  }

  if (!all(channels %in% 1:32)) {
    stop("channels must be integers between 1 and 32")
  }

  # -- Internal: parse one monitor file --------------------------
  read_one_monitor <- function(path, role) {
    cat(sprintf("  Reading [%s]: %s\n", role, basename(path)))

    dt <- fread(path, header = FALSE)

    cat(sprintf("    Total columns detected: %d\n", ncol(dt)))
    cat(sprintf("    V2 sample: %s | V3 sample: %s\n", dt[[2]][1], dt[[3]][1]))

    # Detect datetime layout (Case A or Case B)
    if (grepl("\\d{2}:\\d{2}:\\d{2}", dt[[3]][1])) {
      cat("    Datetime format: Case A (date in V2, time in V3)\n")
      datetime_str <- paste(dt[[2]], dt[[3]])
      datetime_str <- gsub("(\\d{1,2} [A-Za-z]{3} )(\\d{2})( )",
                           "\\120\\2\\3", datetime_str)
      dt[, datetime := dmy_hms(datetime_str, tz = tz)]
      activity_start_offset <- 11

    } else if (grepl("\\d{2}:\\d{2}:\\d{2}", dt[[5]][1])) {
      cat("    Datetime format: Case B (split cols, time in V5)\n")
      year_str <- ifelse(nchar(as.character(dt[[4]])) == 2,
                         paste0("20", dt[[4]]),
                         as.character(dt[[4]]))
      datetime_str <- paste(dt[[2]], dt[[3]], year_str, dt[[5]])
      dt[, datetime := dmy_hms(datetime_str, tz = tz)]
      activity_start_offset <- 6

    } else {
      stop(sprintf("Cannot detect datetime format in '%s'. Check columns 2-6.",
                   basename(path)))
    }

    if (any(is.na(dt$datetime))) {
      warning(sprintf("%d rows failed datetime parsing in %s",
                      sum(is.na(dt$datetime)), basename(path)))
    }

    # Align to minute — prevents sub-minute residuals misaligning
    # rows when merging across three monitor files
    dt[, datetime := floor_date(datetime, "minute")]

    activity_indices <- activity_start_offset:(activity_start_offset + 31)

    if (max(activity_indices) > ncol(dt)) {
      stop(sprintf(
        "Expected activity cols %d:%d but file only has %d columns",
        min(activity_indices), max(activity_indices), ncol(dt)
      ))
    }

    cat(sprintf("    Activity cols: %d to %d\n",
                min(activity_indices), max(activity_indices)))

    # Extract datetime + selected channel columns
    out <- dt[, .(datetime)]
    for (ch in channels) {
      col_idx <- activity_indices[ch]
      set(out, j = paste0("ch", ch), value = as.numeric(dt[[col_idx]]))
    }

    return(out)
  }

  # -- Read all three monitors -----------------------------------
  cat("\n=== Reading monitor files ===\n")
  dat_plug   <- read_one_monitor(monitor_files["plug"],   "plug")
  dat_middle <- read_one_monitor(monitor_files["middle"], "middle")
  dat_food   <- read_one_monitor(monitor_files["food"],   "food")

  # -- Validate row counts ---------------------------------------
  n_rows <- c(nrow(dat_plug), nrow(dat_middle), nrow(dat_food))
  if (length(unique(n_rows)) != 1) {
    stop(sprintf(
      "Monitor files have different row counts: plug=%d, middle=%d, food=%d\nFiles must be time-synchronized.",
      n_rows[1], n_rows[2], n_rows[3]
    ))
  }

  if (!all(dat_plug$datetime == dat_middle$datetime) ||
      !all(dat_plug$datetime == dat_food$datetime)) {
    warning("Datetime values differ between monitors. Using plug monitor timestamps.")
  }

  # -- Find common start across all three monitors --------------
  common_start     <- max(min(dat_plug$datetime),
                          min(dat_middle$datetime),
                          min(dat_food$datetime))
  pre_window_start <- common_start + 300   # +5 minutes

  cat(sprintf("  Common file start: %s\n",
              format(common_start,     "%Y-%m-%d %H:%M")))
  cat(sprintf("  Pre-window start : %s (+5 min offset)\n",
              format(pre_window_start, "%Y-%m-%d %H:%M")))

  # -- Time filtering --------------------------------------------
  analysis_start <- NULL

  apply_time_filter <- function(dt_in) {
    dt_in <- dt_in[datetime >= pre_window_start]
    if (!is.null(start_datetime)) {
      analysis_start <<- as.POSIXct(start_datetime, tz = tz)
      cat(sprintf("  Analysis start   : %s\n",
                  format(analysis_start, "%Y-%m-%d %H:%M")))
    }
    if (!is.null(end_datetime)) {
      end_dt <- as.POSIXct(end_datetime, tz = tz)
      dt_in  <- dt_in[datetime <= end_dt]
      cat(sprintf("  End              : %s\n",
                  format(end_dt, "%Y-%m-%d %H:%M")))
    }
    return(dt_in)
  }

  dat_plug   <- apply_time_filter(dat_plug)
  dat_middle <- apply_time_filter(dat_middle)
  dat_food   <- apply_time_filter(dat_food)

  # -- Build summed activity table (dead detection only) --------
  ch_cols <- paste0("ch", channels)
  dat_sum <- copy(dat_plug)

  for (col in ch_cols) {
    dat_sum[, (col) := dat_plug[[col]] + dat_middle[[col]] + dat_food[[col]]]
  }

  # -- Summary ---------------------------------------------------
  cat(sprintf("\n=== Done ===\n"))
  cat(sprintf("  Channels  : %s\n", paste(channels, collapse = ", ")))
  cat(sprintf("  Full read : %s  to  %s (%d rows incl. pre-window)\n",
              format(min(dat_plug$datetime), "%Y-%m-%d %H:%M"),
              format(max(dat_plug$datetime), "%Y-%m-%d %H:%M"),
              nrow(dat_plug)))
  cat(sprintf("  Output    : list with $plug, $middle, $food, $summed, $analysis_start\n\n"))

  return(list(
    plug           = dat_plug,
    middle         = dat_middle,
    food           = dat_food,
    summed         = dat_sum,
    analysis_start = analysis_start
  ))
}


# ============================================================
# STEP 2: DETECT DEAD CHANNELS FROM SUMMED SIGNAL
# ============================================================

detect_dead_channels <- function(
    monitors,
    death_threshold_hours = 12
) {

  cat("=== Dead Channel Detection (summed signal) ===\n")
  cat(sprintf("  Death threshold : %.1f hours continuous inactivity\n",
              death_threshold_hours))

  summed         <- monitors$summed
  ch_cols        <- grep("^ch\\d+$", names(summed), value = TRUE)
  channels       <- as.integer(sub("ch", "", ch_cols))
  threshold_mins <- death_threshold_hours * 60

  results <- vector("list", length(channels))

  for (i in seq_along(channels)) {
    ch       <- channels[i]
    activity <- summed[[paste0("ch", ch)]]

    is_zero    <- activity == 0
    rle_result <- rle(is_zero)

    max_zero_mins  <- 0
    death_datetime <- as.POSIXct(NA)

    if (any(rle_result$values)) {
      zero_lengths  <- rle_result$lengths[rle_result$values]
      max_zero_mins <- max(zero_lengths)

      if (max_zero_mins >= threshold_mins) {
        cumlen     <- cumsum(rle_result$lengths)
        run_idx    <- which(rle_result$values &
                              rle_result$lengths == max_zero_mins)[1]
        death_idx  <- if (run_idx > 1) cumlen[run_idx - 1] + 1L else 1L
        death_datetime <- summed$datetime[death_idx]
      }
    }

    max_zero_hours <- max_zero_mins / 60
    is_dead        <- max_zero_hours >= death_threshold_hours

    results[[i]] <- data.table(
      channel              = ch,
      status               = if (is_dead) "DEAD" else "ALIVE",
      max_inactivity_hours = round(max_zero_hours, 2),
      death_datetime       = death_datetime
    )
  }

  summary_dt <- rbindlist(results)

  n_dead  <- sum(summary_dt$status == "DEAD")
  n_alive <- sum(summary_dt$status == "ALIVE")

  cat(sprintf("  Channels scanned : %d\n", nrow(summary_dt)))
  cat(sprintf("  Alive            : %d\n", n_alive))
  cat(sprintf("  Dead             : %d\n", n_dead))

  if (n_dead > 0) {
    cat("\n  Dead channels:\n")
    dead_dt <- summary_dt[status == "DEAD"]
    for (j in seq_len(nrow(dead_dt))) {
      cat(sprintf("    Ch %2d | max inactivity: %.1f h | death onset: %s\n",
                  dead_dt$channel[j],
                  dead_dt$max_inactivity_hours[j],
                  format(dead_dt$death_datetime[j], "%Y-%m-%d %H:%M")))
    }
  } else {
    cat("  No dead channels detected.\n")
  }

  cat("=== Done ===\n\n")
  return(summary_dt)
}


# ============================================================
# STEP 3: REMOVE DEAD CHANNELS FROM INDIVIDUAL MONITOR DATA
# ============================================================

remove_dead_channels <- function(
    monitors,
    dead_summary
) {

  dead_channels <- dead_summary[status == "DEAD", channel]

  if (length(dead_channels) == 0) {
    cat("=== Remove Dead Channels: no dead channels to remove ===\n\n")
    return(monitors)
  }

  cat(sprintf("=== Removing %d dead channel(s): %s ===\n",
              length(dead_channels),
              paste(sort(dead_channels), collapse = ", ")))

  dead_cols <- paste0("ch", dead_channels)

  for (role in c("plug", "middle", "food")) {
    cols_present <- intersect(dead_cols, names(monitors[[role]]))
    if (length(cols_present) > 0) {
      monitors[[role]][, (cols_present) := NULL]
    }
  }

  surviving          <- grep("^ch\\d+$", names(monitors$plug), value = TRUE)
  surviving_channels <- sort(as.integer(sub("ch", "", surviving)))
  cat(sprintf("  Channels remaining: %s\n",
              paste(surviving_channels, collapse = ", ")))
  cat("=== Done ===\n\n")

  return(monitors)
}
