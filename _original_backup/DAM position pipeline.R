# ============================================================
# DAM POSITION PIPELINE
#
# Step order:
#   1. Read all three monitor files individually
#      (pre-window = file common start + 5 min → start_datetime)
#   2. Detect dead channels from summed signal (RLE, full recording)
#   3. Remove dead channels from individual monitor data
#   4. Assign position zones + sleep zones (state machine)
#      - pre-window seeds position state, then trimmed before
#        sleep detection and plotting
#      - UNKNOWN diagnostics printed per channel
#   4.5 Backfill startup UNKNOWN zones (forward-lookup from
#      first known zone after analysis_start)
#   5. Visualize: individual timelines, per-fly heatmap,
#      group summary heatmap, summary bar plot
#   6. Export Excel workbook
# ============================================================

library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(openxlsx)   # Excel export (install.packages("openxlsx") if needed)

# ── Color palette for zones ──────────────────────────────────
zone_colors <- c(
  "Z1"      = "#2166AC",
  "Z2"      = "#92C5DE",
  "Z3"      = "#F4A582",
  "Z4"      = "#D6604D",
  "UNKNOWN" = "#CCCCCC"
)

zone_labels <- c(
  "Z1"      = "Z1 (plug end)",
  "Z2"      = "Z2 (plug-mid)",
  "Z3"      = "Z3 (mid-food)",
  "Z4"      = "Z4 (food end)",
  "UNKNOWN" = "Unknown"
)


# ============================================================
# STEP 1: READ MONITOR FILES
#
# Returns a named list:
#   $plug, $middle, $food  — individual monitor data.tables
#                            columns: datetime, ch1 ... ch32
#                            time range: pre-window start → end_datetime
#   $summed                — summed activity (analysis window only)
#   $analysis_start        — POSIXct: the original start_datetime,
#                            used to trim dat after state machine
#
# Pre-window logic:
#   1. Read all three monitors fully (no filter yet)
#   2. Find the latest first timepoint across all three monitors
#      (common_start) — guarantees all files are aligned
#   3. Add 5 minutes to skip initial recording artefacts
#      → pre_window_start = common_start + 5 min
#   4. Filter all three monitors from pre_window_start onward
#
# The state machine in Step 4 runs on the full pre-window range,
# giving every fly the maximum opportunity to break a beam before
# the analysis window begins. Pre-window rows are trimmed before
# sleep detection and do not appear in any output.
#
# $summed is NOT trimmed to analysis window — dead detection must
# scan the full recording from pre_window_start so flies that died
# before or during the pre-window are correctly identified.
# ============================================================

read_position_monitors <- function(
    monitor_files,         # Named vector: c(plug=, middle=, food=)
    channels       = 1:32,
    start_datetime = NULL,
    end_datetime   = NULL
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
  
  # ── Internal: parse one monitor file ──────────────────────
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
      dt[, datetime := dmy_hms(datetime_str, tz = "America/Chicago")]
      activity_start_offset <- 11
      
    } else if (grepl("\\d{2}:\\d{2}:\\d{2}", dt[[5]][1])) {
      cat("    Datetime format: Case B (split cols, time in V5)\n")
      year_str <- ifelse(nchar(as.character(dt[[4]])) == 2,
                         paste0("20", dt[[4]]),
                         as.character(dt[[4]]))
      datetime_str <- paste(dt[[2]], dt[[3]], year_str, dt[[5]])
      dt[, datetime := dmy_hms(datetime_str, tz = "America/Chicago")]
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
  
  # ── Read all three monitors ────────────────────────────────
  cat("\n=== Reading monitor files ===\n")
  dat_plug   <- read_one_monitor(monitor_files["plug"],   "plug")
  dat_middle <- read_one_monitor(monitor_files["middle"], "middle")
  dat_food   <- read_one_monitor(monitor_files["food"],   "food")
  
  # ── Validate row counts ───────────────────────────────────
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
  
  # ── Find common start across all three monitors ──────────
  # Use the latest first timepoint across all three monitors so
  # all files are guaranteed to be aligned before the pre-window.
  # Add 5 minutes to skip any initial recording artefacts.
  common_start     <- max(min(dat_plug$datetime),
                          min(dat_middle$datetime),
                          min(dat_food$datetime))
  pre_window_start <- common_start + 300   # +5 minutes
  
  cat(sprintf("  Common file start: %s\n",
              format(common_start,     "%Y-%m-%d %H:%M")))
  cat(sprintf("  Pre-window start : %s (+5 min offset)\n",
              format(pre_window_start, "%Y-%m-%d %H:%M")))
  
  # ── Time filtering ────────────────────────────────────────
  # Individual monitors are filtered from pre_window_start so the
  # state machine has the maximum possible beam data to seed the
  # initial position before the analysis window begins.
  # Pre-window rows are trimmed from dat after zone assignment (Step 4).
  analysis_start <- NULL
  
  apply_time_filter <- function(dt_in) {
    dt_in <- dt_in[datetime >= pre_window_start]
    if (!is.null(start_datetime)) {
      analysis_start <<- as.POSIXct(start_datetime, tz = "America/Chicago")
      cat(sprintf("  Analysis start   : %s\n",
                  format(analysis_start, "%Y-%m-%d %H:%M")))
    }
    if (!is.null(end_datetime)) {
      end_dt <- as.POSIXct(end_datetime, tz = "America/Chicago")
      dt_in  <- dt_in[datetime <= end_dt]
      cat(sprintf("  End              : %s\n",
                  format(end_dt, "%Y-%m-%d %H:%M")))
    }
    return(dt_in)
  }
  
  dat_plug   <- apply_time_filter(dat_plug)
  dat_middle <- apply_time_filter(dat_middle)
  dat_food   <- apply_time_filter(dat_food)
  
  # ── Build summed activity table ───────────────────────────
  # Sum plug + middle + food per channel — used for dead detection only.
  # NOT trimmed to analysis window — dead detection must scan the full
  # recording from pre_window_start so flies that died before or during
  # the pre-window are correctly identified.
  ch_cols <- paste0("ch", channels)
  dat_sum <- copy(dat_plug)
  
  for (col in ch_cols) {
    dat_sum[, (col) := dat_plug[[col]] + dat_middle[[col]] + dat_food[[col]]]
  }
  
  # ── Summary ───────────────────────────────────────────────
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
#
# Runs RLE on summed (plug + middle + food) activity per channel.
# A channel is flagged DEAD if its longest continuous zero-
# activity run >= death_threshold_hours.
#
# Returns a data.table (one row per channel):
#   channel | status | max_inactivity_hours | death_datetime
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
#
# Drops dead channel columns from $plug, $middle, and $food.
# $summed is not modified — it has served its purpose.
#
# Returns the monitors list with dead columns removed.
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


# ============================================================
# STEP 4: ASSIGN POSITION ZONES ONLY
#
# - Reshapes monitor data to long format
# - Runs position state machine on full window (incl. pre-window)
# - Trims pre-window rows
# - Classifies UNKNOWN rows and prints diagnostics
# - Does NOT run sleep detection (moved to Step 4.5 so it runs
#   on already-backfilled zones)
#
# Input : monitors list (after remove_dead_channels)
# Output: list with
#   $dat          — long-format data.table (analysis window):
#                   datetime, channel, B_plug, B_middle, B_food,
#                   zone, unknown_type
#   $unknown_diag — per-channel UNKNOWN diagnostic data.table
# ============================================================

assign_position_zones <- function(monitors) {
  
  cat("=== Build Long Format + Position State Machine ===\n")
  
  ch_cols        <- grep("^ch\\d+$", names(monitors$plug), value = TRUE)
  channels       <- sort(as.integer(sub("ch", "", ch_cols)))
  analysis_start <- monitors$analysis_start
  
  cat(sprintf("  Channels         : %d\n", length(channels)))
  cat(sprintf("  Timepoints (full): %d (incl. pre-window from file start)\n",
              nrow(monitors$plug)))
  
  # ── Reshape to long format (full window incl. pre-window) ─
  long_list <- vector("list", length(channels))
  
  for (i in seq_along(channels)) {
    ch  <- channels[i]
    col <- paste0("ch", ch)
    long_list[[i]] <- data.table(
      datetime = monitors$plug$datetime,
      channel  = ch,
      B_plug   = as.numeric(monitors$plug[[col]]),
      B_middle = as.numeric(monitors$middle[[col]]),
      B_food   = as.numeric(monitors$food[[col]])
    )
  }
  
  dat_full <- rbindlist(long_list)
  setorder(dat_full, channel, datetime)
  
  # ── Position state machine (runs on full window) ──────────
  process_one_channel <- function(bp_vec, bm_vec, bf_vec) {
    
    n       <- length(bp_vec)
    zone    <- rep("UNKNOWN", n)
    current <- "UNKNOWN"
    
    for (t in 1:n) {
      bp <- bp_vec[t]; bm <- bm_vec[t]; bf <- bf_vec[t]
      
      if (bp == 0 && bm == 0 && bf == 0) { zone[t] <- current; next }
      if (bp > 0 && bm == 0 && bf == 0)  { current <- "Z1"; zone[t] <- current; next }
      if (bf > 0 && bm == 0 && bp == 0)  { current <- "Z4"; zone[t] <- current; next }
      if (bp > 0 && bm > 0  && bf == 0)  { current <- "Z2"; zone[t] <- current; next }
      if (bm > 0 && bf > 0  && bp == 0)  { current <- "Z3"; zone[t] <- current; next }
      
      if (bm > 0 && bp == 0 && bf == 0) {
        if (t < n) {
          bp_next <- bp_vec[t+1]; bm_next <- bm_vec[t+1]; bf_next <- bf_vec[t+1]
          if (bp_next > 0 || (bm_next > 0 && current %in% c("Z1","Z2"))) {
            current <- "Z2"
          } else if (bf_next > 0 || (bm_next > 0 && current %in% c("Z3","Z4"))) {
            current <- "Z3"
          } else {
            current <- if (current %in% c("Z1","Z2")) "Z2" else "Z3"
          }
        } else {
          current <- if (current %in% c("Z1","Z2")) "Z2" else "Z3"
        }
        zone[t] <- current; next
      }
      
      zone[t] <- current   # ambiguous (all-three or plug+food): carry forward
    }
    
    return(zone)
  }
  
  cat("  Running state machine per channel...\n")
  dat_full[, zone := process_one_channel(B_plug, B_middle, B_food), by = channel]
  
  # ── Trim pre-window ───────────────────────────────────────
  if (!is.null(analysis_start)) {
    dat <- dat_full[datetime >= analysis_start]
    cat(sprintf("  Pre-window trimmed. Analysis rows: %d per channel\n",
                nrow(dat) / length(channels)))
  } else {
    dat <- copy(dat_full)
  }
  
  # ── UNKNOWN diagnostics ───────────────────────────────────
  cat("  Classifying UNKNOWN rows...\n")
  
  classify_unknown <- function(zone_vec, bp_vec, bm_vec, bf_vec) {
    n            <- length(zone_vec)
    unknown_type <- rep(NA_character_, n)
    first_known  <- which(zone_vec != "UNKNOWN")[1]
    
    for (t in which(zone_vec == "UNKNOWN")) {
      if (is.na(first_known) || t < first_known) {
        unknown_type[t] <- "startup"
      } else {
        bp <- bp_vec[t]; bm <- bm_vec[t]; bf <- bf_vec[t]
        if ((bp > 0 && bm > 0 && bf > 0) ||
            (bp > 0 && bf > 0 && bm == 0)) {
          unknown_type[t] <- "ambiguous_beam_pattern"
        } else {
          unknown_type[t] <- "ambiguous_carry_forward"
        }
      }
    }
    return(unknown_type)
  }
  
  dat[, unknown_type := classify_unknown(zone, B_plug, B_middle, B_food),
      by = channel]
  
  # Per-channel diagnostic summary
  n_timepoints <- nrow(dat) / length(channels)
  unknown_diag <- dat[zone == "UNKNOWN", .N, by = .(channel, unknown_type)]
  unknown_diag[, total_rows     := n_timepoints]
  unknown_diag[, pct_of_channel := round(100 * N / n_timepoints, 2)]
  setorder(unknown_diag, channel, unknown_type)
  
  cat("\n  UNKNOWN zone summary (before backfill):\n")
  cat(sprintf("  %-8s %-30s %8s %10s\n",
              "Channel", "Type", "Count", "% of ch"))
  cat(sprintf("  %s\n", strrep("-", 60)))
  for (i in seq_len(nrow(unknown_diag))) {
    cat(sprintf("  Ch %2d   %-30s %8d %9.1f%%\n",
                unknown_diag$channel[i],
                unknown_diag$unknown_type[i],
                unknown_diag$N[i],
                unknown_diag$pct_of_channel[i]))
  }
  n_unk <- sum(dat$zone == "UNKNOWN")
  cat(sprintf("\n  Total UNKNOWN: %d / %d rows (%.1f%%)\n",
              n_unk, nrow(dat), 100 * n_unk / nrow(dat)))
  cat("=== Done ===\n\n")
  
  return(list(dat = dat, unknown_diag = unknown_diag))
}


# ============================================================
# STEP 4.5: BACKFILL STARTUP UNKNOWN + DETECT SLEEP BOUTS
#
# Two operations that must run together in this order:
#
# A) Backfill startup UNKNOWN zones:
#    For each channel with startup UNKNOWN rows, scan forward
#    from analysis_start until the first non-UNKNOWN zone is
#    found. Backfill all preceding startup UNKNOWN rows with
#    that zone — however far forward the scan needs to go.
#
# B) Sleep bout detection (now runs on corrected zones):
#    Detects inactivity bouts >= sleep_threshold_min and assigns
#    sleep_zone from the (now backfilled) zone at bout onset.
#    This ensures sleep bouts during the startup period are
#    assigned the correct zone rather than UNKNOWN.
#
# Input : result list from assign_position_zones()
# Output: same list with:
#   $dat updated — zone backfilled, sleep_zone and bout_start added
#   $unknown_diag rebuilt — reflects only remaining UNKNOWN after backfill
# ============================================================

backfill_and_detect_sleep <- function(result, sleep_threshold_min = 5) {
  
  dat          <- result$dat
  unknown_diag <- result$unknown_diag
  channels     <- sort(unique(dat$channel))
  
  # ── A) Backfill startup UNKNOWN ───────────────────────────
  startup_channels <- unknown_diag[unknown_type == "startup", channel]
  
  if (length(startup_channels) == 0) {
    cat("=== Backfill Startup UNKNOWN: nothing to backfill ===\n")
  } else {
    cat("=== Backfill Startup UNKNOWN ===\n")
    cat(sprintf("  Channels with startup UNKNOWN: %s\n",
                paste(sort(startup_channels), collapse = ", ")))
    
    n_filled_total <- 0
    
    for (ch in startup_channels) {
      ch_dat <- dat[channel == ch]
      setorder(ch_dat, datetime)
      
      # Find first non-UNKNOWN zone in the entire channel
      first_known_idx <- which(ch_dat$zone != "UNKNOWN")[1]
      
      if (is.na(first_known_idx)) {
        cat(sprintf("  Ch %2d: no known zone found — skipping (check dead detection)\n", ch))
        next
      }
      
      fill_zone <- ch_dat$zone[first_known_idx]
      fill_time <- ch_dat$datetime[first_known_idx]
      
      n_startup <- sum(ch_dat$zone[seq_len(first_known_idx - 1)] == "UNKNOWN")
      
      # Backfill zone and clear unknown_type for these rows
      dat[channel == ch & datetime < fill_time & zone == "UNKNOWN",
          `:=`(zone = fill_zone, unknown_type = NA_character_)]
      
      n_filled_total <- n_filled_total + n_startup
      
      cat(sprintf("  Ch %2d: filled %d rows with %s (first beam break: %s)\n",
                  ch, n_startup, fill_zone,
                  format(fill_time, "%Y-%m-%d %H:%M")))
    }
    
    cat(sprintf("\n  Total rows backfilled: %d\n", n_filled_total))
  }
  
  # Rebuild unknown_diag from updated zones
  n_timepoints     <- nrow(dat) / length(channels)
  unknown_diag_new <- dat[zone == "UNKNOWN", .N, by = .(channel, unknown_type)]
  
  if (nrow(unknown_diag_new) == 0) {
    cat("  All UNKNOWN rows resolved.\n")
    unknown_diag_new <- data.table(
      channel        = integer(),
      unknown_type   = character(),
      N              = integer(),
      total_rows     = integer(),
      pct_of_channel = numeric()
    )
  } else {
    unknown_diag_new[, total_rows     := n_timepoints]
    unknown_diag_new[, pct_of_channel := round(100 * N / n_timepoints, 2)]
    setorder(unknown_diag_new, channel, unknown_type)
    cat(sprintf("  Remaining UNKNOWN rows: %d\n", sum(unknown_diag_new$N)))
  }
  
  cat("=== Done ===\n\n")
  
  # ── B) Sleep bout detection (on backfilled zones) ─────────
  cat("=== Sleep Bout Detection ===\n")
  cat(sprintf("  Sleep threshold : %d minutes\n", sleep_threshold_min))
  
  assign_sleep_zone <- function(bp_vec, bm_vec, bf_vec, zone_vec) {
    
    n          <- length(bp_vec)
    is_active  <- (bp_vec + bm_vec + bf_vec) > 0
    sleep_zone <- rep(NA_character_, n)
    bout_start <- rep(FALSE, n)
    
    t <- 1
    while (t <= n) {
      if (!is_active[t]) {
        bout_end <- t
        while (bout_end < n && !is_active[bout_end + 1]) bout_end <- bout_end + 1
        bout_len <- bout_end - t + 1
        
        if (bout_len >= sleep_threshold_min) {
          sz <- if (t > 1) zone_vec[t - 1] else zone_vec[t]
          sleep_zone[t] <- sz
          bout_start[t] <- TRUE
        }
        t <- bout_end + 1
      } else {
        t <- t + 1
      }
    }
    
    return(list(sleep_zone = sleep_zone, bout_start = bout_start))
  }
  
  dat[, c("sleep_zone", "bout_start") := {
    res <- assign_sleep_zone(B_plug, B_middle, B_food, zone)
    list(res$sleep_zone, res$bout_start)
  }, by = channel]
  
  sleep_bouts <- dat[bout_start == TRUE]
  cat(sprintf("  Total sleep bouts detected : %d\n", nrow(sleep_bouts)))
  cat("  Sleep zone distribution (after backfill):\n")
  print(table(sleep_bouts$sleep_zone, useNA = "ifany"))
  cat("=== Done ===\n\n")
  
  return(list(dat = dat, unknown_diag = unknown_diag_new))
}


# ============================================================
# STEP 5: VISUALIZATIONS
# ============================================================

# 5A: Individual fly movement path timelines (faceted) ------------
#
# For each fly:
#   Active periods  — geom_step line colored by origin zone,
#                     line width scaled by total beam count
#   Inactive periods — grey flat line + a single dot at run start,
#                     dot size scales continuously with duration (minutes)
#                     dot fill = grey, border = zone color
#   Light/dark      — background shading (dark phase = grey)
#   X-axis          — elapsed hours from start_datetime (t = 0)

plot_individual_timelines <- function(
    dat,
    channels_to_plot  = NULL,
    ncol_facet        = 4,
    lights_on         = "05:00:00",   # ZT lights-on time HH:MM:SS
    lights_off        = "17:00:00",   # ZT lights-off time HH:MM:SS
    dot_size_range    = c(1, 8),      # min/max dot size for inactive runs
    line_width_range  = c(0.3, 1.5), # min/max line width for active steps
    output_file       = "position_timelines.pdf",
    width             = 20,
    height            = NULL
) {
  
  if (is.null(channels_to_plot)) channels_to_plot <- sort(unique(dat$channel))
  
  plot_dat <- dat[channel %in% channels_to_plot]
  setorder(plot_dat, channel, datetime)
  
  tz <- lubridate::tz(plot_dat$datetime[1])
  t0 <- min(plot_dat$datetime)   # start_datetime = t=0
  
  # ── Elapsed hours ──────────────────────────────────────────
  plot_dat[, elapsed_hr := as.numeric(difftime(datetime, t0, units = "hours"))]
  
  # ── Zone factor (ordered plug → food) ─────────────────────
  zone_order <- c("Z1","Z2","Z3","Z4")
  zone_ylabs <- c("Z1 (plug)","Z2 (plug-mid)","Z3 (mid-food)","Z4 (food)")
  plot_dat[, zone_f := factor(zone_anchored,
                              levels = zone_order,
                              labels = zone_ylabs)]
  
  # ── Total beam activity per bin ────────────────────────────
  plot_dat[, activity := B_plug + B_middle + B_food]
  
  # ── Separate active and inactive rows ─────────────────────
  active_dat   <- plot_dat[activity > 0 & !is.na(zone_f)]
  inactive_dat <- plot_dat[activity == 0]
  
  # ── Build inactive run summary ────────────────────────────
  # For each contiguous inactive run per channel:
  #   - start elapsed_hr, end elapsed_hr, duration_min, zone_f at start
  inactive_runs <- inactive_dat[, {
    # RLE to find contiguous runs
    is_gap  <- c(0, diff(elapsed_hr)) > (1/60 * 2)  # >2 min gap = new run
    run_id  <- cumsum(is_gap) + 1L
    .SD[, .(
      start_hr     = elapsed_hr[1],
      end_hr       = elapsed_hr[.N],
      duration_min = .N,
      zone_f       = zone_f[1]    # zone at start of inactive run
    ), by = run_id]
  }, by = channel]
  
  # Dot size: sqrt scale so very long bouts don't overwhelm
  max_dur <- max(inactive_runs$duration_min, na.rm = TRUE)
  min_dur <- min(inactive_runs$duration_min, na.rm = TRUE)
  inactive_runs[, dot_size := scales::rescale(
    sqrt(duration_min),
    to   = dot_size_range,
    from = sqrt(c(min_dur, max_dur))
  )]
  
  # ── Light/dark background rectangles ─────────────────────
  hr_max          <- max(plot_dat$elapsed_hr, na.rm = TRUE)
  lights_on_secs  <- as.numeric(lubridate::hms(lights_on))
  lights_off_secs <- as.numeric(lubridate::hms(lights_off))
  
  day_seq <- seq(as.Date(t0, tz = tz),
                 as.Date(t0 + (hr_max + 24) * 3600, tz = tz),
                 by = "day")
  
  dark_rects <- rbindlist(Filter(Negate(is.null), lapply(day_seq, function(d) {
    day_origin <- as.POSIXct(as.character(d), tz = tz)
    # evening dark: lights_off → midnight
    d1s <- as.numeric(difftime(day_origin + lights_off_secs, t0, units = "hours"))
    d1e <- as.numeric(difftime(day_origin + 86400,           t0, units = "hours"))
    # morning dark: midnight → lights_on
    d2s <- as.numeric(difftime(day_origin,                   t0, units = "hours"))
    d2e <- as.numeric(difftime(day_origin + lights_on_secs,  t0, units = "hours"))
    rows <- list()
    if (d1e > 0 && d1s < hr_max)
      rows[[1]] <- data.table(xmin = max(d1s, 0), xmax = min(d1e, hr_max))
    if (d2e > 0 && d2s < hr_max)
      rows[[2]] <- data.table(xmin = max(d2s, 0), xmax = min(d2e, hr_max))
    if (length(rows) == 0) return(NULL)
    unique(rbindlist(rows))
  })))
  
  # ── Auto height ───────────────────────────────────────────
  n_rows <- ceiling(length(channels_to_plot) / ncol_facet)
  if (is.null(height)) height <- n_rows * 3 + 2
  
  # ── Plot ──────────────────────────────────────────────────
  p <- ggplot() +
    
    # Dark phase background
    geom_rect(data = dark_rects,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "grey80", alpha = 0.35, inherit.aes = FALSE) +
    
    # Inactive flat lines (grey)
    geom_segment(data = inactive_runs[!is.na(zone_f)],
                 aes(x = start_hr, xend = end_hr,
                     y = zone_f,   yend = zone_f),
                 color     = "grey70",
                 linewidth = 0.4,
                 lineend   = "round") +
    
    # Inactive dots — size = duration, border = zone color
    geom_point(data = inactive_runs[!is.na(zone_f)],
               aes(x    = start_hr,
                   y    = zone_f,
                   size = dot_size,
                   color = zone_f),
               shape = 21,
               fill  = "grey85",
               stroke = 0.8,
               alpha  = 0.85) +
    
    # Active step lines — colored by origin zone, width by activity
    geom_step(data = active_dat,
              aes(x         = elapsed_hr,
                  y         = zone_f,
                  color     = zone_f,
                  linewidth = activity,
                  group     = channel),
              alpha    = 0.75,
              linejoin = "round") +
    
    # Facet per channel
    facet_wrap(~ paste0("Ch ", channel),
               ncol   = ncol_facet,
               scales = "free_x") +
    
    # Zone colors
    scale_color_manual(
      values = c(
        "Z1 (plug)"     = zone_colors["Z1"],
        "Z2 (plug-mid)" = zone_colors["Z2"],
        "Z3 (mid-food)" = zone_colors["Z3"],
        "Z4 (food)"     = zone_colors["Z4"]
      ),
      name = "Zone"
    ) +
    
    # Line width scale
    scale_linewidth_continuous(
      range  = line_width_range,
      name   = "Beam count\n(activity)"
    ) +
    
    # Dot size scale — show actual minutes in legend
    scale_size_identity(guide = "none") +
    
    scale_x_continuous(
      breaks = seq(0, ceiling(hr_max), by = 12),
      labels = function(x) paste0(x, "h"),
      expand = expansion(mult = 0.01)
    ) +
    
    scale_y_discrete(drop = FALSE) +
    
    labs(
      title    = "Fly Movement Path - Individual Channels",
      subtitle = paste0("Colored line = active movement (width ~ beam count)  |  ",
                        "Grey line + dot = inactive  |  Dot size ~ duration (min)  |  ",
                        "Dark grey = dark phase  |  t=0 = start_datetime"),
      x = "Elapsed time (hours)",
      y = "Position Zone"
    ) +
    
    theme_bw(base_size = 9) +
    theme(
      strip.background   = element_rect(fill = "grey90"),
      strip.text         = element_text(face = "bold", size = 8),
      axis.text.x        = element_text(size = 7),
      axis.text.y        = element_text(size = 7),
      legend.position    = "bottom",
      legend.box         = "horizontal",
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "grey93"),
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(size = 7, color = "grey40")
    ) +
    
    guides(
      color     = guide_legend(override.aes = list(linewidth = 1), nrow = 1),
      linewidth = guide_legend(nrow = 1)
    )
  
  ggsave(output_file, p,
         width = width, height = height, units = "in", dpi = 150,
         limitsize = FALSE)
  cat(sprintf("Saved: %s (%.0f x %.0f in)\n", output_file, width, height))
  return(p)
}


# 5B: Group-level sleep zone heatmap -------------------------------

plot_sleep_zone_heatmap <- function(
    dat,
    channels_to_plot = NULL,
    group_label      = "All flies",
    output_file      = "sleep_zone_heatmap.pdf",
    width            = 14,
    height           = 6
) {
  
  if (is.null(channels_to_plot)) channels_to_plot <- sort(unique(dat$channel))
  
  sleep_dat   <- dat[bout_start_anchored == TRUE & channel %in% channels_to_plot]
  bout_counts <- sleep_dat[, .N, by = .(channel, sleep_zone_anchored)]
  setnames(bout_counts, "sleep_zone_anchored", "sleep_zone")
  
  bout_counts[, total      := sum(N),     by = channel]
  bout_counts[, proportion := N / total]
  
  all_combos <- CJ(channel    = channels_to_plot,
                   sleep_zone = c("Z1","Z2","Z3","Z4"))
  bout_counts <- merge(all_combos, bout_counts,
                       by = c("channel","sleep_zone"), all.x = TRUE)
  bout_counts[is.na(proportion), proportion := 0]
  bout_counts[is.na(N),          N          := 0]
  
  bout_counts[, zone_f := factor(sleep_zone,
                                 levels = c("Z4","Z3","Z2","Z1"),
                                 labels = c("Z4 (food)","Z3 (mid-food)",
                                            "Z2 (plug-mid)","Z1 (plug)"))]
  
  p <- ggplot(bout_counts,
              aes(x = factor(channel), y = zone_f, fill = proportion)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(N > 0, N, "")),
              size = 2.5, color = "white", fontface = "bold") +
    scale_fill_gradient2(low = "white", mid = "#92C5DE", high = "#2166AC",
                         midpoint = 0.5, limits = c(0, 1),
                         labels = percent_format(),
                         name = "Proportion\nof sleep bouts") +
    labs(title    = sprintf("Sleep Zone Distribution - %s", group_label),
         subtitle = "Numbers = bout count  |  Color = proportion per fly",
         x = "Channel (fly)", y = "Sleep Zone") +
    theme_bw(base_size = 11) +
    theme(axis.text.x     = element_text(size = 8),
          axis.text.y     = element_text(size = 9),
          legend.position = "right",
          panel.grid      = element_blank(),
          plot.title      = element_text(face = "bold"))
  
  ggsave(output_file, p, width = width, height = height, units = "in", dpi = 150)
  cat(sprintf("Saved: %s (%.0f x %.0f in)\n", output_file, width, height))
  return(p)
}


# 5C: Group summary bar plot (mean +/- SEM) ------------------------

plot_sleep_zone_summary <- function(
    dat,
    channels_to_plot = NULL,
    group_label      = "All flies",
    output_file      = "sleep_zone_summary.pdf",
    width            = 7,
    height           = 5
) {
  
  if (is.null(channels_to_plot)) channels_to_plot <- sort(unique(dat$channel))
  
  sleep_dat <- dat[bout_start_anchored == TRUE & channel %in% channels_to_plot]
  
  per_fly <- sleep_dat[, .N, by = .(channel, sleep_zone_anchored)]
  setnames(per_fly, "sleep_zone_anchored", "sleep_zone")
  per_fly[, total      := sum(N),     by = channel]
  per_fly[, proportion := N / total]
  
  all_combos <- CJ(channel    = channels_to_plot,
                   sleep_zone = c("Z1","Z2","Z3","Z4"))
  per_fly <- merge(all_combos, per_fly,
                   by = c("channel","sleep_zone"), all.x = TRUE)
  per_fly[is.na(proportion), proportion := 0]
  
  summary_dt <- per_fly[, .(
    mean_prop = mean(proportion),
    sem_prop  = sd(proportion) / sqrt(.N),
    n_flies   = .N
  ), by = sleep_zone]
  
  summary_dt[, zone_f := factor(sleep_zone,
                                levels = c("Z1","Z2","Z3","Z4"),
                                labels = c("Z1\n(plug)","Z2\n(plug-mid)",
                                           "Z3\n(mid-food)","Z4\n(food)"))]
  
  p <- ggplot(summary_dt, aes(x = zone_f, y = mean_prop, fill = sleep_zone)) +
    geom_col(width = 0.6, color = "black", linewidth = 0.3) +
    geom_errorbar(aes(ymin = mean_prop - sem_prop,
                      ymax = mean_prop + sem_prop),
                  width = 0.2, linewidth = 0.5) +
    scale_fill_manual(values = zone_colors, guide = "none") +
    scale_y_continuous(labels  = percent_format(),
                       expand  = expansion(mult = c(0, 0.05))) +
    labs(title    = sprintf("Sleep Zone Preference - %s", group_label),
         subtitle = sprintf("n = %d flies  |  Mean +/- SEM",
                            length(channels_to_plot)),
         x = "Zone", y = "Proportion of sleep bouts") +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title         = element_text(face = "bold"))
  
  ggsave(output_file, p, width = width, height = height, units = "in", dpi = 150)
  cat(sprintf("Saved: %s (%.0f x %.0f in)\n", output_file, width, height))
  return(p)
}


# 5D: Group summary heatmap (mean proportion per zone) -------------

plot_sleep_zone_summary_heatmap <- function(
    dat,
    channels_to_plot = NULL,
    group_label      = "All flies",
    output_file      = "sleep_zone_summary_heatmap.pdf",
    width            = 4,
    height           = 6
) {
  
  if (is.null(channels_to_plot)) channels_to_plot <- sort(unique(dat$channel))
  
  sleep_dat <- dat[bout_start_anchored == TRUE & channel %in% channels_to_plot]
  
  per_fly <- sleep_dat[, .N, by = .(channel, sleep_zone_anchored)]
  setnames(per_fly, "sleep_zone_anchored", "sleep_zone")
  per_fly[, total      := sum(N), by = channel]
  per_fly[, proportion := N / total]
  
  all_combos <- CJ(channel    = channels_to_plot,
                   sleep_zone = c("Z1","Z2","Z3","Z4"))
  per_fly <- merge(all_combos, per_fly,
                   by = c("channel","sleep_zone"), all.x = TRUE)
  per_fly[is.na(proportion), proportion := 0]
  
  summary_dt <- per_fly[, .(
    mean_prop = mean(proportion),
    sem_prop  = sd(proportion) / sqrt(.N),
    n_flies   = .N
  ), by = sleep_zone]
  
  summary_dt[, zone_f := factor(sleep_zone,
                                levels = c("Z4","Z3","Z2","Z1"),
                                labels = c("Z4 (food)","Z3 (mid-food)",
                                           "Z2 (plug-mid)","Z1 (plug)"))]
  
  summary_dt[, label := sprintf("%.1f%%\n+/-%.1f%%",
                                mean_prop * 100, sem_prop * 100)]
  
  p <- ggplot(summary_dt,
              aes(x = group_label, y = zone_f, fill = mean_prop)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = label), size = 3, color = "white", fontface = "bold") +
    scale_fill_gradient2(low = "white", mid = "#92C5DE", high = "#2166AC",
                         midpoint = 0.5, limits = c(0, 1),
                         labels = percent_format(),
                         name = "Mean proportion\nof sleep bouts") +
    labs(title    = sprintf("Sleep Zone Summary - %s", group_label),
         subtitle = sprintf("n = %d flies  |  Mean +/- SEM",
                            length(channels_to_plot)),
         x = NULL, y = "Sleep Zone") +
    theme_bw(base_size = 11) +
    theme(axis.text.x     = element_text(size = 9),
          axis.text.y     = element_text(size = 9),
          legend.position = "right",
          panel.grid      = element_blank(),
          plot.title      = element_text(face = "bold"))
  
  ggsave(output_file, p, width = width, height = height, units = "in", dpi = 150)
  cat(sprintf("Saved: %s (%.0f x %.0f in)\n", output_file, width, height))
  return(p)
}


# ============================================================
# STEP 6: EXCEL EXPORT
#
# Sheets:
#   Per_fly              — bout counts + proportions per channel x zone
#   Group_summary        — mean +/- SEM per zone
#   Dead_channels        — dead_summary table
#   UNKNOWN_diag         — startup/ambiguous UNKNOWN counts per channel
#   Transit_summary      — per-channel counts of each transit type
#   Anchor_summary       — per-channel SLEEP/STOP anchor counts + confidence
#   Transit_inconsistent — row-level detail of TRANSIT_INCONSISTENT bins
#   Anchor_ambiguous     — row-level detail of ambiguous anchor zones
# ============================================================

export_results_excel <- function(
    dat,
    dead_summary,
    unknown_diag,
    anchors,           # data.table returned by assign_position_anchored()
    group_label  = "All flies",
    output_file  = "sleep_zone_results.xlsx"
) {
  
  cat(sprintf("=== Exporting Excel: %s ===\n", basename(output_file)))
  
  channels_to_export <- sort(unique(dat$channel))
  sleep_dat <- dat[bout_start_anchored == TRUE & channel %in% channels_to_export]
  
  # ── Sheet 1: Per_fly ──────────────────────────────────────
  per_fly <- sleep_dat[, .N, by = .(channel, sleep_zone_anchored)]
  setnames(per_fly, "sleep_zone_anchored", "sleep_zone")
  per_fly[, total      := sum(N), by = channel]
  per_fly[, proportion := round(N / total, 4)]
  
  all_combos <- CJ(channel    = channels_to_export,
                   sleep_zone = c("Z1","Z2","Z3","Z4"))
  per_fly <- merge(all_combos, per_fly,
                   by = c("channel","sleep_zone"), all.x = TRUE)
  per_fly[is.na(N),           N           := 0]
  per_fly[is.na(proportion),  proportion  := 0]
  per_fly[is.na(total),       total       := 0]
  
  per_fly_wide   <- dcast(per_fly, channel ~ sleep_zone,
                          value.var = "proportion", fill = 0)
  per_fly_wide_n <- dcast(per_fly, channel ~ paste0(sleep_zone, "_n"),
                          value.var = "N", fill = 0)
  per_fly_export <- merge(per_fly_wide, per_fly_wide_n, by = "channel")
  setorder(per_fly_export, channel)
  
  # ── Sheet 2: Group_summary ────────────────────────────────
  all_combos2 <- CJ(channel    = channels_to_export,
                    sleep_zone = c("Z1","Z2","Z3","Z4"))
  per_fly2 <- sleep_dat[, .N, by = .(channel, sleep_zone_anchored)]
  setnames(per_fly2, "sleep_zone_anchored", "sleep_zone")
  per_fly2 <- merge(all_combos2, per_fly2,
                    by = c("channel","sleep_zone"), all.x = TRUE)
  per_fly2[is.na(N), N := 0]
  per_fly2[, total      := sum(N), by = channel]
  per_fly2[, proportion := ifelse(total == 0, 0, N / total)]
  
  group_summary <- per_fly2[, .(
    n_flies   = .N,
    mean_prop = round(mean(proportion), 4),
    sem_prop  = round(sd(proportion) / sqrt(.N), 4),
    mean_pct  = round(mean(proportion) * 100, 2),
    sem_pct   = round(sd(proportion) / sqrt(.N) * 100, 2)
  ), by = sleep_zone]
  
  group_summary[, sleep_zone := factor(sleep_zone,
                                       levels = c("Z1","Z2","Z3","Z4"))]
  setorder(group_summary, sleep_zone)
  group_summary[, group := group_label]
  setcolorder(group_summary,
              c("group","sleep_zone","n_flies","mean_prop","sem_prop",
                "mean_pct","sem_pct"))
  
  # ── Sheet 3: Dead_channels ────────────────────────────────
  dead_export <- copy(dead_summary)
  dead_export[, death_datetime := as.character(death_datetime)]
  
  # ── Sheet 4: UNKNOWN_diag ─────────────────────────────────
  unknown_export <- copy(unknown_diag)
  
  # ── Sheet 5: Transit_summary ──────────────────────────────
  # Per-channel count of each transit type + % of channel rows
  n_rows_per_ch <- nrow(dat) / length(channels_to_export)
  
  transit_summary <- dat[!is.na(transit_type),
                         .N, by = .(channel, transit_type)]
  transit_summary[, total_rows   := n_rows_per_ch]
  transit_summary[, pct_of_ch   := round(100 * N / n_rows_per_ch, 2)]
  
  # Pivot wider so each transit type is a column
  transit_wide <- dcast(transit_summary,
                        channel ~ transit_type,
                        value.var = "N", fill = 0)
  setorder(transit_wide, channel)
  
  # Also add % columns
  transit_wide_pct <- dcast(transit_summary,
                            channel ~ paste0(transit_type, "_pct"),
                            value.var = "pct_of_ch", fill = 0)
  transit_export <- merge(transit_wide, transit_wide_pct, by = "channel")
  setorder(transit_export, channel)
  
  # ── Sheet 6: Anchor_summary ───────────────────────────────
  # Per-channel counts of SLEEP and STOP anchors by confidence level
  anchor_summary <- anchors[, .(
    n_anchors    = .N,
    n_sleep      = sum(anchor_type == "SLEEP"),
    n_stop       = sum(anchor_type == "STOP"),
    n_confirmed  = sum(confidence == "confirmed"),
    n_inf_before = sum(confidence == "inferred_before"),
    n_inf_after  = sum(confidence == "inferred_after"),
    n_before_only = sum(confidence == "before_only"),
    n_after_only  = sum(confidence == "after_only"),
    n_ambiguous  = sum(confidence == "ambiguous"),
    mean_dur_sleep = round(mean(duration_min[anchor_type == "SLEEP"],
                                na.rm = TRUE), 1),
    mean_dur_stop  = round(mean(duration_min[anchor_type == "STOP"],
                                na.rm = TRUE), 1)
  ), by = channel]
  setorder(anchor_summary, channel)
  
  # ── Sheet 7: Transit_inconsistent (row-level detail) ──────
  transit_incon <- dat[transit_type == "TRANSIT_INCONSISTENT",
                       .(datetime, channel,
                         B_plug, B_middle, B_food,
                         zone_anchored, transit_type,
                         anchor_type, anchor_zone)]
  transit_incon[, datetime := as.character(datetime)]
  setorder(transit_incon, channel, datetime)
  
  if (nrow(transit_incon) == 0) {
    transit_incon <- data.table(
      note = "No TRANSIT_INCONSISTENT bins detected — all transits resolved cleanly."
    )
  }
  
  # ── Sheet 8: Anchor_ambiguous (row-level detail) ──────────
  anchor_ambig <- anchors[confidence == "ambiguous",
                          .(channel, start_idx, end_idx,
                            duration_min, anchor_type,
                            zone, confidence)]
  setorder(anchor_ambig, channel, start_idx)
  
  if (nrow(anchor_ambig) == 0) {
    anchor_ambig <- data.table(
      note = "No ambiguous anchors detected — all anchor zones resolved."
    )
  }
  
  # ── Write workbook ────────────────────────────────────────
  wb <- createWorkbook()
  
  addWorksheet(wb, "Per_fly")
  writeData(wb, "Per_fly", per_fly_export)
  
  addWorksheet(wb, "Group_summary")
  writeData(wb, "Group_summary", group_summary)
  
  addWorksheet(wb, "Dead_channels")
  writeData(wb, "Dead_channels", dead_export)
  
  addWorksheet(wb, "UNKNOWN_diag")
  writeData(wb, "UNKNOWN_diag", unknown_export)
  
  addWorksheet(wb, "Transit_summary")
  writeData(wb, "Transit_summary", transit_export)
  
  addWorksheet(wb, "Anchor_summary")
  writeData(wb, "Anchor_summary", anchor_summary)
  
  addWorksheet(wb, "Transit_inconsistent")
  writeData(wb, "Transit_inconsistent", transit_incon)
  
  addWorksheet(wb, "Anchor_ambiguous")
  writeData(wb, "Anchor_ambiguous", anchor_ambig)
  
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat(sprintf("  Sheets written:\n"))
  cat(sprintf("    Per_fly, Group_summary, Dead_channels, UNKNOWN_diag\n"))
  cat(sprintf("    Transit_summary, Anchor_summary\n"))
  cat(sprintf("    Transit_inconsistent (%d flagged bins)\n",
              if (is.null(transit_incon$note)) nrow(transit_incon) else 0))
  cat(sprintf("    Anchor_ambiguous (%d flagged anchors)\n",
              if (is.null(anchor_ambig$note)) nrow(anchor_ambig) else 0))
  cat(sprintf("  Saved: %s\n", output_file))
  cat("=== Done ===\n\n")
}


# ============================================================
# RUN
# ============================================================

output_dir  <- "/Users/cchuang/OneDrive - St. Jude Children's Research Hospital/DAM position/"
group_label <- "hTauV337M 50D"
group_slug  <- gsub(" ", "_", group_label)

# Step 1: Read all three monitors
# Pre-window = file start + 5 min → analysis start (seeds state machine)
monitors <- read_position_monitors(
  monitor_files = c(
    plug   = "/Users/cchuang/Desktop/Locomotion activity monitor data/Sleep/hTauV337M Mhc_CG8093/Retest/with isogenic control/50D/female/Monitor7.txt",
    middle = "/Users/cchuang/Desktop/Locomotion activity monitor data/Sleep/hTauV337M Mhc_CG8093/Retest/with isogenic control/50D/female/Monitor8.txt",
    food   = "/Users/cchuang/Desktop/Locomotion activity monitor data/Sleep/hTauV337M Mhc_CG8093/Retest/with isogenic control/50D/female/Monitor9.txt"
  ),
  channels       = 1:32,
  start_datetime = "2026-01-29 05:00:00",
  end_datetime   = "2026-01-30 04:59:00"
)

# Step 2: Detect dead channels from summed signal
dead_summary <- detect_dead_channels(monitors, death_threshold_hours = 12)

# Optional: review before removing
#   print(dead_summary)
#   print(dead_summary[status == "DEAD"])

# Step 3: Remove dead channel columns from individual monitors
monitors <- remove_dead_channels(monitors, dead_summary)

# Step 4: Assign position zones (state machine only, no sleep detection yet)
result       <- assign_position_zones(monitors)

# Step 4.5: Backfill startup UNKNOWN zones, then detect sleep bouts
#   Backfill must run before sleep detection so sleep_zone is assigned
#   from corrected zones rather than UNKNOWN
result       <- backfill_and_detect_sleep(result, sleep_threshold_min = 5)

dat          <- result$dat
unknown_diag <- result$unknown_diag

# Step 4A: Anchor-based position assignment
# source("assign_position_anchored.R")   # load if in separate file
anchor_result <- assign_position_anchored(
  dat,
  sleep_threshold_min = 5,
  look_window         = 5
)
dat     <- anchor_result$dat
anchors <- anchor_result$anchors

# Optional diagnostics before plotting:
#   print_anchor_timeline(dat, channel_num = 5)
#   dat[transit_type == "TRANSIT_INCONSISTENT"]
#   anchors[confidence == "ambiguous"]

# Step 5: Plot
p5a <- plot_individual_timelines(
  dat,
  channels_to_plot = sort(unique(dat$channel)),
  ncol_facet       = 4,
  lights_on        = "05:00:00",
  lights_off       = "17:00:00",
  output_file      = file.path(output_dir, paste0(group_slug, "_position_timelines.pdf"))
)

p5b <- plot_sleep_zone_heatmap(
  dat,
  group_label = group_label,
  output_file = file.path(output_dir, paste0(group_slug, "_sleep_zone_heatmap.pdf"))
)

p5c <- plot_sleep_zone_summary(
  dat,
  group_label = group_label,
  output_file = file.path(output_dir, paste0(group_slug, "_sleep_zone_summary.pdf"))
)

p5d <- plot_sleep_zone_summary_heatmap(
  dat,
  group_label = group_label,
  output_file = file.path(output_dir, paste0(group_slug, "_sleep_zone_summary_heatmap.pdf"))
)

# Step 6: Export Excel
export_results_excel(
  dat          = dat,
  dead_summary = dead_summary,
  unknown_diag = unknown_diag,
  anchors      = anchors,
  group_label  = group_label,
  output_file  = file.path(output_dir, paste0(group_slug, "_results.xlsx"))
)

