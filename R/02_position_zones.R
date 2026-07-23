# ============================================================
# R/02_position_zones.R  —  ZONE STATE MACHINE + SLEEP (defs only)
#
# Provides:
#   assign_position_zones()      — state-machine zone assignment
#   backfill_and_detect_sleep()  — backfill startup UNKNOWN + sleep bouts
#
# Produces the state-machine columns on dat:
#   zone, unknown_type, sleep_zone, bout_start
# (The anchor method in R/03_anchor.R adds the *_anchored columns.)
#
# Requires (loaded by the run script): data.table, lubridate
# ============================================================


# ============================================================
# STEP 4: ASSIGN POSITION ZONES ONLY
# ============================================================

assign_position_zones <- function(monitors) {

  cat("=== Build Long Format + Position State Machine ===\n")

  ch_cols        <- grep("^ch\\d+$", names(monitors$plug), value = TRUE)
  channels       <- sort(as.integer(sub("ch", "", ch_cols)))
  analysis_start <- monitors$analysis_start

  cat(sprintf("  Channels         : %d\n", length(channels)))
  cat(sprintf("  Timepoints (full): %d (incl. pre-window from file start)\n",
              nrow(monitors$plug)))

  # -- Reshape to long format (full window incl. pre-window) ----
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

  # -- Position state machine (runs on full window) -------------
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

  # -- Trim pre-window ------------------------------------------
  if (!is.null(analysis_start)) {
    dat <- dat_full[datetime >= analysis_start]
    cat(sprintf("  Pre-window trimmed. Analysis rows: %d per channel\n",
                nrow(dat) / length(channels)))
  } else {
    dat <- copy(dat_full)
  }

  # -- UNKNOWN diagnostics --------------------------------------
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
# ============================================================

backfill_and_detect_sleep <- function(result, sleep_threshold_min = 5) {

  dat          <- result$dat
  unknown_diag <- result$unknown_diag
  channels     <- sort(unique(dat$channel))

  # -- A) Backfill startup UNKNOWN ------------------------------
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

      first_known_idx <- which(ch_dat$zone != "UNKNOWN")[1]

      if (is.na(first_known_idx)) {
        cat(sprintf("  Ch %2d: no known zone found — skipping (check dead detection)\n", ch))
        next
      }

      fill_zone <- ch_dat$zone[first_known_idx]
      fill_time <- ch_dat$datetime[first_known_idx]

      n_startup <- sum(ch_dat$zone[seq_len(first_known_idx - 1)] == "UNKNOWN")

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

  # -- B) Sleep bout detection (on backfilled zones) ------------
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
