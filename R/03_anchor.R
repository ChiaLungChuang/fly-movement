# ============================================================
# R/03_anchor.R  —  ANCHOR-BASED POSITION ASSIGNMENT (defs only)
#
# Physically rigorous alternative to the state machine. Ties each
# sleep bout to the zone the fly was actually resting in, and
# classifies every active bin as a transit type so movement paths
# can be traced.
#
# Provides:
#   assign_position_anchored()   — main entry; adds *_anchored cols
#   print_anchor_timeline()      — per-channel diagnostic
#   (plus internal helpers: beam_to_zone, resolve_boundary, ...)
#
# Key output columns added to dat:
#   zone_anchored        — confirmed zone (anchors + RETURN/MULTIPASS)
#   sleep_zone_anchored  — sleep zone from SLEEP anchors only
#   bout_start_anchored  — TRUE at start of each SLEEP anchor
#   anchor_type          — "SLEEP", "STOP", NA
#   anchor_zone          — zone at anchor, NA for active bins
#   transit_type         — TRANSIT_* for active bins, NA for anchors
#
# Requires (loaded by the run script): data.table
# ============================================================


# ============================================================
# HELPER: beam_to_zone()
# ============================================================

beam_to_zone <- function(bp, bm, bf) {

  # All zero — cannot determine from this bin alone
  if (bp == 0 && bm == 0 && bf == 0) return(NA_character_)

  # -- Two-beam confirmed patterns ------------------------------
  if (bp > 0 && bm > 0 && bf == 0) return("Z2")   # plug-mid corridor
  if (bp == 0 && bm > 0 && bf > 0) return("Z3")   # mid-food corridor

  # -- Single-beam boundary patterns ----------------------------
  if (bp > 0 && bm == 0 && bf == 0) return("Z1_Z2_boundary")
  if (bp == 0 && bm > 0 && bf == 0) return("Z2_Z3_boundary")
  if (bp == 0 && bm == 0 && bf > 0) return("Z3_Z4_boundary")

  # -- Physically ambiguous multi-beam patterns -----------------
  if (bp > 0 && bm == 0 && bf > 0) return("INCONSISTENT")  # plug+food, no middle
  if (bp > 0 && bm > 0  && bf > 0) return("MULTIPASS")     # all three

  return(NA_character_)
}


# ============================================================
# HELPER: resolve_boundary()
# ============================================================

resolve_boundary <- function(
    boundary,      # e.g. "Z1_Z2_boundary"
    ch_dat,        # full channel data ordered by datetime
    ref_idx,       # index of the boundary bin
    direction      # "before" or "after"
) {

  n <- nrow(ch_dat)

  if (direction == "before") {
    if (ref_idx - 1 < 1) {
      if (boundary == "Z1_Z2_boundary") return("Z1")
      if (boundary == "Z3_Z4_boundary") return("Z4")
      return(NA_character_)
    }
    idx_seq <- seq(ref_idx - 1, 1L, by = -1)
  } else {
    if (ref_idx + 1 > n) {
      if (boundary == "Z1_Z2_boundary") return("Z1")
      if (boundary == "Z3_Z4_boundary") return("Z4")
      return(NA_character_)
    }
    idx_seq <- seq(ref_idx + 1, n, by = 1)
  }

  for (i in idx_seq) {
    bp <- ch_dat$B_plug[i]
    bm <- ch_dat$B_middle[i]
    bf <- ch_dat$B_food[i]
    pattern <- beam_to_zone(bp, bm, bf)

    if (is.na(pattern)) next   # all zero — fly inactive, keep searching

    if (boundary == "Z1_Z2_boundary") {
      if (pattern == "Z2")             return("Z2")
      if (pattern == "Z2_Z3_boundary") return("Z2")
      if (pattern == "Z3")             return("Z2")
      if (pattern == "Z3_Z4_boundary") return("Z2")
      if (pattern == "Z4")             return("Z2")   # far side — came from Z2
      if (pattern == "Z1_Z2_boundary") return("Z1")  # still at plug end
      if (pattern == "INCONSISTENT" ||
          pattern == "MULTIPASS")      next
    }

    if (boundary == "Z2_Z3_boundary") {
      if (pattern == "Z2")             return("Z2")
      if (pattern == "Z3")             return("Z3")
      if (pattern == "Z1_Z2_boundary") return("Z2")
      if (pattern == "Z3_Z4_boundary") return("Z3")
      if (pattern == "Z4")             return("Z3")   # came from food side
      if (pattern == "Z1")             return("Z2")   # came from plug side
      if (pattern == "Z2_Z3_boundary" ||
          pattern == "INCONSISTENT"   ||
          pattern == "MULTIPASS")      next
    }

    if (boundary == "Z3_Z4_boundary") {
      if (pattern == "Z3")             return("Z3")
      if (pattern == "Z2_Z3_boundary") return("Z3")
      if (pattern == "Z2")             return("Z3")
      if (pattern == "Z1_Z2_boundary") return("Z3")
      if (pattern == "Z1")             return("Z3")   # far side — came from Z3
      if (pattern == "Z3_Z4_boundary") return("Z4")  # still at food end
      if (pattern == "INCONSISTENT" ||
          pattern == "MULTIPASS")      next
    }
  }

  # Searched entire channel and found nothing conclusive
  if (boundary == "Z1_Z2_boundary") return("Z1")
  if (boundary == "Z2_Z3_boundary") return(NA_character_)
  if (boundary == "Z3_Z4_boundary") return("Z4")

  return(NA_character_)
}


# ============================================================
# HELPER: anchor_zone_from_context()
# ============================================================

anchor_zone_from_context <- function(
    ch_dat,
    t_start_idx,
    t_end_idx,
    look_window = 5    # kept for API compatibility; search extends full channel
) {

  n <- nrow(ch_dat)

  # Returns list(zone, how): how = "confirmed" (Z2/Z3) or "resolved" (boundary)
  get_zone_from_direction <- function(start, direction) {

    if (direction == "before") {
      if (start < 1) return(list(zone = NA_character_, how = NA_character_))
      idx_seq <- seq(start, 1L, by = -1)
    } else {
      if (start > n) return(list(zone = NA_character_, how = NA_character_))
      idx_seq <- seq(start, n, by = 1)
    }

    for (i in idx_seq) {
      bp <- ch_dat$B_plug[i]
      bm <- ch_dat$B_middle[i]
      bf <- ch_dat$B_food[i]

      pattern <- beam_to_zone(bp, bm, bf)
      if (is.na(pattern)) next   # all zero — fly inactive, keep searching

      if (pattern %in% c("Z2", "Z3"))
        return(list(zone = pattern, how = "confirmed"))

      if (grepl("boundary", pattern)) {
        resolved <- resolve_boundary(pattern, ch_dat, i, direction)
        if (!is.na(resolved)) return(list(zone = resolved, how = "resolved"))
        next
      }
      # INCONSISTENT or MULTIPASS — skip and keep searching
    }

    return(list(zone = NA_character_, how = NA_character_))
  }

  res_before <- get_zone_from_direction(t_start_idx - 1, "before")
  res_after  <- get_zone_from_direction(t_end_idx  + 1, "after")

  zone_before <- res_before$zone
  zone_after  <- res_after$zone
  how_before  <- res_before$how
  how_after   <- res_after$how

  if (!is.na(zone_before) && !is.na(zone_after)) {
    if (zone_before == zone_after) {
      return(list(zone = zone_before, confidence = "confirmed"))
    } else {
      # Prefer whichever was confirmed directly by a two-beam pattern
      if (how_before == "confirmed" && how_after != "confirmed") {
        return(list(zone = zone_before, confidence = "inferred_before"))
      } else if (how_after == "confirmed" && how_before != "confirmed") {
        return(list(zone = zone_after,  confidence = "inferred_after"))
      } else {
        return(list(zone = zone_before, confidence = "inferred_before"))
      }
    }
  } else if (!is.na(zone_before)) {
    return(list(zone = zone_before, confidence = "before_only"))
  } else if (!is.na(zone_after)) {
    return(list(zone = zone_after, confidence = "after_only"))
  } else {
    return(list(zone = NA_character_, confidence = "ambiguous"))
  }
}


# ============================================================
# HELPER: compute_parity()
# ============================================================

compute_parity <- function(bp_sum, bm_sum, bf_sum) {
  list(
    plug_odd   = (bp_sum %% 2) == 1,
    middle_odd = (bm_sum %% 2) == 1,
    food_odd   = (bf_sum %% 2) == 1,
    total      = bp_sum + bm_sum + bf_sum
  )
}


# ============================================================
# HELPER: classify_transit()
# ============================================================

classify_transit <- function(
    zone_before, zone_after,
    bp_sum, bm_sum, bf_sum
) {

  parity <- compute_parity(bp_sum, bm_sum, bf_sum)
  total  <- parity$total

  if (total == 0) return("TRANSIT_UNANCHORED")

  if (is.na(zone_before) && is.na(zone_after)) return("TRANSIT_UNANCHORED")

  # Only one anchor available — classify from parity alone
  if (is.na(zone_before) || is.na(zone_after)) {
    if (total == 1) return("TRANSIT_PASS")
    if (!parity$plug_odd && !parity$middle_odd && !parity$food_odd) {
      return("TRANSIT_RETURN")
    }
    return("TRANSIT_MULTIPASS")
  }

  # Both anchors known — same zone before and after → TRANSIT_RETURN.
  # Anchor agreement is ground truth (DAM beam parity is unreliable here).
  if (zone_before == zone_after) {
    return("TRANSIT_RETURN")
  }

  # Both anchors known — different zones, fly moved
  if (total == 1) return("TRANSIT_PASS")
  if (total == 2) {
    if (!parity$plug_odd && !parity$middle_odd && !parity$food_odd) {
      return("TRANSIT_RETURN")
    }
    return("TRANSIT_PASS")
  }
  return("TRANSIT_MULTIPASS")
}


# ============================================================
# PASS 1+2: identify_and_zone_anchors()
# ============================================================

identify_and_zone_anchors <- function(
    ch_dat,
    sleep_threshold_min = 5,
    look_window         = 5
) {

  n         <- nrow(ch_dat)
  is_active <- (ch_dat$B_plug + ch_dat$B_middle + ch_dat$B_food) > 0

  anchors <- data.table(
    start_idx    = integer(),
    end_idx      = integer(),
    duration_min = integer(),
    anchor_type  = character(),
    zone         = character(),
    confidence   = character()
  )

  t <- 1L
  while (t <= n) {
    if (!is_active[t]) {
      run_end <- t
      while (run_end < n && !is_active[run_end + 1L]) {
        run_end <- run_end + 1L
      }
      dur   <- run_end - t + 1L
      atype <- if (dur >= sleep_threshold_min) "SLEEP" else "STOP"
      ctx   <- anchor_zone_from_context(ch_dat, t, run_end, look_window)

      anchors <- rbind(anchors, data.table(
        start_idx    = t,
        end_idx      = run_end,
        duration_min = dur,
        anchor_type  = atype,
        zone         = ctx$zone,
        confidence   = ctx$confidence
      ))

      t <- run_end + 1L
    } else {
      t <- t + 1L
    }
  }

  return(anchors)
}


# ============================================================
# PASS 3: reconstruct_transit_channel()
# ============================================================

reconstruct_transit_channel <- function(
    ch_dat,
    anchors,
    sleep_threshold_min = 5
) {

  n <- nrow(ch_dat)

  anchor_type_vec   <- rep(NA_character_, n)
  anchor_zone_vec   <- rep(NA_character_, n)
  transit_type_vec  <- rep(NA_character_, n)
  zone_anchored_vec <- rep(NA_character_, n)

  # Mark anchor bins
  for (i in seq_len(nrow(anchors))) {
    idx <- anchors$start_idx[i]:anchors$end_idx[i]
    anchor_type_vec[idx]   <- anchors$anchor_type[i]
    anchor_zone_vec[idx]   <- anchors$zone[i]
    zone_anchored_vec[idx] <- anchors$zone[i]
  }

  is_active <- (ch_dat$B_plug + ch_dat$B_middle + ch_dat$B_food) > 0

  t <- 1L
  while (t <= n) {
    if (is_active[t] && is.na(anchor_type_vec[t])) {

      bout_end <- t
      while (bout_end < n &&
             is_active[bout_end + 1L] &&
             is.na(anchor_type_vec[bout_end + 1L])) {
        bout_end <- bout_end + 1L
      }

      # Nearest anchor before with known zone
      anchor_before_zone <- NA_character_
      search <- t - 1L
      while (search >= 1L) {
        if (!is.na(anchor_type_vec[search]) &&
            !is.na(anchor_zone_vec[search])) {
          anchor_before_zone <- anchor_zone_vec[search]
          break
        }
        search <- search - 1L
      }

      # Nearest anchor after with known zone
      anchor_after_zone <- NA_character_
      search <- bout_end + 1L
      while (search <= n) {
        if (!is.na(anchor_type_vec[search]) &&
            !is.na(anchor_zone_vec[search])) {
          anchor_after_zone <- anchor_zone_vec[search]
          break
        }
        search <- search + 1L
      }

      bout_idx <- t:bout_end
      bp_sum   <- sum(ch_dat$B_plug[bout_idx])
      bm_sum   <- sum(ch_dat$B_middle[bout_idx])
      bf_sum   <- sum(ch_dat$B_food[bout_idx])

      ttype <- classify_transit(anchor_before_zone, anchor_after_zone,
                                bp_sum, bm_sum, bf_sum)

      transit_type_vec[bout_idx] <- ttype

      if (ttype == "TRANSIT_RETURN" && !is.na(anchor_before_zone)) {
        zone_anchored_vec[bout_idx] <- anchor_before_zone
      } else if (ttype == "TRANSIT_MULTIPASS" && !is.na(anchor_after_zone)) {
        zone_anchored_vec[bout_idx] <- anchor_after_zone
      }
      # TRANSIT_PASS, TRANSIT_INCONSISTENT, TRANSIT_UNANCHORED → NA

      t <- bout_end + 1L
    } else {
      t <- t + 1L
    }
  }

  return(list(
    anchor_type   = anchor_type_vec,
    anchor_zone   = anchor_zone_vec,
    transit_type  = transit_type_vec,
    zone_anchored = zone_anchored_vec
  ))
}


# ============================================================
# PASS 4: assign_sleep_zones_anchored()
# ============================================================

assign_sleep_zones_anchored <- function(ch_dat, anchors) {

  n              <- nrow(ch_dat)
  sleep_zone_vec <- rep(NA_character_, n)
  bout_start_vec <- rep(FALSE, n)

  sleep_anchors <- anchors[anchor_type == "SLEEP" & !is.na(zone)]

  for (i in seq_len(nrow(sleep_anchors))) {
    idx <- sleep_anchors$start_idx[i]
    sleep_zone_vec[idx] <- sleep_anchors$zone[i]
    bout_start_vec[idx] <- TRUE
  }

  return(list(
    sleep_zone_anchored = sleep_zone_vec,
    bout_start_anchored = bout_start_vec
  ))
}


# ============================================================
# MAIN: assign_position_anchored()
# ============================================================

assign_position_anchored <- function(
    dat,
    sleep_threshold_min = 5,
    look_window         = 5
) {

  cat("=== Anchor-Based Position Assignment ===\n")
  cat(sprintf("  Sleep threshold : %d minutes\n", sleep_threshold_min))
  cat(sprintf("  Look window     : %d bins\n",    look_window))

  channels <- sort(unique(dat$channel))
  cat(sprintf("  Channels        : %d\n", length(channels)))

  dat[, anchor_type          := NA_character_]
  dat[, anchor_zone          := NA_character_]
  dat[, transit_type         := NA_character_]
  dat[, zone_anchored        := NA_character_]
  dat[, sleep_zone_anchored  := NA_character_]
  dat[, bout_start_anchored  := FALSE]

  all_anchors <- vector("list", length(channels))

  for (i in seq_along(channels)) {
    ch     <- channels[i]
    ch_dat <- dat[channel == ch]
    setorder(ch_dat, datetime)

    anchors   <- identify_and_zone_anchors(ch_dat, sleep_threshold_min,
                                           look_window)
    transit   <- reconstruct_transit_channel(ch_dat, anchors,
                                             sleep_threshold_min)
    sleep_out <- assign_sleep_zones_anchored(ch_dat, anchors)

    dat[channel == ch,
        `:=`(
          anchor_type         = transit$anchor_type,
          anchor_zone         = transit$anchor_zone,
          transit_type        = transit$transit_type,
          zone_anchored       = transit$zone_anchored,
          sleep_zone_anchored = sleep_out$sleep_zone_anchored,
          bout_start_anchored = sleep_out$bout_start_anchored
        )]

    if (nrow(anchors) > 0) {
      anchors[, channel := ch]
      all_anchors[[i]] <- anchors
    }
  }

  all_anchors_dt <- rbindlist(all_anchors, fill = TRUE)

  # -- Console summary ------------------------------------------
  cat("\n  Anchor summary per channel:\n")
  cat(sprintf("  %-8s %-6s %-6s %-12s %-14s\n",
              "Channel", "SLEEP", "STOP", "Ambiguous", "Inconsistent"))
  cat(sprintf("  %s\n", strrep("-", 52)))

  for (ch in channels) {
    ch_anc  <- all_anchors_dt[channel == ch]
    n_sleep <- sum(ch_anc$anchor_type == "SLEEP",    na.rm = TRUE)
    n_stop  <- sum(ch_anc$anchor_type == "STOP",     na.rm = TRUE)
    n_ambig <- sum(ch_anc$confidence  == "ambiguous", na.rm = TRUE)
    n_incon <- sum(dat[channel == ch,
                       transit_type == "TRANSIT_INCONSISTENT"],
                   na.rm = TRUE)
    cat(sprintf("  Ch %2d    %4d   %4d   %10d   %12d\n",
                ch, n_sleep, n_stop, n_ambig, n_incon))
  }

  cat("\n  Transit type distribution:\n")
  tc <- dat[!is.na(transit_type), .N, by = transit_type]
  setorder(tc, -N)
  for (j in seq_len(nrow(tc))) {
    cat(sprintf("    %-28s : %d bins\n", tc$transit_type[j], tc$N[j]))
  }

  sleep_bouts <- dat[bout_start_anchored == TRUE]
  cat(sprintf("\n  Sleep bouts (SLEEP anchors): %d\n", nrow(sleep_bouts)))
  if (nrow(sleep_bouts) > 0) {
    cat("  Sleep zone distribution:\n")
    print(table(sleep_bouts$sleep_zone_anchored, useNA = "ifany"))
  }

  n_incon_total <- sum(dat$transit_type == "TRANSIT_INCONSISTENT",
                       na.rm = TRUE)
  n_ambig_total <- sum(all_anchors_dt$confidence == "ambiguous",
                       na.rm = TRUE)
  if (n_incon_total > 0 || n_ambig_total > 0) {
    cat("\n  *** Manual review recommended ***\n")
    cat(sprintf("  TRANSIT_INCONSISTENT bins : %d\n", n_incon_total))
    cat(sprintf("  Ambiguous anchor zones    : %d\n", n_ambig_total))
    cat("  Inspect: dat[transit_type == 'TRANSIT_INCONSISTENT']\n")
    cat("  Inspect: anchors[confidence == 'ambiguous']\n")
  }

  cat("=== Done ===\n\n")

  return(list(dat = dat, anchors = all_anchors_dt))
}


# ============================================================
# DIAGNOSTIC: print_anchor_timeline()
# ============================================================

print_anchor_timeline <- function(dat, channel_num, max_rows = 100) {

  ch_dat <- dat[channel == channel_num]
  setorder(ch_dat, datetime)

  cat(sprintf("\n=== Anchor timeline: Channel %d ===\n", channel_num))
  cat(sprintf("  %-18s %-5s %-5s %-5s %-6s %-12s %-22s %-12s\n",
              "Datetime", "Plug", "Mid", "Food",
              "Anchor", "Zone", "Transit", "Sleep zone"))
  cat(sprintf("  %s\n", strrep("-", 92)))

  rows_shown <- 0
  for (i in seq_len(nrow(ch_dat))) {
    row <- ch_dat[i]
    has_info <- (!is.na(row$anchor_type) ||
                   !is.na(row$transit_type) ||
                   isTRUE(row$bout_start_anchored))
    if (!has_info) next
    if (rows_shown >= max_rows) {
      cat(sprintf("  ... (truncated at %d rows, use max_rows to see more)\n",
                  max_rows))
      break
    }
    cat(sprintf("  %-18s %5d %5d %5d %-6s %-12s %-22s %-12s\n",
                format(row$datetime, "%m/%d %H:%M"),
                row$B_plug, row$B_middle, row$B_food,
                ifelse(is.na(row$anchor_type), "", row$anchor_type),
                ifelse(is.na(row$anchor_zone), "", row$anchor_zone),
                ifelse(is.na(row$transit_type), "", row$transit_type),
                ifelse(is.na(row$sleep_zone_anchored), "",
                       row$sleep_zone_anchored)))
    rows_shown <- rows_shown + 1
  }
  cat("=== End timeline ===\n\n")
}
