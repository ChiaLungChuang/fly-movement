# ============================================================
# ANCHOR-BASED POSITION ASSIGNMENT
#
# Replaces the state machine zone assignment with a physically
# rigorous anchor-based approach:
#
#   Pass 1+2: identify_and_zone_anchors()
#     - Find all zero-activity runs (SLEEP >= 5 min, STOP 1-4 min)
#     - Assign zone from surrounding beam context using corrected
#       beam_to_zone() and boundary resolution logic
#
#   Pass 3: reconstruct_transit_channel()
#     - For each active bin, find nearest anchor before and after
#       (no fixed window — extends until anchor found)
#     - Classify transit using beam count parity:
#         TRANSIT_PASS        — fly passed through, no confirmed stop
#         TRANSIT_RETURN      — fly went out and came back
#         TRANSIT_MULTIPASS   — multiple crossings, endpoint known
#         TRANSIT_INCONSISTENT — parity contradicts anchors, flag
#         TRANSIT_UNANCHORED  — no anchor found on either side
#
#   Pass 4: assign_sleep_zones_anchored()
#     - Sleep zones from SLEEP anchors ONLY
#     - STOP anchors contribute to zone_anchored but not sleep_zone
#     - TRANSIT_* bins excluded from sleep zone analysis
#
# Key output columns:
#   zone_anchored        — confirmed zone (anchors + RETURN/MULTIPASS)
#                          NA for TRANSIT_PASS/INCONSISTENT/UNANCHORED
#   sleep_zone_anchored  — sleep zone from SLEEP anchors only
#   bout_start_anchored  — TRUE at start of each SLEEP anchor
#   anchor_type          — "SLEEP", "STOP", NA
#   anchor_zone          — zone at anchor, NA for active bins
#   transit_type         — TRANSIT_* for active bins, NA for anchors
# ============================================================

library(data.table)


# ============================================================
# HELPER: beam_to_zone()
#
# Corrected beam pattern interpretation.
#
# Two-beam patterns are the most reliable — the fly was
# definitively in the corridor between those two monitors.
#
# Single-beam patterns are ambiguous — the fly could be
# stationary on either side of that beam, or crossing it.
# Returns a boundary label rather than a zone so the caller
# can resolve using anchor context.
#
# Returns:
#   "Z2"              — plug + middle both active (confirmed)
#   "Z3"              — middle + food both active (confirmed)
#   "Z1_Z2_boundary"  — plug only (ambiguous: Z1 or Z2)
#   "Z2_Z3_boundary"  — middle only (ambiguous: Z2 or Z3)
#   "Z3_Z4_boundary"  — food only (ambiguous: Z3 or Z4)
#   "INCONSISTENT"    — plug + food active, middle zero
#                       (physically improbable in one minute)
#   "MULTIPASS"       — all three active (traversed full tube)
#   NA                — all zero (cannot determine)
# ============================================================

beam_to_zone <- function(bp, bm, bf) {
  
  # All zero — cannot determine from this bin alone
  if (bp == 0 && bm == 0 && bf == 0) return(NA_character_)
  
  # ── Two-beam confirmed patterns ───────────────────────────
  # Fly was definitively in the corridor between these two monitors
  if (bp > 0 && bm > 0 && bf == 0) return("Z2")   # plug-mid corridor
  if (bp == 0 && bm > 0 && bf > 0) return("Z3")   # mid-food corridor
  
  # ── Single-beam boundary patterns ────────────────────────
  # Ambiguous — fly at or crossing this beam boundary
  if (bp > 0 && bm == 0 && bf == 0) return("Z1_Z2_boundary")
  if (bp == 0 && bm > 0 && bf == 0) return("Z2_Z3_boundary")
  if (bp == 0 && bm == 0 && bf > 0) return("Z3_Z4_boundary")
  
  # ── Physically ambiguous multi-beam patterns ─────────────
  if (bp > 0 && bm == 0 && bf > 0) return("INCONSISTENT")  # plug+food, no middle
  if (bp > 0 && bm > 0  && bf > 0) return("MULTIPASS")     # all three
  
  return(NA_character_)
}


# ============================================================
# HELPER: resolve_boundary()
#
# When beam_to_zone() returns a boundary label, use additional
# context bins to determine which side of the boundary the
# fly was on.
#
# Logic:
#   Z1_Z2_boundary:
#     Look further back/forward for a two-beam pattern or
#     another single-beam pattern that disambiguates.
#     - If next confirmed pattern is Z2 or Z2_Z3 → fly was in Z2
#     - If next confirmed pattern is Z1_Z2 again → fly in Z1
#       (bouncing at plug end)
#     - If context is an anchor (all zero) → fly stopped near
#       this boundary; use the anchor zone from the other side
#
#   Same logic applies for Z2_Z3_boundary and Z3_Z4_boundary.
#
# Returns: resolved zone string ("Z1","Z2","Z3","Z4") or NA
# ============================================================

resolve_boundary <- function(
    boundary,      # e.g. "Z1_Z2_boundary"
    ch_dat,        # full channel data ordered by datetime
    ref_idx,       # index of the boundary bin
    direction      # "before" or "after" — which direction to look
    # no look_window limit — searches entire channel
) {
  
  n <- nrow(ch_dat)
  
  # Build full search sequence in the given direction
  if (direction == "before") {
    if (ref_idx - 1 < 1) {
      # At the very start of recording — use endpoint heuristic
      if (boundary == "Z1_Z2_boundary") return("Z1")
      if (boundary == "Z3_Z4_boundary") return("Z4")
      return(NA_character_)
    }
    idx_seq <- seq(ref_idx - 1, 1L, by = -1)
  } else {
    if (ref_idx + 1 > n) {
      # At the very end of recording — use endpoint heuristic
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
    
    # Resolve based on boundary type and the first non-zero pattern found
    if (boundary == "Z1_Z2_boundary") {
      if (pattern == "Z2")             return("Z2")
      if (pattern == "Z2_Z3_boundary") return("Z2")
      if (pattern == "Z3")             return("Z2")
      if (pattern == "Z3_Z4_boundary") return("Z2")
      if (pattern == "Z4")             return("Z2")   # far side — came from Z2
      if (pattern == "Z1_Z2_boundary") return("Z1")  # still at plug end
      if (pattern == "INCONSISTENT" ||
          pattern == "MULTIPASS")      next            # skip, keep searching
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
  # Fall back to endpoint heuristic
  if (boundary == "Z1_Z2_boundary") return("Z1")
  if (boundary == "Z2_Z3_boundary") return(NA_character_)
  if (boundary == "Z3_Z4_boundary") return("Z4")
  
  return(NA_character_)
}


# ============================================================
# HELPER: anchor_zone_from_context()
#
# For a zero-activity run (anchor) from t_start_idx to t_end_idx,
# determines which zone the fly was resting in.
#
# Uses beam_to_zone() on surrounding bins, then resolves any
# boundary labels using resolve_boundary().
#
# Resolution priority:
#   1. Confirmed two-beam pattern (Z2 or Z3) — highest confidence
#   2. Resolved boundary from context — medium confidence
#   3. Heuristic boundary resolution — lower confidence
#   4. NA — truly ambiguous, flag for review
#
# If before and after give different zones:
#   → Use zone_before (fly arrived here, then stopped)
#   → Flag as "inferred_before" confidence
# ============================================================

anchor_zone_from_context <- function(
    ch_dat,
    t_start_idx,
    t_end_idx,
    look_window = 5    # kept as parameter but no longer used as hard limit
    # search extends to full channel if needed
) {
  
  n <- nrow(ch_dat)
  
  # ── Helper: get zone from one direction ───────────────────
  # Returns list(zone, how):
  #   zone — resolved zone string or NA
  #   how  — "confirmed" (direct two-beam pattern Z2/Z3) or
  #          "resolved"  (boundary resolved from context)
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

      # Confirmed two-beam patterns — highest confidence, return directly
      if (pattern %in% c("Z2", "Z3"))
        return(list(zone = pattern, how = "confirmed"))

      # Boundary patterns — resolve using full channel context
      if (grepl("boundary", pattern)) {
        resolved <- resolve_boundary(pattern, ch_dat, i, direction)
        if (!is.na(resolved)) return(list(zone = resolved, how = "resolved"))
        next   # could not resolve this bin, keep searching
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

  # ── Resolve final anchor zone ─────────────────────────────
  if (!is.na(zone_before) && !is.na(zone_after)) {
    if (zone_before == zone_after) {
      return(list(zone = zone_before, confidence = "confirmed"))
    } else {
      # Zones disagree — prefer whichever was confirmed directly by a two-beam
      # pattern. Two-beam bins are unambiguous (fly was in that corridor);
      # resolved boundary bins infer the zone from context and often capture a
      # fly still in transit rather than at rest.
      if (how_before == "confirmed" && how_after != "confirmed") {
        return(list(zone = zone_before, confidence = "inferred_before"))
      } else if (how_after == "confirmed" && how_before != "confirmed") {
        return(list(zone = zone_after,  confidence = "inferred_after"))
      } else {
        # Both same confidence — fly arrived from zone_before and stopped
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
  
  # Both anchors NA — cannot classify, not an inconsistency
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
  # Parity of raw DAM beam-break sums is unreliable for same-zone transits:
  # DAM counts interruptions per minute (not single crossings), so a fly
  # bouncing near one beam produces odd parity while remaining in the same zone.
  # The anchor agreement is the ground truth here.
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
      
      # Find end of active bout
      bout_end <- t
      while (bout_end < n &&
             is_active[bout_end + 1L] &&
             is.na(anchor_type_vec[bout_end + 1L])) {
        bout_end <- bout_end + 1L
      }
      
      # Search backward for nearest anchor WITH a known zone
      # Skip anchors whose zone is NA — they cannot inform transit
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
      
      # Search forward for nearest anchor WITH a known zone
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
      
      # Classify transit
      bout_idx <- t:bout_end
      bp_sum   <- sum(ch_dat$B_plug[bout_idx])
      bm_sum   <- sum(ch_dat$B_middle[bout_idx])
      bf_sum   <- sum(ch_dat$B_food[bout_idx])
      
      ttype <- classify_transit(anchor_before_zone, anchor_after_zone,
                                bp_sum, bm_sum, bf_sum)
      
      transit_type_vec[bout_idx] <- ttype
      
      # Assign zone_anchored for resolvable transit types
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
    look_window         = 5    # kept for API compatibility; boundary resolution
    # now searches the full channel regardless
) {
  
  cat("=== Anchor-Based Position Assignment ===\n")
  cat(sprintf("  Sleep threshold : %d minutes\n", sleep_threshold_min))
  cat(sprintf("  Look window     : %d bins\n",    look_window))
  
  channels <- sort(unique(dat$channel))
  cat(sprintf("  Channels        : %d\n", length(channels)))
  
  # Initialize output columns
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
  
  # ── Console summary ───────────────────────────────────────
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
  
  # Transit distribution
  cat("\n  Transit type distribution:\n")
  tc <- dat[!is.na(transit_type), .N, by = transit_type]
  setorder(tc, -N)
  for (j in seq_len(nrow(tc))) {
    cat(sprintf("    %-28s : %d bins\n", tc$transit_type[j], tc$N[j]))
  }
  
  # Sleep zone summary
  sleep_bouts <- dat[bout_start_anchored == TRUE]
  cat(sprintf("\n  Sleep bouts (SLEEP anchors): %d\n", nrow(sleep_bouts)))
  if (nrow(sleep_bouts) > 0) {
    cat("  Sleep zone distribution:\n")
    print(table(sleep_bouts$sleep_zone_anchored, useNA = "ifany"))
  }
  
  # Manual review flags
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
#
# Prints the anchor and transit sequence for one channel.
# Use for manual review of flagged bins.
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


# ============================================================
# HOW TO INTEGRATE INTO THE MAIN PIPELINE
#
# Add after Step 4.5 (backfill_and_detect_sleep):
#
#   anchor_result <- assign_position_anchored(
#     dat,
#     sleep_threshold_min = 5,
#     look_window         = 5
#   )
#   dat     <- anchor_result$dat
#   anchors <- anchor_result$anchors
#
# Optional diagnostics:
#   print_anchor_timeline(dat, channel_num = 5)
#   dat[transit_type == "TRANSIT_INCONSISTENT"]
#   anchors[confidence == "ambiguous"]
#
# Visualizations: pass use_anchored = TRUE to plot functions
#   (see updated plot functions in dam_position_pipeline.R)
# ============================================================