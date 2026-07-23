# ============================================================
# R/04_plots.R  —  VISUALIZATIONS (single canonical set)
#
# One set of plot functions. Every function takes a `use_anchored`
# flag that selects which zone assignment to visualize:
#
#   use_anchored = TRUE  (default) -> zone_anchored / sleep_zone_anchored /
#                                     bout_start_anchored   (anchor method)
#   use_anchored = FALSE           -> zone / sleep_zone / bout_start
#                                     (state-machine + backfill method)
#
# This replaces the two former same-named copies that silently
# overwrote each other depending on source order.
#
# Provides:
#   plot_individual_timelines()
#   plot_sleep_zone_heatmap()
#   plot_sleep_zone_summary()
#   plot_sleep_zone_summary_heatmap()
#
# Requires (loaded by the run script): data.table, ggplot2, scales, lubridate
# ============================================================


# -- Shared zone palette --------------------------------------
zone_colors <- c(
  "Z1"      = "#2166AC",  # blue        - near plug
  "Z2"      = "#92C5DE",  # light blue  - plug-middle
  "Z3"      = "#F4A582",  # light red   - middle-food
  "Z4"      = "#D6604D",  # red         - near food
  "UNKNOWN" = "#CCCCCC"   # grey
)

zone_labels <- c(
  "Z1"      = "Z1 (plug end)",
  "Z2"      = "Z2 (plug-mid)",
  "Z3"      = "Z3 (mid-food)",
  "Z4"      = "Z4 (food end)",
  "UNKNOWN" = "Unknown"
)

# -- Internal helper: pick the right column names -------------
.zone_cols <- function(use_anchored) {
  if (use_anchored) {
    list(zone = "zone_anchored",
         sleep_zone = "sleep_zone_anchored",
         bout_start = "bout_start_anchored")
  } else {
    list(zone = "zone",
         sleep_zone = "sleep_zone",
         bout_start = "bout_start")
  }
}


# ============================================================
# 5A: Individual fly movement path timelines (faceted)
#
# Active periods   — geom_step line colored by zone, width ~ beam count
# Inactive periods — grey flat line + dot at run start, dot size ~ duration
# Light/dark       — background shading (dark phase = grey)
# X-axis           — elapsed hours from start_datetime (t = 0)
# ============================================================

plot_individual_timelines <- function(
    dat,
    channels_to_plot  = NULL,
    use_anchored      = TRUE,
    ncol_facet        = 4,
    lights_on         = "05:00:00",
    lights_off        = "17:00:00",
    dot_size_range    = c(1, 8),
    line_width_range  = c(0.3, 1.5),
    output_file       = "position_timelines.pdf",
    width             = 20,
    height            = NULL
) {

  cols <- .zone_cols(use_anchored)

  if (is.null(channels_to_plot)) channels_to_plot <- sort(unique(dat$channel))

  plot_dat <- dat[channel %in% channels_to_plot]
  setorder(plot_dat, channel, datetime)

  tz <- lubridate::tz(plot_dat$datetime[1])
  t0 <- min(plot_dat$datetime)   # start_datetime = t=0

  plot_dat[, elapsed_hr := as.numeric(difftime(datetime, t0, units = "hours"))]

  # Zone factor (ordered plug -> food); UNKNOWN/NA dropped from paths
  zone_order <- c("Z1","Z2","Z3","Z4")
  zone_ylabs <- c("Z1 (plug)","Z2 (plug-mid)","Z3 (mid-food)","Z4 (food)")
  plot_dat[, zone_f := factor(get(cols$zone),
                              levels = zone_order,
                              labels = zone_ylabs)]

  plot_dat[, activity := B_plug + B_middle + B_food]

  active_dat   <- plot_dat[activity > 0 & !is.na(zone_f)]
  inactive_dat <- plot_dat[activity == 0]

  # Contiguous inactive runs per channel
  inactive_runs <- inactive_dat[, {
    is_gap  <- c(0, diff(elapsed_hr)) > (1/60 * 2)  # >2 min gap = new run
    run_id  <- cumsum(is_gap) + 1L
    .SD[, .(
      start_hr     = elapsed_hr[1],
      end_hr       = elapsed_hr[.N],
      duration_min = .N,
      zone_f       = zone_f[1]
    ), by = run_id]
  }, by = channel]

  max_dur <- max(inactive_runs$duration_min, na.rm = TRUE)
  min_dur <- min(inactive_runs$duration_min, na.rm = TRUE)
  inactive_runs[, dot_size := scales::rescale(
    sqrt(duration_min),
    to   = dot_size_range,
    from = sqrt(c(min_dur, max_dur))
  )]

  # Light/dark background rectangles
  hr_max          <- max(plot_dat$elapsed_hr, na.rm = TRUE)
  lights_on_secs  <- as.numeric(lubridate::hms(lights_on))
  lights_off_secs <- as.numeric(lubridate::hms(lights_off))

  day_seq <- seq(as.Date(t0, tz = tz),
                 as.Date(t0 + (hr_max + 24) * 3600, tz = tz),
                 by = "day")

  dark_rects <- rbindlist(Filter(Negate(is.null), lapply(day_seq, function(d) {
    day_origin <- as.POSIXct(as.character(d), tz = tz)
    d1s <- as.numeric(difftime(day_origin + lights_off_secs, t0, units = "hours"))
    d1e <- as.numeric(difftime(day_origin + 86400,           t0, units = "hours"))
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

  n_rows <- ceiling(length(channels_to_plot) / ncol_facet)
  if (is.null(height)) height <- n_rows * 3 + 2

  method_tag <- if (use_anchored) "anchor method" else "state-machine method"

  p <- ggplot() +

    geom_rect(data = dark_rects,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "grey80", alpha = 0.35, inherit.aes = FALSE) +

    geom_segment(data = inactive_runs[!is.na(zone_f)],
                 aes(x = start_hr, xend = end_hr,
                     y = zone_f,   yend = zone_f),
                 color     = "grey70",
                 linewidth = 0.4,
                 lineend   = "round") +

    geom_point(data = inactive_runs[!is.na(zone_f)],
               aes(x    = start_hr,
                   y    = zone_f,
                   size = dot_size,
                   color = zone_f),
               shape = 21,
               fill  = "grey85",
               stroke = 0.8,
               alpha  = 0.85) +

    geom_step(data = active_dat,
              aes(x         = elapsed_hr,
                  y         = zone_f,
                  color     = zone_f,
                  linewidth = activity,
                  group     = channel),
              alpha    = 0.75,
              linejoin = "round") +

    facet_wrap(~ paste0("Ch ", channel),
               ncol   = ncol_facet,
               scales = "free_x") +

    scale_color_manual(
      values = c(
        "Z1 (plug)"     = unname(zone_colors["Z1"]),
        "Z2 (plug-mid)" = unname(zone_colors["Z2"]),
        "Z3 (mid-food)" = unname(zone_colors["Z3"]),
        "Z4 (food)"     = unname(zone_colors["Z4"])
      ),
      name = "Zone"
    ) +

    scale_linewidth_continuous(
      range  = line_width_range,
      name   = "Beam count\n(activity)"
    ) +

    scale_size_identity(guide = "none") +

    scale_x_continuous(
      breaks = seq(0, ceiling(hr_max), by = 12),
      labels = function(x) paste0(x, "h"),
      expand = expansion(mult = 0.01)
    ) +

    scale_y_discrete(drop = FALSE) +

    labs(
      title    = sprintf("Fly Movement Path - Individual Channels (%s)", method_tag),
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

  if (!is.null(output_file)) {
    ggsave(output_file, p,
           width = width, height = height, units = "in", dpi = 150,
           limitsize = FALSE)
    cat(sprintf("Saved: %s (%.0f x %.0f in)\n", output_file, width, height))
  }
  return(p)
}


# -- Internal helper: sleep bouts as (channel, sleep_zone) ----
# Restricts to Z1-Z4 so both methods are comparable.
.sleep_bouts_long <- function(dat, use_anchored, channels_to_plot) {
  cols <- .zone_cols(use_anchored)
  sd   <- dat[get(cols$bout_start) == TRUE & channel %in% channels_to_plot]
  sd   <- copy(sd)
  sd[, sleep_zone := get(cols$sleep_zone)]
  sd[sleep_zone %in% c("Z1","Z2","Z3","Z4")]
}


# ============================================================
# 5B: Group-level sleep zone heatmap
# ============================================================

plot_sleep_zone_heatmap <- function(
    dat,
    channels_to_plot = NULL,
    use_anchored     = TRUE,
    group_label      = "All flies",
    output_file      = "sleep_zone_heatmap.pdf",
    width            = 14,
    height           = 6
) {

  if (is.null(channels_to_plot)) channels_to_plot <- sort(unique(dat$channel))

  sleep_dat   <- .sleep_bouts_long(dat, use_anchored, channels_to_plot)
  bout_counts <- sleep_dat[, .N, by = .(channel, sleep_zone)]

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

  if (!is.null(output_file)) {
    ggsave(output_file, p, width = width, height = height, units = "in", dpi = 150)
    cat(sprintf("Saved: %s (%.0f x %.0f in)\n", output_file, width, height))
  }
  return(p)
}


# ============================================================
# 5C: Group summary bar plot (mean +/- SEM)
# ============================================================

plot_sleep_zone_summary <- function(
    dat,
    channels_to_plot = NULL,
    use_anchored     = TRUE,
    group_label      = "All flies",
    output_file      = "sleep_zone_summary.pdf",
    width            = 7,
    height           = 5
) {

  if (is.null(channels_to_plot)) channels_to_plot <- sort(unique(dat$channel))

  sleep_dat <- .sleep_bouts_long(dat, use_anchored, channels_to_plot)

  per_fly <- sleep_dat[, .N, by = .(channel, sleep_zone)]
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

  if (!is.null(output_file)) {
    ggsave(output_file, p, width = width, height = height, units = "in", dpi = 150)
    cat(sprintf("Saved: %s (%.0f x %.0f in)\n", output_file, width, height))
  }
  return(p)
}


# ============================================================
# 5D: Group summary heatmap (mean proportion per zone)
# ============================================================

plot_sleep_zone_summary_heatmap <- function(
    dat,
    channels_to_plot = NULL,
    use_anchored     = TRUE,
    group_label      = "All flies",
    output_file      = "sleep_zone_summary_heatmap.pdf",
    width            = 4,
    height           = 6
) {

  if (is.null(channels_to_plot)) channels_to_plot <- sort(unique(dat$channel))

  sleep_dat <- .sleep_bouts_long(dat, use_anchored, channels_to_plot)

  per_fly <- sleep_dat[, .N, by = .(channel, sleep_zone)]
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

  if (!is.null(output_file)) {
    ggsave(output_file, p, width = width, height = height, units = "in", dpi = 150)
    cat(sprintf("Saved: %s (%.0f x %.0f in)\n", output_file, width, height))
  }
  return(p)
}
