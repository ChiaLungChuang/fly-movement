# ============================================================
# R/06_chronogram.R  —  ACTIVITY CHRONOGRAM (function defs only)
#
# Two ways to get chronogram-format data:
#   A) read_activity_sheets()      — read Rtivity "Activity Chronogram.xlsx"
#   B) make_activity_chronogram()  — build the SAME format from this
#                                    pipeline's `dat` (no Rtivity needed)
#
# Plus the shared analysis/plot helpers:
#   exclude_channels(), summarize_activity(),
#   plot_activity_chronogram(), save_plot(), filter_by_time()
#
# Chronogram-format columns (long):
#   File | Labels | Channels | Time_min | Activity
#   NOTE: "Time_min" holds ELAPSED SECONDS from t0 (Rtivity's own
#         convention — the x-axis code divides by 3600 to get hours).
#   Activity = beam-break count summed within each 10-min bin per fly.
#
# Requires (loaded by the run script): readxl, dplyr, ggplot2, rlang, data.table
# ============================================================


# -- A) Read Rtivity chronogram sheets, applying label remapping ----
read_activity_sheets <- function(excel_file, target_sheets, label_map) {
  lapply(target_sheets, function(sheet) {
    df <- readxl::read_excel(excel_file, sheet = sheet)
    df <- df %>%
      dplyr::mutate(Labels = dplyr::case_when(
        !!!lapply(seq_along(label_map), function(i) {
          rlang::expr(Labels == !!names(label_map)[i] ~ !!label_map[[i]])
        }),
        TRUE ~ Labels))
    return(df)
  }) %>%
    dplyr::bind_rows()
}


# -- B) Build chronogram-format data from pipeline `dat` -------------
# monitor: "plug" | "middle" | "food" | "summed" (default)
# bin_min: bin width in minutes (Rtivity chronogram uses 10)
# file_label: value for the File column (defaults to the monitor name)
make_activity_chronogram <- function(dat,
                                     group_label,
                                     monitor    = "summed",
                                     bin_min    = 10,
                                     file_label = NULL) {

  stopifnot(monitor %in% c("plug", "middle", "food", "summed"))

  d <- data.table::as.data.table(dat)[, .(datetime, channel,
                                          B_plug, B_middle, B_food)]

  d[, activity := switch(monitor,
                         plug   = B_plug,
                         middle = B_middle,
                         food   = B_food,
                         summed = B_plug + B_middle + B_food)]

  t0      <- min(d$datetime)
  bin_sec <- bin_min * 60
  d[, Time_min := floor(as.numeric(difftime(datetime, t0, units = "secs")) /
                          bin_sec) * bin_sec]

  binned <- d[, .(Activity = sum(activity, na.rm = TRUE)),
              by = .(channel, Time_min)]

  binned[, `:=`(
    File     = if (is.null(file_label)) monitor else file_label,
    Labels   = group_label,
    Channels = channel
  )]

  out <- binned[, .(File, Labels, Channels, Time_min, Activity)]
  data.table::setorder(out, Channels, Time_min)
  as.data.frame(out)
}


# -- Remove dead/excluded channels ----------------------------------
exclude_channels <- function(data, exclusions) {
  data %>%
    dplyr::rowwise() %>%
    dplyr::filter(
      !(File %in% names(exclusions) &&
          Labels %in% names(exclusions[[File]]) &&
          Channels %in% exclusions[[File]][[Labels]])
    ) %>%
    dplyr::ungroup()
}


# -- Summarize: mean activity per channel per 10-min bin + 95% CI ---
summarize_activity <- function(data) {
  data %>%
    dplyr::group_by(Labels, Time_min) %>%
    dplyr::summarize(
      total_activity               = sum(Activity, na.rm = TRUE),
      num_channels                 = dplyr::n_distinct(paste(File, Channels, sep = "_")),
      average_activity_per_channel = total_activity / num_channels,
      sd_activity                  = sd(Activity, na.rm = TRUE),
      n                            = dplyr::n(),
      sem_activity                 = sd_activity / sqrt(n),
      lower_ci                     = average_activity_per_channel - 1.96 * sem_activity,
      upper_ci                     = average_activity_per_channel + 1.96 * sem_activity,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      minutes = Time_min / 60,
      hours   = minutes / 60
    )
}


# -- Plot chronogram -------------------------------------------------
plot_activity_chronogram <- function(summary_data,
                                     color_values,
                                     title        = "Activity Over Time",
                                     lights_on_h  = 0,
                                     lights_off_h = 12) {

  summary_data <- summary_data[is.finite(summary_data$Time_min), ]
  max_time     <- max(summary_data$Time_min, na.rm = TRUE)
  max_y        <- max(summary_data$upper_ci,
                      summary_data$average_activity_per_channel,
                      na.rm = TRUE)

  ggplot2::ggplot(summary_data,
                  ggplot2::aes(x     = Time_min,
                               y     = average_activity_per_channel,
                               color = Labels)) +

    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_ci,
                                      ymax = upper_ci,
                                      fill = Labels),
                         alpha       = 0.2,
                         show.legend = FALSE,
                         color       = NA) +

    ggplot2::geom_point(size = 0.01, stroke = 1) +
    ggplot2::geom_line(linewidth = 1.2) +

    ggplot2::scale_x_continuous(
      breaks = seq(0, max_time, by = 3600),
      labels = function(x) x / 3600,
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +

    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.08, 0.05))
    ) +

    ggplot2::geom_vline(xintercept = c(3600 * 12, 3600 * 24),
                        linetype = "dashed", color = "black", linewidth = 0.8) +

    ggplot2::annotate("rect",
                      xmin = 12 * 3600, xmax = 24 * 3600,
                      ymin = -Inf,      ymax = Inf,
                      fill = "grey", alpha = 0.3) +

    ggplot2::annotate("text", x = 6  * 3600, y = max_y * 1.05, label = "Light Cycle",
                      color = "black", size = 4, vjust = 1) +
    ggplot2::annotate("text", x = 18 * 3600, y = max_y * 1.05, label = "Dark Cycle",
                      color = "black", size = 4, vjust = 1) +

    ggplot2::scale_fill_manual(values  = color_values) +
    ggplot2::scale_color_manual(values = color_values) +

    ggplot2::labs(title = title,
                  x     = "Time (Hours)",
                  y     = "Activity per 10 minutes",
                  color = "Labels",
                  fill  = "Labels") +

    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      axis.title.x     = ggplot2::element_text(size = 14, face = "bold"),
      axis.title.y     = ggplot2::element_text(size = 14, face = "bold"),
      axis.text        = ggplot2::element_text(size = 12),
      legend.position  = "right",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(linewidth = 0.5, color = "black"),
      panel.border     = ggplot2::element_blank(),
      legend.title     = ggplot2::element_blank()
    )
}


# -- Save plot as PDF ------------------------------------------------
save_plot <- function(plot, filename, width_px = 7200, height_px = 3600) {
  ggplot2::ggsave(filename, plot = plot,
                  width = width_px, height = height_px,
                  units = "px", dpi = 600)
  message("Saved: ", filename)
}


# -- Filter to a time window and re-zero the time axis ---------------
filter_by_time <- function(data, after_sec = NULL, before_sec = NULL) {
  if (is.null(after_sec))  after_sec  <- min(data$Time_min, na.rm = TRUE)
  if (is.null(before_sec)) before_sec <- max(data$Time_min, na.rm = TRUE)

  data %>%
    dplyr::filter(Time_min >= after_sec & Time_min <= before_sec) %>%
    dplyr::mutate(Time_min = Time_min - after_sec)
}
