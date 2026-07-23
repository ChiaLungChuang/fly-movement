# ============================================================
# run_chronogram.R  —  DRIVER: Activity chronogram
#
# Two modes (set `mode` in CONFIG):
#
#   "pipeline"  — build the chronogram from THIS pipeline's `dat`
#                 (no Rtivity needed). Sources run_position.R if `dat`
#                 is not already in the session.
#
#   "rtivity"   — read Rtivity's "Activity Chronogram.xlsx" output
#                 (the original multi-group / multi-age workflow).
#
# One chronogram per run. To compare ages (10D/30D/50D), copy the
# CONFIG + RUN block, or see the commented examples at the bottom.
# ============================================================


# ============================================================
# CONFIG  —  edit this block only
# ============================================================

project_dir <- "/Users/cchuang/Desktop/Fly movement"

mode <- "pipeline"        # "pipeline"  or  "rtivity"

output_dir <- file.path(project_dir, "output", "Chronogram")

# ---- shared plotting ----
title        <- "Activity Over Time - hTauV337M 50D"
lights_on_h  <- 12        # ZT hour lights turn ON  (dark -> light)
lights_off_h <- 24        # ZT hour lights turn OFF (light -> dark)

# ---- mode = "pipeline" settings ----
group_label     <- "hTauV337M 50D"
chrono_monitor  <- "summed"   # plug | middle | food | summed
bin_min         <- 10
# one color per Labels value present in the data
color_values    <- c("hTauV337M 50D" = "#0072B2")

# ---- mode = "rtivity" settings ----
excel_file    <- "/Users/cchuang/Desktop/Locomotion activity monitor data/hTauV337M_Mhc_CG8093/Sleep/Retest/with isogenic control/50D/Male/Modified script/Activity Chronogram.xlsx"
target_sheets <- c("iso male", "CG8093 male")
label_map     <- c("iso male" = "isogenic hTauV337M", "CG8093 male" = "hTauV337M, CG8093")
rtivity_colors <- c("isogenic hTauV337M" = "#0072B2", "hTauV337M, CG8093" = "#FFA500")
# dead-channel exclusions (File -> Labels -> channels)
exclusions <- list(
  "Sleep 1st 50D Control CG8093.txt" = list("Control" = c(10)),
  "Sleep 2nd 50D Control CG8093.txt" = list("Control" = c(7),  "CG8093" = c(7, 9, 10, 14)),
  "Sleep 3rd 50D Control CG8093.txt" = list("Control" = c(11), "CG8093" = c(5, 11, 14))
)


# ============================================================
# LIBRARIES + FUNCTION LIBRARY  (do not edit below)
# ============================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(rlang)
library(data.table)

source(file.path(project_dir, "R", "06_chronogram.R"))

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# ============================================================
# BUILD CHRONOGRAM-FORMAT DATA
# ============================================================

if (mode == "pipeline") {

  if (!exists("dat")) {
    cat("  `dat` not found — sourcing run_position.R ...\n")
    source(file.path(project_dir, "run_position.R"))
  }

  chrono_long <- make_activity_chronogram(
    dat, group_label = group_label,
    monitor = chrono_monitor, bin_min = bin_min
  )
  plot_colors <- color_values

} else if (mode == "rtivity") {

  cat(sprintf("  Sheets in file: %s\n",
              paste(excel_sheets(excel_file), collapse = ", ")))
  chrono_long <- read_activity_sheets(excel_file, target_sheets, label_map)
  chrono_long <- exclude_channels(chrono_long, exclusions)
  plot_colors <- rtivity_colors

} else {
  stop("mode must be 'pipeline' or 'rtivity'")
}


# ============================================================
# QC + SUMMARIZE + PLOT
# ============================================================

chrono_long %>%
  dplyr::group_by(File, Labels) %>%
  dplyr::summarize(channel_count = dplyr::n_distinct(Channels), .groups = "drop") %>%
  print()

summary_activity <- summarize_activity(chrono_long)
summary_activity <- summary_activity %>%
  dplyr::filter(!is.na(Labels) & !is.na(Time_min))

chrono_plot <- plot_activity_chronogram(
  summary_activity,
  plot_colors,
  title        = title,
  lights_on_h  = lights_on_h,
  lights_off_h = lights_off_h
)

save_plot(chrono_plot,
          file.path(output_dir, "Activity_Chronogram_full.pdf"))

write.csv(summary_activity,
          file.path(output_dir, "Activity_Chronogram_full_PlottingData.csv"),
          row.names = FALSE)


# ---- Optional: single day (Day 2 = 24h..48h, seconds) ----
summary_day2 <- filter_by_time(summary_activity,
                               after_sec = 86400, before_sec = 172800)
if (nrow(summary_day2) > 0) {
  chrono_plot_day2 <- plot_activity_chronogram(
    summary_day2, plot_colors,
    title = paste0(title, " (Day 2)"),
    lights_on_h = 12, lights_off_h = 24
  )
  save_plot(chrono_plot_day2,
            file.path(output_dir, "Activity_Chronogram_day2.pdf"))
  write.csv(summary_day2,
            file.path(output_dir, "Activity_Chronogram_day2_PlottingData.csv"),
            row.names = FALSE)
}

cat("=== Chronogram complete ===\n")


# ============================================================
# EXAMPLE: comparing ages via the "rtivity" mode
#
# The original workflow read a separate Rtivity file per age. To
# reproduce it, set mode <- "rtivity" and swap the CONFIG block per
# age, e.g.:
#
#   # 10D female (2 groups)
#   excel_file    <- ".../10D/1st/Modified script/Full chronogram.xlsx"
#   target_sheets <- c("iso female", "CG8093 female")
#   label_map     <- c("iso female"   = "isogenic hTauV337M",
#                      "CG8093 female" = "hTauV337M, CG8093")
#   rtivity_colors <- c("isogenic hTauV337M" = "#0072B2",
#                       "hTauV337M, CG8093"  = "#FFA500")
#   exclusions <- list(
#     "Sleep 10D control.txt"   = list("Control"   = c(10, 28)),
#     "Sleep 10D hTauV337M.txt" = list("hTauV337M" = c(21, 22, 23, 24)))
#
#   # 30D female — target_sheets/exclusions per your 30D file, etc.
# ============================================================
