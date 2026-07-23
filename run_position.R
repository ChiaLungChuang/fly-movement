# ============================================================
# run_position.R  —  DRIVER: DAM position + sleep pipeline
#
# Edit the CONFIG block, then source() this whole file.
# It loads the function library in R/ (in dependency order),
# runs the pipeline, and writes plots + an Excel workbook to output/.
#
# Produces in the environment (used by run_activity.R):
#   dat, dead_summary, unknown_diag, anchors
# ============================================================


# ============================================================
# CONFIG  —  edit this block only
# ============================================================

# Folder that contains R/, data/, output/  (this project folder)
project_dir <- "/Users/cchuang/Desktop/Fly movement"

# The three monitors that watch the SAME set of vials at different
# positions along the tube (plug end / middle / food end).
monitor_files <- c(
  plug   = file.path(project_dir, "data", "Monitor7.txt"),
  middle = file.path(project_dir, "data", "Monitor8.txt"),
  food   = file.path(project_dir, "data", "Monitor9.txt")
)

group_label    <- "hTauV337M 50D"
channels       <- 1:32
start_datetime <- "2026-01-29 05:00:00"
end_datetime   <- "2026-01-30 04:59:00"

death_threshold_hours <- 12          # continuous inactivity -> DEAD
sleep_threshold_min   <- 5           # inactivity minutes -> sleep bout
look_window           <- 5           # anchor look window (API compat)

lights_on  <- "05:00:00"             # ZT0
lights_off <- "17:00:00"             # ZT12

# TRUE  = visualize the anchor method   (zone_anchored / sleep_zone_anchored)
# FALSE = visualize the state-machine method (zone / sleep_zone)
use_anchored <- TRUE

output_dir <- file.path(project_dir, "output")


# ============================================================
# LIBRARIES + FUNCTION LIBRARY  (do not edit below)
# ============================================================

library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(openxlsx)

r_dir <- file.path(project_dir, "R")
source(file.path(r_dir, "01_read_qc.R"))        # read + dead-channel QC
source(file.path(r_dir, "02_position_zones.R")) # state machine + sleep
source(file.path(r_dir, "03_anchor.R"))         # anchor-based position
source(file.path(r_dir, "04_plots.R"))          # plots (use_anchored flag)
source(file.path(r_dir, "05_export.R"))         # Excel export

group_slug <- gsub(" ", "_", group_label)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
out <- function(suffix) file.path(output_dir, paste0(group_slug, suffix))


# ============================================================
# PIPELINE
# ============================================================

# 1-3: read, detect dead, remove dead
monitors     <- read_position_monitors(monitor_files, channels,
                                        start_datetime, end_datetime)
dead_summary <- detect_dead_channels(monitors, death_threshold_hours)
monitors     <- remove_dead_channels(monitors, dead_summary)

# 4 + 4.5: state-machine zones, backfill startup UNKNOWN, sleep bouts
result       <- assign_position_zones(monitors)
result       <- backfill_and_detect_sleep(result, sleep_threshold_min)
dat          <- result$dat
unknown_diag <- result$unknown_diag

# 4A: anchor-based position + sleep-zone + transit classification
anchor_result <- assign_position_anchored(dat, sleep_threshold_min, look_window)
dat     <- anchor_result$dat
anchors <- anchor_result$anchors

# Optional diagnostics:
#   print_anchor_timeline(dat, channel_num = 5)
#   dat[transit_type == "TRANSIT_INCONSISTENT"]
#   anchors[confidence == "ambiguous"]


# ============================================================
# PLOTS
# ============================================================

plot_individual_timelines(
  dat,
  use_anchored = use_anchored,
  lights_on    = lights_on,
  lights_off   = lights_off,
  output_file  = out("_position_timelines.pdf")
)

plot_sleep_zone_heatmap(
  dat,
  use_anchored = use_anchored,
  group_label  = group_label,
  output_file  = out("_sleep_zone_heatmap.pdf")
)

plot_sleep_zone_summary(
  dat,
  use_anchored = use_anchored,
  group_label  = group_label,
  output_file  = out("_sleep_zone_summary.pdf")
)

plot_sleep_zone_summary_heatmap(
  dat,
  use_anchored = use_anchored,
  group_label  = group_label,
  output_file  = out("_sleep_zone_summary_heatmap.pdf")
)


# ============================================================
# EXCEL
# ============================================================

export_results_excel(
  dat          = dat,
  dead_summary = dead_summary,
  unknown_diag = unknown_diag,
  anchors      = anchors,
  group_label  = group_label,
  output_file  = out("_results.xlsx")
)

cat("=== Position pipeline complete ===\n")
