# Source the engine + app helpers once for the whole test run.
# Tests run against the same functions the app auto-sources from R/.

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(openxlsx)
  library(readxl)
  library(dplyr)
  library(rlang)
})

# Locate the app root (works from tests/testthat/, tests/, or app root).
.find_app_root <- function() {
  cand <- c(".", "..", "../..")
  for (d in cand) {
    if (dir.exists(file.path(d, "R")) &&
        file.exists(file.path(d, "R", "01_read_qc.R"))) {
      return(normalizePath(d))
    }
  }
  stop("Could not locate dam-shiny app root (R/ with engine files).")
}

app_root <- .find_app_root()

# Source engine files (skip UI/server modules that need shiny at load time is
# unnecessary — they only define functions — but shiny may not be attached, so
# source only the engine + state helpers the tests exercise).
engine_files <- c("00_dam_align.R", "01_read_qc.R", "02_position_zones.R",
                  "03_anchor.R", "04_plots.R", "05_export.R", "06_chronogram.R",
                  "07_activity.R", "app_config.R")
for (f in engine_files) source(file.path(app_root, "R", f))

# parse_channels lives in mod_group_manager.R alongside shiny UI code; pull just
# that helper in without attaching shiny by sourcing in a guarded way.
if (requireNamespace("shiny", quietly = TRUE)) {
  suppressPackageStartupMessages(library(shiny))
  source(file.path(app_root, "R", "utils_state.R"))
  # Module files define functions only; their UI halves reference
  # shinydashboard::box() but nothing runs at source() time.
  for (m in c("mod_group_manager.R", "mod_position_sleep.R", "mod_downloads.R")) {
    source(file.path(app_root, "R", m))
  }
}

# Path to bundled sample monitors.
# Slot assignment follows the rig convention: within an apparatus the monitors
# run food, central, plug in file order, so for MonitorN the role is
# (N - 1) mod 3 -- 0 food, 1 central, 2 plug. Monitor7 is therefore the FOOD
# end and Monitor9 the PLUG end. This was transposed until 2026-08-04; the
# picker order in the app reads Plug -> Middle -> Food, which is the reverse of
# file order, and the fixtures inherited that mistake.
sample_paths <- list(
  plug   = file.path(app_root, "inst", "extdata", "Monitor9.txt"),
  middle = file.path(app_root, "inst", "extdata", "Monitor8.txt"),
  food   = file.path(app_root, "inst", "extdata", "Monitor7.txt")
)

# Synthetic staggered-end monitor set: three Case-A files starting together,
# the middle one genuinely stopping an hour early. Alignment trims all three
# to the common end, so the offered period must be the trimmed window, not the
# longest file's extent. The bundled samples can't cover this — they share a
# start and an end, so intersected and widest ranges coincide there.
# Day is fixed ("3 Jun 26") and the span stays inside it, so no locale-
# dependent month formatting is needed.
make_staggered_monitors <- function(n_long = 200L, n_short = 140L) {
  t0 <- as.POSIXct("2026-06-03 16:36:00", tz = "UTC")
  mk <- function(path, n) {
    ts <- t0 + (0:(n - 1)) * 60
    writeLines(sprintf("%d\t3 Jun 26\t%s\t1\t0\t7\t0\tCt\t0\t0\t%s",
                       seq_len(n),
                       format(ts, "%H:%M:%S", tz = "UTC"),
                       paste(rep(0, 32), collapse = "\t")),
               path)
  }
  p <- c(plug = tempfile(fileext = ".txt"),
         middle = tempfile(fileext = ".txt"),
         food = tempfile(fileext = ".txt"))
  mk(p[["plug"]], n_long); mk(p[["middle"]], n_short); mk(p[["food"]], n_long)
  p
}
