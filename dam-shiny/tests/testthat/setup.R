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
engine_files <- c("01_read_qc.R", "02_position_zones.R", "03_anchor.R",
                  "04_plots.R", "05_export.R", "06_chronogram.R",
                  "07_activity.R", "app_config.R")
for (f in engine_files) source(file.path(app_root, "R", f))

# parse_channels lives in mod_group_manager.R alongside shiny UI code; pull just
# that helper in without attaching shiny by sourcing in a guarded way.
if (requireNamespace("shiny", quietly = TRUE)) {
  suppressPackageStartupMessages(library(shiny))
  source(file.path(app_root, "R", "utils_state.R"))
  source(file.path(app_root, "R", "mod_group_manager.R"))
}

# Path to bundled sample monitors.
sample_paths <- list(
  plug   = file.path(app_root, "inst", "extdata", "Monitor7.txt"),
  middle = file.path(app_root, "inst", "extdata", "Monitor8.txt"),
  food   = file.path(app_root, "inst", "extdata", "Monitor9.txt")
)
