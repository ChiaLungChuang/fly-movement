# ============================================================
# app.R  —  DAM Position + Sleep Shiny app (Phase 0 scaffold)
#
# Shiny auto-sources every file in ./R at startup, so the analysis
# engine (01_read_qc.R … 06_chronogram.R) AND the UI/module files
# (app_ui.R, app_server.R, mod_*.R, utils_state.R) are all loaded
# automatically — no manual source() calls needed here.
#
# Run:  from the dam-shiny/ folder, click "Run App" in RStudio,
#       or  shiny::runApp("dam-shiny")
# ============================================================

library(shiny)
library(shinydashboard)   # swap to bs4Dash later if you prefer
library(DT)

# Engine libraries (loaded here so the engine functions can run when
# the modules call them in later phases; harmless in Phase 0).
suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(openxlsx)
  library(readxl)
  library(dplyr)
  library(rlang)
  library(zip)
  library(future)
  library(promises)
})

# Group runs execute in background R processes so the UI stays responsive
# (see mod_group_manager.R). Workers are capped to leave a core for the app.
future::plan(future::multisession,
             workers = max(1L, min(4L, future::availableCores() - 1L)))

# Upload limit from config.yml (default 250 MB). app_config() is defined in
# R/app_config.R, which Shiny auto-sources before app.R runs.
options(shiny.maxRequestSize = app_config()$max_upload_mb * 1024^2)

shinyApp(ui = app_ui(), server = app_server)
