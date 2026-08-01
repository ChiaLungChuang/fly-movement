#!/usr/bin/env Rscript
# ============================================================
# tools/engine_copy_diff.R -- drift check for duplicated engine files
#
# WHAT THIS IS FOR
#
# This repo carries two live copies of part of the analysis engine:
#
#   R/<file>            sourced by run_position.R (the standalone script)
#   dam-shiny/R/<file>  sourced by the Shiny app
#
# They are duplicated, not shared. Nothing makes an edit to one reach the
# other, so a change applied to a single copy makes the app and the script
# produce different results from identical input, silently -- which is exactly
# what nearly happened when export_results_excel() gained its use_anchored
# parameter. This script fails when a file that is meant to agree no longer
# does.
#
# WHAT IS IN THE SET, AND WHAT IS NOT
#
# Only files BOTH copies are meant to keep identical are checked. The rest of
# root R/ has deliberately forked for the app's benefit and must NOT be synced:
#
#   01_read_qc.R  app adds a tz parameter and source_names logging
#   04_plots.R    app adds output_file = NULL support for return-only plots
#
# Those are listed as FORKED below so their absence from the checked set is a
# recorded decision rather than an oversight. Three further copies exist
# outside this repo -- an archived monolith in _original_backup/, a separate
# project under ~/Desktop/DAM position/, and the superseded staging repo
# dam-shiny-integration -- none of which are meant to track this one. See the
# provenance discussion in the commit that introduced this file.
#
# De-duplicating properly is a separate ticket. Until then this is the guard.
#
# HOW TO RUN
#
#   Rscript dam-shiny/tools/engine_copy_diff.R
#   Rscript dam-shiny/tools/engine_copy_diff.R /path/to/repo/root
#
# The repo root defaults to the parent of this script's parent, so it works
# from any working directory. Kept out of the test suite deliberately: it is a
# repo-layout invariant, not a property of the app's behaviour, and the test
# suite runs against dam-shiny/ alone.
#
# EXIT STATUS
#
#   0  every file in the checked set is identical across both copies
#   1  drift -- a checked file differs, or is missing from one side
#   2  bad usage (repo root not found, or not laid out as expected)
# ============================================================

# Files both copies must keep byte-identical.
SHARED <- c("02_position_zones.R", "03_anchor.R", "05_export.R",
            "06_chronogram.R")

# Deliberately divergent: app-only capability the script does not have.
FORKED <- c("01_read_qc.R" = "app adds tz + source_names",
            "04_plots.R"   = "app adds output_file = NULL (return-only plots)")

this_script <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (!length(f)) return("")
  # Rscript encodes spaces in --file= as "~+~"; this repo's path contains one.
  normalizePath(gsub("~+~", " ", f[1], fixed = TRUE), mustWork = FALSE)
}

die <- function(status, ...) { message(...); quit(status = status, save = "no") }

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1 && nzchar(args[1])) args[1] else {
  sd <- dirname(this_script())                       # dam-shiny/tools
  if (!nzchar(sd)) "." else file.path(sd, "..", "..")  # -> repo root
}
root <- normalizePath(root, mustWork = FALSE)

script_dir <- file.path(root, "R")
app_dir    <- file.path(root, "dam-shiny", "R")
if (!dir.exists(script_dir) || !dir.exists(app_dir)) {
  die(2, sprintf("Expected both %s and %s to exist.\n", script_dir, app_dir),
         "Pass the repo root as the first argument.")
}

cat("repo root : ", root, "\n", sep = "")
cat("comparing : R/  <->  dam-shiny/R/\n\n")

drift <- character(0)

cat("CHECKED -- must be byte-identical\n")
for (f in SHARED) {
  a <- file.path(script_dir, f); b <- file.path(app_dir, f)
  if (!file.exists(a) || !file.exists(b)) {
    cat(sprintf("  %-22s MISSING (%s)\n", f,
                if (!file.exists(a)) "not in R/" else "not in dam-shiny/R/"))
    drift <- c(drift, sprintf("%s: missing from one copy", f))
    next
  }
  same <- identical(readLines(a, warn = FALSE), readLines(b, warn = FALSE))
  cat(sprintf("  %-22s %s\n", f, if (same) "ok" else "DRIFTED"))
  if (!same) {
    d <- suppressWarnings(system2("diff", c("-u", shQuote(a), shQuote(b)),
                                  stdout = TRUE, stderr = FALSE))
    cat(paste0("      ", utils::head(d[-(1:2)], 40)), sep = "\n")
    if (length(d) > 42) cat("      ... (", length(d) - 42, " more lines)\n", sep = "")
    cat("\n")
    drift <- c(drift, sprintf("%s: copies differ", f))
  }
}

cat("\nFORKED -- divergence is intended, not checked\n")
for (f in names(FORKED)) cat(sprintf("  %-22s %s\n", f, FORKED[[f]]))

extra <- setdiff(list.files(script_dir, pattern = "[.]R$"),
                 c(SHARED, names(FORKED)))
if (length(extra)) {
  cat("\nUNCLASSIFIED -- present in R/ but in neither list; classify these\n")
  cat("  ", paste(extra, collapse = ", "), "\n", sep = "")
  drift <- c(drift, sprintf("unclassified file(s): %s",
                            paste(extra, collapse = ", ")))
}

if (length(drift)) {
  cat("\nFAILED -- the duplicated engine copies have diverged:\n")
  cat(paste0("  - ", drift), sep = "\n")
  cat("\nApply the change to both copies, or move the file to FORKED with a",
      "\nreason if the divergence is deliberate.\n")
  quit(status = 1, save = "no")
}

cat("\nOK -- every shared engine file is identical across both copies.\n")
quit(status = 0, save = "no")
