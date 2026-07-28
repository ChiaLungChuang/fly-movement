#!/usr/bin/env Rscript
# ============================================================
# tools/vendor_diff.R -- staleness check for the vendored alignment engine
#
# WHAT THIS IS FOR
#
# R/00_dam_align.R is a vendored, ADAPTED copy of the damalignr package
# (github.com/ChiaLungChuang/damalignr). Because it was adapted rather than
# copied verbatim, `diff` against upstream is useless -- it reports the
# deliberate changes and the real drift with equal weight. This script
# compares the two the way the provenance header claims they were compared:
# function by function, on parsed-and-deparsed bodies, so whitespace,
# comments and formatting differences do not register.
#
# It is a maintenance tool, NOT part of the test suite. It needs a local
# clone of damalignr, which will not exist in CI or on another machine, so
# wiring it into tests/ would just produce a test that skips everywhere.
#
# HOW TO RUN
#
#   Rscript dam-shiny/tools/vendor_diff.R /path/to/damalignr
#   DAMALIGNR_DIR=/path/to/damalignr Rscript dam-shiny/tools/vendor_diff.R
#
# The upstream path comes from the first argument, or from DAMALIGNR_DIR if
# no argument is given. There is no default -- the path is machine-specific
# and hardcoding one would make this work here and nowhere else. Point it at
# the repo root (the directory holding R/ and DESCRIPTION), not at R/.
#
# The vendored file is located relative to this script, so it can be run
# from any working directory.
#
# HOW TO READ THE OUTPUT
#
# Every vendored and upstream definition is placed in exactly one bucket:
#
#   IDENTICAL      body matches upstream exactly. These are the ones this
#                  script polices -- see the exit status below.
#   ADAPTED        deliberately changed. A unified diff of the deparsed
#                  bodies is printed so the change can be eyeballed. A diff
#                  that has GROWN since the header was written is the signal
#                  to look closer; this script cannot tell an old adaptation
#                  from new upstream drift, because both show up as "not
#                  identical". Read it against the per-function notes in
#                  R/00_dam_align.R's provenance header.
#   VENDORED-ONLY  written for this app; no upstream counterpart to drift.
#   UPSTREAM-ONLY  present upstream, not vendored (the print/write/CLI layer).
#
# EXIT STATUS
#
#   0  every function expected to be identical still is
#   1  something expected to be identical has DRIFTED, or a mapped function
#      has disappeared from either side -- i.e. the header is now wrong
#   2  bad usage (no path given, or the path is not a damalignr checkout)
#
# A non-zero exit means R/00_dam_align.R's provenance header no longer
# describes reality. Re-verify, then update the header and EXPECTED_IDENTICAL
# below in the same commit.
# ============================================================

# --- functions expected to be byte-identical to upstream ----------------
# Keep in sync with the "identical to upstream" block of the provenance
# header in R/00_dam_align.R. Moving a name out of here is a deliberate
# act: it means an adaptation was introduced on purpose.
EXPECTED_IDENTICAL <- c(
  .dam_read_raw     = "read_dam_raw",
  .dam_subrow_order = ".subrow_order",
  "%||%"            = "%||%"
)

# --- functions deliberately adapted from an upstream counterpart --------
EXPECTED_ADAPTED <- c(
  .dam_subrow_mismatches      = ".diagnose_subrow_mismatches",
  .dam_first_misalignment     = ".diagnose_alignment",
  .dam_trim_to_common_end     = "trim_to_common_end",
  .dam_nearest_template       = ".nearest_template",
  .dam_match_final            = "match_final_timestamp_rows",
  .dam_filter_start           = "filter_start",
  .dam_repair_missing_subrows = "repair_missing_subrows"
)

MAP <- c(EXPECTED_IDENTICAL, EXPECTED_ADAPTED)


# --- locate inputs ------------------------------------------------------

# Path to this script, so the vendored file can be found from any cwd.
# Rscript encodes spaces in --file= as "~+~", which matters here: this repo
# lives under a path containing a space.
this_script <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (!length(f)) return("")
  normalizePath(gsub("~+~", " ", f[1], fixed = TRUE), mustWork = FALSE)
}

die <- function(status, ...) {
  message(...)
  quit(status = status, save = "no")
}

args <- commandArgs(trailingOnly = TRUE)
up <- if (length(args) >= 1 && nzchar(args[1])) args[1] else Sys.getenv("DAMALIGNR_DIR", "")

if (!nzchar(up)) {
  die(2, "usage: Rscript vendor_diff.R /path/to/damalignr\n",
         "   or: DAMALIGNR_DIR=/path/to/damalignr Rscript vendor_diff.R\n",
         "No default path: it is machine-specific by nature.")
}
up <- normalizePath(up, mustWork = FALSE)
if (!dir.exists(file.path(up, "R"))) {
  die(2, sprintf("Not a damalignr checkout (no R/ directory): %s\n", up),
         "Point this at the repo root, not at R/.")
}

script_dir <- dirname(this_script())
vendored <- normalizePath(file.path(script_dir, "..", "R", "00_dam_align.R"),
                          mustWork = FALSE)
if (!file.exists(vendored)) {
  die(2, sprintf("Cannot find the vendored file at %s", vendored))
}


# --- extract function definitions --------------------------------------
# Parse rather than source: no package code is executed, and the upstream
# clone is only ever read.
defs <- function(files) {
  out <- list()
  for (f in files) {
    for (e in parse(f)) {
      if (!is.call(e) || length(e) != 3) next
      op <- e[[1]]
      if (!is.symbol(op) || !(as.character(op) %in% c("<-", "="))) next
      lhs <- e[[2]]
      nm <- if (is.symbol(lhs)) as.character(lhs)
            else if (is.character(lhs)) lhs
            else next
      rhs <- e[[3]]
      if (is.call(rhs) && is.symbol(rhs[[1]]) &&
          identical(as.character(rhs[[1]]), "function")) {
        out[[nm]] <- paste(deparse(eval(rhs)), collapse = "\n")
      }
    }
  }
  out
}

V <- defs(vendored)
P <- defs(list.files(file.path(up, "R"), pattern = "[.]R$", full.names = TRUE))

upstream_sha <- tryCatch({
  s <- suppressWarnings(system2("git", c("-C", shQuote(up), "rev-parse", "HEAD"),
                                stdout = TRUE, stderr = FALSE))
  if (length(s)) s[1] else NA_character_
}, error = function(e) NA_character_)

cat("vendored : ", vendored, "\n", sep = "")
cat("upstream : ", up, "\n", sep = "")
cat("upstream HEAD: ", if (is.na(upstream_sha)) "(not a git repo)" else upstream_sha,
    "\n\n", sep = "")


# --- compare ------------------------------------------------------------

unified_diff <- function(a, b) {
  t1 <- tempfile(); t2 <- tempfile()
  on.exit(unlink(c(t1, t2)), add = TRUE)
  writeLines(strsplit(a, "\n")[[1]], t1)
  writeLines(strsplit(b, "\n")[[1]], t2)
  suppressWarnings(system2("diff", c("-u", shQuote(t1), shQuote(t2)),
                           stdout = TRUE, stderr = FALSE))[-(1:2)]
}

problems <- character(0)

cat("IDENTICAL -- expected to match upstream exactly\n")
for (v in names(EXPECTED_IDENTICAL)) {
  u <- EXPECTED_IDENTICAL[[v]]
  if (is.null(V[[v]]) || is.null(P[[u]])) {
    cat(sprintf("  %-28s <- %-28s MISSING (%s)\n", v, u,
                if (is.null(V[[v]])) "not in vendored file" else "not in upstream"))
    problems <- c(problems, sprintf("%s: mapped function missing", v))
    next
  }
  if (identical(V[[v]], P[[u]])) {
    cat(sprintf("  %-28s <- %-28s ok\n", v, u))
  } else {
    cat(sprintf("  %-28s <- %-28s DRIFTED\n", v, u))
    cat(paste0("      ", unified_diff(P[[u]], V[[v]])), sep = "\n")
    cat("\n")
    problems <- c(problems, sprintf("%s: no longer identical to %s", v, u))
  }
}

cat("\nADAPTED -- deliberate changes; read against the provenance header\n")
for (v in names(EXPECTED_ADAPTED)) {
  u <- EXPECTED_ADAPTED[[v]]
  if (is.null(V[[v]]) || is.null(P[[u]])) {
    cat(sprintf("  %-28s <- %-28s MISSING (%s)\n", v, u,
                if (is.null(V[[v]])) "not in vendored file" else "not in upstream"))
    problems <- c(problems, sprintf("%s: mapped function missing", v))
    next
  }
  if (identical(V[[v]], P[[u]])) {
    # Not a failure: upstream may simply have adopted the same change.
    cat(sprintf("  %-28s <- %-28s now identical (adaptation gone?)\n", v, u))
  } else {
    d <- unified_diff(P[[u]], V[[v]])
    cat(sprintf("  %-28s <- %-28s adapted (%d diff lines)\n", v, u, length(d)))
    cat(paste0("      ", d), sep = "\n")
    cat("\n")
  }
}

vendored_only <- setdiff(names(V), names(MAP))
upstream_only <- setdiff(names(P), unname(MAP))

cat("\nVENDORED-ONLY -- written for this app, nothing upstream to drift\n")
cat("  ", paste(vendored_only, collapse = ", "), "\n", sep = "")
cat("\nUPSTREAM-ONLY -- present upstream, not vendored\n")
cat("  ", paste(upstream_only, collapse = ", "), "\n", sep = "")

cat(sprintf("\n%d vendored / %d upstream definitions; %d mapped.\n",
            length(V), length(P), length(MAP)))

if (length(problems)) {
  cat("\nFAILED -- R/00_dam_align.R's provenance header is now out of date:\n")
  cat(paste0("  - ", problems), sep = "\n")
  cat("\nRe-verify, then update the header and EXPECTED_IDENTICAL together.\n")
  quit(status = 1, save = "no")
}

cat("\nOK -- everything expected to be identical still is.\n")
quit(status = 0, save = "no")
