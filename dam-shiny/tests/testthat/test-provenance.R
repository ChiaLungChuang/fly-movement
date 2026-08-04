# Provenance: every exported artefact must record how it was made.
#
# The alignment report lived only in rv$log -- shared across groups, clearable
# by a button, and gone at session end -- while the repaired data it described
# travelled on into figures. And nothing in a bundle recorded the zone method,
# the source files, or whether alignment had run at all.
#
# Absence must never need interpreting: the per-group file is written for EVERY
# done group, and an unaligned group gets an explicit "not required" line
# rather than a missing file.

.pgroup <- function(gid, label, use_anchored = TRUE, aligned = FALSE,
                    death = 12, sleep_thr = 5, lights_on = "05:00:00") {
  g <- new_group(gid, label, "#0072B2")
  g$status <- "done"
  g$config$use_anchored          <- use_anchored
  g$config$death_threshold_hours <- death
  g$config$sleep_threshold_min   <- sleep_thr
  g$config$lights_on             <- lights_on
  g$monitor_names <- list(plug = "Monitor9.txt", middle = "Monitor8.txt",
                          food = "Monitor7.txt")
  g$monitor_paths <- list(plug = "/tmp/0.txt", middle = "/tmp/1.txt",
                          food = "/tmp/2.txt")
  g$aligned <- aligned
  if (aligned) g$alignment_report <- c("middle: 1 blank line at line 4570",
                                       "Repaired 1 dropped sub-row(s) in place.")
  g$dat <- data.table::data.table(
    channel = 1L, datetime = as.POSIXct("2026-01-28 15:12:00", tz = "UTC"))
  g
}

test_that("a group's provenance records method, files and alignment", {
  skip_if_not(exists("group_provenance_df"), "helper not defined")

  rows <- group_provenance_df(.pgroup("g1", "A", use_anchored = FALSE,
                                        aligned = TRUE))
  val <- function(p) rows$Value[rows$Parameter == p]

  # Real upload names, not the Shiny datapaths.
  expect_match(val("Source files"), "Monitor7.txt", fixed = TRUE)
  expect_false(grepl("0.txt", val("Source files"), fixed = TRUE))

  # Alignment status is explicit, and the report travels with it.
  expect_match(val("Alignment"), "aligned", ignore.case = TRUE)
  expect_true(any(grepl("dropped sub-row", rows$Value, fixed = TRUE)))
})

test_that("an unaligned group says so explicitly rather than saying nothing", {
  skip_if_not(exists("group_provenance_df"), "helper not defined")

  rows <- group_provenance_df(.pgroup("g1", "A", aligned = FALSE))
  al <- rows$Value[rows$Parameter == "Alignment"]
  expect_length(al, 1)
  expect_match(al, "not required", fixed = TRUE)
})

test_that("the zone method actually used is recorded, both ways round", {
  skip_if_not(exists("group_provenance_df"), "helper not defined")

  a <- group_provenance_df(.pgroup("g1", "A", use_anchored = TRUE))
  m <- group_provenance_df(.pgroup("g1", "A", use_anchored = FALSE))
  expect_equal(a$Value[a$Parameter == "Zone method"], "anchor")
  expect_equal(m$Value[m$Parameter == "Zone method"], "state machine")
})

test_that("identical groups report no divergence", {
  skip_if_not(exists("bundle_divergence"), "helper not defined")

  gs <- list(a = .pgroup("a", "A"), b = .pgroup("b", "B"))
  d <- bundle_divergence(gs)
  expect_false(d$any)
  expect_equal(nrow(d$table), 0L)
})

test_that("divergence is reported per parameter with each group's value", {
  skip_if_not(exists("bundle_divergence"), "helper not defined")

  gs <- list(a = .pgroup("a", "A", use_anchored = TRUE,  death = 12),
             b = .pgroup("b", "B", use_anchored = FALSE, death = 24))
  d <- bundle_divergence(gs)
  expect_true(d$any)

  # Per parameter, not a single "groups differ" flag: a bundle differing only
  # in lights_on is a different problem from one differing in zone method.
  expect_true("Zone method" %in% d$table$Parameter)
  expect_true("Death threshold (h)" %in% d$table$Parameter)
  expect_false("Lights on" %in% d$table$Parameter)   # this one agrees

  # Each group's own value is shown, not just the fact of disagreement.
  zm <- d$table[d$table$Parameter == "Zone method", ]
  expect_equal(zm$A, "anchor")
  expect_equal(zm$B, "state machine")
})

test_that("alignment status counts as divergence", {
  # A bundle mixing repaired and pristine data is a scientific divergence and
  # would fall straight through a config-only comparison.
  skip_if_not(exists("bundle_divergence"), "helper not defined")

  gs <- list(a = .pgroup("a", "A", aligned = TRUE),
             b = .pgroup("b", "B", aligned = FALSE))
  d <- bundle_divergence(gs)
  expect_true(d$any)
  expect_true("Alignment" %in% d$table$Parameter)
})

test_that("the bundle-root record is written either way", {
  skip_if_not(exists("bundle_provenance_text"), "helper not defined")

  same <- bundle_provenance_text(list(a = .pgroup("a", "A"),
                                      b = .pgroup("b", "B")))
  expect_true(any(grepl("identical analysis parameters", same, fixed = TRUE)))

  diff <- bundle_provenance_text(list(a = .pgroup("a", "A", use_anchored = TRUE),
                                      b = .pgroup("b", "B", use_anchored = FALSE)))
  expect_true(any(grepl("DIFFER", diff)))
  expect_true(any(grepl("Zone method", diff, fixed = TRUE)))
  # The warning has to be findable, not buried at the end.
  expect_lt(min(grep("DIFFER", diff)), 10L)
})

# -- wiring pins -------------------------------------------------------------
# The bundle's content function renders four PDFs and nine workbooks per group,
# which is too heavy to invoke in a unit test. The record's CONTENT is covered
# by the pure-function tests above; these assert it is actually reached, the
# same instrument used to pin export_results_excel()'s call site.

.downloads_src <- function() {
  f <- file.path(app_root, "R", "mod_downloads.R")
  skip_if_not(file.exists(f), "module source missing")
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

test_that("the bundle writes a root provenance record", {
  txt <- .downloads_src()
  expect_match(txt, "bundle_provenance_text(gs)", fixed = TRUE)
  expect_match(txt, "PROVENANCE.txt", fixed = TRUE)
})

test_that("the bundle writes a per-group provenance file", {
  txt <- .downloads_src()
  expect_match(txt, "group_provenance_df(g)", fixed = TRUE)
  expect_match(txt, "_provenance.txt", fixed = TRUE)
})

test_that("the workbook receives the group's provenance rows", {
  txt <- .downloads_src()
  expect_match(txt, "provenance = group_provenance_extra(g)", fixed = TRUE)
})

test_that("the Downloads tab surfaces divergence before download", {
  txt <- .downloads_src()
  expect_match(txt, "bundle_divergence(gs)", fixed = TRUE)
})
