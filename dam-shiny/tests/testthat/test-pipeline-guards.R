# Guards in run_group_pipeline() that stop a run at the step that actually
# knows what went wrong, rather than letting a degenerate table travel two
# steps further and fail inside data.table internals.
#
# Observed: dead-channel detection classified all 32 channels dead and removed
# them. "Assigning position zones" then received a table with no ch* columns
# and threw
#     some columns are not in the data.table: [channel, datetime]
# which names neither the channels, nor the threshold, nor the removal step
# that emptied it.

# All-zero 42-column DAM file: every channel silent, so every channel is dead
# at any threshold shorter than the recording.
.write_silent_dam <- function(path, n_min = 40) {
  rows <- vapply(seq_len(n_min), function(m) {
    tm <- format(as.POSIXct("2026-06-03 16:36:00", tz = "UTC") + (m - 1) * 60,
                 "%H:%M:%S")
    paste(c(m, "3 Jun 26", tm, 1, 0, 1, 0, "Ct", 0, 0, rep(0L, 32)),
          collapse = "\t")
  }, character(1))
  writeLines(rows, path)
  invisible(path)
}

.silent_group <- function(threshold_hours = 0.1) {
  p <- vapply(1:3, function(i) tempfile(fileext = ".txt"), character(1))
  for (f in p) .write_silent_dam(f)
  g <- new_group("gSilent", "All silent", "#0072B2")
  g$monitor_paths <- list(plug = p[1], middle = p[2], food = p[3])
  g$config$death_threshold_hours <- threshold_hours
  g$config$tz <- "UTC"
  list(group = g, paths = p)
}

test_that("a run that removes every channel stops with an explicit message", {
  skip_if_not(exists("run_group_pipeline"), "engine not sourced")

  fx <- .silent_group()
  on.exit(unlink(fx$paths), add = TRUE)

  err <- tryCatch({
    invisible(utils::capture.output(run_group_pipeline(fx$group)))
    NULL
  }, error = function(e) conditionMessage(e))

  expect_false(is.null(err))

  # Names what happened, where, and with which setting -- and is not the
  # data.table message from two steps later.
  expect_match(err, "channel", ignore.case = TRUE)
  expect_match(err, "dead", ignore.case = TRUE)
  expect_match(err, "0\\.1")                      # the threshold in force
  expect_false(grepl("some columns are not in the data.table", err, fixed = TRUE))
})

test_that("the guard does not fire when channels survive", {
  skip_if_not(exists("run_group_pipeline"), "engine not sourced")
  skip_if_not(all(file.exists(unlist(sample_paths))), "sample data missing")

  g <- new_group("gLive", "Live", "#0072B2")
  g$monitor_paths <- sample_paths
  g$config$death_threshold_hours <- 12
  g$config$channels <- 1:8

  # False-positive check: whatever else this run does, it must not be stopped
  # by the empty-channel guard. Kept as an unconditional assertion -- behind an
  # `if (!is.null(err))` it asserted nothing on the happy path and testthat
  # rightly reported an empty test.
  err <- tryCatch({
    invisible(utils::capture.output(run_group_pipeline(g)))
    ""
  }, error = function(e) conditionMessage(e))

  expect_false(grepl("nothing left to analyse", err, fixed = TRUE))
})
