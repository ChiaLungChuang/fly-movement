# FINDINGS, recorded as failing tests. Not fixed here, and not to be skipped,
# relaxed or deleted: they pass when the defects are fixed.
#
# 1. Rows treated as minutes. detect_dead_channels() compares a run of zero
#    ROWS with death_threshold_hours * 60, and backfill_and_detect_sleep()
#    compares a run of quiet ROWS with sleep_threshold_min. Both are right only
#    at 60 s bins. At 30 s bins a fly silent for 6 h is called dead under a 12 h
#    rule, and 2.5 min of quiet is called sleep under a 5 min rule.
#
# 2. Case B reads the wrong columns. read_one_monitor() takes Case B when the
#    file's third field is not a time, i.e. when fread() splits on whitespace:
#    a space-delimited DAM file, where the date "3 Jun 26" becomes three fields
#    (V2-V4) and the time is V5. Case B then reads channels from V6, which in
#    that layout is the STATUS field; channel 1 is V13 (44 fields in all).
#    The bundled samples (data/ and inst/extdata/, tab-delimited, 42 fields)
#    take Case A and are unaffected; the controls below say so.
#
# Frames and files are built in the test; nothing reads external data.

.minute_frame <- function(bin_s, quiet_rows, lead = 4L, tail = 4L) {
  n  <- lead + quiet_rows + tail
  t0 <- as.POSIXct("2026-06-03 00:00:00", tz = "UTC")
  data.table(
    datetime = t0 + (0:(n - 1)) * bin_s,
    ch1      = c(rep(3, lead), rep(0, quiet_rows), rep(3, tail))
  )
}

# ── 1a. death detection ───────────────────────────────────────────────────────

test_that("FINDING: 7 h of silence at 30 s bins is not a 12 h death", {
  summed <- .minute_frame(30, quiet_rows = 7L * 120L)      # 840 rows = 7 h
  invisible(utils::capture.output(
    res <- detect_dead_channels(list(summed = summed), death_threshold_hours = 12)))
  expect_equal(res$status[res$channel == 1], "ALIVE")
  expect_equal(res$max_inactivity_hours[res$channel == 1], 7)
})

test_that("control: 7 h of silence at 60 s bins is not a 12 h death", {
  summed <- .minute_frame(60, quiet_rows = 7L * 60L)
  invisible(utils::capture.output(
    res <- detect_dead_channels(list(summed = summed), death_threshold_hours = 12)))
  expect_equal(res$status[res$channel == 1], "ALIVE")
  expect_equal(res$max_inactivity_hours[res$channel == 1], 7)
})

# ── 1b. sleep bouts ───────────────────────────────────────────────────────────

.sleep_bouts <- function(bin_s, quiet_rows) {
  f   <- .minute_frame(bin_s, quiet_rows)
  dat <- data.table(channel = 1L, datetime = f$datetime,
                    B_plug = f$ch1, B_middle = 0, B_food = 0, zone = "PLUG",
                    unknown_type = NA_character_)
  diag <- data.table(channel = integer(), unknown_type = character())
  invisible(utils::capture.output(
    out <- backfill_and_detect_sleep(list(dat = dat, unknown_diag = diag),
                                     sleep_threshold_min = 5)))
  sum(out$dat$bout_start)
}

test_that("FINDING: 3 min of quiet at 30 s bins is not a 5 min sleep bout", {
  expect_equal(.sleep_bouts(30, quiet_rows = 6L), 0)        # 6 rows = 3 min
})

test_that("control: 3 min of quiet at 60 s bins is not a 5 min sleep bout", {
  expect_equal(.sleep_bouts(60, quiet_rows = 3L), 0)
})

# ── 2. Case B ─────────────────────────────────────────────────────────────────

.write_dam <- function(path, sep, n = 20L) {
  t0  <- as.POSIXct("2026-06-03 16:36:00", tz = "UTC")
  ts  <- t0 + (0:(n - 1)) * 60
  chs <- paste(40 + 1:32, collapse = sep)                    # channel k = 40 + k
  # Fields 1-10: index, date, time, status=1, extras=0, monitor=7, tube=0,
  # data type=Ct, unused=0, light=0; then 32 channels.
  writeLines(sprintf(paste("%d", "3 Jun 26", "%s", "1", "0", "7", "0", "Ct", "0", "0",
                           "%s", sep = sep),
                     seq_len(n), format(ts, "%H:%M:%S", tz = "UTC"), chs), path)
  path
}

.read_three <- function(sep) {
  d <- tempfile("dam"); dir.create(d)
  files <- c(plug   = .write_dam(file.path(d, "M3.txt"), sep),
             middle = .write_dam(file.path(d, "M2.txt"), sep),
             food   = .write_dam(file.path(d, "M1.txt"), sep))
  log <- utils::capture.output(
    m <- suppressWarnings(read_position_monitors(files, channels = c(1, 32), tz = "UTC")))
  list(m = m, log = log)
}

test_that("FINDING: a space-delimited DAM file (Case B) reads channel 1 from channel 1", {
  r <- .read_three(" ")
  expect_true(any(grepl("Case B", r$log)))                   # it IS the Case B path
  expect_equal(unique(r$m$plug$ch1), 41)                     # got the status field
  expect_equal(unique(r$m$plug$ch32), 72)
})

test_that("control: the same file tab-delimited (Case A) reads the right columns", {
  r <- .read_three("\t")
  expect_true(any(grepl("Case A", r$log)))
  expect_equal(unique(r$m$plug$ch1), 41)
  expect_equal(unique(r$m$plug$ch32), 72)
})

test_that("control: the bundled sample monitors take Case A", {
  skip_if_not(all(file.exists(unlist(sample_paths))), "sample data missing")
  log <- utils::capture.output(
    suppressWarnings(read_position_monitors(unlist(sample_paths), channels = 1, tz = "UTC")))
  expect_equal(sum(grepl("Case A", log)), 3)
  expect_false(any(grepl("Case B", log)))
})
