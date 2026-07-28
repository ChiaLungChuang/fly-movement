# Tests for detect_dead_channels(), add_time_metadata(), parse_channels().

test_that("detect_dead_channels flags a fully silent channel", {
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  n  <- 60 * 24   # 24 h of minutes
  summed <- data.table::data.table(datetime = t0 + (0:(n - 1)) * 60)
  summed[, ch1 := 1L]          # always active -> ALIVE
  summed[, ch2 := 0L]          # fully silent  -> DEAD
  monitors <- list(summed = summed)

  ds <- detect_dead_channels(monitors, death_threshold_hours = 12)
  expect_equal(ds[channel == 1, status], "ALIVE")
  expect_equal(ds[channel == 2, status], "DEAD")
})

test_that("detect_dead_channels respects the threshold", {
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  n  <- 60 * 24
  summed <- data.table::data.table(datetime = t0 + (0:(n - 1)) * 60)
  # 6 h silent then active: below a 12 h threshold -> ALIVE
  summed[, ch1 := c(rep(0L, 60 * 6), rep(1L, n - 60 * 6))]
  ds <- detect_dead_channels(list(summed = summed), death_threshold_hours = 12)
  expect_equal(ds[channel == 1, status], "ALIVE")
})

test_that("add_time_metadata assigns light/dark and experiment day", {
  t0 <- as.POSIXct("2026-01-29 05:00:00", tz = "America/Chicago")
  dt <- data.table::data.table(
    datetime = t0 + c(0, 3600 * 6, 3600 * 13, 3600 * 25),  # 05:00,11:00,18:00,+1d06:00
    channel  = 1L, B_plug = 0, B_middle = 0, B_food = 0
  )
  out <- add_time_metadata(dt, lights_on = "05:00:00", lights_off = "17:00:00",
                           tz = "America/Chicago")
  expect_equal(out$day_night, c(TRUE, TRUE, FALSE, TRUE))   # light phase flag
  expect_equal(out$experiment_day, c(1L, 1L, 1L, 2L))
})

test_that("detect_datetime_range finds the overlapping window across files", {
  skip_if_not(all(file.exists(unlist(sample_paths))), "sample data missing")
  r <- detect_datetime_range(unlist(sample_paths), tz = "UTC")
  expect_s3_class(r$min, "POSIXct")
  expect_true(r$min < r$max)
  # detected values match the file wall-clock (parsed as UTC, no shift)
  expect_equal(format(r$min, "%Y-%m-%d %H:%M", tz = "UTC"), "2026-01-28 15:07")
  expect_true(r$earliest <= r$min && r$latest >= r$max)
})

test_that("detect_datetime_range errors when files don't overlap", {
  d1 <- tempfile(fileext = ".txt"); d2 <- tempfile(fileext = ".txt")
  # Case-A rows: idx, "DD Mon YY", "HH:MM:SS", then activity cols.
  mk <- function(path, day) {
    rows <- sprintf("%d\t%d Jan 26\t%02d:00:00\t0\tCt\t%s",
                    1:3, day, 0:2, paste(rep(0, 32), collapse = "\t"))
    writeLines(rows, path)
  }
  mk(d1, 1); mk(d2, 20)   # day 1 vs day 20 — no overlap
  expect_error(detect_datetime_range(c(d1, d2), tz = "UTC"), "overlap")
})

test_that("read_monitor_datetimes reads past a blank line (dropped reading)", {
  # A bare fread() stops at the first blank line. On real data a single dropped
  # reading mid-file truncated the period selector to everything before it.
  p <- tempfile(fileext = ".txt")
  row <- function(i, hh, mm) sprintf("%d\t1 Jan 26\t%02d:%02d:00\t0\tCt\t%s",
                                     i, hh, mm, paste(rep(0, 32), collapse = "\t"))
  writeLines(c(row(1, 0, 0), row(2, 0, 1), "",
               row(3, 0, 2), row(4, 0, 3)), p)

  dts <- read_monitor_datetimes(p, tz = "UTC")
  expect_length(dts, 4)
  expect_equal(format(max(dts), "%H:%M", tz = "UTC"), "00:03")
})

test_that("detect_datetime_range spans rows after a blank line", {
  p1 <- tempfile(fileext = ".txt"); p2 <- tempfile(fileext = ".txt")
  row <- function(i, hh, mm) sprintf("%d\t1 Jan 26\t%02d:%02d:00\t0\tCt\t%s",
                                     i, hh, mm, paste(rep(0, 32), collapse = "\t"))
  body <- c(row(1, 0, 0), row(2, 0, 1), "", row(3, 0, 2), row(4, 0, 3))
  writeLines(body, p1); writeLines(body, p2)

  r <- detect_datetime_range(c(p1, p2), tz = "UTC")
  expect_equal(format(r$max, "%H:%M", tz = "UTC"), "00:03")  # not "00:01"
  expect_equal(format(r$latest, "%H:%M", tz = "UTC"), "00:03")
})

test_that("parse_channels handles ranges, lists and clamping", {
  skip_if_not(exists("parse_channels"), "shiny not available")
  expect_equal(parse_channels("1:32"), 1:32)
  expect_equal(parse_channels("1,3,5"), c(1L, 3L, 5L))
  expect_equal(parse_channels("1:4, 10"), c(1L, 2L, 3L, 4L, 10L))
  expect_equal(parse_channels(""), 1:32)             # default
  expect_equal(parse_channels("0,40,5"), 5L)         # out-of-range dropped
})
