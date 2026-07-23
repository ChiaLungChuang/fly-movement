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

test_that("parse_channels handles ranges, lists and clamping", {
  skip_if_not(exists("parse_channels"), "shiny not available")
  expect_equal(parse_channels("1:32"), 1:32)
  expect_equal(parse_channels("1,3,5"), c(1L, 3L, 5L))
  expect_equal(parse_channels("1:4, 10"), c(1L, 2L, 3L, 4L, 10L))
  expect_equal(parse_channels(""), 1:32)             # default
  expect_equal(parse_channels("0,40,5"), 5L)         # out-of-range dropped
})
