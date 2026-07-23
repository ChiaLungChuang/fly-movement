# Unit tests for make_activity_chronogram() + summarize_activity() (R/06).

# Small synthetic dat: 2 channels, 25 minutes, known beam counts.
make_toy_dat <- function() {
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  dt <- data.table::data.table(
    datetime = rep(t0 + (0:24) * 60, times = 2),
    channel  = rep(c(1L, 2L), each = 25),
    B_plug   = 1, B_middle = 2, B_food = 3
  )
  dt[]
}

test_that("summed chronogram sums the three monitors per bin", {
  d <- make_toy_dat()
  cg <- make_activity_chronogram(d, "G", monitor = "summed", bin_min = 10)
  expect_true(all(c("File", "Labels", "Channels", "Time_min", "Activity") %in%
                    names(cg)))
  # bin 0 = minutes 0..9 -> 10 rows * (1+2+3) = 60 per channel
  first_bin <- cg[cg$Time_min == 0 & cg$Channels == 1, "Activity"]
  expect_equal(first_bin, 60)
})

test_that("Time_min is elapsed seconds (Rtivity convention)", {
  d <- make_toy_dat()
  cg <- make_activity_chronogram(d, "G", monitor = "plug", bin_min = 10)
  # bins should be multiples of 600 seconds
  expect_true(all(cg$Time_min %% 600 == 0))
  expect_equal(sort(unique(cg$Time_min)), c(0, 600, 1200))
})

test_that("single monitor selects only that beam", {
  d <- make_toy_dat()
  cg <- make_activity_chronogram(d, "G", monitor = "food", bin_min = 10)
  # bin 0 = 10 rows * 3 = 30
  expect_equal(cg[cg$Time_min == 0 & cg$Channels == 1, "Activity"], 30)
})

test_that("summarize_activity yields per-group per-bin CI columns", {
  d  <- make_toy_dat()
  cg <- make_activity_chronogram(d, "G", monitor = "summed", bin_min = 10)
  s  <- summarize_activity(cg)
  expect_true(all(c("Labels", "Time_min", "average_activity_per_channel",
                    "lower_ci", "upper_ci") %in% names(s)))
  # both channels identical -> zero variance -> CI equals mean
  expect_equal(s$lower_ci, s$average_activity_per_channel)
})

test_that("invalid monitor name errors", {
  d <- make_toy_dat()
  expect_error(make_activity_chronogram(d, "G", monitor = "nope"))
})
