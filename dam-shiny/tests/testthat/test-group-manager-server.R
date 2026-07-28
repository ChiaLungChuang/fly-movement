# Module-server test for the file-driven period selector: uploading the three
# monitor files should detect the recording range, and the date/time inputs
# should feed correct start/end strings (parsed as UTC) into the group config.

test_that("uploading files detects the period and builds the run config", {
  skip_if_not(exists("mod_group_manager_server"), "shiny not available")
  skip_if_not(all(file.exists(unlist(sample_paths))), "sample data missing")

  fileval <- function(p) data.frame(name = basename(p), size = 1L,
                                    type = "text/plain", datapath = p,
                                    stringsAsFactors = FALSE)

  rv <- init_state()
  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    session$setInputs(
      plug   = fileval(sample_paths$plug),
      middle = fileval(sample_paths$middle),
      food   = fileval(sample_paths$food)
    )

    # Detection fired from the uploads.
    rng <- detected_range()
    expect_false(is.null(rng))
    expect_equal(format(rng$min, "%Y-%m-%d %H:%M", tz = "UTC"), "2026-01-28 15:07")
    expect_true(rng$min < rng$max)

    # The renderUI would create these; simulate a narrowed window.
    session$setInputs(
      start_date  = as.Date("2026-01-29"), start_time  = "05:00",
      finish_date = as.Date("2026-01-30"), finish_time = "04:59",
      channels = "1:32", lights_on = "05:00:00", lights_off = "17:00:00",
      sleep_thr = 5, death_thr = 12, bout_win = 1, use_anchored = TRUE
    )

    cfg <- form_config()
    expect_equal(cfg$start_datetime, "2026-01-29 05:00:00")  # HH:MM -> HH:MM:00
    expect_equal(cfg$end_datetime,   "2026-01-30 04:59:00")
    expect_equal(cfg$tz, "UTC")                              # no timezone field
    expect_equal(cfg$channels, 1:32)
  })
})

test_that("the period selector UI offers the post-alignment window", {
  # detect_datetime_range() returns both an intersected pair (min/max) and a
  # widest pair (earliest/latest). The UI must render the intersected one --
  # the window that will still exist after alignment trims to the common end.
  # This asserts on the rendered HTML rather than on detected_range(), so
  # switching the renderUI to rng$earliest/rng$latest fails here instead of
  # silently offering hours that alignment is about to discard.
  skip_if_not(exists("mod_group_manager_server"), "shiny not available")

  p <- make_staggered_monitors(n_long = 200L, n_short = 140L)
  fileval <- function(x) data.frame(name = basename(x), size = 1L,
                                    type = "text/plain", datapath = x,
                                    stringsAsFactors = FALSE)

  rv <- init_state()
  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    session$setInputs(plug   = fileval(p[["plug"]]),
                      middle = fileval(p[["middle"]]),
                      food   = fileval(p[["food"]]))

    html <- output$period_ui$html
    expect_true(grepl("18:55", html, fixed = TRUE))   # common end -- offered
    expect_false(grepl("19:55", html, fixed = TRUE))  # widest end -- must not be
    expect_true(grepl("16:36", html, fixed = TRUE))   # common start

    # "Reset to full range" must reset to the same window, not the widest.
    session$setInputs(start_date  = as.Date("2026-06-03"), start_time  = "17:30",
                      finish_date = as.Date("2026-06-03"), finish_time = "18:00")
    session$setInputs(reset_period = 1)
    rng <- detected_range()
    expect_equal(format(rng$max, "%H:%M", tz = "UTC"), "18:55")
  })
})

test_that("build_datetime tolerates blank and second-precision times", {
  skip_if_not(exists("mod_group_manager_server"), "shiny not available")
  rv <- init_state()
  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    d <- as.Date("2026-01-29")
    expect_equal(build_datetime(d, "05:00"),    "2026-01-29 05:00:00")
    expect_equal(build_datetime(d, "05:00:30"), "2026-01-29 05:00:30")
    expect_equal(build_datetime(d, ""),         "2026-01-29 00:00:00")  # blank -> midnight
    expect_null(build_datetime(NULL, "05:00"))                          # no date -> NULL
  })
})
