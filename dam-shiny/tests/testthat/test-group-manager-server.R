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
