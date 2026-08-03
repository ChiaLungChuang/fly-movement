# Tests for the built-in config.yml reader (R/app_config.R).

cfg_path <- file.path(app_root, "config.yml")

test_that("default profile is read with correct types", {
  skip_if_not(file.exists(cfg_path), "config.yml missing")
  cfg <- app_config(cfg_path, profile = "default")
  expect_equal(cfg$tz, "UTC")
  expect_equal(cfg$max_upload_mb, 250)         # coerced to numeric
  expect_identical(cfg$use_anchored, TRUE)     # coerced to logical
  expect_equal(cfg$channels, "1:32")           # left as string
})

test_that("named profile inherits missing keys from default", {
  skip_if_not(file.exists(cfg_path), "config.yml missing")
  cfg <- app_config(cfg_path, profile = "fly_room_2")
  expect_equal(cfg$lights_on, "06:00:00")      # overridden
  expect_equal(cfg$lights_off, "18:00:00")     # overridden
  expect_equal(cfg$sleep_threshold_min, 5)     # inherited from default
  expect_equal(cfg$tz, "UTC")                  # inherited from default
})

test_that("missing config file falls back to built-in defaults", {
  cfg <- app_config(tempfile(fileext = ".yml"))
  expect_equal(cfg$tz, "UTC")
  expect_equal(cfg$max_upload_mb, 250)
})


# -- Death threshold range ---------------------------------------------------
# The observed maximum inactivity on real data was 73.6 h while the input
# capped at 72, so the UI could not express "nothing is dead" on the very
# dataset in question. There is no server-side clamp, and `max` on an HTML
# number input is a validation hint rather than a hard block -- typing past the
# spinner may or may not reach the server depending on the browser -- so the
# control's own ceiling is the only supported route.

test_that("the death-threshold input accepts values above a three-day recording", {
  skip_if_not(requireNamespace("shinydashboard", quietly = TRUE),
              "shinydashboard not available")
  skip_if_not(exists("mod_group_manager_ui"), "module not sourced")

  suppressPackageStartupMessages(library(shinydashboard))
  ui <- paste(as.character(mod_group_manager_ui("gm")), collapse = "")

  # Isolate the death-threshold input tag, so a `max` belonging to a
  # neighbouring numericInput cannot satisfy this by accident.
  tag <- regmatches(ui, regexpr("<input[^>]*id=\"gm-death_thr\"[^>]*>", ui))
  expect_length(tag, 1)

  mx <- as.numeric(sub('.*max="([0-9.]+)".*', "\\1", tag))
  expect_gte(mx, 168)          # one week; covers a 73.6 h observation with room
  expect_match(tag, 'min="1"') # unchanged
})
