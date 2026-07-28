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
