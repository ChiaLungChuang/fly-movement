# End-to-end integration test: run the full engine on the bundled sample
# monitors via run_group_pipeline() and check the shape of the results.
# Skips cleanly if shiny (for new_group/run_group_pipeline) or the sample
# data is unavailable.

test_that("run_group_pipeline produces a complete result on sample data", {
  skip_if_not(exists("run_group_pipeline"), "shiny/utils_state not available")
  skip_if_not(all(file.exists(unlist(sample_paths))), "sample data missing")

  g <- new_group("t1", "Test group", "#0072B2")
  g$monitor_paths <- sample_paths
  g$config$start_datetime <- "2026-01-29 05:00:00"
  g$config$end_datetime   <- "2026-01-30 04:59:00"

  # Silence engine console output.
  invisible(utils::capture.output(
    r <- suppressWarnings(run_group_pipeline(g))
  ))

  expect_equal(r$status, "done")
  expect_s3_class(r$dat, "data.table")
  expect_true(nrow(r$dat) > 0)

  # anchor columns present
  expect_true(all(c("zone_anchored", "sleep_zone_anchored",
                    "bout_start_anchored", "transit_type") %in% names(r$dat)))

  # dead-channel summary has one row per channel scanned
  expect_true(all(c("channel", "status") %in% names(r$dead_summary)))

  # activity summaries: 4 monitors x 3 metrics x 3 groupings
  expect_setequal(names(r$summaries), c("plug", "middle", "food", "summed"))
  expect_setequal(names(r$summaries$summed),
                  c("activity_total", "bout_activity", "bout_duration"))

  # chronogram data has the Rtivity columns
  expect_true(all(c("File", "Labels", "Channels", "Time_min", "Activity") %in%
                    names(r$chrono_long)))
})

test_that("pipeline runs from the lean payload the async worker sends", {
  # mod_group_manager strips the group to these five fields before handing it
  # to the background process (results are never serialized). If the pipeline
  # ever starts depending on a stripped field, this test catches it.
  skip_if_not(exists("run_group_pipeline"), "shiny/utils_state not available")
  skip_if_not(all(file.exists(unlist(sample_paths))), "sample data missing")

  g <- new_group("t3", "Lean group", "#0072B2")
  g$monitor_paths <- sample_paths
  g$config$start_datetime <- "2026-01-29 05:00:00"
  g$config$end_datetime   <- "2026-01-30 04:59:00"

  lean <- g[c("id", "label", "color", "monitor_paths", "config")]
  expect_setequal(names(lean),
                  c("id", "label", "color", "monitor_paths", "config"))

  invisible(utils::capture.output(
    r <- suppressWarnings(run_group_pipeline(lean))
  ))
  expect_equal(r$status, "done")
  expect_true(nrow(r$dat) > 0)
})

test_that("missing monitor files raise a clear error", {
  skip_if_not(exists("run_group_pipeline"), "shiny/utils_state not available")
  g <- new_group("t2", "Bad group", "#000000")
  g$monitor_paths <- list(plug = NULL, middle = NULL, food = NULL)
  expect_error(run_group_pipeline(g), "monitor files")
})
