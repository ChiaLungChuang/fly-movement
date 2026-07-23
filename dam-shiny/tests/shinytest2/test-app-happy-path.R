# App-level happy-path test with shinytest2.
#
# Drives the real app: add the bundled demo group, run it, and confirm the
# pipeline completes and downstream tabs render. Requires the {shinytest2}
# package and a headless Chrome (via {chromote}); skips cleanly otherwise.
#
# Run from the app root with:
#   Rscript -e 'shinytest2::test_app("tests/shinytest2")'
# or point it at the app explicitly:
#   Rscript -e 'shinytest2::test_app(".")'

library(testthat)

test_that("demo group runs end-to-end and QC renders", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  library(shinytest2)

  # App root is two levels up from tests/shinytest2/.
  app_dir <- normalizePath(file.path("..", ".."))
  skip_if_not(file.exists(file.path(app_dir, "app.R")), "app.R not found")

  app <- AppDriver$new(
    app_dir = app_dir,
    name    = "dam-demo",
    timeout = 30 * 1000,
    load_timeout = 60 * 1000
  )
  on.exit(app$stop(), add = TRUE)

  # 1) Add the demo group (uses inst/extdata sample monitors — no upload).
  app$click("groups-demo")
  app$wait_for_idle()

  # The groups table should now list one group.
  tbl <- app$get_value(output = "groups-group_tbl")
  expect_false(is.null(tbl))

  # 2) Select the row and run the pipeline. The anchor pass takes a few
  #    seconds; give the run a generous idle timeout.
  app$set_inputs(`groups-group_tbl_rows_selected` = 1,
                 allow_no_input_binding_ = TRUE)
  app$click("groups-run")
  app$wait_for_idle(timeout = 120 * 1000)

  # 3) Data & QC tab should now render the dead-channel table for the group.
  app$set_inputs(tabs = "qc")
  app$wait_for_idle(timeout = 30 * 1000)
  dead <- app$get_value(output = "qc-dead_tbl")
  expect_false(is.null(dead))

  # 4) Chronogram (combined) should produce a plot once a group is done.
  app$set_inputs(tabs = "chrono")
  app$wait_for_idle(timeout = 30 * 1000)
  chrono <- app$get_value(output = "chrono-chrono")
  expect_false(is.null(chrono))
})
