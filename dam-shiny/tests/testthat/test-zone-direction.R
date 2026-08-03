# Zone assignment is DIRECTIONAL, end to end.
#
# test-beam_to_zone.R already pins direction at the primitive: beam_to_zone()
# maps (plug, middle, 0) -> Z2 and (0, middle, food) -> Z3. What nothing pinned
# is the plumbing between the monitor slots and those beams -- that the table
# in monitors$plug becomes B_plug and therefore drives Z1/Z2, and monitors$food
# drives Z3/Z4.
#
# That gap is why a transposed monitor-to-slot assignment has been invisible.
# Swapping the two end files maps Z1<->Z4 and Z2<->Z3, which is an exact
# symmetry of the data: counts, transit structure and anchor confidence are all
# preserved, so every existing assertion holds either way round. Only an
# assertion that names a zone for a known beam pattern can tell the two apart.

.mk_monitors <- function(plug, middle, food) {
  t0 <- as.POSIXct("2026-01-28 15:12:00", tz = "UTC")
  dt <- t0 + (seq_along(plug) - 1L) * 60
  list(
    plug   = data.table::data.table(datetime = dt, ch1 = plug),
    middle = data.table::data.table(datetime = dt, ch1 = middle),
    food   = data.table::data.table(datetime = dt, ch1 = food),
    analysis_start = NULL
  )
}

test_that("the plug slot drives Z1/Z2 and the food slot drives Z3/Z4", {
  skip_if_not(exists("assign_position_zones"), "engine not sourced")

  # One fly, four minutes, walking plug end -> food end:
  #   plug only | plug+middle | middle+food | food only
  mon <- .mk_monitors(plug   = c(1, 1, 0, 0),
                      middle = c(0, 1, 1, 0),
                      food   = c(0, 0, 1, 1))

  invisible(utils::capture.output(res <- assign_position_zones(mon)))
  z <- res$dat$zone

  expect_equal(z, c("Z1", "Z2", "Z3", "Z4"))

  # Stated as the mirror too, so the failure message names the actual defect
  # rather than just an unequal vector.
  expect_equal(z[1], "Z1"); expect_false(identical(z[1], "Z4"))
  expect_equal(z[2], "Z2"); expect_false(identical(z[2], "Z3"))
  expect_equal(z[3], "Z3"); expect_false(identical(z[3], "Z2"))
  expect_equal(z[4], "Z4"); expect_false(identical(z[4], "Z1"))
})

test_that("transposing the end slots mirrors the zones", {
  # The failure mode itself, asserted directly: this is what a transposed
  # upload produces, and it is why no count- or structure-based test can see
  # it. The zone sequence reverses and nothing else changes.
  skip_if_not(exists("assign_position_zones"), "engine not sourced")

  correct <- .mk_monitors(plug   = c(1, 1, 0, 0),
                          middle = c(0, 1, 1, 0),
                          food   = c(0, 0, 1, 1))
  # Same three files, plug and food swapped between the slots.
  swapped <- .mk_monitors(plug   = c(0, 0, 1, 1),
                          middle = c(0, 1, 1, 0),
                          food   = c(1, 1, 0, 0))

  invisible(utils::capture.output({
    a <- assign_position_zones(correct)$dat$zone
    b <- assign_position_zones(swapped)$dat$zone
  }))

  expect_equal(a, c("Z1", "Z2", "Z3", "Z4"))
  expect_equal(b, c("Z4", "Z3", "Z2", "Z1"))

  # Identical in every respect a count- or structure-based assertion can see.
  expect_equal(sort(a), sort(b))
  expect_equal(length(unique(a)), length(unique(b)))
})
