# Unit tests for beam_to_zone() — the core beam-pattern -> zone map (R/03).

test_that("two-beam corridor patterns are confirmed zones", {
  expect_equal(beam_to_zone(1, 1, 0), "Z2")   # plug + middle
  expect_equal(beam_to_zone(0, 1, 1), "Z3")   # middle + food
})

test_that("single-beam patterns are boundaries", {
  expect_equal(beam_to_zone(1, 0, 0), "Z1_Z2_boundary")
  expect_equal(beam_to_zone(0, 1, 0), "Z2_Z3_boundary")
  expect_equal(beam_to_zone(0, 0, 1), "Z3_Z4_boundary")
})

test_that("all-zero returns NA (indeterminate from one bin)", {
  expect_true(is.na(beam_to_zone(0, 0, 0)))
})

test_that("physically ambiguous multi-beam patterns are flagged", {
  expect_equal(beam_to_zone(1, 0, 1), "INCONSISTENT")  # plug+food, no middle
  expect_equal(beam_to_zone(1, 1, 1), "MULTIPASS")     # all three
})

test_that("higher beam counts behave like presence (>0)", {
  expect_equal(beam_to_zone(3, 2, 0), "Z2")
  expect_equal(beam_to_zone(0, 5, 9), "Z3")
})
