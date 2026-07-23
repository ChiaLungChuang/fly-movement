# Unit tests for classify_transit() — active-bin transit typing (R/03).

test_that("no beam activity is UNANCHORED", {
  expect_equal(classify_transit("Z1", "Z2", 0, 0, 0), "TRANSIT_UNANCHORED")
})

test_that("no anchors on either side is UNANCHORED", {
  expect_equal(classify_transit(NA, NA, 1, 0, 0), "TRANSIT_UNANCHORED")
})

test_that("same zone before and after is a RETURN (anchor agreement wins)", {
  expect_equal(classify_transit("Z2", "Z2", 1, 1, 1), "TRANSIT_RETURN")
  expect_equal(classify_transit("Z3", "Z3", 1, 0, 0), "TRANSIT_RETURN")
})

test_that("single beam between different anchors is a PASS", {
  expect_equal(classify_transit("Z1", "Z2", 1, 0, 0), "TRANSIT_PASS")
})

test_that("one-sided anchor with single beam is a PASS", {
  expect_equal(classify_transit("Z2", NA, 0, 1, 0), "TRANSIT_PASS")
  expect_equal(classify_transit(NA, "Z3", 0, 0, 1), "TRANSIT_PASS")
})

test_that("even parity across many beams reads as MULTIPASS when one-sided", {
  # one-sided anchor, total > 1, all-even parity -> RETURN
  expect_equal(classify_transit("Z2", NA, 2, 2, 0), "TRANSIT_RETURN")
  # one-sided anchor, odd parity present -> MULTIPASS
  expect_equal(classify_transit("Z2", NA, 1, 1, 1), "TRANSIT_MULTIPASS")
})
