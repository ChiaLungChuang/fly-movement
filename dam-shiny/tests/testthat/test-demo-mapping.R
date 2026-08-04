# The bundled demo data's slot assignment, pinned by filename.
#
# The rig convention: within an apparatus the monitors run food, central, plug
# in FILE order, so for MonitorN the role is (N - 1) mod 3 -- 0 food,
# 1 central, 2 plug. Monitor7 is the food end, Monitor9 the plug end.
#
# This was transposed from the initial commit until 388132a, invisibly: the
# transposition is an exact symmetry of the data, so no count, structure or
# QC assertion could see it. The directional tests in test-zone-direction.R
# close that gap for SYNTHETIC fixtures, where the beam pattern is authored.
# They cannot close it for the bundled files, whose correct assignment is a
# fact about the rig rather than about the data.
#
# So this is the convention check written as a test rather than as runtime
# code: filenames only, no expectation about what the data contains.

.role_by_convention <- function(path) {
  # Match before coercing: as.integer() on a non-matching name warns with
  # "NAs introduced by coercion". The rule has to be SILENT on names that do
  # not parse -- that is the behaviour the README specifies for this check --
  # and a warning is not silent.
  b <- basename(path)
  if (!grepl("^Monitor\\d+\\.txt$", b)) return(NA_character_)
  n <- as.integer(sub("^Monitor(\\d+)\\.txt$", "\\1", b))
  c("food", "central", "plug")[((n - 1L) %% 3L) + 1L]
}

test_that("the convention maps monitor numbers to roles as stated", {
  # Pin the rule itself, so a mistake in it cannot silently excuse a
  # mistake in the mapping below.
  expect_equal(.role_by_convention("Monitor7.txt"), "food")
  expect_equal(.role_by_convention("Monitor8.txt"), "central")
  expect_equal(.role_by_convention("Monitor9.txt"), "plug")
  expect_equal(.role_by_convention("Monitor1.txt"), "food")   # apparatus 1
  expect_equal(.role_by_convention("Monitor12.txt"), "plug")  # apparatus 4
  expect_true(is.na(.role_by_convention("something_else.txt")))
})

test_that("the bundled sample files are wired to the slots the convention gives", {
  skip_if_not(exists("sample_paths"), "fixture not defined")

  expect_equal(basename(sample_paths$food),   "Monitor7.txt")
  expect_equal(basename(sample_paths$middle), "Monitor8.txt")
  expect_equal(basename(sample_paths$plug),   "Monitor9.txt")

  # Same statement via the rule, so the two cannot drift apart.
  expect_equal(.role_by_convention(sample_paths$food),   "food")
  expect_equal(.role_by_convention(sample_paths$middle), "central")
  expect_equal(.role_by_convention(sample_paths$plug),   "plug")
})

test_that("the app's demo group wires the same files to the same slots", {
  # The demo observer resolves its path via system.file()/inst and cannot be
  # driven from tests/testthat, so this reads the wiring at source -- the same
  # instrument used to pin export_results_excel()'s call site. sample_paths
  # above is the behavioural half; this stops the two diverging.
  src <- file.path(app_root, "R", "mod_group_manager.R")
  skip_if_not(file.exists(src), "module source missing")
  lines <- readLines(src, warn = FALSE)

  start <- grep('paths <- list\\(plug\\s*=\\s*file\\.path\\(ex,', lines)
  expect_length(start, 1)
  block <- paste(lines[start:(start + 2L)], collapse = " ")

  expect_match(block, 'plug\\s*=\\s*file\\.path\\(ex,\\s*"Monitor9\\.txt"\\)')
  expect_match(block, 'middle\\s*=\\s*file\\.path\\(ex,\\s*"Monitor8\\.txt"\\)')
  expect_match(block, 'food\\s*=\\s*file\\.path\\(ex,\\s*"Monitor7\\.txt"\\)')
})
