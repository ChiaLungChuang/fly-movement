# The Engine log must identify which monitor file is which.
#
# Shiny's fileInput stores uploads under generated datapaths, so three
# different monitors all logged as
#     Reading [plug]: 0.txt
#     Reading [middle]: 0.txt
#     Reading [food]: 0.txt
# and the log could not even confirm three distinct files had been read.
# The fixture below reproduces that faithfully: real monitor data copied to
# datapath-style names.

.as_upload <- function() {
  skip_if_not(all(file.exists(unlist(sample_paths))), "sample data missing")
  dir <- file.path(tempdir(), paste0("upload_", as.integer(runif(1, 1, 1e6))))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  # Shiny names uploads 0.txt, 1.txt, 2.txt regardless of what the user picked.
  dest <- file.path(dir, c("0.txt", "1.txt", "2.txt"))
  file.copy(unlist(sample_paths)[1:3], dest)
  list(
    paths = c(plug = dest[1], middle = dest[2], food = dest[3]),
    names = list(plug = "Monitor7.txt", middle = "Monitor8.txt",
                 food = "Monitor9.txt"),
    dir   = dir
  )
}

.reading_lines <- function(txt) grep("Reading \\[", txt, value = TRUE)

test_that("the log shows the original upload names, not the datapaths", {
  skip_if_not(exists("read_position_monitors"), "engine not sourced")
  u <- .as_upload(); on.exit(unlink(u$dir, recursive = TRUE), add = TRUE)

  out <- utils::capture.output(
    suppressWarnings(read_position_monitors(u$paths, channels = 1:4,
                                            tz = "UTC",
                                            source_names = u$names)))
  lines <- .reading_lines(out)
  expect_length(lines, 3)
  expect_match(lines[1], "Monitor7.txt", fixed = TRUE)
  expect_match(lines[2], "Monitor8.txt", fixed = TRUE)
  expect_match(lines[3], "Monitor9.txt", fixed = TRUE)

  # The bug was that three distinct monitors were indistinguishable.
  expect_equal(length(unique(lines)), 3L)

  # A plain upload shows the name alone. The datapath it happens to be stored
  # under is an implementation detail, and annotating every line with it was
  # noise; the aligned flag now carries the case that genuinely needs saying.
  expect_match(lines[1], "Reading \\[plug\\]: Monitor7\\.txt$")
  expect_false(any(grepl("0.txt", lines, fixed = TRUE)))
})

test_that("without upload names the log falls back to the file's own basename", {
  skip_if_not(exists("read_position_monitors"), "engine not sourced")
  skip_if_not(all(file.exists(unlist(sample_paths))), "sample data missing")

  mf <- c(plug = sample_paths$plug, middle = sample_paths$middle,
          food = sample_paths$food)
  out <- utils::capture.output(
    suppressWarnings(read_position_monitors(mf, channels = 1:4, tz = "UTC")))
  lines <- .reading_lines(out)
  expect_length(lines, 3)
  expect_match(lines[1], "Monitor7.txt", fixed = TRUE)
})

test_that("an aligned temp copy is shown as read-from, not as the upload", {
  skip_if_not(exists("read_position_monitors"), "engine not sourced")
  u <- .as_upload(); on.exit(unlink(u$dir, recursive = TRUE), add = TRUE)

  # Stand in for dam_write_aligned() output: same data, different filename.
  aligned <- file.path(u$dir, c("Monitor7_aligned.txt", "Monitor8_aligned.txt",
                                "Monitor9_aligned.txt"))
  file.copy(unname(u$paths), aligned)
  mf <- c(plug = aligned[1], middle = aligned[2], food = aligned[3])

  out <- utils::capture.output(
    suppressWarnings(read_position_monitors(mf, channels = 1:4, tz = "UTC",
                                            source_names = u$names,
                                            aligned = TRUE)))
  lines <- .reading_lines(out)
  # Both facts present: which upload it came from, and that the run read a
  # repaired copy rather than the upload itself.
  expect_match(lines[1], "Monitor7.txt", fixed = TRUE)
  expect_match(lines[1], "Monitor7_aligned.txt", fixed = TRUE)
  expect_match(lines[1], "aligned copy", fixed = TRUE)

  # Without the flag the same paths read as a plain upload -- the flag, not
  # the filename, is what distinguishes them.
  out2 <- utils::capture.output(
    suppressWarnings(read_position_monitors(mf, channels = 1:4, tz = "UTC",
                                            source_names = u$names)))
  expect_false(any(grepl("aligned copy", .reading_lines(out2), fixed = TRUE)))
})

test_that("the worker payload carries the upload names", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_group_manager_server"), "module not sourced")

  g <- new_group("g1", "G", "#0072B2")
  g$monitor_paths <- list(plug = "0.txt", middle = "1.txt", food = "2.txt")
  g$monitor_names <- list(plug = "Monitor7.txt", middle = "Monitor8.txt",
                          food = "Monitor9.txt")
  rv <- shiny::reactiveValues(groups = list(g1 = g), selected = NULL,
                              log = character(0))

  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    lean <- dam_lean_group(rv$groups$g1)
    expect_true("monitor_names" %in% names(lean))
    expect_equal(lean$monitor_names$plug, "Monitor7.txt")

    # Running on aligned temps swaps the paths but must NOT rewrite the names:
    # the upload is still what the user chose.
    aligned <- c(plug = "a_aligned.txt", middle = "b_aligned.txt",
                 food = "c_aligned.txt")
    lean2 <- dam_lean_group(rv$groups$g1, aligned = aligned)
    expect_equal(unlist(lean2$monitor_paths, use.names = FALSE), unname(aligned))
    expect_equal(lean2$monitor_names$plug, "Monitor7.txt")
  })
})
