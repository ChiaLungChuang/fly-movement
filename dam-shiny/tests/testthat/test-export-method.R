# export_results_excel() honours the selected zone method, and records it.
#
# It previously read sleep_zone_anchored / zone_anchored unconditionally, so a
# bundle built for a state-machine group shipped state-machine PDFs next to an
# anchor-method workbook, with nothing on either artefact saying so. The
# workbook now takes use_anchored and always writes a Provenance sheet: a
# workbook gets emailed and opened months later, separated from the bundle that
# explained it.

.mk_dat <- function() {
  # Two channels whose two methods DISAGREE, so a test that ignores the
  # method cannot accidentally pass.
  n <- 4L
  data.table::data.table(
    datetime = rep(as.POSIXct("2026-01-28 15:12:00", tz = "UTC") +
                     (seq_len(n) - 1L) * 60, each = 2L),
    channel  = rep(1:2, times = n),
    B_plug = 0L, B_middle = 0L, B_food = 0L,
    zone                = "Z1",
    zone_anchored       = "Z4",
    sleep_zone          = "Z1",
    sleep_zone_anchored  = "Z4",
    bout_start          = TRUE,
    bout_start_anchored = TRUE,
    # Real values: the exporter aggregates transit_type and cannot build its
    # Transit_summary sheet from an all-NA column.
    transit_type = rep(c("NO_TRANSIT", "TRANSIT_OK"), length.out = n * 2L),
    anchor_type  = "SLEEP",
    anchor_zone  = "Z4"
  )
}

.mk_side <- function() {
  list(
    dead = data.table::data.table(channel = 1:2, status = "ALIVE",
                                  max_inactive_h = 0,
                                  death_datetime = as.POSIXct(NA)),
    unknown = data.table::data.table(channel = integer(), n = integer()),
    # One real anchor per channel: anchor_summary aggregates with mean() over
    # subsets, which yields ragged column lengths on an empty table.
    anchors = data.table::data.table(
      channel = 1:2, start_idx = c(1L, 1L), end_idx = c(4L, 4L),
      duration_min = c(4, 4), anchor_type = c("SLEEP", "STOP"),
      zone = c("Z4", "Z4"), confidence = c("confirmed", "confirmed"))
  )
}

.read_sheet <- function(f, sheet) {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx missing")
  openxlsx::read.xlsx(f, sheet = sheet)
}

test_that("the workbook reports the columns of the selected zone method", {
  skip_if_not(exists("export_results_excel"), "engine not sourced")
  dat <- .mk_dat(); s <- .mk_side()

  f_anchor <- tempfile(fileext = ".xlsx")
  f_sm     <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(f_anchor, f_sm)), add = TRUE)

  invisible(utils::capture.output(
    export_results_excel(dat, s$dead, s$unknown, s$anchors, "G",
                         f_anchor, use_anchored = TRUE)))
  invisible(utils::capture.output(
    export_results_excel(dat, s$dead, s$unknown, s$anchors, "G",
                         f_sm, use_anchored = FALSE)))

  a <- .read_sheet(f_anchor, "Per_fly")
  m <- .read_sheet(f_sm, "Per_fly")

  # The fixture puts every anchor bout in Z4 and every state-machine bout in
  # Z1, so the two workbooks must disagree in exactly that way.
  expect_true("Z4" %in% names(a))
  expect_equal(sum(a$Z4), 2)     # both channels, all bouts in Z4
  expect_equal(sum(m$Z1), 2)     # state machine puts them in Z1
  expect_false(isTRUE(all.equal(a$Z4, m$Z4)))
})

test_that("every workbook records the method it used, in the file", {
  skip_if_not(exists("export_results_excel"), "engine not sourced")
  dat <- .mk_dat(); s <- .mk_side()

  for (ua in c(TRUE, FALSE)) {
    f <- tempfile(fileext = ".xlsx")
    invisible(utils::capture.output(
      export_results_excel(dat, s$dead, s$unknown, s$anchors, "G", f,
                           use_anchored = ua)))
    prov <- .read_sheet(f, "Provenance")
    expect_true("Zone method" %in% prov$Parameter)
    expect_equal(prov$Value[prov$Parameter == "Zone method"],
                 if (ua) "anchor" else "state machine")
    unlink(f)
  }
})

test_that("caller-supplied provenance rows are carried into the sheet", {
  skip_if_not(exists("export_results_excel"), "engine not sourced")
  dat <- .mk_dat(); s <- .mk_side()
  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f), add = TRUE)

  extra <- data.frame(Parameter = c("Source files", "Alignment"),
                      Value = c("Monitor7.txt, Monitor8.txt, Monitor9.txt",
                                "not required"),
                      stringsAsFactors = FALSE)
  invisible(utils::capture.output(
    export_results_excel(dat, s$dead, s$unknown, s$anchors, "G", f,
                         use_anchored = FALSE, provenance = extra)))

  prov <- .read_sheet(f, "Provenance")
  expect_true(all(c("Zone method", "Source files", "Alignment") %in% prov$Parameter))
  expect_equal(prov$Value[prov$Parameter == "Alignment"], "not required")
})

test_that("the default reproduces the pre-parameter anchor-only behaviour", {
  # Any workbook produced before use_anchored existed is anchor-method by
  # construction; the default must keep that true for run_position.R, which
  # has not opted in.
  skip_if_not(exists("export_results_excel"), "engine not sourced")
  dat <- .mk_dat(); s <- .mk_side()
  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f), add = TRUE)

  invisible(utils::capture.output(
    export_results_excel(dat, s$dead, s$unknown, s$anchors, "G", f)))
  prov <- .read_sheet(f, "Provenance")
  expect_equal(prov$Value[prov$Parameter == "Zone method"], "anchor")
})

test_that("the app's call site passes use_anchored explicitly", {
  # The TRUE default is for run_position.R's benefit. If the app's call ever
  # stops passing the value it silently reverts to anchor-only and restores
  # exactly the defect this work removed -- and no other caller reads the
  # app's copy, so nothing else would surface it.
  src <- file.path(app_root, "R", "mod_downloads.R")
  skip_if_not(file.exists(src), "module source missing")
  txt <- paste(readLines(src, warn = FALSE), collapse = "\n")

  # Extract the call by balancing parentheses -- the argument list nests
  # file.path(paste0(...)), which a fixed-depth regex cannot span.
  lines <- readLines(src, warn = FALSE)
  start <- grep("export_results_excel\\(", lines)
  expect_length(start, 1)
  depth <- 0L; call <- character(0)
  for (i in seq(start, length(lines))) {
    call <- c(call, lines[i])
    chars <- strsplit(lines[i], "")[[1]]
    depth <- depth + sum(chars == "(") - sum(chars == ")")
    if (depth <= 0L && i > start - 1L) break
  }
  call <- paste(call, collapse = "\n")
  expect_match(call, "use_anchored\\s*=")
})
