# Integration tests for the vendored damalignr alignment layer
# (R/00_dam_align.R) and its wiring into the group manager.
#
# Reproduces the real Monitor2 case: a V4=50 sub-row dropped mid-recording
# leaves a blank line, which a bare fread() truncates on -- making the file
# look short and mismatched. The alignment layer must diagnose this, repair
# it, and hand read_position_monitors() files it accepts.

# -- fixture builders --------------------------------------------------------

# Write a 42-column DAM file: `n_min` minutes, two sub-rows (V4 = 1, 50) each.
# drop_v4_at: minute index whose V4=50 sub-row is omitted (mid-file gap).
# blank_after_drop: if TRUE, leave a literal blank line where it was dropped
#   (the exact artefact a bare fread() stops on).
.write_dam <- function(path, n_min = 20, start_min = 0,
                       drop_v4_at = NA, blank_after_drop = FALSE) {
  rows <- list()
  for (m in seq_len(n_min)) {
    idx <- m
    tm  <- format(as.POSIXct("2026-06-03 16:36:00", tz = "UTC") +
                    (start_min + m - 1) * 60, "%H:%M:%S")
    mk <- function(v4) c(idx, "3 Jun 26", tm, v4, 0, 1, 0, "Ct", 0, 0,
                         rep(0L, 32))
    rows[[length(rows) + 1L]] <- mk(1)
    if (!(!is.na(drop_v4_at) && m == drop_v4_at)) {
      rows[[length(rows) + 1L]] <- mk(50)
    }
  }
  lines <- vapply(rows, function(r) paste(r, collapse = "\t"), character(1))
  if (!is.na(drop_v4_at) && blank_after_drop) {
    # blank line at the position the dropped V4=50 would have occupied
    pos <- 0L
    for (m in seq_len(drop_v4_at)) pos <- pos + if (m == drop_v4_at) 1L else 2L
    lines <- append(lines, "", after = pos)
  }
  writeLines(lines, path)
  invisible(path)
}

.tmp <- function() tempfile(fileext = ".txt")


test_that("dam_plan_alignment flags a mid-file dropped sub-row and repairs it", {
  a <- .tmp(); b <- .tmp(); c <- .tmp()
  .write_dam(a, n_min = 20)
  .write_dam(b, n_min = 20, drop_v4_at = 8, blank_after_drop = TRUE)  # Monitor2 analogue
  .write_dam(c, n_min = 20)
  on.exit(unlink(c(a, b, c)))

  plan <- dam_plan_alignment(c(plug = a, middle = b, food = c))

  expect_true(plan$needs_alignment)
  expect_equal(unname(plan$before), c(40L, 39L, 40L))  # b is one sub-row short
  expect_equal(length(unique(plan$after)), 1L)          # all equal after
  expect_equal(unname(plan$after["middle"]), 40L)       # repaired back to 40
  expect_equal(plan$n_inserted, 1L)
  expect_true(any(grepl("blank line", plan$report)))
})

test_that("dam_write_aligned output is accepted by read_position_monitors, raw is not", {
  skip_if_not(exists("read_position_monitors"), "engine not sourced")
  a <- .tmp(); b <- .tmp(); c <- .tmp()
  .write_dam(a, n_min = 30)
  .write_dam(b, n_min = 30, drop_v4_at = 10, blank_after_drop = TRUE)
  .write_dam(c, n_min = 30)
  on.exit(unlink(c(a, b, c)))
  raw <- c(plug = a, middle = b, food = c)

  # Raw files: the engine's row-count guard must fire.
  expect_error(
    suppressWarnings(invisible(utils::capture.output(
      read_position_monitors(raw, 1:32, tz = "America/Chicago")))),
    "different row counts"
  )

  # Aligned files: the engine reads them without error.
  plan  <- dam_plan_alignment(raw)
  paths <- dam_write_aligned(plan)
  expect_error(
    invisible(utils::capture.output(
      m <- read_position_monitors(paths, 1:32, tz = "America/Chicago"))),
    NA
  )
  expect_equal(nrow(m$plug), nrow(m$middle))
  expect_equal(nrow(m$plug), nrow(m$food))
})

test_that("dam_plan_alignment is a no-op on already-aligned sample data", {
  skip_if_not(all(file.exists(unlist(sample_paths))), "sample data missing")
  plan <- dam_plan_alignment(c(plug   = sample_paths$plug,
                               middle = sample_paths$middle,
                               food   = sample_paths$food))
  expect_false(plan$needs_alignment)
  expect_equal(plan$before, plan$after)
})

test_that("group manager routes a misaligned group to the confirmation dialog", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_group_manager_server"), "module not sourced")

  a <- .tmp(); b <- .tmp(); c <- .tmp()
  .write_dam(a, n_min = 20)
  .write_dam(b, n_min = 20, drop_v4_at = 8, blank_after_drop = TRUE)
  .write_dam(c, n_min = 20)
  on.exit(unlink(c(a, b, c)))

  g1 <- new_group("g1", "Misaligned", "#0072B2")
  g1$monitor_paths <- list(plug = a, middle = b, food = c)
  rv <- shiny::reactiveValues(groups = list(g1 = g1), selected = NULL,
                              log = character(0))

  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    shiny::isolate({
      # request_run should divert to the modal (pending populated), NOT run.
      # In the app it is called from observeEvent (a reactive consumer); here
      # isolate() supplies that context. tryCatch tolerates the mock session's
      # showModal, which runs only AFTER pending state is set.
      tryCatch(request_run("g1"), error = function(e) NULL)
      expect_true("g1" %in% pending_gids())
      expect_true(isTRUE(pending_plans()[["g1"]]$needs_alignment))
      # the group was diverted to the dialog, NOT handed to the runner:
      # run_one() would have appended it to the module's run queue.
      expect_length(queue(), 0)
    })
  })
})

# -- guard: unusable monitor paths must never reach the aligner ---------------
# request_run() built its check with c(plug=, middle=, food=), which flattens
# to an atomic vector: a NULL path disappears rather than becoming an element,
# so is.null(p) could never fire and a group missing one file arrived at
# dam_plan_alignment() as a length-2 vector. That is not a loud failure --
# the aligner diagnosed the two surviving files, pronounced them fine, and
# alignment was skipped silently. Three NULLs gave length 0, where
# any(logical(0)) is FALSE too. Only the empty-string case ever worked.
#
# Routing is asserted via queue(), which run_one() appends to before it
# invokes the task -- the module's own record of "this group went to the
# runner". Stubbing run_one() from inside testServer does not work: the
# module's request_run() resolves run_one in the module environment, which a
# rebinding in the test block does not reach.

.unusable_path_cases <- list(
  "one path NULL"  = list(plug = NULL, middle = "b.txt", food = "c.txt"),
  "one path empty" = list(plug = "",   middle = "b.txt", food = "c.txt"),
  "all three NULL" = list(plug = NULL, middle = NULL,    food = NULL)
)

for (.case in names(.unusable_path_cases)) local({
  case  <- .case
  paths <- .unusable_path_cases[[case]]

  test_that(sprintf("request_run sends a group to run_one when %s", case), {
    skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
    skip_if_not(exists("mod_group_manager_server"), "module not sourced")

    # Build the group fully before handing it to reactiveValues: a
    # read-modify-write of rv$groups outside a reactive context fails.
    g <- new_group("g1", case, "#0072B2")
    g$monitor_paths <- paths
    rv <- shiny::reactiveValues(groups = list(g1 = g), selected = NULL,
                                log = character(0))

    # Count aligner calls without changing its behaviour. The module closure
    # resolves dam_plan_alignment lexically, so the global binding is the one
    # request_run() sees.
    planned <- 0L
    orig    <- get("dam_plan_alignment", envir = globalenv())
    assign("dam_plan_alignment",
           function(...) { planned <<- planned + 1L; orig(...) },
           envir = globalenv())
    on.exit(assign("dam_plan_alignment", orig, envir = globalenv()), add = TRUE)

    shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
      shiny::isolate({
        # tryCatch tolerates the mock session's task invocation, which run_one()
        # reaches only after it has already recorded the group in queue().
        tryCatch(request_run("g1"), error = function(e) NULL)
        expect_equal(queue(), "g1")        # routed to run_one()
        expect_length(pending_gids(), 0)   # and no confirmation modal
      })
    })

    expect_equal(planned, 0L)   # never reached dam_plan_alignment()
  })
})


# -- confirming alignment must not repoint the group at temp files -----------
# The confirm handler used to do g$monitor_paths <- list(<temps>), so the group
# permanently described files under tempdir() instead of the user's uploads.
# Now the temps travel with the run (dam_lean_group) and are cached per gid so
# the dialog is asked once rather than on every run.

test_that("confirming alignment leaves monitor_paths on the originals", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_group_manager_server"), "module not sourced")

  a <- .tmp(); b <- .tmp(); cc <- .tmp()
  .write_dam(a, n_min = 20)
  .write_dam(b, n_min = 20, drop_v4_at = 8, blank_after_drop = TRUE)
  .write_dam(cc, n_min = 20)
  on.exit(unlink(c(a, b, cc)), add = TRUE)

  originals <- list(plug = a, middle = b, food = cc)
  g <- new_group("g1", "Misaligned", "#0072B2")
  g$monitor_paths <- originals
  rv <- shiny::reactiveValues(groups = list(g1 = g), selected = NULL,
                              log = character(0))

  # Count aligner calls, and capture the temps the writer produced, through the
  # global bindings the module resolves.
  planned  <- 0L
  written  <- NULL
  orig_plan  <- get("dam_plan_alignment", envir = globalenv())
  orig_write <- get("dam_write_aligned",  envir = globalenv())
  assign("dam_plan_alignment",
         function(...) { planned <<- planned + 1L; orig_plan(...) },
         envir = globalenv())
  assign("dam_write_aligned",
         function(...) { written <<- orig_write(...); written },
         envir = globalenv())
  on.exit({
    assign("dam_plan_alignment", orig_plan,  envir = globalenv())
    assign("dam_write_aligned",  orig_write, envir = globalenv())
  }, add = TRUE)

  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    shiny::isolate(tryCatch(request_run("g1"), error = function(e) NULL))
    expect_true("g1" %in% pending_gids())          # held at the dialog
    expect_equal(planned, 1L)

    # Confirm.
    tryCatch(session$setInputs(dam_confirm_align = 1), error = function(e) NULL)

    # 1. the group still points at what the user uploaded
    expect_equal(rv$groups$g1$monitor_paths, originals)

    # 2. the run got the temps instead. dam_lean_group() is the payload handed
    #    to run_task$invoke(); assert on it directly, since the task itself
    #    cannot be intercepted from here.
    cached <- shiny::isolate(aligned_cache())[["g1"]]$paths
    expect_false(is.null(cached))
    expect_equal(unname(cached), unname(unlist(written)))
    expect_true(all(file.exists(cached)))

    lean <- dam_lean_group(rv$groups$g1, aligned = cached)
    expect_equal(unlist(lean$monitor_paths, use.names = FALSE), unname(cached))
    expect_false(identical(unlist(lean$monitor_paths, use.names = FALSE),
                           unlist(originals, use.names = FALSE)))

    # 3. a second run reuses the cache: no re-plan, no dialog
    pending_gids(character(0))
    shiny::isolate(tryCatch(request_run("g1"), error = function(e) NULL))
    expect_equal(planned, 1L)                      # NOT re-planned
    expect_length(pending_gids(), 0)               # dialog not re-raised

    # 4. a cached temp that has vanished falls back to re-planning rather than
    #    handing the worker a dead path
    unlink(cached[[1]])
    expect_false(all(file.exists(cached)))
    pending_gids(character(0))
    shiny::isolate(tryCatch(request_run("g1"), error = function(e) NULL))
    expect_equal(planned, 2L)                      # re-planned
    expect_true("g1" %in% pending_gids())          # and asked again
    expect_null(shiny::isolate(aligned_cache())[["g1"]])   # stale entry dropped
  })
})

test_that("the aligned-path cache drops entries when the originals change", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_group_manager_server"), "module not sourced")

  a <- .tmp(); b <- .tmp(); cc <- .tmp()
  for (p in c(a, b, cc)) .write_dam(p, n_min = 5)
  on.exit(unlink(c(a, b, cc)), add = TRUE)

  g <- new_group("g1", "G", "#0072B2")
  g$monitor_paths <- list(plug = a, middle = b, food = cc)
  rv <- shiny::reactiveValues(groups = list(g1 = g), selected = NULL,
                              log = character(0))

  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    shiny::isolate({
      dam_cache_put("g1", rv$groups$g1$monitor_paths,
                    c(plug = a, middle = b, food = cc))
      expect_equal(unname(dam_cache_get("g1", rv$groups$g1$monitor_paths)),
                   c(a, b, cc))

      # Same gid, different uploads -> the cached plan was computed against
      # other files, so it must not be reused.
      changed <- list(plug = cc, middle = b, food = a)
      expect_null(dam_cache_get("g1", changed))
      expect_null(aligned_cache()[["g1"]])
    })
  })
})
