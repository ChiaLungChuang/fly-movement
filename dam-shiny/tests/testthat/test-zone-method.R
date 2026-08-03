# One zone method per group, one source of truth.
#
# Both zone methods run on every group unconditionally; use_anchored selects
# which columns the OUTPUTS read. Before this change there were two
# independent selectors -- the Position & Sleep tab's radio (input$method) and
# the group's config$use_anchored, which the Downloads bundle reads -- so a
# user could view anchor-method plots and download state-machine PDFs. The
# only distinguishing mark was the method_tag in the plot title, which does not
# survive panel cropping during figure assembly.
#
# The group's config is now the single source of truth; the tab radio edits it.
# That makes the initialisation hazard below the most dangerous failure mode in
# the change: a naive write-back stamps "anchor" onto every group merely VIEWED.

.done_group <- function(gid, label, use_anchored) {
  g <- new_group(gid, label, "#0072B2")
  g$config$use_anchored <- use_anchored
  g$status <- "done"
  # group_choices(only_done) and grp() both require results to be present.
  g$dat <- data.table::data.table(channel = 1L,
                                  datetime = as.POSIXct("2026-01-28 15:12:00",
                                                        tz = "UTC"))
  g
}

test_that("the tab radio writes the method back to the group's config", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_position_sleep_server"), "module not sourced")

  rv <- shiny::reactiveValues(
    groups = list(gA = .done_group("gA", "A", TRUE)),
    selected = "gA", log = character(0))

  shiny::testServer(mod_position_sleep_server, args = list(rv = rv), {
    session$setInputs(group = "gA")
    session$setInputs(method_gA = "sm")
    expect_false(isTRUE(rv$groups$gA$config$use_anchored))

    session$setInputs(method_gA = "anchor")
    expect_true(isTRUE(rv$groups$gA$config$use_anchored))
  })
})

test_that("merely selecting a group in the tab does not modify its config", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_position_sleep_server"), "module not sourced")

  # gA is anchor, gB is state-machine. Viewing gB must not stamp gA's method
  # (or the radio's default) onto it.
  rv <- shiny::reactiveValues(
    groups = list(gA = .done_group("gA", "A", TRUE),
                  gB = .done_group("gB", "B", FALSE)),
    selected = "gA", log = character(0))

  shiny::testServer(mod_position_sleep_server, args = list(rv = rv), {
    session$setInputs(group = "gA")
    expect_true(isTRUE(rv$groups$gA$config$use_anchored))

    # Switch to gB. The radio is still showing gA's method at this instant.
    session$setInputs(group = "gB")
    expect_false(isTRUE(rv$groups$gB$config$use_anchored))   # NOT overwritten
    expect_true(isTRUE(rv$groups$gA$config$use_anchored))    # gA untouched

    # And back again, both still intact.
    session$setInputs(group = "gA")
    expect_true(isTRUE(rv$groups$gA$config$use_anchored))
    expect_false(isTRUE(rv$groups$gB$config$use_anchored))
  })
})

test_that("the export method matches the method shown on the tab", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_position_sleep_server"), "module not sourced")

  # The regression: tab set to state machine, group config left at anchor.
  # mod_downloads reads g$config$use_anchored, so before the fix the bundle
  # would render anchor-method PDFs for a view showing state machine.
  rv <- shiny::reactiveValues(
    groups = list(gA = .done_group("gA", "A", TRUE)),
    selected = "gA", log = character(0))

  shiny::testServer(mod_position_sleep_server, args = list(rv = rv), {
    session$setInputs(group = "gA")
    session$setInputs(method_gA = "sm")

    shown    <- use_anchored()                       # what the tab renders
    exported <- isTRUE(rv$groups$gA$config$use_anchored)  # what Downloads reads
    expect_equal(exported, shown)
    expect_false(exported)
  })
})


# -- the radio value must be bound to the group it came from ------------------
# A shared `method` input carries no group identity, so the write-back had to
# read input$group at fire time and assume the two agreed. They are independent
# inputs. Two arrival patterns break that assumption:
#
#   batched   a radio edit and a group switch applied in one flush
#   echo      updateRadioButtons round-trips through the browser, so the sync
#             for group B returns as an input$method event that can land after
#             the user has already moved to group C -- no radio click at all
#
# compare-and-skip cannot help in either case: it fires exactly when the values
# differ, which is exactly when the write is wrong.
#
# The fix gives each group its own input id, so a late value is attributed to
# the group it belongs to. These first two tests deliver a value through the
# OLD unbound name and assert it changes nothing -- that a method value not
# bound to a group cannot touch whichever group happens to be selected.

.set_group_method <- function(session, gid, value) {
  args <- list(); args[[paste0("method_", gid)]] <- value
  do.call(session$setInputs, args)
}

test_that("an unbound method value cannot retarget the selected group", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_position_sleep_server"), "module not sourced")

  rv <- shiny::reactiveValues(
    groups = list(gA = .done_group("gA", "A", TRUE),
                  gB = .done_group("gB", "B", TRUE)),
    selected = "gA", log = character(0))

  shiny::testServer(mod_position_sleep_server, args = list(rv = rv), {
    session$setInputs(group = "gA")
    # A radio edit and a group switch applied together, as one flush would.
    session$setInputs(method = "sm", group = "gB")
    expect_true(isTRUE(rv$groups$gB$config$use_anchored))   # B untouched
  })
})

test_that("a late sync echo cannot retarget a group the user moved on to", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_position_sleep_server"), "module not sourced")

  # A=anchor, B=state machine, C=anchor. Switching A -> B -> C faster than B's
  # sync echo returns delivers "sm" while C is selected. The user never touched
  # the radio.
  rv <- shiny::reactiveValues(
    groups = list(gA = .done_group("gA", "A", TRUE),
                  gB = .done_group("gB", "B", FALSE),
                  gC = .done_group("gC", "C", TRUE)),
    selected = "gA", log = character(0))

  shiny::testServer(mod_position_sleep_server, args = list(rv = rv), {
    session$setInputs(group = "gA")
    session$setInputs(group = "gB")
    session$setInputs(group = "gC")
    session$setInputs(method = "sm")     # B's echo, arriving late

    expect_true(isTRUE(rv$groups$gC$config$use_anchored))    # C untouched
    expect_false(isTRUE(rv$groups$gB$config$use_anchored))   # B keeps its own
  })
})

test_that("each group's own control edits that group and no other", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_position_sleep_server"), "module not sourced")

  rv <- shiny::reactiveValues(
    groups = list(gA = .done_group("gA", "A", TRUE),
                  gB = .done_group("gB", "B", TRUE)),
    selected = "gA", log = character(0))

  shiny::testServer(mod_position_sleep_server, args = list(rv = rv), {
    session$setInputs(group = "gA")
    .set_group_method(session, "gA", "sm")
    expect_false(isTRUE(rv$groups$gA$config$use_anchored))   # A edited
    expect_true(isTRUE(rv$groups$gB$config$use_anchored))    # B untouched

    # A value for A arriving while B is selected still lands on A.
    session$setInputs(group = "gB")
    .set_group_method(session, "gA", "anchor")
    expect_true(isTRUE(rv$groups$gA$config$use_anchored))
    expect_true(isTRUE(rv$groups$gB$config$use_anchored))
  })
})


# -- dynamic-UI hazards ------------------------------------------------------
# Rendering the control per group invites two mistakes that a value-only
# assertion cannot see.

test_that("one edit writes once, however many times groups were switched", {
  # If the writer were created inside renderUI, an observer would accumulate
  # per group switch and a single edit would write once per accumulated
  # observer. That is terminating and invisible -- the writes agree -- so it
  # can only be caught by counting.
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_position_sleep_server"), "module not sourced")

  rv <- shiny::reactiveValues(
    groups = list(gA = .done_group("gA", "A", TRUE),
                  gB = .done_group("gB", "B", TRUE),
                  gC = .done_group("gC", "C", TRUE)),
    selected = "gA", log = character(0))

  shiny::testServer(mod_position_sleep_server, args = list(rv = rv), {
    for (i in 1:4) {
      # Touch the rendered control on every switch. Outputs are lazy in
      # testServer, so without this renderUI never runs and an observer created
      # inside it would never be created either -- the test would pass by not
      # exercising the thing it is meant to catch.
      for (gid in c("gA", "gB", "gC")) {
        session$setInputs(group = gid)
        invisible(output$method_ui)
      }
    }
    expect_equal(method_writes(), 0L)      # switching alone writes nothing

    session$setInputs(group = "gA")
    .set_group_method(session, "gA", "sm") # one user edit
    expect_equal(method_writes(), 1L)
    expect_false(isTRUE(rv$groups$gA$config$use_anchored))
  })
})

test_that("a value for a removed group is dropped, not resurrected", {
  # Shiny keeps input values for UI that has gone away, so method_<gid>
  # outlives its group. 17c893b was exactly this class of bug for the group
  # selection, so pin it rather than reason about it.
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_position_sleep_server"), "module not sourced")

  rv <- shiny::reactiveValues(
    groups = list(gA = .done_group("gA", "A", TRUE),
                  gB = .done_group("gB", "B", TRUE)),
    selected = "gA", log = character(0))

  shiny::testServer(mod_position_sleep_server, args = list(rv = rv), {
    session$setInputs(group = "gB")
    .set_group_method(session, "gB", "sm")
    expect_false(isTRUE(rv$groups$gB$config$use_anchored))
    n_before <- method_writes()

    rv$groups[["gB"]] <- NULL              # group removed
    session$setInputs(group = "gA")

    # A late value on the dead id: no error, no write, and no config entry
    # conjured for a group that no longer exists.
    expect_no_error(.set_group_method(session, "gB", "anchor"))
    expect_null(rv$groups[["gB"]])
    expect_false("gB" %in% names(rv$groups))
    expect_equal(method_writes(), n_before)
    expect_true(isTRUE(rv$groups$gA$config$use_anchored))   # gA untouched
  })
})


test_that("the rendered control carries the selected group's id and value", {
  # The tests above set method_<gid> directly, so they would still pass if
  # renderUI emitted the wrong id. Assert what is actually rendered.
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_position_sleep_server"), "module not sourced")

  rv <- shiny::reactiveValues(
    groups = list(gA = .done_group("gA", "A", TRUE),
                  gB = .done_group("gB", "B", FALSE)),
    selected = "gA", log = character(0))

  shiny::testServer(mod_position_sleep_server, args = list(rv = rv), {
    session$setInputs(group = "gA")
    html <- as.character(output$method_ui$html)
    expect_match(html, "method_gA", fixed = TRUE)
    expect_false(grepl("method_gB", html, fixed = TRUE))
    # Initialised from the group's stored value, not from a default.
    expect_match(html, 'value="anchor"[^>]*checked')

    session$setInputs(group = "gB")
    html <- as.character(output$method_ui$html)
    expect_match(html, "method_gB", fixed = TRUE)
    expect_false(grepl("method_gA", html, fixed = TRUE))
    expect_match(html, 'value="sm"[^>]*checked')
  })
})
