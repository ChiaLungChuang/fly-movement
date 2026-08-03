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
    session$setInputs(method = "sm")
    expect_false(isTRUE(rv$groups$gA$config$use_anchored))

    session$setInputs(method = "anchor")
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
    session$setInputs(method = "sm")

    shown    <- use_anchored()                       # what the tab renders
    exported <- isTRUE(rv$groups$gA$config$use_anchored)  # what Downloads reads
    expect_equal(exported, shown)
    expect_false(exported)
  })
})
