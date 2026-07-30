# Selection identity in the group manager.
#
# The table hands the server a ROW INDEX. Storing that index as the selection
# desynchronises the moment the list changes underneath it:
#
#   [A, B, C], select B (index 2), remove B  ->  [A, C]
#   index 2 now points at C. In range, no error, and every "selected" action
#   silently operates on a group the user never chose.
#
# Removing the LAST group is the same bug with the index out of range, which
# throws inside a reactive and tears the session down. The crash is the loud
# case; the mis-selection is the dangerous one.
#
# Both tests below therefore assert on identity, not just on survival.

.mk_groups <- function(labels) {
  gs <- lapply(labels, function(l) new_group(paste0("g", l), l, "#0072B2"))
  names(gs) <- paste0("g", labels)
  gs
}

test_that("removing the selected last group leaves no selection and does not throw", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_group_manager_server"), "module not sourced")

  rv <- shiny::reactiveValues(groups = .mk_groups(c("A", "B")),
                              selected = NULL, log = character(0))

  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    session$setInputs(group_tbl_rows_selected = 2L)
    expect_equal(selected_gid(), "gB")

    session$setInputs(remove = 1L)
    expect_equal(names(rv$groups), "gA")

    # The mock session keeps the stale index at 2 (there is no browser to clear
    # it), which is exactly the race the proxy clear exists for: selected_gid()
    # re-executes on the flush while the client still believes row 2 is
    # selected. It must be total -- NULL, never a subscript error.
    expect_no_error(sel <- selected_gid())
    expect_null(sel)
  })
})

test_that("removing a middle group does not silently reselect the next one", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_group_manager_server"), "module not sourced")

  rv <- shiny::reactiveValues(groups = .mk_groups(c("A", "B", "C")),
                              selected = NULL, log = character(0))

  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    session$setInputs(group_tbl_rows_selected = 2L)
    expect_equal(selected_gid(), "gB")

    session$setInputs(remove = 1L)
    expect_equal(names(rv$groups), c("gA", "gC"))

    # Stale index 2 is now IN RANGE and points at gC. Resolving it would hand
    # "Run selected" / "Remove selected" a group the user never chose.
    sel <- selected_gid()
    expect_false(identical(sel, "gC"))
    expect_null(sel)

    # The stored selection itself is cleared, not merely masked by the group
    # having disappeared. Without this the server would be relying on the
    # client's redraw to drop the selection; asserting on selected_id()
    # distinguishes "we cleared it" from "selected_gid() happens to return
    # NULL because the group is gone".
    expect_null(selected_id())

    # rv$selected drives the per-group tabs; it must not point at the removed
    # group either.
    expect_false(identical(rv$selected, "gB"))
  })
})

test_that("a stale out-of-range index from the client resolves to no selection", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_group_manager_server"), "module not sourced")

  rv <- shiny::reactiveValues(groups = .mk_groups(c("A", "B")),
                              selected = NULL, log = character(0))

  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    # Start from a real selection, so an unguarded lookup leaves the OLD gid
    # in place rather than merely failing to set a new one. Asserting on the
    # resulting value is what makes this bite: an out-of-range index that
    # throws inside the observer would silently strand the previous selection.
    session$setInputs(group_tbl_rows_selected = 1L)
    expect_equal(selected_id(), "gA")

    # A desynced client reports a row that no longer exists.
    session$setInputs(group_tbl_rows_selected = 5L)
    expect_null(selected_id())
    expect_null(selected_gid())
  })
})

test_that("selection survives a removal that is not the selected group", {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not available")
  skip_if_not(exists("mod_group_manager_server"), "module not sourced")

  rv <- shiny::reactiveValues(groups = .mk_groups(c("A", "B", "C")),
                              selected = NULL, log = character(0))

  shiny::testServer(mod_group_manager_server, args = list(rv = rv), {
    # Select C (index 3), then remove A by selecting it first.
    session$setInputs(group_tbl_rows_selected = 3L)
    expect_equal(selected_gid(), "gC")

    session$setInputs(group_tbl_rows_selected = 1L)
    session$setInputs(remove = 1L)
    expect_equal(names(rv$groups), c("gB", "gC"))
    expect_null(selected_gid())
  })
})
