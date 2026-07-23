# ============================================================
# app_server.R  —  top-level server: shared state + module wiring
# ============================================================

app_server <- function(input, output, session) {

  # Single shared reactive store (see utils_state.R)
  rv <- init_state()

  # ---- Combined (cross-group) reactives used by comparison tabs ----

  # Stack every done group's chronogram-format data (carries Labels).
  combined_chrono <- reactive({
    gs <- done_groups(rv)
    if (!length(gs)) return(NULL)
    data.table::rbindlist(
      lapply(gs, function(g) g$chrono_long),
      fill = TRUE
    )
  })

  # Per-fly sleep-zone rows across groups, tagged with Labels.
  combined_sleep <- reactive({
    gs <- done_groups(rv)
    if (!length(gs)) return(NULL)
    data.table::rbindlist(
      lapply(gs, function(g) {
        if (is.null(g$dat)) return(NULL)
        d <- g$dat[bout_start_anchored == TRUE,
                   .(channel, sleep_zone = sleep_zone_anchored)]
        d[, Labels := g$label]
        d
      }),
      fill = TRUE
    )
  })

  # ---- Module servers (each gets the shared store) ----
  mod_group_manager_server("groups",   rv, parent = session)
  mod_qc_server("qc",                  rv)
  mod_position_sleep_server("position", rv)
  mod_activity_server("activity",      rv)
  mod_chronogram_server("chrono",      rv, combined_chrono, combined_sleep)
  mod_downloads_server("downloads",    rv)
}
