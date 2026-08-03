# ============================================================
# mod_downloads.R  —  bundle all outputs (zip)
# For every done group: sleep/position PDFs, the results workbook,
# the 9 activity workbooks, and the chronogram CSV — zipped together.
# ============================================================

mod_downloads_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(width = 12, status = "primary", solidHeader = TRUE,
        title = "Downloads",
        p("Download a zip with every completed group's plots (PDF), the ",
          "sleep-position results workbook, the nine activity workbooks, and ",
          "chronogram plotting data (CSV)."),
        uiOutput(ns("status")),
        downloadButton(ns("bundle"), "Download all (.zip)", class = "btn-success")
    )
  )
}

mod_downloads_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    output$status <- renderUI({
      gs <- done_groups(rv)
      if (!length(gs)) return(tags$em("No completed groups yet. Run a group first."))

      d <- bundle_divergence(gs)
      tagList(
        tags$ul(lapply(gs, function(g) tags$li(g$label))),
        # Surfaced here as well as in the bundle: this catches it while the
        # user can still act, where the bundle file is what survives to
        # whoever opens the artefacts later. Neither alone is enough.
        if (d$any) tags$div(
          style = paste("border:1px solid #c00; background:#fdf2f2;",
                        "padding:8px; border-radius:4px; font-size:12px;"),
          tags$b("These groups differ in analysis parameters."),
          tags$div("Comparing them directly may be method-confounded:"),
          tags$ul(lapply(seq_len(nrow(d$table)), function(i) {
            r <- d$table[i, ]
            tags$li(tags$b(r[["Parameter"]]), ": ",
                    paste(sprintf("%s = %s", names(r)[-1], unlist(r[-1])),
                          collapse = "   |   "))
          })),
          tags$div(tags$em("The bundle records this in PROVENANCE.txt."))
        )
      )
    })

    output$bundle <- downloadHandler(
      filename = function() sprintf("dam_results_%s.zip", Sys.Date()),
      content  = function(file) {
        gs <- done_groups(rv)
        if (!length(gs)) {
          showNotification("Nothing to bundle — run a group first.", type = "warning")
          writeLines("No completed groups.", file)
          return()
        }

        root <- file.path(tempdir(), paste0("dam_bundle_", as.integer(Sys.time())))
        dir.create(root, showWarnings = FALSE, recursive = TRUE)

        withProgress(message = "Assembling bundle", value = 0, {
          n <- length(gs)
          for (i in seq_along(gs)) {
            g    <- gs[[i]]
            slug <- gsub("[^A-Za-z0-9]+", "_", g$label)
            gdir <- file.path(root, slug)
            dir.create(gdir, showWarnings = FALSE, recursive = TRUE)
            ua   <- g$config$use_anchored

            # -- Plots (PDF) --------------------------------------
            ggplot2::ggsave(file.path(gdir, paste0(slug, "_position_timelines.pdf")),
              plot_individual_timelines(g$dat, use_anchored = ua,
                lights_on = g$config$lights_on, lights_off = g$config$lights_off,
                output_file = NULL), width = 20, height = 12, limitsize = FALSE)
            ggplot2::ggsave(file.path(gdir, paste0(slug, "_sleep_zone_heatmap.pdf")),
              plot_sleep_zone_heatmap(g$dat, use_anchored = ua,
                group_label = g$label, output_file = NULL), width = 14, height = 6)
            ggplot2::ggsave(file.path(gdir, paste0(slug, "_sleep_zone_summary.pdf")),
              plot_sleep_zone_summary(g$dat, use_anchored = ua,
                group_label = g$label, output_file = NULL), width = 7, height = 5)
            ggplot2::ggsave(file.path(gdir, paste0(slug, "_sleep_zone_summary_heatmap.pdf")),
              plot_sleep_zone_summary_heatmap(g$dat, use_anchored = ua,
                group_label = g$label, output_file = NULL), width = 4, height = 6)

            # -- Results workbook --------------------------------
            # use_anchored passed EXPLICITLY. Its TRUE default exists for
            # run_position.R's benefit; relying on it here would silently
            # restore the anchor-only workbook this commit fixes.
            export_results_excel(
              dat = g$dat, dead_summary = g$dead_summary,
              unknown_diag = g$unknown_diag, anchors = g$anchors,
              group_label = g$label,
              use_anchored = ua,
              provenance = group_provenance_extra(g),
              output_file = file.path(gdir, paste0(slug, "_results.xlsx")))

            # -- Activity workbooks (9) --------------------------
            actdir <- file.path(gdir, "Activity")
            dir.create(actdir, showWarnings = FALSE)
            settings <- activity_settings_df(
              g$dat, g$dead_summary, g$label,
              g$config$lights_on, g$config$lights_off, g$config$bout_window_min)
            for (spec in activity_export_spec(slug)) {
              write_activity_workbook(g$summaries, spec$grouping, spec$metric,
                                      g$label, settings,
                                      file.path(actdir, spec$filename))
            }

            # -- Chronogram data (CSV) ---------------------------
            data.table::fwrite(g$chrono_long,
                               file.path(gdir, paste0(slug, "_chronogram_summed_10min.csv")))

            # -- Provenance (TXT) --------------------------------
            # Written for EVERY done group, aligned or not: a missing file
            # would have to be interpreted, and "no alignment happened" and
            # "nobody recorded whether it did" are different statements.
            prov <- group_provenance_df(g)
            writeLines(
              c(sprintf("Provenance -- %s", g$label), strrep("=", 60), "",
                sprintf("%-24s %s", prov$Parameter, prov$Value)),
              file.path(gdir, paste0(slug, "_provenance.txt")))

            incProgress(1 / n, detail = g$label)
          }
        })

        # Bundle-root record. Unconditional: it states either that the groups
        # agree or exactly how they differ, so the absence of a warning is
        # never something a reader has to interpret.
        writeLines(bundle_provenance_text(gs), file.path(root, "PROVENANCE.txt"))

        old <- setwd(root); on.exit(setwd(old), add = TRUE)
        zip::zipr(zipfile = file, files = list.files(root, recursive = FALSE))
      }
    )
  })
}
