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
      if (!length(gs)) {
        tags$em("No completed groups yet. Run a group first.")
      } else {
        tags$ul(lapply(gs, function(g) tags$li(g$label)))
      }
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
            export_results_excel(
              dat = g$dat, dead_summary = g$dead_summary,
              unknown_diag = g$unknown_diag, anchors = g$anchors,
              group_label = g$label,
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

            incProgress(1 / n, detail = g$label)
          }
        })

        old <- setwd(root); on.exit(setwd(old), add = TRUE)
        zip::zipr(zipfile = file, files = list.files(root, recursive = FALSE))
      }
    )
  })
}
