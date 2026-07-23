# ============================================================
# R/05_export.R  —  EXCEL EXPORT (function definition only)
#
# Provides:
#   export_results_excel()  — sleep-position results workbook
#
# Sheets: Per_fly, Group_summary, Dead_channels, UNKNOWN_diag,
#         Transit_summary, Anchor_summary, Transit_inconsistent,
#         Anchor_ambiguous
#
# Uses the anchor-method columns (sleep_zone_anchored etc.).
#
# Requires (loaded by the run script): data.table, openxlsx
# ============================================================

export_results_excel <- function(
    dat,
    dead_summary,
    unknown_diag,
    anchors,           # data.table returned by assign_position_anchored()
    group_label  = "All flies",
    output_file  = "sleep_zone_results.xlsx"
) {

  cat(sprintf("=== Exporting Excel: %s ===\n", basename(output_file)))

  channels_to_export <- sort(unique(dat$channel))
  sleep_dat <- dat[bout_start_anchored == TRUE & channel %in% channels_to_export]

  # -- Sheet 1: Per_fly -----------------------------------------
  per_fly <- sleep_dat[, .N, by = .(channel, sleep_zone_anchored)]
  setnames(per_fly, "sleep_zone_anchored", "sleep_zone")
  per_fly[, total      := sum(N), by = channel]
  per_fly[, proportion := round(N / total, 4)]

  all_combos <- CJ(channel    = channels_to_export,
                   sleep_zone = c("Z1","Z2","Z3","Z4"))
  per_fly <- merge(all_combos, per_fly,
                   by = c("channel","sleep_zone"), all.x = TRUE)
  per_fly[is.na(N),           N           := 0]
  per_fly[is.na(proportion),  proportion  := 0]
  per_fly[is.na(total),       total       := 0]

  per_fly_wide   <- dcast(per_fly, channel ~ sleep_zone,
                          value.var = "proportion", fill = 0)
  per_fly_wide_n <- dcast(per_fly, channel ~ paste0(sleep_zone, "_n"),
                          value.var = "N", fill = 0)
  per_fly_export <- merge(per_fly_wide, per_fly_wide_n, by = "channel")
  setorder(per_fly_export, channel)

  # -- Sheet 2: Group_summary -----------------------------------
  all_combos2 <- CJ(channel    = channels_to_export,
                    sleep_zone = c("Z1","Z2","Z3","Z4"))
  per_fly2 <- sleep_dat[, .N, by = .(channel, sleep_zone_anchored)]
  setnames(per_fly2, "sleep_zone_anchored", "sleep_zone")
  per_fly2 <- merge(all_combos2, per_fly2,
                    by = c("channel","sleep_zone"), all.x = TRUE)
  per_fly2[is.na(N), N := 0]
  per_fly2[, total      := sum(N), by = channel]
  per_fly2[, proportion := ifelse(total == 0, 0, N / total)]

  group_summary <- per_fly2[, .(
    n_flies   = .N,
    mean_prop = round(mean(proportion), 4),
    sem_prop  = round(sd(proportion) / sqrt(.N), 4),
    mean_pct  = round(mean(proportion) * 100, 2),
    sem_pct   = round(sd(proportion) / sqrt(.N) * 100, 2)
  ), by = sleep_zone]

  group_summary[, sleep_zone := factor(sleep_zone,
                                       levels = c("Z1","Z2","Z3","Z4"))]
  setorder(group_summary, sleep_zone)
  group_summary[, group := group_label]
  setcolorder(group_summary,
              c("group","sleep_zone","n_flies","mean_prop","sem_prop",
                "mean_pct","sem_pct"))

  # -- Sheet 3: Dead_channels -----------------------------------
  dead_export <- copy(dead_summary)
  dead_export[, death_datetime := as.character(death_datetime)]

  # -- Sheet 4: UNKNOWN_diag ------------------------------------
  unknown_export <- copy(unknown_diag)

  # -- Sheet 5: Transit_summary ---------------------------------
  n_rows_per_ch <- nrow(dat) / length(channels_to_export)

  transit_summary <- dat[!is.na(transit_type),
                         .N, by = .(channel, transit_type)]
  transit_summary[, total_rows   := n_rows_per_ch]
  transit_summary[, pct_of_ch   := round(100 * N / n_rows_per_ch, 2)]

  transit_wide <- dcast(transit_summary,
                        channel ~ transit_type,
                        value.var = "N", fill = 0)
  setorder(transit_wide, channel)

  transit_wide_pct <- dcast(transit_summary,
                            channel ~ paste0(transit_type, "_pct"),
                            value.var = "pct_of_ch", fill = 0)
  transit_export <- merge(transit_wide, transit_wide_pct, by = "channel")
  setorder(transit_export, channel)

  # -- Sheet 6: Anchor_summary ----------------------------------
  anchor_summary <- anchors[, .(
    n_anchors    = .N,
    n_sleep      = sum(anchor_type == "SLEEP"),
    n_stop       = sum(anchor_type == "STOP"),
    n_confirmed  = sum(confidence == "confirmed"),
    n_inf_before = sum(confidence == "inferred_before"),
    n_inf_after  = sum(confidence == "inferred_after"),
    n_before_only = sum(confidence == "before_only"),
    n_after_only  = sum(confidence == "after_only"),
    n_ambiguous  = sum(confidence == "ambiguous"),
    mean_dur_sleep = round(mean(duration_min[anchor_type == "SLEEP"],
                                na.rm = TRUE), 1),
    mean_dur_stop  = round(mean(duration_min[anchor_type == "STOP"],
                                na.rm = TRUE), 1)
  ), by = channel]
  setorder(anchor_summary, channel)

  # -- Sheet 7: Transit_inconsistent (row-level detail) ---------
  transit_incon <- dat[transit_type == "TRANSIT_INCONSISTENT",
                       .(datetime, channel,
                         B_plug, B_middle, B_food,
                         zone_anchored, transit_type,
                         anchor_type, anchor_zone)]
  transit_incon[, datetime := as.character(datetime)]
  setorder(transit_incon, channel, datetime)

  if (nrow(transit_incon) == 0) {
    transit_incon <- data.table(
      note = "No TRANSIT_INCONSISTENT bins detected — all transits resolved cleanly."
    )
  }

  # -- Sheet 8: Anchor_ambiguous (row-level detail) -------------
  anchor_ambig <- anchors[confidence == "ambiguous",
                          .(channel, start_idx, end_idx,
                            duration_min, anchor_type,
                            zone, confidence)]
  setorder(anchor_ambig, channel, start_idx)

  if (nrow(anchor_ambig) == 0) {
    anchor_ambig <- data.table(
      note = "No ambiguous anchors detected — all anchor zones resolved."
    )
  }

  # -- Write workbook -------------------------------------------
  wb <- createWorkbook()

  addWorksheet(wb, "Per_fly")
  writeData(wb, "Per_fly", per_fly_export)

  addWorksheet(wb, "Group_summary")
  writeData(wb, "Group_summary", group_summary)

  addWorksheet(wb, "Dead_channels")
  writeData(wb, "Dead_channels", dead_export)

  addWorksheet(wb, "UNKNOWN_diag")
  writeData(wb, "UNKNOWN_diag", unknown_export)

  addWorksheet(wb, "Transit_summary")
  writeData(wb, "Transit_summary", transit_export)

  addWorksheet(wb, "Anchor_summary")
  writeData(wb, "Anchor_summary", anchor_summary)

  addWorksheet(wb, "Transit_inconsistent")
  writeData(wb, "Transit_inconsistent", transit_incon)

  addWorksheet(wb, "Anchor_ambiguous")
  writeData(wb, "Anchor_ambiguous", anchor_ambig)

  saveWorkbook(wb, output_file, overwrite = TRUE)

  cat(sprintf("  Sheets written:\n"))
  cat(sprintf("    Per_fly, Group_summary, Dead_channels, UNKNOWN_diag\n"))
  cat(sprintf("    Transit_summary, Anchor_summary\n"))
  cat(sprintf("    Transit_inconsistent (%d flagged bins)\n",
              if (is.null(transit_incon$note)) nrow(transit_incon) else 0))
  cat(sprintf("    Anchor_ambiguous (%d flagged anchors)\n",
              if (is.null(anchor_ambig$note)) nrow(anchor_ambig) else 0))
  cat(sprintf("  Saved: %s\n", output_file))
  cat("=== Done ===\n\n")
}
