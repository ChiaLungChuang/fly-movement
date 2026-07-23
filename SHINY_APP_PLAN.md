# Shiny App Build Plan — DAM Position + Sleep (Rtivity-like)

**Hand this file to Claude Code.** It is a build spec, not finished code.
Suggested kickoff prompt:

> Read `SHINY_APP_PLAN.md`. Implement Phase 0, then stop and let me test.
> Reuse the existing functions in `R/` as the analysis engine — do not
> rewrite them. Ask before changing any engine function signature.

---

## 1. Goal

Turn the existing DAM position/sleep/activity pipeline into a deployable
Shiny app, similar in feel to Rtivity, that supports **multiple labelled
groups** (genotypes / ages / conditions) and **compares them** on shared
chronograms and sleep-position summaries.

The scientific core already exists and is tested-by-use as scripts. The app
is a thin reactive UI on top of it.

## 2. Architecture principle (most important)

**Engine / UI separation.** The files in `R/` (`01_read_qc.R` …
`06_chronogram.R`) are pure function definitions with no side effects.
Shiny auto-sources everything in an app's `R/` directory at startup, so the
engine drops in as-is. The app layer only:

1. collects inputs (uploaded files + config),
2. calls the engine functions,
3. holds results in reactive state,
4. renders plots/tables and offers downloads.

Do **not** duplicate analysis logic into the server. If a calculation is
missing, add it as a new function in `R/`, not inline in `server`.

## 3. Target file tree

```
dam-shiny/
├── app.R                     # thin: sources nothing manually; calls run_app()/shinyApp
├── R/
│   ├── 01_read_qc.R          # ENGINE (copied from current project, unchanged)
│   ├── 02_position_zones.R   # ENGINE
│   ├── 03_anchor.R           # ENGINE
│   ├── 04_plots.R            # ENGINE (see §6 tweak: allow return-only, no ggsave)
│   ├── 05_export.R           # ENGINE
│   ├── 06_chronogram.R       # ENGINE (already returns objects)
│   ├── app_ui.R              # top-level UI shell (dashboard + tabs)
│   ├── app_server.R          # top-level server; wires modules + shared state
│   ├── mod_group_manager.R   # add/label/configure/run groups
│   ├── mod_qc.R              # dead channels + UNKNOWN diagnostics
│   ├── mod_position_sleep.R  # timelines + sleep-zone plots (per group)
│   ├── mod_activity.R        # activity summaries + workbook downloads
│   ├── mod_chronogram.R      # combined chronogram across groups
│   ├── mod_downloads.R       # bundle/zip all outputs
│   └── utils_state.R         # reactiveValues schema + helpers
├── inst/
│   └── extdata/              # small sample monitor files for demo/tests
├── tests/
│   ├── testthat/             # engine unit tests (testthat)
│   └── shinytest2/           # app-level tests
├── renv.lock                 # pinned dependencies
├── Dockerfile                # for Shiny Server / container deploy
├── config.yml                # non-secret defaults ({config} pkg)
└── README.md
```

Keep the current `run_position.R` / `run_activity.R` / `run_chronogram.R`
as-is in the original project — they stay useful for headless/batch runs and
as reference for the exact call order the app must reproduce.

## 4. Reactive state model

Single `reactiveValues` store (in `utils_state.R`), keyed by group id:

```r
rv <- reactiveValues(
  groups = list()   # named by group_id
)
# each groups[[id]] holds:
#   label          chr    e.g. "hTauV337M 50D"
#   color          chr    hex, for chronogram/summary
#   monitor_paths  list(plug=, middle=, food=)   (uploaded temp paths)
#   config         list(channels, start_datetime, end_datetime,
#                        lights_on, lights_off, tz,
#                        death_threshold_hours, sleep_threshold_min,
#                        look_window, bout_window_min, use_anchored)
#   status         chr    "configured" | "running" | "done" | "error"
#   # results (filled after a run):
#   monitors, dat, dead_summary, unknown_diag, anchors, summaries,
#   chrono_long
```

Derived reactives:

- `combined_chrono()` — `rbindlist` of each done group's `chrono_long`
  (already carries `Labels`); feeds the comparison chronogram.
- `combined_sleep()` — per-fly sleep-zone proportions across groups with a
  `Labels` column; feeds cross-group sleep-position comparison + stats.
- `group_colors()` — named vector `Labels -> color` for all plots.

The engine's `dat` is one long `data.table` per group keyed by
`channel + datetime`; sleep and position already live in the same table, so
comparisons are joins/binds on `Labels`.

## 5. UI — tabs (dashboard shell: `bs4Dash` preferred, `shinydashboard` fallback)

1. **Groups** (`mod_group_manager`)
   - Add group → upload 3 monitor files, assign role (plug/middle/food),
     label, color, and the config inputs (dates, lights on/off, tz,
     thresholds, `use_anchored`).
   - Per-group **Run** button; a table listing groups with status badges.
   - "Run all pending" convenience button.
2. **Data & QC** (`mod_qc`) — per selected group: dead-channel table
   (`dead_summary`), UNKNOWN diagnostics, transit-type distribution,
   anchor-confidence summary. All from existing outputs.
3. **Position & Sleep** (`mod_position_sleep`) — per group: individual
   movement timelines, sleep-zone heatmap, sleep-zone summary bar +
   summary heatmap. `use_anchored` toggle switches column set (engine
   already supports it).
4. **Activity** (`mod_activity`) — per group: the 9 activity summary tables
   (metric × grouping) as `DT::datatable`, each downloadable as the
   corresponding `.xlsx` workbook.
5. **Chronogram** (`mod_chronogram`) — **combined across groups**: activity
   chronogram (mean ± 95% CI) colored by `Labels`; monitor selector
   (plug/middle/food/summed), bin width, full-recording vs single-day
   (`filter_by_time`). Built from `make_activity_chronogram()` per group.
6. **Downloads** (`mod_downloads`) — zip bundle of all plots (PDF) + all
   workbooks + plotting-data CSVs for every done group.

Add a persistent **log/console** panel (collapsible) that captures the
engine's `cat()` output (`capture.output`) so users see progress messages.

## 6. Engine tweaks required (small, do first, ask before changing signatures)

1. **`R/04_plots.R` — make saving optional.** Each `plot_*` already returns
   the ggplot. Change so that when `output_file = NULL` (new default in the
   app path) it **skips `ggsave` and just returns `p`**. Shiny renders the
   returned object with `renderPlot`; downloads call `ggsave` inside a
   `downloadHandler`. Do not delete the file-saving path (batch scripts use it).
2. **Parametrize the timezone.** `tz = "America/Chicago"` is hardcoded in
   `read_position_monitors` (and the activity time-metadata block). Promote
   it to a `tz` argument / config input so deployed instances aren't wrong.
3. **Uploaded-file mapping.** `read_position_monitors` takes a named vector
   of paths; Shiny `fileInput` gives temp paths + original names. The group
   manager must build `c(plug=, middle=, food=)` from user role assignment.
4. **Activity time-metadata as a function.** The `day_night` /
   `experiment_day` block currently lives at top level in `run_activity.R`.
   Extract it into `R/06`-style function `add_time_metadata(dat, lights_on,
   lights_off, tz)` returning `dat`, so the app can call it without sourcing
   a run script. Likewise wrap the activity summary/export body into
   `build_activity_summaries()` and `write_activity_workbook()` functions.
5. **Progress hooks.** Wrap long calls (`assign_position_zones`,
   `assign_position_anchored`) in `withProgress`/`incProgress`. For
   responsiveness with multiple groups, use `ExtendedTask` + `future`
   (async) so the UI doesn't block — see Phase 4.

Keep every change backward-compatible with the existing `run_*.R` scripts.

## 7. Dependencies

Core: `shiny`, `bs4Dash` (or `shinydashboard`), `data.table`, `lubridate`,
`ggplot2`, `scales`, `openxlsx`, `readxl`, `dplyr`, `rlang`, `DT`,
`shinycssloaders`, `zip`, `config`.
Async/perf: `future`, `promises`.
Dev/test: `renv`, `testthat`, `shinytest2`.
Optional: `plotly` (interactive timelines), `golem` (only if you want a
package-style app — not required; plain modules are fine).

## 8. Deployment (must be deployable)

- **`renv`**: `renv::init()`, commit `renv.lock`.
- **No absolute paths.** All I/O via uploads and `tempfile()/tempdir()`
  scoped to the session. Remove every `/Users/cchuang/...` default; put
  demo paths only behind `inst/extdata`.
- **Upload limit**: `options(shiny.maxRequestSize = 250 * 1024^2)`.
- **`Dockerfile`** from `rocker/shiny` (or `rocker/r-ver` + `shiny-server`);
  restore with `renv::restore()`; expose 3838; copy app to
  `/srv/shiny-server/`.
- **`config.yml`** for non-secret defaults (default lights on/off, thresholds)
  read via `config::get()`; still overridable in the UI.
- Session isolation: never write to a shared path; each run uses a session
  temp dir. Memory: a full 32-channel multi-day `dat` is large — free group
  results the user removes.

## 9. Build phases (each independently testable)

- **Phase 0 — scaffold.** App skeleton (`app.R`, `app_ui.R`, `app_server.R`,
  empty modules), copy `R/` engine in, `renv` init, sample data in
  `inst/extdata`, app launches to an empty dashboard. Smoke test.
- **Phase 1 — single-group MVP.** One group: upload → configure → run
  position+sleep → view timelines + sleep-zone plots → download PDFs +
  `_results.xlsx`. Proves the engine wiring end to end.
- **Phase 2 — activity + chronogram (single group).** Activity summary
  tables + workbook downloads; chronogram tab via `make_activity_chronogram`.
- **Phase 3 — multi-group.** Group manager (add/remove/label/color), per-group
  runs, and the **combined** chronogram + cross-group sleep-position summary.
- **Phase 4 — async, polish, deploy.** `ExtendedTask`/`future` for non-blocking
  runs, progress bars, log panel, zip bundle, `Dockerfile`, tests, README.

## 10. Concrete task checklist for Claude Code

- [ ] Scaffold app + copy engine into `R/`; confirm Shiny auto-sources it.
- [ ] `utils_state.R`: define `rv` schema + `add_group()/remove_group()/`
      `set_group_results()` helpers.
- [ ] Engine tweak: `plot_*` return-only when `output_file = NULL`.
- [ ] Engine tweak: parametrize `tz`; extract `add_time_metadata()`,
      `build_activity_summaries()`, `write_activity_workbook()`.
- [ ] `mod_group_manager`: upload UI, role/label/color/config, Run button,
      status table; on run, call engine in the documented order and store
      results.
- [ ] `mod_qc`, `mod_position_sleep`, `mod_activity`, `mod_chronogram`,
      `mod_downloads` per §5.
- [ ] Wire modules in `app_server.R`; shared `rv`, `combined_*` reactives.
- [ ] `withProgress` around slow calls; capture `cat()` to log panel.
- [ ] Phase 4: async, `Dockerfile`, `renv.lock`, `config.yml`.
- [ ] Tests: `testthat` for engine (esp. `beam_to_zone`, `classify_transit`,
      `make_activity_chronogram`); `shinytest2` happy-path with sample data.

## 11. Engine call order the app must reproduce (from `run_position.R`)

```
monitors     <- read_position_monitors(monitor_files, channels, start_dt, end_dt, tz)
dead_summary <- detect_dead_channels(monitors, death_threshold_hours)
monitors     <- remove_dead_channels(monitors, dead_summary)
result       <- assign_position_zones(monitors)
result       <- backfill_and_detect_sleep(result, sleep_threshold_min)
dat          <- result$dat ; unknown_diag <- result$unknown_diag
anchor_result<- assign_position_anchored(dat, sleep_threshold_min, look_window)
dat          <- anchor_result$dat ; anchors <- anchor_result$anchors
# plots: plot_individual_timelines / plot_sleep_zone_* (use_anchored flag)
# excel: export_results_excel(dat, dead_summary, unknown_diag, anchors, label, file)
# activity: add_time_metadata -> build monitor tables -> detect_bouts ->
#           build_activity_summaries -> write_activity_workbook
# chronogram: make_activity_chronogram(dat, label, monitor, bin_min)
```

## 12. Gotchas / notes

- `plot_individual_timelines` omits unresolved transit bins
  (`zone_anchored = NA`); document this in the UI or add a fallback if users
  expect continuous paths.
- The chronogram `Time_min` column is **elapsed seconds**, not minutes
  (Rtivity convention). Keep it consistent app-wide.
- Multi-day windows: current default config is a single ~24 h window; the
  app should let users pick any window per group, and the chronogram/day
  filters assume seconds-from-start.
- Anchor pass is O(channels × timepoints) with per-channel loops — expect
  several seconds per group; this is why Phase 4 makes runs async.
- Keep group results independent; a bad file for one group must not abort
  the others (wrap each run in `tryCatch`, set `status = "error"`).
```
