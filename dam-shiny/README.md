# dam-shiny — DAM Position + Sleep (Rtivity-like)

A Shiny app on top of the DAM position/sleep/activity pipeline in `R/`. It
takes multiple labelled **groups** (genotypes / ages / conditions), runs the
existing analysis engine on each, and compares them on a shared activity
chronogram and sleep-position summaries — the Rtivity workflow, self-contained.

The engine (`01_read_qc.R` … `07_activity.R`) is the same tested code the batch
`run_*.R` scripts use; the app only collects inputs, calls the engine, holds
results in reactive state, and renders/downloads them.

## Run it

```r
install.packages(c("shiny","shinydashboard","DT","data.table","lubridate",
                   "ggplot2","scales","openxlsx","readxl","dplyr","rlang","zip"))
shiny::runApp("dam-shiny")   # or open dam-shiny/app.R and click "Run App"
```

Fastest way to see it work: on the **Groups** tab press **Add demo group**
(uses the bundled `inst/extdata/Monitor7/8/9.txt`), select the row, press
**Run selected**, then browse the other tabs.

## Tabs

1. **Groups** — add a group (3 monitor files + label/color), set the run
   config (lights on/off, channels, thresholds, bout window, anchor toggle),
   and **Run selected** / **Run all pending**. The **analysis period** is
   detected automatically from the earliest/latest timestamps in the uploaded
   files and can be narrowed with the date/time pickers. Status table +
   collapsible engine log.
2. **Data & QC** — per group: dead channels, UNKNOWN diagnostics, transit-type
   distribution, anchor-confidence summary.
3. **Position & Sleep** — per group: individual movement timelines, sleep-zone
   heatmap, and sleep-zone preference bar (anchor / state-machine toggle). Each
   downloadable as PDF.
4. **Activity** — per group: the activity summary tables (metric × grouping ×
   monitor) as a table, downloadable as the matching `.xlsx` workbook.
5. **Chronogram** — **combined across groups**: activity over time (mean ± 95%
   CI) colored by group, with signal (plug/middle/food/summed), bin width, and
   single-day filter. PDF + CSV download.
6. **Downloads** — one zip with every completed group's plots (PDF), results
   workbook, the nine activity workbooks, and chronogram CSV.

## Layout

```
dam-shiny/
├── app.R                 # entry point (Shiny auto-sources R/)
├── config.yml            # non-secret run defaults (read by R/app_config.R)
├── renv.lock             # pinned dependency versions
├── Dockerfile            # shiny-server image
├── .dockerignore
├── R/
│   ├── 01_read_qc.R … 06_chronogram.R   # ENGINE (tz parametrized; plot_* return-only when output_file=NULL)
│   ├── 07_activity.R     # ENGINE: activity summaries extracted from run_activity.R
│   ├── app_config.R      # tiny config.yml reader (no {config} dependency)
│   ├── utils_state.R     # reactive store, group helpers, run_group_pipeline()
│   ├── app_ui.R          # dashboard shell
│   ├── app_server.R      # shared state + combined reactives + module wiring
│   └── mod_*.R           # one module per tab
├── inst/extdata/         # sample Monitor7/8/9.txt for the demo
└── tests/
    ├── testthat.R        # runner: Rscript tests/testthat.R
    ├── testthat/         # engine unit + integration tests
    └── shinytest2/       # app-level happy-path test

../.github/workflows/dam-shiny-tests.yaml   # CI (lives at the repo root)
```

## Configuration

`config.yml` holds the non-secret defaults that prefill the **Run settings**
form (lights on/off, channels, thresholds, upload limit). It's read by a
minimal built-in parser in `R/app_config.R` — no `{config}` package needed.
Pick an alternate profile with the `DAM_CONFIG_PROFILE` environment variable;
missing keys fall back to `default`, then to hardcoded values.

The **analysis period is not configured here** — it is detected from the
uploaded files. Timestamps are parsed as naive wall-clock (`tz` is internal,
fixed to `UTC`), so the dates/times shown always match the files with no
timezone shift; there is no timezone control in the UI.

## Zone methods — both always run

Two independent zone assignments are computed for **every** group, in this
order, and neither is conditional:

```
assign_position_zones()     -> zone / sleep_zone / bout_start          (state machine)
assign_position_anchored()  -> zone_anchored / sleep_zone_anchored / … (anchor method)
```

`use_anchored` selects **which column set is displayed and exported**, not
which computation runs. Nothing in `run_group_pipeline()` branches on it.

This matters when reading the Engine log. The line

```
Assigning position zones (state machine)
```

names the step that is running at that moment; the anchor pass is logged
separately a few steps later ("Anchor-based position assignment"). Seeing the
state-machine line does **not** mean the anchor method was skipped — it never
is.

### Where the method is chosen

The group's `config$use_anchored` is the single source of truth (since
`f9d5e5e`). Two controls write it:

| Control | Effect |
|---|---|
| "Use anchor method for plots & exports" checkbox, Groups tab | Seeds the value for **newly added** groups, like every other run setting in that panel |
| "Zone method for outputs" radio, Position & Sleep tab | **Initialises from** the focused group's config, and **writes back** to it when changed |

The radio is not view-only. Selecting a different group re-syncs the radio to
that group's stored value; changing the radio edits the group. Both the
on-screen plots and the Downloads bundle read `config$use_anchored`, so they
cannot disagree.

Merely *viewing* a group does not modify it. The write-back observer keys on
the radio input alone, so a group switch cannot trigger a write.

## Provenance in exports

Every bundle records how it was made.

| Artefact | Contents |
|---|---|
| `PROVENANCE.txt` (bundle root) | Written unconditionally. States either "All groups share identical analysis parameters" or a per-parameter table of exactly how they differ, with each group's own value. |
| `<group>_provenance.txt` | Written for **every** completed group. Zone method, source files (original upload names, not Shiny datapaths), alignment status and report, analysis window, channels, thresholds. |
| `Provenance` sheet in `results.xlsx` | The same record inside the workbook, so a file that gets emailed on its own still says what produced it. |

A group that needed no alignment gets an explicit `not required — files already
matched` line rather than no line: absence of a record must never be something
a reader has to interpret.

The divergence check compares zone method, alignment status, death and sleep
thresholds, lights on/off, channels, and bout window. Divergence is also shown
in the Downloads tab before download — that surface catches it while you can
still act, the bundle file is what survives to whoever opens the artefacts
later.

### Reproducibility warning — workbooks predating `8f68538`

**This is a numbers change, not a labelling change.**

Before commit `8f68538`, `export_results_excel()` read `sleep_zone_anchored`,
`zone_anchored` and `bout_start_anchored` unconditionally, ignoring
`use_anchored` entirely. So:

- Any `results.xlsx` **without** a `Provenance` sheet is **anchor-method by
  construction**, whatever the group's setting was at the time. That rule is
  the only way to interpret such a file.
- For a **state-machine** group, `Per_fly` and `Group_summary` were derived
  from the anchor columns. The same input re-exported after `8f68538` will
  contain **different per-fly zone counts**. Both files are internally
  consistent; they report different things.
- Anchor-method groups are unaffected — the new default reproduces the old
  behaviour exactly.

## Duplicated engine files

Parts of the engine exist as independent copies, not a shared library. Nothing
makes an edit to one reach another.

| Copy | In the agreement set? |
|---|---|
| `dam-shiny/R/05_export.R` | **yes** — sourced by the app |
| `R/05_export.R` (repo root) | **yes** — sourced by `run_position.R` |
| `_original_backup/DAM position pipeline.R` | no — pre-modular archive, frozen by intent |
| `~/Desktop/DAM position/DAM position pipeline.R` | no — separate project; its body already differs (different lineage, not drift) |
| `~/Desktop/DAM alinger/dam-shiny-integration/` | no — standalone repo, no remote, superseded staging work already imported here |

The agreement set is exactly `{R/05_export.R, dam-shiny/R/05_export.R}`. Check
it with:

```sh
Rscript dam-shiny/tools/engine_copy_diff.R
```

Exit 0 when the set is identical, 1 on drift or a missing file, 2 on bad usage.
Kept out of the test suite deliberately — it is a repo-layout invariant, and
the suite runs against `dam-shiny/` alone.

Two files in root `R/` have **deliberately** forked and are classified FORKED
rather than checked. Syncing them would break the app:

| File | Why it differs |
|---|---|
| `01_read_qc.R` | app adds `tz` and `source_names` (upload-name logging) |
| `04_plots.R` | app adds `output_file = NULL` support for return-only plots |

Any file in root `R/` appearing in neither list **fails** the check, so a new
engine file cannot be added without someone classifying it.

Root `R/` has not been modified since the initial commit. De-duplicating
properly is a queued ticket (see **Known limitations**), not done.

## Async runs

Group runs execute in a **background R process** (`ExtendedTask` + `future`),
so the UI never freezes — you can browse other tabs, add more groups, or
inspect earlier results while a run is in flight.

- A single `ExtendedTask` queues invocations FIFO, so **Run all pending**
  submits every pending group and they process one after another.
- The status column distinguishes `queued` → `running` → `done` / `error`.
- The worker receives only plain data (label, config, file paths) and sources
  the engine itself; no functions or reactives cross the process boundary, and
  result objects are never sent *to* the worker.
- Engine console output is collected in the worker and replayed into the
  **Engine log** panel when the run returns.
- Worker count is capped in `app.R` (`min(4, availableCores() - 1)`) to leave a
  core for the app itself.

Because the work happens in another process, there is no fine-grained progress
bar — a run reports `running`, then completes. Expect a few seconds of worker
startup overhead per run on top of the analysis time.

## Tests

```r
Rscript tests/testthat.R                               # engine unit + integration (fast)
Rscript -e 'shinytest2::test_app("tests/shinytest2")'  # app happy-path (needs shinytest2 + chromote)
```

The `testthat` suite (**221 passing**) covers the engine primitives
(`beam_to_zone`, `classify_transit`,
`make_activity_chronogram`/`summarize_activity`, `detect_dead_channels`,
`add_time_metadata`, `parse_channels`, the config reader), a full
`run_group_pipeline` integration run on the bundled sample data, and the
app-level behaviour added since: alignment integration and the missing-path
guard, group-selection identity, the empty-channel guard, upload-name logging,
zone-method single-source-of-truth, workbook method selection, and provenance.
The `shinytest2` test drives the live app (add demo group → run → check QC +
chronogram render) and skips itself when `shinytest2` isn't installed.

### Coverage honesty

New guards are mutation-tested: the guard is removed, the suite re-run, and the
change kept only if a test actually fails. Two guards survive that check
**uncovered**, and are commented as such in the source rather than left to look
tested:

- the **compare-and-skip** in the zone-method write-back
  (`mod_position_sleep.R`) is a thrash guard, not correctness — the client echo
  carries the value already stored, so removing it causes a redundant write,
  not a wrong one;
- **`ignoreInit`** on the same observer matters only in a browser, where the
  radio always holds a value. `testServer` leaves the input `NULL` until set
  and `observeEvent`'s `ignoreNULL` default swallows it, so no test can
  distinguish it.

Two `testServer` limits worth knowing before writing module tests:

- **Stubbing a function that the module resolves in its own environment does
  not work.** Rebinding `run_one` inside a `testServer` block does not reach
  `request_run`, and `<<-` from there writes to the global environment rather
  than the test's. Assert on observable module state instead (`queue()`,
  `pending_gids()`, `rv$groups`).
- **A module must be sourced in `tests/testthat/setup.R` to be testable.**
  Three zone-method tests initially reported green while silently *skipping*,
  because `mod_position_sleep.R` was not sourced.

### Continuous integration

`.github/workflows/dam-shiny-tests.yaml` (at the **repository root**, one level
above this folder) restores `renv.lock`, runs the `testthat` suite, and checks
that the app object constructs, on every push/PR touching `dam-shiny/`. It
activates once the project is a git repository pushed to GitHub.

## Deployment (Docker)

```sh
docker build -t dam-shiny .
docker run --rm -p 3838:3838 dam-shiny
# open http://localhost:3838/dam-shiny/
```

The image is based on `rocker/shiny`, restores exact package versions from
`renv.lock` (reusing the base image's binary package repo for a fast build),
and serves via shiny-server on port 3838. No absolute paths; uploads use
session temp files.

## Engine changes (backward-compatible with the batch `run_*.R` scripts)

- `read_position_monitors()` takes a `tz` argument (default `America/Chicago`)
  instead of hardcoding the timezone; the app passes `tz = "UTC"`.
- `read_monitor_datetimes()` / `detect_datetime_range()` (in `01_read_qc.R`)
  parse just the datetime column to find a file set's overlapping recording
  window — used by the Groups tab to auto-fill the analysis period.
- The `plot_*` functions skip `ggsave` and just return the ggplot when
  `output_file = NULL`; the file-saving path is unchanged for the scripts.
- Activity logic from `run_activity.R` is extracted into functions in
  `07_activity.R` (`add_time_metadata`, `build_activity_summaries`,
  `write_activity_workbook`, …). `run_activity.R` keeps its inline copy, so
  headless runs are unaffected.
- `export_results_excel()` takes `use_anchored` (default `TRUE`) and an
  optional `provenance` data frame, and always writes a `Provenance` sheet.
  The default reproduces the pre-`8f68538` behaviour exactly, so
  `run_position.R` is unchanged until it opts in. **Both copies of
  `05_export.R` carry this** — see **Duplicated engine files**.

## Known limitations and open questions

### Limitations

- **Duplicated engine files.** `05_export.R` exists as two independent copies
  that must be edited together (see **Duplicated engine files**).
  `engine_copy_diff.R` detects drift; it does not prevent it.
  De-duplicating properly — one copy, sourced by both the app and
  `run_position.R` — is a queued ticket.
- **Workbooks predating `8f68538`** have no `Provenance` sheet and are
  anchor-method by construction. For a state-machine group the per-fly zone
  counts differ from a re-export. See the reproducibility warning above.
- **Two guards are uncovered by tests** (the thrash guard and `ignoreInit`);
  see **Coverage honesty**.
- **A run where every channel is classified dead stops with an error** rather
  than returning a result. The per-channel detail is still printed to the
  Engine log, but the group never reaches `done`, so it does not appear in the
  QC tab or in Downloads. Raise the death threshold to inspect such a
  recording.
- **Death threshold is capped at 72 h** by the input's `max`. There is no
  server-side clamp, so a typed value above it may still reach the engine
  depending on the browser, but the control presents 72 as the ceiling.
- The anchor pass is the slow part of a run. Multiple groups process
  sequentially in the background.
- No absolute paths; uploads use session temp files. Upload limit is
  configurable in `config.yml` (`max_upload_mb`, default 250).
- `Time_min` in chronogram data is elapsed **seconds** (Rtivity convention).

### Open questions

- **The 5-minute pre-window offset is unresolved.** `read_position_monitors()`
  discards rows before `common_start + 300 s`, while
  `assign_position_zones()` separately trims to the user's `analysis_start`.
  With the period selector's default (the common file start) the trim removes
  nothing, so the first five minutes are dropped and the state machine gets no
  warm-up. Verified: the trim removes 0 rows in that case and invents none, so
  **nothing is padded and no inactivity run can be inflated by it** — but the
  logged "Analysis start" names a timestamp that is not in the output.
  Whether the offset is a settling discard, a warm-up lead-in, or two
  mechanisms that disagree in the default case is a biology question and is
  **not settled**. Do not treat the current behaviour as intentional or as a
  bug until it is decided.
- **A queued run's result can in principle be dropped.** When two or more runs
  are queued on the single `ExtendedTask`, the transition to `success` for an
  earlier run can be overwritten by the next run's `running` before Shiny
  flushes, so the observer never collects it. Reproduced in isolation; **not
  observed in the live app**, where a real pipeline takes seconds. Parked.
