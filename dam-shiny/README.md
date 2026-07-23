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
   config (dates, timezone, lights on/off, channels, thresholds, bout window,
   anchor toggle), and **Run selected** / **Run all pending**. Status table +
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
form (timezone, lights on/off, thresholds, upload limit). It's read by a
minimal built-in parser in `R/app_config.R` — no `{config}` package needed.
Pick an alternate profile with the `DAM_CONFIG_PROFILE` environment variable;
missing keys fall back to `default`, then to hardcoded values.

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

The `testthat` suite covers `beam_to_zone`, `classify_transit`,
`make_activity_chronogram`/`summarize_activity`, `detect_dead_channels`,
`add_time_metadata`, `parse_channels`, the config reader, and a full
`run_group_pipeline` integration run on the bundled sample data. The
`shinytest2` test drives the live app (add demo group → run → check QC +
chronogram render) and skips itself when `shinytest2` isn't installed.

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
  instead of hardcoding the timezone.
- The `plot_*` functions skip `ggsave` and just return the ggplot when
  `output_file = NULL`; the file-saving path is unchanged for the scripts.
- Activity logic from `run_activity.R` is extracted into functions in
  `07_activity.R` (`add_time_metadata`, `build_activity_summaries`,
  `write_activity_workbook`, …). `run_activity.R` keeps its inline copy, so
  headless runs are unaffected.

## Notes / limits

- Runs are asynchronous (see **Async runs** above); the anchor pass is the slow
  part of a run. Multiple groups process sequentially in the background.
- No absolute paths; uploads use session temp files. Upload limit is
  configurable in `config.yml` (`max_upload_mb`, default 250).
- `Time_min` in chronogram data is elapsed **seconds** (Rtivity convention).
- Async runs (`ExtendedTask`/`future`) are the one remaining Phase 4 item not
  yet wired; see `SHINY_APP_PLAN.md` §9.
```
