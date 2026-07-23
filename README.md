# DAM Position + Sleep Pipeline

Combines three DAM monitors that watch the **same vials at different
positions** along the tube (plug end / middle / food end) so that sleep
bouts can be tied to *where* in the tube the fly was resting, and so that
movement between positions can be traced over time.

## Folder layout

```
Fly movement/
├── run_position.R        <- run this first (config at top)
├── run_activity.R        <- run this second (activity workbooks)
├── run_chronogram.R      <- optional (activity-over-time chronogram)
├── R/                    <- function library (sourced by the run scripts; no side effects)
│   ├── 01_read_qc.R        read monitors, detect + remove dead channels
│   ├── 02_position_zones.R state-machine zones, backfill, sleep bouts
│   ├── 03_anchor.R         anchor-based position + transit classification
│   ├── 04_plots.R          plots (single set, use_anchored flag)
│   ├── 05_export.R         Excel results workbook
│   └── 06_chronogram.R     activity chronogram helpers + generator
├── data/                 <- raw monitor files (Monitor7/8/9.txt)
├── output/               <- generated PDFs + XLSX land here
└── _original_backup/     <- your three original scripts, untouched
```

## How to run

1. Open `run_position.R`, edit the **CONFIG** block (monitor paths,
   `group_label`, `start_datetime` / `end_datetime`, thresholds,
   `lights_on` / `lights_off`, and `use_anchored`).
2. `source("run_position.R")` — reads data, assigns zones, detects sleep
   bouts, classifies transits, writes plots and `<group>_results.xlsx`.
   Leaves `dat`, `dead_summary`, `unknown_diag`, `anchors` in the session.
3. `source("run_activity.R")` — reuses `dat`/`dead_summary` (or sources
   `run_position.R` automatically if they are missing) and writes the 9
   activity workbooks to `output/Activity per monitor/`.

## Two position methods, one switch

Zones Z1 (plug) -> Z4 (food) are assigned two ways:

- **State machine** (`R/02`): fast carry-forward columns `zone`,
  `sleep_zone`, `bout_start`.
- **Anchor method** (`R/03`): physically rigorous columns `zone_anchored`,
  `sleep_zone_anchored`, `bout_start_anchored`, plus `transit_type` for
  every active bin. This is the one to use for tracing movement and
  sleep position.

The plot functions in `R/04` take a single `use_anchored` argument
(default `TRUE`) that selects which set of columns to visualize, so there
is only ever **one** copy of each plot function. (Previously two same-named
copies existed and silently overwrote each other depending on source order.)

## What changed vs the original scripts

- Function definitions and run/config are separated: `R/*.R` only defines,
  `run_*.R` configures and executes.
- The anchor functions are now sourced automatically (the old script had
  the `source()` line commented out, so the pipeline could not run
  end-to-end).
- The duplicate plot functions were merged into one `use_anchored`-driven set.
- Filenames cleaned (no spaces; `acitvity` typo fixed).
- Paths default to this project folder so it runs self-contained; change
  `output_dir` in the CONFIG blocks if you want results elsewhere.

## Activity chronogram (`run_chronogram.R`)

Activity-over-time (10-min bins, mean ± 95% CI per group). Set `mode` in
its CONFIG block:

- `"pipeline"` — builds the chronogram directly from this pipeline's `dat`
  via `make_activity_chronogram()` (no Rtivity needed). Sources
  `run_position.R` automatically if `dat` isn't in the session.
- `"rtivity"` — reads Rtivity's `Activity Chronogram.xlsx`, i.e. your
  original multi-group / multi-age (10D/30D/50D) workflow. Commented
  examples for the three ages are at the bottom of the script.

Output goes to `output/Chronogram/`. Note: the `Time_min` column holds
elapsed **seconds** (Rtivity's own convention; the x-axis divides by 3600).

## Known gaps

- `plot_individual_timelines` only draws active bins whose zone is
  resolved; anchor transit types `TRANSIT_PASS` / `INCONSISTENT` /
  `UNANCHORED` have `zone_anchored = NA` and are omitted from the path.
