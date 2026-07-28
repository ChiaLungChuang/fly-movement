# ============================================================
# R/00_dam_align.R  --  DAM file alignment (vendored from damalignr)
#
# ---- PROVENANCE ----
#   Source repo : https://github.com/ChiaLungChuang/damalignr
#   Commit      : 4c73a76f8932c820b10fe8c702b04cd9db0615d8  (2026-07-08,
#                 "Initial commit: damalignr package + Shiny aligner app")
#   Vendored    : 2026-07-23 (this repo's commit 68ea306)
#   Status      : ADAPTED, not verbatim -- do not expect a clean diff.
#
# Verified on 2026-07-28 against that commit, function body by function
# body (parse + deparse, so formatting and comments were not compared).
# What the check found:
#
#   identical to upstream
#     .dam_read_raw       <- read_dam_raw
#     .dam_subrow_order   <- .subrow_order
#     %||%                <- %||%
#
#   adapted, same behaviour -- `file_paths` -> `roles` for labelling,
#   sapply -> vapply, cat() progress chatter removed (the app shows a
#   confirmation modal instead), errors reworded with call. = FALSE
#     .dam_subrow_mismatches   <- .diagnose_subrow_mismatches
#     .dam_first_misalignment  <- .diagnose_alignment
#     .dam_trim_to_common_end  <- trim_to_common_end
#     .dam_nearest_template    <- .nearest_template
#     .dam_match_final         <- match_final_timestamp_rows
#
#   adapted, behaviour deliberately changed
#     .dam_filter_start <- filter_start
#         upstream takes a file path and re-reads it, matching on V1+V2+V3;
#         this takes an in-memory data.table and matches on V2+V3 only (V1
#         has been renumbered by the time it is called).
#     .dam_repair_missing_subrows <- repair_missing_subrows
#         `verbose` dropped; insert count accumulated and returned as
#         attr(out, "n_inserted"), which the modal reports.
#
#   no upstream counterpart -- written for this app, replacing upstream's
#   diagnose_monitor_files() (prints, returns a data.table with attributes)
#   and process_monitor_files() (writes *_processed.txt beside the inputs)
#     .dam_diagnose_list, .dam_parse_ts, dam_plan_alignment, dam_write_aligned
#
#   present upstream, intentionally not vendored
#     diagnose_monitor_files, process_monitor_files, run_aligner_app
#
# So: the primitives above are current as of 4c73a76, but this file cannot
# be refreshed by copying upstream over it. To check for staleness, diff
# the named upstream functions against their counterparts here and re-apply
# the adaptations. At the time of verification 4c73a76 was the only commit
# in the upstream repo and matched origin/main.
#
# Self-contained copy of the damalignr alignment engine, vendored into
# this app so it deploys with the Docker/renv image without a dependency
# on the (private) damalignr GitHub package.
#
# Public entry points used by the app:
#   dam_plan_alignment(mf)         -> diagnose + (if needed) align IN MEMORY,
#                                     returning before/after row counts and a
#                                     plain-English report. Does NOT write files
#                                     or mutate anything -- safe to call for the
#                                     confirmation dialog.
#   dam_write_aligned(plan, dir)   -> write the already-computed aligned tables
#                                     to temp files; returns named file paths.
#
# Why this exists: read_position_monitors() (R/01_read_qc.R) hard-stops when
# the three monitors have different row counts, and reads with a bare fread()
# that silently truncates at a blank line left by a dropped sub-row. This
# module diagnoses those problems and repairs them (staggered start/end,
# partial final minute, mid-file dropped sub-row) so the three files cover the
# same window with matching rows before the engine reads them.
#
# Only base R + data.table are used (data.table is already a hard dependency
# of this app). No code runs at source() time -- definitions only.
# ============================================================


# ---- integrity-aware reader --------------------------------------------
# Unlike bare fread(), this reads past blank/malformed lines instead of
# stopping at them, and reports where they were.
.dam_read_raw <- function(file_path, expected_cols = 42L) {
  lines    <- readLines(file_path, warn = FALSE)
  is_blank <- trimws(lines) == ""
  blank_lines <- which(is_blank)
  nonblank <- lines[!is_blank]

  if (length(nonblank) == 0) {
    return(list(data = data.table::data.table(), blank_lines = blank_lines,
                malformed_lines = integer(0), n_lines = length(lines)))
  }

  nfields <- lengths(strsplit(nonblank, "\t", fixed = TRUE))
  malformed_lines <- which(!is_blank)[nfields != expected_cols]

  d <- data.table::fread(text = paste(nonblank, collapse = "\n"),
                         header = FALSE, sep = "\t", fill = TRUE,
                         blank.lines.skip = TRUE)

  list(data = d, blank_lines = blank_lines,
       malformed_lines = malformed_lines, n_lines = length(lines))
}


# ---- structured cross-file diagnosis -----------------------------------
# reads: named list (roles) of .dam_read_raw() results.
# Returns list(needs_alignment, report[chr], counts[named int],
#              subrow_mismatches[data.table], first_misalignment).
.dam_diagnose_list <- function(reads, roles) {
  report <- character(0)
  counts <- setNames(vapply(reads, function(r) nrow(r$data), integer(1)), roles)

  first_ts <- vapply(reads, function(r) {
    d <- r$data; if (!nrow(d)) return(NA_character_)
    paste(d$V2[1], d$V3[1])
  }, character(1))
  last_ts <- vapply(reads, function(r) {
    d <- r$data; n <- nrow(d); if (!n) return(NA_character_)
    paste(d$V2[n], d$V3[n])
  }, character(1))

  needs <- FALSE

  # blank / malformed lines
  for (role in roles) {
    r <- reads[[role]]
    if (length(r$blank_lines) > 0) {
      needs <- TRUE
      report <- c(report, sprintf(
        "%s: %d blank line(s) at line %s -- a dropped reading. A bare read stops here (loses everything after); this reads past it.",
        role, length(r$blank_lines), paste(r$blank_lines, collapse = ", ")))
    }
    if (length(r$malformed_lines) > 0) {
      needs <- TRUE
      report <- c(report, sprintf("%s: %d line(s) without the expected column count at line %s.",
                                  role, length(r$malformed_lines),
                                  paste(r$malformed_lines, collapse = ", ")))
    }
  }

  if (length(unique(counts)) > 1) {
    needs <- TRUE
    report <- c(report, sprintf("Row counts differ: %s.",
                                paste(sprintf("%s=%d", roles, counts), collapse = ", ")))
  }
  if (length(unique(first_ts)) > 1) {
    needs <- TRUE
    report <- c(report, sprintf("Start times differ (%s) -- monitors began logging at different times.",
                                paste(unique(first_ts), collapse = " vs ")))
  }
  if (length(unique(last_ts)) > 1) {
    needs <- TRUE
    report <- c(report, sprintf("End times differ (%s) -- one monitor's tail is shorter.",
                                paste(unique(last_ts), collapse = " vs ")))
  }

  subrow_mismatches <- .dam_subrow_mismatches(reads, roles)
  if (nrow(subrow_mismatches) > 0) {
    needs <- TRUE
    for (i in seq_len(nrow(subrow_mismatches))) {
      report <- c(report, sprintf("Sub-row mismatch at %s %s: a monitor dropped a reading.",
                                  subrow_mismatches$V2[i], subrow_mismatches$V3[i]))
    }
  }

  misalign <- .dam_first_misalignment(reads, roles)
  if (!is.null(misalign)) {
    needs <- TRUE
    report <- c(report, sprintf("Row-by-row alignment first breaks at data row %d (%s).",
                                misalign$row, misalign$detail))
  }

  list(needs_alignment = needs, report = report, counts = counts,
       subrow_mismatches = subrow_mismatches, first_misalignment = misalign)
}

# per-minute sub-row count mismatch across files present in >= 2 monitors
.dam_subrow_mismatches <- function(reads, roles) {
  empty <- data.table::data.table(V2 = character(), V3 = character())
  ids <- make.unique(roles, sep = "_")
  counts <- data.table::rbindlist(Map(function(r, id) {
    d <- r$data
    if (nrow(d) == 0) return(NULL)
    cc <- d[, .(n = .N), by = .(V2, V3)]
    cc[, file := id]
    cc
  }, reads, ids))
  if (is.null(counts) || nrow(counts) == 0) return(empty)
  wide <- data.table::dcast(counts, V2 + V3 ~ file, value.var = "n", fill = 0L)
  file_cols <- setdiff(names(wide), c("V2", "V3"))
  vals <- as.matrix(wide[, file_cols, with = FALSE])
  present <- rowSums(vals > 0)
  unequal <- apply(vals, 1, function(x) length(unique(x)) > 1)
  wide[present >= 2 & unequal]
}

# first physical row where the (date,time) sequence diverges across files
.dam_first_misalignment <- function(reads, roles) {
  if (length(reads) < 2) return(NULL)
  seqs <- lapply(reads, function(r) {
    d <- r$data; if (!nrow(d)) return(character(0))
    paste(d$V2, d$V3)
  })
  n <- min(lengths(seqs))
  if (n == 0) return(NULL)
  mat <- vapply(seqs, function(s) s[seq_len(n)], character(n))
  if (is.null(dim(mat))) mat <- matrix(mat, nrow = n)
  bad <- which(apply(mat, 1, function(x) length(unique(x)) > 1))
  if (length(bad) == 0) return(NULL)
  row <- bad[1]
  detail <- paste(sprintf("%s=[%s]", roles, mat[row, ]), collapse = "  ")
  list(row = row, detail = detail)
}


# ---- alignment primitives (vendored) -----------------------------------

# Parse a "3 Jun 26" / "16:36:00" pair to POSIXct for start comparison.
.dam_parse_ts <- function(date_str, time_str) {
  as.POSIXct(paste(date_str, time_str), format = "%d %b %y %H:%M:%S", tz = "UTC")
}

# Trim one data.table to begin at the first row whose (V2,V3) == start_key.
.dam_filter_start <- function(d, start_key) {
  hit <- which(d$V2 == start_key[1] & d$V3 == start_key[2])
  if (length(hit) == 0) {
    stop(sprintf("Common start %s %s not found in one monitor.", start_key[1], start_key[2]),
         call. = FALSE)
  }
  d[hit[1]:.N]
}

# Trim a named list to the earliest-ending monitor's final timestamp.
.dam_trim_to_common_end <- function(data_list) {
  ref_idx  <- which.min(vapply(data_list, nrow, integer(1)))
  end_date <- data_list[[ref_idx]]$V2[nrow(data_list[[ref_idx]])]
  end_time <- data_list[[ref_idx]]$V3[nrow(data_list[[ref_idx]])]
  lapply(data_list, function(d) {
    end_row <- which(d$V2 == end_date & d$V3 == end_time)
    if (length(end_row) == 0) {
      stop(sprintf("Common end timestamp %s %s not found in one monitor.", end_date, end_time),
           call. = FALSE)
    }
    d[1:max(end_row)]
  })
}

# Nearest same-monitor row bearing V4 == mv, by index distance.
.dam_nearest_template <- function(d, target_idx, mv) {
  cand <- d[V4 == mv]
  if (nrow(cand) == 0) {
    stop(sprintf("No template row with V4=%s to synthesise a missing sub-row.", mv), call. = FALSE)
  }
  cand[which.min(abs(as.numeric(cand$V1) - as.numeric(target_idx)))]
}

# Infer a monitor's within-minute sub-row order (1 = ascending, -1 = descending).
.dam_subrow_order <- function(d) {
  counts <- d[, .N, by = .(V2, V3)]
  multi  <- counts[N > 1]
  if (nrow(multi) == 0) return(1L)
  fm <- d[V2 == multi$V2[1] & V3 == multi$V3[1]]
  if (fm$V4[1] > fm$V4[2]) -1L else 1L
}

# Insert mid-file dropped sub-rows so every monitor realigns from that point.
# Boundary (first/last) minutes are left to the trim/match steps.
.dam_repair_missing_subrows <- function(data_list, activity_cols = paste0("V", 11:42)) {
  all_min <- data.table::rbindlist(
    lapply(data_list, function(d) unique(d[, .(V2, V3, V4)])))
  expected <- all_min[, .(v4 = list(sort(unique(V4)))), by = .(V2, V3)]
  data.table::setkey(expected, V2, V3)

  inserted_total <- 0L
  out <- lapply(seq_along(data_list), function(i) {
    d <- data.table::copy(data_list[[i]])
    present <- d[, .(have = list(sort(unique(V4)))), by = .(V2, V3)]
    n_min <- nrow(present)
    if (n_min < 3) return(d)

    inserts <- list()
    for (k in 2:(n_min - 1)) {
      dt <- present$V2[k]; tm <- present$V3[k]
      exp_v4  <- expected[.(dt, tm), v4][[1]]
      missing <- setdiff(exp_v4, present$have[[k]])
      for (mv in missing) {
        sib    <- d[V2 == dt & V3 == tm][1]
        tmpl   <- .dam_nearest_template(d, sib$V1, mv)
        newrow <- data.table::copy(tmpl)
        newrow$V1 <- sib$V1; newrow$V2 <- dt; newrow$V3 <- tm; newrow$V4 <- mv
        for (cc in intersect(activity_cols, names(newrow))) newrow[[cc]] <- 0L
        inserts[[length(inserts) + 1L]] <- newrow
      }
    }
    if (length(inserts) == 0) return(d)
    inserted_total <<- inserted_total + length(inserts)
    res <- data.table::rbindlist(c(list(d), inserts), use.names = TRUE)
    ord <- .dam_subrow_order(data_list[[i]])
    data.table::setorderv(res, c("V1", "V4"), c(1L, ord))
    res[]
  })
  names(out) <- names(data_list)
  attr(out, "n_inserted") <- inserted_total
  out
}

# Trim the final shared timestamp to the fewest sub-rows present across files.
.dam_match_final <- function(trimmed_list) {
  final_ts <- paste(trimmed_list[[1]]$V2[nrow(trimmed_list[[1]])],
                    trimmed_list[[1]]$V3[nrow(trimmed_list[[1]])])
  n_at_final <- vapply(trimmed_list, function(d) sum(paste(d$V2, d$V3) == final_ts), integer(1))
  min_n <- min(n_at_final)
  lapply(trimmed_list, function(d) {
    final_rows <- which(paste(d$V2, d$V3) == final_ts)
    if (length(final_rows) > min_n) {
      drop_rows <- utils::tail(final_rows, length(final_rows) - min_n)
      d <- d[-drop_rows]
    }
    d
  })
}


# ---- public: plan (diagnose + align in memory, no side effects) --------
dam_plan_alignment <- function(mf, expected_cols = 42L) {
  roles <- names(mf)
  reads <- lapply(mf, .dam_read_raw, expected_cols = expected_cols)
  names(reads) <- roles

  diag   <- .dam_diagnose_list(reads, roles)
  before <- setNames(vapply(reads, function(r) nrow(r$data), integer(1)), roles)

  if (!diag$needs_alignment) {
    return(list(needs_alignment = FALSE,
                report = "All three monitors already cover the same window with matching rows.",
                before = before, after = before, roles = roles,
                aligned = NULL, mf = mf, n_inserted = 0L))
  }

  data_list <- setNames(lapply(reads, function(r) r$data), roles)

  # 1. common start = latest first-timestamp across monitors
  starts <- vapply(data_list, function(d) .dam_parse_ts(d$V2[1], d$V3[1]), numeric(1))
  ref    <- which.max(starts)
  start_key <- c(data_list[[ref]]$V2[1], data_list[[ref]]$V3[1])
  filtered  <- lapply(data_list, .dam_filter_start, start_key = start_key)
  names(filtered) <- roles

  # 2. trim to common end  3. repair dropped sub-rows  4. match final minute
  trimmed  <- .dam_trim_to_common_end(filtered)
  repaired <- .dam_repair_missing_subrows(trimmed)
  matched  <- .dam_match_final(repaired)
  names(matched) <- roles

  after <- setNames(vapply(matched, nrow, integer(1)), roles)
  n_ins <- attr(repaired, "n_inserted") %||% 0L

  action <- c(
    sprintf("Common window start: %s %s.", start_key[1], start_key[2]),
    if (n_ins > 0) sprintf("Repaired %d dropped sub-row(s) in place (zero activity, monitor's own order).", n_ins),
    sprintf("Result: %s -- all monitors now match and cover the full shared window.",
            paste(sprintf("%s=%d", roles, after), collapse = ", "))
  )

  list(needs_alignment = TRUE,
       report = c(diag$report, "", "Planned fix:", action),
       before = before, after = after, roles = roles,
       aligned = matched, mf = mf, n_inserted = n_ins)
}

# NULL-coalescing helper (base R has none).
`%||%` <- function(a, b) if (is.null(a)) b else a


# ---- public: write aligned tables to temp files ------------------------
# Returns a named character vector of file paths in the same role order as
# plan$mf, suitable to drop into read_position_monitors().
dam_write_aligned <- function(plan, out_dir = tempfile("dam_aligned_")) {
  if (!isTRUE(plan$needs_alignment)) return(plan$mf)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  roles <- plan$roles
  out <- setNames(character(length(roles)), roles)
  for (role in roles) {
    base <- basename(as.character(plan$mf[[role]]))
    if (!nzchar(base) || base == "NA") base <- paste0(role, ".txt")
    p <- file.path(out_dir, sub("\\.txt$", "", base) |> paste0("_aligned.txt"))
    data.table::fwrite(plan$aligned[[role]], p, sep = "\t",
                       quote = FALSE, col.names = FALSE)
    out[[role]] <- p
  }
  out
}
