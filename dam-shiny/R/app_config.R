# ============================================================
# R/app_config.R  —  minimal config.yml reader (no {config} dependency)
#
# Parses the flat key: value pairs under a chosen top-level profile in
# config.yml. Kept intentionally tiny so deployment needs no extra package.
# Values fill the "Run settings" form defaults; everything stays overridable
# in the UI. Missing file or key -> the built-in fallback default is used.
# ============================================================

# Built-in fallbacks (used when config.yml is absent or a key is missing).
# The analysis period is detected from the uploaded files, not configured here.
.app_defaults <- list(
  tz                    = "UTC",   # parse file timestamps as naive wall-clock
  lights_on             = "05:00:00",
  lights_off            = "17:00:00",
  channels              = "1:32",
  death_threshold_hours = 12,
  sleep_threshold_min   = 5,
  bout_window_min       = 1,
  use_anchored          = TRUE,
  max_upload_mb         = 250
)

# Parse the flat "key: value" lines under `<profile>:` in config.yml.
# Returns a named list of strings/numbers/logicals.
.read_config_profile <- function(path, profile = "default") {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)

  # locate the profile block (a top-level, non-indented "profile:" line)
  hdr <- grep(sprintf("^%s:\\s*$", profile), lines)
  if (!length(hdr)) return(list())
  start <- hdr[1] + 1L

  out <- list()
  for (i in seq(start, length(lines))) {
    ln <- lines[i]
    if (grepl("^\\S", ln)) break            # next top-level block -> stop
    if (grepl("^\\s*#", ln) || !nzchar(trimws(ln))) next
    m <- regmatches(ln, regexec("^\\s+([A-Za-z0-9_]+):\\s*(.*)$", ln))[[1]]
    if (length(m) != 3) next
    key <- m[2]; val <- trimws(m[3])
    val <- sub("\\s+#.*$", "", val)         # strip trailing comment
    val <- gsub('^"|"$', "", val)           # strip surrounding quotes
    out[[key]] <- .coerce_config_value(val)
  }
  out
}

.coerce_config_value <- function(val) {
  if (val %in% c("true", "TRUE", "yes"))  return(TRUE)
  if (val %in% c("false", "FALSE", "no")) return(FALSE)
  if (grepl("^-?[0-9]+(\\.[0-9]+)?$", val)) return(as.numeric(val))
  val
}

# Public: merged defaults (config.yml over built-ins).
app_config <- function(path = "config.yml",
                       profile = Sys.getenv("DAM_CONFIG_PROFILE", "default")) {
  cfg <- .read_config_profile(path, profile)
  if (!identical(profile, "default")) {
    cfg <- modifyList(.read_config_profile(path, "default"), cfg)
  }
  modifyList(.app_defaults, cfg)
}
