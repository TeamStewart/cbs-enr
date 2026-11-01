## Test or live data on the published dashboard, from dashboard_mode.txt (see the comments there).
## Base R only: .github/workflows/pages.yml runs it with a bare R install.
##
##   Rscript --vanilla scripts/util/dashboard_mode.R source <unix time of the live data, or NA>
##     prints test, live or empty (empty = render the "awaiting results" page)
##   Rscript scripts/util/dashboard_mode.R cutover-now
##     sets cutover to the current Eastern time (switch to live early); then commit and push

DASHBOARD_MODE_FILE = "dashboard_mode.txt"
DASHBOARD_MODE_TZ = "America/New_York"

read_dashboard_mode <- function(path = DASHBOARD_MODE_FILE) {
  x = trimws(sub("#.*", "", readLines(path, warn = FALSE)))
  x = x[nzchar(x)]
  kv = setNames(trimws(sub("^[^:]*:", "", x)), trimws(sub(":.*", "", x)))
  mode = kv[["mode"]]
  if (!mode %in% c("auto", "test", "live")) stop(path, ": mode must be auto, test or live, not ", mode)
  cutover = as.POSIXct(kv[["cutover"]], tz = DASHBOARD_MODE_TZ, format = "%Y-%m-%d %H:%M")
  if (is.na(cutover)) stop(path, ": cutover must look like 2026-11-03 18:00, not ", kv[["cutover"]])
  list(mode = mode, cutover = cutover)
}

# "test", "live" or "empty", for live data written at live_time (NA: there is none)
dashboard_source <- function(live_time = NA, now = Sys.time(), cfg = read_dashboard_mode()) {
  live_time = as.numeric(as.POSIXct(live_time, tz = "UTC"))
  now = as.numeric(now)
  cutover = as.numeric(cfg$cutover)
  has_live = !is.na(live_time)
  switch(cfg$mode,
    test = "test",
    live = if (has_live) "live" else "empty",
    auto = if (now < cutover) "test" else if (has_live && live_time >= cutover) "live" else "empty"
  )
}

# Whether data written now would be published: automation.R uploads mit.html and starts
# Pages builds only then
dashboard_is_live <- function(now = Sys.time(), cfg = read_dashboard_mode()) {
  dashboard_source(live_time = now, now = now, cfg = cfg) == "live"
}

if (sys.nframe() == 0) {
  args = commandArgs(trailingOnly = TRUE)
  if (identical(args[1], "source")) {
    t = suppressWarnings(as.numeric(args[2]))
    cat(dashboard_source(if (is.na(t)) NA else as.POSIXct(t, origin = "1970-01-01", tz = "UTC")), "\n", sep = "")
  } else if (identical(args[1], "cutover-now")) {
    now = format(Sys.time(), "%Y-%m-%d %H:%M", tz = DASHBOARD_MODE_TZ)
    x = readLines(DASHBOARD_MODE_FILE, warn = FALSE)
    x = sub("^cutover:.*", paste("cutover:", now), x)
    writeLines(x, DASHBOARD_MODE_FILE)
    message("cutover: ", now, " (Eastern). Commit and push ", DASHBOARD_MODE_FILE, " to main to publish.")
  } else {
    stop("usage: dashboard_mode.R source <unix time|NA> | cutover-now")
  }
}
