# ========================================
## Watcher: shared helpers (paths, JSON state, time, notes, findings).
## Design: docs/watcher_design.md. Sourced by tick.R, checks.R, apply.R, l2_context.R.
# ========================================

suppressPackageStartupMessages({
  library(jsonlite)
})

WATCH_REPO = normalizePath(Sys.getenv("CBS_WATCH_REPO", "."), mustWork = FALSE)
WATCH_HOME = path.expand(Sys.getenv("CBS_WATCH_HOME", "~/.cache/cbs-enr-watch"))
WATCH_STATE = file.path(WATCH_HOME, "state")
WATCH_TZ = "America/New_York"

watch_path = function(...) file.path(WATCH_HOME, ...)
state_path = function(...) file.path(WATCH_STATE, ...)

watch_init = function() {
  for (d in c(WATCH_STATE, state_path("cycles"), state_path("l2"), watch_path("notes"))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

now_et = function() as.POSIXct(Sys.time(), tz = WATCH_TZ)
fmt_time = function(t = now_et()) format(t, "%Y-%m-%dT%H:%M:%S%z", tz = WATCH_TZ)
parse_time = function(x) {
  if (is.null(x) || !length(x) || is.na(x) || !nzchar(x)) return(as.POSIXct(NA))
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S%z", tz = WATCH_TZ)
}
# run_time as automation.R writes it: 2026_11_03_20_05_00 (local time on the scraper laptop, ET)
parse_run_time = function(x) as.POSIXct(x, format = "%Y_%m_%d_%H_%M_%S", tz = WATCH_TZ)

# "23:30" -> the next 23:30 ET (today if still ahead, else tomorrow)
parse_clock = function(hhmm, from = now_et()) {
  if (!grepl("^[0-9]{1,2}:[0-9]{2}$", hhmm)) return(as.POSIXct(NA))
  t = as.POSIXct(paste(format(from, "%Y-%m-%d", tz = WATCH_TZ), hhmm), format = "%Y-%m-%d %H:%M", tz = WATCH_TZ)
  if (t <= from) t = t + 86400
  t
}

# default end of a ruling: 06:00 ET the next morning
end_of_night = function(from = now_et()) parse_clock("06:00", from)

read_json_file = function(path, default = NULL) {
  if (!file.exists(path) || file.size(path) == 0) return(default)
  tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) default)
}

# atomic: write to a temp file next to it, then rename
write_json_file = function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp = paste0(path, ".tmp")
  write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA)
  file.rename(tmp, path)
  invisible(path)
}

# one line per decision, for humans and for L2's context
note = function(...) {
  line = paste0(format(now_et(), "%H:%M:%S"), " ", paste0(...))
  cat(line, "\n", sep = "", file = watch_path("notes", paste0(format(now_et(), "%Y-%m-%d"), ".md")), append = TRUE)
  message(line)
  invisible(line)
}

stopped = function() file.exists(watch_path("STOP"))

# ---- findings ----
# One finding per problem per cycle; `key` is stable across cycles (what rulings match on).
finding = function(check, key, message, severity = "P2", state = NA_character_, value = NULL,
                   evidence = NULL, transient = FALSE) {
  list(
    check = check, key = paste0(check, ":", key), state = state, severity = severity,
    message = message, value = value, evidence = evidence, transient = transient
  )
}

sev_rank = function(s) match(s, c("P0", "P1", "P2"))
max_sev = function(a, b) if (is.na(sev_rank(a)) || (!is.na(sev_rank(b)) && sev_rank(b) < sev_rank(a))) b else a

# ---- poll close ----
poll_close = local({
  cache = NULL
  function() {
    if (is.null(cache)) {
      p = file.path(WATCH_REPO, "scripts", "watch", "poll_close.csv")
      d = utils::read.csv(p, comment.char = "#", stringsAsFactors = FALSE)
      cache <<- setNames(d$close_et, d$state)
    }
    cache
  }
})

# the state's last polls have closed (election day = ELECTION_DATE). Unknown state: TRUE.
polls_closed = function(state, now = now_et(), election_date = Sys.getenv("CBS_ELECTION_DATE", "2026-11-03")) {
  pc = poll_close()
  vapply(state, function(s) {
    hhmm = pc[s]
    if (is.na(hhmm)) return(TRUE)
    close = as.POSIXct(paste(election_date, hhmm), format = "%Y-%m-%d %H:%M", tz = WATCH_TZ)
    # times past midnight (AK, HI) are written as 24:00/25:00
    if (is.na(close)) {
      h = as.integer(sub(":.*", "", hhmm)); m = as.integer(sub(".*:", "", hhmm))
      close = as.POSIXct(election_date, tz = WATCH_TZ) + h * 3600 + m * 60
    }
    now >= close
  }, logical(1))
}

`%||%` = function(a, b) if (is.null(a) || !length(a) || (length(a) == 1 && is.na(a))) b else a

# cycle.json's `finished` ("2026-11-03 21:05:12 EST") or an ISO time
parse_time_any = function(x) {
  if (is.null(x) || !length(x) || is.na(x)) return(as.POSIXct(NA))
  t = as.POSIXct(sub(" [A-Z]{3,4}$", "", x), format = "%Y-%m-%d %H:%M:%S", tz = WATCH_TZ)
  if (is.na(t)) t = parse_time(x)
  t
}
