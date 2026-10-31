# ========================================
## Watcher: incidents (state/incidents.json), rulings (state/decisions.json) and the Slack
## verb parser. Only tick.R calls these, one tick at a time, so the files need no locking.
# ========================================

load_incidents = function() read_json_file(state_path("incidents.json"), list(next_id = 1L, incidents = list()))
save_incidents = function(db) write_json_file(db, state_path("incidents.json"))

load_rulings = function() read_json_file(state_path("decisions.json"), list())
save_rulings = function(r) write_json_file(r, state_path("decisions.json"))

# rulings still in force
live_rulings = function(rulings = load_rulings(), now = now_et()) {
  Filter(function(r) { u = parse_time(r$until); is.na(u) || u > now }, rulings)
}

# exact key, or a glob only when a human typed `key:<glob>`
ruling_for = function(key, rulings) {
  for (r in rulings) {
    if (identical(r$key, key)) return(r)
    if (isTRUE(r$glob) && grepl(utils::glob2rx(r$key), key)) return(r)
  }
  NULL
}

hist_add = function(inc, ...) {
  inc$history = c(inc$history, paste(format(now_et(), "%H:%M"), ...))
  inc
}

# Fold one cycle's findings into the incident db. A finding with an open incident updates it,
# a new key opens one (unless a ruling covers it), and an incident missing from 2 cycles in a
# row resolves. Transient findings open only after 3 cycles in a row. Returns the db with
# `changed` = ids that are new, escalated or newly resolved (for posting and L2).
reconcile = function(db, findings, rulings = live_rulings(), now = now_et(), checks_run = NULL) {
  open_status = c("open", "awaiting", "approved", "ignored", "denied")
  by_key = list()
  for (id in names(db$incidents)) {
    inc = db$incidents[[id]]
    if (inc$status %in% open_status) by_key[[inc$key]] = id
  }
  # transient counters live with the db so they survive between cycles
  pending = db$pending %||% list()
  changed = character()
  seen = character()

  for (f in findings) {
    seen = c(seen, f$key)
    id = by_key[[f$key]]
    if (is.null(id)) {
      if (isTRUE(f$transient)) {
        pending[[f$key]] = (pending[[f$key]] %||% 0) + 1
        if (pending[[f$key]] < 3) next
      }
      r = ruling_for(f$key, rulings)
      id = paste0("I-", db$next_id)
      db$next_id = db$next_id + 1L
      inc = list(
        id = id, key = f$key, check = f$check, state = f$state, severity = f$severity,
        status = if (!is.null(r) && r$verb %in% c("ignore", "deny")) "ignored" else "open",
        message = f$message, evidence = f$evidence, value = f$value,
        first_seen = fmt_time(now), last_seen = fmt_time(now), cycles = 1L, clean_cycles = 0L,
        slack_ts = NULL, posted_severity = NULL, proposal = NULL, l2_runs = 0L, history = list()
      )
      inc = hist_add(inc, "opened:", f$message, if (!is.null(r)) paste0("(covered by ruling: ", r$text, ")"))
      db$incidents[[id]] = inc
      if (inc$status == "open") changed = c(changed, id)
      by_key[[f$key]] = id
    } else {
      inc = db$incidents[[id]]
      sev = max_sev(inc$severity, f$severity)
      if (!identical(sev, inc$severity)) {
        inc = hist_add(inc, "escalated", inc$severity, "->", sev)
        if (inc$status != "ignored") changed = c(changed, id)
      }
      inc$severity = sev
      inc$message = f$message
      inc$evidence = f$evidence
      inc$value = f$value
      inc$last_seen = fmt_time(now)
      inc$cycles = inc$cycles + 1L
      inc$clean_cycles = 0L
      db$incidents[[id]] = inc
    }
  }
  pending = pending[intersect(names(pending), seen)]
  db$pending = if (length(pending)) pending else NULL

  # resolve what this cycle's checks no longer find; a check that didn't run resolves nothing
  for (key in setdiff(names(by_key), seen)) {
    id = by_key[[key]]
    inc = db$incidents[[id]]
    if (!is.null(checks_run) && !inc$check %in% checks_run) next
    inc$clean_cycles = inc$clean_cycles + 1L
    if (inc$clean_cycles >= 2 && inc$status != "approved") {
      inc$status = "resolved"
      inc = hist_add(inc, "resolved (not seen for 2 cycles)")
      changed = c(changed, id)
    }
    db$incidents[[id]] = inc
  }
  db$changed = unique(changed)
  db
}

# a ruling whose `until` passed lets its incidents speak again
expire_ignores = function(db, rulings = live_rulings()) {
  for (id in names(db$incidents)) {
    inc = db$incidents[[id]]
    if (inc$status == "ignored" && is.null(ruling_for(inc$key, rulings))) {
      inc$status = "open"
      db$incidents[[id]] = hist_add(inc, "ruling expired; watching again")
    }
  }
  db
}

open_incidents = function(db, status = c("open", "awaiting", "approved")) {
  Filter(function(i) i$status %in% status, db$incidents)
}

# ---- verbs ----
# One verb per message. Returns NULL for anything that isn't a verb (free text = context).
#   approve I-7 | deny I-7 | ignore I-7 [until HH:MM] | ignore key:<glob> [until HH:MM]
#   !mode llm|deterministic [until HH:MM] | !stop | !start | !status
parse_verb = function(text) {
  t = trimws(gsub("<[^>]+>", "", text)) # drop Slack mentions like <@U123>
  t = tolower(t)
  until = NULL
  m = regmatches(t, regexec("\\s+until\\s+([0-9]{1,2}:[0-9]{2})\\s*$", t))[[1]]
  if (length(m)) {
    until = m[2]
    t = trimws(sub("\\s+until\\s+[0-9]{1,2}:[0-9]{2}\\s*$", "", t))
  }
  if (grepl("^(approve|deny|ignore)\\s+i-[0-9]+$", t)) {
    p = strsplit(t, "\\s+")[[1]]
    return(list(verb = p[1], incident = toupper(p[2]), until = until))
  }
  if (grepl("^ignore\\s+key:\\S+$", t)) {
    # keys are case-sensitive (target names): take the glob from the original text
    g = regmatches(text, regexec("key:(\\S+)", text))[[1]][2]
    return(list(verb = "ignore", key = g, glob = TRUE, until = until))
  }
  if (grepl("^!mode\\s+(llm|deterministic)$", t)) {
    return(list(verb = "mode", mode = sub("^!mode\\s+", "", t), until = until))
  }
  if (t %in% c("!stop", "!start", "!status")) return(list(verb = sub("^!", "", t)))
  NULL
}

add_ruling = function(key, verb, by, text, incident = NULL, until = NULL, glob = FALSE) {
  r = load_rulings()
  u = if (is.null(until)) end_of_night() else parse_clock(until)
  r[[length(r) + 1]] = list(
    key = key, verb = verb, until = fmt_time(u), by = by, at = fmt_time(), incident = incident,
    text = text, glob = glob
  )
  save_rulings(r)
  r[[length(r)]]
}

# ---- mode ----
# `mode` file: "llm", "deterministic", or "llm until 23:30" (deterministic after that)
effective_mode = function(now = now_et()) {
  p = watch_path("mode")
  if (!file.exists(p)) return("deterministic")
  x = tolower(trimws(readLines(p, warn = FALSE)[1] %||% ""))
  m = regmatches(x, regexec("^(llm|deterministic)(\\s+until\\s+([0-9]{1,2}:[0-9]{2}))?$", x))[[1]]
  if (!length(m)) return("deterministic")
  if (nzchar(m[4])) {
    # the deadline is stored absolute when written by !mode, so "until" means the clock time
    # on the day the file was last written
    t = as.POSIXct(paste(format(file.mtime(p), "%Y-%m-%d"), m[4]), format = "%Y-%m-%d %H:%M", tz = WATCH_TZ)
    if (t <= file.mtime(p)) t = t + 86400
    if (now >= t) return(if (m[2] == "llm") "deterministic" else "llm")
  }
  m[2]
}

set_mode = function(mode, until = NULL) {
  writeLines(if (is.null(until)) mode else paste(mode, "until", until), watch_path("mode"))
}

# ---- Slack text ----
sev_icon = function(s) switch(s %||% "P2", P0 = ":rotating_light:", P1 = ":warning:", ":information_source:")

incident_text = function(inc) {
  paste0(sev_icon(inc$severity), if (identical(inc$severity, "P0")) " <!here>" else "",
         " *", inc$id, "* ", inc$severity, if (!is.na(inc$state %||% NA)) paste0(" ", inc$state) else "",
         ": ", inc$message, "\n_", inc$key, "_")
}

status_text = function(db, cycle_json, cycle, sl, mode) {
  open = open_incidents(db, c("open", "awaiting", "approved"))
  sev = vapply(open, function(i) i$severity, "")
  fin = parse_time_any(cycle_json$finished)
  age = if (is.na(fin)) "?" else round(as.numeric(difftime(now_et(), fin, units = "mins")))
  top = utils::head(Filter(function(i) i$severity %in% c("P0", "P1"), open), 8)
  awaiting = Filter(function(i) i$status == "awaiting", open)
  l2 = sl$last_l2
  paste0(
    "*cbs-enr watcher*", if (stopped()) " :octagonal_sign: *STOP*" else "", " | mode `", mode, "` | updated ",
    format(now_et(), "%H:%M"), "\n",
    "Last cycle ", cycle_json$run_time %||% "?", " (", age, " min ago), enightdata ",
    substr(readLines_safe(watch_path("pipeline-state", "enightdata_sha")), 1, 7), "\n",
    "Open: ", sum(sev == "P0"), " P0, ", sum(sev == "P1"), " P1, ", sum(sev == "P2"), " P2",
    if (length(awaiting)) paste0(" | awaiting approval: ", paste(names(awaiting), collapse = ", ")) else "",
    if (!length(APPROVERS)) "\n:warning: CBS_WATCH_APPROVERS is empty: nobody can approve" else "",
    if (length(top)) paste0("\n", paste("•", vapply(top, function(i) sprintf("%s %s %s", i$id, i$severity, substr(i$message, 1, 90)), ""), collapse = "\n")) else "",
    if (!is.null(l2)) paste0("\nL2 run ", l2$n, " at ", substr(l2$at, 12, 16), if (isTRUE(l2$ok)) ": " else " FAILED: ", substr(l2$summary %||% "", 1, 300)) else ""
  )
}

readLines_safe = function(p) if (file.exists(p)) readLines(p, warn = FALSE)[1] else "?"
