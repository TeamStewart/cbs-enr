# ========================================
## Watcher supervisor: one tick (tick.sh runs it every 60 s under a lock). In order: sync,
## L1 checks on a new cycle, watchdog, Slack replies, L2 results, L2 start, Slack posts.
## Deterministic: the LLM only runs inside L2 (l2.R). Design: docs/watcher_design.md
# ========================================

suppressPackageStartupMessages({
  library(jsonlite)
})
setwd(Sys.getenv("CBS_WATCH_REPO", "."))
source("scripts/util/globals.R")
for (f in c("lib", "slack", "incidents", "l2", "apply", "sync", "checks")) source(file.path("scripts", "watch", paste0(f, ".R")))
watch_init()

STALE_MIN = 10 # no new cycle for this long: P0
CHECKIN_MIN = 30 # deterministic mode: L2 looks over everything this often
AUTH_EVERY_MIN = 60
DIGEST_EVERY_MIN = 2

sl = read_json_file(state_path("slack.json"), list())
sl$last_read = sl$last_read %||% sprintf("%.6f", as.numeric(Sys.time()))
db = expire_ignores(load_incidents())
changed = character()
fold = function(findings, checks_run) {
  db <<- reconcile(db, findings, checks_run = checks_run)
  changed <<- unique(c(changed, db$changed))
  db$changed <<- NULL
}

# ---- 1. sync ----
s = tryCatch(sync_pipeline_state(), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
cycle_json = read_json_file(watch_path("pipeline-state", "cycle.json"), list())

# ---- 2. L1 checks, once per new cycle ----
new_cycle = isTRUE(s$ok) && !is.null(cycle_json$run_time) && !identical(cycle_json$run_time, sl$last_checked)
cycle = read_json_file(state_path("cycles", paste0(sl$last_checked %||% "none", ".json")), list())
if (new_cycle) {
  t0 = Sys.time()
  res = tryCatch(run_checks(), error = function(e) {
    list(findings = list(finding("check_error", "run_checks", paste("run_checks failed:", conditionMessage(e)), "P1")),
         metrics = read_json_file(state_path("metrics_prev.json")), checks_run = "check_error")
  })
  checks_run = res$checks_run %||% unique(vapply(res$findings, `[[`, "", "check"))
  cycle = list(cycle = cycle_json, findings = res$findings, metrics = res$metrics, checks_run = checks_run,
               seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  write_json_file(cycle, state_path("cycles", paste0(cycle_json$run_time, ".json")))
  if (!is.null(res$metrics)) write_json_file(res$metrics, state_path("metrics_prev.json"))
  fold(res$findings, c(checks_run, "check_error"))
  sl$last_checked = cycle_json$run_time
  note("cycle ", cycle_json$run_time, ": ", length(res$findings), " findings, ", length(changed), " new or changed incidents (", cycle$seconds, "s)")
  # approved commands the scraper laptop confirms done no longer need pushing
  if (length(cycle_json$commands_done)) commands_done(unlist(cycle_json$commands_done))
}

# ---- 3. watchdog ----
wd = list()
fin = parse_time_any(cycle_json$finished)
age = if (is.na(fin)) NA else as.numeric(difftime(now_et(), fin, units = "mins"))
if (is.na(age) || age > STALE_MIN) {
  wd[[length(wd) + 1]] = finding("watchdog", "pipeline_stale", sprintf(
    "No new pipeline cycle for %s min (last: %s). automation.R may be down, or the scraper laptop asleep or offline.",
    if (is.na(age)) "?" else round(age), cycle_json$run_time %||% "never"), "P0")
}
if (!isTRUE(s$ok)) {
  sl$sync_fail_since = sl$sync_fail_since %||% fmt_time()
  if (difftime(now_et(), parse_time(sl$sync_fail_since), units = "mins") > STALE_MIN) {
    wd[[length(wd) + 1]] = finding("watchdog", "sync_failed", paste("Can't fetch pipeline-state from GitHub:", s$error %||% "?"), "P1")
  }
} else sl$sync_fail_since = NULL
fold(wd, "watchdog")

if (is.null(sl$last_auth) || difftime(now_et(), parse_time(sl$last_auth), units = "mins") >= AUTH_EVERY_MIN) {
  au = list()
  gh = suppressWarnings(system2("gh", c("auth", "status"), stdout = FALSE, stderr = FALSE))
  if (!identical(gh, 0L)) au[[length(au) + 1]] = finding("auth", "gh", "gh is not logged in on the watcher laptop (gh auth login).", "P1")
  if (!slack_auth_ok()) au[[length(au) + 1]] = finding("auth", "slack", "Slack token rejected (auth.test).", "P1")
  fold(au, "auth")
  sl$last_auth = fmt_time()
}

# ---- 4. Slack in: verbs from approvers, other approver text as context for L2 ----
human = list()
reply = function(inc_or_ts, text) {
  ts = if (is.list(inc_or_ts)) inc_or_ts$slack_ts else inc_or_ts
  slack_post(text, thread_ts = ts)
}
threads = Filter(function(i) !is.null(i$slack_ts) && i$status %in% c("open", "awaiting", "approved", "ignored"), db$incidents)
inbox = lapply(slack_read(sl$last_read), function(m) c(m, incident = list(NULL)))
for (i in utils::tail(threads, 25)) {
  inbox = c(inbox, lapply(slack_read(sl$last_read, i$slack_ts), function(m) c(m, incident = i$id)))
}
inbox = inbox[order(vapply(inbox, function(m) as.numeric(m$ts), 0))]
for (m in inbox) {
  if (as.numeric(m$ts) > as.numeric(sl$last_read)) sl$last_read = m$ts
  where = m$thread_ts %||% m$ts
  if (!m$user %in% APPROVERS) {
    note("ignored a message from ", m$user, " (not an approver)")
    next
  }
  v = parse_verb(m$text %||% "")
  if (is.null(v)) {
    if (!is.null(m$incident)) human[[length(human) + 1]] = list(user = m$user, incident = m$incident, text = m$text)
    next
  }
  if (!is.null(v$incident) && is.null(db$incidents[[v$incident]])) {
    slack_post(paste("No incident", v$incident), thread_ts = where)
    next
  }
  inc = if (!is.null(v$incident)) db$incidents[[v$incident]] else NULL
  msg = switch(v$verb,
    approve = {
      if (is.null(inc$proposal) || identical(inc$proposal$action, "none")) {
        paste(inc$id, "has no proposal to approve.")
      } else {
        out = apply_proposal(inc, by = m$user)
        inc$status = if (out$ok) "approved" else "awaiting"
        inc = hist_add(inc, "approve by", m$user, "->", out$message)
        paste0(if (out$ok) "Done: " else "Failed: ", out$message)
      }
    },
    deny = {
      add_ruling(inc$key, "deny", m$user, m$text, incident = inc$id)
      inc$status = "denied"
      inc = hist_add(inc, "denied by", m$user)
      paste(inc$id, "denied; I won't propose this again tonight.")
    },
    ignore = {
      if (isTRUE(v$glob)) {
        r = add_ruling(v$key, "ignore", m$user, m$text, until = v$until, glob = TRUE)
        for (id in names(db$incidents)) {
          j = db$incidents[[id]]
          if (j$status %in% c("open", "awaiting") && grepl(utils::glob2rx(v$key), j$key)) {
            db$incidents[[id]] = hist_add(modifyList(j, list(status = "ignored")), "ignored by", m$user)
          }
        }
        paste("Ignoring", v$key, "until", substr(r$until, 12, 16))
      } else {
        r = add_ruling(inc$key, "ignore", m$user, m$text, incident = inc$id, until = v$until)
        inc$status = "ignored"
        inc = hist_add(inc, "ignored by", m$user, "until", substr(r$until, 12, 16))
        paste("Ignoring", inc$key, "until", substr(r$until, 12, 16))
      }
    },
    mode = {
      set_mode(v$mode, v$until)
      paste0("Mode: ", v$mode, if (!is.null(v$until)) paste0(" until ", v$until) else "")
    },
    stop = {
      file.create(watch_path("STOP"))
      "STOP set: no L2 runs, every L2 tool blocked, only P0 posts. `!start` to resume."
    },
    start = {
      unlink(watch_path("STOP"))
      "STOP cleared."
    },
    status = {
      sl$status_ts = NULL
      "Re-posting the status message."
    }
  )
  if (!is.null(inc)) db$incidents[[inc$id]] = inc
  note(m$user, ": ", m$text, " -> ", msg)
  slack_post(msg, thread_ts = where)
}

# ---- 5. L2 results ----
for (run in l2_collect()) {
  sl$last_l2 = list(n = run$n, at = fmt_time(), ok = !is.null(run$result), summary = run$result$summary %||% run$error)
  if (is.null(run$result)) {
    note("L2 run ", run$n, " failed: ", substr(run$error, 1, 200))
    next
  }
  r = run$result
  if (nzchar(r$notes %||% "")) note("L2: ", r$notes)
  for (d in r$incidents) {
    inc = db$incidents[[d$id]]
    if (is.null(inc)) next
    inc$l2 = d
    p = d$proposal
    # only jurisdiction targets can be invalidated (apply.R); the models rerun on their own
    if (length(p$targets)) {
      tg = unlist(p$targets)
      if (any(!grepl(CMD_TARGET_RX, tg))) note("L2 ", inc$id, ": dropped targets apply can't run: ", paste(tg[!grepl(CMD_TARGET_RX, tg)], collapse = ", "))
      p$targets = as.list(tg[grepl(CMD_TARGET_RX, tg)])
      if (identical(p$action, "invalidate") && !length(p$targets)) p$action = "none"
    }
    denied = !is.null(ruling_for(inc$key, Filter(function(x) x$verb == "deny", live_rulings())))
    ask = !isTRUE(d$expected) && !is.null(p) && !identical(p$action, "none") && !denied && inc$status == "open"
    if (ask) {
      inc$proposal = p
      inc$status = "awaiting"
    }
    inc = hist_add(inc, "L2:", substr(d$diagnosis, 1, 200))
    # quiet unless it matters: expected problems and P2s only reach the status message
    if (ask || (!isTRUE(d$expected) && inc$severity %in% c("P0", "P1"))) {
      if (is.null(inc$slack_ts)) inc$slack_ts = slack_post(incident_text(inc))
      txt = paste0("*Diagnosis* (", d$confidence, "): ", d$diagnosis)
      if (ask) txt = paste0(txt, "\n*Proposal* (", p$action, "): ", p$text,
                            if (length(p$targets)) paste0("\nThen rerun: ", paste(unlist(p$targets), collapse = ", ")) else "",
                            "\nReply `approve ", inc$id, "`, `deny ", inc$id, "` or `ignore ", inc$id, "`.")
      reply(inc, txt)
    }
    db$incidents[[inc$id]] = inc
  }
  obs = lapply(r$observations, function(o) finding("l2", o$key, o$message, o$severity, o$state %||% NA))
  fold(obs, "l2")
}

# ---- 6. L2 start ----
mode = effective_mode()
undiagnosed = names(Filter(function(i) i$status == "open" && i$severity %in% c("P0", "P1") && (i$l2_runs %||% 0L) == 0L, db$incidents))
why = NULL
if (length(human)) {
  why = list(reason = "human replied", incidents = unique(vapply(human, `[[`, "", "incident")))
} else if (mode == "llm" && new_cycle) {
  why = list(reason = "new cycle (llm mode)", incidents = unique(c(intersect(changed, names(Filter(function(i) i$status == "open", db$incidents))), undiagnosed)))
} else if (length(undiagnosed)) {
  why = list(reason = "new P0/P1 incidents", incidents = undiagnosed)
} else if (is.null(sl$last_checkin) || difftime(now_et(), parse_time(sl$last_checkin), units = "mins") >= CHECKIN_MIN) {
  why = list(reason = "check-in", incidents = character())
}
# CBS_WATCH_NO_L2=1: never start L2 (tests, or to save usage without STOP's posting limits)
if (!is.null(why) && !stopped() && !l2_running() && Sys.getenv("CBS_WATCH_NO_L2") != "1") {
  if (l2_start(db, cycle, why, human)) {
    sl$last_checkin = fmt_time()
    # counted at the start, so a run that fails doesn't restart for the same incident every tick
    for (id in why$incidents) if (!is.null(db$incidents[[id]])) db$incidents[[id]]$l2_runs = (db$incidents[[id]]$l2_runs %||% 0L) + 1L
  }
}

# ---- 7. Slack out ----
digest = sl$digest %||% list()
for (id in changed) {
  inc = db$incidents[[id]]
  if (inc$status == "resolved") {
    if (!is.null(inc$slack_ts)) reply(inc, paste(inc$id, "resolved: not seen for 2 cycles."))
    next
  }
  if (stopped() && inc$severity != "P0") next
  if (inc$severity %in% c("P0", "P1")) {
    if (is.null(inc$slack_ts)) {
      inc$slack_ts = slack_post(incident_text(inc))
    } else if (!identical(inc$posted_severity, inc$severity)) {
      reply(inc, paste0("Escalated to ", inc$severity, ": ", inc$message))
    }
    inc$posted_severity = inc$severity
    db$incidents[[id]] = inc
  } else {
    digest[[length(digest) + 1]] = sprintf("%s %s %s", inc$id, inc$state %||% "", inc$message)
  }
}
if (length(digest) && (is.null(sl$last_digest) || difftime(now_et(), parse_time(sl$last_digest), units = "mins") >= DIGEST_EVERY_MIN)) {
  shown = utils::head(unlist(digest), 25)
  slack_post(paste0("*New P2s* (", length(digest), "):\n", paste("•", shown, collapse = "\n"),
                    if (length(digest) > 25) sprintf("\n…and %d more", length(digest) - 25) else ""))
  digest = list()
  sl$last_digest = fmt_time()
}
sl$digest = if (length(digest)) digest else NULL

status = status_text(db, cycle_json, cycle, sl, mode)
if (is.null(sl$status_ts)) {
  sl$status_ts = slack_post(status)
  if (!is.null(sl$status_ts)) invisible(slack_pin(sl$status_ts))
} else invisible(slack_update(sl$status_ts, status))

save_incidents(db)
write_json_file(sl, state_path("slack.json"))
