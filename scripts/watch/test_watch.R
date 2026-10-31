# ========================================
## Watcher unit tests: verbs, reconcile, rulings, modes. No network, temp WATCH_HOME.
##   Rscript scripts/watch/test_watch.R
# ========================================
Sys.setenv(CBS_WATCH_HOME = tempfile("watch_test_"), SLACK_BOT_TOKEN = "", CBS_WATCH_APPROVERS = "U1")
for (f in c("lib", "slack", "incidents")) source(file.path("scripts", "watch", paste0(f, ".R")))
watch_init()
n_fail = 0
ok = function(cond, what) {
  if (!isTRUE(cond)) { n_fail <<- n_fail + 1; cat("FAIL:", what, "\n") } else cat("ok:", what, "\n")
}

# ---- verbs ----
ok(identical(parse_verb("approve I-7")$incident, "I-7"), "approve")
ok(identical(parse_verb("<@UBOT> Deny i-12")[c("verb", "incident")], list(verb = "deny", incident = "I-12")), "deny with mention, case")
v = parse_verb("ignore I-3 until 23:30")
ok(v$verb == "ignore" && v$until == "23:30", "ignore until")
v = parse_verb("ignore key:target_error:data_TX_*")
ok(isTRUE(v$glob) && v$key == "target_error:data_TX_*", "ignore glob keeps case")
ok(parse_verb("!mode llm until 22:00")$mode == "llm", "mode until")
ok(parse_verb("!stop")$verb == "stop", "stop")
ok(is.null(parse_verb("approve everything please")), "free text is not a verb")
ok(is.null(parse_verb("I approve I-7")), "verb must lead")

# ---- reconcile ----
t0 = as.POSIXct("2026-11-03 20:00", tz = WATCH_TZ)
db = list(next_id = 1L, incidents = list())
f1 = finding("target_error", "data_FL_pasco", "404", "P2", "FL")
f2 = finding("target_error", "data_TX_harris", "timeout", "P2", "TX", transient = TRUE)
db = reconcile(db, list(f1, f2), rulings = list(), now = t0, checks_run = "target_error")
ok(length(db$incidents) == 1 && db$changed == "I-1", "new finding opens; transient waits")
db = reconcile(db, list(f1, f2), rulings = list(), now = t0, checks_run = "target_error")
ok(length(db$incidents) == 1 && db$incidents$`I-1`$cycles == 2, "repeat updates")
db = reconcile(db, list(modifyList(f1, list(severity = "P1")), f2), rulings = list(), now = t0, checks_run = "target_error")
ok(length(db$incidents) == 2 && "I-1" %in% db$changed && db$incidents$`I-1`$severity == "P1", "transient opens on 3rd; escalation flagged")
db = reconcile(db, list(f2), rulings = list(), now = t0, checks_run = "other_check")
ok(db$incidents$`I-1`$status == "open", "a check that didn't run resolves nothing")
db = reconcile(db, list(f2), rulings = list(), now = t0, checks_run = "target_error")
db = reconcile(db, list(f2), rulings = list(), now = t0, checks_run = "target_error")
ok(db$incidents$`I-1`$status == "resolved" && "I-1" %in% db$changed, "resolves after 2 clean cycles")
db = reconcile(db, list(f1), rulings = list(), now = t0, checks_run = "target_error")
ok(length(db$incidents) == 3, "a resolved key that comes back opens a new incident")

# ---- rulings (written relative to the real clock) ----
t0 = now_et()
r = add_ruling("target_error:data_FL_pasco", "ignore", "U1", "ignore I-3", incident = "I-3", until = "23:30")
ok(!is.null(ruling_for("target_error:data_FL_pasco", live_rulings(now = t0))), "ruling matches exact key")
ok(is.null(ruling_for("target_error:data_FL_pasc", live_rulings(now = t0))), "no partial matches")
invisible(add_ruling("target_error:data_TX_*", "ignore", "U1", "ignore key:...", glob = TRUE))
ok(!is.null(ruling_for("target_error:data_TX_dallas", live_rulings(now = t0))), "typed glob matches")
db2 = reconcile(list(next_id = 1L, incidents = list()), list(finding("target_error", "data_TX_dallas", "x")), rulings = live_rulings(now = t0), now = t0, checks_run = "target_error")
ok(db2$incidents$`I-1`$status == "ignored" && !length(db2$changed), "ruled key opens quiet")
late = parse_time(r$until) + 60
ok(is.null(ruling_for("target_error:data_FL_pasco", live_rulings(now = late))), "ruling expires")

# ---- modes ----
ok(effective_mode() == "deterministic", "default mode")
set_mode("llm")
ok(effective_mode() == "llm", "llm")
set_mode("llm", format(now_et() + 3600, "%H:%M"))
ok(effective_mode() == "llm", "llm until later")
ok(effective_mode(now = now_et() + 7200) == "deterministic", "deterministic after the until")

# ---- Slack dry-run ----
ok(SLACK_DRY && !is.null(slack_post("hello")), "dry-run post returns a ts")
ok(file.exists(state_path("slack_outbox.log")), "dry-run writes the outbox")

cat(if (n_fail) sprintf("\n%d FAILED\n", n_fail) else "\nall passed\n")
quit(status = as.integer(n_fail > 0))
