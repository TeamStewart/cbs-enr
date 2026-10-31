# Watcher build: task list

Build order for `docs/watcher_plan.md`. Interfaces are in `docs/watcher_design.md`.
Tick a box when the work is done **and** tested; the note says how it was tested.
A fresh session should start by reading this file, then the design.

## A. Foundations
- [x] A1 `scripts/watch/lib.R`: paths, env, atomic JSON IO, notes, time helpers, poll close (smoke-tested (poll close, clocks, atomic JSON, notes))
- [x] A2 `scripts/watch/poll_close.csv`: last poll close (ET) per state (approximate times, please check them before the night)
- [x] A3 `scripts/watch/slack.R`: post, reply, update, replies, history, auth.test; dry-run outbox (dry-run tested in test_watch.R; live calls untested until there's a token)
- [x] A4 `.gitignore` and the `watch/` layout (state outside the repo, in `$WATCH_HOME`) (state lives in $WATCH_HOME (~/.cache/cbs-enr-watch), outside the repo, so nothing to gitignore)

## B. L1 checks (`scripts/watch/checks.R`)
- [x] B1 sync: fetch `pipeline-state` and `dashboard-live` into `$WATCH_HOME` (sync.R; run against the real branches)
- [x] B2 cycle steps (`cycle.json`): step FALSE, disk (forced trip paths with a fake cycle.json)
- [x] B3 target errors and warnings from the pushed meta and progress; consecutive counts; transient rules; poll-close gating (real meta: 49 errored, 24 findings after removing knock-ons)
- [x] B4 enightdata sha change with an error jump (P0) (injected sha change: P0)
- [x] B5 frozen timestamps (hash of `timestamp_*` in meta unchanged after poll close while others move) (forced with altered prev metrics)
- [x] B6 scraper output from the clean `*_latest.parquet`: rows to 0, votes decrease, candidate set change, 0-byte files (183 files; reads only files whose mtime changed)
- [x] B7 methods and crosswalk match via `returns_dropped()`, with buckets excluded, against baselines; cells cached per state (11-12s per cycle; baseline caveat below)
- [x] B8 models: `status_latest.csv`, plus races that vanished vs `races_from_priors()` (real status_latest.csv: 80 rows, P2 before poll close)
- [x] B9 CBS feed snapshot age (cbs_api/live/ doesn't exist yet; P1 after 19:00 on election day)
- [x] B10 dashboard: rds non-NULL with every contest present, Pages run status, CloudFront `mit.html` Last-Modified (rds + gh work; CloudFront returns 401, needs CBS_WATCH_DASH_AUTH (below))
- [x] B11 redistricting `live.log` age (works)
- [x] B12 findings to incidents: reconcile, rulings, resolve after 2 clean cycles (incidents.R reconcile(); test_watch.R)

## C. Supervisor (`scripts/watch/tick.R`)
- [x] C1 tick order, lock, health ping (dry-run ticks against the real pipeline-state)
- [x] C2 watchdog: pipeline stale (P0), hourly auth (gh, Slack, aws optional) (P0 fired on the real 36-min-old cycle)
- [x] C3 Slack in: verb parser, approver allowlist, rulings, `mode`, `STOP` (parse_verb() and dispatch in tick.R; parser covered by test_watch.R)
- [x] C4 Slack out: incident threads, digest, pinned status message, P0 `@here`, rate limit (dry-run: P0 root post with <!here>, a 128-line P2 digest, status pinned once and then updated)
- [x] C5 modes: `llm`, `deterministic`, `llm until HH:MM`; when L2 runs (effective_mode()/set_mode(); test_watch.R)

## D. L2
- [x] D1 `l2_prompt.md`, `l2_schema.json`, context builder (l2_prompt.md, l2_schema.json, l2_context() in l2.R)
- [x] D2 `l2.sh`: `claude -p` flags, timeout, no API key, output capture (flags tested with a Haiku call: structured_output comes back, timeout via perl alarm)
- [x] D3 `claude_settings.json` allowlist, plus a PreToolUse hook that blocks while STOP exists (tested: dontAsk denied `touch`; the STOP hook blocked Read)
- [x] D4 `probe.R`: run one scraper against a temp Dropbox (probe.R sets paths in memory only (set_all() would save them to config))
- [x] D5 ingest: post diagnoses and proposals, set incidents to awaiting (fake and real L2 results ingested; proposal, then awaiting, then approve, then the command pushed (local bare repo))

## E. Apply and the command channel
- [x] E1 `apply.R`: patch (rebase, check, push to main), invalidate (commands branch), human (queue_invalidate()/push_commands() tested against a local bare repo; patch path untested)
- [x] E2 `automation.R`: run `pipeline-commands` invalidations, `commands_done`, disk, health ping (run_watch_commands() tested end to end with a throwaway targets project (runs once, records the id))

## F. Ops and tests
- [x] F1 `tick.sh`, launchd plist, `install.sh` (written; launchd not loaded on this laptop)
- [x] F2 unit tests: verb parsing, reconcile, mode parsing, ruling expiry (scripts/watch/test_watch.R: 24 checks pass)
- [x] F3 dry-run on real pipeline-state data (no Slack token) (several dry-run ticks; 2 real L2 runs (Sonnet, $0.13 and $0.19 notional); the second found the TX Ellis bug)
- [x] F4 README for the watcher laptop: setup, Slack app, env file, start and stop (scripts/watch/README.md)
- [x] F5 rehearsal checklist for 11/2 (fault injection), for Mason (in scripts/watch/README.md)

## Added along the way
- [x] `inspect.R`, L2's read-only data tool (parquet, target errors, dropped geos). The first real L2 run had nothing it could use to read parquet.
- [x] `CBS_WATCH_NO_L2=1` (ticks never start L2) and `state/dry_inbox.json` (fake Slack messages in dry-run)
- [x] Ingest drops invalidate targets that apply.R can't run, so nobody approves a proposal that will fail
- [x] `apply_patch` tested against a local bare enightdata repo: a bad branch name is rejected; one commit is pushed to main

## Known gaps (v1.1 / v2)
- B10: CloudFront `mit.html` returns 401 (Basic auth). Set `CBS_WATCH_DASH_AUTH=user:pass`, or switch to S3 HeadObject. Without it the check posts a P2 transient each cycle.
- B7 baseline: `precinct_geo_match_rates.csv` measures our ids against votehub polygons, not the feed, and covers only AK, FL, NC and TX. Better: a baseline from the first post-close cycle.
- B4 compares only with the previous cycle, and its P0 resolves after 2 cycles.
- Warnings aren't grouped (about 10 copies of the same `model_fits_AK_*` warning).
- Not tested live: Slack itself (no token yet), launchd on the watcher laptop, a real patch pushed to GitHub.

## Needs Mason
- Slack workspace, bot token, channel ID, approver user IDs
- healthchecks.io URLs (watcher and scraper)
- The watcher laptop: clone, renv restore, Dropbox, `gh auth`, `claude` login
