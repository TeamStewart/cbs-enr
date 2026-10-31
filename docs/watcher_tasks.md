# Watcher build: task list

Build order for `docs/watcher_plan.md`. Interfaces are in `docs/watcher_design.md`.
Tick a box when the work is done **and** tested; the note says how it was tested.
A fresh session should start by reading this file, then the design.

## A. Foundations
- [x] A1 `scripts/watch/lib.R`: paths, env, atomic JSON IO, notes, time helpers, poll close (smoke-tested (poll close, clocks, atomic JSON, notes))
- [x] A2 `scripts/watch/poll_close.csv`: last poll close (ET) per state (approximate times, please check them before the night)
- [x] A3 `scripts/watch/slack.R`: post, reply, update, replies, history, auth.test; dry-run outbox (dry-run tested in test_watch.R; live Slack tested by Mason 2026-10-31: auth.test, post, live tick, pinned status, `!status`)
- [x] A4 `.gitignore` and the `watch/` layout (state outside the repo, in `$WATCH_HOME`) (state lives in $WATCH_HOME (~/.cache/cbs-enr-watch), outside the repo, so nothing to gitignore)

## B. L1 checks (`scripts/watch/checks.R`)
- [x] B1 sync: fetch `pipeline-state` and `dashboard-live` into `$WATCH_HOME` (sync.R; run against the real branches)
- [x] B2 cycle steps (`cycle.json`): step FALSE, disk (forced trip paths with a fake cycle.json)
- [x] B3 target errors and warnings from the pushed meta and progress; consecutive counts; transient rules; poll-close gating (real meta: 49 errored, 24 findings after removing knock-ons). Warnings grouped by shape (`warn_shape()`: quoted strings and numbers masked), key = hash of the shape: real meta 21 warnings -> 7 findings (2026-10-31)
- [x] B4 enightdata sha change with an error jump (P0) (injected sha change: P0). Since 2026-10-31: baseline = newest of the last 3 cycles with a different sha, and once fired the baseline is carried in the metrics while that sha stays installed, so the P0 stays open until errors fall (test_regress.R)
- [x] B5 frozen timestamps (hash of `timestamp_*` in meta unchanged after poll close while others move) (forced with altered prev metrics)
- [x] B6 scraper output from the clean `*_latest.parquet`: rows to 0, votes decrease, candidate set change, 0-byte files (183 files; reads only files whose mtime changed). Rows-to-0 and votes-down compare with the most of the last 3 cycles (test_regress.R)
- [x] B7 methods match via `returns_dropped()`, with buckets excluded; unknown_geo/unknown_method vs the lowest of the last 3 cycles; cells cached per state (11-12s per cycle). The old absolute baseline (`precinct_geo_match_rates.csv`, votehub polygons, 4 states) is gone; B7b replaces it
- [x] B7b `check_crosswalk_match()`, every state with clean files and a crosswalk (13 states on 2026-10-31; ID, IL, IN, KS, KY, MO, NJ, NY, RI, SD, UT, WI have none, listed in metrics `no_crosswalk`). Two maps, checked separately: `crosswalks/{st}.csv` (ENR -> frame -> L2) and `crosswalks/votehub/{st}.csv` (ENR -> VoteHub unit). Per state x map: `rate` (feed precincts that match, in covered counties) and `present` (share of the map's matched ENR ids in the feed; 100% for every state x map on 2026-10-31). Rates come from `scripts/util/crosswalks/match_rates.R`, shared with `/enr-cycle`, which writes the baseline `docs/enr_cycle/crosswalk_match_baseline.csv` at the end of phase 3 (first written 2026-10-31: 23 state x map rows). Trips: rate or present > 5pp below the baseline, or rate down > 5pp from the best of the last 3 cycles; feed counties with no crosswalk that the baseline doesn't list are a P2 line; a crosswalk newer than its baseline row is a P2 `baseline_stale`. ~1 s per cycle (feed geos cached in `state/feed_geos.rds`). Tested: test_regress.R (fake 26_general and baseline: renamed ids trip both maps, known uncovered counties stay quiet, new ones and stale baselines report), test_checks.R on real data (0 crosswalk findings against the baseline)
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
- [x] F1 `tick.sh`, launchd plist, `install.sh` (launchd loaded on the watcher laptop 2026-10-31, the one with this Dropbox checkout; the scraper laptop is a separate machine. Ticks every ~60 s, exit 0, launchd.err empty. Then unloaded with `install.sh remove` after the test; run `bash scripts/watch/install.sh` to start it again. The env file is kept.)
- [x] F2 unit tests: verb parsing, reconcile, mode parsing, ruling expiry (scripts/watch/test_watch.R: 24 checks pass; scripts/watch/test_regress.R: 16 fault-injection checks for the history, B4, B6, warning groups and B7b pass, 2026-10-31)
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
- B7b: CA Nevada, Riverside, San Diego and Solano use the consolidated crosswalks (`ca_*_frame_consolidated.csv`, `ca_*_members.csv`), which `match_rates.R` doesn't read, so they count as uncovered (in the baseline, so quiet). Ventura really has none.
- B7b: the baseline is only as fresh as the last `/enr-cycle`. A crosswalk rebuilt without `match_rates.R --write` gives a P2 `crosswalk_match:baseline_stale`.
- B6 only re-evaluates a file when its mtime changes, so a file that drops and then stops updating resolves after 2 cycles.
- Warning group keys changed on 2026-10-31 (`target_warning:<12-char hash>` instead of the target name); the old per-target incidents resolve on their own after 2 cycles.
- Not tested live: a real patch pushed to GitHub. (Slack and launchd on the watcher laptop tested live 2026-10-31.)

## TX Ellis precinct ids (fixed 2026-10-31)
- Symptom: TX_gov/TX_sen unknown_geo 40% (by rows). In Ellis (`tx_070`), 40 of 60 precincts' feed ids carry a polling-place suffix (`1001-501`), and `match_live_geo()` couldn't match them to the cells (`tx_070_1001`).
- Cause: the crosswalk (`scripts/util/crosswalks/tx.R`, rebuilt 2026-10-30 10:25) already maps them (`precinct_enr = 1001-501`), but `model_inputs/TX_*` were prepped on 2026-10-29 14:40 (bridgetschippers), before that, so Ellis cells kept the YouGov ids.
- Fix: reran `scripts/model/prep/prep_model_inputs.R TX` with `MODEL_INPUTS_OUT` to a temp dir, diffed it with the shared files (only Ellis geo x40, `precinct_enr` filled, and 11 new Reeves precincts the crosswalk now covers; turnout, mode history and the prior unchanged apart from the prior's as-of date), then copied TX_precinct, TX_precinct_mode_history, TX_precinct_cd, TX_race_prior and TX_prep_report into `26_general/input_data/model_inputs/`. Ellis unknown_geo rows: 160 -> 0 (Reeves 108 -> 0). `model_inputs_TX` has `cue = always`, so the scraper laptop picks it up on its next cycle once Dropbox syncs. Originals: Dropbox version history (2026-10-29 14:40).
- Other TX counties with no crosswalk (Dallas, Tarrant, Fort Bend, Robertson; most of TX's remaining unknown_geo) are ENR-cycle work: `docs/enr_cycle/open_problems.md`, "Crosswalks".

## Next steps for a fresh session
1. Rerun `Rscript scripts/watch/test_regress.R`, `test_watch.R` and `test_checks.R` after any checks.R change.
2. Crosswalk gaps and the match baseline belong to `/enr-cycle` (phase 3 ends with `match_rates.R --write`), not to the watcher. The watcher only reads the baseline.

## Needs Mason
- [x] Slack workspace, bot token, channel ID, approver user IDs (done 2026-10-31; values in `~/.config/cbs-enr-watch/env` on the watcher laptop)
- healthchecks.io URLs (watcher and scraper)
- [x] The watcher laptop: this machine (Mason's, with the MIT Dropbox checkout); install.sh run and tested 2026-10-31, then unloaded (not running). The scraper laptop is a different machine.
