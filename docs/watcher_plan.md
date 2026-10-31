# Election-night watcher agents: plan

Status: built 2026-10-31 (`scripts/watch/`, setup in `scripts/watch/README.md`, progress in `docs/watcher_tasks.md`).
Drafted with Claude, reviewed by two subagents (codebase grounding; agent reliability),
then revised with Mason's answers.

## Decisions so far

| Question | Decision |
|---|---|
| Where the watcher runs | A **separate laptop**, not the scraper laptop. The scraper laptop pushes its targets metadata and logs to GitHub each cycle (branch `pipeline-state`, section 0); the watcher reads them from there plus the shared Dropbox. |
| Autonomy | Narrow auto-fix allowlist; everything else asks (section 3). |
| Human channel | Slack, possibly a separate workspace (not the MIT one). Approve allowlist: Mason and Joe (Joe's Slack user ID to be added by Mason). |
| LLM budget | Toggle between two modes during the night (section 1, "Modes"): `llm` (an LLM reviews every cycle) early in the night, `deterministic` (LLM only on anomaly plus periodic check-ins) once things settle. |
| Billing | Mason's Claude subscription only. Never an API key: the supervisor unsets `ANTHROPIC_API_KEY` before calling `claude`. |
| VoteHub `office_filter` | Applies to S3 uploads only; Google Drive gets every office (section 0). |
| enightdata on the night | `automation.R` checks GitHub for a new enightdata commit each cycle and installs it from GitHub; nobody edits the scraper laptop's local checkout. Teammates push scraper fixes to GitHub. The branch is `CBS_ENIGHTDATA_BRANCH` (default `main`). PR #19 (`nov26-scrapers`) merged into `main` on 2026-10-30. |
| Scope cut | Not decided yet. |

## 0. Step 0 fixes (done in this pass)

| Problem | Fix |
|---|---|
| `git2r::pull("../enightdata/")` sat outside `tryCatch`; `../enightdata` doesn't exist on every machine; one failed pull ended the loop | `update_enightdata()` in `automation.R`, run each cycle inside the cycle log. It runs `git ls-remote` on `CBS_ENIGHTDATA_BRANCH` and compares the sha with the last installed one (`~/.cache/cbs-enr-enightdata.installed`). When they differ, it resets a clean clone (`~/.cache/cbs-enr-enightdata`) to that sha, runs `devtools::document()` (a pushed NAMESPACE can be stale), then `renv::install()`. Any error keeps the installed version. `_targets.R` skips its local-checkout install when `CBS_AUTOMATION=1`, which `automation.R` sets. |
| Google Sheet read had no `tryCatch` | Wrapped; a failure keeps the previous `metadata.csv`. |
| A failed `dashboard_data` (NULL under `error = "null"`) was saved and pushed | `export_dashboard_data()` refuses NULL or empty data, so the previous export stays in place and the error shows in `tar_meta()`. |
| The log connection was never closed (R allows 128 open connections) | `close(con)` after the sinks. |
| Environment leftovers (`CBS_MODEL_SIM` from a rehearsal would put sim numbers on the live dashboard) | Preflight at the top of `automation.R`: stops unless `CBS_MODEL_SIM` is empty, `CBS_ELECTION_FOLDER` is `26_general` and `CBS_MODEL_CBS_ENV` is `live`, unless `CBS_AUTOMATION_TEST=1`. Sets `CBS_DASH_RENDER=FALSE` (automation renders `mit.html` itself). |
| `office_filter = NULL` overrode the MI/OH Senate filter, and the filter applied to every destination | Filter restored in `_targets.R`. `create_table_cbs()` (enightdata PR #20) applies it to S3 only: the CBS JSON, S3 csv/parquet (local copies `data/cbs_format/*_s3_latest.*`) and the unused wide CSV. The Drive files (`*_latest.*`) hold every office. |
| The watcher laptop can't see the scraper laptop's `_targets/` or `logs/` | Each cycle, `automation.R` force-pushes `_targets/meta/{meta,progress,process}` and the last 24 cycle logs to branch `pipeline-state`, plus `logs/console.log`, `enightdata_sha` and `cycle.json` (run_time, finish time, and TRUE/FALSE for each step: enightdata, tar_make, dashboard push, S3, Sheet). It is one commit, replaced each run, the same mechanism as `dashboard-live`. Targets objects are not pushed. |

Also launch on the night with `Rscript scripts/automation.R >> logs/console.log 2>&1`, because the
dashboard push, S3 and Sheet messages go to the console, not the per-cycle log. `console.log`
is pushed with the cycle logs.

## 1. Architecture

Three layers. All state lives in files, so any piece can restart cold.

```
SCRAPER LAPTOP
  automation.R (every 5 min)
    tar_make() -> dashboard push -> S3 -> Sheet -> push pipeline-state branch

WATCHER LAPTOP
  launchd, every 60 s:
    sync.sh        git fetch pipeline-state; Dropbox is already synced
    L1 checks.R    deterministic, no LLM. Runs once per new cycle (keyed by run_time in the
                   pushed log). Writes watch/state/cycle_{run_time}.json and deduped
                   watch/state/incidents.jsonl. Posts P0s straight to Slack.
    watchdog.sh    staleness of the pushed cycle markers, hourly auth checks
    supervisor     deterministic. Reads incidents.jsonl and Slack threads, parses reply
                   verbs, writes rulings. Starts L2 when there is work (see Modes).
  L2 claude -p     one stateless call per wake: timeout 600, --max-turns, locked-down
                   settings, prompt built from the state files. Diagnoses, posts to the
                   incident's Slack thread, drafts patches on a branch.
```

Why not a long-lived `/loop` session or a swarm:
- A `/loop` session can't be woken from outside.
- Over about 100 wakes it auto-compacts and loses detail.
- A permission prompt blocks it until someone comes back to the laptop.
- `claude -p` per wake is stateless, so a crash loses one call and nothing else.
- With one writer, leases and worktrees protect nothing in v1.

Moving the watcher to its own laptop also means:
- It doesn't compete with the 12 crew workers for CPU.
- It doesn't share the scraper laptop's failure modes (sleep, network).
- It needs GitHub (`pipeline-state`, `dashboard-live`), the shared Dropbox, and read
  access to S3 and the GH Pages runs.
- It can't read `_targets/objects`. Checks use the Dropbox files (clean parquet,
  summaries, `model_results/`) instead.

### Modes

`$WATCH_HOME/mode` holds `llm` or `deterministic`. Change it with the Slack verb `!mode llm` or
`!mode deterministic`, or by editing the file. An optional schedule such as
`llm until 23:30, then deterministic` can live in the same file.

| Mode | When L2 runs |
|---|---|
| `llm` | Every new cycle: one `claude -p` reviews that cycle's L1 JSON and its diffs against the previous cycle, plus any open incidents |
| `deterministic` | Only on a new or changed incident, a Slack reply, or a check-in every 30 min |

L1, the watchdog and the Slack P0 path run in both modes, so the LLM layer is always optional.

## 2. L1 checks

Data sources corrected against the code. For each check: what it reads (on the watcher
laptop), then when it trips. Open for Mason's edits.

**Liveness**
- Reads: run_time markers in the pushed logs, and the commit time on `pipeline-state`.
- Trips: no new cycle for more than 10 min. The watchdog checks this, so it works even if `automation.R` is dead.

**Target errors**
- Reads: `tar_meta(fields = c(error, warnings))` and `tar_progress()` from the pushed meta.
- Trips: a new error, or the same target erroring 3 cycles in a row.

**Frozen timestamps**
- Reads: in the pushed meta, the `data_*` build time compared with the `timestamp_*` value changes, per state.
- Trips: unchanged after the state's polls close while other counties in the state move. Gated by poll-close time.

**Scraper output**
- Reads: `{ST}/clean/*_latest.parquet` and `summaries/*.parquet` (Dropbox).
- Trips: any of
  - rows drop to 0
  - votes decrease (from the most of the last 3 cycles)
  - the candidate set changes
  - a 0-byte Dropbox placeholder file

**Methods and crosswalk match**
- Reads: `returns_dropped(cells, returns)` (`scripts/model/live/R/returns.R`), which gives the `unknown_method` and `unknown_geo` vote share, matched through `match_live_geo()`.
- Trips: a rise of more than 5pp from the lowest of the last 3 cycles.
- Excludes bucket rows (`bucket_class()`, `reporting_levels.csv`), which are about 31% of the vote in NC and AK.

**Crosswalk match (every collected state)**
- Reads: the feed's geos from every `{ST}/clean/*_latest.parquet`, and two different maps: `input_data/crosswalks/{st}.csv` (ENR id -> frame -> L2 precinct) and `input_data/crosswalks/votehub/{st}.csv` (ENR id -> VoteHub unit).
- Measures, per state x map: the share of feed precincts that match, and the share of the map's matched ENR ids still present in the feed (100% when the crosswalks were built).
- Baseline: `docs/enr_cycle/crosswalk_match_baseline.csv`, written by `/enr-cycle` (`scripts/util/crosswalks/match_rates.R --write`, same code as the check).
- Trips: rate or present more than 5pp below the baseline, or the match rate down more than 5pp from the best of the last 3 cycles. Feed counties with no crosswalk that the baseline doesn't already list (P2); fixing them is ENR-cycle work.

**`trust_stage`**
- Reads: share of complete cells by state, compared with metadata `trust_stage`.
- Trips: an untrusted county never completes.

**Models**
- Reads: `model_results/status_latest.csv`, plus a diff of race_ids against `races_from_priors()`.
- Trips: a status that isn't ok, or a race that vanished (a failed state writes no row at all).

**CBS feed**
- Reads: the snapshot age in `26_general/cbs_api/live/` and the CBS status columns in the status file.
- Trips: stale, or an error.

**Dashboard**
- Reads: `dashboard_data.rds` from `dashboard-live` (non-NULL, every `DASHBOARD_CONTESTS` contest present), `gh run list pages.yml`, and S3 `mit.html` LastModified.
- Trips: stale for 2 cycles, or a workflow failure.

**VoteHub uploads**
- Reads: `tbl_cbs_*` errors (meta), and S3 parquet LastModified for each upload row.
- Trips: stale, or an error.

**Redistricting**
- Reads: `model_results/redistricting/live.log` mtime.
- Trips: stale while the models update. The process count is visible only on the scraper laptop, so that check is a v2 item.

**Auth (hourly)**
- Reads, on both laptops: `aws sts get-caller-identity`, `gh auth status`, the Google token, the Slack token.
- Trips: expires within 60 min.

**Disk**
- Reads: `df`, with the scraper laptop's figure pushed in the console log.
- Trips: below the threshold.

## 3. Autonomy tiers

**Auto, done by L1 (deterministic)**
- Post alerts and annotate incidents.
- Suppress known transients (HTTP 405, 429 and 5xx) until they repeat for 3 cycles.

**Auto, done by L2**
- Diagnose and post the evidence.
- Draft patches on a branch. L2 never writes to live code.

**Ask first.** An allowlisted human replies `approve I-42`, then a deterministic `apply` script runs:
- Push an enightdata scraper patch to GitHub. The next cycle installs it.
- `tar_invalidate(data_ST_county)`, run on the scraper laptop between cycles.
  - This is needed because a scraper fix does not invalidate `data_*`: scrapers are looked up with `get()` at run time (`functions.R:8,20`).
  - Without it, a wrong-but-not-erroring output stays wrong until the county posts again.
  - Running it remotely needs a small command channel. One option is a `commands` file on a branch that `automation.R` reads between cycles. To be designed.
- Set `updated=FALSE` in the Sheet. This takes effect one cycle later and removes a `tar_map` branch.
- Any change to models, `model_config.csv`, `functions.R`, `globals.R` or `_targets.R`. Edits to `functions.R` invalidate broadly.

**Never**
- Run `tar_make`.
- Run `tar_invalidate` without approval.
- Push to cbs-enr `main`, or force-push.
- `aws s3 cp`, `aws s3 rm` or `aws s3 sync`.
- `kill` or `rm`.
- Edit generated files: `metadata.csv`, `_targets/`, `data/dashboard/live/`.

**How this is enforced**
- The watcher has its own `settings.json` with a Bash allowlist. A deny-list on Edit/Write alone isn't enough, because `sed -i` or `Rscript -e` gets around it.
- A PreToolUse hook blocks every tool while `$WATCH_HOME/STOP` exists. The Slack verb `!stop` creates that file.

## 4. Slack and human feedback

Setup:
- A bot token with `chat:write` and `channels:history`, in a private channel. This may be a new workspace just for this. A webhook can post but can't read replies.
- One thread per incident; the thread's `ts` is stored in the incident record.
- One pinned status message, updated with `chat.update`. It shows the mode, the last cycle, open incidents and the last LLM run.
- A per-state digest at most every 2 minutes. `@here` only for P0.

Who can act:
- Actions are accepted only from an allowlist of Slack user IDs (Mason, Joe) and only as parsed verbs:
  - `approve I-42`
  - `deny I-42`
  - `ignore I-42 until 23:30`
  - `!mode llm`, `!mode deterministic`
  - `!stop`
- Free text is context for the LLM, never permission.

Avoiding repeat questions:
- The supervisor (not the LLM) writes each ruling to `state/decisions.json` from the parsed verb, and echoes the ruling in the thread.
- A ruling matches the exact `check_id+key`. A wildcard applies only if the human types one.
- Rulings expire by default at the end of the night.
- L1 and L2 check `state/decisions.json` before alerting or asking.
- If nobody answers, the default is always "no change". Nothing auto-acts when a deadline passes.

Notes:
- The night's notes go to `$WATCH_HOME/notes/2026-11-03.md`.
- After the night, good rulings get promoted to `watch/playbook.md` for the next election.

## 5. Catchpoints

**A fix gets overwritten by the next cycle**
- Agents write only to sources of truth (enightdata on GitHub, the Sheet).
- They never write to generated files.

**A fix takes more than 5 min, or the live code is half-edited**
- The scraper laptop installs enightdata only from GitHub.
- Patches stay on a branch until they are approved.
- The apply step is one push.

**Teammates push to enightdata mid-night**
- The apply script rebases onto origin before pushing.
- Every install is of a whole GitHub commit, never a half-merged local tree.

**A bad enightdata push breaks every state at once**
- L1 tracks the installed `RemoteSha` and compares error counts before and after it changes.
- On a jump in errors, L1 posts a P0 naming the commit.
- Rolling back is a human revert on GitHub.

**A cycle runs past 5 min, so the next one starts at once**
- Checks key on run_time, so an overlapping cycle is not checked twice.
- Liveness comes from cycle markers, not log mtime.

**`pipeline-state` push fails or lags**
- Its own staleness is a watchdog check.
- The watcher falls back to the Dropbox files.

**Subscription usage window runs out**
- The watcher shares Mason's subscription, so heavy interactive use on the night draws from the same limit.
- Early in the night, `llm` mode costs the most. Watch the pinned status message.
- L1, the watchdog and the Slack P0 path keep working with no LLM.

**Auth expires (AWS SSO around 2am, GitHub, Google)**
- Hourly check on both laptops, with a warning 60 min ahead.

**The agent "fixes" the check instead of the data (for example, maps an unknown method to `eday`)**
- Method mappings are always ask-tier.
- They need evidence from the vendor's page.

**Prompt injection from scraped pages or Slack text**
- Both are treated as data.
- L2 has no push or S3 tools.
- Only verbs from allowlisted user IDs unlock actions.

**The scraper laptop sleeps or loses network**
- Run it under `caffeinate`.
- The external dead-man ping pages a phone.

## 6. Open items

- Which L1 checks and thresholds to keep (Mason to edit section 2).
- Slack workspace and the user IDs for the allowlist.
- The command channel for `tar_invalidate` and other approved actions that must run on the scraper laptop.
- Scope for the first build, and the rehearsal plan. Proposed rehearsal on 11/2 with the sim feed and injected faults:
  - break a scraper
  - add a new method label
  - freeze a timestamp
  - kill `automation.R`
  - expire a token
  - send a reply from a user not on the allowlist
  - create `STOP`
  - run 2 hours unattended
- v2: Sonnet worker subagents in worktrees with leases, auto-apply for playbook patterns, an Agent SDK daemon, model-jump checks.
