# Watcher: implementation design

This file holds the interfaces behind `docs/watcher_plan.md`: files, formats and who writes what.
The task list is in `docs/watcher_tasks.md`. Code is in `scripts/watch/`.

## Processes

One launchd job on the watcher laptop runs `scripts/watch/tick.sh` every 60 s. A lock
directory makes ticks never overlap, and every step of a tick runs in order inside one
`Rscript scripts/watch/tick.R` process. Only that process writes the state files, so the
state needs no locking of its own. Each step:

1. **sync**
   - Fetches `pipeline-state` and `dashboard-live` from GitHub into `$WATCH_HOME`.
   - Pings `CBS_WATCH_HEALTHCHECK_URL`, if set.
2. **checks (L1)**
   - Runs once for each new `cycle.json` `run_time`.
   - Writes `state/cycles/{run_time}.json`.
   - Reconciles findings into `state/incidents.json`.
3. **watchdog**
   - Pipeline staleness, every tick.
   - Auth checks, hourly.
4. **slack in**
   - Reads replies in awaiting threads and top-level verbs.
   - Parses the verbs and acts on them: rulings, mode, stop, approve.
5. **L2 ingest**
   - If a finished L2 run left `state/l2/out_{n}.json`, posts its diagnoses and proposals, and marks it ingested.
6. **L2 launch**
   - If the mode, a new incident or a check-in calls for it, and no L2 is running, starts `scripts/watch/l2.sh` detached.
   - Pidfile: `state/l2/running.pid`.
7. **slack out**
   - Posts new or escalated incidents and the digest.
   - Updates the pinned status message.

L2 is `claude -p` on Mason's subscription (`ANTHROPIC_API_KEY` unset), with a 600 s timeout.
It never posts or pushes. It writes one JSON result, which the next tick ingests.

The scraper laptop's `automation.R` does three new things each cycle:
- Pings `CBS_HEALTHCHECK_URL`, if set.
- Adds `disk_free_gb` to `cycle.json`.
- Runs approved commands from branch `pipeline-commands` (see "Command channel").

## Paths

`WATCH_HOME` = `$CBS_WATCH_HOME`, default `~/.cache/cbs-enr-watch`. It sits outside Dropbox,
so state never syncs back to the scraper laptop.

```
$WATCH_HOME/
  repo/                 side git repo (fetch only) for pipeline-state, dashboard-live, pipeline-commands
  pipeline-state/       checkout of pipeline-state: _targets/meta/*, logs/*, cycle.json, enightdata_sha
  dashboard-live/       dashboard_data.rds
  enightdata/           clone used for L2 patch branches (watch/I-<n>)
  state/
    incidents.json      {"next_id": n, "incidents": {"I-1": {...}, ...}}
    decisions.json      [ruling, ...]
    cycles/{run_time}.json   L1 output (findings + metrics) per cycle
    metrics_prev.json   last cycle's per-jurisdiction metrics, for diffs
    slack.json          {status_ts, last_seen_ts, ...}
    slack_outbox.log    every Slack call in dry-run (no token)
    l2/                 prompt_{n}.md, out_{n}.json, log_{n}.txt, running.pid
    commands.json       approved commands not yet confirmed done by the scraper laptop
    last_auth_check, last_checkin
  mode                  "llm", "deterministic", or "llm until 23:30"
  STOP                  exists means: L2 never starts, the hook blocks all tools, Slack posts only P0
  notes/YYYY-MM-DD.md   one line per decision (supervisor and L2)
```

Settings in the environment (`~/.config/cbs-enr-watch/env`, chmod 600, read by `tick.sh`):

| Variable | Use |
|---|---|
| `SLACK_BOT_TOKEN` | Bot token. Unset means dry-run: calls go to `slack_outbox.log`. |
| `SLACK_CHANNEL` | Channel ID |
| `CBS_WATCH_APPROVERS` | Comma-separated Slack user IDs allowed to use verbs |
| `CBS_WATCH_HEALTHCHECK_URL` | Dead-man ping for the watcher |
| `CBS_WATCH_MODEL` | L2 model, default `sonnet` |
| `CBS_WATCH_REPO` | Repo checkout on the watcher laptop, default this repo |
| `CBS_WATCH_DASH_AUTH` | `user:pass` for the CloudFront dashboard (HTTP 401 without it) |
| `CBS_WATCH_NO_L2` | `1` = ticks never start L2 (tests) |
| `PATH_DROPBOX` | As in `globals.R` |

## Findings (L1 output)

Each check returns zero or more findings:

```json
{"check": "target_error", "key": "target_error:data_FL_pasco", "state": "FL",
 "severity": "P2", "message": "data_FL_pasco errored: HTTP 404 Not Found.",
 "value": null, "evidence": {"error": "..."}, "transient": false}
```

- `key` is unique, stable across cycles, and is what rulings match on.
- `severity`:
  - `P0`: page the humans, posted at once, `@here`.
    - The pipeline is stale.
    - `tar_make` failed.
    - Errors jumped after an enightdata change.
    - The dashboard is stale or its data is NULL.
  - `P1`: posted in its own thread.
    - A model status isn't ok, or a race vanished.
    - After poll close: a scraper's rows drop to 0, its votes decrease, or its timestamp freezes.
    - Match rate drops.
    - The CBS feed is stale.
    - A cycle step failed.
  - `P2`: listed in the digest and the status message only.
    - Target errors before the state's poll close.
    - Warnings.
- `transient: true` (HTTP 405/429/5xx, timeouts) becomes an incident only after 3 cycles in a row.
- Poll-close gating uses `scripts/watch/poll_close.csv` (state, ET time of the state's last poll closing).

## Incidents

```json
{"id": "I-7", "key": "...", "check": "...", "state": "FL", "severity": "P1",
 "status": "open|awaiting|approved|denied|ignored|resolved",
 "message": "...", "evidence": {}, "first_seen": "...", "last_seen": "...", "cycles": 3,
 "clean_cycles": 0, "slack_ts": null, "posted_severity": null,
 "proposal": null, "l2_runs": 0, "history": ["...one line per event..."]}
```

Reconcile, for each cycle:
- A finding with an open incident updates it: `last_seen`, `cycles`, and a severity raise.
- A finding with no open incident opens a new one, unless a live ruling matches its key.
- An open incident with no finding this cycle gets `clean_cycles + 1`. At 2 it becomes `resolved`, and a "resolved" reply goes to its thread if it had one.
- `ignored` incidents stay quiet until their ruling expires.

## Rulings (`decisions.json`)

```json
{"key": "target_error:data_FL_pasco", "verb": "ignore", "until": "2026-11-04T06:00:00-05:00",
 "by": "U123", "at": "...", "incident": "I-7", "text": "ignore I-7 until 23:30"}
```

- The supervisor writes rulings from parsed verbs. The LLM never writes them.
- A ruling matches the exact key. A human may type a wildcard with `ignore key:<glob>`.
- The default `until` is 06:00 ET the next morning.

## Slack verbs

These are accepted only from `CBS_WATCH_APPROVERS` IDs, in an incident thread or at the top level.

| Verb | Effect |
|---|---|
| `approve I-7` | Runs the incident's proposal through `apply.R` |
| `deny I-7` | Status `denied`; ruling `deny` (no re-ask for that key tonight) |
| `ignore I-7 [until HH:MM]` | Ruling `ignore` |
| `ignore key:<glob> [until HH:MM]` | Wildcard ruling |
| `!mode llm`, `!mode deterministic`, `!mode llm until HH:MM` | Writes `mode` |
| `!stop`, `!start` | Creates or removes `STOP` |
| `!status` | Re-posts the status message |

Any other text in an incident thread from an approver counts as context. It triggers an L2 run
in both modes, and L2 sees it as human input. Messages from anyone else are ignored and logged.

## L2

`scripts/watch/l2.sh <n>` runs:

```
env -u ANTHROPIC_API_KEY timeout 600 claude -p "$(cat prompt_n.md)" \
  --model "$CBS_WATCH_MODEL" --permission-mode dontAsk \
  --settings scripts/watch/claude_settings.json --setting-sources "" \
  --output-format json --json-schema "$(cat scripts/watch/l2_schema.json)" \
  --no-session-persistence --add-dir <repo> <dropbox 26_general> <WATCH_HOME>
```

- **Prompt:** `scripts/watch/l2_prompt.md` (role, rules, tiers), followed by the generated context:
  - the mode
  - the latest cycle JSON
  - open incidents
  - live rulings
  - the playbook
  - the tail of tonight's notes
  - human messages
- **Tools:** `Read`, `Grep`, `Glob`, and `WebFetch` for vendor pages. Bash only for
  `Rscript scripts/watch/probe.R *` (runs a scraper against a temp Dropbox, never the real
  one), `Rscript scripts/watch/inspect.R *` (read-only parquet, errors, dropped geos), and
  `git -C $ENIGHTDATA` (no push). Edit/Write only under `$WATCH_HOME/enightdata/**` and `$WATCH_HOME/notes/**`.
- **Output:** see `l2_schema.json`.
  - `summary`
  - `incidents[]`, each with `id`, `diagnosis`, `confidence`, and `proposal`. A proposal is `{action: none|patch|invalidate|human, text, branch, targets}`.
  - `observations[]`
  - `notes`

## Apply (`apply.R`, run by the supervisor on `approve`)

| Proposal | What runs |
|---|---|
| `patch` | In `$WATCH_HOME/enightdata`: fetch, rebase `watch/I-n` onto `origin/main`, check it parses (`load_all` in a subprocess), `git push origin HEAD:main`. Then queue `invalidate` for the proposal's `targets`. |
| `invalidate` | Append `{id, action: "invalidate", targets, by, at}` to `state/commands.json` and force-push the file to branch `pipeline-commands`. |
| `human` | Nothing runs; the text is the instruction for a person (Sheet edits, model config). |

## Command channel (scraper laptop)

At the start of each cycle, `automation.R`:
1. Fetches `pipeline-commands`.
2. For each command whose id isn't in `~/.cache/cbs-enr-commands.done`, runs it. Only `invalidate`
   is allowed, and only for names matching `^(timestamp|data|tbl_cbs|summary)_[A-Z]{2}_`.
3. Records the id as done.
4. Lists the ids in `cycle.json` under `commands_done`.

The watcher drops a command from `commands.json` once it shows up in `commands_done`.
