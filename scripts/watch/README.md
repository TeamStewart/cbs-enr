# Election-night watcher

This watches the cbs-enr pipeline from a second laptop and talks to people in Slack. The plan
is in `docs/watcher_plan.md`, the interfaces are in `docs/watcher_design.md`, and the build
status is in `docs/watcher_tasks.md`.

The scraper laptop runs `scripts/automation.R`. Each cycle it pushes branch `pipeline-state`
and runs approved commands from branch `pipeline-commands`. The watcher laptop runs
`scripts/watch/tick.sh` every 60 s from launchd. A tick does these steps in order:
- fetch the pipeline state
- run the deterministic checks (L1) once per cycle
- run the watchdog
- read Slack verbs
- start `claude -p` (L2) when needed
- post to Slack

## Files

| File | What |
|---|---|
| `tick.R`, `tick.sh` | The supervisor (one tick) and its launchd wrapper (env file, lock, PATH) |
| `sync.R`, `checks.R` | Fetch `pipeline-state`/`dashboard-live`; the L1 checks |
| `incidents.R` | Incidents, rulings, Slack verbs, modes, status text |
| `slack.R` | Slack Web API client. With no token, calls go to `state/slack_outbox.log` (dry-run). |
| `l2.R`, `l2.sh`, `l2_prompt.md`, `l2_schema.json` | The L2 run: context, `claude -p` flags, instructions, output |
| `probe.R` | L2's only Bash tool: runs one scraper against a temp Dropbox |
| `apply.R` | What `approve` runs: patch push, invalidate command, or nothing (`human`) |
| `playbook.md` | Known patterns, read by every L2 run |
| `poll_close.csv` | Last poll close (ET) per state, which gates severities. **Check these.** |
| `test_watch.R`, `test_checks.R` | Unit tests; checks against the real pipeline state |

Runtime state lives in `~/.cache/cbs-enr-watch` (`CBS_WATCH_HOME`), outside Dropbox:

| Path | What |
|---|---|
| `state/` | incidents, rulings, cycles, L2 runs |
| `notes/` | tonight's log |
| `mode` | the current mode |
| `STOP` | the kill switch |
| `tick.log` | output of every tick |

## Set up the watcher laptop

1. Clone cbs-enr and run `renv::restore()`. Install enightdata from GitHub
   (`renv::install("TeamStewart/enightdata")`); `checks.R` needs it for the model helpers.
2. Have the shared Dropbox synced, with `26_general` available offline (not online-only).
   Set `PATH_DROPBOX` in `~/.Renviron` if `globals.R` doesn't find it.
3. `gh auth login`. You need push access to TeamStewart/cbs-enr (for `pipeline-commands`)
   and TeamStewart/enightdata (for approved patches). Also check SSH access to GitHub:
   `ssh -T git@github.com`.
4. `claude` logged in with Mason's subscription (`claude /login`). No `ANTHROPIC_API_KEY`
   anywhere: `l2.sh` unsets it, but don't keep one in your shell profile either.
5. Slack (see below), then fill in `~/.config/cbs-enr-watch/env`. `install.sh` creates the
   file on first run.
6. `scripts/watch/install.sh` loads the launchd job.
   - If ticks fail with "Operation not permitted" on the Dropbox paths, give `/bin/bash`
     Full Disk Access (System Settings > Privacy & Security).
   - Stop the job with `scripts/watch/install.sh remove`.
7. Optional: two healthchecks.io checks, each with a 10-minute period and phone alerts.
   - The watcher's goes in `CBS_WATCH_HEALTHCHECK_URL` in the env file.
   - The scraper laptop's goes in `CBS_HEALTHCHECK_URL` in that laptop's `~/.Renviron`.

### Slack app

1. Create an app at https://api.slack.com/apps (From a manifest) in the workspace you want:

   ```yaml
   display_information:
     name: cbs-enr watcher
   features:
     bot_user:
       display_name: cbs-enr watcher
   oauth_config:
     scopes:
       bot: [chat:write, channels:history, groups:history, pins:write]
   ```

2. Install the app, then copy the Bot User OAuth Token (`xoxb-...`) to `SLACK_BOT_TOKEN`.
3. Create a private channel and `/invite @cbs-enr watcher`. The channel ID is in the
   channel's details, at the bottom; put it in `SLACK_CHANNEL`.
4. Find each approver's member ID: profile > ... > Copy member ID. Put them in
   `CBS_WATCH_APPROVERS`, comma-separated. Only these people's verbs count.

## On the night

Reply in an incident's thread, or at the top level of the channel:

| Verb | Effect |
|---|---|
| `approve I-7` | Runs the proposal: pushes the patch to enightdata `main` and queues a rerun of the targets, or queues the invalidate |
| `deny I-7` | No, and don't propose it again tonight |
| `ignore I-7 [until 23:30]` | Quiet until then; the default is 06:00 |
| `ignore key:target_error:data_TX_* [until 23:30]` | A wildcard ruling (only when you type the `*`) |
| `!mode llm`, `!mode deterministic`, `!mode llm until 23:30` | `llm`: L2 reviews every cycle. `deterministic`: L2 runs only on new P0/P1s, replies and a 30-min check-in. |
| `!stop`, `!start` | The kill switch: no L2, every L2 tool blocked, only P0 posts |
| `!status` | Re-post the status message |

Any other text from an approver in a thread goes to L2 as context, and starts a run.

Without Slack, edit the files directly:
- `echo llm > ~/.cache/cbs-enr-watch/mode`
- `touch ~/.cache/cbs-enr-watch/STOP`

Then read `notes/` and `state/slack_outbox.log`.

## Test

```sh
Rscript scripts/watch/test_watch.R                     # unit tests
CBS_WATCH_HOME=/tmp/wt Rscript scripts/watch/tick.R    # one dry-run tick against the real pipeline-state
```

## Rehearsal checklist (11/2)

Run the scraper laptop on a sim, with `CBS_AUTOMATION_TEST=1` and `CBS_MODEL_SIM=...`.
Run the watcher with Slack live. For each fault, note how long it takes to show up in Slack.

- [ ] Normal cycles: the status message updates every cycle, and there are no P1s.
- [ ] Break a scraper: push a commit to enightdata that errors for one county. Expect an incident, an L2 diagnosis and a proposal; `approve` pushes the fix and the invalidate runs next cycle.
- [ ] A new vote-method label in one county's clean file: a methods-check incident, with proposal `human`.
- [ ] Freeze a timestamp: a P1 after 30 min, after poll close.
- [ ] Kill `automation.R`: a P0 within about 11 min, and the scraper's healthchecks.io alert.
- [ ] Sleep the watcher laptop: the watcher's healthchecks.io alert.
- [ ] `gh auth logout` on the watcher laptop: an auth P1 within the hour.
- [ ] A reply from someone not on the allowlist: ignored and noted.
- [ ] `!stop`: no L2 runs, and L2 tool calls are blocked. `!start` resumes.
- [ ] `!mode llm`, then `!mode deterministic`: L2 every cycle, then only on events.
- [ ] Two hours unattended: no runaway L2 runs, and usage stays within the subscription limits.
- [ ] Afterwards: unset `CBS_MODEL_SIM` and `CBS_AUTOMATION_TEST` on the scraper laptop, and clear `~/.cache/cbs-enr-watch/state` on the watcher. Keep `~/.cache/cbs-enr-commands.done`, so commands already run never run again.
