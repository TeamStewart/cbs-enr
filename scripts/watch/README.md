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

## Election Day (11/3)

The rehearsal ran on 11/2. Start the scraper first: the watcher posts a P0 when no new
cycle has arrived for 10 min.

Scraper laptop (the Mac mini, user `stewart`):
1. `git pull` on `main`.
2. Open a new terminal, so no rehearsal settings are left over. `CBS_MODEL_SIM`,
   `CBS_AUTOMATION_TEST`, `CBS_ELECTION_FOLDER` and `CBS_MODEL_CBS_ENV` must be unset;
   `automation.R` refuses to start otherwise.
3. Start it before the 18:00 ET cutover in `dashboard_mode.txt`. Publishing to Pages and
   CloudFront starts on its own at the cutover:
   `caffeinate -i Rscript scripts/automation.R`
4. Keep `~/.cache/cbs-enr-commands.done`, so commands already run never run again.

Watcher laptop (Mason's):
1. `git pull` on `main`.
2. In `~/.config/cbs-enr-watch/env`, take out `CBS_ELECTION_DATE` if it is still there. Add
   `CBS_WATCH_DASH_AUTH=user:pass` if you have the CloudFront login; without it the dashboard
   check posts a P2 every cycle.
3. Clear the rehearsal's incidents: `rm -rf ~/.cache/cbs-enr-watch/state`.
4. Once the first scraper cycle has pushed, run `bash scripts/watch/install.sh`. Check
   `~/.cache/cbs-enr-watch/tick.log` and the pinned status message in Slack (`!status`).
5. Keep the laptop awake and plugged in (`caffeinate -dims &`). launchd doesn't tick while
   the laptop sleeps.

After the night: `bash scripts/watch/install.sh remove` on the watcher, and stop
`automation.R` on the scraper.

