# Scraper laptop: what the published dashboard shows

The scraper laptop runs `scripts/automation.R` (see README, "Election Night"). Each cycle it
runs `tar_make()`, force-pushes the dashboard data to the `dashboard-live` branch, and, once
the dashboard is live, starts a GitHub Pages build and uploads `mit.html` to CBS's S3 bucket.
The watcher (`docs/watcher_plan.md`) reads `dashboard-live` and `pipeline-state` from another
laptop.

Whether the published dashboard shows test or live data is set in code, not by environment
variables or one-off workflow runs. Full details: `docs/dashboard_local.md`, section "Test or
live data: `dashboard_mode.txt`".

## The switch: `dashboard_mode.txt`

`dashboard_mode.txt` at the repo root, committed on `main`, has two settings:

```
mode: auto
cutover: 2026-11-03 18:00
```

`cutover` is Eastern time. `scripts/util/dashboard_mode.R` implements the rules, and three
places use it:

| Where | Before the cutover (`mode: auto`) | After the cutover |
|---|---|---|
| GitHub Pages (`.github/workflows/pages.yml`) | test data (`data/dashboard/dashtest_data.rds`) with a red TEST DATA badge | `dashboard-live` data pushed at or after the cutover; else an "awaiting results" page |
| `automation.R` | pushes to `dashboard-live` only; no Pages build, no `mit.html` upload (the S3 step shows as NA in `cycle.json`) | pushes, starts a Pages build, uploads `mit.html` every cycle |
| Watcher (`scripts/watch/checks.R`) | skips the "CloudFront `mit.html` stale" check | checks it |

`mode: test` keeps test data everywhere and never uploads `mit.html`. `mode: live` publishes
whatever `dashboard-live` holds, however old, and uploads `mit.html` every cycle.

Pages rebuilds on any push to `main` that changes `dashboard_mode.txt`, and once on a
schedule at the default cutover (2026-11-03 23:01 UTC = 18:01 EST). The scheduled run takes
the test data off the page even if no live push has arrived yet.

## Common actions

- **Switch to live early and clear rehearsal data.** Run
  `Rscript scripts/util/dashboard_mode.R cutover-now`, then commit and push
  `dashboard_mode.txt`. Data pushed to `dashboard-live` before the new cutover is ignored, so
  the page shows "awaiting results" until the next automation cycle pushes.
- **One-off look at live data on Pages** (the file does not change):
  `gh workflow run pages.yml -f data=live`. It stays until the next build without
  `data=live`. Before the cutover, automation does not start builds, so the next push to
  `main` that touches the dashboard replaces it.
- **Force test data once:** `gh workflow run pages.yml -f data=test`.
- **Rehearsal on the scraper laptop before the cutover**
  (`CBS_AUTOMATION_TEST=1 Rscript scripts/automation.R`): data still goes to
  `dashboard-live` for the watcher, but neither copy publishes it. To test the S3 upload,
  call `upload_dashboard_s3()` by hand, or set `mode: live` for the rehearsal and set it
  back afterward.
- **See which source a build used:** the "Pick data" step log in
  `gh run view <id> --log` prints the mode, cutover, live data time, and the result.

## Caveat: which copy of the file is read

`automation.R` reads `dashboard_mode.txt` from the scraper laptop's working copy, and the
watcher reads it from its own checkout (`WATCH_REPO`). GitHub Pages reads it from `main`.
After an early switch, make sure the scraper laptop's copy has the change too (Dropbox
sync or `git pull`). Otherwise Pages may say "live" while the laptop still skips the S3
upload and Pages builds.
