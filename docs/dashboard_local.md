# Running the dashboard locally

The dashboard is `index.qmd`, rendered to a self-contained `index.html` (`embed-resources: true`). It still has to be served over HTTP. Opened as a `file://` URL (double-click, or `open index.html`), Quarto's OJS runtime fails to start (`Cannot read properties of undefined (reading 'setLocalResolver')`) and every box and chart stays blank. Serve the project root with any static server and open the page from localhost:

```bash
python3 -m http.server 8000 --bind 127.0.0.1   # from the project root; Ctrl-C to stop
open "http://127.0.0.1:8000/index.html"        # or dashtest.html
```

The page also loads Observable's stdlib (d3, Plot) from jsdelivr at view time, so it needs internet access.

## One-time setup

```r
install.packages(c("pak", "renv"))
pak::pak("CoryMcCartan/enightmodels")
pak::pak("TeamStewart/enightdata")
renv::install()   # from the project root
```

Also requires the Quarto CLI (`quarto --version`) and your username in the `PATH_DROPBOX` block of `scripts/util/globals.R`.

## Live data (pipeline)

```bash
Rscript -e 'targets::tar_make(dashboard)'   # scrapes upstream targets, then renders index.qmd
open "http://127.0.0.1:8000/index.html?c=GA-sen"   # server above running; ?c= opens a specific selection
```

Use the `CBS_ELECTION_FOLDER` env var to point at a folder other than the default `26_general`. Selections shown are set by `DASHBOARD_CONTESTS` in `scripts/util/globals.R`.

## Model results (Models page)

The Models page plots enightmodels output over time for the selected contest. `read_model_results()` (`scripts/functions.R`) stacks every `{ELECTION_FOLDER}/model_results/predictions_*.csv` (schema in the enightmodels README, plus `geo` = `scraped`/`all` and `race_id`) into a time series keyed on `time`, takes each race/model's latest row from `status_*.csv`, and reads the race totals' intervals from `turnout_*.csv` (CONTRACT.md §15.4). Predicted Turnout draws that interval as a ribbon; runs written before the turnout table existed, and models with no turnout interval (conformal, `ratio_strata`), show the line only. The `model_results` target re-reads the folder after `model_out` writes it, so a new model run shows up on the next dashboard render.

Rows join to `DASHBOARD_CONTESTS` on state + contest + district (NA district = statewide). Contests with no rows show a "not modeled" message; models that appear only in the status files (failed runs) are listed but can't be picked.

Model names are `<model>[/<spec>][/<interval>]`, with the model's default spec and interval left off (`scripts/model/live/R/config.R`). `model_parts()` (`scripts/functions.R`) looks each name up in `{ELECTION_FOLDER}/model_config.csv` to get `model_base`, `model_spec`, and `model_interval`, so `swing/conformal` reads as swing · ratio · conformal. Names missing from the config are split on `/`, with `default` for missing parts.

The toolbar at the bottom of the page has one Model select (grouped by base model, readable spec and interval labels from `SPEC_LABELS` / `INTERVAL_LABELS` in `index.qmd`; failed models are listed but disabled), a "Statewide" checkbox when the picked model has `geo = "all"` rows (overlaid dashed), and an "As of" date-time in Eastern time. The plots run up to "As of", and "Latest" snaps back to the newest run (the page follows new runs until an earlier time is picked). Add a row to `SPEC_LABELS` / `INTERVAL_LABELS` when a new spec or interval is added to `demos.R` / `results.R`.

The Models table on the left has one row per model with predictions, each at its last run at or before "As of" (a run time shows next to the model when it is older than the newest run): interval method and level (from `model_conf`), predicted Dem two-party share with its interval, predicted turnout with its interval when the model gives one, and "Match", the share of reporting ENR precincts (any votes) that are in the models' inputs with a past vote (`26_general/input_data/model_inputs/{ST}_precinct.parquet`). Match is the same for every model; the note under the table gives the counts and the share of counted votes they hold. Click a row to plot that model.

### Precinct maps

The Results Map and Remaining Vote Map tabs draw VoteHub precincts. Geometry is static for the night, so it is built once and fetched by the page at view time, never embedded:

```bash
Rscript scripts/util/make_dashboard_geo.R          # every state with input_data/crosswalks/votehub/{st}.csv
Rscript scripts/util/make_dashboard_geo.R GA TX    # these states
```

This writes `data/dashboard/geo/{st}.json` (simplified GeoJSON keyed on VoteHub `unit_id`, with each precinct's point on surface; `KEEP=` sets the share of vertices kept, default 0.02). The page loads `{DASHBOARD_GEO_URL}/{st}.json` (`scripts/util/globals.R`, env `CBS_DASH_GEO_URL`; default the relative `data/dashboard/geo`, so serving the project root works, and GitHub Pages copies the folder). To host the files elsewhere (S3 / CloudFront), upload the folder once and set `CBS_DASH_GEO_URL` to its URL; the bucket needs CORS for the page's origin.

County and state outlines come from a second static file per state, also built once:

```bash
Rscript scripts/util/make_dashboard_counties.R          # every state with a {st}.json
Rscript scripts/util/make_dashboard_counties.R MI TX    # these states
```

This writes `data/dashboard/geo/{st}_counties.json` from Census cartographic boundaries (`tigris::counties(cb = TRUE)`, 1:500k; AK uses its 40 state house districts, as CBS and the model inputs do), one feature per county (`id` = 5-digit FIPS) plus the dissolved state outline (`id` = `"state"`). When the file exists, both maps draw county outlines and the state outline over the precincts and fit the map to the whole state, so partly scraped states (TX, OH, MI) show whole. Without it the maps fall back to the precinct extent.

- **Results Map:** counted votes per unit (ENR precincts joined to `unit_id` through `scripts/model/lookups/model_precincts.csv` and the VoteHub crosswalk), filled by two-party margin; grey is not reporting or not matched. Counties with no ENR precincts (the `geo` county prefix) are filled from the latest CBS county snapshot the models saved (`26_general/cbs_api/{env}/{ST}_{contest}/`, `scripts/model/live/R/cbs_county_pull.R`; `read_cbs_counties()` in `scripts/functions.R`), with the snapshot time in the caption. `env` is `DASHBOARD_CBS_ENV` (env `CBS_DASH_CBS_ENV`, default `live`). Snapshots are only written at model runs, so these lag the model cycle. The margin uses CBS's per-method Dem and Rep rows, which can fall short of the county total (`method_shortfall`); a county whose method rows are all zero stays grey.
- **Shift Map:** for each unit with counted votes, the shift in two-party margin from 2024 president to the contest's counted votes. 2024 is VoteHub's presidential estimate on the 2026 precincts (`26_general/input_data/votehub-shapefiles/estimated_past_results.csv`, `read_pres24()` in `scripts/functions.R`; no CA rows), joined on `unit_id` as `dem24` / `rep24` in `mres`. Only counted votes are used, no projections or CBS county fills, so a partly counted unit is compared with its full 2024 vote. Arrows start at the precinct's point on surface and point up-left (35° above horizontal) in the Dem color for a Dem shift, up-right in the Rep color for a Rep shift; length is proportional to the shift and the arrowhead is the same size on every arrow, capped at 30 points. The caption gives the shift over all units shown together. Arrows keep their length when zoomed. It has a **Basemap** picker (see below).
- **Remaining Vote Map:** from the picked model's latest `model_results/precincts_latest.parquet` (`write_precincts()` in `scripts/model/live/R/output.R`): projected final votes minus counted votes per unit, a circle at the precinct's point on surface sized by expected remaining votes and filled by their projected margin. It always shows the latest run, not "As of". It has a **Basemap** picker.
- **Basemap picker (Shift Map, Remaining Vote Map):** each map has its own (`basePicker()` in `index.qmd`), left-aligned under the map's legend, default None. It shades each unit by an L2 share of registrants (% white, % Black, % Hispanic, % Asian, % male), from `read_l2()`'s Total rows aggregated to `unit_id` through `read_geo_units()`, registrant-weighted (`mdemo` in `build_dashboard_data()`); units with no L2 match stay grey. The fill is literal colors, not a Plot color scale (the Remaining Vote Map's circles use that), so its key is drawn under the picker and exports list the bins as legend keys.

Both maps zoom (wheel, pinch, double-click) and pan (drag); Reset at the top right undoes it. Hovering a precinct shows its numbers (browser tooltip, which follows the zoom; Plot's pointer tips don't).

Every map labels the state capital (star, bold) and its major cities, drawn over everything else, arrows and circles included. The list is `CITIES` in `index.qmd` (name, lon, lat, capital, and `"end"` to put the label left of the point); add a state there when it gets a map. Labels move with zoom and pan but keep their size, and use the page font live and Styrene B in exports.

Each render embeds only the per-unit numbers (`mres`, `mprec` in `build_dashboard_data()`).

### Demographics page

Swing by Race, Swing by Urbanicity and Swing by Turnout plot each counted precinct's shift in Dem two-party share against its share in the history year, in blocks. Each block of at least 20 precincts has a vote-weighted linear fit of shift on history-year share (solid blue); every block has its unweighted precinct-average shift (dashed, also in the block's corner); the thin gray line is no shift. They use counted votes only (the latest count, not "As of"), so they don't depend on the picked model. Race blocks are precincts with at least the page toolbar's "Race ≥" share of L2 registrants Black or Hispanic, plus all matched precincts and the rest; Turnout blocks are thirds of precincts by L2 turnout in the history year. Urbanicity is the county's USDA RUCC 2023 label from `scripts/model/lookups/model_counties.csv` (urban 1–3, suburban 4, rural 5–9), read by the render's setup chunk; it covers the states in `STATE_FIPS` in `scripts/model/prep/make_model_reference.R` (AK, CA, FL, GA, MI, MN, NC, OH, TX); others show a message instead, and AK's district-keyed rows are mostly blank.

Race EI, Race Turnout EI and EI Diagnostics show each L2 race group's (White, Black, Hispanic, Asian) Dem two-party share and turnout from ecological inference with [seine](https://corymccartan.com/seine/) (McCartan & Kuriwaki), for the counted votes and for 2024 on the same precincts: those with an L2 match that have counted enough of their vote (below). `dashboard_ei()` (`scripts/functions.R`) fits it in `build_dashboard_data()`, set up like the NYC model (`scripts/model/modeling-cory/nyc/R/nyc_enr_ei.R`): everything is a share of today's L2 registrants, with predictors the race shares of registrants of known race (L2 has no other group) and outcomes Dem, Rep, other and non-voting per registrant (`total` = registrants), so groups can turn out at different rates. No covariates, `ei_ridge()` + `ei_riesz()` + `ei_est()`. A group's turnout is 1 − its non-voting estimate and its Dem two-party share is Dem ÷ (Dem + Rep), with delta-method 95% intervals from `vcov()`. Ballots above registrants are scaled down to them, and precincts over 105% are dropped. 2024 is all L2: today's registrants who voted in 2024 (`l2_turnout24`), split by L2 party label (`l2_dem24`, `l2_rep24` from `vote24_dem`/`vote24_rep`; the rest is other) and treated as if it were vote choice. It isn't: labels are party registration, or L2's model of it where a state has none (GA), so the 2024 Dem share runs more polarized than the vote (Black voters typically come out above 100% Dem and are flagged). L2 also misses 2024 voters who have since left the file, so 2024 turnout reads a little low.

Precincts are kept by `ei_complete_votes()`: counted votes at least `EI_MIN_FRAC` (0.5) of the precinct's 2022 votes, taken from L2 (`l2_turnout22` × registrants: today's registrants who voted in 2022). The EI runs only with at least `EI_MIN_N` (25) such precincts; otherwise the page shows a message. To try another rule, pass a function to `dashboard_ei(complete = )`: it gets one row per precinct (counted `dem`, `rep`, `total`, `min_stage` over its vote-method rows, and the L2 columns) and returns the geos to use. `ei_complete_stage()` is one: every vote-method row at stage ≥ `EI_MIN_STAGE` (5; `get_data()` downgrades stage 6 to 5 unless `trust_stage`, see `scripts/model/docs/CONTRACT.md` §8). Partly counted precincts make "now" turnout read low, since uncounted ballots look like non-voters.

Estimates are unbounded. One is flagged unreliable when the group's turnout is outside 0–100% or its Dem or Rep vote estimate is negative, typical of small groups. It's drawn hollow at the plot's edge and marked † in EI Diagnostics, which also gives each group's share of registrants, the regressions' R² and the Riesz representer's second moment ν² (large ν²: the race shares carry little information about the group).

On the Results page, the Dem/Rep boxes add a "Proj." line with the latest scraped-geo vote projection and two-party share from `DASHBOARD_MODEL` (`scripts/util/globals.R`, written as `base/spec/interval`, e.g. `ratio_strata/strat_g/conformal`, or the raw model name). The line is blank when that model has no rows for the contest or its latest status is not `ok`.

## Test data (no live election)

Builds test data in `26_dashtest` from the simulated nights, then renders against it without touching the targets store:

- **Live results:** each `26_sim/{st}_base` scenario (truth + schedule, `scripts/sim/sim_snapshot.R`) is replayed at the sim test checkpoints (every 15 minutes 8pm-4am ET, then Nov 4/5/7/10 at noon) through `make_summary()`, as one statewide feed per state.
- **Models:** `live_sim_{st}_base.csv` from `26_general/model_tests/coverage/run_config` (`scripts/model/live/tests/sim_night.R` with the `run = TRUE` models in `model_config.csv`, as on the night, on feed-like returns: no cell is ever flagged complete, see `scripts/model/live/README.md`). `26_general/model_config.csv` is copied alongside. These are converted to enightmodels `predictions_*.csv` / `turnout_*.csv` / `status_*.csv` in `26_dashtest/model_results`, all with `geo = "scraped"`; the turnout interval is `live_sim`'s `lo_turnout` / `hi_turnout`. `CBS_DASH_SIM_DIRS=a,b,...` reads other `coverage/` folders in that order, a model in more than one read from the first (e.g. `main,specs_conformal` for every spec); a `live_sim` file whose truth differs from the current sim (the sims get regenerated) is skipped with a warning. `live_sim` has only the two-party share and turnout, so each party's share is the predicted two-party share times the counted two-party share of the vote at that checkpoint, and predicted votes are share x `est_turnout`. Models with no result at a checkpoint are left out of that run; models with no result at all are listed as failed. `precincts_latest.parquet` is faked from each model's latest run: every precinct's uncounted truth (including sim precincts the feed never reports) projected with noise in turnout and two-party split.
- **CBS counties:** the CBS test feed's latest snapshots (`DASHBOARD_CBS_ENV = "test"`), unrelated to the sim night. TX fills; MI's 10 unscraped counties have all-zero method rows in the test feed and stay grey; OH has no snapshot.
- **Other inputs:** fake history from the final count (with vote methods, except GA), and the sim states' crosswalks and L2 files copied from `26_general/input_data`.

Only `DASHBOARD_CONTESTS` rows that have a `_base` sim are shown (currently AK, FL, GA, MI, MN, NC, OH, TX Senate and GA, TX Governor; IA is left out until it has a zero file). A new `{st}_base` scenario with a `live_sim` file is picked up automatically once its contest is in `DASHBOARD_CONTESTS`.

```bash
CBS_ELECTION_FOLDER=26_dashtest Rscript scripts/util/make_dashboard_test_data.R

CBS_ELECTION_FOLDER=26_dashtest \
CBS_DASH_RDS=data/dashboard/dashtest_data.rds \
quarto render index.qmd --output dashtest.html

open "http://127.0.0.1:8000/dashtest.html"   # server above running
```

The script saves the rds to both `26_dashtest/dashboard_data.rds` in Dropbox and `data/dashboard/dashtest_data.rds` in the repo. After the first run, only the `quarto render` step is needed when editing `index.qmd` or `custom.scss`. It ends by running `scripts/util/make_redist_test_data.R`, which adds the Redistricting tab's data (`redist`) to the repo copy: the 2024 president baseline, counted Senate/governor/House votes (sim precincts plus the CBS test feed's counties, so it needs `CBS_API_KEY`), and the fake model precinct projections regrouped into old and new districts. If that step fails, the previous build's `redist` is kept. To refresh only `redist`: `CBS_ELECTION_FOLDER=26_dashtest Rscript scripts/util/make_redist_test_data.R`.

## Public test site (GitHub Pages)

The dashboard is published at <https://teamstewart.github.io/cbs-enr/>. `.github/workflows/pages.yml` renders `index.qmd` and deploys it on every push to `main` that touches `index.qmd`, `custom.scss`, `assets/`, `data/dashboard/`, or `dashboard_mode.txt`, on each automation dispatch, and once at the default cutover (cron, 2026-11-03 23:01 UTC). To redeploy by hand: `gh workflow run pages.yml`. Check status with `gh run list -w pages.yml`.

### Test or live data: `dashboard_mode.txt`

`dashboard_mode.txt` (repo root, committed on `main`) decides which data both published copies show. `scripts/util/dashboard_mode.R` implements it for the workflow, `automation.R`, and the watcher.

| `mode` | Before `cutover` (default 2026-11-03 18:00 ET) | After `cutover` | `automation.R` uploads `mit.html` / starts Pages builds |
|---|---|---|---|
| `auto` (default) | test data | live data pushed to `dashboard-live` at or after the cutover; else an "awaiting results" page | only after the cutover |
| `test` | test data | test data | never |
| `live` | whatever `dashboard-live` holds | same | every cycle |

Test data renders carry a red TEST DATA badge in the header.

Common actions:

- Switch to live early and drop older rehearsal data: `Rscript scripts/util/dashboard_mode.R cutover-now`, then commit and push `dashboard_mode.txt`. The page shows "awaiting results" until the next automation push.
- One-off look at whatever `dashboard-live` holds: `gh workflow run pages.yml -f data=live`. It stays until the next build without `data=live`. Before the cutover the automation does not start builds, so only a push to `main` replaces it.
- Force test data once: `gh workflow run pages.yml -f data=test`.
- Rehearsals (`CBS_AUTOMATION_TEST=1`) before the cutover still push to `dashboard-live` (the watcher reads it), but neither copy publishes it. To test the S3 upload, call `upload_dashboard_s3()` by hand or set `mode: live` for the rehearsal.

To refresh the test data, rerun `make_dashboard_test_data.R`, then commit and push the new rds.

### CBS copy (S3)

After the `dashboard_mode.txt` cutover, `scripts/automation.R` also renders `index.qmd` locally from the same `data/dashboard/live/dashboard_data.rds` it pushes, then uploads it to `cbsn-elections-external-models/20261103-US-G/mit.html` (`Content-Type: text/html`, `Cache-Control: no-cache`, as in 2024/2025), served behind CloudFront basic auth at <https://dqvla57lr7v2z.cloudfront.net/20261103-US-G/mit.html>. At startup it uploads `data/dashboard/geo/*.json` to `20261103-US-G/data/dashboard/geo/`, where the page's relative geo fetch resolves; restart the automation after rebuilding geo files. The render adds about 20 seconds and the upload about 5 seconds to each cycle.

### Regenerating the model runs

The sims (`scripts/sim/make_sim.R`; FL, MI, OH and TX with `--counties=zero`) and then the `run = TRUE` models:

```bash
CFG="$HOME/Dropbox (MIT)/Research/CBS-MIT Election Data/26_general/model_config.csv"
MODELS=$(awk -F, 'NR>1 && $6=="TRUE" {print $1}' "$CFG" | sort -u | paste -sd, -)
for sc in ak_base fl_base ga_base mi_base mn_base nc_base oh_base tx_base; do
  Rscript scripts/model/live/tests/sim_night.R $sc --models=$MODELS --cores=8 \
    --out="$(dirname "$CFG")/model_tests/coverage/run_config"
done
```

One turnout scenario per state (drawn with `set.seed(20261030)`) checks the turnout fits away from the prior: `{st}_test_turnout_low` (`TURNOUT_RATIO=0.85`: GA, MI, MN, TX), `oh_test_turnout_high` (`TURNOUT_RATIO=1.2`), `{st}_test_turnout_demo` (`TURNOUT_DEMO=his=-0.6,col=0.3`: AK, FL, NC). Their runs are in `coverage/run_config_turnout`; the dashboard shows only the `_base` nights.

### Regenerating the spec runs

`coverage/specs_conformal` holds the 28 `<model>/<spec>/conformal` names in `model_config.csv` for `swing`, `ratio_strata`, `pois_bayes`, and `medsl_sur` that `coverage/main` doesn't already have. To rerun them (about 75 minutes on 6 cores, TX about half of that):

```bash
CFG="$HOME/Dropbox (MIT)/Research/CBS-MIT Election Data/26_general/model_config.csv"
MODELS=$(awk -F, '$4=="conformal" && $2!="lm_bayes" && $1!="swing/conformal" && $1!="ratio_strata/conformal" && $1!="pois_bayes/conformal" {print $1}' "$CFG" | paste -sd, -)
for sc in ak_base ga_base mi_base mn_base nc_base oh_base tx_base; do
  Rscript scripts/model/live/tests/sim_night.R $sc --models=$MODELS --cores=6 \
    --out="$(dirname "$CFG")/model_tests/coverage/specs_conformal"
done
```

`medsl_sur` fails set-up in MN and returns no results in MI, as in `coverage/main`, so it shows as failed there.
