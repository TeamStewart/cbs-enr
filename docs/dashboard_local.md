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
- **Shift Map:** for each unit with counted votes, the shift in two-party margin from 2024 president to the contest's counted votes. 2024 is VoteHub's presidential estimate on the 2026 precincts (`26_general/input_data/votehub-shapefiles/estimated_past_results.csv`, `read_pres24()` in `scripts/functions.R`; no CA rows), joined on `unit_id` as `dem24` / `rep24` in `mres`. Only counted votes are used, no projections or CBS county fills, so a partly counted unit is compared with its full 2024 vote. Arrows start at the precinct's point on surface and point up-left (35° above horizontal) in the Dem color for a Dem shift, up-right in the Rep color for a Rep shift; length is proportional to the shift and the arrowhead is the same size on every arrow, capped at 30 points. The caption gives the shift over all units shown together. Arrows keep their length when zoomed.
- **Remaining Vote Map:** from the picked model's latest `model_results/precincts_latest.parquet` (`write_precincts()` in `scripts/model/live/R/output.R`): projected final votes minus counted votes per unit, a circle at the precinct's point on surface sized by expected remaining votes and filled by their projected margin. It always shows the latest run, not "As of".

Both maps zoom (wheel, pinch, double-click) and pan (drag); Reset at the top right undoes it. Hovering a precinct shows its numbers (browser tooltip, which follows the zoom; Plot's pointer tips don't).

Every map labels the state capital (star, bold) and its major cities, drawn over everything else, arrows and circles included. The list is `CITIES` in `index.qmd` (name, lon, lat, capital, and `"end"` to put the label left of the point); add a state there when it gets a map. Labels move with zoom and pan but keep their size, and use the page font live and Styrene B in exports.

Each render embeds only the per-unit numbers (`mres`, `mprec` in `build_dashboard_data()`).

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

The script saves the rds to both `26_dashtest/dashboard_data.rds` in Dropbox and `data/dashboard/dashtest_data.rds` in the repo. After the first run, only the `quarto render` step is needed when editing `index.qmd` or `custom.scss`.

## Public test site (GitHub Pages)

The test dashboard is published at <https://teamstewart.github.io/cbs-enr/>. `.github/workflows/pages.yml` renders `index.qmd` against the committed `data/dashboard/dashtest_data.rds` and deploys it on every push to `main` that touches `index.qmd`, `custom.scss`, `cbs_white.svg`, or `data/dashboard/`. To refresh the data, rerun `make_dashboard_test_data.R`, then commit and push the new rds. To redeploy by hand: `gh workflow run pages.yml`. Check status with `gh run list -w pages.yml`.

### CBS copy (S3)

On election night `scripts/automation.R` also renders `index.qmd` locally from the same `data/dashboard/live/dashboard_data.rds` it pushes, then uploads it to `cbsn-elections-external-models/20261103-US-G/mit.html` (`Content-Type: text/html`, `Cache-Control: no-cache`, as in 2024/2025), served behind CloudFront basic auth at <https://dqvla57lr7v2z.cloudfront.net/20261103-US-G/mit.html>. At startup it uploads `data/dashboard/geo/*.json` to `20261103-US-G/data/dashboard/geo/`, where the page's relative geo fetch resolves; restart the automation after rebuilding geo files. The render adds about 20 seconds and the upload about 5 seconds to each cycle.

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
