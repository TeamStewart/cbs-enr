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

The Models page plots enightmodels output over time for the selected contest. `read_model_results()` (`scripts/functions.R`) stacks every `{ELECTION_FOLDER}/model_results/predictions_*.csv` (schema in the enightmodels README, plus `geo` = `scraped`/`all` and `race_id`) into a time series keyed on `time`, and takes each race/model's latest row from `status_*.csv`. The `model_results` target re-reads the folder on every `tar_make()`, so a new model run shows up on the next dashboard render.

Rows join to `DASHBOARD_CONTESTS` on state + contest + district (NA district = statewide). Contests with no rows show a "not modeled" message; models that appear only in the status files (failed runs) are listed but can't be picked.

Model names are `<model>[/<spec>][/<interval>]`, with the model's default spec and interval left off (`scripts/model/live/R/config.R`). `model_parts()` (`scripts/functions.R`) looks each name up in `{ELECTION_FOLDER}/model_config.csv` to get `model_base`, `model_spec`, and `model_interval`, so `swing/conformal` reads as swing · ratio · conformal. Names missing from the config are split on `/`, with `default` for missing parts. The picker has three selects: Model, Demographics (the specs that model has results for), and Interval (the intervals for that model + spec). Changing one keeps the other choices when they still exist. The dropdowns show readable labels from `SPEC_LABELS` and `INTERVAL_LABELS` in `index.qmd` (label + description per code; unlisted codes show as-is), mark each model's default spec and interval "(default)" (`spec_default` / `interval_default` from `model_parts()`), and the caption under the controls gives the full description and the model name. Add a row there when a new spec or interval is added to `demos.R` / `results.R`. The "Statewide projection" checkbox appears only when the picked model has `geo = "all"` rows, and overlays them dashed.

On the Results page, the Dem/Rep boxes add a "Proj." line with the latest scraped-geo vote projection and two-party share from `DASHBOARD_MODEL` (`scripts/util/globals.R`, written as `base/spec/interval`, e.g. `ratio_strata/strat_g/conformal`, or the raw model name). The line is blank when that model has no rows for the contest or its latest status is not `ok`.

## Test data (no live election)

Builds test data in `26_dashtest` from the simulated nights, then renders against it without touching the targets store:

- **Live results:** each `26_sim/{st}_base` scenario (truth + schedule, `scripts/sim/sim_snapshot.R`) is replayed at the sim test checkpoints (every 15 minutes 8pm-4am ET, then Nov 4/5/7/10 at noon) through `make_summary()`, as one statewide feed per state.
- **Models:** `live_sim_{st}_base.csv` from `26_general/model_tests/coverage/main` (each model's default fits) and `coverage/specs_conformal` (every other demographic spec with the conformal interval), both from `scripts/model/live/tests/sim_night.R` and stacked, with `main` winning for a model in both. `26_general/model_config.csv` is copied alongside. These are converted to enightmodels `predictions_*.csv` / `status_*.csv` in `26_dashtest/model_results`, all with `geo = "scraped"`. `live_sim` has only the two-party share and turnout, so each party's share is the predicted two-party share times the counted two-party share of the vote at that checkpoint, and predicted votes are share x `est_turnout`. Models with no result at a checkpoint are left out of that run; models with no result at all are listed as failed.
- **Other inputs:** fake history from the final count (with vote methods, except GA), and the sim states' crosswalks and L2 files copied from `26_general/input_data`.

Only `DASHBOARD_CONTESTS` rows that have a `_base` sim are shown (currently AK, GA, MI, MN, NC, OH Senate and TX Senate + Governor). A new `{st}_base` scenario with a `live_sim` file is picked up automatically once its contest is in `DASHBOARD_CONTESTS`.

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
