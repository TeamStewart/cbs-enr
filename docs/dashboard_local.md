# Running the dashboard locally

The dashboard is `index.qmd`, rendered to a self-contained `index.html` (`embed-resources: true`). No server is needed; open the HTML file in a browser.

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
open index.html                              # ?c=GA-sen in the URL opens a specific selection
```

Use the `CBS_ELECTION_FOLDER` env var to point at a folder other than the default `26_general`. Selections shown are set by `DASHBOARD_CONTESTS` in `scripts/util/globals.R`.

## Test data (no live election)

Builds fake Dem-vs-Rep data from the `26_primaries` snapshots into `26_dashtest`, then renders against it without touching the targets store. It copies the dashboard states' crosswalks and L2 files from `26_general/input_data` for the explorer's `l2_*` variables. States with `26_general` files for the dashboard contest (currently GA, NC, NE, PA) have their primary votes moved onto the general precinct geos; the rest keep primary geos:

```bash
CBS_ELECTION_FOLDER=26_dashtest Rscript scripts/util/make_dashboard_test_data.R

CBS_ELECTION_FOLDER=26_dashtest \
CBS_DASH_RDS=data/dashboard/dashtest_data.rds \
quarto render index.qmd --output dashtest.html

open dashtest.html
```

The script saves the rds to both `26_dashtest/dashboard_data.rds` in Dropbox and `data/dashboard/dashtest_data.rds` in the repo. After the first run, only the `quarto render` step is needed when editing `index.qmd` or `custom.scss`.

## Public test site (GitHub Pages)

The test dashboard is published at <https://teamstewart.github.io/cbs-enr/>. `.github/workflows/pages.yml` renders `index.qmd` against the committed `data/dashboard/dashtest_data.rds` and deploys it on every push to `main` that touches `index.qmd`, `custom.scss`, `cbs_white.svg`, or `data/dashboard/`. To refresh the data, rerun `make_dashboard_test_data.R`, then commit and push the new rds. To redeploy by hand: `gh workflow run pages.yml`. Check status with `gh run list -w pages.yml`.
