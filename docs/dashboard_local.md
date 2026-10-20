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

Builds fake Dem-vs-Rep data from the `26_primaries` snapshots into `26_dashtest`, then renders against it without touching the targets store. It copies the dashboard states' crosswalks and L2 files from `26_general/input_data` for the explorer's `l2_*` variables. States with `26_general` files for the dashboard contest (currently NC) have their primary votes moved onto the general precinct geos; the rest keep primary geos:

```bash
CBS_ELECTION_FOLDER=26_dashtest Rscript scripts/util/make_dashboard_test_data.R

CBS_ELECTION_FOLDER=26_dashtest \
CBS_DASH_RDS="$HOME/Dropbox (MIT)/Research/CBS-MIT Election Data/26_dashtest/dashboard_data.rds" \
quarto render index.qmd --output dashtest.html

open dashtest.html
```

Replace the `CBS_DASH_RDS` path with your own `PATH_DROPBOX` from `globals.R`. After the first run, only the `quarto render` step is needed when editing `index.qmd` or `custom.scss`.

## Public test site (GitHub Pages)

The test dashboard is published at <https://teamstewart.github.io/cbs-enr/> from the `gh-pages` branch, which holds only `index.html` and `.nojekyll`. To redeploy after rendering `dashtest.html`:

```bash
D=$(mktemp -d) && cp dashtest.html "$D/index.html" && touch "$D/.nojekyll"
cd "$D" && git init -q -b gh-pages && git add . && git commit -qm "Deploy dashboard test data"
git push -f git@github.com:TeamStewart/cbs-enr.git gh-pages
```

Pages rebuilds in about a minute (`gh api repos/TeamStewart/cbs-enr/pages/builds/latest --jq .status`).
