# CBS Election Night Reporting

A place for storing our resources for CBS Election Night Reporting software.

Maintained by: Joe Loffredo, Mason Reece, and Charles Stewart III

## Setup

- `install.packages("pak")`; then `pak::pak("CoryMcCartan/enightmodels")` and `pak::pak("TeamStewart/enightdata")`
- Clone this repo, open it in Positron or RStudio, then `install.packages("renv")` and `renv::install()` to get all the packages needed. Double-check you are on the correct R version

## Repository Overview

The repository contains several distinct sections. Although the structure of the repository blends them all together, we internally treat them as separate sections. Additionally, this repository and the code within relies on an external folder, set by the global variable `DROPBOX_PATH` for storing data -- the size of which can be quite substantial depending on the scale and detailed required for the election in question (e.g., the 2020 election in \~5 states was 11GB). Below we overview each section in turn.

### Pipeline Management

This repository uses a makefile style pipeline, controlled by the R package `targets`. In short, this helps us by reducing the number of full updates of data/estimates/figures/pages we produce to only occur when a new batch of votes is released. 

- `_targets.R` is the main script file, which sets up the structure of the pipeline and imports all relevant functions
  
- `/_targets` and `_targets.yaml` are generated automatically, and should never be modified

### Scripts

The `/scripts` folder contains all scripts used for the project. They tend to fall into three categories: pre-election, election night, and post-election. All files referenced below are relative to the `/scripts` folder.

#### Pre-Election

Before the election, it is our goal to both identify how data *will* be released by election offices and gather historical data that can be merged *live* with incoming election data. To assist with this purpose we have some scripts, which are the following

-   `/create_xwalks` for creating crosswalks matching precincts in the previous election to the upcoming election

-   `/util/create_precinct_merge.R` builds history files for use in modeling, the primary task of which is to create historical estimates for current election precincts using old precincts. Sometimes the borders of precincts change, so the script uses shapefiles and areal interpolation to project new estimates.

    -   For help downloading shapefiles, `/util/download_shapefiles.R` and `/util/decode_polyline.py` are used to scrape shapefiles from Clarity sites, when shapefiles are not available from the state/county.

-   `/util/fl_county_links.R` scrapes the election night website links for all Florida counties, since they are inconsistent per year.

-   `/voteshift/count_history.R` generates figures of the historical counting pace, by county, in each state we have data from. /`voteshift/cbs_vote_counts.qmd` generates tables with similar data.

#### Election Night

On election night, run the `automation.R` script once and it will continuously loop on a predetermined interval (set in the script), running the pipeline every time the script starts. The pipeline mainly relies on `functions.R`, which in turns imports `scrapers.R`, `plotters.R`, `models.R`, `/util/util.R` and `util/globals.R`. The most likely things to change for each election is `globals.R` and `scrapers.R`.

#### Post-Election

Sometimes we create ad-hoc scripts to do some post-election analysis. `analysis_reporting_pace.R` is one example.

### Website Deployment

One of the functions of the pipeline is to build a simple website consisting of several HTML files. These HTML files are useful for our internal team to check on the results, but they are also uploaded automatically to the CBS election data tool, which allows the Data Desk team to see the same website.

-   The Quarto pages for each webpage are found in `/pages` and `index.qmd` - `/docs`, `/index_files`, `index.html` are generated automatically and should never be modified

#### Dashboard (`index.qmd`)

The dashboard is rendered by the `dashboard` target on every `tar_make()`. A dropdown next to the title switches between State x Contest selections; every selection gets the same value boxes (last update, precincts reported, total/Election Day votes, leading Dem and Rep candidates) and the same charts (vote totals/counts/shares over time, precinct margin and swing histograms, a swing scatter, and a precinct explorer). All charts are drawn in the browser (Observable Plot), so one HTML file covers every selection. `?c=GA-sen` in the URL opens a specific selection.

-   **Adding or removing a selection:** edit `DASHBOARD_CONTESTS` in `scripts/util/globals.R` (`state`, `contest` code as it appears in the clean data, `district` or `NA`, optional `label`, optional `hist_contest`) and rerun `tar_make()`. Counties scraped separately for the same state are pooled.
-   **Pipeline pieces:** each jurisdiction's `summary_*` target (`make_summary()`) writes a small per-run file to `{ELECTION_FOLDER}/{STATE}/summaries/` (these build the time series) and returns its precinct table; `dashboard_data` (`build_dashboard_data()`) combines them for the configured selections.
-   **History (optional):** `{ELECTION_FOLDER}/history/{STATE}_history.parquet`, long format with columns `geo`, `contest`, `party`, `year`, `votes`, and optionally `method`, `district`, `county`. `geo` must match the live scraper's `geo` ids. When present, the dashboard adds "% of 'YY" to the vote boxes, the swing histogram, and the swing scatter; otherwise those show a "no history" note. The latest `year` for the matching contest is used.
-   **L2 (optional):** `{ELECTION_FOLDER}/input_data/crosswalks/{st}.csv` plus the latest `input_data/l2/{st}_YYYYMMDD_precincts.parquet` add `l2_*` shares (registered `n`, turnout 20/22/24, male, white/black/hispanic/asian) to the precinct explorer's X/Y options. Turnout follows the explorer's Vote Mode (`vote24_mail / n` for Mail; ENR `adv` uses early, `abs` uses mail, `prov` has none). Picking an `l2_*` variable shows the share of the contest's ENR precincts matched through the crosswalk; unmatched precincts are dropped.
-   **Testing without a live election:** `CBS_ELECTION_FOLDER=26_dashtest Rscript scripts/util/make_dashboard_test_data.R` builds fake Dem-vs-Rep data from the `26_primaries` snapshots, then `CBS_ELECTION_FOLDER=26_dashtest CBS_DASH_RDS="<PATH_DROPBOX>/26_dashtest/dashboard_data.rds" quarto render index.qmd --output dashtest.html`.

### Version control and package management:

We use the R package `renv` to maintain a consistent package and version state for all users. It's two basic uses are to `renv::restore()` to set-up a new user and `renv::snapshot()` to update the list of packages in use. Additionally, because of the use of the R `targets` package, users need to also run `targets::tar_renv()` when before snapshotting to correctly identify all the packages.

-   `/renv`, `renv.lock`, `requirements.txt` and `_targets_packages.R` are generated automatically and should never be modified

### Miscellaneous

-   `/logs` are generated automatically by the pipeline to log what it does (or skips) each run
  
-   `cbs-enr.Rproj` is an R project file for organizing our work
