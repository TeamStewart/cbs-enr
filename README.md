# CBS Election Night Reporting

A place for storing our resources for CBS Election Night Reporting software.

Maintained by: Joe Loffredo, Mason Reece, and Charles Stewart III

## Repository Overview

The repository contains several distinct sections. Although the structure of the repository blends them all together, we internally treat them as separate sections. Additionally, this repository and the code within relies on an external folder, set by the global variable `DROPBOX_PATH` for storing data -- the size of which can be quite substantial depending on the scale and detailed required for the election in question (e.g., the 2020 election in \~5 states was 11GB). Below we overview each section in turn.

### Pipeline Management

This repository uses a makefile style pipeline, controlled by the R package `targets`. In short, this helps us by reducing the number of full updates of data/estimates/figures/pages we produce to only occur when a new batch of votes is released. - `_targets.R` is the main script file, which sets up the structure of the pipeline and imports all relevant functions - `/_targets` and `_targets.yaml` are generated automatically, and should never be modified

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

### Version control and package management:

We use the R package `renv` to maintain a consistent package and version state for all users. It's two basic uses are to `renv::restore()` to set-up a new user and `renv::snapshot()` to update the list of packages in use. Additionally, because of the use of the R `targets` package, users need to also run `targets::tar_renv()` when before snapshotting to correctly identify all the packages.

-   `/renv`, `renv.lock`, `requirements.txt` and `_targets_packages.R` are generated automatically and should never be modified

### Miscellaneous

-   `/logs` are generated automatically by the pipeline to log what it does (or skips) each run
-   `cbs-enr.Rproj` is an R project file for organizing our work
