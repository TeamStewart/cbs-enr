# ========================================
## MAIN TARGETS PIPELINE
## AUTHORS: Mason Reece and Joseph R. Loffredo
## FIRST CREATED: February 2024
# ========================================
## SETUP
# ========================================
rm(list=ls())
gc()

suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(tidyverse)
})

source("scripts/functions.R")

options(
  readr.show_col_types = FALSE,
  gargle_oauth_email = TRUE,
  scipen = 999
)

tar_option_set(
  packages = c(
    # tar_renv()
    "data.table", "tidyverse", "glue", "janitor", "fs", "aws.s3", "gt",
    "googledrive", "httr2", "rvest", "reticulate", "sf", "xml2", "jsonlite"
  ),
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 4)
)

tar_config_set(
  seconds_meta_append = 15,
  seconds_reporter = 0.5
)

# get the metadata file, with the following structure
# - state: (string) caps abbreviation of the state
# - county: (string) lowercase county of interest in that state, if NA then statewide
# - path: (string) generic cell for path/number/ID used by custom scrapers to get file
# - preelection_totals: (boolean) 
# - upload: (boolean) whether to upload this jurisdiction to CBS AWS
metadata = get_gsheet(sheet = "metadata", col_types = "cccll") |> drop_na(path)

# ========================================
## PIPELINE
# ====================Z====================
list(
  tar_map(
    metadata,
    tar_target(timestamp, get_timestamp(state, county, path), cue = tar_cue(mode = "always")),
    tar_target(data, get_data(state, county, timestamp, path), error = "continue"),
    # tar_target(tbl_cbs, create_table_cbs(data, state, county, timestamp, upload)),
    tar_target(model, run_models(data, state, county, timestamp, preelection_totals)),
    tar_target(plot_voteShare, make_plot_voteShare(model, state, county), error = "continue"),
    tar_target(plot_margin2020, make_plot_margin2020(model, state, county), error = "continue"),
    tar_target(plot_votesEDay, make_plot_votesEDay(model, state, county), error = "continue"),
    tar_target(plot_votesAll, make_plot_votesAll(model, state, county), error = "continue"),
    tar_target(tbl_countyMode, make_tbl_countyMode(model, state, county), error = "continue"),
    tar_target(tbl_county, make_tbl_county(model, state, county)),
    names = c(state, county)
  ),
  tar_quarto(website, cache = FALSE, quiet = FALSE),
  tar_target(uploads, upload_html(website))
)
