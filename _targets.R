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
    "data.table", "tidyverse", "glue", "janitor", "fs", "aws.s3", "marginaleffects",
    "googledrive", "rvest", "reticulate", "xml2", "jsonlite", "httr2",
    "devtools", "pak"
  ),
  error = "continue",
  controller = crew::crew_controller_local(workers = 2)
)

# get the metadata file, with the following structure
# - state: (string) all caps abbreviation of the state
# - county: (string) lowercase county of interest in that state, if NA then statewide
# - path: (string) generic cell for path/number/ID used by custom scrapers to get file
# - upload: (boolean) whether to upload this jurisdiction to CBS AWS
metadata = readr::read_csv(glue::glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/metadata.csv"), col_types = "cccll")

# ========================================
## PIPELINE
# ====================Z====================
list(
  tar_map(
    metadata,
    tar_target(timestamp, get_timestamp(state, county, path), cue = tar_cue(mode = "always")),
    tar_target(history, get_history(state)),
    tar_target(data, get_data(state, county, timestamp, path)),
    tar_target(tbl_cbs, create_table_cbs(data, state, county, timestamp, upload)),
    tar_target(model, run_models(data, state, county, timestamp, history)),
    names = c(state, county)
  )
)
