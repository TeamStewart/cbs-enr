# ========================================
## MAIN TARGETS PIPELINE
## AUTHORS: Mason Reece and Joseph R. Loffredo
## FIRST CREATED: Februrary 2024
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
  gargle_oauth_email = TRUE
)

tar_option_set(
  packages = c(
    # tar_renv()
    "data.table", "tidyverse", "glue", "janitor", "fs", "aws.s3", 
    "googledrive", "httr2", "rvest", "reticulate", "sf", "xml2"
  ),
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 1)
)

tar_config_set(
  seconds_meta_append = 15,
  seconds_reporter = 0.5
)

# get the metadata file, with the following structure
# - state: (string) caps abbreviation of the state
# - county: (string) lowercase county of interest in that state, if NA then statewide
# - path: (string) generic cell for path/number/ID used by custom scrapers to get file
# - model_swing: (boolean) whether to model swing for this county/state
# - model_turnout: (boolean) whether to model turnout for this county/state
# - upload: (boolean) whether to upload this jurisdiction to CBS AWS
metadata = get_gsheet(sheet = "metadata")

# ========================================
## PIPELINE
# ========================================
list(
  tar_map(
    metadata,
    tar_target(timestamp, get_timestamp(state, county, path), cue = tar_cue(mode = "always")),
    tar_target(data, get_data(state, county, timestamp, path), error = "continue"),
    tar_target(tbl_gen, create_table_generic(data, state, county, type, timestamp)),
    tar_target(tbl_cbs, create_table_cbs(data, state, county, type, timestamp)),
    tar_target(models, run_models(data, state, county, model_swing, model_turnout)),
    names = c(state, county)
  )
)
