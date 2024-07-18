# ========================================
## MAIN TARGETS PIPELINE
## AUTHORS: Mason Reece and Joseph R. Loffredo
## FIRST CREATED: Februrary 2024
# ========================================
## SETUP
# ========================================
rm(list=ls())
gc()

library(targets)
library(tarchetypes)
suppressMessages(library(tidyverse))

source("scripts/functions.R")

options(
  timeout = max(300, getOption("timeout")),
  readr.show_col_types = FALSE
)

tar_option_set(
  packages = c(
    "data.table", "tidyverse", "gt", "xml2", "aws.s3", "jsonlite", "fixest", "googledrive",
    "marginaleffects", "rlang", "reticulate", "rvest", "httr2", "glue", "fs", "polite","janitor"
  ),
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  error = "null",
  controller = crew::crew_controller_local(workers = 3)
)

tar_config_set(
  seconds_meta_append = 15,
  seconds_reporter = 0.5
)

# generate the lookup table with important information for each state
metadata = read_csv("metadata.csv")

# ========================================
## PIPELINE
# ========================================
list(
  tar_map(
    metadata,
    tar_target(time, get_timestamp(state, county, type, path), cue = tar_cue(mode = "always")),
    tar_target(clean, process_data(state, county, type, time, path = path), error = "continue"),
    tar_target(tbl_all, general_table(clean, state, county, type, time)),
    tar_target(cbs, convert_cbs(clean, state, county, type, time, election_date, cbs_lookup, cbs_s3_path, google_drive_folder, upload = T)),
    # tar_target(models, run_models(clean, state, time)),
    names = c(state, county, type)
  )
)
