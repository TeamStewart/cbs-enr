# ========================================
## MAIN TARGETS PIPELINE
## AUTHORS: Mason Reece and Joseph R. Loffredo
## FIRST CREATED: February 2024
# ========================================
## SETUP
# ========================================
rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(glue)
})

source("scripts/functions.R")

options(
  readr.show_col_types = FALSE,
  gargle_oauth_email = TRUE,
  scipen = 999
)

tar_option_set(
  packages = c(
    # targets::tar_renv() 
    "data.table",
    "tidyverse",
    "glue",
    "fs",
    "reticulate",
    "gt",
    "patchwork",
    "enightmodels", # cory package
    "enightdata" # internal package for scrapers and timestamps
  ),
  error = "continue",
  controller = crew::crew_controller_local(workers = 3)
)

enightdata::set_all(
  dropbox = PATH_DROPBOX,
  gdrive = PATH_GDRIVE,
  election_folder = ELECTION_FOLDER,
  s3_bucket = PATH_CBS_S3,
  election_date = ELECTION_DATE
)

# get the metadata file, with the following structure
# - state: (string) all caps abbreviation of the state
# - county: (string) lowercase county of interest in that state, if NA then statewide
# - path: (string) generic cell for url path used to scrape data
# - path_timestamp: (string) optional cell for timestamp, if NA then use `path`
# - upload: (boolean) whether to upload this jurisdiction to CBS AWS
metadata = enightdata::read_local("metadata.csv", colClasses=list(character=1:4, logical=5)) |> 
  dplyr::mutate(
    path_timestamp = dplyr::coalesce(path_timestamp, path)
  ) |> 
  dplyr::filter(updated)
create_dirs(metadata)

# ========================================
## PIPELINE
# ========================================
data_map <- tar_map(
    metadata,
    unlist = FALSE,
    tar_target(timestamp, get_timestamp(state, county, path), cue = tar_cue(mode = "always")),
    # tar_target(history, get_history(state, impute = FALSE)),
    tar_target(datac, get_data(state, county, timestamp, path)),
    # tar_target(summary, make_summary(data, state, county, timestamp, history)),
    # tar_target(tbl_cbs, create_table_cbs(data, state, county, timestamp, upload)),
    names = c(state, county)
  )

list(
  data_map,
  tar_combine(
    data,
    data_map[["datac"]],
    command = vctrs::vec_rbind(!!!.x)
  ),
  tar_target(upload_s, upload_state(data, state = "TX"))
  # tar_quarto(dashboard, "index.qmd", quiet = FALSE),
  # tar_target(upload_h, upload_html(dashboard[1]))
)
