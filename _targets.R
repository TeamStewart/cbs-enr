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

devtools::document(PATH_ENIGHTDATA)
# renv (not pak) so dependency versions stay at renv.lock; enightdata itself
# is in renv's ignored.packages since each machine installs its own checkout
renv::install(PATH_ENIGHTDATA, prompt = FALSE)

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
  imports = c("enightdata"),
  error = "null",
  controller = crew::crew_controller_local(workers = 12)
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
# - trust_stage: (boolean) whether we trust this county's stage variable
metadata = enightdata::read_local("metadata.csv", colClasses=list(character=1:4, logical=5:8)) |> 
  dplyr::mutate(
    path_timestamp = dplyr::na_if(path_timestamp, ""),
    path_timestamp = dplyr::coalesce(path_timestamp, path),
    county = dplyr::na_if(county, "")
  ) |> 
  dplyr::filter(build, updated)

create_dirs(metadata)

# office_filter = tibble::tribble(
#   ~state, ~jurisdiction, ~contest, ~district,
#   "TX", "tx", "sen_dem", NA,
#   "TX", "tx", "sen_rep", NA,
#   "TX", "tx", "gov_dem", NA,
#   "TX", "tx", "gov_rep", NA,
#   "TX", "tx", "ag_dem", NA,
#   "TX", "tx", "ag_rep", NA,
#   "TX", "tx", "house_dem", 30,
#   "TX", "tx", "house_rep", 30,
#   "TX", "tx", "house_dem", 33,
#   "TX", "tx", "house_rep", 33,
# )

office_filter = NULL

# ========================================
## PIPELINE
# ========================================
list(
  # all states/counties
  tar_map(
    metadata,
    tar_target(timestamp, get_timestamp(state, county, path_timestamp), cue = tar_cue(mode = "always")),
    tar_target(data, get_data(state, county, timestamp, path, trust_stage)),
    tar_target(tbl_cbs, create_table_cbs(data, state, county, timestamp, upload, do_cbs = FALSE, office_filter = office_filter)),
    tar_target(summary, make_summary(data, state, county)),
    names = c(state, county)
  ),
  # dashboard: contests shown are set by DASHBOARD_CONTESTS in scripts/util/globals.R
  tar_combine(dash_inputs, map_single[["summary"]], command = list(!!!.x)),
  tar_target(dashboard_data, build_dashboard_data(dash_inputs, DASHBOARD_CONTESTS)),
  tar_quarto(dashboard, "index.qmd", quiet = FALSE)
)
