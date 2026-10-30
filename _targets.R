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

# destinations for metadata rows with upload = TRUE; any of "s3_parquet",
# "s3_csv", "drive_parquet", "drive_csv"
upload_default = c("s3_parquet", "drive_csv")

# get the metadata file, with the following structure
# - state: (string) all caps abbreviation of the state
# - county: (string) lowercase county of interest in that state, if NA then statewide
# - path: (string) generic cell for url path used to scrape data
# - path_timestamp: (string) optional cell for timestamp, if NA then use `path`
# - upload: (string) where to upload this jurisdiction, comma-separated, any of
#   "s3_parquet", "s3_csv", "drive_parquet", "drive_csv"; blank or FALSE uploads
#   nothing, TRUE uses `upload_default` below
# - trust_stage: (boolean) whether we trust this county's stage variable
metadata = enightdata::read_local("metadata.csv", colClasses=list(character=c(1:4, 6), logical=c(5, 7, 8))) |> 
  dplyr::mutate(
    path_timestamp = dplyr::na_if(path_timestamp, ""),
    path_timestamp = dplyr::coalesce(path_timestamp, path),
    county = dplyr::na_if(county, ""),
    upload = trimws(dplyr::coalesce(upload, "")),
    upload = dplyr::case_when(
      toupper(upload) == "TRUE" ~ paste(upload_default, collapse = ","),
      toupper(upload) == "FALSE" ~ "",
      .default = upload
    )
  ) |> 
  dplyr::filter(build, updated)

create_dirs(metadata)

# Filter for what to upload to S3
# In 2026, VoteHub only interested in MI and OH
office_filter = tibble::tribble(
  ~state, ~jurisdiction, ~contest, ~district,
  "MI", "mi", "sen", NA,
  "OH", "oh", "sen", NA,
)

office_filter = NULL

# ========================================
## PIPELINE
# ========================================
# all states/counties
map_single = tar_map(
  metadata,
  tar_target(timestamp, get_timestamp(state, county, path_timestamp), cue = tar_cue(mode = "always")),
  tar_target(data, get_data(state, county, timestamp, path, trust_stage)),
  tar_target(tbl_cbs, create_table_cbs(data, state, county, timestamp, upload, do_cbs = FALSE, office_filter = office_filter)),
  tar_target(summary, make_summary(data, state, county)),
  names = c(state, county)
)

list(
  map_single,
  # dashboard: contests shown are set by DASHBOARD_CONTESTS in scripts/util/globals.R
  # election-night models, after the scrapes: the run = TRUE rows of model_config.csv, on the
  # real returns unless MODEL_SIM (scripts/util/globals.R) names a sim scenario
  tar_target(model_config, path.expand(fs::path(PATH_DROPBOX, ELECTION_FOLDER, "model_config.csv")), format = "file"),
  tar_target(model_status, run_models(model_config, MODEL_SIM, MODEL_SIM_AT, after = dash_inputs), cue = tar_cue(mode = "always")),
  tar_combine(dash_inputs, map_single[["summary"]], command = list(!!!.x)),
  # enightmodels output, re-read every run ({ELECTION_FOLDER}/model_results)
  tar_target(model_results, read_model_results(), cue = tar_cue(mode = "always")),
  tar_target(dashboard_data, build_dashboard_data(dash_inputs, DASHBOARD_CONTESTS, model_results)),
  tar_quarto(dashboard, "index.qmd", quiet = FALSE)
)
