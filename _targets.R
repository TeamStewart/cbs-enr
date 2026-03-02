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

pak::local_install(PATH_ENIGHTDATA, upgrade = FALSE)

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
    path_timestamp = dplyr::na_if(path_timestamp, ""),
    path_timestamp = dplyr::coalesce(path_timestamp, path)
  ) |> 
  dplyr::filter(updated)

create_dirs(metadata)

office_filter = tibble::tribble(
  ~state, ~jurisdiction, ~contest, ~district,
  "TX", "tx", "sen_dem", NA,
  "TX", "tx", "sen_rep", NA,
  "TX", "tx", "gov_dem", NA,
  "TX", "tx", "gov_rep", NA,
  "TX", "tx", "ag_dem", NA,
  "TX", "tx", "ag_rep", NA,
  "TX", "tx", "house_dem", 30,
  "TX", "tx", "house_rep", 30,
  "TX", "tx", "house_dem", 33,
  "TX", "tx", "house_rep", 33,
)

# ========================================
## PIPELINE
# ========================================
list(
  # first, parse jurisdictions with multiple URLs (e.g., for Dem/Rep primary)
  tar_map(
    dplyr::filter(metadata, dplyr::n() > 1, .by = c(state, county)) |>
      dplyr::summarise(
        paths = list(path),
        path_timestamps = list(path_timestamp),
        .by = c(state, county, upload)
      ),
    tar_target(timestamp, purrr::map2(path_timestamps, paths, \(pt, p) get_timestamp(state, county, pt)), cue = tar_cue(mode = "always")),
    tar_target(data, purrr::map2(paths, timestamp, \(p, ts) get_data(state, county, ts, p)) |> dplyr::bind_rows()),
    tar_target(tbl_cbs, create_table_cbs(data, state, county, timestamp[[1]], upload, do_cbs = FALSE, office_filter = office_filter)),
    names = c(state, county)
  ),
  # parse remaining, normal jurisdictions
  tar_map(
    dplyr::filter(metadata, dplyr::n() == 1, .by = c(state, county)),
    tar_target(timestamp, get_timestamp(state, county, path_timestamp), cue = tar_cue(mode = "always")),
    # tar_target(history, get_history(state, impute = FALSE)),
    tar_target(data, get_data(state, county, timestamp, path)),
    # tar_target(summary, make_summary(data, state, county, timestamp, history)),
    tar_target(tbl_cbs, create_table_cbs(data, state, county, timestamp, upload, do_cbs = FALSE, office_filter = office_filter)),
    names = c(state, county)
  )
  # tar_quarto(dashboard, "index.qmd", quiet = FALSE),
  # tar_target(upload_h, upload_html(dashboard[1]))
)
