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
# election-night models (scripts/model/live/; sets single-threaded BLAS before the crew
# workers start, which inherit it) and their targets wrappers
source("scripts/model/live/load.R")
source("scripts/model_targets.R")

# renv (not pak) so dependency versions stay at renv.lock; enightdata itself
# is in renv's ignored.packages since each machine installs its own checkout.
# Only when the checkout changed (commit or uncommitted edits) since this machine last
# installed it: document + install take ~20s, a big slice of a 5-minute cycle.
local({
  stamp = file.path(tools::R_user_dir("cbs-enr", "cache"), "enightdata_installed")
  git = \(...) suppressWarnings(system2("git", c("-C", shQuote(path.expand(PATH_ENIGHTDATA)), ...), stdout = TRUE, stderr = TRUE))
  state = rlang::hash(c(git("rev-parse", "HEAD"), git("status", "--porcelain"), git("diff", "HEAD")))
  if (!requireNamespace("enightdata", quietly = TRUE) || !file.exists(stamp) || readLines(stamp, warn = FALSE)[1] != state) {
    devtools::document(PATH_ENIGHTDATA)
    renv::install(PATH_ENIGHTDATA, prompt = FALSE)
    dir.create(dirname(stamp), recursive = TRUE, showWarnings = FALSE)
    writeLines(state, stamp)
  }
})

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

# election-night models: per state in priors.csv (or the MODEL_SIM state), each waiting
# only on that state's scrapes; within a state, one branch per race x model
# (scripts/model_targets.R). `feed` is the state's scrape summaries, for ordering; a sim
# run doesn't wait on the scrapes.
summary_names = if (nzchar(MODEL_SIM)) character() else vapply(map_single[["summary"]], \(t) t$settings$name, "")
model_values = tibble::tibble(ST = model_states(MODEL_SIM)) |>
  dplyr::mutate(feed = purrr::map(ST, \(s) as.call(c(as.symbol("list"), rlang::syms(grep(paste0("^summary_", s, "_"), summary_names, value = TRUE))))))

map_models = tar_map(
  model_values,
  names = "ST",
  tar_target(model_inputs, model_state_inputs(ST), format = "qs", cue = tar_cue(mode = "always")),
  tar_target(model_cbs, model_state_cbs(ST, MODEL_SIM, MODEL_SIM_AT, MODEL_CBS_ENV), format = "qs", cue = tar_cue(mode = "always")),
  tar_target(model_slices, model_state_slices(ST, model_inputs, model_cbs, MODEL_SIM, MODEL_SIM_AT, feed = feed), format = "qs", iteration = "list", cue = tar_cue(mode = "always")),
  tar_target(model_fits, model_fit(model_slices, model_names, model_inputs), pattern = cross(model_slices, model_names), format = "qs", iteration = "list")
)

list(
  map_single,
  map_models,
  # dashboard: contests shown are set by DASHBOARD_CONTESTS in scripts/util/globals.R
  # which models run: the run = TRUE rows of model_config.csv, on the real returns unless
  # MODEL_SIM (scripts/util/globals.R) names a sim scenario
  tar_target(model_config, path.expand(fs::path(PATH_DROPBOX, ELECTION_FOLDER, "model_config.csv")), format = "file"),
  tar_target(model_names, models_from_config(model_config)),
  # writes model_results/ (or the sim's folder) and returns that folder
  tar_combine(model_out, map_models[["model_fits"]], command = model_write(list(!!!.x), MODEL_SIM, MODEL_REDISTRICTING)),
  tar_combine(dash_inputs, map_single[["summary"]], command = list(!!!.x)),
  tar_target(model_results, read_model_results(model_out)),
  tar_target(dashboard_data, build_dashboard_data(dash_inputs, DASHBOARD_CONTESTS, model_results)),
  # data/dashboard/live/dashboard_data.rds, which automation.R pushes for the published page
  tar_target(dashboard_export, export_dashboard_data(dashboard_data), format = "file"),
  if (DASHBOARD_RENDER) tar_quarto(dashboard, "index.qmd", quiet = FALSE)
)
