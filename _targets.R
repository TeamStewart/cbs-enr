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
    # tar_renv()
    "data.table",
    "tidyverse",
    "glue",
    "janitor",
    "fs",
    "aws.s3",
    "marginaleffects",
    "googledrive",
    "rvest",
    "reticulate",
    "xml2",
    "jsonlite",
    "httr2",
    "qs2",
    "devtools",
    "pak",
    "gt",
    "patchwork",
    "tidymodels",
    "enightmodels" # cory package
  ),
  error = "continue",
  controller = crew::crew_controller_local(workers = 4)
)

# get the metadata file, with the following structure
# - state: (string) all caps abbreviation of the state
# - county: (string) lowercase county of interest in that state, if NA then statewide
# - path: (string) generic cell for path/number/ID used by custom scrapers to get file
# - upload: (boolean) whether to upload this jurisdiction to CBS AWS
metadata = readr::read_csv(glue::glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/metadata.csv"), col_types = "cccll")

tests = list.files(
  glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/input_data/VA/test_files"),
  pattern = "VA_.*?$",
  full.names = TRUE
)

models = tidyr::expand_grid(
  method = c("lm", "xgboost"),
  uncertainty = "conformal",
  subset = c(''),
  outcome = c(
    "turnout", 
    "votes_governor_25_dem_precinct_total", "votes_governor_25_rep_precinct_total", 
    "votes_governor_25_dem_share", "votes_governor_25_rep_share"
  ),
  c1 = list(
    ## all combined with past vote/turnout
    NULL,
    # cory's full model
    c("l_votes21", "l_votes24", "p_age_18_29", "p_age_65_up", "p_white_col", "p_white_noncol", "p_black_col", "p_black_noncol", "p_hisp", "p_noncol_male", "p_noncol_fem", "p_dem", "p_ind"),
    # simplified
    # c("l_votes21", "l_votes24", "votePct_gov_21_dem", "votePct_potus_24_dem"),
    # minimal
    c("l_votes24")
  )
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    c2 = dplyr::case_when(
      outcome == "votes_governor_25_dem_precinct_total" ~ "votes_potus_24_dem",
      outcome == "votes_governor_25_rep_precinct_total" ~ "votes_potus_24_rep",
      outcome == "votes_governor_25_dem_share" ~ "votePct_potus_24_dem",
      outcome == "votes_governor_25_rep_share" ~ "votePct_potus_24_rep",
      outcome == "turnout" ~ "votes_precFinal_24",
      .default = NA_character_
    ),
    covars = list(c(c2, c1))
  ) |> 
  dplyr::select(-c1, -c2)
  # tidyr::expand_grid(paths = tests)

# ========================================
## PIPELINE
# ========================================
models <- tar_map(
    models,
    tar_target(model, run_models(
      data_VA_NA, "VA", NA, timestamp_VA_NA, history_VA_NA, office = "Governor", 
      method = method, uncertainty = uncertainty, outcome = outcome, residualize = TRUE,
      subset = subset, covars = covars, weight_var = "turnout"
    ), error = "trim"),
    tar_target(summary, get_model_summary(model)),
    names = c(outcome, method, uncertainty, subset)
  )

list(
  tar_map(
    metadata,
    tar_target(timestamp, get_timestamp(state, county, path), cue = tar_cue(mode = "always")),
    tar_target(history, get_history(state, impute = TRUE, impute_group = "polstratum")),
    tar_target(history_noimpute, get_history(state, impute = FALSE)),
    tar_target(data, get_data(state, county, timestamp, path)),
    tar_target(summary, make_summary(data, state, county, timestamp, history)),
    tar_target(tbl_cbs, create_table_cbs(data, state, county, timestamp, upload)),
    names = c(state, county)
  ),
  models,
  tar_combine(
    models_summary,
    models[["summary"]],
    command = dplyr::bind_rows(!!!.x)
  ),
  tar_quarto(dashboard, "index.qmd", quiet = FALSE),
  tar_target(save_summaries, save_modelsummary(models_summary, "VA", NA, timestamp_VA_NA)),
  tar_target(corymodel, cory_modeling(data_VA_NA, history_VA_NA, timestamp_VA_NA)),
  tar_target(upload, upload_html(dashboard[1], "VA"))
)
