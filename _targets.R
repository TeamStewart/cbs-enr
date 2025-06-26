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
  scipen = 999,
  timeout=60000
)

tar_option_set(
  packages = c(
    # tar_renv()
    "data.table", "tidyverse", "glue", "janitor", "fs", "aws.s3", "gt", "pak",
    "googledrive", "httr2", "rvest", "reticulate", "sf", "xml2", "jsonlite", "qs2"
  ),
  memory = "transient",
  error = "continue",
  format = "qs",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 3)
)

# get the metadata file, with the following structure
# - state: (string) caps abbreviation of the state
# - county: (string) lowercase county of interest in that state, if NA then statewide
# - path: (string) generic cell for path/number/ID used by custom scrapers to get file
# - preelection_totals: (boolean) 
# - upload: (boolean) whether to upload this jurisdiction to CBS AWS
metadata = read_csv("data/metadata.csv", col_types = "cccll") |> 
  drop_na(path) |> 
  mutate(upload = FALSE)

# ========================================
## PIPELINE
# ====================Z====================
list(
  tar_map(
    metadata,
    tar_target(timestamp, get_timestamp(state, county, path), cue = tar_cue(mode = "always")),
    tar_target(data, get_data(state, county, timestamp, path)),
    tar_target(tbl_cbs, create_table_cbs(data, state, county, timestamp, upload)),
    # tar_target(model, run_models(data, state, county, timestamp, preelection_totals)),
    # tar_target(plot_voteShare, make_plot_voteShare(model, state, county)),
    # tar_target(plot_margin2020, make_plot_margin2020(model, state, county)),
    # tar_target(plot_votesEDay, make_plot_votesEDay(model, state, county)),
    # tar_target(plot_votesAll, make_plot_votesAll(model, state, county)),
    # tar_target(tbl_countyMode, make_tbl_countyMode(model, state, county)),
    # tar_target(tbl_county, make_tbl_county(model, state, county)),
    names = c(state, county)
  )
  # tar_quarto(page_GA, path = "pages/GA.qmd"),
  # tar_quarto(page_NC, path = "pages/NC.qmd"),
  # tar_quarto(page_AZ_Maricopa, path = "pages/AZ_Maricopa.qmd"),
  # tar_quarto(page_MI_Eaton, path = "pages/MI_Eaton.qmd"),
  # tar_quarto(page_MI_Ingham, path = "pages/MI_Ingham.qmd"),
  # tar_quarto(page_MI_Macomb, path = "pages/MI_Macomb.qmd"),
  # tar_quarto(page_MI_Oakland, path = "pages/MI_Oakland.qmd"),
  # tar_quarto(page_PA_Allegheny, path = "pages/PA_Allegheny.qmd"),
  # tar_quarto(page_PA_Delaware, path = "pages/PA_Delaware.qmd"),
  # tar_quarto(page_PA_Philadelphia, path = "pages/PA_Philadelphia.qmd"),
  # tar_quarto(website, cache = FALSE, quiet = FALSE),
  # tar_target(uploads, upload_html(), priority = 0, cue = tar_cue(mode = "always"))
)
