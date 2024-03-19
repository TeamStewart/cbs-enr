# ========================================
## MAIN TARGETS PIPELINE
## AUTHORS: Mason Reece and Joseph R. Loffredo
## FIRST CREATED: Februrary 2024
# ========================================
## SETUP
# ========================================
library(targets)
library(tarchetypes)
suppressMessages(library(tidyverse))

source("scripts/functions.R")

options(timeout = max(300, getOption("timeout")),
        readr.show_col_types = FALSE)

tar_option_set(
  packages = c("data.table", "tidyverse", "gt", "xml2", "aws.s3", "jsonlite", "fixest", "googledrive",
               "marginaleffects", "rlang", "reticulate", "rvest", "httr2", "tabulizer"),
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  error = "null"
)

tar_config_set(
  seconds_meta_append = 15,
  seconds_reporter = 0.5
)

# generate the lookup table with important information for each state
values <- tibble(
  state = c(
    "NC" 
    ,"GA"
    ,"FL"
    ,"FL"
    ,"FL"
    ,"AZ"
    ),
  county = c(
    "ALL"
    ,"ALL"
    ,"ALL"
    ,"ORANGE"
    ,"MIAMI-DADE"
    ,"MARICOPA"
    ),
  type = "primary",
  path = c(
    # NC - ALL
    "https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2024_03_05/results_pct_20240305.zip"
    # GA - ALL
    ,"120015"
    # FL - ALL
    ,"https://flelectionfiles.floridados.gov/enightfilespublic/20240319_ElecResultsFL.txt"
    # FL - Orange
    ,""
    # FL - Miami-Dade
    ,''
    # AZ
    ,''
    )
)

# ========================================
## PIPELINE
# ========================================
list(
  tar_map(
    values,
    tar_target(time, get_timestamp(state, county, type, path), cue = tar_cue(mode = 'always')),
    tar_target(clean, process_data(state, county, type, time, path = path)),
    tar_target(tbl_all, general_table(clean, state, county, type, time)),
    tar_target(cbs, convert_cbs(clean, state, county, type, time, upload=TRUE)),
    # tar_target(models, run_models(clean, state, time)),
    names = c(state, county, type)
  ),
  # finally, build the website
  tar_quarto(website, cue = tar_cue(mode = 'always'))
)