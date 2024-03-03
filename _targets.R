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

options(timeout = max(300, getOption("timeout")))

tar_option_set(
  packages = c("data.table", "tidyverse", "gt", "xml2", "aws.s3", "jsonlite", "fixest", "googledrive",
               "marginaleffects"),
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE
)

tar_config_set(
  seconds_meta_append = 15,
  seconds_reporter = 0.5
)

# generate the lookup table with important information for each state
values <- tibble(
  state = c("NC", "TX"),
  county = c("ALL", "BEXAR"),
  type = "primary",
  raw_url = c(
    "https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2024_03_05/results_pct_20240305.zip", 
    "https://elections.bexar.org/DocumentCenter/View/38047/March-1-2022-Precinct-by-Precinct-Report-CSV"),
  raw_path = c(
    "data/raw/nc_primary.zip",
    "data/raw/tx_bexar_primary_22.csv"
  )
)

# ========================================
## PIPELINE
# ========================================
list(
  tar_map(
    values,
    tar_target(time, get_timestamp(state, county, type)),
    tar_target(link, command = raw_url, format = "url"),
    tar_target(file, download_file(raw_url, raw_path), format = "file"),
    tar_target(clean, process_data(state, county, type, time, path = raw_path, success = file)),
    tar_target(tbl_all, general_table(clean, state, county, type, time)),
    tar_target(cbs, convert_cbs(clean, state, county, type, time, upload=TRUE)),
    names = c(state, county, type)
  ),
  # finally, build the website
  tar_quarto(website)
)
