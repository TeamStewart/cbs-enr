##################################################
## Project: CBS ENR 2025
## Script purpose: Build VA Test files
## Date: October 2025
##################################################
rm(list = ls())
gc()
library(tidyverse)
library(fs)
library(glue)
library(lubridate)

DATA_DIR <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data'
test_files <- dir_ls(path = glue("{DATA_DIR}/25_general/input_data/VA/test_files"), regexp = "gov2_")

reformat_test <- function(df){
  df |> 
    pivot_longer(cols = starts_with("gov"), names_to = 'vote_mode', values_to = 'precinct_total') |>
    mutate(vote_mode = str_remove(vote_mode, "gov_")) |>
    separate(vote_mode, into = c('vote_mode','candidate_name'), sep = "_") |>
    mutate(
      candidate_name = case_match(
        candidate_name,
        'dem' ~ 'Abigal Spanberger',
        'rep' ~ 'Winsome Earle-Sears',
        .default = 'Write-ins'
      ),
      candidate_party = case_match(
        candidate_name,
        'Abigal Spanberger' ~ 'Democrat',
        'Winsome Earle-Sears' ~ 'Republican',
        .default = 'Other'
      ),
      state = 'VA',
      race_id = 'cc535',
      race_name = 'Governor',
      virtual_precinct = FALSE,
      precinct_id = str_replace_all(precinct, " - ", "-"),
      vote_mode = case_match(
        vote_mode,
        'early' ~ 'Early Voting',
        'eday' ~ 'Election Day',
        'mail' ~ 'Absentee/Mail',
        'prov' ~ 'Provisional',
        'post' ~ 'Post-Election',
        .default = 'Other'
      )
    ) |>
    select(state, race_id, race_name, candidate_name, candidate_party, jurisdiction = county, precinct_id,
           virtual_precinct, vote_mode, precinct_total)
}

test_dfs <- map(test_files, ~read_csv(.x) |> reformat_test())

timestamps <- c(
  "2025-11-03 19:45:00", 
  "2025-11-03 20:37:00", 
  "2025-11-03 21:24:00", 
  "2025-11-03 22:38:00", 
  "2025-11-03 23:58:00"
) |>
  map(~force_tz(as_datetime(.x), tzone = "America/New_York"))

test_dfs <- map2(test_dfs, timestamps, function(df, ts) {
  df |>
    mutate(timestamp = ts) |>
    relocate(timestamp, .before = vote_mode)
})

# Save reformatted test files with timestamp in name with underscores like VA_2025_10_22_14_41_56
walk2(test_dfs, timestamps, function(df, ts) {
  ts_str <- format(ts, "%Y_%m_%d_%H_%M_%S")
  write_csv(df, glue("{DATA_DIR}/25_general/input_data/VA/test_files/VA_{ts_str}.csv"))
})
