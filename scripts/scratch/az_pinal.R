library(tidyverse)
source("scripts/functions.R")

state = 'AZ'
county = 'PINAL'
path = ''

file <- get_clarity(state, county, path) |> read_csv() 


get_clarity(state, county, path) |> 
  read_csv() |> 
  mutate(
    state = state,
    virtual_precinct = FALSE
  ) |> 
  mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
  mutate(candidate_name = case_match(
    vote_mode,
    "Undervotes" ~ "Undervote",
    "Overvotes" ~ "Overvote",
    .default = candidate_name
  ),
  # remove incumbent indicator from Biden
  candidate_name = str_remove_all(candidate_name, "\\(I\\)$") |> str_trim() |> str_squish()
  ) |> 
  mutate(vote_mode = case_match(
    vote_mode, 
    "Election Day Votes" ~ "Election Day",
    "Advance Voting Votes" ~ "Early Voting",
    "Absentee by Mail Votes" ~ "Absentee/Mail",
    "Provisional Votes" ~ "Provisional",
    c("Undervotes", "Overvotes") ~ "Aggregated",
    .default = NA_character_
  )) |> 
  mutate(candidate_party = case_match(
    candidate_party,
    "REP" ~ "Republican",
    "DEM" ~ "Democrat",
    .default = candidate_party
  )) |> 
  mutate(race_name = case_match(
    race_name,
    "President of the US - Rep" ~ "President-Republican",
    "President of the US - Dem" ~ "President-Democrat",
    "President of the US/Presidente de los Estados Unidos - Rep" ~ "President-Republican", 
    "President of the US/Presidente de los Estados Unidos - Dem" ~ "President-Democrat",
    .default = race_name
  )) |> 
  select(state, race_id, race_name, candidate_name, candidate_party, 
         jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)