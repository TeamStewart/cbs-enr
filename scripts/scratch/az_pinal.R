library(tidyverse)
library(glue)
library(fs)
library(rvest)
library(httr2)
library(renv)
library(reticulate)
source("scripts/functions.R")

state = 'AZ'
county = 'PINAL'
path = '121811'
type = 'state_primary'
timestamp <- tar_read(time_AZ_MARICOPA_state_primary)

file <- get_clarity(state, county, path, type, timestamp) |> 
  read_csv() |> 
  filter(vote_mode != 'regVotersCounty') |>
  mutate(
    state = state,
    jurisdiction = str_to_upper(jurisdiction),
    virtual_precinct = FALSE
  ) |> 
  mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
  mutate(candidate_name = case_match(
    vote_mode,
    "Undervotes" ~ "Undervote",
    "Overvotes" ~ "Overvote",
    .default = candidate_name
  ),
  candidate_name = case_match(
    candidate_name,
    "NEZ, JONATHAN" ~ "Jonathan Nez",
    "CRANE, ELI" ~ "Eli Crane",
    "SMITH, JACK" ~ "Jack Smith",
    "SCHAFFNER, KATRINA" ~ "Katrina Schaffner",
    "BIGGS, ANDY" ~ "Andy Biggs",
    "ENGEL, KIRSTEN" ~ "Kirsten Engel",
    "CISCOMANI, JUAN" ~ "Juan Ciscomani",
    "WINN, KATHLEEN" ~ "Kathleen Winn",
    "GRIJALVA, RAÚL M." ~ "Raúl Grijalva",
    "BUTIEREZ SR., DANIEL FRANCIS" ~ "Daniel Butierez",
    "GALLEGO, RUBEN" ~ "Ruben Gallego",
    "LAKE, KARI" ~ "Kari Lake",
    "LAMB, MARK" ~ "Mark Lamb",
    "REYE, ELIZABETH JEAN" ~ "Elizabeth Reye",
    "UNDER VOTES" ~ "Undervote",
    "OVER VOTES"~ "Overvote",
    .default = candidate_name
  )
  ) |> 
  mutate(vote_mode = case_match(
    vote_mode, 
    "Election Day" ~ "Election Day",
    "Early Voting" ~ "Early Voting",
    "Provisional" ~ "Provisional",
    c("Undervotes", "Overvotes") ~ "Aggregated",
    .default = NA_character_
  )) |> 
  mutate(candidate_party = case_match(
    candidate_party,
    # TODO: Fix mapping
    "REP" ~ "Republican",
    "DEM" ~ "Democrat",
    .default = candidate_party
  )) |> 
  mutate(race_name = case_match(
    race_name,
    # TODO: Fix mapping
    "REP - U.S. Senator" ~ "US SENATE-Republican",
    "DEM - U.S. Senator" ~ "US SENATE-Democrat",
    "REP - U.S. Representative - CD2" ~ "US HOUSE-02-Republican",
    "DEM - U.S. Representative - CD2" ~ "US HOUSE-02-Democrat",
    "REP - U.S. Representative - CD5" ~ "US HOUSE-05-Republican",
    "DEM - U.S. Representative - CD5" ~ "US HOUSE-05-Democrat",
    "REP - U.S. Representative - CD6" ~ "US HOUSE-06-Republican",
    "DEM - U.S. Representative - CD6" ~ "US HOUSE-06-Democrat",
    "REP - U.S. Representative - CD7" ~ "US HOUSE-07-Republican",
    "DEM - U.S. Representative - CD7" ~ "US HOUSE-07-Democrat",
    .default = NA_character_
  )) |> 
  filter(!is.na(race_name)) |>
  select(state, race_id, race_name, candidate_name,
         candidate_party, jurisdiction, precinct_id, virtual_precinct,vote_mode, precinct_total)
