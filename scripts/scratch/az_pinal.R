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
path = '104398'
type = 'state_primary'
timestamp <- tar_read(time_AZ_MARICOPA_state_primary)

file <- get_clarity(state, county, path, type, timestamp) |> read_csv() 


get_clarity(state, county, path, type, timestamp) |> 
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
  candidate_name = case_when(
    str_detect(candidate_name, regex("Write-in", ignore_case = T)) ~ "Write-ins",
    str_detect(candidate_name, regex("Cherny", ignore_case = T)) ~ "Andrei Cherny",
    str_detect(candidate_name, regex("-Woods", ignore_case = T)) ~ "Marlene Galán-Woods",
    str_detect(candidate_name, regex("Horne", ignore_case = T)) ~ "Andrew Horne",
    str_detect(candidate_name, regex("Kroemer", ignore_case = T)) ~ "Kurt Kroemer",
    str_detect(candidate_name, regex("Callaghan", ignore_case = T)) ~ "Conor O'Callaghan",
    str_detect(candidate_name, regex("Shah", ignore_case = T)) ~ "Amish Shah",
    str_detect(candidate_name, regex("Backie", ignore_case = T)) ~ "Robert Backie",
    str_detect(candidate_name, regex("George, Kim", ignore_case = T)) ~ "Kim George",
    str_detect(candidate_name, regex("Schweikert", ignore_case = T)) ~ "David Schweikert",
    str_detect(candidate_name, regex("Ansari", ignore_case = T)) ~ "Yassamin Ansari",
    str_detect(candidate_name, regex("Raquel", ignore_case = T)) ~ "Raquel Terán",
    str_detect(candidate_name, regex("Wooten", ignore_case = T)) ~ "Duane Wooten",
    str_detect(candidate_name, regex("Mendoza", ignore_case = T)) ~ "Jesus Mendoza",
    str_detect(candidate_name, regex("Zink", ignore_case = T)) ~ "Jeff Zink",
    str_detect(candidate_name, regex("Stanton", ignore_case = T)) ~ "Greg Stanton",
    str_detect(candidate_name, regex("Cooper", ignore_case = T)) ~ "Kelly Cooper",
    str_detect(candidate_name, regex("Davison", ignore_case = T)) ~ "Jerone Davison",
    str_detect(candidate_name, regex("Giles", ignore_case = T)) ~ "Dave Giles",
    str_detect(candidate_name, regex("Jasser", ignore_case = T)) ~ "Zuhdi Jasser",
    str_detect(candidate_name, regex("GALLEGO", ignore_case = T)) ~ "Ruben Gallego",
    str_detect(candidate_name, regex("LAKE", ignore_case = T)) ~ "Kari Lake",
    str_detect(candidate_name, regex("LAMB", ignore_case = T)) ~ "Mark Lamb",
    str_detect(candidate_name, regex("REYE", ignore_case = T)) ~ "Elizabeth Reye",
    candidate_name == "UNDER VOTES" ~ "Undervote",
    candidate_name == "OVER VOTES"~ "Overvote",
    TRUE ~ candidate_name
  )
  ) |> 
  mutate(vote_mode = case_match(
    vote_mode, 
    # TODO: Fix mapping, they seem to alternate it
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
  mutate(race_name = case_when(
    # TODO: Fix mapping
    race_name == "U.S. Senator" & candidate_party == 'DEM' ~ "US SENATE-Democrat", 
    race_name == "U.S. Senator" & candidate_party == 'REP' ~ "US SENATE-Republican",
    race_name == "U.S. Representative in Congress DISTRICT 1" & candidate_party == 'DEM' ~ "US HOUSE-01-Democrat",
    race_name == "U.S. Representative in Congress DISTRICT 1" & candidate_party == 'REP' ~ "US HOUSE-01-Republican",
    race_name == "U.S. Representative in Congress DISTRICT 3" & candidate_party == 'DEM' ~ "US HOUSE-03-Democrat",
    race_name == "U.S. Representative in Congress DISTRICT 3" & candidate_party == 'REP' ~ "US HOUSE-03-Republican",
    race_name == "U.S. Representative in Congress DISTRICT 4" & candidate_party == 'DEM' ~ "US HOUSE-04-Democrat",
    race_name == "U.S. Representative in Congress DISTRICT 4" & candidate_party == 'REP' ~ "US HOUSE-04-Republican"
    .default = NA_character_
  )) |> 
  filter(!is.na(race_name)) |>
  select(state, race_id, race_name, candidate_name, candidate_party, 
         jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
