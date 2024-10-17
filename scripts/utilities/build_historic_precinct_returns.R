##################################################
## Project: CBS ENR
## Script purpose: Build 2020 precinct file from MEDSL
## Date: October 2024
## Author: Joseph R. Loffredo
##################################################

rm(list = ls())
gc()

library(tidyverse)
library(arrow)

# Step 1: Process the dataset
returns <- open_dataset("/Users/josephloffredo/Dropbox (MIT)/CVR_parquet/returns/by-precinct-mode/") |>
  collect() |>
  mutate(
    keep = case_when(
      office == "US PRESIDENT" & state %in% c('GEORGIA', 'NORTH CAROLINA') ~ TRUE,
      office == "US PRESIDENT" & state == "MICHIGAN" & county_name %in% c('OAKLAND','MACOMB') ~ TRUE,
      office == "US PRESIDENT" & state == "PENNSYLVANIA" & county_name %in% c('ALLEGHENY', 'PHILADELPHIA', 'DELAWARE') ~ TRUE,
      office == 'GOVERNOR' & state == 'NORTH CAROLINA' ~ TRUE,
      office %in% c("US PRESIDENT","US SENATE") & state == "ARIZONA" & county_name %in% c('MARICOPA','PIMA') ~ TRUE,
      TRUE ~ FALSE
    ),
    candidate = case_when(
      candidate %in% c("JOSEPH R BIDEN", "DONALD J TRUMP", "MARK KELLY", "MARTHA MCSALLY", "ROY COOPER", "DAN FOREST") ~ candidate,
      candidate == "OVERVOTES" ~ "Overvote/Undervote",
      candidate == "UNDERVOTES" ~ "Overvote/Undervote",
      TRUE ~ "ALL OTHERS"
    ) |> str_to_title(),
    party_simplified = case_match(
      party_simplified,
      "DEMOCRAT" ~ "Democrat",
      "REPUBLICAN" ~ "Republican",
      "OVERVOTE" ~ "Overvote/Undervote",
      "UNDERVOTE" ~ "Overvote/Undervote",
      .default = "All Others"
    ),
    office = case_match(
      office,
      "US PRESIDENT" ~ "President",
      "GOVERNOR" ~ "Governor",
      "US SENATE" ~ "US Senate",
      .default = NA_character_
    ),
    mode = case_when(
      party_simplified == "OVERVOTE" ~ "Overvote/Undervote",
      party_simplified == "UNDERVOTE" ~ "Overvote/Undervote",
      mode == "TOTAL" ~ "Total",
      mode == "ABSENTEE" ~ "Absentee/Mail",
      mode == "PROVISIONAL" ~ "Provisional",
      mode == "ELECTION DAY" ~ "Election Day",
      mode == "EARLY VOTING" ~ "Early Voting",
      mode == "ONE STOP" ~ "Early Voting",
      mode == "EARLY" ~ "Early Voting",
      mode == "2ND ABSENTEE" ~ "Absentee/Mail",
      mode == "MAIL" ~ "Absentee/Mail",
      mode == "ABSENTEE BY MAIL" ~ "Absentee/Mail",
      mode == "MAIL BALLOTS" ~ "Absentee/Mail",
      mode == "IN PERSON ABSENTEE" ~ "Early Voting",
      mode == "FAILSAFE PROVISIONAL" ~ "Provisional",
      mode == "Election Day Votes" ~ "Election Day",
      mode == "Advance Voting Votes" ~ "Early Voting",
      mode == "Absentee by Mail Votes" ~ "Absentee/Mail",
      mode == "Provisional Votes" ~ "Provisional",
      TRUE ~ 'Miscellaneous'
    ),
    precinct = ifelse(state == 'MICHIGAN', glue::glue("{jurisdiction_name}-{precinct}"), precinct)
  ) |>
  filter(keep) |>
  select(-c(county_fips:jurisdiction_fips,district,magnitude,special,writein,dist_state,party_detailed,keep))

# Step 2: Summarise and ensure "Total" rows using .by
summary <- returns |>
  summarise(
    precinct_total = sum(votes, na.rm = TRUE),
    .by = c("state", "county_name", "precinct", "office", "party_simplified", "candidate", "mode")
  ) |>
  rename(
    race_name = office,
    candidate_name = candidate,
    candidate_party = party_simplified,
    jurisdiction = county_name,
    precinct_name = precinct,
    vote_mode = mode
  )

# Step 3: Add "Total" rows for combinations that do not have them
totals_needed <- summary |>
  summarise(
    total_exists = any(vote_mode == "Total"),
    precinct_total_sum = sum(precinct_total[vote_mode %in% c("Early Voting", "Election Day", "Provisional", "Absentee/Mail", "Miscellaneous")], na.rm = TRUE),
    .by = c("state", "race_name", "candidate_name", "candidate_party", "jurisdiction", "precinct_name")
  ) |>
  filter(!total_exists) |>
  mutate(vote_mode = "Total", precinct_total = precinct_total_sum) |>
  select(-total_exists, -precinct_total_sum)

# Combine the new "Total" rows with the original summary
final_data <- bind_rows(summary, totals_needed) |>
  arrange(state, race_name, candidate_name, jurisdiction, precinct_name, vote_mode)

write_csv(final_data, "data/input/previous_returns_2020.csv")

