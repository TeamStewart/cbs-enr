##################################################
## Project: CBS ENR 2025
## Script purpose: NY Test File
## Date: October 2025
##################################################

rm(list = ls())
gc()

library(tidyverse)
library(glue)

DATA_DIR <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data'

ny_history <- read_csv(glue("{DATA_DIR}/25_general/input_data/NY/NY_history.csv")) |>
  select(ad,ed, votes_potus_24_dem, votes_potus_24_rep, votes_precFinal_24) |>
  mutate(
    votes_potus_24_other = votes_precFinal_24 - (votes_potus_24_dem + votes_potus_24_rep)
  )

ed_identifiers <- read_csv(
  glue("{DATA_DIR}/25_general/input_data/NY/NYC_20250809_demo_prec_20250812.csv"), 
  col_select = c("ad","ed")) |>
  distinct()

candidates <- read_tsv(glue("{DATA_DIR}/25_general/cbs_lookups/candidate_ids.tsv")) |>
  filter(STATE == 'New York') |>
  mutate(
    candidate_name = NAME,
    candidate_party = case_match(
      candidate_name,
      "Zohran Mamdani" ~ "Democrat",
      "Curtis Sliwa" ~ "Republican",
      "Irene Estrada" ~ "Conservative",
      "Zohran Mamdani" ~ "Working Families",
      "Eric Adams" ~ "Safe&Affordable/EndAntiSemitism",
      "Joseph Hernandez" ~ "Quality of Life",
      "Andrew Cuomo" ~ "Fight and Deliver",
      "Jim Walden" ~ "Integrity",
      .default = NA_character_
    )
  ) |>
  select(candidate_name, candidate_party)

output <- ed_identifiers |>
  expand_grid(candidates) |>
  mutate(
    state = 'NY',
    race_id = NA,
    race_name = 'Mayor',
    virtual_precinct = FALSE,
    timestamp = now(),
    vote_mode = 'Total',
    jurisdiction = 'New York'
  ) |>
  select(state, race_id, race_name, candidate_name, candidate_party,
         jurisdiction, ad, ed, virtual_precinct, timestamp,
         vote_mode) |>
  left_join(ny_history, by = c("ad","ed")) |>
  mutate(
    precinct_total = case_when(
      candidate_party == "Democrat" ~ (0.65 + runif(n(), min = -0.05, max = 0.05)) * votes_potus_24_dem,
      candidate_party == "Fight and Deliver" ~ (0.2 + runif(n(), min = -0.05, max = 0.05)) * votes_potus_24_dem + (0.1 + runif(n(), min = -0.05, max = 0.05)) * votes_potus_24_rep,
      candidate_party == "Republican" ~ (0.2 + runif(n(), min = -0.05, max = 0.05)) * votes_potus_24_rep,
      TRUE ~ (1 + runif(n(), min = -0.2, max = 0.2)) * votes_potus_24_other
    ) |> round(0),
    precinct_id = glue("{ad}_{str_pad(ed, 3, 'left', '0')}"),
    jurisdiction = case_when(
      ad %in% c(37, 61, 65:76) ~ "New York",
      ad %in% c(77:87) ~ "Bronx",
      ad %in% c(41:61, 64) ~ "Kings",
      ad %in% c(23:40) ~ "Queens",
      ad %in% c(61:64) ~ "Richmond",
      TRUE ~ NA_character_
    )
  ) |>
  select(
    state, race_id, race_name, candidate_name, candidate_party,
    jurisdiction, ad, ed, precinct_id, virtual_precinct, timestamp,
    vote_mode, precinct_total
  )

output <- output |>
  mutate(
    precinct_total = if_else(ad %in% c(61,63,64), 0 , precinct_total)
  )

write_csv(output, glue("{DATA_DIR}/25_general/input_data/NY/NY_test_file.csv"))
