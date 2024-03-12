rm(list=ls())
gc()

library(tidyverse)
library(targets)
library(httr2)
library(data.table)
library(xml2)

data <- read_csv("data/clean/GA/ALL_primary_latest.csv")

formatted |> 
  pivot_wider(names_from = vote_mode, values_from = precinct_total)

formatted |>
  dplyr::summarise(n = dplyr::n(), .by = c(state.x, race_id, office, candidate_name, candidate_party, jurisdiction, pcntName,
                                           virtual_precinct, jCde, eDate, real_precinct, state_name, st, cnty, county_fips, state.y, ofc, election, eType, cId, jType, pcnt, pcntUUID,
                                           vote_mode)) |>
  dplyr::filter(n > 1L) |> 
  select(vote_mode)
