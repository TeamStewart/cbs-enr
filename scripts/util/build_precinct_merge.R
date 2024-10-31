##################################################
## Project: CBS ENR 2024
## Script purpose: Build base file for modeling
## Date: November 2024
##################################################

library(tidyverse)
library(sf)
library(glue)
library(arrow)

rm(list = ls())
gc()

sf_use_s2(FALSE)
options(scipen = 800)

# Load 2020 precincts
precincts_2020 <- open_dataset("~/Dropbox (MIT)/CVR_parquet/returns/by-precinct-mode/") |>
  filter(state %in% c('ARIZONA', 'GEORGIA', 'MICHIGAN', 'NORTH CAROLINA', 'PENNSYLVANIA')) |>
  collect()

# North Carolina ----------------------------------------------------------


# MEDSL 2020 precinct data, by mode
## Specifically, this gets Dem totals by vote mode, with column for total votes
data20 <- precincts_2020 |> 
  filter(state == 'NORTH CAROLINA', office == "US PRESIDENT") |> 
  select(county = county_name, precinct_20 = precinct, candidate, vote_mode = mode, votes_20 = votes) |> 
  mutate(
    state = "NC",
    votes_precFinal_20 = sum(votes_20),
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    vote_mode = case_match(
      vote_mode,
      "ABSENTEE BY MAIL" ~ "Absentee/Mail",
      "ONE STOP" ~ "Early Voting",
      "ELECTION DAY" ~ "Election Day",
      "PROVISIONAL" ~ "Provisional",
    ),
  ) |> 
  filter(str_detect(candidate, "BIDEN")) |> 
  select(-candidate)

# 2020 shapefiles, from VEST data
shp20 <- read_sf("data/shapefiles/nc_20/") |> 
  st_transform("NAD83") |>
  select(
    precinct_20 = prec_id, county = county_nam
  ) |> 
  st_make_valid() |> 
  drop_na(county)

# 2022 shapefiles, from the GA legis reapportionment website
shp24 <- read_sf("data/shapefiles/nc_24/") |> 
  st_transform("NAD83") |>
  select(
    precinct_24 = prec_id, county = county_nam
  ) |> 
  st_make_valid() |> 
  drop_na(county)

# merge the shapes together so that we can compute an overlap score
intersection <- st_intersection(shp24, shp20) |> filter(county == county.1)

data_history <- intersection |> 
  # create the areal weighting
  mutate(area = st_area(intersection) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_20, precinct_24) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(county, precinct_24)) |> 
  # now full-join to ensure all county x precincts are represented in the data
  # so that we can fill in some missigness
  full_join(
    expand(shp20, nesting(county, precinct_20), vote_mode = c("Absentee/Mail", "Early Voting", "Election Day", "Provisional")),
    join_by(county, precinct_20),
    relationship = "many-to-many"
  ) |> 
  # add the matched 2020 data
  left_join(data20, join_by(county, precinct_20, vote_mode), relationship = "many-to-many") |> 
  # fill in missigness with county mean so we can make estimates
  mutate(
    across(c(votes_20, votePct_dem_20, votePct_rep_20, votes_precFinal_20), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votePct_dem_20 = sum(votePct_dem_20 * weight),
    votePct_rep_20 = sum(votePct_rep_20 * weight),
    # votePct_diff_20 = sum(votePct_diff_20 * weight),
    votes_precFinal_20 = sum(votes_precFinal_20 * weight),
    votes_20 = sum(votes_20 * weight),
    .by = c(county, precinct_24, vote_mode)
  )


