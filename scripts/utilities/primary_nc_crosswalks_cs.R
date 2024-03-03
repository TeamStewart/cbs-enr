##################################################
## Project: CBS Election Night
## Script purpose: Build NC Primary xwalks for Charles
## Date: March 2023
##################################################

rm(list=ls())
gc()

library(tidyverse)
library(data.table)

## Read in files
## CBS Regions
regions <- read_csv("data/input/NC/regions.csv", col_names = c("jurisdiction", "region"), skip = 1)

# Voter registration snaphshots
vreg <- fread(
  cmd = "curl -L https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvoter_Statewide.zip | funzip", 
  select = c(
    'ncid','voter_status_desc','race_code','ethnic_code','party_cd','age_at_year_end',
    'county_id', 'county_desc', 'precinct_abbrv', 'precinct_desc', 'vtd_abbrv', 'vtd_desc'
  )) |>
  mutate(
    # recode race
    race = case_when(
      race_code == 'B' ~ 'black',
      race_code == "W" ~ "white",
      race_code == 'A' ~ "asian",
      TRUE ~ "other"
    ),
    # recode party
    party = case_when(
      party_cd == "DEM" ~ "Democrat",
      party_cd == "REP" ~ "Republican",
      party_cd == "LIB" ~ "Libertarian",
      party_cd == "GRE" ~ "Green",
      party_cd == "UNA" ~ "Unaffiliated",
      TRUE ~ "Other"
    )
  )

# because we are looking at primary, get demos at precinct X party level
vreg_by_party <- vreg |>
  group_by(county_id, county_desc, precinct_abbrv, precinct_desc, vtd_abbrv, vtd_desc, party) |>
  summarise(
    precinct_total_party_reg = n(),
    precinct_total_party_black = sum(race == 'black'),
    precinct_total_party_white = sum(race == 'white'),
    precinct_total_party_hispanic = sum(ethnic_code == 'HL'),
    precinct_total_party_nonwhite = sum(race != 'white'),
    precinct_avg_party_age = mean(age_at_year_end,na.rm = T)
  )

# in case we want demos just at precinct level
vreg_total <- vreg |>
  group_by(county_id, county_desc, precinct_abbrv, precinct_desc, vtd_abbrv, vtd_desc) |>
  summarise(
    precinct_total_reg = n(),
    precinct_total_una = sum(party == 'Unaffiliated'),
    precinct_total_black = sum(race == 'black'),
    precinct_total_white = sum(race == 'white'),
    precinct_total_hispanic = sum(ethnic_code == 'HL'),
    precinct_total_nonwhite = sum(race != 'white'),
    precinct_avg_age = mean(age_at_year_end,na.rm = T)
  )

# grab voter reg history stat to help create lagged denominators
vreg_lag <- fread(cmd = "curl -L https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2020_03_03/voter_stats_20200303.zip | funzip") |>
  mutate(
    # recode party
    party = case_when(
      party_cd == "DEM" ~ "Democrat",
      party_cd == "REP" ~ "Republican",
      party_cd == "LIB" ~ "Libertarian",
      party_cd == "GRE" ~ "Green",
      party_cd == "UNA" ~ "Unaffiliated",
      TRUE ~ "Other"
    )
  )

# vreg precinct X party lag
vreg_by_party_lag <- vreg_lag |>
  group_by(county_desc, precinct_abbrv, party) |>
  summarise(precinct_total_party_reg_lag = sum(total_voters))

# vreg precinct lag
vreg_total_lag <- vreg_lag |>
  group_by(county_desc, precinct_abbrv) |>
  summarise(precinct_total_reg_lag = sum(total_voters))


# Use voter turnout file: more encompassing n for all races
precinct_turnout <- fread(cmd = "curl -L http://dl.ncsbe.gov/ENRS/2020_03_03/history_stats_20200303.zip | funzip") |>
  group_by(county_desc, precinct_abbrv, voted_party_cd, voting_method) |>
  summarise(precinct_total = sum(total_voters)) |>
  mutate(vote_mode = case_when(
    voting_method == 'IN-PERSON' ~ "election_day",
    voting_method == "ABS-1STOP" ~ "early_voting",
    voting_method == "ABS-MAIL" ~ "absentee",
    voting_method == "PROV" ~ "provisional",
    TRUE ~ 'other'
  )) |>
  pivot_wider(names_from = "vote_mode",values_from = "precinct_total",id_cols = c("county_desc","precinct_abbrv","voted_party_cd"),values_fn = sum, names_prefix = "mode_total_lag_") |>
  mutate(across(c(mode_total_lag_election_day, mode_total_lag_early_voting, mode_total_lag_absentee, mode_total_lag_provisional, mode_total_lag_other), ~replace_na(., 0))) |>
  rowwise() |>
  mutate(precinct_total_turnout_lag = sum(c_across(c(mode_total_lag_election_day, mode_total_lag_early_voting, mode_total_lag_absentee, mode_total_lag_provisional, mode_total_lag_other)), na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    party = case_when(
      voted_party_cd == "DEM" ~ "Democrat",
      voted_party_cd == "REP" ~ "Republican",
      voted_party_cd == "LIB" ~ "Libertarian",
      voted_party_cd == "GRE" ~ "Green",
      voted_party_cd == "UNA" ~ "Independent",
      TRUE ~ "Other"
    )
  )

precinct_xwalk <- vreg_by_party |>
  left_join(precinct_turnout) |>
  left_join(vreg_total) |>
  left_join(vreg_by_party_lag) |>
  left_join(vreg_total_lag) |>
  filter(party %in% c('Democrat','Republican'))



### jurisdiction = county_desc, precinct_id = precinct_abbrv

# Now calculate percentages
## precinct is for election-day statistics
precinct_party_xwalk <- precinct_xwalk |>
  mutate(
    precinct_pct_party_black = precinct_total_party_black / precinct_total_party_reg,
    precinct_pct_party_white = precinct_total_party_white / precinct_total_party_reg,
    precinct_pct_party_nonwhite = precinct_total_party_nonwhite / precinct_total_party_reg,
    
    precinct_pct_black = precinct_total_black / precinct_total_reg,
    precinct_pct_white = precinct_total_white / precinct_total_reg,
    precinct_pct_nonwhite = precinct_total_nonwhite / precinct_total_reg,
    
    precinct_pct_una = precinct_total_una / precinct_total_reg,
    
    # our eday turnout = lagged eday vote total / lagged total registered voters
    precinct_pct_party_election_day_lag = mode_total_lag_election_day / precinct_total_party_reg_lag
  ) |>
  left_join(regions, by = c("county_desc" = "jurisdiction")) |>
  select(county_id, county_desc, cbs_region = region, precinct_abbrv, precinct_desc, vtd_abbrv, vtd_desc, party, precinct_total_reg, precinct_total_party_reg,
         precinct_pct_black, precinct_pct_party_black, precinct_pct_white, precinct_pct_party_white, precinct_pct_nonwhite, precinct_pct_party_nonwhite,
         precinct_avg_age,precinct_avg_party_age, precinct_pct_una, precinct_pct_party_election_day_lag) |>
  ungroup()

## jurisdiction level for early and absentee and over all turnout
# just to get count-level demos (not by party)
county_vreg_race <- vreg |>
  group_by(county_id, county_desc) |>
  summarise(
    county_pct_black = sum(race == 'black') / n(),
    county_pct_white = sum(race == 'white') / n(),
    county_pct_nonwhite = sum(race != 'white') / n(),
    county_total_reg = n(),
    county_pct_una = sum(party == 'Unaffiliated') / n(),
    county_avg_age = mean(age_at_year_end,na.rm = T))

county_vreg_age <- vreg |>
  group_by(county_id, county_desc, party) |>
  summarise(county_avg_party_age = mean(age_at_year_end,na.rm = T))

county_party_xwalk <- precinct_xwalk |>
  group_by(county_id, county_desc, party) |>
  summarise(
    county_total_party_reg = sum(precinct_total_party_reg,na.rm = T),
    county_pct_party_black = sum(precinct_total_party_black,na.rm = T) / county_total_party_reg,
    county_pct_party_white = sum(precinct_total_party_white,na.rm = T) / county_total_party_reg,
    county_pct_party_nonwhite = sum(precinct_total_party_nonwhite,na.rm = T) / county_total_party_reg,
    
    county_pct_party_election_day_lag = sum(mode_total_lag_election_day,na.rm = T) / sum(precinct_total_party_reg_lag,na.rm = T),
    county_pct_party_absentee_lag = sum(mode_total_lag_absentee,na.rm = T) / sum(precinct_total_party_reg_lag,na.rm = T),
    county_pct_party_early_lag = sum(mode_total_lag_early_voting,na.rm = T) / sum(precinct_total_party_reg_lag,na.rm = T),
    
    county_pct_party_turnout_lag = sum(precinct_total_turnout_lag,na.rm = T) / sum(precinct_total_party_reg_lag,na.rm = T)
  ) |>
  left_join(county_vreg_race) |>
  left_join(county_vreg_age) |>
  left_join(regions, by = c("county_desc" = "jurisdiction")) |>
  select(county_id, county_desc, cbs_region = region, party, county_total_reg, county_total_party_reg,
         county_pct_black, county_pct_party_black, county_pct_white, county_pct_party_white, county_pct_nonwhite, county_pct_party_nonwhite,
         county_avg_age,county_avg_party_age, county_pct_una, 
         county_pct_party_election_day_lag, county_pct_party_absentee_lag, county_pct_party_early_lag, county_pct_party_turnout_lag) |>
  ungroup()

# Write csv with vf names for charles
write_csv(precinct_party_xwalk, file = "data/input/NC/precinct_party_xwalk_cs.csv")
write_csv(county_party_xwalk, file = "data/input/NC/county_party_xwalk_cs.csv")

