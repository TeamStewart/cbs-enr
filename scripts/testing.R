rm(list=ls())
gc()

library(tidyverse)
library(targets)

data <- tar_read(clean_NC_ALL_primary) |> 
  filter(str_detect(race_name, regex("Governor|President", ignore_case=TRUE))) |> 
  mutate(race_name = str_remove_all(race_name, " DEM$| REP$|-DEM$|-REP$|-LIB$| LIB$|-Democrat|-Republican|-Libertarian")) |> 
  filter(candidate_party %in% c("Democrat", "Republican")) |> 
  mutate(
    jCde = case_when(
      str_detect(race_name, "^US House") ~ str_extract(race_name, "\\d+") |> as.numeric() |> as.character(),
      .default = "0"
    ),
    race_name = case_when(
      str_detect(race_name, "^US House") ~ "House",
      race_name == "NC SECRETARY OF STATE" ~ "Secretary Of State",
      race_name == "NC ATTORNEY GENERAL" ~ "Attorney General",
      race_name == "NC LIEUTENANT GOVERNOR" ~ "Lt Governor",
      str_detect(race_name, "^Governor") ~ "Governor",
      .default = race_name
    )
  ) |> 
  rename(
    office = race_name
  ) |> 
  mutate(eDate = "2024-03-05",
         real_precinct = ifelse(virtual_precinct, "N", "Y")) |> 
  left_join(lookup_geo, join_by(state == postalCode, jurisdiction == county_name))

lookup_cands <- read_csv("data/input/cbs_lookups/2024-03-01 Primary Candidates.csv",
                  col_types = cols(.default = col_character()),
                  col_names = c("state", "office", "ofc", "election", "etype", "jCde", "candidate_name", "candidate_lastname", "cId")) |>
  filter(state == "North Carolina") |>
  mutate(candidate_party = case_match(
    etype,
    "D" ~ "Democrat",
    "R" ~ "Republican"))

lookup_geo <- read_csv("data/input/cbs_lookups/All States and Counties.csv",
                       skip = 1,
                       col_names = c("state_name", "postalCode", "st", "state_fips", "county_name", "cnty", "county_fips")) |> 
  select(-state_fips) |> 
  mutate(county_name = str_to_upper(county_name))

match <- data |>
  distinct(office, jCde, candidate_party, candidate_name) |>
  left_join(lookup_cands, join_by(office, jCde, candidate_party, candidate_name))

lookup_cands_remaining <- lookup_cands |>
  anti_join(filter(match, !is.na(state)), join_by(candidate_name))

match2 <- match |>
  filter(is.na(state)) |>
  select(office, jCde, candidate_party, candidate_name) |>
  mutate(candidate_lastname = str_remove_all(candidate_name, "Jr.*?$|III$|,") |> str_squish()) |>
  mutate(candidate_lastname = str_extract(candidate_lastname, "\\s(\\w+)$") |> str_squish()) |>
  # select(-candidate_name) |>
  left_join(select(lookup_cands_remaining, -candidate_name), join_by(office, jCde, candidate_party, candidate_lastname))

match |>
  filter(!is.na(state)) |>
  bind_rows(match2) |>
  rename(eType = etype) |> 
  write_csv("data/input/cbs_lookups/primary_cands.csv")

lookup_cands <- read_csv("data/input/cbs_lookups/primary_cands_nc.csv", 
                         col_types = cols(.default = col_character())) |> 
  select(-state, -candidate_lastname)


# jType
# key

modified <- data |> 
  filter(candidate_party %in% c("Democrat", "Republican")) |> 
  mutate(
    jCde = case_when(
      str_detect(race_name, "^US House") ~ str_extract(race_name, "\\d+") |> as.numeric() |> as.character(),
      .default = "0"
    ),
    race_name = case_when(
      str_detect(race_name, "^US House") ~ "House",
      race_name == "NC SECRETARY OF STATE" ~ "Secretary Of State",
      race_name == "NC ATTORNEY GENERAL" ~ "Attorney General",
      race_name == "NC LIEUTENANT GOVERNOR" ~ "Lt Governor",
      str_detect(race_name, "^Governor") ~ "Governor",
      .default = race_name
    )
  ) |> 
  rename(
    office = race_name
  ) |> 
  mutate(eDate = "2024-03-05",
         real_precinct = ifelse(virtual_precinct, "N", "Y"),
         etype = "P") |> 
  left_join(lookup_geo, join_by(jurisdiction == county_name)) |> 
  left_join(lookup_cands, join_by(office, jCde, candidate_party, candidate_name)) |> 
  mutate(jType = ifelse(jCde == "0", "SW", "CD")) |> 
  mutate(precinct_id = str_c(county_fips, precinct_id, sep = "_")) |> 
  rename(pcntName = precinct_id) |> 
  mutate(pcnt = pcntName,
         pcntUUID = pcntName)



example <- tibble(nc = jsonlite::read_json("~/Downloads/nc_results_2022.json")) |> 
  unnest_wider(col = nc)

example |> 
  distinct(jType)


final <- modified |> 
  pivot_wider(names_from = vote_mode, values_from = precinct_total) |> 
  rename(edayVote = `Election Day`,
         earlyInPersonVote = `Early Voting`,
         earlyByMailVote = `Absentee/Mail`,
         provisionalVote = Provisional,
         cName = candidate_name) |> 
  mutate(cVote = edayVote + earlyInPersonVote + earlyByMailVote + provisionalVote,
         ts = tar_read(time_NC_ALL_primary)) |> 
  select(eDate, jType, real_precinct, st, etype, jCde, ofc, cnty, pcnt, pcntUUID, pcntName, 
         cId, cName, cVote, edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote,
         ts) |> 
  nest(candidates = c(cId, cName, cVote, edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote)) |> 
  # mutate(key = list(eDate, st, etype, jType, jCde, ofc, cnty, pcnt, pcntUUID))
  nest(key = c(eDate, st, etype, jType, jCde, ofc, cnty, pcnt, pcntUUID)) |>
  mutate(key2 = key) |>
  unnest(cols = key2) |> 
  mutate(key = map(key, as.list)) |> 
  # mutate(key = map(key, pull)) |> 
  select(eDate, jType, real_precinct, key, pcntName, candidates, jCde, ts, st, ofc, etype, pcntUUID, cnty)

tar_read(cbs_NC_ALL_primary) |> 
  filter(pcntName == "37001_01") |> 
  select(key, candidates) |> 
  unnest(cols = candidates)
