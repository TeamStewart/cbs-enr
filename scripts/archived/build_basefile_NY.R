##################################################
## Project: CBS ENR 2025
## Script purpose: Build NY basefiles
## Date: September 2025
##################################################

rm(list = ls())
gc()

options(scipen = 999)

library(tidyverse)
library(sf)
library(glue)
library(fs)
library(fastLink)

DATA_DIR <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data'

#### Load files ####
shapefile_2021 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/nyedwi_21d/nyedwi.shp'))
shapefile_2024 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/nyedwi_24d/nyedwi.shp'))
shapefile_2025 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/nyedwi_25c/nyedwi.shp'))
l2_identifiers <- read_csv(glue("{DATA_DIR}/25_general/input_data/NY/ny_20250809_demo_prec_20250812.csv"))
ny25_primary <- read_csv(glue("{DATA_DIR}/25_general/input_data/NY/nyc_prim_prec_cvr_summary.csv"))
ny24_potus <- read_csv(glue("{DATA_DIR}/25_general/input_data/NY/00000100000Citywide President Vice President Citywide EDLevel.csv"), col_names = F)
ny21_mayor <- read_csv(glue("{DATA_DIR}/25_general/input_data/NY/00000200000Citywide Mayor Citywide EDLevel.csv"), col_names = F)

#### Clean up identifiers ####
l2_identifiers <- l2_identifiers |> 
  select(fips, county, ad, ed, precinct_lookup = precinct.num, precinct_25 = precinct, precinct_cbs)

#### Shapefile merge ####
shapefile_2021 <- shapefile_2021 |> rename(precinct_lookup_2021 = ElectDist)
shapefile_2024 <- shapefile_2024 |> rename(precinct_lookup_2024 = ElectDist)
shapefile_2025 <- shapefile_2025 |> rename(precinct_lookup_2025 = ElectDist)

intersection_21_25 <- st_intersection(shapefile_2021, shapefile_2025)
intersection_24_25 <- st_intersection(shapefile_2024, shapefile_2025)

shape_xwalk_21_25 <- intersection_21_25 |> 
  # create the areal weighting
  mutate(area = st_area(intersection_21_25) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_lookup_2021, precinct_lookup_2025) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(precinct_lookup_2025)) |>
  filter(weight > 0) |>
  select(precinct_lookup_2025, precinct_lookup_2021, weight_21_25 = weight)

shape_xwalk_24_25 <- intersection_24_25 |>
  # create the areal weighting
  mutate(area = st_area(intersection_24_25) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_lookup_2024, precinct_lookup_2025) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(precinct_lookup_2025)) |>
  filter(weight > 0) |>
  select(precinct_lookup_2025, precinct_lookup_2024, weight_24_25 = weight)

crosswalk_24_25 <- l2_identifiers |>
  left_join(shape_xwalk_24_25, by = c("precinct_lookup" = "precinct_lookup_2025")) |>
  select(fips, county, ad, ed, precinct_cbs, precinct_25, precinct_24 = precinct_lookup_2024, weight_24_25)

crosswalk_21_25 <- l2_identifiers |>
  left_join(shape_xwalk_21_25, by = c("precinct_lookup" = "precinct_lookup_2025")) |>
  select(fips, county, ad, ed, precinct_cbs, precinct_25, precinct_21 = precinct_lookup_2021, weight_21_25)

#### Clean up old election results ####
result_columns <- c('AD','ED','County','EDAD Status','Event','Party/Independent Body','Office/Position Title','District Key','VoteFor','Unit Name', 'Tally')

# 2024 POTUS
ny24_potus_clean <- ny24_potus[, -c(1:11)]
colnames(ny24_potus_clean) <- result_columns

ny24_potus_clean <- ny24_potus_clean |>
  janitor::clean_names() |>
  filter(str_detect(unit_name, "Harris|Trump|Scatter") & edad_status == 'IN-PLAY') |>
  mutate(
    candidate_party = case_when(
      str_detect(unit_name, "Harris") ~ "Democrat",
      str_detect(unit_name, "Trump") ~ "Republican",
      str_detect(unit_name, "Scatter") ~ NA),
    candidate_name = case_when(
      str_detect(unit_name, "Harris") ~ "Kamala Harris",
      str_detect(unit_name, "Trump") ~ "Donald Trump",
      str_detect(unit_name, "Scatter") ~ "Write-in"),) |>
  select(ad, ed, candidate_name, candidate_party, tally) |>
  summarise(
    votes_potus_24_dem = sum(tally[candidate_party == "Democrat"], na.rm = T),
    votes_potus_24_rep = sum(tally[candidate_party == "Republican"], na.rm = T),
    votes_potus_24_other = sum(tally[is.na(candidate_party)], na.rm = T),
    votes_precFinal_24 = votes_potus_24_dem + votes_potus_24_rep + votes_potus_24_other,
    .by = c(ad, ed)
  ) |>
  mutate(precinct_24 = glue("{ad}{ed}") |> as.double()) |>
  select(-c(ad, ed))

# 2021 Mayor
ny21_mayor_clean <- ny21_mayor[, -c(1:11)]
colnames(ny21_mayor_clean) <- result_columns    

ny21_mayor_clean <- ny21_mayor_clean |>
  janitor::clean_names() |>
  filter(str_detect(unit_name, "Adams|Sliwa|Pepitone|Bojas|Prussman|Mateo") & edad_status == 'IN-PLAY') |>
  mutate(
    candidate_party = case_when(
      str_detect(unit_name, "Adams") ~ "Democrat",
      str_detect(unit_name, "Sliwa") ~ "Republican",
      TRUE ~ NA_character_),
    candidate_name = case_when(
      str_detect(unit_name, "Adams") ~ "Eric Adams",
      str_detect(unit_name, "Sliwa") ~ "Curtis Sliwa",
      TRUE ~ "Other")) |>
  select(ad, ed, candidate_name, candidate_party, tally) |>
  summarise(
    votes_mayor_21_dem = sum(tally[candidate_party == "Democrat"], na.rm = T),
    votes_mayor_21_rep = sum(tally[candidate_party == "Republican"], na.rm = T),
    votes_mayor_21_other = sum(tally[is.na(candidate_party)], na.rm = T),
    votes_precFinal_21 = votes_mayor_21_dem + votes_mayor_21_rep + votes_mayor_21_other,
    .by = c(ad, ed)
  ) |>
  mutate(precinct_21 = glue("{ad}{ed}") |> as.double()) |>
  select(-c(ad, ed))

#### Build history file ####
history_file_24 <- crosswalk_24_25 |>
  left_join(ny24_potus_clean, by = 'precinct_24') |>
  summarize(
    votes_potus_24_dem = sum(votes_potus_24_dem * weight_24_25, na.rm = T),
    votes_potus_24_rep = sum(votes_potus_24_rep * weight_24_25, na.rm = T),
    votes_precFinal_24 = sum(votes_precFinal_24 * weight_24_25, na.rm = T),
    votePct_potus_24_dem = votes_potus_24_dem / votes_precFinal_24,
    votePct_potus_24_rep = votes_potus_24_rep / votes_precFinal_24,
    .by = c(fips, county, ad,ed,precinct_cbs,precinct_25)
  )

history_file_21 <- crosswalk_21_25 |>
  left_join(ny21_mayor_clean, by = 'precinct_21') |>
  summarize(
    votes_mayor_21_dem = sum(votes_mayor_21_dem * weight_21_25, na.rm = T),
    votes_mayor_21_rep = sum(votes_mayor_21_rep * weight_21_25, na.rm = T),
    votes_precFinal_21 = sum(votes_precFinal_21 * weight_21_25, na.rm = T),
    votePct_mayor_21_dem = votes_mayor_21_dem / votes_precFinal_21,
    votePct_mayor_21_rep = votes_mayor_21_rep / votes_precFinal_21,
    .by = c(fips, county, ad,ed,precinct_cbs,precinct_25)
  )

history_file <- history_file_24 |>
  full_join(history_file_21, by = c("fips", "county", "ad", "ed", "precinct_cbs", "precinct_25")) |>
  mutate(
    across(where(is.numeric), ~ replace_na(.x, 0)),
    vote_mode = 'Total') |>
  relocate(vote_mode, .after = precinct_25) |>
  arrange(fips, county, ad, ed) |>
  left_join(ny25_primary, by = c("ad", "ed", "precinct_cbs"))

#### Save files ####
write_csv(history_file, glue("{DATA_DIR}/25_general/input_data/NY/NY_history.csv"))
write_csv(crosswalk_21_25, glue("{DATA_DIR}/25_general/input_data/NY/NY_21_25_xwalk.csv"))
write_csv(crosswalk_24_25, glue("{DATA_DIR}/25_general/input_data/NY/NY_24_25_xwalk.csv"))
