##################################################
## Project: CBS ENR 2025
## Script purpose: Build VA basefiles
## Date: August 2025
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
shapefile_2021 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/va_2021/2021_NovGeneral.shp'))
shapefile_2024 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/va_2024/_statewide/_statewide.shp'))
shapefile_2025 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/va_2025/_statewide/_statewide.shp'))
va21 <- read_csv(glue("{DATA_DIR}/25_general/input_data/VA/election_results/Virginia_Elections_Database__2021_Governor_General_Election_including_precincts.csv"))
va24 <- read_csv(glue("{DATA_DIR}/25_general/input_data/VA/election_results/va24.csv")) 
l2_identifiers <- read_csv(glue("{DATA_DIR}/25_general/input_data/VA/va_20250917_demo_prec_cd_20250929.csv"))

#### Cleanup results files ####
va24 <- va24 |>
  mutate(
    # Remove everything within and including parentheses
    jurisdiction = if_else(
      !str_detect(jurisdiction, "City"),
      glue("{jurisdiction} County"),
      jurisdiction
    ) |> str_to_upper(),
    precinct_id = str_remove_all(precinct_id, "\\s*\\([^()]*\\)"),
    precinct_id = str_replace_all(precinct_id, " - ", "-"),
    virtual_precinct = ifelse(str_detect(precinct_id, "PROVISIONAL"), TRUE, FALSE)
  )
VA_vote_modes <- va24 |> pull(vote_mode) |> unique() |> str_subset("Post", negate = T)

va21 <- va21 |>
  janitor::clean_names() |>
  mutate(
    state = 'VA',
    jurisdiction = str_to_upper(county_city),
    precinct_id = pct,
    # Make all central voting locations virtual precinct
    virtual_precinct = case_when(
      str_detect(pct, "Central Absentee") ~ TRUE,
      str_detect(pct, "Provisional") ~ TRUE,
      TRUE ~ FALSE
    ),
    # Remove everything inside and including parentheses from precinct_id
    precinct_id = str_remove_all(precinct_id, "\\s*\\([^()]*\\)"),
    precinct_id = str_replace_all(precinct_id, " - ", "-"),
  ) |>
  select(state, jurisdiction, precinct_id, virtual_precinct, glenn_allen_youngkin, terence_richard_mc_auliffe, princess_latece_teira_blanding) |>
  pivot_longer(
    cols = c(glenn_allen_youngkin, terence_richard_mc_auliffe, princess_latece_teira_blanding),
    names_to = "candidate_name",
    values_to = "precinct_total"
  ) |>
  mutate(
    race_id = NA,
    race_name = "Governor",
    candidate_party = case_when(
      candidate_name == "glenn_allen_youngkin" ~ "Republican",
      candidate_name == "terence_richard_mc_auliffe" ~ "Democrat",
      candidate_name == "princess_latece_teira_blanding" ~ "Libertarian",
      TRUE ~ "Other"
    ),
    candidate_name = case_when(
      candidate_name == "glenn_allen_youngkin" ~ "Glenn Youngkin",
      candidate_name == "terence_richard_mc_auliffe" ~ "Terry McAuliffe",
      candidate_name == "princess_latece_teira_blanding" ~ "Princess Blanding",
      TRUE ~ candidate_name
    ),
    vote_mode = case_when(
      str_detect(precinct_id, "^##ab") ~ "Absentee/Mail",
      str_detect(precinct_id, "##ev") ~ "Early Voting",
      str_detect(precinct_id, "##pe") ~ "Provisional",
      str_detect(precinct_id, "Provisional") ~ "Provisional",
      TRUE ~ "Election Day"
    ),
    timestamp = '2025-03-05 09:57:04'
  ) |>
  select(colnames(va24)) |>
  filter(!is.na(jurisdiction)) |>
  mutate(precinct_total = as.numeric(precinct_total)) |>
  summarise(
    precinct_total = sum(precinct_total, na.rm = TRUE),
    .by = c(state, race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, vote_mode, timestamp)
  )

#### Clean up identifiers ####
l2_identifiers <- l2_identifiers |> 
  select(fips, county, precinct_cbs, precinct_l2) |>
  mutate(precinct_lookup = str_replace_all(precinct_l2, " - ","-")) |>
  drop_na(precinct_l2)

#### Cleanup input files ####
create_precinct_identifiers <- function(shapefile){
  shapefile |>
    mutate(
      jurisdiction = str_to_upper(CountyName),
      precinct_id = glue("{str_remove(PrcnctFIPS,'^0+')}-{str_to_upper(PrcnctName)}")
    ) |>
    select(jurisdiction, precinct_id, geometry)
}

shapefile_2024 <- create_precinct_identifiers(shapefile_2024) |> rename(precinct_24 = precinct_id)
shapefile_2025 <- create_precinct_identifiers(shapefile_2025) |> rename(precinct_25 = precinct_id) |>
  mutate(precinct_25 = str_replace_all(precinct_25, "PRECINCT","WARD"))
shapefile_2021 <- sf::st_transform(shapefile_2021, st_crs(shapefile_2024)) |>
  mutate(precinct_21 = str_remove(precinct_n,"^0+"))

#### Join precinct shapefiles ####
intersection_24_25 <- st_intersection(shapefile_2024, shapefile_2025) |> filter(jurisdiction == jurisdiction.1)
shape_xwalk_24_25 <- intersection_24_25 |> 
  # create the areal weighting
  mutate(area = st_area(intersection_24_25) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_24, precinct_25) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(jurisdiction, precinct_25)) |>
  filter(weight > 0) |>
  select(jurisdiction, precinct_25, precinct_24, weight)

intersection_21_25 <- st_intersection(shapefile_2021, shapefile_2025) |> filter(jurisdiction == CountyName)
shape_xwalk_21_25 <- intersection_21_25 |> 
  # create the areal weighting
  mutate(area = st_area(intersection_21_25) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_21, precinct_25) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(jurisdiction, precinct_25)) |>
  filter(weight > 0) |>
  select(jurisdiction, precinct_25, precinct_21, weight)

#### Create crosswalk ####
precinct_id_xwalk <- l2_identifiers |>
  left_join(
    shapefile_2025 |> st_drop_geometry(), 
    by = c("county" = "jurisdiction", "precinct_lookup" = "precinct_25"),
    keep = TRUE)

mismatches <- precinct_id_xwalk |> filter(is.na(precinct_25)) |> select(-c(jurisdiction, precinct_25))

# Fuzzy matching using fastLink #
dfA_temp <- mismatches |>
  select(fips, county, precinct_l2, county, precinct_lookup) |>
  rename(
    jurisdiction = county,
    precinct = precinct_lookup
  )

dfB_temp <- shapefile_2025 |>
  st_drop_geometry() |>
  select(jurisdiction, precinct_25) |>
  rename(precinct = precinct_25) |>
  filter(jurisdiction %in% unique(mismatches$county))

fl_out <- fastLink(
  dfA = dfA_temp,
  dfB = dfB_temp,
  varnames = c("jurisdiction", "precinct"),
  stringdist.match = c("jurisdiction", "precinct"),
  partial.match = c("precinct"),
  threshold.match = 0.85,
  verbose = FALSE
)

matches_df <- getMatches(dfA_temp, dfB_temp, fl_out, threshold.match = 0.85, combine.dfs = FALSE)
dfA_matches <- matches_df$dfA.match
dfB_matches <- matches_df$dfB.match

# Create matched pairs by combining the matched rows
matched_pairs <- dfA_matches |>
  filter(posterior >= 0.85) |>
  mutate(match_id = row_number()) |>
  left_join(
    dfB_matches |> mutate(match_id = row_number()),
    by = "match_id",
    suffix = c("_A", "_B")
  ) |>
  left_join(
    mismatches |> select(fips, county, precinct_l2, precinct_cbs),
    by = c("fips", "precinct_l2")
  ) |>
  transmute(
    fips, county, precinct_l2, precinct_cbs,
    jurisdiction_lookup = jurisdiction_A,
    precinct_lookup = precinct_A,
    jurisdiction = jurisdiction_B,
    precinct_25 = precinct_B
  )

# Update the crosswalk with fuzzy matches
precinct_id_xwalk <- precinct_id_xwalk |>
  filter(!is.na(precinct_25)) |>
  bind_rows(
    matched_pairs |>
      select(fips, county, precinct_l2, precinct_cbs, jurisdiction_lookup, 
             precinct_lookup, jurisdiction, precinct_25)
  )

crosswalk_24_25 <- precinct_id_xwalk |>
  select(-c(precinct_lookup)) |>
  left_join(
    shape_xwalk_24_25,
    by = c("jurisdiction", "precinct_25")
  ) |>
  select(fips, county, precinct_l2, precinct_cbs, precinct_25, precinct_24, weight) |>
  st_drop_geometry() |>
  arrange(county, precinct_l2)

crosswalk_21_25 <- precinct_id_xwalk |>
  select(-c(precinct_lookup)) |>
  left_join(
    shape_xwalk_21_25,
    by = c("jurisdiction", "precinct_25")
  ) |>
  select(fips, county, precinct_l2, precinct_cbs, precinct_25, precinct_21, weight) |>
  st_drop_geometry() |>
  arrange(county, precinct_l2)

# Historical results ------------------------------------------------------
fill_missing_mode <- function(data, target_modes) {
  column_names <- colnames(data)
  
  data |>
    complete(
      vote_mode = target_modes, 
      nesting(state, jurisdiction, precinct_24)) |>
    fill(vote_mode, .direction = "down") |>
    select(all_of(column_names)) |>
    mutate(across(where(is.numeric), ~ replace_na(., 0))) 
}

##### 2024 ##### 
va24_summary <- va24 |>
  filter(race_name == 'President') |>
  mutate(
    votes_precFinal_24 = sum(precinct_total, na.rm = TRUE),
    votes_potus_24_dem = sum(precinct_total * str_detect(candidate_name, "Harris"), na.rm = TRUE),
    votes_potus_24_rep = sum(precinct_total * str_detect(candidate_name, "Trump"), na.rm = TRUE),
    votePct_potus_24_dem = votes_potus_24_dem / sum(precinct_total, na.rm = TRUE),
    votePct_potus_24_rep = votes_potus_24_rep / sum(precinct_total, na.rm = TRUE),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |>
  select(-c(candidate_name, candidate_party, precinct_total)) |>
  rename(precinct_24 = precinct_id) |>
  distinct() |>
  fill_missing_mode(VA_vote_modes) |>
  select(county=jurisdiction, precinct_24, starts_with("vote"))

expansion_grid <- st_drop_geometry(shapefile_2024) |>
  distinct(jurisdiction, precinct_24) |>
  expand_grid(vote_mode = VA_vote_modes) |>
  rename(county = jurisdiction)

history_file_24_25 <- crosswalk_24_25 |>
 # now full-join to ensure all county x precincts are represented in the data
  # so that we can fill in some missigness
  full_join(
    expansion_grid,
    join_by(county, precinct_24),
    relationship = "many-to-many"
  ) |>
  left_join(va24_summary, by = c("county", "precinct_24", "vote_mode")) |>
  # fill in missingness with county mean so we can make estimates
  mutate(
    across(c(votes_potus_24_dem, votes_potus_24_rep, votePct_potus_24_dem, votePct_potus_24_rep, votes_precFinal_24), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votes_potus_24_dem = sum(votes_potus_24_dem * weight),
    votes_potus_24_rep = sum(votes_potus_24_rep * weight),
    votes_precFinal_24 = sum(votes_precFinal_24 * weight),
    votePct_potus_24_dem = sum(votePct_potus_24_dem * weight),
    votePct_potus_24_rep = sum(votePct_potus_24_rep * weight),
    .by = c(county, precinct_25, vote_mode)
  )

history_file_total_24_25 <- history_file_24_25 |>
  summarize(
    votes_potus_24_dem = sum(votes_potus_24_dem, na.rm = TRUE),
    votes_potus_24_rep = sum(votes_potus_24_rep, na.rm = TRUE),
    votes_precFinal_24 = sum(votes_precFinal_24, na.rm = TRUE),
    votePct_potus_24_dem = votes_potus_24_dem / votes_precFinal_24,
    votePct_potus_24_rep = votes_potus_24_rep / votes_precFinal_24,
    .by = c(county, precinct_25)
  ) |>
  mutate(vote_mode = "Total") |>
  select(county, precinct_25, vote_mode, starts_with("votes"), starts_with("votePct"))

history_file_24_25 <- bind_rows(history_file_24_25, history_file_total_24_25) |>
  arrange(county, precinct_25, vote_mode)

##### 2021 #####
va21_summary <- va21 |>
  mutate(
    precinct_21_join = str_extract(precinct_id, "^[^-]+"),
    votes_precFinal_21 = sum(precinct_total, na.rm = TRUE),
    votes_gov_21_dem = sum(precinct_total * str_detect(candidate_name, "McAuliffe"), na.rm = TRUE),
    votes_gov_21_rep = sum(precinct_total * str_detect(candidate_name, "Youngkin"), na.rm = TRUE),
    votePct_gov_21_dem = votes_gov_21_dem / sum(precinct_total, na.rm = TRUE),
    votePct_gov_21_rep = votes_gov_21_rep / sum(precinct_total, na.rm = TRUE),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |>
  select(-c(candidate_name, candidate_party, precinct_total)) |>
  rename(precinct_21 = precinct_id) |>
  distinct() |>
  select(county=jurisdiction, precinct_21, starts_with("vote"), precinct_21_join)

expansion_grid <- st_drop_geometry(shapefile_2021) |>
  distinct(CountyName, precinct_21) |>
  expand_grid(vote_mode = VA_vote_modes) |>
  rename(county = CountyName)

history_file_21_25 <- crosswalk_21_25 |>
  # now full-join to ensure all county x precincts are represented in the data
  # so that we can fill in some missigness
  full_join(
    expansion_grid,
    join_by(county, precinct_21),
    relationship = "many-to-many"
  ) |>
  left_join(va21_summary, by = c("county", "precinct_21"="precinct_21_join", "vote_mode")) |>
  # fill in missingness with county mean so we can make estimates
  mutate(
    across(c(votes_gov_21_dem, votes_gov_21_rep, votePct_gov_21_dem, votePct_gov_21_rep, votes_precFinal_21), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votes_gov_21_dem = sum(votes_gov_21_dem * weight),
    votes_gov_21_rep = sum(votes_gov_21_rep * weight),
    votes_precFinal_21 = sum(votes_precFinal_21 * weight),
    votePct_gov_21_dem = sum(votePct_gov_21_dem * weight),
    votePct_gov_21_rep = sum(votePct_gov_21_rep * weight),
    .by = c(county, precinct_25, vote_mode)
  ) |>
  mutate_all(~ifelse(is.nan(.), NA, .))

history_file_total_21_25 <- history_file_21_25 |>
  summarize(
    votes_gov_21_dem = sum(votes_gov_21_dem, na.rm = TRUE),
    votes_gov_21_rep = sum(votes_gov_21_rep, na.rm = TRUE),
    votes_precFinal_21 = sum(votes_precFinal_21, na.rm = TRUE),
    votePct_gov_21_dem = votes_gov_21_dem / votes_precFinal_21,
    votePct_gov_21_rep = votes_gov_21_rep / votes_precFinal_21,
    .by = c(county, precinct_25)
  ) |>
  mutate(vote_mode = "Total") |>
  select(county, precinct_25, vote_mode, starts_with("votes"), starts_with("votePct"))

history_file_21_25 <- bind_rows(history_file_21_25, history_file_total_21_25) |>
  arrange(county, precinct_25, vote_mode)

# append full identifiers
history_file <- crosswalk_24_25 |>
  select(fips:precinct_25) |>
  distinct() |>
  left_join(history_file_24_25, by = c("county", "precinct_25")) |>
  left_join(history_file_21_25, by = c("county", "precinct_25", "vote_mode")) |>
  select(fips:precinct_cbs, precinct_25, vote_mode, votes_potus_24_dem, votes_potus_24_rep, votes_precFinal_24, votePct_potus_24_dem, votePct_potus_24_rep,
         votes_gov_21_dem, votes_gov_21_rep, votes_precFinal_21, votePct_gov_21_dem, votePct_gov_21_rep) |>
  arrange(county, precinct_25, vote_mode)

write_csv(va24, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2024_long.csv"))
write_csv(va21, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2021_long.csv"))

write_csv(history_file, glue("{DATA_DIR}/25_general/input_data/VA/VA_history.csv"))

write_csv(crosswalk_24_25, glue("{DATA_DIR}/25_general/input_data/VA/VA_24_25_xwalk.csv"))

# Edit crosswalk
results_identifiers <- va21 |>
  select(jurisdiction, precinct_id) |>
  distinct() |>
  mutate(precinct_id_join = str_extract(precinct_id, "^[^-]+"))

crosswalk_21_25 <- crosswalk_21_25 |>
  left_join(
    results_identifiers,
    by = c("county" = "jurisdiction", "precinct_21" = "precinct_id_join"),
    keep = TRUE
  ) |>
  select(fips, county, precinct_l2, precinct_cbs, precinct_25, precinct_21 = precinct_id, weight)

write_csv(crosswalk_21_25, glue("{DATA_DIR}/25_general/input_data/VA/VA_21_25_xwalk.csv"))

va24_wide <- va24 |>
  mutate(
    vote_mode = case_match(
      vote_mode,
      "Early Voting" ~ "early",
      "Election Day" ~ "eday",
      "Absentee/Mail" ~ "mail",
      "Provisional" ~ "provisional",
      .default = 'other'
    )
  ) |>
  pivot_wider(
    names_from = vote_mode, values_from = precinct_total, values_fill = 0, values_fn = sum)

write_csv(va24_wide, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2024_wide.csv"))

va21_wide <- va21 |>
  mutate(
    vote_mode = case_match(
      vote_mode,
      "Early Voting" ~ "early",
      "Election Day" ~ "eday",
      "Absentee/Mail" ~ "mail",
      "Provisional" ~ "provisional",
      .default = 'other'
    )
  ) |>
  pivot_wider(
    names_from = vote_mode, values_from = precinct_total, values_fill = 0, values_fn = sum)

write_csv(va21_wide, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2021_wide.csv"))

