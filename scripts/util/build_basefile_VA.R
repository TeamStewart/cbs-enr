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
shapefile_2024 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/va_2024/_statewide/_statewide.shp'))
shapefile_2025 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/va_2025/_statewide/_statewide.shp'))
va24 <- read_csv(glue("{DATA_DIR}/25_general/input_data/VA/election_results/va24.csv")) 
l2_identifiers <- read_csv(glue("{DATA_DIR}/25_general/input_data/VA/va_20250603_demo_prec_20250717.csv"))

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
shapefile_2025 <- create_precinct_identifiers(shapefile_2025) |> rename(precinct_25 = precinct_id)

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

l2_identifiers <- l2_identifiers |> 
  select(fips:precinct_cbs) |>
  mutate(precinct_lookup = str_replace_all(precinct_l2, " - ","-")) |>
  drop_na(precinct_l2)

#### Join precinct shapefiles ####
VA_vote_modes <- va24 |> pull(vote_mode) |> unique() |> str_subset("Post", negate = T)

intersection <- st_intersection(shapefile_2024, shapefile_2025) |> filter(jurisdiction == jurisdiction.1)

shape_xwalk <- intersection |> 
  # create the areal weighting
  mutate(area = st_area(intersection) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_24, precinct_25) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(jurisdiction, precinct_25)) |>
  filter(weight > 0) |>
  select(jurisdiction, precinct_25, precinct_24, weight)

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
  filter(posterior >= 0.98) |>
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
  # Make sure first character of each precinct matches
  filter(str_sub(precinct_A, 1, 1) == str_sub(precinct_B, 1, 1)) |>
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

crosswalk <- precinct_id_xwalk |>
  select(-c(precinct_lookup)) |>
  left_join(
    shape_xwalk,
    by = c("jurisdiction", "precinct_25")
  ) |>
  select(fips, county, precinct_l2, precinct_cbs, precinct_25, precinct_24, weight) |>
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

va24_summary <- va24 |>
  filter(race_name == 'President') |>
  mutate(
    votes_precFinal_24 = sum(precinct_total, na.rm = TRUE),
    votes_24_dem = sum(precinct_total * str_detect(candidate_name, "Harris"), na.rm = TRUE),
    votes_24_rep = sum(precinct_total * str_detect(candidate_name, "Trump"), na.rm = TRUE),
    votePct_24_dem = votes_24_dem / sum(precinct_total, na.rm = TRUE),
    votePct_24_rep = votes_24_rep / sum(precinct_total, na.rm = TRUE),
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

history_file <- crosswalk |>
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
    across(c(votes_24_dem, votes_24_rep, votePct_24_dem, votePct_24_rep, votes_precFinal_24), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votes_24_dem = sum(votes_24_dem * weight),
    votes_24_rep = sum(votes_24_rep * weight),
    votePct_24_dem = sum(votePct_24_dem * weight),
    votePct_24_rep = sum(votePct_24_rep * weight),
    votes_precFinal_24 = sum(votes_precFinal_24 * weight),
    .by = c(county, precinct_25, vote_mode)
  )

# append full identifiers
history_file <- crosswalk |>
  select(fips:precinct_25) |>
  distinct() |>
  left_join(history_file, by = c("county", "precinct_25")) |>
  select(fips:precinct_cbs, precinct_25, vote_mode, starts_with("votes"), starts_with("votePct"))

write_csv(va24, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2024_long.csv"))
write_csv(history_file, glue("{DATA_DIR}/25_general/input_data/VA/VA_history.csv"))
write_csv(crosswalk, glue("{DATA_DIR}/25_general/input_data/VA/VA_xwalk.csv"))

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

