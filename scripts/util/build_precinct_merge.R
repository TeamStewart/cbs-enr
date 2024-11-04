##################################################
## Project: CBS ENR 2024
## Script purpose: Build base file for modeling
## Date: November 2024
##################################################

library(tidyverse)
library(sf)
library(glue)
library(here)
library(httr2)
library(rvest)
library(reticulate)
library(xml2)
library(jsonlite)
library(janitor)

rm(list = ls())
gc()

sf_use_s2(FALSE)
options(scipen = 800)

source(here("scripts", "util", "globals.R"))

fill_missing_mode <- function(data, target_modes) {
  column_names <- colnames(data)
  
  data |>
    complete(
      vote_mode = target_modes, 
      nesting(state, county, precinct_20)) |>
    fill(vote_mode, .direction = "down") |>
    select(all_of(column_names)) |>
    mutate(across(where(is.numeric), ~ replace_na(., 0))) 
}

# Arizona -----------------------------------------------------------------
az_vote_modes <- read_csv("data/clean/AZ/AZ_Maricopa_latest.csv") |> pull(vote_mode) |> unique()

data20 <- read_csv(glue("{PATH_DROPBOX}/MEDSL_2020_precinct/2020-az-precinct-general.csv")) |> 
  filter(stage == "GEN", office == "US PRESIDENT", county_name %in% c('MARICOPA','PIMA')) |> 
  select(county = county_name, precinct_20 = precinct, candidate, vote_mode = mode, votes_20 = votes) |> 
  mutate(
    state = "AZ",
    votes_precFinal_20 = sum(votes_20),
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    vote_mode = case_match(
      vote_mode,
      "ELECTION DAY" ~ "Election Day",
      "EARLY" ~ "Early Voting",
      "PROVISIONAL" ~ "Provisional"
    ),
  ) |>
  select(-c(candidate, votes_20)) |>
  distinct() |>
  fill_missing_mode(az_vote_modes)

# 2020 shapefiles, from VEST data
shp20 <- read_sf(here("data/shapefiles/az_20/")) |> 
  filter(COUNTYFP20 %in% c("013", "019")) |>
  st_transform("NAD83") |>
  mutate(
    county = case_match(
      COUNTYFP20,
      "013" ~ "MARICOPA",
      "019" ~ "PIMA"),
    precinct_20 = case_when(
      county == "MARICOPA" ~ str_c(str_sub(GEOID20, -4), NAME20, sep = " "),
      county == "PIMA" ~ NAME20 |> as.character() |> str_pad(width = 3, side = "left", pad = "0")
    )
  ) |>
  select(precinct_20, county) |>
  st_make_valid() |> 
  drop_na(county)

# 2024 shapefiles, geojson
maricopa_shp24 <- read_sf("data/shapefiles/az_maricopa_24.geojson") |>
  st_transform("NAD83") |>
  mutate(
    county = "MARICOPA",
    precinct_24 = str_c(str_pad(PctNum, 4, "left", "0"), BdName, sep = " "),
  ) |>
  select(precinct_24, county)

pima_shp24 <- read_sf("data/shapefiles/az_pima_24.geojson") |>
  st_transform("NAD83") |>
  mutate(
    county = "PIMA",
    precinct_24 = str_pad(PRECINCT, 3, "left", "0")
  ) |>  
  select(precinct_24, county)

shp24 <- bind_rows(maricopa_shp24, pima_shp24)

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
    expand(shp20, nesting(county, precinct_20), vote_mode = az_vote_modes),
    join_by(county, precinct_20),
    relationship = "many-to-many"
  ) |> 
  # add the matched 2020 data
  left_join(data20, join_by(county, precinct_20, vote_mode), relationship = "many-to-many") |> 
  # fill in missigness with county mean so we can make estimates
  mutate(
    across(c(votes_20_dem, votes_20_rep, votePct_dem_20, votePct_rep_20, votes_precFinal_20), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votePct_dem_20 = sum(votePct_dem_20 * weight),
    votePct_rep_20 = sum(votePct_rep_20 * weight),
    # votePct_diff_20 = sum(votePct_diff_20 * weight),
    votes_precFinal_20 = sum(votes_precFinal_20 * weight),
    votes_20_dem = sum(votes_20_dem * weight),
    votes_20_rep = sum(votes_20_rep * weight),
    .by = c(county, precinct_24, vote_mode)
  )

# Write csv
write_csv(data_history, glue("{PATH_DROPBOX}/history/AZ_history.csv"))

# Georgia -----------------------------------------------------------------
ga_vote_modes <- read_csv("data/clean/GA/GA_latest.csv") |> pull("vote_mode") |> unique()
# MEDSL 2020 precinct data, by mode
## Specifically, this gets Dem totals by vote mode, with column for total votes
data20 <- read_csv(glue("{PATH_DROPBOX}/MEDSL_2020_precinct/2020-ga-precinct-general.csv")) |> 
  filter(stage == "GEN", office == "US PRESIDENT") |> 
  select(county = county_name, precinct_20 = precinct, candidate, vote_mode = mode, votes_20 = votes) |> 
  mutate(
    state = "GA",
    votes_precFinal_20 = sum(votes_20),
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    precinct_20 = case_when(
      .default = precinct_20,
      county == "SPALDING" ~ str_pad(precinct_20, width = 2, side = "left", pad = "0"),
      county == "CHATHAM" ~ str_sub(precinct_20, start = 7)
    ),
    vote_mode = case_match(
      vote_mode,
      "ABSENTEE" ~ "Absentee/Mail",
      "ABSENTEE BY MAIL" ~ "Absentee/Mail",
      "ADVANCED VOTING" ~ "Early Voting",
      "EARLY" ~ "Early Voting",
      "ONE STOP" ~ "Early Voting",
      "ELECTION DAY" ~ "Election Day",
      "PROVISIONAL" ~ "Provisional",
    ),
  ) |> 
  select(-c(candidate, votes_20)) |>
  distinct() |>
  fill_missing_mode(ga_vote_modes)

# 2020 shapefiles, from VEST data
shp20 <- read_sf(here("data/shapefiles/ga_2020/")) |> 
  st_transform("NAD83") |>
  select(
    precinct_20 = PRECINCT_N, county = CTYNAME
  ) |> 
  mutate(
    # change several precinct names to match MEDSL data
    precinct_20 = str_to_upper(precinct_20),
    precinct_20 = case_when(
      .default = precinct_20,
      county == "CHATTAHOOCHEE" ~ str_remove(precinct_20, fixed(" (INCLUDES FTBEN 1-3)")),
      county == "WILKES" & precinct_20 == "TIGNALL SCH LUNCH RM" ~ "TIGNAL SCH LUNCH RM",
      county == "THOMAS" & precinct_20 == "LITTLE OCHLOCKNEE" ~ "LITTLE OCHLOCKNEE BAPTIST CHURCH",
      county == "LAMAR" & precinct_20 == "CHAPPELL MILL VFD" ~ "CHAPPELL MILL V FD",
      county == "COBB" & str_detect(precinct_20, "POWDER SPRINGS") ~ str_replace(precinct_20, "POWDER SPRINGS", "POWDERS SPRINGS"),
      county == "CHATHAM" & precinct_20 == "POOLER REC CENTER GYM" ~ "POOLER RECREATION CENTER GYMNASIUM",
      county == "CHATHAM" & precinct_20 == "RESURRECTION OF OUR LORD CHURCH" ~ "RESUR OF OUR LORD CHURCH",
      county == "GWINNETT" ~ str_remove(precinct_20, "^\\d{3} ")
    )
  ) |> 
  st_make_valid() |> 
  drop_na(county)

# 2022 shapefiles, from the GA legis reapportionment website
shp24 <- read_sf(here("data/shapefiles/ga-precincts2022-shape/")) |> 
  select(county = COUNTY, precinct_24 = PRECINCT_N, geometry) |> 
  drop_na(county) |> 
  mutate(precinct_24 = str_to_upper(precinct_24)) |> 
  mutate(precinct_24 = case_when(
    .default = precinct_24,
    county == "BARROW" ~ str_remove(precinct_24, "^0+"),
    county == "SPALDING" ~ str_remove(precinct_24, "^0+")
  ))

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
    expand(shp20, nesting(county, precinct_20), vote_mode = ga_vote_modes),
    join_by(county, precinct_20),
    relationship = "many-to-many"
  ) |> 
  # add the matched 2020 data
  left_join(data20, join_by(county, precinct_20, vote_mode), relationship = "many-to-many") |> 
  # fill in missigness with county mean so we can make estimates
  mutate(
    across(c(votes_20_dem, votes_20_rep, votePct_dem_20, votePct_rep_20, votes_precFinal_20), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votePct_dem_20 = sum(votePct_dem_20 * weight),
    votePct_rep_20 = sum(votePct_rep_20 * weight),
    # votePct_diff_20 = sum(votePct_diff_20 * weight),
    votes_precFinal_20 = sum(votes_precFinal_20 * weight),
    votes_20_dem = sum(votes_20_dem * weight),
    votes_20_rep = sum(votes_20_rep * weight),
    .by = c(county, precinct_24, vote_mode)
  )

# Write csv
write_csv(data_history, glue("{PATH_DROPBOX}/history/GA_history.csv"))

# Michigan ----------------------------------------------------------------
mi_vote_modes <- c("Absentee/Mail", "Early Voting", "Election Day", "Provisional", "Overvote/Undervote")

## Oakland County -- clarity site
state <- 'MI'; county <- 'Oakland'; path = 105840
source("scripts/scrapers.R")

# Get 2020 from clarity
# Download Clarity files
get_clarity(state, county, path)

# Build list of Clarity files
raw_files <- list.files(path = glue('data/raw/{state}'), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE)

clean_oakland_mi <- function(file){
  cleaned <- read_csv(file) |>
    filter(race_name == "Electors of President and Vice-President of the United States") |>
    mutate(
      state = "MI",
      county = "Oakland",
      timestamp = timestamp |> ymd_hms() |> with_tz(tzone = "America/New_York"),
      # Recode candidate names
      candidate = case_match(
        candidate_name,
        # Oakland, MI presidential candidates
        "Joseph R. Biden/Kamala D. Harris" ~ "BIDEN",
        "Donald J. Trump/Michael R. Pence" ~ "TRUMP",
        .default = 'Other'
      )
    ) |>
    mutate(
      # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
      vote_mode = case_match(
        vote_mode,
        "Election" ~ "Election Day",
        "Absentee" ~ "Absentee/Mail",
        .default = "Provisional"
      )
    ) |>
    rename(precinct_20 = precinct_id) |>
    summarise(
      votes_20 = sum(precinct_total, na.rm = T),
      .by = c("state", "county", "precinct_20", "candidate", "vote_mode", "timestamp"))
  
  file_timestamp <- cleaned |> pull(timestamp) |> unique() |> max() |> str_replace_all("-|:| ", "_")
  
  write_csv(cleaned, file = glue("{PATH_DROPBOX}/misc_precinct_historical/{state}_{county}_{file_timestamp}.csv"))
}

cleaned_files <- lapply(raw_files, clean_oakland_mi)

oakland_data20 <- read_csv(list.files(path = glue("{PATH_DROPBOX}/misc_precinct_historical"), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE) |> max()) |>
  select(-timestamp) |>
  mutate(
    votes_precFinal_20 = sum(votes_20),
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(county, precinct_20, vote_mode)
  ) |>
  select(-c(candidate, votes_20)) |>
  distinct()

## Macomb County
link <- 'https://macombgovdocs.org/clerk/electionresults/2020/November20/5-bd-print.html'
raw_table <- (read_html(link) |> 
                html_nodes("table") |> 
                html_table(fill = TRUE))[[2]] |> 
  as_tibble(.name_repair = function(names) {
    # Replace empty column names with placeholders
    names[names == ""] <- paste0("Unnamed_", seq_along(names[names == ""]))
    make.unique(names)
  }) |>
  select(-c(Unnamed_1, Unnamed_3, `POLL BOOK TOTALS`, ends_with(".2"))) |>
  janitor::clean_names() |>
  rename_with(~ case_when(
    . == "unnamed_2" ~ "precinct",
    str_detect(., "_1$") ~ paste0("electionday_", str_remove(., "_1$")),
    TRUE ~ paste0("absentee_", .)
  )) |>
  filter(precinct != '' & precinct != 'TOTALS') |>
  mutate(township = ifelse(!grepl("^\\d+$", precinct), precinct, NA)) |>  # Identify township rows by non-digit values
  fill(township, .direction = "down") |>  # Fill down the township names
  filter(grepl("^\\d+$", precinct)) |>
  mutate(precinct = str_c(township, precinct, sep = " ")) |>
  select(-township) |>
  pivot_longer(cols = -precinct, names_to = c("vote_mode", "candidate"), names_pattern = "([^_]+)_(.*)", values_to = "votes_20") |>
  mutate(
    candidate = case_when(
      str_detect(candidate, "biden") ~ "BIDEN",
      str_detect(candidate, "trump") ~ "TRUMP",
      TRUE ~ "Other"
    ),
    vote_mode = case_match(
      vote_mode,
      "electionday" ~ "Election Day",
      "absentee" ~ "Absentee/Mail"
    ),
    votes_20 = as.numeric(votes_20)
  ) |>
  summarise(
    votes_20 = sum(votes_20, na.rm = T),
    .by = c(precinct, candidate, vote_mode)) |>
  rename(precinct_20 = precinct) |>
  mutate(
    state = "MI",
    county = "Macomb")

macomb_data20 <- raw_table |>
  mutate(
    votes_precFinal_20 = sum(votes_20),
    .by = c(state, county, precinct_20, vote_mode)
  ) |> 
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(state, county, precinct_20, vote_mode)
  ) |>
  filter(str_detect(candidate, "BIDEN")) |> 
  select(-c(candidate, votes_20)) |>
  distinct() |>
  select(colnames(oakland_data20))
 
## Ingham County
ingham_data20 <- readxl::read_xlsx(glue("{PATH_DROPBOX}/misc_precinct_historical/OFFICIALNovember2020StatementOfVotesCastRPT.xlsx")) |>
  filter(Precinct != "Total" & Precinct != 'Precinct') |>
  select(-c(2,3,10,11)) |>
  mutate(vote_mode = ifelse(Precinct %in% c("Election Day", "AV Counting Boards"), Precinct, NA),
         Precinct = ifelse(Precinct %in% c("Election Day", "AV Counting Boards"), NA, Precinct)) %>%
  # Fill the Precinct column downwards
  fill(Precinct) %>%
  # Filter out rows that do not have a vote_mode (i.e., only precinct labels)
  filter(!is.na(vote_mode)) |>
  pivot_longer(cols = -c(Precinct, vote_mode), names_to = "candidate", values_to = "votes_20") |>
  mutate(
    state = 'MI',
    county = 'Ingham',
    candidate = case_when(
      str_detect(candidate, "Biden") ~ "BIDEN",
      str_detect(candidate, "Trump") ~ "TRUMP",
      TRUE ~ "Other"
    ),
    vote_mode = case_match(
      vote_mode,
      "Election Day" ~ "Election Day",
      "AV Counting Boards" ~ "Absentee/Mail"
    ),
    votes_20 = as.numeric(votes_20)
  ) |>
  clean_names() |>
  rename(precinct_20 = precinct) |>
  summarise(
    votes_20 = sum(votes_20, na.rm = T),
    .by = c(state, county, precinct_20, candidate, vote_mode)) |>
  mutate(
    votes_precFinal_20 = sum(votes_20),
    .by = c(state, county, precinct_20, vote_mode)
  ) |> 
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(state, county, precinct_20, vote_mode)
  ) |>
  select(-c(candidate, votes_20)) |>
  distinct() |>
  select(colnames(oakland_data20))

## Eaton County
state <- 'MI'; county <- 'Eaton'; path = 106278
source("scripts/scrapers.R")

# Get 2020 from clarity
# Download Clarity files
get_clarity(state, county, path)

# Build list of Clarity files
raw_files <- list.files(path = glue('data/raw/{state}'), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE)

clean_eaton_mi <- function(file){
  cleaned <- read_csv(file) |>
    filter(race_name == "Electors of President and Vice-President of the United States") |>
    mutate(
      state = "MI",
      county = "Eaton",
      timestamp = timestamp |> ymd_hms() |> with_tz(tzone = "America/New_York"),
      # Recode candidate names
      candidate = case_match(
        candidate_name,
        # Oakland, MI presidential candidates
        "Joseph R. Biden/Kamala D. Harris" ~ "BIDEN",
        "Donald J. Trump/Michael R. Pence" ~ "TRUMP",
        .default = 'Other'
      )
    ) |>
    mutate(
      # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
      vote_mode = case_match(
        vote_mode,
        "Election" ~ "Election Day",
        "Absentee" ~ "Absentee/Mail",
        .default = "Provisional"
      )
    ) |>
    rename(precinct_20 = precinct_id) |>
    summarise(
      votes_20 = sum(precinct_total, na.rm = T),
      .by = c("state", "county", "precinct_20", "candidate", "vote_mode", "timestamp")) 
  
  file_timestamp <- cleaned |> pull(timestamp) |> unique() |> max() |> str_replace_all("-|:| ", "_")
  
  write_csv(cleaned, file = glue("{PATH_DROPBOX}/misc_precinct_historical/{state}_{county}_{file_timestamp}.csv"))
}

cleaned_files <- lapply(raw_files, clean_eaton_mi)

eaton_data20 <- read_csv(list.files(path = glue("{PATH_DROPBOX}/misc_precinct_historical"), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE) |> max()) |>
  select(-timestamp) |>
  mutate(
    votes_precFinal_20 = sum(votes_20),
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(county, precinct_20, vote_mode)
  ) |>
  select(-c(candidate, votes_20)) |>
  distinct()

# 2020 shapefiles, from VEST data
township_lookup <- tigris::county_subdivisions(state = "MI", year = 2020) |>
  mutate(NAMELSAD = str_to_title(NAMELSAD)) |>
  select(COUNTYFIPS = COUNTYFP, MCDFIPS = COUSUBFP, NAMELSAD) |>
  st_drop_geometry()

shp20 <- read_sf("data/shapefiles/MI_2020_Voting_Precincts/") |> 
  st_transform("NAD83") |>
  left_join(township_lookup, by = c("COUNTYFIPS", "MCDFIPS")) |>
  filter(COUNTYFIPS %in% c('125','099','065','045')) |>
  mutate(
    county = case_match(
      COUNTYFIPS,
      "125" ~ "Oakland",
      "099" ~ "Macomb",
      "065" ~ "Ingham",
      "045" ~ "Eaton"),
    NAMELSAD = case_when(
      county %in% c('Eaton', 'Ingham') ~ str_replace(NAMELSAD, "(.*) City$", "City of \\1"),
      county == 'Macomb' ~ str_to_upper(NAMELSAD) |> str_remove_all(" CITY"),
      TRUE ~ NAMELSAD),
    PRECINCT = str_replace(PRECINCT, "^0+", ""),
    PRECINCT = ifelse(county == 'Macomb', str_pad(PRECINCT, 2, pad = "0"), PRECINCT),
    WARD = str_replace(WARD, "^0+", ""),
    precinct_20 = case_when(
      county == 'Macomb' ~ glue("{NAMELSAD} {PRECINCT}"),
      WARD != '' ~ glue("{NAMELSAD}, Ward {WARD}, Precinct {PRECINCT}"),
      TRUE ~ glue("{NAMELSAD}, Precinct {PRECINCT}"))
    ) |>
  select(precinct_20, county) |> 
  st_make_valid() |> 
  drop_na(county)

# 2022 shapefiles, from the GA legis reapportionment website
shp24 <- read_sf("data/shapefiles/MI_2024_Voting_Precincts/") |> 
  st_transform("NAD83") |>
  filter(COUNTYFIPS %in% c('125','099','065','045')) |>
  mutate(
    county = case_match(
      COUNTYFIPS,
      "125" ~ "Oakland",
      "099" ~ "Macomb",
      "065" ~ "Ingham",
      "045" ~ "Eaton")
  ) |>
  select(
    precinct_24 = Precinct_L, county
  ) |> 
  st_make_valid() |> 
  drop_na(county)

data20 <- bind_rows(oakland_data20, macomb_data20, ingham_data20, eaton_data20) |>
  fill_missing_mode(mi_vote_modes)

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
    across(c(votes_20_dem, votes_20_rep, votePct_dem_20, votePct_rep_20, votes_precFinal_20), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votePct_dem_20 = sum(votePct_dem_20 * weight),
    votePct_rep_20 = sum(votePct_rep_20 * weight),
    # votePct_diff_20 = sum(votePct_diff_20 * weight),
    votes_precFinal_20 = sum(votes_precFinal_20 * weight),
    votes_20_dem = sum(votes_20_dem * weight),
    votes_20_rep = sum(votes_20_rep * weight),
    .by = c(county, precinct_24, vote_mode)
  ) |>
  mutate(
    # last fixes for getting data to join
    precinct_24 = case_when(
      county == 'Macomb' ~ str_replace_all(precinct_24, c("Township" = "Twp", "City of "="", "Precinct" = "Pct")) |>
        str_trim() |> str_squish(),
      county == 'Oakland' ~ str_replace_all(
        precinct_24, 
        c(
          "^(Township of|Charter Township of) (\\w+), Precinct (\\d+)$" = "\\2 Township, Precinct \\3",
          "^City of " = ""
        )
      ),
      TRUE ~ precinct_24
    )
  )

# Write csv
write_csv(data_history, glue("{PATH_DROPBOX}/history/MI_history.csv"))


# North Carolina ----------------------------------------------------------
nc_vote_modes <- read_csv("data/clean/NC/NC_latest.csv") |> pull(vote_mode) |>
# MEDSL 2020 precinct data, by mode
## Specifically, this gets Dem totals by vote mode, with column for total votes
data20 <- read_csv(glue("{PATH_DROPBOX}/MEDSL_2020_precinct/2020-nc-precinct-general.csv")) |> 
  filter(stage == "GEN", office == "US PRESIDENT") |> 
  select(county = county_name, precinct_20 = precinct, candidate, vote_mode = mode, votes_20 = votes) |> 
  mutate(
    state = "NC",
    votes_precFinal_20 = sum(votes_20),
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
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
  select(-c(candidate, votes_20)) |>
  distinct() |>
  fill_missing_mode(nc_vote_modes)

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
    across(c(votes_20_dem, votes_20_rep, votePct_dem_20, votePct_rep_20, votes_precFinal_20), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votePct_dem_20 = sum(votePct_dem_20 * weight),
    votePct_rep_20 = sum(votePct_rep_20 * weight),
    # votePct_diff_20 = sum(votePct_diff_20 * weight),
    votes_precFinal_20 = sum(votes_precFinal_20 * weight),
    votes_20_dem = sum(votes_20_dem * weight),
    votes_20_rep = sum(votes_20_rep * weight),
    .by = c(county, precinct_24, vote_mode)
  )


# Write csv
write_csv(data_history, glue("{PATH_DROPBOX}/history/NC_history.csv"))

# Pennsylvania ------------------------------------------------------------
pa_vote_modes <- c(
  read_csv("data/clean/PA/PA_Allegheny_latest.csv") |> pull(vote_mode),
  read_csv("data/clean/PA/PA_Delaware_latest.csv") |> pull(vote_mode),
  read_csv("data/clean/PA/PA_Philadelphia_latest.csv") |> pull(vote_mode)
) |> unique()

#### Philadelphia ####
philly_20 <- readxl::read_excel(glue("{PATH_DROPBOX}/misc_precinct_historical/PA_Philadelphia_2020.xlsx")) |>
  clean_names() |> 
  filter(div != 'TOTAL') |>
  select(div, type, starts_with("president_and")) |>
  pivot_longer(cols = starts_with("president_and"), names_to = "candidate", values_to = "votes_20") |>
  mutate(
    state = 'PA',
    county = 'Philadelphia',
    type = case_match(
      type,
      "E" ~ "Election Day",
      "M" ~ "Absentee/Mail",
      "P" ~ "Provisional",
    ),
    candidate = case_match(
      candidate,
      "president_and_vice_president_of_the_united_states_joseph_r_biden_and_kamala_d_harris" ~ "BIDEN",
      "president_and_vice_president_of_the_united_states_donald_j_trump_and_michael_r_pence" ~ "TRUMP",
      .default = "Other"
    ),
  ) |>
  rename(
    precinct_20 = div,
    vote_mode = type
  ) |>
  mutate(
    state = "PA",
    votes_precFinal_20 = sum(votes_20),
    .by = c(county, precinct_20, vote_mode)
  ) |>
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  select(-c(candidate, votes_20)) |>
  distinct() |>
  fill_missing_mode(pa_vote_modes)

# 2020 shapefiles, from PA Open Data
shp20 <- read_sf("data/shapefiles/PhillyPlanning_Political_Divisions2020/") |> 
  st_transform("NAD83") |>
  mutate(
    precinct_20 = str_sub(DIVISION_N, 1, 2) |> paste0("-", str_sub(DIVISION_N, 3, 4)),
    county = 'Philadelphia') |>
  select(precinct_20, county) |> 
  st_make_valid()

# 2024 shapefiles, from City of Philadelphia
shp24 <- read_sf("data/shapefiles/Philadelphia_2024/") |> 
  st_transform("NAD83") |>
  mutate(
    precinct_24 = str_sub(DIVISION_N, 1, 2) |> paste0("-", str_sub(DIVISION_N, 3, 4)),
    county = 'Philadelphia') |>
  select(precinct_24, county) |> 
  st_make_valid()

# merge the shapes together so that we can compute an overlap score
intersection <- st_intersection(shp24, shp20) |> filter(county == county.1)

philly_data_history <- intersection |> 
  # create the areal weighting
  mutate(area = st_area(intersection) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_20, precinct_24) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(county, precinct_24)) |> 
  # now full-join to ensure all county x precincts are represented in the data
  # so that we can fill in some missigness
  full_join(
    expand(shp20, nesting(county, precinct_20), vote_mode = pa_vote_modes),
    join_by(county, precinct_20),
    relationship = "many-to-many"
  ) |> 
  # add the matched 2020 data
  left_join(philly_20, join_by(county, precinct_20, vote_mode), relationship = "many-to-many") |> 
  # fill in missigness with county mean so we can make estimates
  mutate(
    across(c(votes_20_dem, votes_20_rep, votePct_dem_20, votePct_rep_20, votes_precFinal_20), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votePct_dem_20 = sum(votePct_dem_20 * weight),
    votePct_rep_20 = sum(votePct_rep_20 * weight),
    # votePct_diff_20 = sum(votePct_diff_20 * weight),
    votes_precFinal_20 = sum(votes_precFinal_20 * weight),
    votes_20_dem = sum(votes_20_dem * weight),
    votes_20_rep = sum(votes_20_rep * weight),
    .by = c(county, precinct_24, vote_mode)
  ) |>
  mutate(county = str_to_title(county))

#### Delaware ####
delaware_20 <- read_csv(glue("{PATH_DROPBOX}/misc_precinct_historical/20201103__pa__general__precinct.csv")) |>
  filter(county == 'Delaware' & office == 'President') |>
  select(-c(district, absentee, military, extra, votes, party)) |>
  mutate(precinct_20 = str_remove(precinct, "-.*") |> str_trim() |> str_squish()) |>
  pivot_longer(cols = c(election_day, mail, provisional), names_to = "vote_mode", values_to = "votes_20") |>
  mutate(
    state = 'PA',
    county = 'Delaware',
    vote_mode = case_match(
      vote_mode,
      "election_day" ~ "Election Day",
      "mail" ~ "Absentee/Mail",
      "provisional" ~ "Provisional"
    ),
    candidate = case_match(
      candidate,
      "Joseph R. Biden" ~ "BIDEN",
      "Donald J. Trump" ~ "TRUMP",
      .default = "Other"
    ),
  ) |>
  summarise(votes_20 = sum(votes_20), .by = c(state, county, precinct_20, candidate, vote_mode)) |>
  mutate(
    votes_precFinal_20 = sum(votes_20),
    .by = c(county, precinct_20, vote_mode)
  ) |>
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  select(-c(candidate, votes_20)) |>
  distinct() |>
  fill_missing_mode(pa_vote_modes)

# 2020 shapefiles, from PA Open Data
shp20 <- read_sf("data/shapefiles/pa_2020/") |> 
  st_transform("NAD83") |>
  filter(COUNTYFP == '045') |>
  mutate(
    county = 'Delaware') |>
  select(precinct_20 = NAME, county) |> 
  st_make_valid()

# 2024 shapefiles, from City of Philadelphia
shp24 <- read_sf("data/shapefiles/Delaware_2024/") |> 
  st_transform("NAD83") |>
  mutate(
    county = 'Delaware') |>
  select(precinct_24 = name, county) |> 
  st_make_valid()

# merge the shapes together so that we can compute an overlap score
intersection <- st_intersection(shp24, shp20)

delaware_name_xwalk <- read_csv(glue("{PATH_DROPBOX}/misc_precinct_historical/delaware_precinct_xwalk.csv"))

delaware_data_history <- intersection |> 
  # create the areal weighting
  mutate(area = st_area(intersection) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_20, precinct_24) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(county, precinct_24)) |> 
  # now full-join to ensure all county x precincts are represented in the data
  # so that we can fill in some missigness
  full_join(
    expand(shp20, nesting(county, precinct_20), vote_mode = pa_vote_modes),
    join_by(county, precinct_20),
    relationship = "many-to-many"
  ) |> 
  # fix precinct_names
  mutate(
    precinct_20 = case_when(
      str_detect(precinct_20, "-") ~ str_replace(precinct_20, "(.*) (\\d+)-(\\d+)", "\\1 \\2D \\3P"),
      TRUE ~ str_replace(precinct_20, "(.*) (\\d+)", "\\1 \\2P")
    )
  ) |> 
  # add the matched 2020 data
  left_join(delaware_20, join_by(county, precinct_20, vote_mode), relationship = "many-to-many") |> 
  # fill in missigness with county mean so we can make estimates
  mutate(
    across(c(votes_20_dem, votes_20_rep, votePct_dem_20, votePct_rep_20, votes_precFinal_20), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votePct_dem_20 = sum(votePct_dem_20 * weight),
    votePct_rep_20 = sum(votePct_rep_20 * weight),
    # votePct_diff_20 = sum(votePct_diff_20 * weight),
    votes_precFinal_20 = sum(votes_precFinal_20 * weight),
    votes_20_dem = sum(votes_20_dem * weight),
    votes_20_rep = sum(votes_20_rep * weight),
    .by = c(county, precinct_24, vote_mode)
  ) |>
  mutate(county = str_to_title(county)) |>
  left_join(delaware_name_xwalk, by = c("precinct_24" = "shp")) |>
  mutate(precinct_24 = cleaned) |>
  select(-cleaned) |>
  select(county, precinct_24, everything())

#### Allegheny ####
state = 'PA'
county = 'Allegheny'
path = 106267
source("scripts/scrapers.R")

# Get 2020 from clarity
# Download Clarity files
get_clarity(state, county, path)

# Build list of Clarity files
raw_files <- list.files(path = glue('data/raw/{state}'), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE)

clean_allegheny_pa <- function(file){
  cleaned <- read_csv(file) |>
    filter(race_name == "Presidential Electors") |>
    mutate(
      state = "PA",
      county = "Allegheny",
      timestamp = timestamp |> ymd_hms() |> with_tz(tzone = "America/New_York"),
      # Recode candidate names
      candidate = case_match(
        candidate_name,
        # Oakland, MI presidential candidates
        "DEM Joseph R. Biden/Kamala D. Harris" ~ "BIDEN",
        "REP Donald J. Trump/Mike R. Pence" ~ "TRUMP",
        .default = 'Other'
      )
    ) |>
    mutate(
      # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
      vote_mode = case_match(
        vote_mode,
        "Election Day" ~ "Election Day",
        "Absentee" ~ "Absentee/Mail",
        .default = "Provisional"
      )
    ) |>
    rename(precinct_20 = precinct_id) |>
    summarise(
      votes_20 = sum(precinct_total, na.rm = T),
      .by = c("state", "county", "precinct_20", "candidate", "vote_mode", "timestamp")) 
  
  file_timestamp <- cleaned |> pull(timestamp) |> unique() |> max() |> str_replace_all("-|:| ", "_")
  
  write_csv(cleaned, file = glue("{PATH_DROPBOX}/misc_precinct_historical/{state}_{county}_{file_timestamp}.csv"))
}

cleaned_files <- lapply(raw_files, clean_allegheny_pa)

# Return latest timestamped version
allegheny_20 <- read_csv(list.files(path = glue("{PATH_DROPBOX}/misc_precinct_historical"), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE) |> max()) |>
  select(-timestamp) |>
  mutate(
    votes_precFinal_20 = sum(votes_20),
    .by = c(county, precinct_20, vote_mode)
  ) |> 
  mutate(
    votes_20_dem = sum(votes_20 * str_detect(candidate, "BIDEN")),
    votes_20_rep = sum(votes_20 * str_detect(candidate, "TRUMP")),
    votePct_dem_20 = sum(votes_20 * str_detect(candidate, "BIDEN")) / sum(votes_20),
    votePct_rep_20 = sum(votes_20 * str_detect(candidate, "TRUMP")) / sum(votes_20),
    # votePct_diff_20 = votePct_dem_20 - votePct_rep_20,
    .by = c(county, precinct_20, vote_mode)
  ) |>
  select(-c(candidate, votes_20)) |>
  distinct() |>
  fill_missing_mode(pa_vote_modes)

# 2020 shapefiles, from PA Open Data
shp20 <- read_sf("data/shapefiles/pa_2020/") |> 
  st_transform("NAD83") |>
  filter(COUNTYFP == '003') |>
  mutate(
    county = 'Allegheny') |>
  select(precinct_20 = NAME, county) |> 
  st_make_valid()

# 2024 shapefiles, from City of Philadelphia
shp24 <- read_sf("data/shapefiles/Allegheny_2024/") |> 
  st_transform("NAD83") |>
  mutate(
    county = 'Allegheny') |>
  select(precinct_24 = prcnct_, county) |> 
  st_make_valid()

# merge the shapes together so that we can compute an overlap score
intersection <- st_intersection(shp24, shp20)

allegheny_data_history <- intersection |> 
  # create the areal weighting
  mutate(area = st_area(intersection) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_20, precinct_24) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(county, precinct_24)) |> 
  # now full-join to ensure all county x precincts are represented in the data
  # so that we can fill in some missigness
  full_join(
    expand(shp20, nesting(county, precinct_20), vote_mode = pa_vote_modes),
    join_by(county, precinct_20),
    relationship = "many-to-many"
  ) |> 
  # fix precinct_names
  mutate(
    precinct_20 = case_when(
      str_detect(precinct_20, "-") ~ str_replace(precinct_20, "(.*) (\\d+)-(\\d+)", "\\1 \\2D \\3P"),
      TRUE ~ str_replace(precinct_20, "(.*) (\\d+)", "\\1 \\2P")
    ),
    precinct_20 = str_replace(precinct_20, " TP ", " TWP ")
  ) |> 
  # add the matched 2020 data
  left_join(allegheny_20, join_by(county, precinct_20, vote_mode), relationship = "many-to-many") |> 
  # fill in missigness with county mean so we can make estimates
  mutate(
    across(c(votes_20_dem, votes_20_rep, votePct_dem_20, votePct_rep_20, votes_precFinal_20), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votePct_dem_20 = sum(votePct_dem_20 * weight),
    votePct_rep_20 = sum(votePct_rep_20 * weight),
    # votePct_diff_20 = sum(votePct_diff_20 * weight),
    votes_precFinal_20 = sum(votes_precFinal_20 * weight),
    votes_20_dem = sum(votes_20_dem * weight),
    votes_20_rep = sum(votes_20_rep * weight),
    .by = c(county, precinct_24, vote_mode)
  ) |>
  mutate(county = str_to_title(county))

data_history <- bind_rows(allegheny_data_history, philly_data_history, delaware_data_history)

# Write csv
write_csv(data_history, glue("{PATH_DROPBOX}/history/PA_history.csv"))


# Testing -----------------------------------------------------------------
zero_file <- read_csv(glue("{PATH_DROPBOX}/24_general/MI/cl"))

# test <- zero_file |> select(precinct_id) |> distinct() |> 
#   anti_join(allegheny_data_history, by = c("precinct_id" = "precinct_24"))
