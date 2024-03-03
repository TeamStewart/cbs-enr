##################################################
## Project: CBS Presidential Primary
## Script purpose: Collect North Carolina
## Date: February 2024
## Author: Joe Loffredo
##################################################

rm(list = ls())
gc()

# Load packages -----------------------------------------------------------
library(tidyverse)
library(data.table)
library(xml2)

# Define constants --------------------------------------------------------
STATE <- "NC"
ELECTION_TYPE <- "primary"

## Retrieving latest timestamps
LAST_UPDATED_TIMESTAMP <- read_xml("https://s3.amazonaws.com/dl.ncsbe.gov?delimiter=/&prefix=ENRS/2024_03_05/") |>
  as_list() |>
  pluck("ListBucketResult", function(x) x[names(x) == "Contents"]) |>
  map(
    ~ tibble(
      Key = pluck(.x, "Key", 1),
      LastModified = pluck(.x, "LastModified", 1)
    )
  ) |>
  list_rbind() |>
  filter(Key == "ENRS/2024_03_05/results_pct_20240305.zip") |>
  pull(LastModified) |>
  ymd_hms(tz = "America/New_York") |>
  str_replace_all("-|:| ", "_")

# Read data ---------------------------------------------------------------
## Precinct-level results available at: https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2024_03_05/results_pct_20240305.zip
precinct_raw <- fread(cmd = "curl -L https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2024_03_05/results_pct_20240305.zip | funzip")


# Prepare output ----------------------------------------------------------
precinct_output <- precinct_raw |>
  mutate(
    state = STATE,
    # Recode candidate party: Democrat, Republican, Libertarian, Green, Independent, Other
    candidate_party = case_when(
      `Choice Party` == "DEM" ~ "Democrat",
      `Choice Party` == "REP" ~ "Republican",
      `Choice Party` == "LIB" ~ "Libertarian",
      `Choice Party` == "GRE" ~ "Green",
      `Choice Party` == "UNA" ~ "Independent",
      TRUE ~ "Other"
    ),
    # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
    # If primary, append election label: str_extract(`Contest Name`, "\\(.*?\\)") -- remove for general
    race_name = case_when(
      str_detect(`Contest Name`, "PRESIDENTIAL PREFERENCE") ~ str_c("President-", candidate_party),
      str_detect(`Contest Name`, "US SENATE") ~ str_c("US Senate-", candidate_party),
      str_detect(`Contest Name`, "US HOUSE OF REPRESENTATIVES") ~ str_c("US House-", str_extract(`Contest Name`, "DISTRICT [0-9]+"), "-", candidate_party),
      str_detect(`Contest Name`, "NC GOVERNOR") ~ str_c("Governor-", candidate_party),
      str_detect(`Contest Name`, "NC HOUSE OF REPRESENTATIVES") ~ str_c("State Legislature-Lower District ", str_extract(`Contest Name`, "[0-9]+"), "-", candidate_party),
      str_detect(`Contest Name`, "NC STATE SENATE") ~ str_c("State Legislature-Upper District ", str_extract(`Contest Name`, "[0-9]+"), "-", )
    ) |> str_replace_all(c("\\(|\\)" = "", "DISTRICT" = "District")),
    # Create virtual precinct column: real == TRUE, administrative == FALSE
    virtual_precinct = `Real Precinct` == "N"
  ) |>
  rename(
    race_id = `Contest Group ID`,
    candidate_name = Choice,
    jurisdiction = County,
    precinct_id = Precinct
  ) |>
  select(
    state, race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, `Election Day`:`Provisional`
  ) |>
  filter(!is.na(race_name)) |>
  pivot_longer(cols = `Election Day`:`Provisional`, names_to = "vote_mode", values_to = "precinct_total") |>
  mutate(
    # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
    vote_mode = case_when(
      vote_mode == "Election Day" ~ "Election Day",
      vote_mode == "Provisional" ~ "Provisional",
      vote_mode == "Absentee by Mail" ~ "Absentee/Mail",
      vote_mode == "One Stop" ~ "Early Voting",
      vote_mode == "Early Voting" ~ "Early Voting",
      TRUE ~ "Other"
    )
  ) |>
  arrange(race_name, candidate_party, candidate_name, jurisdiction)

# Save output -------------------------------------------------------------
## latest file
write_csv(precinct_output, sprintf("data/clean/%s/%s_latest.csv", STATE, ELECTION_TYPE))

## save timestamped version
write_csv(precinct_output, sprintf("data/clean/%s/%s_%s.csv", STATE, ELECTION_TYPE, LAST_UPDATED_TIMESTAMP))
