library(data.table)
library(tidyverse)
library(glue)
library(janitor)
library(fs)
library(aws.s3)
library(googledrive)
library(httr2)
library(rvest)
library(reticulate)
library(xml2)
library(jsonlite)

#renv::use_python()

state = 'PA'
county = 'Philadelphia'
path = 'https://philadelphiaresults.azurewebsites.us/ResultsExport.aspx?'
timestamp = pa_timestamp()

path <- read_html(path) |>
  html_nodes(xpath = "//a[contains(text(), '2024 November General Election Results.txt')]") |>
  html_attr('href')

path <- str_c('https://elections.maricopa.gov',path)

# Download file
source_python("scripts/util/dynamic_download.py")
get_file(path, county, state)

# Rename raw file to include identifiers and timestamp
## Get list of raw files
files <- list.files(path = "data/raw/AZ", full.names = TRUE)
## Get file info and sort by modification time in descending order
most_recent_file <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
## Rename file
raw_file_path = glue('data/raw/AZ/az_maricopa_{timestamp}.txt')
file.rename(most_recent_file, raw_file_path)

cleaned <- fread(raw_file_path, header = TRUE, sep = "\t", quote = "") |>
  # clean column names
  clean_names() |> 
  # Filter to target contests: President, Governor
  filter(contest_name %in% c('Presidential Electors', 'US Senate')) |>
  # recode variables
  mutate(
    timestamp = timestamp |> ymd_hms(tz = "America/New_York"),
    state = 'AZ',
    county = 'Maricopa',
    # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
    contest_name = case_match(
      contest_name,
      "Presidential Electors" ~ "President",
      "US Senate" ~ "Senate"),
    # Recode candidate party: Democrat, Republican, Libertarian, Constitution, Green, Independent, Justice for All
    candidate_affiliation = case_match(
      candidate_affiliation,
      "DEM" ~ "Democrat",
      "REP" ~ "Republican",
      "LBT" ~ "Libertarian",
      "GRN" ~ "Green",
      .default = "Other"
    ),
    # Recode candidate names
    candidate_name = case_match(
      candidate_name,
      # AZ presidential candidates
      "OLIVER / TER MAAT" ~ "Chase Oliver",
      "TRUMP / VANCE" ~ "Donald Trump",
      "STEIN / WARE" ~ "Jill Stein",
      "HARRIS / WALZ" ~ "Kamala Harris",
      "Write-in" ~ "Write-ins",
      # AZ senate candidates
      "GALLEGO, RUBEN" ~ "Ruben Gallego",
      "LAKE, KARI" ~ "Kari Lake",
      "QUINTANA, EDUARDO" ~ "Eduardo Quintana",
    ), 
    virtual_precinct = FALSE,
  ) |>
  pivot_longer(cols = c(starts_with("votes_"), "undervotes","overvotes"), 
               names_to = "vote_mode", 
               values_to = "precinct_total") |>
  # Remove the "votes_" prefix from the vote_mode values
  mutate(vote_mode = str_replace(vote_mode, "votes_", "")) |>
  select(
    state, race_id = contest_id, race_name = contest_name , candidate_name,
    candidate_party = candidate_affiliation, jurisdiction = county, precinct_id = precinct_name, 
    virtual_precinct, timestamp, vote_mode, precinct_total)

# Produce precinct-level under/overvote totals
over_under <- cleaned |>
  filter(vote_mode %in% c("undervotes", "overvotes")) |>
  mutate(
    candidate_name = ifelse(vote_mode == "undervotes", "Undervotes", "Overvotes"),
    candidate_party = "",
    vote_mode = "Overvote/Undervote") |>
  summarise(
    precinct_total = sum(precinct_total, na.rm = T), 
    .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","timestamp","vote_mode"))

# Combine cleaned data with over/undervote totals
cleaned |>
  filter(!vote_mode %in% c("undervotes", "overvotes")) |>
  bind_rows(over_under) |>
  arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
  
  
  