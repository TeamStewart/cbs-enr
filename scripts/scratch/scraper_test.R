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

#renv::use_python()

state = 'NC'
county = NA
path = 'http://dl.ncsbe.gov/ENRS/2024_11_05/results_pct_20241105.zip'
timestamp = nc_timestamp()

test = fread(cmd = glue("unzip -p {raw_file_path}")) |>
  # Clean raw file variable names
  clean_names() |>
  # Filter to target contests: President, Governor
  filter(contest_name %in% c('US PRESIDENT', 'NC GOVERNOR')) |>
  mutate(
    timestamp = timestamp,
    state = "NC",
    # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
    contest_name = case_match(
      contest_name,
      "US PRESIDENT" ~ "President",
      "NC GOVERNOR" ~ "Governor"), 
      # Recode candidate party: Democrat, Republican, Libertarian, Constitution, Green, Independent, Justice for All
      choice_party = case_match(
        choice_party,
        "DEM" ~ "Democrat",
        "REP" ~ "Republican",
        "LIB" ~ "Libertarian",
        "CST" ~ "Constitution",
        "GRE" ~ "Green",
        "UNA" ~ "Independent",
        "JFA" ~ "Justice for All",
        .default = "Other"),
    # Recode candidate names
    choice = case_match(
      choice,
      # NC presidential candidates
      "Chase Oliver" ~ "Chase Oliver",
      "Cornel West" ~ "Cornel West",
      "Donald J. Trump" ~ "Donald Trump",
      "Jill Stein" ~ "Jill Stein",
      "Kamala D. Harris" ~ "Kamala Harris",
      "Randall Terry" ~ "Randall Terry",
      "Write-In (Miscellaneous)" ~ "Write-In",
      # NC gubernatorial candidates
      "Josh Stein" ~ "Josh Stein",
      "Mark Robinson" ~ "Mark Robinson",
      "Mike Ross" ~ "Mike Ross",
      "Vinny Smith" ~ "Vinny Smith",
      "Wayne Turner" ~ "Wayne Turner"
      ), 
    # Create virtual precinct column: real == TRUE, administrative == FALSE
    virtual_precinct = (real_precinct == "N")
  ) |>
  rename(
    race_id = contest_group_id,
    race_name = contest_name,
    candidate_name = choice,
    candidate_party = choice_party,
    jurisdiction = county,
    precinct_id = precinct
  ) |>
  select(
    state, race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, election_day:provisional
  ) |>
  pivot_longer(cols = election_day:provisional, names_to = "vote_mode", values_to = "precinct_total") |>
  mutate(
    # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
    vote_mode = case_match(
      vote_mode,
      "election_day" ~ "Election Day",
      "early_voting" ~ "Early Voting",
      "absentee_by_mail" ~ "Absentee/Mail",
      "provisional" ~ "Provisional",
      .default = "Other"
    )
  ) |>
  arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
