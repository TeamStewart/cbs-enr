rm(list=ls())
gc()

library(tidyverse)
library(targets)
library(httr2)
library(data.table)
library(xml2)

version <- request("https://results.enr.clarityelections.com/GA/113667/current_ver.txt") |> 
  req_headers("Accept" = "application/txt") |> 
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
  req_perform() |> 
  resp_body_string() 

counties <- request(sprintf("https://results.enr.clarityelections.com/GA/113667/%s/json/en/electionsettings.json", version)) |> 
  req_headers("Accept" = "application/json") |> 
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
  req_perform() |> 
  resp_body_json() |>
  pluck("settings", "electiondetails", "participatingcounties") |> 
  as_tibble_col() |> 
  unnest(cols = value) |> 
  separate_wider_delim(cols = value, delim = "|", names = c("county", "sitenum", "version", "timestamp", "unknown")) |> 
  mutate(url = sprintf("https://results.enr.clarityelections.com/GA/%s/%s/%s/reports/detailxml.zip", county, sitenum, version))

county <- "Appling"

request(counties$url[1]) |> 
  req_headers("Accept" = "application/zip") |> 
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
  req_perform(path = sprintf("data/raw/ga_%s.zip", county))

d <- read_xml("data/raw/detail.xml") |> 
  as_list() |> 
  pluck("ElectionResult", function(x) x[names(x) == "Contest"]) |> 
  as_tibble_col()

d |> 
  unnest_longer(value) |> 
  unnest_longer(col = value, names_repair = "unique") |> 
  unnest_longer(col = value)
  
drop_cols <- "Precinct Summary|Electionware Copyright|Summary Results|Joint Primary Election|1-Mar-22|Total Votes Cast|Statistics|Registered Voters|Ballots Cast|Voter Turnout|March 1, 2022"

d <- fread(path, 
           select = c(1, 2, 7, 9, 12),
           skip = 3,
           fill = TRUE,
           blank.lines.skip = TRUE,
           colClasses = list(character=1:12),
           na.strings = "",
           col.names = c("candidate_name", "race_name", "Election Day", "Absentee/Mail", "Early Voting")) |> 
  as_tibble() |> 
  mutate(race_name = na_if(race_name, "Vote For 1")) |> 
  fill(race_name, .direction = "down") |> 
  filter(!(str_detect(candidate_name, drop_cols))) |> 
  mutate(precinct_id = str_extract(candidate_name, "\\d{4}")) |> 
  fill(precinct_id, .direction = "down") |> 
  filter(candidate_name != precinct_id) |> 
  mutate(candidate_party = str_extract(race_name, ".*?\\s") |> str_squish(),
         race_name = str_remove(race_name, candidate_party) |> str_squish(),
         candidate_party = case_match(candidate_party,
                                      "DEM" ~ "Democrat",
                                      "REP" ~ "Republican"),
         jurisdiction = county,
         state = state) |> 
  pivot_longer(cols = c(`Election Day`, `Absentee/Mail`, `Early Voting`), values_to = "precinct_total", names_to = "vote_mode") |> 
  # mutate(race_name = case_when(
  #   str_detect(race_name, "United States President") ~ "President",
  #   str_detect(race_name, "United States Senator") ~ "US Senate",
  #   str_detect(race_name, "United States Representative") ~ "US House",
  #   str_detect(race_name, "Governor") ~ "Governor",
  #   str_detect(race_name, "State Representative") ~ "State Legislature-Lower District",
  #   str_detect(race_name, "State Senator") ~ "State Legislature-Upper District",
  #   .default = race_name
  # )) |> 
  mutate(precinct_total = as.numeric(precinct_total),
         precinct_total = replace_na(precinct_total, 0)) |> 
  mutate(virtual_precinct = FALSE) |> 
  filter(max(precinct_total) > 0, .by = c(race_name, candidate_name, candidate_party)) |> 
  select(state, jurisdiction, precinct_id, race_name, candidate_name, candidate_party, vote_mode, precinct_total, virtual_precinct)