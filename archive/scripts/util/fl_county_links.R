##################################################
## Project: CBS ENR
## Script purpose: Retrieve FL County Links
## Date: June 2024
##################################################

library(tidyverse)
library(rvest)
library(polite)
library(furrr)

base_url <- 'https://floridaelectionwatch.gov/CountyReportingStatus'

# Read in the webpage
html <- read_html(base_url)

# prepare lookup table
county_names <- html |> html_elements('.linkCounty') |> html_text() |> str_trim() |> str_squish() |> str_to_upper()
## remove TOTAL as a county name
county_names <- county_names[county_names != 'TOTAL']
county_links <- html |> html_nodes("td.linkCounty a") |> html_attr('href')

link_table <- tibble(county = county_names, link = county_links) |>
  filter(!str_detect(link, 'do-not-post-this')) |>
  mutate(
    type = case_when(
      str_detect(link,'enr.electionsfl.org') ~ 'ENR',
      str_detect(link,'clarity') | str_detect(link, 'enr.vote') ~ 'Clarity',
      TRUE ~ 'other'
    )) |>
  filter(type != 'other')

# Retrieve reports link from FL ENR template
get_reports_link <- function(url, type){
  if (type != "ENR") return(url)
  
  html <- bow(url, delay = 5) |> scrape()
  if (is_null(html)) return(NA) 
  
  base_url <- str_extract(url, "https?://enr\\.electionsfl\\.org")
  
  report_links <- html |>
    html_nodes('a') |>
    html_attr('href') |>
    str_trim() |>
    keep(~ str_detect(.x, 'Reports'))
  
  if (length(report_links) > 0) {
    report_url <- str_c(base_url, report_links)
    
    # Check to see if csv exists
    if(is_empty(
      bow(report_url) |> scrape() |> html_node(xpath = '//a[contains(@href, "CandidateResultsbyPrecinctandParty_") and contains(@href, ".csv")]'))
    ) {return(NA)}
    else{
      return(report_url)
    }
    
    return(str_c(base_url, report_links))
  } else {
    return(NA)
  }
}

# Set up parallel processing using furrr
plan(multisession, workers = parallel::detectCores() - 1)

# Parallelize the report link retrieval
link_table <- link_table |>
  mutate(report_link = future_map2_chr(link, type, get_reports_link),
         county = str_replace_all(county,'\\.',""),
         report_link = ifelse(type == 'Clarity', str_extract(report_link,"[0-9]{6}"), report_link)) |>
  drop_na(report_link) |>
  select(county, report_link)

write_csv(link_table, 'data/input/fl_county_links.csv')
