##################################################
## Project: CBS ENR
## Script purpose: Retrieve FL County Links
## Date: June 2024
##################################################

library(tidyverse)
library(rvest)

base_url <- 'https://floridaelectionwatch.gov/CountyReportingStatus'

# Read in the webpage
html <- read_html(base_url)

# prepare lookup table
county_names <- html |> html_elements('.linkCounty') |> html_text() |> str_trim() |> str_squish() |> str_to_upper()
## remove TOTAL as a county name
county_names <- county_names[county_names != 'TOTAL']
county_links <- html |> html_nodes("td.linkCounty a") |> html_attr('href')

write_csv(
  tibble(county = county_names, link = county_links),
  'data/input/FL_county_links.csv'
)
