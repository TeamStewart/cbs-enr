rm(list=ls())
gc()

library(tidyverse)
library(rvest)

# start with the "Total" page
base = read_html("https://enr.boenyc.gov/CD26916AD0.html")

timestamp = html_text(base) |> str_extract("Information As Of: (.*?)AD:", group=1) |> ymd_hms(tz = "US/Eastern")

links = tibble(
  link = base |> html_elements("a") |> html_attr("href"),
  ad = base |> html_elements("a") |> html_attr("title")
) |> 
  filter(str_detect(ad, "^AD \\d+$")) |> 
  mutate(
    link = glue::glue("https://enr.boenyc.gov/{link}")
  )

get_ad <- function(link){
  
  t = read_html(link) |> 
    html_element(".underline") |> 
    html_table(head=TRUE) |> 
    janitor::clean_names(case = "title")
  
  t[-1,]
  
}

o = links |> 
  mutate(
    results = map(link, get_ad)
  ) |> 
  select(ad, results) |> 
  unnest_longer(results) |> 
  unnest_wider(results) |> 
  rename(
    precinct = X,
    pct_reported = X_2
  ) |> 
  mutate(
    pct_reported = str_remove(pct_reported, "%") |> as.numeric() / 100,
    precinct = str_squish(precinct)
  ) |> 
  select(-matches("^X_\\d+$")) |> 
  pivot_longer(cols = -c(ad, precinct, pct_reported), names_to = "candidate", values_to = "precinct_total")
  
