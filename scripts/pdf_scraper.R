# install things of relevance
# install Java for MacOS: https://adoptium.net/
# install.packages("rJava")
# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

# library(tabulizer)
library(tidyverse)

# locate_areas("data/raw/fl/216-official-election-results-details_0.pdf", pages = 2)
# get_page_dims("data/raw/fl/216-official-election-results-details_0.pdf", pages=2)

selects <- "\\d{4}\\sPCT|Hutchinson|Christie|Haley|DeSantis|Binkley|Ramaswamy|Biden|Trump|Stuckenberg|Johnson|Scott|Williamson|Phillips"

get_table <- function(path, p){
  extract_tables(
    path,
    pages = p,
    guess = FALSE
  ) |> 
    map(as_tibble) |> 
    map(~ filter(.x, str_detect(V1, selects))) |> 
    list_rbind() |> 
    mutate(
      precinct_id = str_extract(V1, "^\\d{4}(?=\\s+PCT)")
    ) |> 
    fill(precinct_id, .direction = "down") |> 
    filter(!str_detect(V1, "^\\d{4}(?=\\s+PCT)")) |> 
    mutate(across(everything(), ~ na_if(.x, ""))) |> 
    mutate(V1 = str_remove(V1, " \\..*?$") |> str_squish()) |> 
    bind_rows(
      tibble(V7=character(), V8=character(), V9=character())
    ) |> 
    mutate(
      `Election Day` = coalesce(V7, V8),
      "Early Voting" = str_extract(V3, "(\\.\\d{2})(\\d+)", group = 2)
    ) |> 
    rename(
      candidate_name = V1,
      Provisional = V9,
      "Absentee/Mail" = V6
    ) |> 
    select(-starts_with("V", ignore.case = FALSE)) |> 
    pivot_longer(cols = c(Provisional, `Election Day`, 'Early Voting', 'Absentee/Mail'), 
                 names_to = "vote_mode", values_to = "precinct_total") |> 
    mutate(precinct_total = as.numeric(precinct_total),
           precinct_total = replace_na(precinct_total, 0))
  
  
}

d <- tibble(
  path = "data/raw/fl/216-official-election-results-details_0.pdf",
  p = 1:(get_n_pages(path)-500)
) |> 
  mutate(tbl = map2(path, p, get_table)) |> 
  select(tbl) |> 
  unnest(cols = tbl)
  
