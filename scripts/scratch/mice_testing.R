rm(list=ls())
gc()

{
  library(tidyverse)
  library(data.table)
  library(mice)
  library(marginaleffects)
  library(fixest)
  library(brms)
}

get_clarity <- function(state, path){
  
  # Statewide clarity site
  version <- request(glue("https://results.enr.clarityelections.com/{state}/{path}/current_ver.txt")) |> 
    req_headers("Accept" = "application/txt") |> 
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
    req_retry(max_tries = 5) |> 
    req_perform() |> 
    resp_body_string()
  
  counties <- request(glue("https://results.enr.clarityelections.com/{state}/{path}/{version}/json/en/electionsettings.json")) |> 
    req_headers("Accept" = "application/json") |> 
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
    req_retry(max_tries = 5) |> 
    req_perform() |> 
    resp_body_json() |> 
    pluck("settings", "electiondetails", "participatingcounties") |> 
    as_tibble_col() |> 
    unnest(cols = value) |> 
    separate_wider_delim(cols = value, delim = "|", names = c("county", "sitenum", "version", "timestamp", "unknown")) |> 
    mutate(county_url = glue("https://results.enr.clarityelections.com/{state}/{county}/{sitenum}/current_ver.txt")) |> 
    mutate(version = map_chr(county_url, ~ request(.x) |> 
        req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
        req_perform() |> 
        resp_body_string()
    )) |> 
    mutate(url = glue("https://results.enr.clarityelections.com/{state}/{county}/{sitenum}/{version}/json/en/electionsettings.json")) |> 
    select(county, sitenum, timestamp, url) |> 
    mutate(
      version = map(url, ~ request(.x) |> 
          req_headers("Accept" = "application/json") |> 
          req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
          req_retry(max_tries = 5) |> 
          req_perform() |> 
          resp_body_json() |> 
          pluck("versions")
      )
    ) |> 
    unnest_longer(col = version) |> 
    mutate(
      url = glue("https://results.enr.clarityelections.com/{state}/{county}/{sitenum}/{version}/reports/detailxml.zip"),
      raw_file_path = glue("{PATH_DROPBOX}/24_general/{state}/raw/{county}_{version}.zip"),
      downloaded = file_exists(raw_file_path) & file_size(raw_file_path) > 3000
    ) |> 
    filter(
      version == max(version),
      .by = county
    )
  
  download_file <- function(url, version) {
    
    fs::dir_create(glue("{PATH_DROPBOX}/24_general/{state}/raw/"))
    
    tryCatch(
      # Send the request to download the file
      request(url) |> 
        req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
        req_retry(max_tries = 5) |> 
        # Define the path with the `version` suffix for each file
        req_perform(
          path = str_c(
            glue("{PATH_DROPBOX}/24_general/{state}/raw/"),
            str_extract(url, glue("({state}/)(.*?)(/)"), group = 2),
            "_", version, ".zip")),
      # Handle 404 error silently
      httr2_http_404 = function(cnd) NULL
    )
  }
  
  counties |> 
    filter(!downloaded) |> 
    mutate(out = walk2(url, version, download_file))
  
  source_python("scripts/util/clarity_scraper.py")
  
  counties |> pull(raw_file_path) |> walk(.f = \(x) get_data_clarity(state, x, PATH_DROPBOX))
  
}

base = fread("~/Dropbox (MIT)/Research/2024 Election Results/24_general/GA/clean/GA_2024_11_05_20_29_15.csv")[
  race_name == "President",
  list(jurisdiction, precinct_id, vote_mode, precinct_total, candidate_party)
] |> 
  mutate(
    total_votes = sum(precinct_total),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |> 
  mutate(
    vote_pct = precinct_total / total_votes
  ) |> 
  filter(candidate_party == "Democrat") |> 
  select(-candidate_party, -precinct_total, -total_votes)

hist = fread("~/Dropbox (MIT)/Research/2024 Election Results/history/GA_history.csv") |> 
  select(-contains("rep"))

merged = left_join(base, hist, join_by(jurisdiction == county, precinct_id == precinct_24, vote_mode)) |> 
  mutate(
    precinct_total = na_if(precinct_total, 0),
    total_votes = na_if(total_votes, 0),
    vote_pct = na_if(vote_pct, NaN)
  )

init = mice(
  merged,
  # method = "rf",
  # formulas = list(precinct_total ~ jurisdiction + vote_mode + votes_20 + votes_precFinal_20),
  maxit = 0
)

init$predictorMatrix

post <- init$post
post["precinct_total"] = "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(0, max(imp[[j]][, i][r])))"

m = mice(
  merged, 
  # method = "rf",
  # formulas = list(precinct_total ~ jurisdiction + vote_mode + votes_20 + votes_precFinal_20),
  # predictorMatrix = matrix(c(0, 1, 0, 1, 0, 1, 1), nrow=1),
  # method = ifelse(colnames(merged) == "precinct_total", "rf", ""),
  post = post
)

o = with(m, lm(precinct_total ~ votes_20 + vote_mode + jurisdiction + precinct_id))

preds = predictions(o, type = "response") |> as_tibble()

preds

fit <- brm(
  bf(
    precinct_total ~ votes_20_dem + jurisdiction + vote_mode
  ),
  data = m,
  backend = "cmdstanr",
  cores = 4
)

fit <- brm(
  bf(
    precinct_total | mi() ~ votes_20_dem + jurisdiction + vote_mode
  ) + set_rescor(FALSE),
  family = poisson(),
  data = merged,
  backend = "cmdstanr",
  cores = 4
)

complete(m, "stacked") |> 
  arrange(precinct_total) |> 
  slice_head(n=1, by = c(jurisdiction, precinct_id, vote_mode)) |> 
  pull(precinct_total) |> sum() |> prettyNum(big.mark=",")

complete(m, "stacked") |> 
  arrange(desc(precinct_total)) |> 
  slice_head(n=1, by = c(jurisdiction, precinct_id, vote_mode)) |> 
  pull(precinct_total) |> sum() |> prettyNum(big.mark=",")

complete(m, "stacked") |> 
  summarize(
    precinct_total = mean(precinct_total),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |> 
  pull(precinct_total) |> sum() |> prettyNum(big.mark=",")


get_preds <- function(path){
  
  base = fread(path)[
    race_name == "President",
    list(jurisdiction, precinct_id, vote_mode, precinct_total, candidate_party)
  ] |> 
    mutate(
      total_votes = sum(precinct_total),
      .by = c(jurisdiction, precinct_id, vote_mode)
    ) |> 
    mutate(
      vote_pct = precinct_total / total_votes
    ) |> 
    filter(candidate_party == "Democrat") |> 
    select(-candidate_party, -precinct_total, -total_votes)
  
  merged = left_join(base, hist, join_by(jurisdiction == county, precinct_id == precinct_24, vote_mode)) |> 
    mutate(
      vote_pct = na_if(vote_pct, NaN)
    )
  
  m = mice(
    merged, 
    print=FALSE
  )
  
  complete(m, "stacked") |> 
    summarize(
      vote_pct = mean(vote_pct),
      .by = c(jurisdiction, precinct_id, vote_mode)
    )
  
  
}

files = list.files("~/Dropbox (MIT)/Research/2024 Election Results/24_general/GA/clean/", pattern = "\\d\\.csv$", full.names = TRUE)

sims = tibble(
  file = files,
  time = fs::path_file(file) |> str_remove("^GA_") |> ymd_hms(),
) |> 
  filter(time < ymd("2024-11-07")) |> 
  mutate(
    out = map(file, get_preds, .progress = TRUE)
  )


sims |> 
  filter(time < ymd("2024-11-07")) |> 
  ggplot(aes(x = time, y = est)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::label_comma())
