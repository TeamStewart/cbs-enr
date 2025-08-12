rm(list=ls())
gc()

{
  library(tidyverse)
  library(data.table)
  # library(tidymodels)
  library(marginaleffects)
}

########################################
# setup history files
# s
# this creates a tibble with the following columns:
# - `county`
# - `precinct_24`, the name of the 2024 precinct
# - `vote_mode`
# - `candidate_party`, the party of the candidate used as the reference category for modeling.
#   in most cases, this is just the same office as the one of interest
# - `vote_hist`, the number of votes cast for this county x precinct_24 x vote_mode x candidate_party
# - `tot_2party_hist`, the total votes cast in this precinct x vote_mode, for only Republicans & Democrats.
#   often used for weighting
# - `pct_2party_hist`, what percentage of `tot_2party_hist` was cast for the `candidate_party` in this county x precinct_24 x vote_mode unit
########################################

hist = fread("~/Dropbox (MIT)/Research/2024 Election Results/history/GA_history.csv") |> 
  mutate(
    precinct_24 = str_to_lower(precinct_24),
  ) |> 
  summarize(
    across(votes_20_dem:votes_precFinal_20, sum), 
    .by = c(county, precinct_24, vote_mode)
  ) |> 
  pivot_longer(cols = c(votes_20_dem, votes_20_rep), names_to = "candidate_party", values_to = "precinct_total") |> 
  mutate(
    precinct_24 = ifelse(county == "Candler", "jack strickland community center", precinct_24),
    candidate_party = case_when(
      str_detect(candidate_party, "_dem") ~ "Democrat",
      str_detect(candidate_party, "_rep") ~ "Republican"
    )
  ) |> 
  select(-votePct_dem_20, -votePct_rep_20) |>
  mutate(
    tot_2party_hist = sum(precinct_total),
    pct_2party_hist = precinct_total / sum(precinct_total),
    .by = c(county, precinct_24, vote_mode)
  ) |> 
  select(
    county, precinct_24, vote_mode, candidate_party, 
    vote_hist = precinct_total, tot_2party_hist, pct_2party_hist
  ) |> 
  distinct()

########################################
# now work with the 'incoming raw data'
########################################

base = fread("/Users/mason/Dropbox (MIT)/Research/2024 Election Results/24_general/GA/clean/GA_2024_11_05_19_24_56.csv")[
  race_name == "President",
  list(jurisdiction, precinct_id, vote_mode, precinct_total, candidate_party, candidate_name)
] |> 
  summarize(
    precinct_total = sum(precinct_total, na.rm=TRUE),
    .by = c(jurisdiction, precinct_id, vote_mode, candidate_party)
  ) |>
  mutate(
    precinct_id = str_to_lower(precinct_id),
    vote_mode = as_factor(vote_mode),
    precinct_total = ifelse(precinct_total == 0 & vote_mode != "Provisional", NA, precinct_total)
  ) |> 
  # join the history files
  left_join(
    hist, 
    join_by(jurisdiction == county, precinct_id == precinct_24, vote_mode, candidate_party),
  ) |> 
  rename(
    # rename to match the history file formatting
    vote_enr = precinct_total
  ) |> 
  mutate(
    # total votes cast in all modes for all candidates, used for weighting
    tot_enr = sum(vote_enr),
    .by = c(jurisdiction, precinct_id)
  ) |>
  # now, filter down to just Rs and Ds for two party calculations
  filter(candidate_party %in% c("Democrat", "Republican")) |>
  mutate(
    tot_2party_enr = sum(vote_enr),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |>
  mutate(
    pct_2party_enr = vote_enr / tot_2party_enr,
  ) |>
  # only do prediction on Democrats
  filter(candidate_party == "Democrat")

generate_predictions <- function(path) {
  
  base = fread(path)[
    race_name == "President",
    list(jurisdiction, precinct_id, vote_mode, precinct_total, candidate_party)
  ] |> 
    # sometimes there are mixups, this unifies into the expected dataframe. mostly applies to third-party candidates
    # who are grouped together in this step
    summarize(
      precinct_total = sum(precinct_total, na.rm=TRUE),
      .by = c(jurisdiction, precinct_id, vote_mode, candidate_party)
    ) |>
    mutate(
      precinct_id = str_to_lower(precinct_id),
    ) |> 
    # join the history files
    left_join(
      hist, 
      join_by(jurisdiction == county, precinct_id == precinct_24, vote_mode, candidate_party),
    ) |> 
    mutate(
      precinct_total = case_when(
        .default = precinct_total,
        precinct_total == 0 & vote_mode != "Provisional" ~ NA,
        (precinct_total / vote_hist) < 0.1 & vote_mode == "Absentee/Mail" ~ NA
      )
    ) |> 
    rename(
      # rename to match the history file formatting
      vote_enr = precinct_total
    ) |> 
    mutate(
      # total votes cast in all modes for all candidates, used for weighting
      tot_enr = sum(vote_enr),
      .by = c(jurisdiction, precinct_id)
    ) |>
    # now, filter down to just Rs and Ds for two party calculations
    filter(candidate_party %in% c("Democrat", "Republican")) |>
    mutate(
      tot_2party_enr = sum(vote_enr),
      .by = c(jurisdiction, precinct_id, vote_mode)
    ) |>
    mutate(
      pct_2party_enr = vote_enr / tot_2party_enr,
    )
  
  modes = base |> 
    drop_na(vote_enr) |> 
    count(vote_mode) |> 
    filter(n>1) |> 
    pull() |> 
    sort()
  
  all_modes = identical(modes, distinct(base, vote_mode) |> pull() |> sort())
  
  if (all_modes) {
    lm_fit <- lm((pct_2party_enr-pct_2party_hist) ~ pct_2party_hist + vote_mode + candidate_party, data = base)
  } else {
    lm_fit <- lm((pct_2party_enr-pct_2party_hist) ~ pct_2party_hist + candidate_party, data = base)
  }
  
  # lm_fit <- glm(vote_enr ~ vote_hist + vote_mode, data = base, family = "poisson")
  
  # rf_fit <-
  # rand_forest(trees = 1000, mtry = .preds(), min_n = 10) |>
  # set_mode("regression") |>
  # set_engine("ranger", keep.inbag = TRUE) |> 
  # linear_reg(mode = "regression") |>
  # fit(
  #   vote_enr ~ vote_hist + vote_mode,
  #   data = base
  # )
  
  meanFill = base |>
    mutate(
      across(
        c(pct_2party_hist, vote_hist, tot_2party_hist),
        ~ ifelse(is.na(.x), median(.x, na.rm=TRUE), .x)
      ),
      .by = jurisdiction
    ) |> 
    mutate(
      across(
        c(pct_2party_hist, vote_hist, tot_2party_hist),
        ~ ifelse(is.na(.x), median(.x, na.rm=TRUE), .x)
      )
    )
  
  preds = predictions(lm_fit, newdata = meanFill, vcov = ~ jurisdiction) |> 
    inferences(method = "delta") |>
    as_tibble()
  
  pred_missing = preds |> 
    filter(is.na(vote_enr)) |> 
    select(estimate, conf.low, conf.high, vote_mode, tot_2party_hist, pct_2party_hist, vote_hist, candidate_party) |> 
    mutate(
      vote_enr_lower = (pct_2party_hist + conf.low) * tot_2party_hist,
      vote_enr_upper = (pct_2party_hist + conf.high) * tot_2party_hist,
      vote_enr = (pct_2party_hist + estimate) * tot_2party_hist
    )
  
  base |> 
    filter(!is.na(vote_enr)) |> 
    mutate(
      vote_enr_lower = vote_enr,
      vote_enr_upper = vote_enr
    ) |> 
    bind_rows(pred_missing) |>
    summarize(
      across(starts_with("vote_enr"), sum),
      .by = candidate_party
    )
  
}

files = list.files("~/Dropbox (MIT)/Research/2024 Election Results/24_general/GA/clean", full.names = TRUE, pattern = "\\d\\.csv$")
files = files[str_detect(files, "11_0")]

ts = tibble(
  path = files,
  time = fs::path_file(path) |> str_remove("^GA_") |> ymd_hms()
) |> 
  mutate(
    out = map(path, generate_predictions, .progress = TRUE)
  )

ts |> 
  unnest_longer(col = out) |>
  unnest_wider(col = out) |>
  drop_na(vote_enr) |> 
  filter(time < ymd_hm("2024-11-07 01:00")) |>
  ggplot(aes(x = time, y = vote_enr, ymin = vote_enr_lower, ymax = vote_enr_upper, color = candidate_party)) +
  geom_pointrange(fatten=1) +
  ### truth lines
  geom_hline(yintercept = 2548017, color = "#3791FF", linetype = "dashed") +
  geom_hline(yintercept = 2663117, color = "#F6573E", linetype = "dashed") +
  scale_color_manual(values = c("Democrat" = "#3791FF", "Republican" = "#F6573E")) +
  theme_bw()


all = tibble(
  path = files,
  time = fs::path_file(path) |> str_remove("^GA_") |> ymd_hms()
) |> 
  mutate(
    file = map(path, fread)
  ) |> 
  unnest(cols = file) |> 
  filter(race_name == "President", candidate_name == "Kamala Harris") |> 
  select(time, jurisdiction, precinct_id, vote_mode, precinct_total)

all |> 
  filter(
    precinct_total != 0
  ) |> 
  filter(
    max(precinct_total) != min(precinct_total),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |> 
  distinct(jurisdiction, precinct_id, vote_mode, precinct_total) |> 
  arrange(jurisdiction, precinct_id, vote_mode)