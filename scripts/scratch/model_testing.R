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

hist = fread("~/Dropbox (MIT)/Research/CBS-MIT Election Data/24_general/history/GA_history.csv") |> 
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
    pct_2party_hist = case_when(
      tot_2party_hist == 0 ~ 0,
      .default = precinct_total / sum(precinct_total)
    ),
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

files = list.files("~/Dropbox (MIT)/Research/CBS-MIT Election Data/24_general/GA/clean", full.names = TRUE, pattern = "\\d\\.csv$")
files = files[str_detect(files, "11_0")]

base = fread(files[6])[
  race_name == "President",
  list(jurisdiction, precinct_id, vote_mode, precinct_total, candidate_party, candidate_name)
] |> 
  summarize(
    precinct_total = sum(precinct_total, na.rm=TRUE),
    .by = c(jurisdiction, precinct_id, vote_mode, candidate_party)
  ) |>
  mutate(
    precinct_id = str_to_lower(precinct_id),
    vote_mode = as_factor(vote_mode)
  )

reports = base |> 
  summarize(
    precinct_total = sum(precinct_total, na.rm=TRUE),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |> 
  filter(vote_mode != "Provisional") |> 
  summarize(
    reported = !any(precinct_total == 0),
    .by = c(jurisdiction, precinct_id)
  )

base = base |> 
  left_join(reports, join_by(jurisdiction, precinct_id)) |> 
  mutate(
    precinct_total = if_else(
      reported,
      precinct_total,
      NA
    )
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
    pct_2party_enr = case_when(
      tot_2party_enr == 0 ~ 0,
      .default = vote_enr / tot_2party_enr
    )
  )

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
      vote_mode = as_factor(vote_mode)
    )
  
  reports = base |> 
    summarize(
      precinct_total = sum(precinct_total, na.rm=TRUE),
      .by = c(jurisdiction, precinct_id, vote_mode)
    ) |> 
    filter(vote_mode != "Provisional") |> 
    summarize(
      reported = !any(precinct_total == 0),
      .by = c(jurisdiction, precinct_id)
    )
  
  base = base |> 
    left_join(reports, join_by(jurisdiction, precinct_id)) |> 
    mutate(
      precinct_total = if_else(
        reported,
        precinct_total,
        NA
      )
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
      pct_2party_enr = case_when(
        tot_2party_enr == 0 ~ 0,
        .default = vote_enr / tot_2party_enr
      )
    )
  
  if (nrow(drop_na(base, vote_enr)) < 10) {
    cli::cli_alert_info("Not enough completed precincts. Stopping")
    return(NULL)
  }
  
  modes = base |> 
    drop_na(vote_enr) |>
    filter(vote_enr > 0) |> 
    count(vote_mode) |> 
    filter(n>2) |> 
    pull(vote_mode) |> 
    sort()
  
  all_modes = identical(modes, distinct(base, vote_mode) |> pull() |> sort())
  
  imputes = base |> 
    summarize(
      vote_hist = median(vote_hist, na.rm=TRUE),
      tot_2party_hist = median(tot_2party_hist, na.rm=TRUE),
      .by = c(jurisdiction, candidate_party, vote_mode)
    ) |> 
    mutate(
      vote_hist2 = median(vote_hist, TRUE),
      tot_2party_hist2 = median(tot_2party_hist, TRUE),
      .by = c(candidate_party, vote_mode)
    ) |> 
    mutate(
      vote_hist = coalesce(vote_hist, vote_hist2),
      tot_2party_hist = coalesce(tot_2party_hist, tot_2party_hist2),
      pct_2party_hist = vote_hist / tot_2party_hist,
      pct_2party_hist = ifelse(is.nan(pct_2party_hist), 0, pct_2party_hist)
    )
  
  train <- base |> 
    drop_na(vote_enr) |> 
    slice_sample(prop = 1, by = jurisdiction) |> 
    left_join(imputes, join_by(jurisdiction, candidate_party, vote_mode)) |> 
    mutate(
      pct_2party_hist = coalesce(pct_2party_hist.x, pct_2party_hist.y),
      tot_2party_hist = coalesce(tot_2party_hist.x, tot_2party_hist.y),
      vote_hist = coalesce(vote_hist.x, vote_hist.y)
    ) |> 
    select(-ends_with(".y"), -ends_with(".x"))
  
  calibration <- base |> 
    drop_na(vote_enr) |> 
    anti_join(train, by = join_by(jurisdiction, precinct_id, vote_mode, candidate_party)) |> 
    left_join(imputes, join_by(jurisdiction, candidate_party, vote_mode)) |> 
    mutate(
      pct_2party_hist = coalesce(pct_2party_hist.x, pct_2party_hist.y),
      tot_2party_hist = coalesce(tot_2party_hist.x, tot_2party_hist.y),
      vote_hist = coalesce(vote_hist.x, vote_hist.y)
    ) |> 
    select(-ends_with(".y"), -ends_with(".x"))
  
  fit <- lm(
    formula = as.formula(paste0("pct_2party_enr ~ pct_2party_hist + candidate_party", if (all_modes) "+vote_mode" else "")), 
    data = train
  )
  
  test = base |> 
    filter(is.na(vote_enr)) |> 
    left_join(imputes, join_by(jurisdiction, candidate_party, vote_mode)) |> 
    mutate(
      pct_2party_hist = coalesce(pct_2party_hist.x, pct_2party_hist.y),
      tot_2party_hist = coalesce(tot_2party_hist.x, tot_2party_hist.y),
      vote_hist = coalesce(vote_hist.x, vote_hist.y)
    ) |> 
    select(-ends_with(".y"), -ends_with(".x"))
  
  preds = predictions(fit, vcov = ~ jurisdiction, conf_level = 0.95) |> 
    # inferences(method = "simulation", R=500) |>
    inferences(
      method = "conformal_cv+",
      R=20,
      conformal_test = test,
      conformal_score = "residual_abs"
    ) |>
    as_tibble()
  
  pred_missing = preds |> 
    filter(is.na(vote_enr)) |> 
    select(vote_mode, candidate_party, estimate, conf.low, conf.high, tot_2party_hist, pct_2party_hist, vote_hist) |> 
    mutate(
      vote_enr_lower = conf.low * tot_2party_hist,
      vote_enr_upper = conf.high * tot_2party_hist,
      vote_enr = estimate * tot_2party_hist
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
  filter(time < ymd_hm("2024-11-06 01:00")) |>
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