rm(list=ls())
gc()

{
  library(tidyverse)
  library(data.table)
  library(tidymodels)
}

########################################
# setup history files
########################################

hist = fread("~/Dropbox (MIT)/Research/2024 Election Results/history/GA_history.csv") |> 
  mutate(precinct_24 = str_to_lower(precinct_24)) |> 
  summarize(across(votes_20_dem:votes_precFinal_20, sum), .by = c(county, precinct_24, vote_mode)) |> 
  pivot_longer(cols = c(votes_20_dem, votes_20_rep), names_to = "candidate_party", values_to = "precinct_total") |> 
  mutate(
    candidate_party = case_when(
      str_detect(candidate_party, "_dem") ~ "Democrat",
      str_detect(candidate_party, "_rep") ~ "Republican"
    )
  )

########################################
# now work with the 'incoming raw data'
########################################

base = fread("~/Dropbox (MIT)/Research/2024 Election Results/24_general/GA/clean/GA_2024_11_05_20_29_15.csv")[
  race_name == "President",
  list(jurisdiction, precinct_id, vote_mode, precinct_total, candidate_party)
] |> 
  summarize(
    precinct_total = sum(precinct_total, na.rm=TRUE),
    .by = c(jurisdiction, precinct_id, vote_mode, candidate_party)
  ) |> 
  mutate(
    precinct_total = na_if(precinct_total, 0),
    jurisdiction = as_factor(jurisdiction),
    vote_mode = as_factor(vote_mode)
  ) |> 
  mutate(
    total_votes = sum(precinct_total, na.rm=TRUE),
    .by = c(jurisdiction, precinct_id)
  ) |>
  mutate(
    total_votes_byMode = sum(precinct_total, na.rm=TRUE),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |>
  mutate(
    vote_pct = precinct_total / total_votes_byMode,
    precinct_id = str_to_lower(precinct_id)
  ) |> 
  left_join(
    hist, 
    join_by(jurisdiction == county, precinct_id == precinct_24, vote_mode, candidate_party),
    suffix = c("_enr", "_hist")
  ) |> 
  filter(candidate_party %in% c("Democrat", "Republican"))

generate_predictions <- function(path) {
  
  base = fread(path)[
    race_name == "President",
    list(jurisdiction, precinct_id, vote_mode, precinct_total, candidate_party)
  ] |> 
    summarize(
      precinct_total = sum(precinct_total, na.rm=TRUE),
      .by = c(jurisdiction, precinct_id, vote_mode, candidate_party)
    ) |> 
    mutate(
      precinct_total = na_if(precinct_total, 0),
      precinct_id = str_to_lower(precinct_id),
      vote_mode = as_factor(vote_mode)
    ) |> 
    left_join(
      hist, 
      join_by(jurisdiction == county, precinct_id == precinct_24, vote_mode, candidate_party),
      suffix = c("_enr", "_hist")
    ) |> 
    filter(candidate_party %in% c("Democrat", "Republican"))
  
  tryCatch({
    
    rf_fit <- 
      rand_forest(trees = 500, mtry = .preds()) |> 
      set_mode("regression") |> 
      set_engine("ranger", keep.inbag = TRUE) |> 
      fit(
        precinct_total_enr ~ precinct_total_hist + vote_mode + candidate_party,
        data = base
      )
    
    bind_cols(
      predict(rf_fit, new_data = base),
      predict(rf_fit, new_data = base, type = "conf_int"),
      base
    ) |> 
      mutate(
        precinct_total_enr_lower = coalesce(precinct_total_enr, .pred_lower, precinct_total_hist),
        precinct_total_enr_upper = coalesce(precinct_total_enr, .pred_upper, precinct_total_hist),
        precinct_total_enr = coalesce(precinct_total_enr, .pred, precinct_total_hist)
      ) |> 
      summarize(
        vote_est = sum(precinct_total_enr),
        vote_low = sum(precinct_total_enr_lower, na.rm = TRUE),
        vote_upp = sum(precinct_total_enr_upper, na.rm = TRUE),
        .by = candidate_party
      )
    
  }, error = function(e){
    return(NA)
  })
  
}

files = list.files("~/Dropbox (MIT)/Research/2024 Election Results/24_general/GA/clean", full.names = TRUE, pattern = "\\d\\.csv$")
files = files[str_detect(files, "11_05|11_06")]

ts = tibble(
  path = files,
  time = fs::path_file(path) |> str_remove("^GA_") |> ymd_hms()
) |> 
  mutate(
    out = map(path, generate_predictions, .progress = TRUE)
  )

ts |> 
  unnest_auto(col = out) |> 
  unnest_wider(col = out) |> 
  drop_na(vote_est) |> 
  pivot_wider(names_from = candidate_party, values_from = c(vote_est, vote_low, vote_upp)) |> 
  mutate(
    vote_est = vote_est_Democrat - vote_est_Republican,
    vote_low = vote_upp_Democrat - vote_low_Republican,
    vote_upp = vote_low_Democrat - vote_upp_Republican,
  ) |> 
  ggplot(aes(x = time, y = vote_est, ymin = vote_low, ymax = vote_upp)) +
  annotate(
    "rect",
    ymin=0, xmin = as.POSIXct(-Inf, origin = '2014-10-15'), xmax=as.POSIXct(Inf, origin = '2014-10-15'), ymax = Inf,
    fill = "#3791FF", alpha = 0.4, color = NA
  ) +
  annotate(
    "rect",
    ymin=-Inf, xmin = as.POSIXct(-Inf, origin = '2014-10-15'), xmax=as.POSIXct(Inf, origin = '2014-10-15'), ymax = 0,
    fill = "#F6573E", alpha = 0.4, color = NA
  ) +
  geom_pointrange(fatten=1) +
  # true vote total
  geom_hline(yintercept = -115100, color = "black", linetype = "dashed") +
  # scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  theme_bw()
