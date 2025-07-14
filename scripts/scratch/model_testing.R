rm(list=ls())
gc()

{
  library(tidyverse)
  library(data.table)
  library(marginaleffects)
  library(fixest)
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

# hist_turnout = select(hist, county:vote_mode, votes_precFinal_20) |> 
#   summarize(
#     votes_precFinal_20 = sum(votes_precFinal_20),
#     .by = c(county, precinct_24, vote_mode)
#   ) |> 
#   mutate(
#     total_votes = sum(votes_precFinal_20),
#     .by = c(county, precinct_24)
#   )
# 
# hist_voteshare = select(hist, county:vote_mode, votes_20_dem, votes_20_rep) |> 
#   pivot_longer(cols = c(votes_20_dem, votes_20_rep), names_to = "candidate_party", values_to = "precinct_total") |> 
#   summarize(
#     precinct_total = sum(precinct_total),
#     .by = c(county, precinct_24, vote_mode, candidate_party)
#   ) |> 
  # mutate(
  #   candidate_party = case_when(
  #     str_detect(candidate_party, "dem$") ~ "Democrat",
  #     str_detect(candidate_party, "rep$") ~ "Republican"
  #   )
  # )

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

# turnout = base |> 
#   select(jurisdiction:vote_mode, total_votes, total_votes_byMode) |> 
#   mutate(
#     total_votes = na_if(total_votes, 0),
#     total_votes_byMode = na_if(total_votes_byMode, 0)
#   ) |> 
#   left_join(
#     hist_turnout, 
#     join_by(jurisdiction == county, precinct_id == precinct_24, vote_mode),
#     suffix = c("_enr", "_hist")
#   )
# 
# voteshare = base |> 
#   select(jurisdiction:vote_mode, candidate_party, precinct_total) |> 
#   mutate(
#     precinct_total = na_if(precinct_total, 0)
#   ) |> 
#   left_join(
#     hist_voteshare, 
#     join_by(jurisdiction == county, precinct_id == precinct_24, vote_mode, candidate_party),
#     suffix = c("_enr", "_hist")
#   )
# 
# fit_turnout = glm(
#   total_votes_byMode ~ votes_precFinal_20 + vote_mode + jurisdiction,
#   data = turnout,
#   family = "poisson"
# )
# 
# turnout |> 
#   select(jurisdiction, precinct_id, vote_mode, total_votes_byMode, votes_precFinal_20) |> 
#   mutate(
#     py = predict(fit_turnout, type = "response")
#   )
# 
# 
# predictions(fit_turnout, datagrid(votes_precFinal_20 = turnout$votes_precFinal_20)) |> 
#   as_tibble()
# 
# noNA = drop_na(base, vote_pct_hist, vote_mode, jurisdiction, votes_precFinal_20, candidate_party) |> 
#   filter(votes_precFinal_20 > 0)
# 
# 
# fit <- feols(
#   vote_pct_enr ~ vote_pct_hist | vote_mode + jurisdiction,
#   data = base,
#   weights = ~ votes_precFinal_20,
#   split = ~ candidate_party
# )
# 
# predictions(fit[[1]], vcov=FALSE) |> as_tibble()
# 
# base |> 
#   mutate(
#     pred_dem = predictions(fit[[1]], vcov = FALSE)$estimate,
#     pred_rep = predictions(fit[[2]], vcov = FALSE)$estimate
#   )
# 
# 
# predictions(fit[[1]], vcov = FALSE)
# predictions(fit[[1]], vcov = FALSE, by = "vote_mode")
# predictions(fit[[1]], vcov = FALSE, newdata = datagrid(vote_pct_hist, vote_mode, jurisdiction = NA))

m = mice(
  base, 
  m = 10,
  # method = "rf",
  # formulas = list(precinct_total ~ jurisdiction + vote_mode + votes_20 + votes_precFinal_20),
  # predictorMatrix = matrix(c(0, 1, 0, 1, 0, 1, 1), nrow=1),
  method = ifelse(colnames(base) == "vote_pct_enr", "rf", ""),
  # post = post
)

o = with(m, lm(vote_pct_enr ~ vote_pct_hist + vote_mode + jurisdiction, subset = candidate_party == "Democrat"))

(preds = predictions(o, type = "response") |> as_tibble())

library(tidymodels)

lm_fit <- linear_reg() |> 
  fit(
    vote_pct_enr ~ vote_pct_hist + vote_mode + candidate_party,
    data = base
  )

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
      # jurisdiction = as_factor(jurisdiction),
      vote_mode = as_factor(vote_mode)
    ) |> 
    # mutate(
    #   total_votes = sum(precinct_total, na.rm=TRUE),
    #   .by = c(jurisdiction, precinct_id)
    # ) |>
    # mutate(
    #   total_votes_byMode = sum(precinct_total, na.rm=TRUE),
    #   .by = c(jurisdiction, precinct_id, vote_mode)
    # ) |>
    mutate(
      # vote_pct = precinct_total / total_votes_byMode,
      precinct_id = str_to_lower(precinct_id)
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
    vote_low = vote_low_Democrat - vote_low_Republican,
    vote_upp = vote_upp_Democrat - vote_upp_Republican,
  ) |> 
  # filter(candidate_party == "Democrat") |> 
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
