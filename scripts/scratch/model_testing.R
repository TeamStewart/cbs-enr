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

base = fread("~/Dropbox (MIT)/Research/2024 Election Results/24_general/GA/clean/GA_2024_11_07_09_37_14.csv")[
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
      rand_forest(trees = 400, mtry = .preds(), min_n = 10) |>
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
        across(starts_with(".pred"), ~ case_when(
          .default = .x,
          .x > quantile(.x, 0.99, na.rm = TRUE) ~ quantile(.x, 0.99, na.rm = TRUE),
          .x < quantile(.x, 0.01, na.rm = TRUE) ~ quantile(.x, 0.01, na.rm = TRUE),
        )),
        precinct_total_enr_lower = coalesce(precinct_total_enr, .pred_lower, precinct_total_hist),
        precinct_total_enr_upper = coalesce(precinct_total_enr, .pred_upper, precinct_total_hist),
        precinct_total_enr = coalesce(precinct_total_enr, .pred, precinct_total_hist),
        # precinct_total_enr_lower = ifelse(precinct_total_enr_lower < quantile(precinct_total_enr_lower, 0.01), quantile(precinct_total_enr_lower, 0.01), precinct_total_enr_lower),
        # precinct_total_enr_upper = ifelse(precinct_total_enr_upper > quantile(precinct_total_enr_upper, 0.99), quantile(precinct_total_enr_upper, 0.99), precinct_total_enr_upper),
        # precinct_total_enr = ifelse(precinct_total_enr > quantile(precinct_total_enr, 0.99), quantile(precinct_total_enr, 0.99), precinct_total_enr_upper),
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
files = files[str_detect(files, "11_05|11_06|11_07")]

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
  drop_na(vote_est) |> 
  pivot_wider(names_from = candidate_party, values_from = c(vote_est, vote_low, vote_upp)) |> 
  mutate(
    vote_est = vote_est_Democrat - vote_est_Republican,
    vote_low = vote_upp_Democrat - vote_low_Republican,
    vote_upp = vote_low_Democrat - vote_upp_Republican,
  ) |> 
  filter(time < ymd("2024-11-08")) |> 
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
  # geom_point() +
  # true vote total
  geom_hline(yintercept = -115100, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  # scale_color_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
  theme_bw()

######
# model tuning
######

noNA = drop_na(base, precinct_total_enr, precinct_total_hist)

rf_recipe <- recipe(
  precinct_total_enr ~ precinct_total_hist + vote_mode + candidate_party,
  data = noNA
) |> 
  step_normalize(all_numeric_predictors())

rf_spec <- rand_forest(
  trees = tune(),           # Tune number of trees
  mtry = .preds(),            # Tune mtry parameter
  min_n = tune()            # Tune minimum node size
) |> 
  set_mode("regression") |> 
  set_engine("ranger", keep.inbag = TRUE)

# 3. Create a workflow combining recipe and model
rf_workflow <- workflow() |> 
  add_recipe(rf_recipe) |> 
  add_model(rf_spec)

# 4. Create resampling folds for cross-validation
rf_folds <- vfold_cv(noNA, v = 5, strata = precinct_total_enr)

# 5. Define the parameter grid for tuning
rf_grid <- grid_random(
  trees(range = c(100, 1000)),
  min_n(range = c(2, 30)),
  size = 30
)

rf_tune_results <- rf_workflow |> 
  tune_grid(
    resamples = rf_folds,
    grid = rf_grid,
    metrics = metric_set(rmse, rsq, mae),
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )

# best results are ~500 trees, 18 min_n
(best_rf <- show_best(rf_tune_results, metric = "rmse", n = 1))

fit_final <- rf_workflow |> 
  finalize_workflow(best_rf) |> 
  fit(data = noNA) |> 
  extract_fit_parsnip(final_rf_fit)

