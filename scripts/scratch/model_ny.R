rm(list = ls())
gc()

{
  library(tidyverse)
  library(data.table)
  library(marginaleffects)
  library(gbm3)
}

hist = fread("~/Dropbox (MIT)/Research/CBS-MIT Election Data/25_general/history/NY_history.csv")
l2 = fread("~/Dropbox (MIT)/Research/CBS-MIT Election Data/25_general/input_data/NY/NYC_20250809_demo_prec_20250812.csv")

data = left_join(hist, l2, join_by(precinct_cbs, ad, ed))

data_models = data |> 
  select(
    votes_mayor_21_dem, votes_potus_24_dem, votes_precFinal_24, votes_precFinal_21, n:vote20, idp, con, wfp, primary21_dem, primary21
  ) |> 
  select(
    -male, -rep, -col
  ) |> 
  mutate(
    votes_mayor_21_dem = as.integer(votes_mayor_21_dem)
  )

ggplot(data_models, aes(x = votes_mayor_21_dem, y = votes_potus_24_dem)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  theme_bw()

m = gbm3::gbm(
  formula = votes_mayor_21_dem ~ .,
  weights = data_models$votes_precFinal_24,
  data = select(data_models, -votes_precFinal_24),
  distribution = list(name = "quantile", alpha = 0.25),
  shrinkage = 0.01,
  train.fraction = 0.8,
  cv.folds = 5,
  verbose = TRUE
)

m2 = gbm3::gbm(
  formula = votes_mayor_21_dem ~ .,
  weights = data_models$votes_precFinal_24,
  data = select(data_models, -votes_precFinal_24),
  distribution = "poisson",
  shrinkage = 0.01,
  train.fraction = 0.8,
  cv.folds = 5,
  verbose = TRUE
)

