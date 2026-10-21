# ========================================
## run.R -- one way to call any model on any race.
##
##   run_race("swing", cells, returns_one_race)   -> the model's full result (list)
##   run_all(cells, returns, models = c("swing", "cory"))
##                                               -> one row per race x model
##
## Every model takes the same two inputs -- cells (cells.R) and standard returns
## (returns.R) -- and returns the same list:
##   model, status, settings
##   race   one row: est_twop, lo_twop, hi_twop, p_dem_win, est_turnout, lo_turnout,
##          hi_turnout, raw_twop, counted_votes, expected_votes, obs_cells, complete_cells
##   cells  projections below the race level (precinct x method, or precinct for cory)
##   draws  simulation / bootstrap draws, if the model has them
#
## To add a model: write fit_<name>(cells, returns_or_counts, ...) returning that list, and
## add it to run_race() below.
# ========================================

MODELS = c("swing", "cory")      # model_y is not wired in yet -- see R/model_y.R

run_race = function(model, cells, returns, ...) {
  switch(model,
    swing = fit_swing(cells, cell_counts(cells, returns), ...),
    cory  = fit_cory(cells, returns, ...),
    y     = stop("model_y is not wired in yet: R/model_y.R is an untouched copy of tx_gov_trial.R ",
                 "and still needs to be wrapped as fit_y(cells, returns)."),
    stop("Unknown model '", model, "'. Choose from: ", paste(MODELS, collapse = ", "))
  )
}

# A model that errors on one race is recorded (status = "error: ...") rather than stopping
# the other races -- on election night one bad race must not take down the rest.
run_all = function(cells, returns, models = MODELS, ...) {
  races = sort(unique(returns$race_id))
  purrr::map_dfr(races, function(r) {
    ret = filter(returns, race_id == r)
    purrr::map_dfr(models, function(m) {
      res = tryCatch(run_race(m, cells, ret, ...),
                     error = function(e) list(race = tibble(status = paste("error:", conditionMessage(e)))))
      bind_cols(tibble(race_id = r, model = m), res$race)
    })
  })
}
