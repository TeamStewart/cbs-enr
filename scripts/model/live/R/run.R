# ========================================
## run.R -- one way to call any model on any race.
##
##   run_race("swing", cells, returns_one_race)   -> the model's full result (list)
##   run_all(cells, returns, models = c("swing", "cory", "medsl"))
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

MODELS = c("swing", "cory", "medsl", "cpois")

run_race = function(model, cells, returns, ...) {
  switch(model,
    swing = fit_swing(cells, cell_counts(cells, returns), ...),
    # precincts = TRUE so model_cory contributes rows to the precinct dump
    # (write_precincts()); it returns cells = NULL without it. Costs an extra run of both
    # model_legacy stages per race -- watch it in tests/time_models.R, since model_cory is
    # already the slowest in CA at ~58s.
    cory  = fit_cory(cells, returns, precincts = TRUE, ...),
    medsl = fit_medsl(cells, returns, ...),
    cpois = fit_cpois(cells, returns, ...),
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
