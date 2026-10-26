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

# Every model that is IMPLEMENTED and runnable. run_race() dispatches on this, and
# --models= validates against it, so a name here can always be asked for explicitly.
MODELS = c("swing", "cory", "medsl", "cpois", "cprec")

# What actually runs on a refresh unless --models= says otherwise. These are NOT the same
# list, and cprec is the reason.
#
# model_cory_prec is OUT OF THE DEFAULT RUN ON COST, not correctness. Measured on tx_base
# at 22:00, 2 races x 5 models: the whole refresh took 425s and cprec was 423s of it --
# model_medsl 37s, model_cory_pois 30s, model_cory 20s, model_simpleswing 12s. Its runtime
# is flat in draws and LINEAR IN CELLS, so the frame expansion makes it worse, not better:
# TX went from 6,134 precincts to 15,084 and CA is 26,700, which puts a single CA race in
# the thousands of seconds against a 300s refresh cadence. One model cannot be allowed to
# decide whether the other four are published on time.
#
# It stays wired, sourced and tested -- `--models=...,cprec` runs it, and that is how to
# re-measure it if someone makes it cheaper (the obvious lever is that it fits one
# conjugate regression per candidate over every cell, so a cell-count cap or a per-state
# draw cap would be the place to look, as MEDSL_MAX_PRECINCT_DRAWS already does for
# model_medsl).
MODELS_DEFAULT = setdiff(MODELS, "cprec")

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
    cprec = fit_cprec(cells, returns, ...),
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
