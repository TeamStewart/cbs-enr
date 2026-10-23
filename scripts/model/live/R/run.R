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

MODELS = c("swing", "cory", "medsl")

# Vote methods the models know (early / eday / mail), and how the feed's other labels map
# onto them (CONTRACT.md's taxonomy: abs = absentee by mail, prov = provisional, cast on
# Election Day).
METHOD_MAP = c(early = "early", eday = "eday", mail = "mail", abs = "mail", prov = "eday")

# Does this race's feed report results by vote method? Only if EVERY row's method maps onto
# early / eday / mail. Precinct totals only ("all"), "adv" (early and mail lumped together)
# or a missing method anywhere in the race -> the race is modeled without method.
returns_by_method = function(returns) {
  m = as.character(returns$method)
  length(m) > 0 && all(!is.na(m) & m %in% names(METHOD_MAP))
}

# A race's cells and returns in the form the models fit:
#   by method   methods mapped onto early / eday / mail (abs -> mail, prov -> eday)
#   without     one cell per precinct, method "all": expected votes summed over methods,
#               returns summed over methods (complete once every method is; stage the highest)
shape_for_method = function(cells, returns) {
  # by method only if the feed splits by method AND the cells do (build_cells() gives one
  # "all" cell per precinct when the state's vote-method history lacks a method)
  by_method = returns_by_method(returns) && !all(cells$method == "all")
  keys = intersect(c("race_id", "state", "election", "geo", "candidate", "party"), names(returns))
  ret = returns |>
    mutate(method = if (by_method) unname(METHOD_MAP[as.character(method)]) else "all") |>
    summarize(votes = sum(votes), stage = max(stage), complete = all(complete), time = max(time),
              .by = all_of(c(keys, "method"))) |>
    mutate(frac = NA_real_)
  if (by_method) return(list(by_method = TRUE, cells = cells, returns = ret))
  cells_all = cells |>
    summarize(county = first(county), geostratum = first(geostratum), dem2 = first(dem2), oth = first(oth),
              exp_votes = sum(exp_votes), .by = geo) |>
    mutate(method = "all") |>
    select(geo, county, geostratum, method, dem2, oth, exp_votes)
  list(by_method = FALSE, cells = cells_all, returns = ret)
}

# Runs one model on one race. When the feed (or the state's vote-method history, see
# build_cells()) has no vote-method split, model_simpleswing
# and model_cory are fit on precinct totals (swing: county fixed effects only; cory:
# model_legacy_config(by_method = FALSE)) and model_medsl, whose equations are by method,
# is skipped with a status saying why.
run_race = function(model, cells, returns, ...) {
  sh = shape_for_method(cells, returns)
  fit = switch(model,
    swing = fit_swing(sh$cells, cell_counts(sh$cells, sh$returns), ...),
    cory  = fit_cory(sh$cells, sh$returns,
                     cfg = if (sh$by_method) NULL else enightmodels::model_legacy_config(by_method = FALSE), ...),
    medsl = if (sh$by_method) fit_medsl(sh$cells, sh$returns, ...) else
      list(model = "medsl", settings = NA_character_,
           status = if (returns_by_method(returns)) "skipped: the state's vote-method history has no split (see build_cells()), which model_medsl needs" else "skipped: this race's results have no vote-method split, which model_medsl needs"),
    stop("Unknown model '", model, "'. Choose from: ", paste(MODELS, collapse = ", "))
  )
  if (!sh$by_method && !is.null(fit$settings) && !is.na(fit$settings))
    fit$settings = paste0(fit$settings, "; fit WITHOUT vote method (the feed gave precinct totals only)")
  fit
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
