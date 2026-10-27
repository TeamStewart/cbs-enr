# ========================================
## run.R -- one way to call any model on any race.
##
##   run_race("swing", cells, returns_one_race)   -> the model's full result (list)
##   fit_model("swing", cells, returns_one_race)  -> the same, with NC-style virtual
##                                                   precincts handled (virtual.R)
##   run_all(cells, returns, models = MODELS)     -> one row per race x model (fit_model())
##
## Every model takes the same two inputs -- cells (cells.R) and standard returns
## (returns.R) -- and returns the same list:
##   model, status, settings
##   race   one row: est_twop, lo_twop, hi_twop, p_dem_win, est_turnout, lo_turnout,
##          hi_turnout, raw_twop, counted_votes, expected_votes, obs_cells, complete_cells
##   cells  projections below the race level (precinct x method, or precinct)
##   draws  simulation / bootstrap draws (twop, turnout, dem, rep), if the model has them
##   pred   per-candidate predictions (candidate, party, pred, pred_min, pred_max), for the
##          models without draws (model_cory, model_cory_pois)
#
## To add a model: write fit_<name>(cells, returns_or_counts, ...) returning that list, and
## add it to run_race() below.
# ========================================

# Every model that is IMPLEMENTED and runnable. run_race() dispatches on this, and
# --models= validates against it, so a name here can always be asked for explicitly.
MODELS = c("swing", "cory", "medsl", "cpois", "cprec", "hybrid", "pbayes")

# What runs on a refresh unless --models= says otherwise. model_cory_prec is left out on
# cost: almost all of its time is enightmodels::lm_bayes(), and it grows faster than linearly
# in cells (TX gov at 22:00: 38s on the 32 scraped counties, 18,402 cells; 133s on all 26,313).
# model_hybrid and model_cory_pbayes are left out until they are evaluated on the sims.
MODELS_DEFAULT = setdiff(MODELS, c("cprec", "hybrid", "pbayes"))

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

# Runs one model on one race. Without a vote-method split (the feed's or the state's history,
# see build_cells()), every model but model_medsl is fit on precinct totals; model_medsl,
# (and model_hybrid), whose equations are by method, is skipped with a status saying why. When only SOME
# counties' results are split by method (a county scraper with precinct totals only, e.g. TX
# Hardin, MI Wayne, most of CA), predict_one() fits model_medsl on those counties and the
# others come from CBS's county results in the statewide rows.
METHOD_ONLY_MODELS = c("medsl", "hybrid")

# the counties (3-digit code in the geo) whose every returns row has a usable method
method_counties = function(returns) {
  returns |>
    mutate(cty = geo_county(geo), ok = !is.na(method) & as.character(method) %in% names(METHOD_MAP)) |>
    summarize(ok = all(ok), .by = cty) |>
    filter(ok, !is.na(cty)) |>
    pull(cty)
}

# `...` goes to the model's fit function (e.g. cbs_county for model_hybrid)
run_race = function(model, cells, returns, ...) {
  sh = shape_for_method(cells, returns)
  needs_method = function(m) list(model = m, settings = NA_character_,
    status = if (returns_by_method(returns)) paste0("skipped: the state's vote-method history has no split (see build_cells()), which model_", m, " needs") else
      paste0("skipped: this race's results have no vote-method split, which model_", m, " needs"))
  fit = switch(model,
    swing = fit_swing(sh$cells, cell_counts(sh$cells, sh$returns), ...),
    cory  = fit_cory(sh$cells, sh$returns, config = enightmodels::model_legacy_config(by_method = sh$by_method), ...),
    medsl = if (sh$by_method) fit_medsl(sh$cells, sh$returns, ...) else needs_method("medsl"),
    cpois = fit_cpois(sh$cells, sh$returns, ...),
    cprec = fit_cprec(sh$cells, sh$returns, ...),
    hybrid = if (sh$by_method) fit_hybrid(sh$cells, sh$returns, ...) else needs_method("hybrid"),
    pbayes = fit_cpbayes(sh$cells, sh$returns, ...),
    stop("Unknown model '", model, "'. Choose from: ", paste(MODELS, collapse = ", "))
  )
  if (!sh$by_method && !is.null(fit$settings) && !is.na(fit$settings))
    fit$settings = paste0(fit$settings, "; fit WITHOUT vote method (the feed gave precinct totals only)")
  fit
}

# A model that errors on one race is recorded (status = "error: ...") rather than stopping
# the other races -- on election night one bad race must not take down the rest.
run_all = function(cells, returns, models = MODELS) {
  races = sort(unique(returns$race_id))
  purrr::map_dfr(races, function(r) {
    ret = filter(returns, race_id == r)
    purrr::map_dfr(models, function(m) {
      res = tryCatch(fit_model(m, cells, ret),
                     error = function(e) list(race = tibble(status = paste("error:", conditionMessage(e)))))
      bind_cols(tibble(race_id = r, model = m), res$race)
    })
  })
}
