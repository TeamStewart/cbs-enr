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
##   model, status, settings, conf (how the interval was made; results.R)
##   race   one row: est_twop, lo_twop, hi_twop, p_dem_win, est_turnout, lo_turnout,
##          hi_turnout, raw_twop, counted_votes, expected_votes, obs_cells, complete_cells
##   cells  projections below the race level (precinct x method, or precinct)
##   draws  simulation / bootstrap draws (twop, turnout, dem, rep), when the interval is from them
##   pred   per-candidate predictions (candidate, party, pred, pred_min, pred_max), when it isn't
#
## A model name is "<model>[/<demo>][/<interval>]": <demo> a demographic spec from demos.R,
## <interval> one of results.R's INTERVALS, e.g. "pois_bayes/strat_d/conformal". Left out,
## the model runs its DEMO_DEFAULT and the first of its MODEL_INTERVALS. The name is also the
## published label, written in that order. medsl_sur takes no spec and only "credible".
##
##   swing         uniform swing, county x method fixed effects (model_swing.R)
##   ratio_strata  Cory's legacy ratio model, ratios pooled within strata (model_ratio_strata.R)
##   pois_bayes    Bayesian Poisson precinct model, enightmodels::pois_bayes() (model_pois_bayes.R)
##   lm_bayes      Cory's NYC forecaster, enightmodels::forecast_prec() (model_lm_bayes.R)
##   medsl_sur     Young-Kyung's model_hybrid.R, run through model_medsl.R
## Retired: model_medsl (fit_medsl() is what medsl_sur runs, but it is no longer fit on its
## own) and model_cory_pois (R/model_cpois.R, not loaded: pois_bayes with a spec and
## /conformal covers it).
##
## To add a model: write fit_<name>(cells, returns_or_counts, ...) returning that list, and
## add it to run_race() below.
# ========================================

# Every model that is IMPLEMENTED and runnable. run_race() dispatches on this, and
# --models= validates against it, so a name here can always be asked for explicitly.
MODELS = c("swing", "ratio_strata", "pois_bayes", "lm_bayes", "medsl_sur")

# What runs on a refresh unless --models= says otherwise. lm_bayes is left out on cost: almost
# all of its time is enightmodels::lm_bayes(), and it grows faster than linearly in cells (TX
# gov at 22:00: 38s on the 32 scraped counties, 18,402 cells; 133s on all 26,313).
MODELS_DEFAULT = setdiff(MODELS, "lm_bayes")

# "pois_bayes/strat_d/conformal" -> list(base = "pois_bayes", demo = "strat_d",
# interval = "conformal"); demo / interval NULL when not given
model_parts = function(model) {
  tok = strsplit(model, "/", fixed = TRUE)[[1]]
  opt = tok[-1]
  bad = setdiff(opt, c(names(DEMOS), INTERVALS))
  if (length(bad)) stop("'", model, "': ", paste(bad, collapse = ", "), " is neither a demographic spec (",
                        paste(names(DEMOS), collapse = ", "), ") nor an interval (", paste(INTERVALS, collapse = ", "), ")")
  pick1 = function(set, what) { x = opt[opt %in% set]; if (length(x) > 1) stop("'", model, "' has more than one ", what); if (length(x)) x }
  list(base = tok[1], demo = pick1(names(DEMOS), "demographic spec"), interval = pick1(INTERVALS, "interval"))
}
model_base = function(model) vapply(model, \(m) model_parts(m)$base, "", USE.NAMES = FALSE)

# the published name: the parts that were given, in the order model/demo/interval
model_label = function(model) {
  p = model_parts(model)
  paste(c(p$base, p$demo, p$interval), collapse = "/")
}

# stops on an unknown model, spec or interval, or one the model can't take
check_models = function(models) {
  for (m in models) {
    p = model_parts(m)
    if (!p$base %in% MODELS) stop("unknown model '", p$base, "'. Choose from: ", paste(MODELS, collapse = ", "))
    if (!is.null(p$demo) && !p$base %in% names(DEMO_DEFAULT)) stop(p$base, " takes no demographic spec (", m, ")")
    if (!is.null(p$interval) && !p$interval %in% MODEL_INTERVALS[[p$base]])
      stop(p$base, " gives intervals ", paste(MODEL_INTERVALS[[p$base]], collapse = ", "), ", not ", p$interval)
  }
  invisible(models)
}

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
# see build_cells()), every model but medsl_sur is fit on precinct totals; medsl_sur, whose
# equations are by method, is skipped with a status saying why. When only SOME counties'
# results are split by method (a county scraper with precinct totals only, e.g. TX Hardin, MI
# Wayne, most of CA), predict_one() fits medsl_sur on those counties and the others come from
# CBS's county results in the statewide rows.
METHOD_ONLY_MODELS = "medsl_sur"

# the counties (3-digit code in the geo) whose every returns row has a usable method
method_counties = function(returns) {
  returns |>
    mutate(cty = geo_county(geo), ok = !is.na(method) & as.character(method) %in% names(METHOD_MAP)) |>
    summarize(ok = all(ok), .by = cty) |>
    filter(ok, !is.na(cty)) |>
    pull(cty)
}

# `...` goes to the model's fit function (e.g. cbs_county for medsl_sur)
run_race = function(model, cells, returns, ...) {
  check_models(model)
  p = model_parts(model)
  base = p$base
  demo = p$demo %||% DEMO_DEFAULT[[base]]
  interval = p$interval %||% MODEL_INTERVALS[[base]][1]
  sh = shape_for_method(cells, returns)
  needs_method = function(m) list(model = m, settings = NA_character_,
    status = if (returns_by_method(returns)) paste0("skipped: the state's vote-method history has no split (see build_cells()), which ", m, " needs") else
      paste0("skipped: this race's results have no vote-method split, which ", m, " needs"))
  fit = switch(base,
    swing = fit_swing(sh$cells, cell_counts(sh$cells, sh$returns), demo = demo, interval = interval,
                      state = returns$state[1], returns = sh$returns, ...),
    ratio_strata = fit_ratio_strata(sh$cells, sh$returns, config = enightmodels::model_legacy_config(by_method = sh$by_method),
                                    demo = demo, interval = interval, ...),
    pois_bayes = fit_pois_bayes(sh$cells, sh$returns, demo = demo, interval = interval, ...),
    lm_bayes = fit_lm_bayes(sh$cells, sh$returns, demo = demo, interval = interval, ...),
    medsl_sur = if (sh$by_method) fit_medsl_sur(sh$cells, sh$returns, ...) else needs_method("medsl_sur")
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
