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
# Models whose equations are by method. When only SOME counties' results come split by
# method (a county scraper that gives precinct totals only, e.g. TX Hardin, MI Wayne, most
# of CA), predict_one() fits these on the counties that do split, and the others come from
# CBS's county results in the statewide rows -- rather than skipping the whole race.
METHOD_ONLY_MODELS = c("medsl", "cpois", "cprec")

# the counties (3-digit code in the geo) whose every returns row has a usable method
method_counties = function(returns) {
  returns |>
    mutate(cty = geo_county(geo), ok = !is.na(method) & as.character(method) %in% names(METHOD_MAP)) |>
    summarize(ok = all(ok), .by = cty) |>
    filter(ok, !is.na(cty)) |>
    pull(cty)
}

run_race = function(model, cells, returns, ...) {
  sh = shape_for_method(cells, returns)
  # models whose equations are by method; without a method split they are skipped, not fit
  needs_method = function(m) list(model = m, settings = NA_character_,
    status = if (returns_by_method(returns)) paste0("skipped: the state's vote-method history has no split (see build_cells()), which model_", m, " needs") else
      paste0("skipped: this race's results have no vote-method split, which model_", m, " needs"))
  fit = switch(model,
    swing = fit_swing(sh$cells, cell_counts(sh$cells, sh$returns), ...),
    # precincts = TRUE so model_cory contributes rows to the precinct dump
    # (write_precincts()); it returns cells = NULL without it. Costs an extra run of both
    # model_legacy stages per race -- watch it in tests/time_models.R, since model_cory is
    # already the slowest in CA at ~58s.
    cory  = fit_cory(sh$cells, sh$returns, precincts = TRUE,
                     cfg = if (sh$by_method) NULL else enightmodels::model_legacy_config(by_method = FALSE), ...),
    medsl = if (sh$by_method) fit_medsl(sh$cells, sh$returns, ...) else needs_method("medsl"),
    # not yet tested without a method split, so skipped there rather than risked
    cpois = if (sh$by_method) fit_cpois(sh$cells, sh$returns, ...) else needs_method("cpois"),
    cprec = if (sh$by_method) fit_cprec(sh$cells, sh$returns, ...) else needs_method("cprec"),
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
