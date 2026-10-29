# ========================================
## model_lm_bayes.R -- lm_bayes: Cory McCartan's Bayesian precinct forecaster,
## enightmodels::forecast_prec(), set up as in his 2025 NYC mayoral run
## (scripts/model/modeling-cory/nyc/R/nyc_enr_forecast.R).
##
## One conjugate regression (lm_bayes) for log turnout, fit on counted-out cells, and one per
## candidate column for its share of all votes, fit on observed cells; each with a prior
## centred on our baseline (prior_turn = expected votes, prior shares = dem2 / oth), worth
## prior_n_turn / prior_n_cands observations. Counted values replace the draws where they are
## in. The default spec is NYC's `simple_pres`, log_s(prior turnout) + past Dem share, with the
## default prior weights of Cory's NYC wrapper (prior_n_turn = 0.5, prior_n_cands = 1.5).
## `demo` (demos.R) adds a demographic spec's columns to the spec, (1 | x) terms as dummies,
## as in NYC's `full` spec (ages, race x education).
##
## Differences from NYC, all because our cells are precinct x method:
##   - the method enters as 0/1 columns, not a factor: lm_bayes() drops factor levels absent
##     from the observed rows (mail, early in the night) and its design matrices then differ.
##   - obs (share observed) = counted out, or counted >= obs_min of expected votes (NYC:
##     precinct total > 1.2 x its early vote); obs_turn = counted out (NYC: >= 98% reporting).
##   - candidates are dem / rep / oth; output.R splits oth across the third-party candidates.
## Turnout prior: expected votes x the cell's congressional-district turnout adjustment
## (district_turnout_adjust(), demos.R), per method -- see fit_lm_bayes().
## Interval (results.R): credible (default) from the posterior draws, with p_dem_win and
## statewide intervals; conf_seq from forecast_summarize()'s prior and posterior race totals,
## as forecast_intervals() does; conformal on each cell's fitted shares from the share
## regressions (predict(), clamped to [0, 1] and normalised).
# ========================================

LM_BAYES_FORM = ~ log_s(prior_turn) + dem2

#' One row per cell for the lm_bayes fit
#'
#' @param cells Cells from `build_cells()`.
#' @param counts `cell_counts()` for those cells.
#' @param obs_min A cell's shares count as observed once it is complete or this share of
#'   its prior turnout is in; unobserved cells carry their prior shares.
#' @return Tibble: turnout, the three share columns, their priors, observed flags, and
#'   method dummies.
lm_bayes_inputs = function(cells, counts, obs_min = 0.6) {
  d = cells |>
    transmute(
      geo = as.character(geo),
      county = as.character(county),
      method = as.character(method),
      dem2,
      oth,
      exp_votes
    ) |>
    bind_cols(select(counts, c_dem, c_rep, c_oth, c_tot, complete)) |>
    mutate(
      turn = c_tot,
      prior_turn = pmax(exp_votes, 0.5),
      prior_dem = dem2 * (1 - oth),
      prior_rep = (1 - dem2) * (1 - oth),
      prior_oth = oth,
      obs_turn = complete & c_tot > 0,
      obs = obs_turn | (c_tot > 0 & c_tot >= obs_min * prior_turn),
      s_dem = if_else(obs, c_dem / pmax(c_tot, 1), prior_dem),
      s_rep = if_else(obs, c_rep / pmax(c_tot, 1), prior_rep),
      s_oth = if_else(obs, c_oth / pmax(c_tot, 1), prior_oth)
    )
  # method dummies against the method with the most expected votes
  meth = d |> summarize(e = sum(exp_votes), .by = method) |> arrange(desc(e)) |> pull(method)
  dummies = paste0("m_", meth)[-1]
  for (i in seq_along(dummies)) {
    d[[dummies[i]]] = as.numeric(d$method == meth[i + 1])
  }
  list(d = d, dummies = dummies)
}

#' Fit lm_bayes to one race
#'
#' Cory's `enightmodels::forecast_prec()` set up as in his NYC run (file header).
#'
#' @param cells Cells from `build_cells()`.
#' @param returns Standard returns for one race.
#' @param ctrl_form Control formula; the demographic spec is added to it.
#' @param prior_n_turn,prior_n_cands Prior strengths for turnout and candidate shares.
#' @param obs_min As `lm_bayes_inputs()`.
#' @param level Interval level.
#' @param precincts Also return per-precinct rows (`draws_precincts()`).
#' @param demo Demographic spec (demos.R).
#' @param interval "credible", "conf_seq" or "conformal" (results.R).
#' @return `model_result()` list.
fit_lm_bayes = function(
  cells,
  returns,
  ctrl_form = LM_BAYES_FORM,
  prior_n_turn = 0.5,
  prior_n_cands = 1.5,
  obs_min = 0.6,
  level = 0.95,
  precincts = TRUE,
  demo = DEMO_DEFAULT$lm_bayes,
  interval = "credible"
) {
  counts = cell_counts(cells, returns)
  counted = race_counted(cells, counts)
  inp = lm_bayes_inputs(cells, counts, obs_min)
  # turnout prior moved by the cell's congressional district's turnout so far, per method
  # (district_turnout_adjust(), demos.R): lm_bayes's own prior is pseudo-observations at the
  # expectation, so it can't borrow across districts by itself
  inp$d$prior_turn = pmax(inp$d$prior_turn * district_turnout_adjust(cells, counts, returns$state[1]), 0.5)
  X = if (check_demo(demo) == "ratio") {
    matrix(numeric(), nrow(cells), 0)
  } else {
    demo_design(demo_frame(cells, returns$state[1]), demo, drop = all.vars(ctrl_form))
  }
  d = bind_cols(inp$d, as_tibble(X))
  # draws cut for big states so draws x cells stays under LM_BAYES_MAX_CELL_DRAWS (memory)
  nd = cap_draws(nrow(d), as.numeric(Sys.getenv("LM_BAYES_MAX_CELL_DRAWS", unset = "2e7")))

  form = reformulate(c(attr(terms(ctrl_form), "term.labels"), colnames(X), inp$dummies))
  environment(form) = list2env(list(log_s = enightmodels::log_s), parent = globalenv())
  settings = sprintf(
    "enightmodels::forecast_prec(), turnout prior = expected votes x district turnout adjustment; demographics %s, ctrl_form %s; prior_n_turn = %g, prior_n_cands = %g; obs_min = %g; %d draws; interval %s %g",
    demo,
    deparse1(form),
    prior_n_turn,
    prior_n_cands,
    obs_min,
    nd,
    interval,
    100 * level
  )
  if (!any(counts$c_tot > 0)) {
    return(model_result(
      "lm_bayes",
      "no_votes",
      settings,
      counted,
      interval = interval,
      level = level
    ))
  }

  set.seed(SEED)
  f = enightmodels::forecast_prec(
    data = d,
    cands = c(s_dem, s_rep, s_oth),
    turn = turn,
    obs = obs,
    obs_turn = obs_turn,
    ctrl_form = form,
    prior_turn = prior_turn,
    prior_cands = c(prior_dem, prior_rep, prior_oth),
    prior_n_turn = prior_n_turn,
    prior_n_cands = prior_n_cands,
    draws = nd
  )
  s = enightmodels::forecast_summarize(f)
  race_draws = function(turn, vote) {
    turnout = as.numeric(posterior::draws_of(turn))
    shares = posterior::draws_of(vote) # draws x (s_dem, s_rep, s_oth)
    tibble(dem = shares[, 1] * turnout, rep = shares[, 2] * turnout, turnout = turnout) |>
      mutate(twop = dem / (dem + rep))
  }
  T = posterior::draws_of(f$post_turn)
  V = posterior::draws_of(f$post_vote)
  fit = sapply(f$fits_vote, \(m) pmax(predict(m, d), 0))
  fit = fit / pmax(rowSums(fit), 1e-9)
  fc = mutate(
    d,
    fin_dem = colMeans(V[,, 1] * T),
    fin_rep = colMeans(V[,, 2] * T),
    fin_oth = colMeans(V[,, 3] * T),
    fit_dem = fit[, 1],
    fit_rep = fit[, 2],
    fit_oth = fit[, 3]
  )

  draws_result(
    "lm_bayes",
    interval,
    settings,
    counted,
    race_draws(s$post_turn, s$post_vote),
    level,
    returns,
    prior = race_draws(s$prior_turn, s$prior_vote),
    fc = fc,
    cells = if (precincts) draws_precincts(T, V[,, 1] * T, V[,, 2] * T, d, level),
    coef = enightmodels::forecast_coefs(f)
  )
}
