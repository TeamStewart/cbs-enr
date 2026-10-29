# ========================================
## model_pois_bayes.R -- pois_bayes: a precinct Poisson model on Cory McCartan's
## enightmodels::pois_bayes() (Bayesian Poisson regression, Laplace approximation).
##
## The same kind of model as Cory's tx-prim-26 model_prec_pois() (each party's votes per
## precinct x method ~ Poisson, log expected votes as the offset, Cory's demographic specs),
## with priors instead of a fallback cascade and posterior predictive draws, so it gets
## p_dem_win and statewide intervals.
##
## Per method, two parts, split as in forecast_prec() and the swing model:
##   turnout  votes_c ~ Poisson(E_c * exp(b0 + X b + u_county)), on cells counted out so far
##   shares   votes_cp ~ Poisson(n_c * s_cp * exp(b0p + X bp + u_county,p)) per party p (dem,
##            rep, oth), on every reporting cell -- a partial count is a sample of the cell's
##            share, not a bound on it
## with E_c the cell's expected votes, n_c its votes counted so far and s_cp the baseline
## share. Each cell's final votes: turnout drawn from the posterior predictive (counted-out
## cells keep their count, partial ones are floored at it), and the votes still to come split
## by party with posterior predictive draws of the party counts at the remaining expected votes.
## X = a demographic spec from demos.R as fixed effects (pois_bayes takes one grouping factor,
## so county is the random intercept); default Cory's strat_dg_cov: dem2, dem2^2, ages 18-29
## and 65+, white college and non-college, standardised, plus geostratum and stratum_demo dummies. Priors centre every fit on the
## baseline, with prior_sd the prior sd of the log rate: half its variance on the intercept, a
## quarter on the county intercepts, a quarter shared across the covariates; the intercept's
## prior mean is set so the prior mean rate is the baseline. A method with no counted-out cell keeps the prior for turnout. Precinct counts
## are overdispersed, so draws are widened by the Pearson dispersion of each fit
## (quasi-Poisson; see pois_bayes_draws()).
##
## Interval (results.R): credible (default) from the draws; conf_seq from the posterior and
## prior predictive draws of the race totals (prior: turnout and party counts at the expected
## votes, as if nothing were counted); conformal on the fitted party shares of each cell.
# ========================================

#' One row per cell for the pois_bayes fit
#'
#' @param cells Cells from `build_cells()`.
#' @param counts `cell_counts()` for those cells.
#' @param state Two-letter state code (for `demo_frame()`).
#' @param demo Demographic spec (demos.R).
#' @return List: d (the cells with counts and completeness, plus the spec's design from
#'   `demo_design()`: continuous columns centred and scaled, weighted by expected votes;
#'   dummies 0/1) and x (the design's column names).
pois_bayes_inputs = function(cells, counts, state, demo) {
  d = demo_frame(cells, state) |>
    mutate(county = as.character(county), method = as.character(method)) |>
    bind_cols(select(counts, c_dem, c_rep, c_oth, c_tot, complete)) |>
    mutate(obs = complete & c_tot > 0)
  X = demo_design(d, demo)
  w = d$exp_votes / sum(d$exp_votes)
  for (j in seq_len(ncol(X))) {
    if (!all(X[, j] %in% 0:1)) {
      m = sum(w * X[, j])
      X[, j] = (X[, j] - m) / max(sqrt(sum(w * (X[, j] - m)^2)), 1e-9)
    }
  }
  list(d = bind_cols(d, as_tibble(X)), x = colnames(X))
}

#' Posterior predictive count draws from a pois_bayes() fit
#'
#' Fits `y ~ form` with `enightmodels::pois_bayes()` on the rows of `d` where `use`. Two
#' corrections to a plain Poisson fit:
#' * the intercept's prior mean is -v/2, v the prior variance of the linear predictor, so
#'   the prior mean of each rate is the baseline (exp of a normal has mean exp(v/2));
#' * overdispersion: phi = Pearson chi-square / df on the fitted rows (at least 1), and
#'   each draw's deviation from the point prediction is scaled by sqrt(phi)
#'   (quasi-Poisson).
#'
#' @param d Cell frame.
#' @param y Outcome column (counts).
#' @param use Logical: rows to fit on.
#' @param off_fit,off_new Log exposure for fitting and for prediction.
#' @param form Right-hand-side formula (fixed effects).
#' @param draws Number of draws.
#' @param pr Prior settings (sd of the log rate and its split).
#' @param off_prior Log exposure for prior predictive draws (conf_seq); NULL for none.
#' @return List: post (draws x rows of `d`), rate (the fitted rate multiplier exp(X b + u)
#'   of each row), prior (prior predictive draws at `off_prior`, else NULL).
pois_bayes_draws = function(d, y, use, off_fit, off_new, form, draws, pr, off_prior = NULL) {
  X = model.matrix(form, d)
  # the prior variance of the log rate, pr$sd^2: half on the intercept, a quarter on the
  # county intercepts, a quarter shared across the covariates (on average over rows)
  x2 = mean(rowSums(X[, -1, drop = FALSE]^2))
  sd = c(pr$sd * sqrt(0.5), rep(pr$sd * sqrt(0.25 / max(x2, 1e-9)), ncol(X) - 1))
  group = pr$sd * sqrt(0.25)
  v = pr$sd^2
  d$y = y
  d$use = use
  d$off = off_fit
  fit = enightmodels::pois_bayes(
    update(form, y ~ .),
    d,
    subset = use,
    offset = off,
    group = county,
    prior_beta_mean = c(-v / 2, rep(0, ncol(X) - 1)),
    prior_beta_sd = sd,
    prior_group_sd = group
  )
  phi = 1
  if (sum(use) > ncol(X) + 10) {
    mu = predict(fit, d[use, ])
    phi = max(1, sum((y[use] - mu)^2 / pmax(mu, 0.5)) / (sum(use) - ncol(X)))
  }
  d$off = off_new
  M = matrix(
    posterior::draws_of(enightmodels::posterior_predict(fit, d, draws = draws)),
    nrow = draws
  )
  mu = predict(fit, d)
  if (phi > 1) {
    M = pmax(
      matrix(mu, draws, nrow(d), byrow = TRUE) +
        sqrt(phi) * (M - matrix(mu, draws, nrow(d), byrow = TRUE)),
      0
    )
  }
  P = NULL
  if (!is.null(off_prior)) {
    d$off = off_prior
    P = matrix(
      posterior::draws_of(enightmodels::prior_predict(fit, d, draws = draws)),
      nrow = draws
    )
  }
  list(post = M, rate = mu / exp(off_new), prior = P)
}

#' Fit one vote method's cells
#'
#' Turnout on counted-out cells, then each party's votes on reporting cells (file header).
#'
#' @param d One method's rows of `pois_bayes_inputs()`.
#' @param form Right-hand-side formula.
#' @param draws Number of draws.
#' @param pr Prior settings.
#' @param prior Also return prior draws (conf_seq).
#' @return List: V (final votes by party, dem / rep / oth, each draws x cells), fit (fitted
#'   share of each cell's votes by party), prior (prior final votes by party, or NULL).
pois_bayes_method = function(d, form, draws, pr, prior = FALSE) {
  n = nrow(d)
  cnt = function(x) matrix(x, draws, n, byrow = TRUE)
  lg = function(x) log(pmax(x, 0.5))
  base = list(dem = (1 - d$oth) * d$dem2, rep = (1 - d$oth) * (1 - d$dem2), oth = d$oth)
  votes = list(dem = d$c_dem, rep = d$c_rep, oth = d$c_oth)
  off_p = function(x) if (prior) lg(x)

  Tf = pois_bayes_draws(
    d,
    d$c_tot,
    d$obs,
    lg(d$exp_votes),
    lg(d$exp_votes),
    form,
    draws,
    pr,
    off_p(d$exp_votes)
  )
  T = ifelse(cnt(d$obs), cnt(d$c_tot), pmax(Tf$post, cnt(d$c_tot)))
  rem = T - cnt(d$c_tot)

  # party counts at the remaining expected votes, normalised to shares of what remains
  e_rem = pmax(colMeans(rem), 1)
  R = lapply(names(base), \(p) {
    pois_bayes_draws(
      d,
      votes[[p]],
      d$c_tot > 0,
      lg(d$c_tot * base[[p]]),
      lg(e_rem * base[[p]]),
      form,
      draws,
      pr,
      off_p(d$exp_votes * base[[p]])
    )
  })
  names(R) = names(base)
  R_tot = R$dem$post + R$rep$post + R$oth$post
  # fitted share: baseline share x fitted rate, normalised (not the floored offsets above)
  fit = lapply(setNames(names(base), names(base)), \(p) base[[p]] * R[[p]]$rate)
  fit_tot = fit$dem + fit$rep + fit$oth
  P_tot = if (prior) R$dem$prior + R$rep$prior + R$oth$prior
  by_party = function(f) lapply(setNames(names(base), names(base)), f)
  list(
    V = by_party(\(p) {
      cnt(votes[[p]]) + rem * ifelse(R_tot > 0, R[[p]]$post / pmax(R_tot, 1), cnt(base[[p]]))
    }),
    fit = by_party(\(p) fit[[p]] / pmax(fit_tot, 1e-9)),
    prior = if (prior) {
      by_party(\(p) Tf$prior * ifelse(P_tot > 0, R[[p]]$prior / pmax(P_tot, 1), cnt(base[[p]])))
    }
  )
}

#' Fit pois_bayes to one race
#'
#' @param cells Cells from `build_cells()`.
#' @param returns Standard returns for one race.
#' @param prior_sd Prior sd of the log rate (split across intercept, county and
#'   covariates; file header).
#' @param demo Demographic spec (demos.R), as fixed effects.
#' @param interval "credible", "conf_seq" or "conformal" (results.R).
#' @param level Interval level.
#' @param precincts Also return per-precinct rows (`draws_precincts()`).
#' @return `model_result()` list.
fit_pois_bayes = function(
  cells,
  returns,
  prior_sd = 0.2,
  demo = DEMO_DEFAULT$pois_bayes,
  interval = "credible",
  level = 0.95,
  precincts = TRUE
) {
  draws = DRAWS
  counts = cell_counts(cells, returns)
  counted = race_counted(cells, counts)
  settings = sprintf(
    paste0(
      "enightmodels::pois_bayes() per method: turnout on counted-out cells (offset log expected ",
      "votes), party votes on reporting cells (offset log counted x baseline share); demographics %s, ",
      "county intercepts; prior sd of the log rate %g; quasi-Poisson ",
      "dispersion; %d draws; interval %s %g"
    ),
    check_demo(demo),
    prior_sd,
    draws,
    interval,
    100 * level
  )
  if (!any(counts$c_tot > 0)) {
    return(model_result(
      "pois_bayes",
      "no_votes",
      settings,
      counted,
      interval = interval,
      level = level
    ))
  }

  inp = pois_bayes_inputs(cells, counts, returns$state[1], demo)
  d = inp$d
  form = if (length(inp$x)) reformulate(inp$x) else ~1
  pr = list(sd = prior_sd)

  set.seed(SEED)
  zero = function() {
    list(
      dem = matrix(0, draws, nrow(d)),
      rep = matrix(0, draws, nrow(d)),
      oth = matrix(0, draws, nrow(d))
    )
  }
  V = zero()
  P = zero()
  fit = list(dem = numeric(nrow(d)), rep = numeric(nrow(d)), oth = numeric(nrow(d)))
  for (m in unique(d$method)) {
    i = which(d$method == m)
    vm = pois_bayes_method(d[i, ], form, draws, pr, prior = interval == "conf_seq")
    for (p in names(V)) {
      V[[p]][, i] = vm$V[[p]]
      fit[[p]][i] = vm$fit[[p]]
      if (!is.null(vm$prior)) P[[p]][, i] = vm$prior[[p]]
    }
  }
  tot = V$dem + V$rep + V$oth
  race_draws = function(V) {
    tibble(dem = rowSums(V$dem), rep = rowSums(V$rep), turnout = rowSums(V$dem + V$rep + V$oth)) |>
      mutate(twop = dem / (dem + rep))
  }
  fc = mutate(
    d,
    fin_dem = colMeans(V$dem),
    fin_rep = colMeans(V$rep),
    fin_oth = colMeans(V$oth),
    fit_dem = fit$dem,
    fit_rep = fit$rep,
    fit_oth = fit$oth
  )

  draws_result(
    "pois_bayes",
    interval,
    settings,
    counted,
    race_draws(V),
    level,
    returns,
    prior = if (interval == "conf_seq") race_draws(P),
    fc = fc,
    cells = if (precincts) draws_precincts(tot, V$dem, V$rep, d, level)
  )
}
