# ========================================
## model_swing.R -- swing: the standard uniform swing model.
##
## Cell c = precinct p x method m, in county k. D_p = dem2, w_p = oth, E_c = exp_votes
## (cells.R); d_c, r_c, t_c = Dem, Rep and total votes counted so far (returns.R).
## A cell is REPORTING if d_c + r_c > 0, and COMPLETE if reporting and fully counted.
##
##   1. swing     y_c = d_c / (d_c + r_c) - D_p  =  a_k + g_m + e_c
##                WLS on reporting cells, weights E_c:  feols(y ~ 1 | county + method)
##   2. turnout   l_c = log(t_c / E_c), on complete cells only, PER METHOD m, nested
##                state -> congressional district -> county (swing_turnout_predict()):
##                  l_c = mu_m + delta_{district,m} + delta_{county,m}
##                mu_m = E-weighted mean over the method's complete cells; each district's
##                and county's deviation shrunk toward 0 by tau^2 / (tau^2 + v), the
##                between-district (between-county) variance tau^2 and the sampling variance
##                v of its mean both estimated from tonight's complete cells (re_shrink(),
##                demos.R; no hand-set constant -- tau^2 = 0, no borrowing, until the data
##                show districts differ).
##                A county with little counted borrows from its district before the state
##                (Ansolabehere et al. 2024, "Forecasting Turnout": districts predict turnout
##                far better than counties). A method with no complete cell keeps its
##                expectation (l_c = 0): early-vote turnout is never carried over to Election
##                Day or mail (the same paper: early vote is a poor predictor of the total).
##   3. per cell  V_c = t_c if complete, else max(t_c, E_c * exp(l_c))
##                S_c = clamp(D_p + swing_c, 0.001, 0.999)
##                remaining two-party votes Q_c = (V_c - t_c) * (1 - w_p)
##                Dem_c = d_c + Q_c * S_c,  Rep_c = r_c + Q_c * (1 - S_c)
##   4. race      two-party Dem share = sum Dem_c / sum (Dem_c + Rep_c); turnout = sum V_c
##   Demographics (`demo`, demos.R; default none): the spec's columns X_c enter both
##   regressions as slopes, y_c = a_k + g_m + X_c b + e_c (and likewise for l_c), once
##   there are more than 10 fitted cells per column; cells whose county has no data fall
##   back as above. geostratum terms are dropped: each geostratum is a set of whole counties,
##   so the county effects already hold it (strat_g = ratio and strat_dg = strat_d here).
##   5. interval  `boot` county bootstrap replicates: resample the reporting counties with
##                replacement, the number of times a county is drawn multiplies its
##                weights, refit 1-4. Draws are re-centred on the point fit; interval is
##                mean +/- z * sd; P(Dem win) = share of draws above 0.5.
##      interval = "conformal" instead: no bootstrap; conformal bounds (results.R) with the
##                fitted share of cell c = (1 - w_p) * S_c for Dem, (1 - w_p) * (1 - S_c) for Rep.
# ========================================

#' Turnout predictions: per method, county within district within state
#'
#' Step 2 of the file header. Covariates `x` (if any, and more than 10 complete cells per
#' column) are fit first as slopes with county x method fixed effects; the nested
#' shrinkage then runs on what they leave.
#'
#' @param dd Cell frame with lr (log turnout ratio), method, county, cd.
#' @param fit_rows Logical: complete cells.
#' @param w Weights (expected votes x bootstrap county weight).
#' @param x Covariate columns.
#' @return Numeric vector of predicted log turnout ratios, one per row (0 for a method with
#'   no complete cell).
swing_turnout_predict = function(dd, fit_rows, w, x = character()) {
  use = fit_rows & w > 0
  out = rep(0, nrow(dd))
  if (!any(use)) {
    return(out)
  }
  xb = rep(0, nrow(dd))
  if (length(x) && sum(use) > 10 * length(x)) {
    fit = tryCatch(
      fixest::feols(
        stats::as.formula(paste("lr ~", paste(x, collapse = " + "), "| county^method")),
        data = dd[use, ],
        weights = w[use],
        notes = FALSE,
        warn = FALSE
      ),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      b = stats::coef(fit)
      b = b[intersect(x, names(b))]
      if (length(b)) xb = as.vector(as.matrix(dd[names(b)]) %*% b)
    }
  }
  r = dd$lr - xb
  for (m in unique(dd$method)) {
    i = dd$method == m
    u = i & use
    if (!any(u)) {
      next # nothing complete for this method: its expectation, not another method's ratio
    }
    f = tibble(cd = dd$cd[u], county = dd$county[u], r = r[u], w = w[u])
    mu = sum(f$w * f$r) / sum(f$w)
    by_cd = re_shrink(f$r, f$w, f$cd, mu = mu)
    f$d_cd = coalesce(unname(by_cd[f$cd]), 0)
    by_cty = re_shrink(f$r - f$d_cd, f$w, f$county, mu = mu)
    d_cd = coalesce(unname(by_cd[dd$cd[i]]), 0)
    d_cty = coalesce(unname(by_cty[dd$county[i]]), 0)
    out[i] = mu + d_cd + d_cty
  }
  out + xb
}

#' Predictions from county + method fixed effects
#'
#' Weighted fit on `fit_rows` with `fixest::feols()`. Covariates `x` are dropped unless there
#' are more than 10 fitted cells per column. Cells whose county (or method) has no data fall
#' back to the weighted mean for that method, then overall.
#'
#' @param dd Cell frame.
#' @param yname Outcome column.
#' @param fit_rows Logical: rows to fit on.
#' @param w Weights.
#' @param x Covariate columns (slopes).
#' @return Numeric vector of predictions, one per row of `dd`.
swing_fe_predict = function(dd, yname, fit_rows, w, x = character()) {
  fr = dd[fit_rows & w > 0, ]
  if (nrow(fr) < 2) {
    return(rep(if (nrow(fr)) fr[[yname]][1] else 0, nrow(dd)))
  }
  if (nrow(fr) <= 10 * length(x)) {
    x = character()
  }
  fit = tryCatch(
    fixest::feols(
      stats::as.formula(paste(yname, "~", paste(c("1", x), collapse = " + "), "| county + method")),
      data = fr,
      weights = w[fit_rows & w > 0],
      notes = FALSE,
      warn = FALSE
    ),
    error = function(e) NULL
  )
  p = if (is.null(fit)) rep(NA_real_, nrow(dd)) else suppressWarnings(predict(fit, newdata = dd))
  by_m = tapply(fr[[yname]] * w[fit_rows & w > 0], fr$method, sum) /
    tapply(w[fit_rows & w > 0], fr$method, sum)
  all_m = sum(fr[[yname]] * w[fit_rows & w > 0]) / sum(w[fit_rows & w > 0])
  fb = unname(ifelse(dd$method %in% names(by_m), by_m[dd$method], all_m))
  if_else(is.na(p), fb, p)
}

#' One fit of the swing model's steps 1-3
#'
#' @param d Cell frame with counts and baselines.
#' @param cw County weights (all 1 for the point fit; bootstrap counts otherwise).
#' @param x Covariate columns.
#' @return Tibble, one row per cell: geo, method, county, swing, log_turnout_ratio, s2
#'   (projected two-party Dem share), dem, rep, tot (projected final votes).
swing_once = function(d, cw, x = character()) {
  w = d$exp_votes * cw[d$county]
  w[is.na(w)] = 0
  rep_cell = d$c_dem + d$c_rep > 0
  done = rep_cell & d$complete
  sw = swing_fe_predict(d, "y", rep_cell, w, x)
  lr = swing_turnout_predict(d, done, w, x)
  tot = if_else(done, d$c_tot, pmax(d$c_tot, d$exp_votes * exp(lr)))
  s = pmin(pmax(d$dem2 + sw, 0.001), 0.999)
  rem2 = (tot - d$c_tot) * (1 - d$oth)
  tibble(
    geo = d$geo,
    method = d$method,
    county = d$county,
    swing = sw,
    log_turnout_ratio = lr,
    s2 = s,
    dem = if_else(done, d$c_dem, d$c_dem + rem2 * s),
    rep = if_else(done, d$c_rep, d$c_rep + rem2 * (1 - s)),
    tot = tot
  )
}

#' Fit the swing model to one race
#'
#' Method in the file header: steps 1-4 for the point estimate, step 5 for the interval.
#'
#' @param cells Cells from `build_cells()`.
#' @param returns Standard returns for one race.
#' @param boot Number of county bootstrap replicates.
#' @param level Interval level.
#' @param demo Demographic spec (demos.R); geostratum terms are absorbed by county.
#' @param interval "bootstrap" or "conformal" (results.R).
#' @return `model_result()` list.
fit_swing = function(
  cells,
  returns,
  boot = 100,
  level = 0.95,
  demo = DEMO_DEFAULT$swing,
  interval = "bootstrap"
) {
  if (!requireNamespace("fixest", quietly = TRUE)) {
    stop('The uniform swing model needs the fixest package. In R: install.packages("fixest")')
  }
  counts = cell_counts(cells, returns)
  d = bind_cols(
    select(cells, geo, method, county, dem2, oth, exp_votes),
    select(counts, c_dem, c_rep, c_oth, c_tot, complete)
  ) |>
    mutate(cd = cell_cd(cells, returns$state[1])) |>
    mutate(y = c_dem / pmax(c_dem + c_rep, 1) - dem2, lr = log(pmax(c_tot, 0.5) / exp_votes))
  X = if (demo == "ratio") {
    matrix(numeric(), nrow(d), 0)
  } else {
    demo_design(demo_frame(cells, returns$state[1]), check_demo(demo), drop = "geostratum")
  }
  d = bind_cols(d, as_tibble(X))
  x = colnames(X)

  settings = sprintf(
    "uniform swing: feols(swing ~ 1 | county + method), weighted by expected votes; turnout per method on fully counted cells, county within congressional district within state (shrinkage from variance components estimated tonight), no carry-over between methods; demographics %s; %s; %g%% interval",
    demo,
    if (interval == "bootstrap") sprintf("%d county bootstrap replicates", boot) else interval,
    100 * level
  )
  counted = race_counted(cells, counts)
  # nothing to estimate before any votes are counted
  if (!any(d$c_tot > 0)) {
    return(model_result("swing", "no_votes", settings, counted, interval = interval, level = level))
  }

  ctys = unique(d$county)
  pt = swing_once(d, setNames(rep(1, length(ctys)), ctys), x)
  point = c(dem = sum(pt$dem), rep = sum(pt$rep), tot = sum(pt$tot))
  if (interval == "conformal") {
    fc = mutate(
      d,
      fin_dem = pt$dem,
      fin_rep = pt$rep,
      fin_oth = pt$tot - pt$dem - pt$rep,
      fit_dem = (1 - oth) * pt$s2,
      fit_rep = (1 - oth) * (1 - pt$s2),
      fit_oth = oth
    )
    v = c(dem = sum(pt$dem), rep = sum(pt$rep), oth = sum(pt$tot - pt$dem - pt$rep))
    pred = cand_bounds(cand_from_party(v, returns), conformal_party(fc, level))
    return(model_result(
      "swing",
      "ok",
      settings,
      counted,
      est = pred_race_est(pred),
      pred = pred,
      cells = votes_precincts(pt),
      point = point,
      interval = interval,
      level = level
    ))
  }
  rep_ctys = unique(d$county[d$c_tot > 0])
  set.seed(SEED)
  draws = lapply(seq_len(boot), function(b) {
    cw = setNames(rep(1, length(ctys)), ctys)
    if (length(rep_ctys) > 1) {
      k = table(factor(sample(rep_ctys, length(rep_ctys), replace = TRUE), levels = rep_ctys))
      cw[rep_ctys] = as.numeric(k)
    }
    o = swing_once(d, cw, x)
    c(s2 = sum(o$dem) / sum(o$dem + o$rep), turn = sum(o$tot), dem = sum(o$dem), rep = sum(o$rep))
  })
  S2 = vapply(draws, `[[`, 0, "s2")
  TT = vapply(draws, `[[`, 0, "turn")
  DV = vapply(draws, `[[`, 0, "dem")
  RV = vapply(draws, `[[`, 0, "rep")
  # centre the bootstrap draws on the point fit
  S2 = S2 - mean(S2) + sum(pt$dem) / sum(pt$dem + pt$rep)
  TT = TT - mean(TT) + sum(pt$tot)
  DV = DV - mean(DV) + sum(pt$dem)
  RV = RV - mean(RV) + sum(pt$rep)
  ci = cred(S2, level)
  ct = cred(TT, level)

  model_result(
    "swing",
    "ok",
    settings,
    counted,
    est = list(
      est_twop = mean(S2),
      lo_twop = ci[["lower"]],
      hi_twop = ci[["upper"]],
      p_dem_win = mean(S2 > 0.5),
      est_turnout = mean(TT),
      lo_turnout = ct[["lower"]],
      hi_turnout = ct[["upper"]]
    ),
    cells = votes_precincts(pt),
    point = point,
    draws = tibble(twop = S2, turnout = TT, dem = DV, rep = RV),
    interval = interval,
    level = level
  )
}
