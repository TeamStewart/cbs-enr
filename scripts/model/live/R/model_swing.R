# ========================================
## model_swing.R -- swing: the standard uniform swing model.
##
## Cell c = precinct p x method m, in county k. D_p = dem2, w_p = oth, E_c = exp_votes
## (cells.R); d_c, r_c, t_c = Dem, Rep and total votes counted so far (returns.R).
## A cell is REPORTING if d_c + r_c > 0, and COMPLETE if reporting and fully counted.
##
##   1. swing     y_c = d_c / (d_c + r_c) - D_p  =  a_k + g_m + e_c
##                WLS on reporting cells, weights E_c:  feols(y ~ 1 | county + method)
##   2. turnout   l_c = log(t_c / E_c)  =  b_k + h_m + u_c,  on complete cells only
##                (if no cell is complete yet, l_c is taken as 0: turnout = expectation)
##      A county (or method) with no data falls back to the E-weighted mean for that
##      method, then the overall mean.
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

# predictions from county + method fixed effects; cells whose county (or method) has no
# data fall back to the weighted average for that method, then overall
swing_fe_predict = function(dd, yname, fit_rows, w, x = character()) {
  fr = dd[fit_rows & w > 0, ]
  if (nrow(fr) < 2) return(rep(if (nrow(fr)) fr[[yname]][1] else 0, nrow(dd)))
  if (nrow(fr) <= 10 * length(x)) x = character()
  fit = tryCatch(fixest::feols(stats::as.formula(paste(yname, "~", paste(c("1", x), collapse = " + "), "| county + method")), data = fr,
                               weights = w[fit_rows & w > 0], notes = FALSE, warn = FALSE),
                 error = function(e) NULL)
  p = if (is.null(fit)) rep(NA_real_, nrow(dd)) else suppressWarnings(predict(fit, newdata = dd))
  by_m = tapply(fr[[yname]] * w[fit_rows & w > 0], fr$method, sum) / tapply(w[fit_rows & w > 0], fr$method, sum)
  all_m = sum(fr[[yname]] * w[fit_rows & w > 0]) / sum(w[fit_rows & w > 0])
  fb = unname(ifelse(dd$method %in% names(by_m), by_m[dd$method], all_m))
  if_else(is.na(p), fb, p)
}

# one fit of steps 1-3; cw = county weights (all 1 for the point fit)
swing_once = function(d, cw, x = character()) {
  w = d$exp_votes * cw[d$county]
  w[is.na(w)] = 0
  rep_cell = d$c_dem + d$c_rep > 0
  done = rep_cell & d$complete
  sw = swing_fe_predict(d, "y", rep_cell, w, x)
  lr = if (any(done)) swing_fe_predict(d, "lr", done, w, x) else rep(0, nrow(d))
  tot = if_else(done, d$c_tot, pmax(d$c_tot, d$exp_votes * exp(lr)))
  s = pmin(pmax(d$dem2 + sw, 0.001), 0.999)
  rem2 = (tot - d$c_tot) * (1 - d$oth)
  tibble(geo = d$geo, method = d$method, county = d$county, swing = sw, log_turnout_ratio = lr, s2 = s,
         dem = if_else(done, d$c_dem, d$c_dem + rem2 * s),
         rep = if_else(done, d$c_rep, d$c_rep + rem2 * (1 - s)), tot = tot)
}

# cells:  build_cells() output; returns: standard returns for ONE race (returns.R)
# seed:   set before the bootstrap so the same snapshot always gives the same interval;
#         NULL leaves the random-number stream alone
# demo:   demographic spec (demos.R)
# interval: "bootstrap" or "conformal" (results.R)
fit_swing = function(cells, returns, boot = 100, level = 0.95, seed = 2026, demo = DEMO_DEFAULT$swing,
                     interval = "bootstrap") {
  if (!requireNamespace("fixest", quietly = TRUE))
    stop('The uniform swing model needs the fixest package. In R: install.packages("fixest")')
  counts = cell_counts(cells, returns)
  d = bind_cols(select(cells, geo, method, county, dem2, oth, exp_votes),
                select(counts, c_dem, c_rep, c_oth, c_tot, complete)) |>
    mutate(y = c_dem / pmax(c_dem + c_rep, 1) - dem2,
           lr = log(pmax(c_tot, 0.5) / exp_votes))
  X = if (demo == "ratio") matrix(numeric(), nrow(d), 0) else demo_design(demo_frame(cells, returns$state[1]), check_demo(demo), drop = "geostratum")
  d = bind_cols(d, as_tibble(X))
  x = colnames(X)

  settings = sprintf("uniform swing: feols(swing ~ 1 | county + method), weighted by expected votes; turnout feols(log ratio ~ 1 | county + method) on fully counted cells; demographics %s; %s; %g%% interval", demo,
                     if (interval == "bootstrap") sprintf("%d county bootstrap replicates", boot) else interval, 100 * level)
  counted = race_counted(cells, counts)
  # nothing to estimate before any votes are counted
  if (!any(d$c_tot > 0)) return(model_result("swing", "no_votes", settings, counted, interval = interval, level = level))

  ctys = unique(d$county)
  pt = swing_once(d, setNames(rep(1, length(ctys)), ctys), x)
  if (interval == "conformal") {
    fc = mutate(d, fin_dem = pt$dem, fin_rep = pt$rep, fin_oth = pt$tot - pt$dem - pt$rep,
                fit_dem = (1 - oth) * pt$s2, fit_rep = (1 - oth) * (1 - pt$s2), fit_oth = oth)
    v = c(dem = sum(pt$dem), rep = sum(pt$rep), oth = sum(pt$tot - pt$dem - pt$rep))
    pred = cand_bounds(cand_from_party(v, returns), conformal_party(fc, level))
    return(model_result("swing", "ok", settings, counted, est = pred_race_est(pred), pred = pred, cells = pt,
                        interval = interval, level = level))
  }
  rep_ctys = unique(d$county[d$c_tot > 0])
  if (!is.null(seed)) set.seed(seed)
  draws = lapply(seq_len(boot), function(b) {
    cw = setNames(rep(1, length(ctys)), ctys)
    if (length(rep_ctys) > 1) {
      k = table(factor(sample(rep_ctys, length(rep_ctys), replace = TRUE), levels = rep_ctys))
      cw[rep_ctys] = as.numeric(k)
    }
    o = swing_once(d, cw, x)
    c(s2 = sum(o$dem) / sum(o$dem + o$rep), turn = sum(o$tot), dem = sum(o$dem), rep = sum(o$rep))
  })
  S2 = vapply(draws, `[[`, 0, "s2"); TT = vapply(draws, `[[`, 0, "turn")
  DV = vapply(draws, `[[`, 0, "dem"); RV = vapply(draws, `[[`, 0, "rep")
  # centre the bootstrap draws on the point fit
  S2 = S2 - mean(S2) + sum(pt$dem) / sum(pt$dem + pt$rep); TT = TT - mean(TT) + sum(pt$tot)
  DV = DV - mean(DV) + sum(pt$dem); RV = RV - mean(RV) + sum(pt$rep)
  ci = cred(S2, level); ct = cred(TT, level)

  model_result("swing", "ok", settings, counted,
               est = list(est_twop = mean(S2), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]],
                          p_dem_win = mean(S2 > 0.5), est_turnout = mean(TT),
                          lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]]),
               cells = pt, draws = tibble(twop = S2, turnout = TT, dem = DV, rep = RV),
               interval = interval, level = level)
}
