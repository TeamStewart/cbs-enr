# ========================================
## results.R -- what every model returns, and how its interval is made.
##
## Every fit_*() returns model_result(): model, status, settings, conf, race (one row: est_twop,
## lo_twop, hi_twop, p_dem_win, est_turnout, lo_turnout, hi_turnout + what is counted), and
##   draws  race-level draws (dem, rep, turnout, twop), when the interval comes from draws
##   pred   per candidate: pred, pred_min, pred_max, when it doesn't (or there is none)
##   cells  per-precinct rows, PREC_COLS's columns (prec_rows()); point c(dem, rep, tot); coef
## output.R, counties.R and virtual.R use `pred` when a fit has it, else the draws.
##
## Intervals (`interval =` on a fit, or "<model>/<interval>" in a model name; run.R):
##   bootstrap  draws from swing's county bootstrap                       county_bootstrap_95
##   credible   posterior draws: the equal-tailed interval (cred())       posterior_95
##   conf_seq   enightmodels::conf_seq(): the anytime-valid confidence sequence from the ratio
##              of prior to posterior draws of each race total (as forecast_intervals() does)
##                                                                        conf_seq_95
##   conformal  Cory's approximate split-conformal interval (tx-prim-26 model_prec_pois()): on
##              the cells with votes, residual = counted share - the model's fitted share, per
##              party x method; each cell's remaining votes get its predicted share shifted by
##              those residuals' quantiles, and the bounds are summed over cells. Residuals are
##              in-sample and the sum treats cells as perfectly correlated, so it is
##              conservative. Turnout is a point.                       approx_conform_95
##   none       point predictions only                                   NA
## conf_seq and conformal give bounds, not draws, so their fits have no p_dem_win and their
## statewide rows (counties.R) are a point, as for any model without draws.
# ========================================

INTERVALS = c("bootstrap", "credible", "conf_seq", "conformal", "none")
# what each model can give; the first is its default
MODEL_INTERVALS = list(swing = c("bootstrap", "conformal"), ratio_strata = c("none", "conformal"),
                       pois_bayes = c("credible", "conf_seq", "conformal"),
                       lm_bayes = c("credible", "conf_seq", "conformal"), medsl_sur = c("credible", "conformal"))
CONF_LABEL = c(bootstrap = "county_bootstrap_%g", credible = "posterior_%g", conf_seq = "conf_seq_%g",
               conformal = "approx_conform_%g")

# what has been counted so far, the same columns for every model (run.R)
race_counted = function(cells, counts) {
    tibble(
        raw_twop = sum(counts$c_dem) / max(sum(counts$c_dem + counts$c_rep), 1),
        counted_votes = sum(counts$c_tot),
        expected_votes = sum(cells$exp_votes),
        obs_cells = sum(counts$c_tot > 0),
        complete_cells = sum(counts$c_tot > 0 & counts$complete)
    )
}

# The result list every model returns. `est` is the race row's estimates; missing ones are NA.
model_result = function(model, status, settings, counted, est = list(), pred = NULL, cells = NULL,
                        draws = NULL, point = NULL, coef = NULL, interval = "none", level = 0.95) {
    cols = c("est_twop", "lo_twop", "hi_twop", "p_dem_win", "est_turnout", "lo_turnout", "hi_turnout")
    race = as_tibble(c(list(status = status), lapply(setNames(cols, cols), \(c) est[[c]] %||% NA_real_)))
    list(
        model = model, status = status, settings = settings,
        conf = if (interval == "none") NA_character_ else sprintf(CONF_LABEL[[interval]], 100 * level),
        race = bind_cols(race, counted),
        pred = pred, cells = cells, draws = draws, point = point, coef = coef
    )
}

# per-candidate predictions, party from the returns (dem / rep / ind / oth, as output.R)
pred_by_cand = function(pred, returns) {
    pty = distinct(returns, candidate, party) |>
        mutate(candidate = as.character(candidate), party = if_else(party %in% c("dem", "rep", "ind"), party, "oth"))
    pred |>
        mutate(candidate = as.character(candidate)) |>
        summarize(across(any_of(c("pred", "pred_min", "pred_max")), sum), .by = candidate) |>
        left_join(pty, by = "candidate") |>
        mutate(party = coalesce(party, "oth"))
}

# How a model's party totals are split among candidates. cand: candidate, party (dem / rep /
# ind / oth) and rep (counted votes), as reported_by_candidate() (output.R). Adds grp (dem, rep
# or oth: every third party) and w, each candidate's share of grp's votes: a party's votes
# split equally across its candidates, third-party votes in proportion to their counted
# votes (equally if none counted yet).
cand_split = function(cand) {
    cand |>
        mutate(grp = if_else(party %in% c("dem", "rep"), party, "oth")) |>
        mutate(w = if (grp[1] == "oth" && sum(rep) > 0) rep / sum(rep) else 1 / n(), .by = grp)
}

# Per-candidate point predictions from party totals v = c(dem, rep, oth) (cand_split()). No bounds.
cand_from_party = function(v, returns) {
    reported_by_candidate(mutate(returns, candidate = as.character(candidate))) |>
        cand_split() |>
        transmute(candidate, party, pred = unname(v[grp]) * w, pred_min = NA_real_, pred_max = NA_real_)
}

# Bounds on each candidate from bounds on their party (tibble party, pred, pred_min,
# pred_max; party dem / rep / oth): each candidate's prediction scaled by party bound / point.
cand_bounds = function(cand, party_b) {
    grp = if_else(cand$party %in% c("dem", "rep"), cand$party, "oth")
    i = match(grp, party_b$party)
    r = function(col) ifelse(party_b$pred[i] > 0, party_b[[col]][i] / party_b$pred[i], NA_real_)
    mutate(cand, pred_min = pred * r("pred_min"), pred_max = pred * r("pred_max"))
}

# race-level estimates from per-candidate predictions. The two-party interval pairs each
# party's bound with the other's opposite bound (conservative); NA without bounds.
pred_race_est = function(pred) {
    s = function(p, col) sum(pred[[col]][pred$party == p])
    twop = function(d, r) d / (d + r)
    list(
        est_twop = twop(s("dem", "pred"), s("rep", "pred")),
        lo_twop = twop(s("dem", "pred_min"), s("rep", "pred_max")),
        hi_twop = twop(s("dem", "pred_max"), s("rep", "pred_min")),
        est_turnout = sum(pred$pred)
    )
}

# race-level estimates from draws (dem, rep, turnout, twop)
draws_race_est = function(dr, level) {
    ci = cred(dr$twop, level)
    ct = cred(dr$turnout, level)
    list(est_twop = mean(dr$twop), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]],
         p_dem_win = mean(dr$dem > dr$rep), est_turnout = mean(dr$turnout),
         lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]])
}

# Conformal bounds by party (see the header). fc: one row per cell with method, c_dem, c_rep,
# c_oth, c_tot (counted), fin_dem, fin_rep, fin_oth (the model's final votes) and fit_dem,
# fit_rep, fit_oth (its fitted share of the cell's votes, data or not).
conformal_party = function(fc, level = 0.95) {
    a = (1 - level) / 2
    cl = function(x) pmin(pmax(x, 0), 1)
    fin_tot = fc$fin_dem + fc$fin_rep + fc$fin_oth
    rem = pmax(fin_tot - fc$c_tot, 0)
    has = fc$c_tot > 0
    bind_rows(lapply(c("dem", "rep", "oth"), function(p) {
        cnt = fc[[paste0("c_", p)]]; fin = fc[[paste0("fin_", p)]]; fit = fc[[paste0("fit_", p)]]
        e = (cnt / pmax(fc$c_tot, 1) - fit)[has]
        q_all = quantile(e, c(a, 1 - a), names = FALSE)
        q = tibble(method = fc$method[has], e = e) |>
            summarize(lo = quantile(e, a, names = FALSE), hi = quantile(e, 1 - a, names = FALSE), .by = method)
        i = match(fc$method, q$method)
        lo = coalesce(q$lo[i], q_all[1]); hi = coalesce(q$hi[i], q_all[2])
        s = ifelse(rem > 0, cl((fin - cnt) / pmax(rem, 1e-9)), fit)
        tibble(party = p, pred = sum(fin), pred_min = sum(cnt + rem * cl(s + lo)), pred_max = sum(cnt + rem * cl(s + hi)))
    }))
}

# Confidence-sequence bounds from prior and posterior draws of the race totals (dem, rep,
# turnout): the two-party share on its scale, votes on the log scale (as forecast_intervals()
# treats turnout). A bound conf_seq() can't find is NA.
conf_seq_bounds = function(prior, post, level = 0.95) {
    cs = function(a, b) { x = enightmodels::conf_seq(a, b, level = level); x[!is.finite(x)] = NA; x }
    lcs = function(a, b) exp(cs(log(pmax(a, 1)), log(pmax(b, 1))))
    tw = function(d) d$dem / (d$dem + d$rep)
    oth = function(d) pmax(d$turnout - d$dem - d$rep, 0)
    ct = lcs(prior$turnout, post$turnout)
    party = bind_rows(lapply(c("dem", "rep", "oth"), function(p) {
        a = if (p == "oth") oth(prior) else prior[[p]]
        b = if (p == "oth") oth(post) else post[[p]]
        x = lcs(a, b)
        tibble(party = p, pred = mean(b), pred_min = x[["lower"]], pred_max = x[["upper"]])
    }))
    ci = cs(tw(prior), tw(post))
    list(est = list(est_twop = mean(tw(post)), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]],
                    est_turnout = mean(post$turnout), lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]]),
         party = party)
}

# The result of a model with draws, under `interval`:
#   bootstrap / credible   the draws
#   conf_seq               needs `prior`, prior draws of the race totals
#   conformal              needs `fc` (see conformal_party())
# `returns` gives the candidates for conf_seq and conformal, whose bounds go in `pred`.
draws_result = function(model, interval, settings, counted, dr, level, returns,
                        prior = NULL, fc = NULL, cells = NULL, coef = NULL) {
    point = c(dem = mean(dr$dem), rep = mean(dr$rep), tot = mean(dr$turnout))
    res = function(...) model_result(model, "ok", settings, counted, cells = cells, coef = coef,
                                     interval = interval, level = level, ...)
    if (interval %in% c("bootstrap", "credible"))
        return(res(est = draws_race_est(dr, level), draws = dr, point = point))
    cand = cand_from_party(c(dem = point[["dem"]], rep = point[["rep"]], oth = point[["tot"]] - point[["dem"]] - point[["rep"]]),
                           returns)
    if (interval == "conf_seq") {
        b = conf_seq_bounds(prior, dr, level)
        return(res(est = b$est, pred = cand_bounds(cand, b$party), point = point))
    }
    pred = cand_bounds(cand, conformal_party(fc, level))
    res(est = pred_race_est(pred), pred = pred, point = point)
}

# The precinct file's columns (CONTRACT.md section 15.3). Every model's `cells` slot has them
# all but race_id, model and time, which output.R adds.
PREC_COLS = c("race_id", "model", "time", "geo", "county", "method",
              "turn_est", "turn_lo", "turn_hi", "dem_est", "dem_lo", "dem_hi",
              "est_dem2", "cnt_prec", "q_share", "n_obs_modes", "p_win")

# per-precinct rows in that shape: p has geo and any of the other columns; the rest are NA
prec_rows = function(p) {
    cols = setdiff(PREC_COLS, c("race_id", "model", "time"))
    for (c in setdiff(cols, names(p))) p[[c]] = if (c %in% c("county", "method")) NA_character_ else NA_real_
    mutate(p, geo = as.character(geo), county = as.character(county)) |> select(all_of(cols))
}

# Per-precinct rows from cell-level final votes (d: geo, dem, rep, tot, and county if known),
# summed over methods; no interval. Used by swing and ratio_strata.
votes_precincts = function(d) {
    d |>
        summarize(across(c(dem, rep, tot), sum), .by = any_of(c("geo", "county"))) |>
        mutate(turn_est = tot, dem_est = dem / pmax(tot, 1e-9), est_dem2 = dem / pmax(dem + rep, 1e-9)) |>
        prec_rows()
}

# Per-precinct rows from cell-level draws (draws x cells matrices of total, Dem and Rep
# votes), summing each precinct's methods within a draw. d: one row per cell with geo,
# county, obs, c_tot. Used by lm_bayes and pois_bayes.
draws_precincts = function(tot, dem, rep, d, level = 0.95) {
    z = qnorm(1 - (1 - level) / 2)
    g = factor(d$geo, levels = unique(d$geo))
    by_prec = function(M) rowsum(t(M), g, reorder = FALSE)          # precincts x draws
    tot = by_prec(tot); dem = by_prec(dem); rep = by_prec(rep)
    msd = function(M) list(m = rowMeans(M), s = apply(M, 1, sd))
    t_ = msd(tot); s_ = msd(dem / pmax(tot, 1e-9)); d2 = dem / pmax(dem + rep, 1e-9)
    d |>
        summarize(county = county[1], n_obs_modes = sum(obs), cnt_prec = sum(c_tot), .by = geo) |>
        mutate(
            turn_est = t_$m, turn_lo = pmax(t_$m - z * t_$s, 0), turn_hi = t_$m + z * t_$s,
            dem_est = s_$m, dem_lo = pmax(s_$m - z * s_$s, 0), dem_hi = pmin(s_$m + z * s_$s, 1),
            est_dem2 = rowMeans(d2), p_win = rowMeans(d2 > 0.5)
        ) |>
        prec_rows()
}
