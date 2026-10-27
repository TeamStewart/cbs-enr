# ========================================
## model_cprec.R -- model_cory_prec: Cory McCartan's Bayesian precinct forecaster,
## enightmodels::forecast_prec(), set up as in his 2025 NYC mayoral run
## (scripts/model/modeling-cory/nyc/R/nyc_enr_forecast.R).
##
## One conjugate regression (lm_bayes) for log turnout, fit on counted-out cells, and one per
## candidate column for its share of all votes, fit on observed cells; each with a prior
## centred on our baseline (prior_turn = expected votes, prior shares = dem2 / oth), worth
## prior_n_turn / prior_n_cands observations. Counted values replace the draws where they are
## in. The default spec is NYC's `simple_pres`, log_s(prior turnout) + past Dem share, with the
## default prior weights of Cory's NYC wrapper (prior_n_turn = 0.5, prior_n_cands = 1.5).
##
## Differences from NYC, all because our cells are precinct x method:
##   - the method enters as 0/1 columns, not a factor: lm_bayes() drops factor levels absent
##     from the observed rows (mail, early in the night) and its design matrices then differ.
##   - obs (share observed) = counted out, or counted >= obs_min of expected votes (NYC:
##     precinct total > 1.2 x its early vote); obs_turn = counted out (NYC: >= 98% reporting).
##   - candidates are dem / rep / oth; output.R splits oth across the third-party candidates.
## Draws are kept, so this model gets intervals, p_dem_win and statewide intervals the same way
## model_medsl does.
# ========================================

CPREC_FORM = ~ log_s(prior_turn) + dem2
CPREC_DRAWS = 500L

# draws are cut for big states so draws x cells stays under CPREC_MAX_CELL_DRAWS (memory)
cprec_draws = function(n_cell, draws = CPREC_DRAWS) {
    budget = as.numeric(Sys.getenv("CPREC_MAX_CELL_DRAWS", unset = "2e7"))
    max(50L, min(as.integer(draws), as.integer(budget %/% max(n_cell, 1))))
}

# one row per cell: turnout, the three share columns, their priors, obs flags, method dummies
cprec_inputs = function(cells, counts, obs_min = 0.6) {
    d = cells |>
        transmute(geo = as.character(geo), county = as.character(county), method = as.character(method),
                  dem2, oth, exp_votes) |>
        bind_cols(select(counts, c_dem, c_rep, c_oth, c_tot, complete)) |>
        mutate(
            turn = c_tot,
            prior_turn = pmax(exp_votes, 0.5),
            prior_dem = dem2 * (1 - oth), prior_rep = (1 - dem2) * (1 - oth), prior_oth = oth,
            obs_turn = complete & c_tot > 0,
            obs = obs_turn | (c_tot > 0 & c_tot >= obs_min * prior_turn),
            s_dem = if_else(obs, c_dem / pmax(c_tot, 1), prior_dem),
            s_rep = if_else(obs, c_rep / pmax(c_tot, 1), prior_rep),
            s_oth = if_else(obs, c_oth / pmax(c_tot, 1), prior_oth)
        )
    # method dummies against the method with the most expected votes
    meth = d |> summarize(e = sum(exp_votes), .by = method) |> arrange(desc(e)) |> pull(method)
    dummies = paste0("m_", meth)[-1]
    for (i in seq_along(dummies)) d[[dummies[i]]] = as.numeric(d$method == meth[i + 1])
    list(d = d, dummies = dummies)
}

# per-precinct summaries from the draws, summing each precinct's methods within a draw
cprec_precincts = function(f, d, level = 0.95) {
    z = qnorm(1 - (1 - level) / 2)
    g = factor(d$geo, levels = unique(d$geo))
    by_prec = function(M) rowsum(t(M), g, reorder = FALSE)          # precincts x draws
    T = posterior::draws_of(f$post_turn)
    V = posterior::draws_of(f$post_vote)
    tot = by_prec(T)
    dem = by_prec(V[, , 1] * T)
    rep = by_prec(V[, , 2] * T)
    msd = function(M) list(m = rowMeans(M), s = apply(M, 1, sd))
    t_ = msd(tot); s_ = msd(dem / pmax(tot, 1e-9)); d2 = dem / pmax(dem + rep, 1e-9)
    d |>
        summarize(county = county[1], n_obs_cells = sum(obs), cnt_prec = sum(c_tot), .by = geo) |>
        mutate(
            turn_est = t_$m, turn_lo = pmax(t_$m - z * t_$s, 0), turn_hi = t_$m + z * t_$s,
            dem_est = s_$m, dem_lo = pmax(s_$m - z * s_$s, 0), dem_hi = pmin(s_$m + z * s_$s, 1),
            est_dem2 = rowMeans(d2), p_dem_win = rowMeans(d2 > 0.5)
        )
}

# cells: build_cells() output; returns: standard returns for ONE race (returns.R)
fit_cprec = function(cells, returns, ctrl_form = CPREC_FORM, prior_n_turn = 0.5, prior_n_cands = 1.5,
                     obs_min = 0.6, draws = CPREC_DRAWS, level = 0.95, precincts = TRUE, seed = 2026) {
    counts = cell_counts(cells, returns)
    counted = race_counted(cells, counts)
    inp = cprec_inputs(cells, counts, obs_min)
    d = inp$d
    nd = cprec_draws(nrow(d), draws)

    form = reformulate(c(attr(terms(ctrl_form), "term.labels"), inp$dummies))
    environment(form) = list2env(list(log_s = enightmodels::log_s), parent = globalenv())
    settings = sprintf("enightmodels::forecast_prec(), ctrl_form %s; prior_n_turn = %g, prior_n_cands = %g; obs_min = %g; %d draws; cred_int_%g",
                       deparse1(form), prior_n_turn, prior_n_cands, obs_min, nd, 100 * level)
    if (!any(counts$c_tot > 0)) return(cory_result("cprec", "no_votes", settings, counted))

    set.seed(seed)
    f = enightmodels::forecast_prec(
        data = d, cands = c(s_dem, s_rep, s_oth), turn = turn, obs = obs, obs_turn = obs_turn,
        ctrl_form = form, prior_turn = prior_turn, prior_cands = c(prior_dem, prior_rep, prior_oth),
        prior_n_turn = prior_n_turn, prior_n_cands = prior_n_cands, draws = nd
    )
    s = enightmodels::forecast_summarize(f)
    turnout = as.numeric(posterior::draws_of(s$post_turn))
    shares = posterior::draws_of(s$post_vote)            # draws x (s_dem, s_rep, s_oth)
    dr = tibble(dem = shares[, 1] * turnout, rep = shares[, 2] * turnout, turnout = turnout) |>
        mutate(twop = dem / (dem + rep))
    ci = cred(dr$twop, level)
    ct = cred(dr$turnout, level)

    cory_result(
        "cprec", "ok", settings, counted,
        est = list(est_twop = mean(dr$twop), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]],
                   p_dem_win = mean(dr$dem > dr$rep), est_turnout = mean(dr$turnout),
                   lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]]),
        cells = if (precincts) cprec_precincts(f, d, level),
        draws = dr,
        point = c(dem = mean(dr$dem), rep = mean(dr$rep), tot = mean(dr$turnout)),
        coef = enightmodels::forecast_coefs(f)
    )
}
