# ========================================
## model_cpbayes.R -- model_cory_pbayes: a precinct Poisson model on Cory McCartan's
## enightmodels::pois_bayes() (Bayesian Poisson regression, Laplace approximation).
##
## A candidate for replacing model_cory_pois: the same kind of model (each party's votes per
## precinct x method ~ Poisson, log expected votes as the offset, Cory's TX covariates), but
## with priors instead of a fallback cascade, and posterior predictive draws instead of a
## conformal interval -- so it gets p_dem_win and statewide intervals like model_medsl.
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
## X = Cory's strat_dg_cov covariates, standardised (dem2, dem2^2, ages 18-29 and 65+, white
## college and non-college) plus geostratum and stratum_demo as fixed factors (pois_bayes takes
## one grouping factor, so county is the random intercept). Priors centre every fit on the
## baseline, with prior_sd the prior sd of the log rate: half its variance on the intercept, a
## quarter on the county intercepts, a quarter shared across the covariates; the intercept's
## prior mean is set so the prior mean rate is the baseline. A method with no counted-out cell keeps the prior for turnout. Precinct counts
## are overdispersed, so draws are widened by the Pearson dispersion of each fit
## (quasi-Poisson; see cpbayes_draws()).
# ========================================

CPBAYES_COVS = c("dem2", "dem2_sq", "p_age_18_29", "p_age_65_up", "p_white_col", "p_white_noncol")
CPBAYES_DRAWS = 500L

.cpbayes = new.env()

# precinct covariates for one state (standardised by expected votes later), cached per session
cpbayes_covs = function(state, dir_26 = find_26_general()) {
    key = paste0("covs_", state)
    if (is.null(.cpbayes[[key]])) {
        f = file.path(dir_26, "input_data", "model_inputs", paste0(state, "_precinct.parquet"))
        .cpbayes[[key]] = read_pq(f) |>
            transmute(geo = as.character(geo), p_age_18_29, p_age_65_up, p_white_col, p_white_noncol,
                      stratum_demo = as.character(stratum_demo))
    }
    .cpbayes[[key]]
}

# one row per cell: counts, completeness, and the covariates centred and scaled (weighted by
# expected votes) as z_<name>
cpbayes_inputs = function(cells, counts, state) {
    d = cells |>
        transmute(geo = as.character(geo), county = as.character(county), method = as.character(method),
                  geostratum = factor(as.character(geostratum)), dem2, oth, exp_votes) |>
        bind_cols(select(counts, c_dem, c_rep, c_oth, c_tot, complete)) |>
        left_join(cpbayes_covs(state), by = "geo") |>
        mutate(stratum_demo = factor(coalesce(stratum_demo, "none")), dem2_sq = dem2^2,
               obs = complete & c_tot > 0)
    w = d$exp_votes / sum(d$exp_votes)
    for (v in CPBAYES_COVS) {
        x = coalesce(d[[v]], sum(w * d[[v]], na.rm = TRUE))
        m = sum(w * x)
        d[[paste0("z_", v)]] = (x - m) / max(sqrt(sum(w * (x - m)^2)), 1e-9)
    }
    d
}

# Posterior predictive count draws (draws x rows of d) from a pois_bayes() fit of y ~ form on
# the rows of `d` where `use`, with log exposure `off_fit` (fit) / `off_new` (prediction).
# Two corrections to a plain Poisson fit:
#   - the intercept's prior mean is -v/2, v the prior variance of the linear predictor, so
#     the prior mean of each rate is the baseline (exp of a normal has mean exp(v/2))
#   - overdispersion: phi = Pearson chi-square / df on the fitted rows (at least 1), and each
#     draw's deviation from the point prediction is scaled by sqrt(phi) (quasi-Poisson)
cpbayes_draws = function(d, y, use, off_fit, off_new, form, draws, pr) {
    X = model.matrix(form, d)
    # the prior variance of the log rate, pr$sd^2: half on the intercept, a quarter on the
    # county intercepts, a quarter shared across the covariates (on average over rows)
    x2 = mean(rowSums(X[, -1, drop = FALSE]^2))
    sd = c(pr$sd * sqrt(0.5), rep(pr$sd * sqrt(0.25 / max(x2, 1e-9)), ncol(X) - 1))
    group = pr$sd * sqrt(0.25)
    v = pr$sd^2
    d$y = y; d$use = use; d$off = off_fit
    fit = enightmodels::pois_bayes(
        update(form, y ~ .), d, subset = use, offset = off, group = county,
        prior_beta_mean = c(-v / 2, rep(0, ncol(X) - 1)), prior_beta_sd = sd, prior_group_sd = group
    )
    phi = 1
    if (sum(use) > ncol(X) + 10) {
        mu = predict(fit, d[use, ])
        phi = max(1, sum((y[use] - mu)^2 / pmax(mu, 0.5)) / (sum(use) - ncol(X)))
    }
    d$off = off_new
    M = matrix(posterior::draws_of(enightmodels::posterior_predict(fit, d, draws = draws)), nrow = draws)
    if (phi > 1) {
        mu = matrix(predict(fit, d), draws, nrow(d), byrow = TRUE)
        M = pmax(mu + sqrt(phi) * (M - mu), 0)
    }
    M
}

# final votes by party in one method's cells: list(dem, rep, oth), each draws x cells
cpbayes_method = function(d, form, draws, pr) {
    n = nrow(d)
    cnt = function(x) matrix(x, draws, n, byrow = TRUE)
    lg = function(x) log(pmax(x, 0.5))
    base = list(dem = (1 - d$oth) * d$dem2, rep = (1 - d$oth) * (1 - d$dem2), oth = d$oth)
    votes = list(dem = d$c_dem, rep = d$c_rep, oth = d$c_oth)

    T = cpbayes_draws(d, d$c_tot, d$obs, lg(d$exp_votes), lg(d$exp_votes), form, draws, pr)
    T = ifelse(cnt(d$obs), cnt(d$c_tot), pmax(T, cnt(d$c_tot)))
    rem = T - cnt(d$c_tot)

    # party counts at the remaining expected votes, normalised to shares of what remains
    e_rem = pmax(colMeans(rem), 1)
    R = lapply(names(base), \(p) cpbayes_draws(d, votes[[p]], d$c_tot > 0, lg(d$c_tot * base[[p]]),
                                               lg(e_rem * base[[p]]), form, draws, pr))
    names(R) = names(base)
    R_tot = R$dem + R$rep + R$oth
    lapply(setNames(names(base), names(base)), \(p) {
        sh = ifelse(R_tot > 0, R[[p]] / pmax(R_tot, 1), cnt(base[[p]]))
        cnt(votes[[p]]) + rem * sh
    })
}

# cells: build_cells() output; returns: standard returns for ONE race (returns.R)
fit_cpbayes = function(cells, returns, prior_sd = 0.2,
                       draws = CPBAYES_DRAWS, level = 0.95, precincts = TRUE, seed = 2026,
                       dir_26 = find_26_general()) {
    counts = cell_counts(cells, returns)
    counted = race_counted(cells, counts)
    settings = sprintf(paste0(
        "enightmodels::pois_bayes() per method: turnout on counted-out cells (offset log expected ",
        "votes), party votes on reporting cells (offset log counted x baseline share); covariates %s ",
        "+ geostratum + stratum_demo, county intercepts; prior sd of the log rate %g; quasi-Poisson ",
        "dispersion; %d draws; posterior_%g"),
        paste(CPBAYES_COVS, collapse = ", "), prior_sd, draws, 100 * level)
    if (!any(counts$c_tot > 0)) return(cory_result("pbayes", "no_votes", settings, counted))

    d = cpbayes_inputs(cells, counts, returns$state[1])
    facs = c("geostratum", "stratum_demo")[vapply(d[c("geostratum", "stratum_demo")], nlevels, 1L) > 1]
    form = reformulate(c(paste0("z_", CPBAYES_COVS), facs))
    pr = list(sd = prior_sd)

    set.seed(seed)
    V = list(dem = matrix(0, draws, nrow(d)), rep = matrix(0, draws, nrow(d)), oth = matrix(0, draws, nrow(d)))
    for (m in unique(d$method)) {
        i = which(d$method == m)
        vm = cpbayes_method(d[i, ], form, draws, pr)
        for (p in names(V)) V[[p]][, i] = vm[[p]]
    }
    tot = V$dem + V$rep + V$oth
    dr = tibble(dem = rowSums(V$dem), rep = rowSums(V$rep), turnout = rowSums(tot)) |>
        mutate(twop = dem / (dem + rep))
    ci = cred(dr$twop, level)
    ct = cred(dr$turnout, level)

    cory_result(
        "pbayes", "ok", settings, counted,
        est = list(est_twop = mean(dr$twop), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]],
                   p_dem_win = mean(dr$dem > dr$rep), est_turnout = mean(dr$turnout),
                   lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]]),
        cells = if (precincts) draws_precincts(tot, V$dem, V$rep, d, level),
        draws = dr,
        point = c(dem = mean(dr$dem), rep = mean(dr$rep), tot = mean(dr$turnout))
    )
}
