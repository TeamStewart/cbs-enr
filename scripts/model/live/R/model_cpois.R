# ========================================
## model_cpois.R -- model_cory_pois: Cory McCartan's precinct Poisson model, model_prec_pois()
## from the 2026 TX primaries (scripts/model/modeling-cory/tx-prim-26/R/prec_model.R).
##
## Cory's code is sourced from that folder as is (one addition in prec_model.R: it also returns
## the per-precinct frame, `prec`). His formulas come from `model_specs` in prec_workflow.R, and
## the default cascade is his `strat_lin` spec: strat_dg_cov, then strat_d_cov, then ratio. Each
## candidate x method gets votes ~ log_s(est) + <spec>, a Poisson GLMM (lme4::glmer), or a
## ridge Poisson GLM (glmnet) for `ratio`; the first spec that fits is used. `est` is rescaled
## by the turnout ratio of the counted-out precincts in each geostratum x method. Intervals are
## split-conformal on each candidate's share of a cell.
##
## Cory wrote it for primaries, where every candidate in a race shares one party's expected
## turnout `est`. In a general election we give it a single pooled party, "all", with `est` =
## the cell's expected votes for every candidate; partisanship enters through dem24_est in the
## spec, as in Cory's covariate specs. Party is put back from the returns afterwards.
# ========================================

CPOIS_SRC = "scripts/model/modeling-cory/tx-prim-26/R"
CPOIS_SPECS = c("strat_dg_cov", "strat_d_cov", "ratio")

# our precinct inputs -> the covariate names in Cory's model_specs
CPOIS_COVS = c(
    dem24_est = "dem2", age_18_29 = "p_age_18_29", age_65_up = "p_age_65_up",
    white_col = "p_white_col", white_noncol = "p_white_noncol", stratum_demo = "stratum_demo"
)

.cpois = new.env()

# Cory's functions and model_specs, sourced once per session into their own environment.
# log_s() is called unqualified in his formulas, so it is bound there too.
cpois_env = function(src = CPOIS_SRC) {
    if (!is.null(.cpois$env)) return(.cpois$env)
    for (p in c("lme4", "broom.mixed", "collapse", "glmnet"))
        if (!requireNamespace(p, quietly = TRUE)) stop("model_cory_pois needs the ", p, " package")
    env = new.env(parent = globalenv())
    env$log_s = enightmodels::log_s
    for (f in c("utils.R", "prec_model.R", "prec_workflow.R")) sys.source(file.path(src, f), envir = env)
    .cpois$env = env
    env
}

# precinct covariates for one state, cached per session
cpois_covs = function(state, dir_26 = find_26_general()) {
    key = paste0("covs_", state)
    if (is.null(.cpois[[key]])) {
        f = file.path(dir_26, "input_data", "model_inputs", paste0(state, "_precinct.parquet"))
        .cpois[[key]] = read_pq(f) |>
            select(geo, all_of(CPOIS_COVS)) |>
            mutate(geo = as.character(geo), stratum_demo = as.character(stratum_demo))
    }
    .cpois[[key]]
}

# Cory's d_race and d_cov. d_cov: one row per precinct x method with est = expected votes and
# the spec covariates. A House race is restricted to the precincts in its returns.
cpois_inputs = function(cells, returns) {
    state = returns$state[1]
    ri = cory_race(returns$race_id[1])
    d_cov = cells |>
        transmute(geo = as.character(geo), county = as.character(county), method = as.character(method),
                  geostratum, party = "all", est = exp_votes) |>
        inner_join(cpois_covs(state), by = "geo") |>
        mutate(across(contains("stratum"), \(x) as.factor(as.character(x))))
    if (!is.na(ri$district)) d_cov = filter(d_cov, geo %in% returns$geo)

    d_race = returns |>
        transmute(geo = as.character(geo), method = as.character(method), candidate = as.character(candidate),
                  party = "all", votes = as.numeric(votes), stage = as.integer(stage), time) |>
        inner_join(distinct(d_cov, geo, county, method), by = c("geo", "method"))
    list(d_race = d_race, d_cov = d_cov, contest = ri$contest, district = ri$district)
}

# cells: build_cells() output; returns: standard returns for ONE race (returns.R)
# specs: names in Cory's model_specs, tried in order
fit_cpois = function(cells, returns, specs = CPOIS_SPECS, turn_ratio = 1.0, level = 0.95) {
    env = cpois_env()
    forms = env$model_specs[specs]
    counts = cell_counts(cells, returns)
    counted = race_counted(cells, counts)
    conf = sprintf("approx_conform_%g", 100 * level)
    settings = sprintf("model_prec_pois(), specs %s; turn_ratio = %g; interval %s",
                       paste(specs, collapse = " -> "), turn_ratio, conf)
    if (!any(counts$c_tot > 0)) return(cory_result("cpois", "no_votes", settings, counted))

    inp = cpois_inputs(cells, returns)
    by_method = !all(inp$d_cov$method == "all")
    # glmer warns on every singular fit; Cory's call site drops them with purrr::quietly()
    res = purrr::quietly(env$model_prec_pois)(
        inp$d_race, inp$d_cov, inp$contest, inp$district,
        formulas = forms, stage_done = 6L, turn_ratio = turn_ratio, conf_level = level, by_method = by_method
    )$result
    if (is.null(res) || !nrow(res$pred)) return(cory_result("cpois", "no_fit", settings, counted))

    used = count(res$pred, model)
    settings = paste0(settings, "; fits: ", paste(used$model, used$n, sep = " x", collapse = ", "))
    pred = cory_pred_by_cand(res$pred, returns)
    if (!(sum(pred$pred[pred$party %in% c("dem", "rep")]) > 0))
        return(cory_result("cpois", "no_fit", settings, counted))

    prec = res$prec |>
        mutate(geo = as.character(geo), candidate = as.character(candidate)) |>
        select(-party) |>
        left_join(distinct(pred, candidate, party), by = "candidate") |>
        summarize(dem = sum(pred[party == "dem"]), rep = sum(pred[party == "rep"]), tot = sum(pred),
                  .by = any_of(c("geo", "county", "method")))

    cory_result(
        "cpois", "ok", settings, counted,
        # no interval on turnout: the conformal residuals are shares, the cell total is a point
        est = pred_race_est(pred), pred = pred, cells = prec, coef = res$coef
    )
}
