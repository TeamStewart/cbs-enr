# ========================================
## model_cpois.R -- RETIRED 2026-10-27, not loaded by load.R. Was model_cory_pois: Cory
## McCartan's precinct Poisson model, model_prec_pois() from the 2026 TX primaries
## (scripts/model/modeling-cory/tx-prim-26/R/prec_model.R). pois_bayes with a demographic spec
## and /conformal (Cory's interval, results.R) replaces it. To run it again: source this file
## after load.R and call fit_cpois(cells, returns, demo = DEMO_DEFAULT$cpois).
##
## Cory's code is sourced from that folder as is (one addition in prec_model.R: it also returns
## the per-precinct frame, `prec`). The formulas are his `model_specs`, kept in demos.R in our
## column names; the default cascade is his `strat_lin` spec: strat_dg_cov, then strat_d_cov,
## then ratio. Each candidate x method gets votes ~ log_s(est) + <spec>, a Poisson GLMM
## (lme4::glmer), or a ridge Poisson GLM (glmnet) without (1 | x) terms; the first spec that
## fits is used. `est` is rescaled by the turnout ratio of the counted-out precincts in each
## geostratum x method. Intervals are split-conformal on each candidate's share of a cell.
##
## Cory wrote it for primaries, where every candidate in a race shares one party's expected
## turnout `est`. In a general election we give it a single pooled party, "all", with `est` =
## the cell's expected votes for every candidate; partisanship enters through dem2 in the
## spec, as in Cory's covariate specs. Party is put back from the returns afterwards.
# ========================================

CPOIS_SRC = "scripts/model/modeling-cory/tx-prim-26/R"

.cpois = new.env()

# Cory's functions, sourced once per session into their own environment. log_s() is called
# unqualified in his formulas, so it is bound there too.
cpois_env = function(src = CPOIS_SRC) {
    if (!is.null(.cpois$env)) return(.cpois$env)
    for (p in c("lme4", "broom.mixed", "collapse", "glmnet"))
        if (!requireNamespace(p, quietly = TRUE)) stop("model_cory_pois needs the ", p, " package")
    env = new.env(parent = globalenv())
    env$log_s = enightmodels::log_s
    for (f in c("utils.R", "prec_model.R")) sys.source(file.path(src, f), envir = env)
    .cpois$env = env
    env
}

# Cory's d_race and d_cov. d_cov: one row per precinct x method with est = expected votes and
# the demographic columns (demos.R). A House race is restricted to the precincts in its returns.
cpois_inputs = function(cells, returns) {
    ri = race_parts(returns$race_id[1])
    d_cov = demo_frame(cells, returns$state[1]) |>
        mutate(county = as.character(county), method = as.character(method), party = "all", est = exp_votes)
    if (!is.na(ri$district)) d_cov = filter(d_cov, geo %in% returns$geo)

    d_race = returns |>
        transmute(geo = as.character(geo), method = as.character(method), candidate = as.character(candidate),
                  party = "all", votes = as.numeric(votes), stage = as.integer(stage), time) |>
        inner_join(distinct(d_cov, geo, county, method), by = c("geo", "method"))
    list(d_race = d_race, d_cov = d_cov, contest = ri$contest, district = ri$district)
}

# cells: build_cells() output; returns: standard returns for ONE race (returns.R)
# demo: demographic specs (demos.R), tried in order; a single one falls back to ratio
fit_cpois = function(cells, returns, demo = DEMO_DEFAULT$cpois, turn_ratio = 1.0, level = 0.95) {
    env = cpois_env()
    specs = unique(c(check_demo(demo), if (length(demo) == 1) "ratio"))
    # evaluated in Cory's environment, where log_s() is bound
    forms = lapply(DEMOS[specs], \(f) { environment(f) = env; f })
    counts = cell_counts(cells, returns)
    counted = race_counted(cells, counts)
    conf = sprintf("approx_conform_%g", 100 * level)
    settings = sprintf("model_prec_pois(), specs %s; turn_ratio = %g; interval %s",
                       paste(specs, collapse = " -> "), turn_ratio, conf)
    if (!any(counts$c_tot > 0)) return(model_result("cpois", "no_votes", settings, counted))

    inp = cpois_inputs(cells, returns)
    by_method = !all(inp$d_cov$method == "all")
    # glmer warns on every singular fit; Cory's call site drops them with purrr::quietly()
    res = purrr::quietly(env$model_prec_pois)(
        inp$d_race, inp$d_cov, inp$contest, inp$district,
        formulas = forms, stage_done = 6L, turn_ratio = turn_ratio, conf_level = level, by_method = by_method
    )$result
    if (is.null(res) || !nrow(res$pred)) return(model_result("cpois", "no_fit", settings, counted))

    used = count(res$pred, model)
    settings = paste0(settings, "; fits: ", paste(used$model, used$n, sep = " x", collapse = ", "))
    pred = pred_by_cand(res$pred, returns)
    if (!(sum(pred$pred[pred$party %in% c("dem", "rep")]) > 0))
        return(model_result("cpois", "no_fit", settings, counted))

    prec = res$prec |>
        mutate(geo = as.character(geo), candidate = as.character(candidate)) |>
        select(-party) |>
        left_join(distinct(pred, candidate, party), by = "candidate") |>
        summarize(dem = sum(pred[party == "dem"]), rep = sum(pred[party == "rep"]), tot = sum(pred),
                  .by = any_of(c("geo", "county", "method")))

    model_result(
        "cpois", "ok", settings, counted,
        # no interval on turnout: the conformal residuals are shares, the cell total is a point
        est = pred_race_est(pred), pred = pred, cells = prec, coef = res$coef
    )
}
