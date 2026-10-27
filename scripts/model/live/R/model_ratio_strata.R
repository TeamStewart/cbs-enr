# ========================================
## model_ratio_strata.R -- ratio_strata: Cory McCartan's legacy ratio model,
## enightmodels::model_legacy().
##
## Cory's own call (tx-prim-26/R/prec_workflow.R, run_model_cty()) runs it on county x method
## returns with `past` = each candidate's expected votes. Here it runs on precinct x method
## cells, with `past` = the cell's expected votes x the candidate's baseline share
## (past_share()). Math: scripts/model/cory_model_spec.tex.
## The ratios are pooled within the strata of `demo` (demos.R): strat_g (geostratum, Cory's
## default), strat_d, strat_dg, or ratio (one pool); specs with covariates don't apply.
## Interval: none (default) or conformal, on the fitted cell shares past x ratio (results.R).
# ========================================

# Expected share of a cell's votes for each candidate: dem2 / oth baseline, with the
# third-party share split 85 / 15 between named minor candidates and the write-in line (as in
# Ethan's sims), the 85 shared equally if there are several minors.
past_share = function(party, candidate, dem2, oth) {
    writein = candidate == "write-in"
    minor = !party %in% c("dem", "rep") & !writein
    n_minor = max(n_distinct(candidate[minor]), 1)
    case_when(
        party == "dem" ~ (1 - oth) * dem2,
        party == "rep" ~ (1 - oth) * (1 - dem2),
        writein ~ oth * 0.15,
        TRUE ~ oth * 0.85 / n_minor
    )
}

# contest and district for enightmodels' RACE_ID. Live ids are "<ST>_<contest>[_<district>]"
# (races.R); in Ethan's sims the race id is just the contest ("gov").
race_parts = function(race_id) {
    if (grepl("^[A-Za-z]{2}_", race_id)) parse_race_id(race_id) else tibble(contest = race_id, district = NA_integer_)
}

# One race's returns in enightmodels' tidy election-data format, with the cell covariates
# (geostratum, dem2, oth, exp_votes, and stratum_demo if a stratum) and `past` joined on.
legacy_elec_data = function(cells, returns, strata = "geostratum") {
    ri = race_parts(returns$race_id[1])
    if ("stratum_demo" %in% strata)
        cells = mutate(demo_frame(cells, returns$state[1]), stratum_demo = as.character(stratum_demo))
    returns |>
        inner_join(select(cells, geo, method, county, geostratum, any_of("stratum_demo"), dem2, oth, exp_votes),
                   by = c("geo", "method")) |>
        mutate(past = exp_votes * past_share(party, candidate, dem2, oth)) |>
        transmute(
            state, election = as.Date(election), jurisdiction = state,
            contest = ri$contest, district = ri$district,
            candidate, party, time, stage, method, geo, votes,
            county, geostratum, pick(any_of("stratum_demo")), dem2, oth, exp_votes, past
        ) |>
        enightmodels::coerce_elec_data()
}

# cells:     build_cells() output; returns: standard returns for ONE race (returns.R)
# config:    enightmodels::model_legacy_config(); its strata are set from `demo`
# precincts: also return per-precinct predictions (`cells`) for write_precincts()
# demo:      demographic spec (demos.R), (1 | x) terms only
# interval:  "none" or "conformal"
fit_ratio_strata = function(cells, returns, config = enightmodels::model_legacy_config(), precincts = TRUE,
                            demo = DEMO_DEFAULT$ratio_strata, interval = "none", level = 0.95) {
    strata = demo_strata(check_demo(demo))
    if (is.null(strata)) stop("ratio_strata takes only specs without covariates (ratio, strat_g, strat_d, strat_dg), not ", demo)
    config$strata = strata
    settings = paste0(
        "enightmodels::model_legacy(), config: ",
        paste(names(config), vapply(config, \(x) paste(x, collapse = ","), ""), sep = " = ", collapse = "; "),
        "; past = expected votes x baseline share; interval ", interval
    )
    counts = cell_counts(cells, returns)
    counted = race_counted(cells, counts)

    d = legacy_elec_data(cells, returns, strata)
    # model_legacy()'s two stages, run once so the precinct frame comes from the same fit
    d_tot = enightmodels::model_legacy_total(d, past, config)
    d_cand = enightmodels::model_legacy_cand(d, d_tot, past, config)

    pred = d_cand |>
        mutate(pred_min = NA_real_, pred_max = NA_real_) |>
        pred_by_cand(returns)
    if (interval == "conformal") {
        # per cell and party: counted, final (pred) and fitted (past x ratio) votes
        fc = d_cand |>
            mutate(p = if_else(party %in% c("dem", "rep"), as.character(party), "oth"), fit = past * ratio) |>
            summarize(cnt = sum(votes), fin = sum(pred), fit = sum(fit), .by = c(geo, method, p)) |>
            mutate(fit = fit / max(sum(fit), 1e-9), .by = c(geo, method)) |>
            pivot_wider(names_from = p, values_from = c(cnt, fin, fit), values_fill = 0) |>
            rename_with(\(x) sub("^cnt_", "c_", x))
        for (v in c("c_oth", "fin_oth", "fit_oth")) if (!v %in% names(fc)) fc[[v]] = 0
        fc$c_tot = fc$c_dem + fc$c_rep + fc$c_oth
        pred = cand_bounds(pred, conformal_party(fc, level))
    }

    prec = if (precincts) {
        d_cand |>
            mutate(geo = as.character(geo), party = as.character(party)) |>
            summarize(dem = sum(pred[party == "dem"]), rep = sum(pred[party == "rep"]), tot = sum(pred), .by = geo)
    }
    coef = distinct(d_cand, candidate, party, method, across(all_of(strata)), ratio)

    model_result(
        "ratio_strata", "ok", settings, counted,
        est = pred_race_est(pred), pred = pred, cells = prec, coef = coef, interval = interval, level = level
    )
}
