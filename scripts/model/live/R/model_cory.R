# ========================================
## model_cory.R -- model_cory: Cory McCartan's legacy ratio model, enightmodels::model_legacy().
##
## Cory's own call (tx-prim-26/R/prec_workflow.R, run_model_cty()) runs it on county x method
## returns with `past` = each candidate's expected votes. Here it runs on precinct x method
## cells, with `past` = the cell's expected votes x the candidate's baseline share
## (cory_past()). No interval. Math: scripts/model/cory_model_spec.tex.
##
## Also holds the helpers the three Cory wrappers share (model_cpois.R, model_cprec.R):
## cory_elec_data(), cory_result(), cory_pred_by_cand(), pred_race_est(), race_counted().
# ========================================

# Expected share of a cell's votes for each candidate: dem2 / oth baseline, with the
# third-party share split 85 / 15 between named minor candidates and the write-in line (as in
# Ethan's sims), the 85 shared equally if there are several minors.
cory_past = function(party, candidate, dem2, oth) {
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
cory_race = function(race_id) {
    if (grepl("^[A-Za-z]{2}_", race_id)) parse_race_id(race_id) else tibble(contest = race_id, district = NA_integer_)
}

# One race's returns in enightmodels' tidy election-data format, with the cell covariates
# (geostratum, dem2, oth, exp_votes) and `past` joined on.
cory_elec_data = function(cells, returns) {
    ri = cory_race(returns$race_id[1])
    returns |>
        inner_join(select(cells, geo, method, county, geostratum, dem2, oth, exp_votes), by = c("geo", "method")) |>
        mutate(past = exp_votes * cory_past(party, candidate, dem2, oth)) |>
        transmute(
            state, election = as.Date(election), jurisdiction = state,
            contest = ri$contest, district = ri$district,
            candidate, party, time, stage, method, geo, votes,
            county, geostratum, dem2, oth, exp_votes, past
        ) |>
        enightmodels::coerce_elec_data()
}

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

# The result list every model returns (run.R). `est` is the race row's estimates; missing ones
# are NA. `pred` is one row per candidate (candidate, party, pred, pred_min, pred_max), for the
# models without draws.
cory_result = function(model, status, settings, counted, est = list(), pred = NULL, cells = NULL,
                       draws = NULL, point = NULL, coef = NULL) {
    cols = c("est_twop", "lo_twop", "hi_twop", "p_dem_win", "est_turnout", "lo_turnout", "hi_turnout")
    race = as_tibble(c(list(status = status), lapply(setNames(cols, cols), \(c) est[[c]] %||% NA_real_)))
    list(
        model = model, status = status, settings = settings,
        race = bind_cols(race, counted),
        pred = pred, cells = cells, draws = draws, point = point, coef = coef
    )
}

# per-candidate predictions, party from the returns (dem / rep / ind / oth, as output.R)
cory_pred_by_cand = function(pred, returns) {
    pty = distinct(returns, candidate, party) |>
        mutate(candidate = as.character(candidate), party = if_else(party %in% c("dem", "rep", "ind"), party, "oth"))
    pred |>
        mutate(candidate = as.character(candidate)) |>
        summarize(across(any_of(c("pred", "pred_min", "pred_max")), sum), .by = candidate) |>
        left_join(pty, by = "candidate") |>
        mutate(party = coalesce(party, "oth"))
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

# cells:     build_cells() output; returns: standard returns for ONE race (returns.R)
# config:    enightmodels::model_legacy_config()
# precincts: also return per-precinct predictions (`cells`) for write_precincts()
fit_cory = function(cells, returns, config = enightmodels::model_legacy_config(), precincts = TRUE) {
    settings = paste0(
        "enightmodels::model_legacy(), config: ",
        paste(names(config), vapply(config, \(x) paste(x, collapse = ","), ""), sep = " = ", collapse = "; "),
        "; past = expected votes x baseline share; no interval"
    )
    counts = cell_counts(cells, returns)
    counted = race_counted(cells, counts)

    d = cory_elec_data(cells, returns)
    # model_legacy()'s two stages, run once so the precinct frame comes from the same fit
    d_tot = enightmodels::model_legacy_total(d, past, config)
    d_cand = enightmodels::model_legacy_cand(d, d_tot, past, config)

    pred = d_cand |>
        mutate(pred_min = NA_real_, pred_max = NA_real_) |>
        cory_pred_by_cand(returns)

    prec = if (precincts) {
        d_cand |>
            mutate(geo = as.character(geo), party = as.character(party)) |>
            summarize(dem = sum(pred[party == "dem"]), rep = sum(pred[party == "rep"]), tot = sum(pred), .by = geo)
    }
    coef = distinct(d_cand, candidate, party, method, geostratum, ratio)

    cory_result(
        "cory", "ok", settings, counted,
        est = pred_race_est(pred), pred = pred, cells = prec, coef = coef
    )
}
