# ========================================
## model_ratio_strata.R -- ratio_strata: Cory McCartan's legacy ratio model,
## enightmodels::model_legacy().
##
## Cory's own call (tx-prim-26/R/prec_workflow.R, run_model_cty()) runs it on county x method
## returns with `past` = each candidate's expected votes. Here it runs on precinct x method
## cells, with `past` = the cell's expected votes x the candidate's baseline share
## (past_share()). Math: scripts/model/docs/cory_model_spec.tex.
## The ratios are pooled within the strata of `demo` (demos.R): strat_g (geostratum, Cory's
## default), strat_d, strat_dg, or ratio (one pool); specs with covariates don't apply.
## Turnout stage: past x the cell's congressional-district turnout adjustment, per method
## (district_turnout_adjust(), demos.R); the candidate stage uses past as is.
## Interval: none (default) or conformal, on the fitted cell shares past x ratio (results.R).
# ========================================

#' Expected share of a cell's votes for each candidate
#'
#' Cory's rule (tx-prim-26 `run_model_cty()`: the party's expected votes / its number of
#' candidates): each party's dem2 / oth baseline share split equally across its candidates,
#' the third-party share equally across every third-party candidate, the write-in line
#' included.
#'
#' @param party,candidate One entry per row (candidate x cell).
#' @param dem2 Baseline two-party Dem share.
#' @param oth Baseline third-party share.
#' @return Numeric vector of expected shares.
past_share = function(party, candidate, dem2, oth) {
  grp = if_else(party %in% c("dem", "rep"), as.character(party), "oth")
  n = as.vector(tapply(candidate, grp, n_distinct)[grp])
  case_when(grp == "dem" ~ (1 - oth) * dem2, grp == "rep" ~ (1 - oth) * (1 - dem2), TRUE ~ oth) / n
}

#' Contest and district of a race id
#'
#' Live ids are `<ST>_<contest>[_<district>]` (races.R); in Ethan's sims the race id is just
#' the contest ("gov").
#'
#' @param race_id Race id.
#' @return List: contest, district.
race_parts = function(race_id) {
  if (grepl("^[A-Za-z]{2}_", race_id)) {
    parse_race_id(race_id)
  } else {
    tibble(contest = race_id, district = NA_integer_)
  }
}

#' One race's returns in enightmodels' tidy election-data format
#'
#' @param cells Cells from `build_cells()`.
#' @param returns Standard returns for one race.
#' @param strata Strata columns to carry.
#' @return Election data with the cell covariates (geostratum, dem2, oth, exp_votes, and
#'   stratum_demo if a stratum) and `past` (`past_share()`) joined on.
legacy_elec_data = function(cells, returns, strata = "geostratum") {
  ri = race_parts(returns$race_id[1])
  if ("stratum_demo" %in% strata) {
    cells = mutate(demo_frame(cells, returns$state[1]), stratum_demo = as.character(stratum_demo))
  }
  returns |>
    inner_join(
      select(cells, geo, method, county, geostratum, any_of("stratum_demo"), dem2, oth, exp_votes),
      by = c("geo", "method")
    ) |>
    mutate(past = exp_votes * past_share(party, candidate, dem2, oth)) |>
    transmute(
      state,
      election = as.Date(election),
      jurisdiction = state,
      contest = ri$contest,
      district = ri$district,
      candidate,
      party,
      time,
      stage,
      method,
      geo,
      votes,
      county,
      geostratum,
      pick(any_of("stratum_demo")),
      dem2,
      oth,
      exp_votes,
      past
    ) |>
    enightmodels::coerce_elec_data()
}

#' Fit ratio_strata to one race
#'
#' Cory's `enightmodels::model_legacy()`, its two stages called directly so the precinct
#' frame comes from the same fit; past = expected votes x baseline share.
#'
#' @param cells Cells from `build_cells()`.
#' @param returns Standard returns for one race.
#' @param config `enightmodels::model_legacy_config()`; its strata are set from `demo`.
#' @param precincts Also return per-precinct predictions (`cells`) for `write_precincts()`.
#' @param demo Demographic spec (demos.R), `(1 | x)` terms only.
#' @param interval "none" or "conformal".
#' @param level Interval level.
#' @return `model_result()` list.
fit_ratio_strata = function(
  cells,
  returns,
  config = enightmodels::model_legacy_config(),
  precincts = TRUE,
  demo = DEMO_DEFAULT$ratio_strata,
  interval = "none",
  level = 0.95
) {
  strata = demo_strata(check_demo(demo))
  if (is.null(strata)) {
    stop(
      "ratio_strata takes only specs without covariates (ratio, strat_g, strat_d, strat_dg), not ",
      demo
    )
  }
  config$strata = strata
  settings = paste0(
    "enightmodels::model_legacy(), config: ",
    paste(
      names(config),
      vapply(config, \(x) paste(x, collapse = ","), ""),
      sep = " = ",
      collapse = "; "
    ),
    "; past = expected votes x baseline share (turnout stage: x district turnout adjustment); interval ",
    interval
  )
  counts = cell_counts(cells, returns)
  counted = race_counted(cells, counts)

  d = legacy_elec_data(cells, returns, strata)
  # turnout stage: each cell's past votes moved by its congressional district's turnout so
  # far, per method (district_turnout_adjust(), demos.R), so a stratum's ratio is applied on
  # top of where the precinct's district is running; the candidate stage keeps `past`
  adj = district_turnout_adjust(cells, counts, returns$state[1])
  d$past_turn = d$past *
    coalesce(adj[match(paste(d$geo, d$method), paste(cells$geo, cells$method))], 1)
  # model_legacy()'s two stages, run once so the precinct frame comes from the same fit
  d_tot = enightmodels::model_legacy_total(d, past_turn, config)
  d_cand = enightmodels::model_legacy_cand(d, d_tot, past, config)

  pred = d_cand |>
    mutate(pred_min = NA_real_, pred_max = NA_real_) |>
    pred_by_cand(returns)
  if (interval == "conformal") {
    # per cell and party: counted, final (pred) and fitted (past x ratio) votes
    fc = d_cand |>
      mutate(
        p = if_else(party %in% c("dem", "rep"), as.character(party), "oth"),
        fit = past * ratio
      ) |>
      summarize(cnt = sum(votes), fin = sum(pred), fit = sum(fit), .by = c(geo, method, p)) |>
      mutate(fit = fit / max(sum(fit), 1e-9), .by = c(geo, method)) |>
      pivot_wider(names_from = p, values_from = c(cnt, fin, fit), values_fill = 0) |>
      rename_with(\(x) sub("^cnt_", "c_", x))
    for (v in c("c_oth", "fin_oth", "fit_oth")) {
      if (!v %in% names(fc)) fc[[v]] = 0
    }
    fc$c_tot = fc$c_dem + fc$c_rep + fc$c_oth
    pred = cand_bounds(pred, conformal_party(fc, level))
  }

  prec = if (precincts) {
    d_cand |>
      mutate(geo = as.character(geo), party = as.character(party)) |>
      summarize(
        dem = sum(pred[party == "dem"]),
        rep = sum(pred[party == "rep"]),
        tot = sum(pred),
        .by = geo
      ) |>
      votes_precincts()
  }
  coef = distinct(d_cand, candidate, party, method, across(all_of(strata)), ratio)

  model_result(
    "ratio_strata",
    "ok",
    settings,
    counted,
    est = pred_race_est(pred),
    pred = pred,
    cells = prec,
    coef = coef,
    interval = interval,
    level = level
  )
}
