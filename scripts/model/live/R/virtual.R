# ========================================
## virtual.R -- votes a county reports in "virtual" precincts instead of by precinct.
##
## In NC, 33 of 100 counties report ALL their early votes in county-level virtual precincts
## (EARLY VOTING, EV-<site>, ...) and 36 report most mail votes that way (ABSENTEE ...), plus
## small PROVISIONAL / TRANSFER rows everywhere. None of these match a precinct in our
## inputs, so the precinct models never see them. Team decision (2026-10-26, "option 2"):
## the models fit the real precincts only, and the virtual votes are added at county level:
##
##   virtual county x method  a method the county reported mostly (>= VIRTUAL_MIN_SHARE)
##                            in virtual precincts in 2024 ({ST}_virtual_history.parquet,
##                            scripts/model/make_mode_results.R). Its precinct cells are
##                            taken out of the model, and the county's votes for that method
##                            are one unit:
##                              before any are reported: projected -- expected votes (the
##                                precinct cells' expected votes for that method) x tonight's
##                                turnout ratio, at the county's 2024 virtual Dem share moved
##                                by tonight's swing (same rule and noise as the rest-of-state
##                                counties in counties.R)
##                              once any are reported: the counted votes, taken as final
##                                (they come in one batch)
##   everything else virtual  (provisional, transfer, a stray unmatched precinct id): counted
##                            votes added as they are, nothing projected
##
## Only model_simpleswing and model_cory are adjusted (VIRTUAL_MODELS; output.R
## predict_one()). model_medsl gets the same rows as county x mode observations from
## bucket_counts() (returns.R) inside its own fit, and model_cory_pois / model_cory_prec are
## left as they are.
## Only for races fit by vote method. A state with no {ST}_virtual_history.parquet and no
## unmatched rows is untouched. Rows are matched on geo + method after METHOD_MAP (run.R).
##
##   vs = virtual_split(cells, returns)    -> list(cells, returns, units) for the model
##   fit = add_virtual(fit, model, vs)     -> the fit with the virtual votes added
# ========================================

VIRTUAL_MIN_SHARE = 0.5
VIRTUAL_MODELS = c("swing", "cory")
# geo_county() (the 3-digit county code in a precinct id) is in vote_modes.R

virtual_history = function(state, dir_26 = find_26_general()) {
  f = file.path(dir_26, "input_data", "model_inputs", paste0(state, "_virtual_history.parquet"))
  if (!file.exists(f)) return(NULL)
  read_pq(f) |> mutate(county_cbs = as.character(county_cbs))
}

virtual_split = function(cells, returns, dir_26 = find_26_general()) {
  none = list(cells = cells, returns = filter(returns, geo %in% cells$geo), units = NULL)
  if (!nrow(returns) || !returns_by_method(returns) || all(cells$method == "all")) return(none)
  st = returns$state[1]
  ret = mutate(returns, method = unname(METHOD_MAP[as.character(method)]))
  cells = mutate(cells, county_cbs = geo_county(geo))
  vh = virtual_history(st, dir_26)
  main = if (is.null(vh)) tibble(county_cbs = character(), method = character(), votes = numeric(),
                                 dem = numeric(), rep = numeric(), oth = numeric(), virt_share = numeric()) else
    filter(vh, virt_share >= VIRTUAL_MIN_SHARE, county_cbs %in% cells$county_cbs)
  in_model = paste(ret$geo, ret$method) %in% paste(cells$geo, cells$method)
  in_model = in_model & !paste(geo_county(ret$geo), ret$method) %in% paste(main$county_cbs, main$method)
  out = ret[!in_model & geo_county(ret$geo) %in% cells$county_cbs, ]
  if (!nrow(main) && sum(out$votes) == 0) return(none)

  dropped = semi_join(cells, main, by = c("county_cbs", "method"))
  cells_m = anti_join(cells, main, by = c("county_cbs", "method")) |> select(-county_cbs)
  counted = out |>
    mutate(county_cbs = geo_county(geo), party = if_else(party %in% c("dem", "rep"), party, "oth")) |>
    summarize(d = sum(votes[party == "dem"]), r = sum(votes[party == "rep"]), o = sum(votes[party == "oth"]),
              .by = c(county_cbs, method))
  # lean of a projected unit: the history's batch lean when it has one, else the precinct
  # baseline of the cells it replaces (AK's history comes from a primary, so it has none)
  expect = dropped |> summarize(base_d = sum(exp_votes * dem2) / max(sum(exp_votes), 1),
                                base_o = sum(exp_votes * oth) / max(sum(exp_votes), 1),
                                exp_base = sum(exp_votes), .by = c(county_cbs, method)) |>
    left_join(transmute(main, county_cbs, method, h24 = dem / pmax(dem + rep, 1), oth24 = oth / pmax(votes, 1)),
              by = c("county_cbs", "method"))
  units = full_join(expect, counted, by = c("county_cbs", "method")) |>
    mutate(across(c(d, r, o, exp_base), \(x) coalesce(x, 0)),
           h24 = coalesce(h24, base_d, 0.5), oth24 = coalesce(oth24, base_o, 0),
           p = 1)        # counted votes are taken as final (see header); project_rest() ignores p when nothing is counted
  list(cells = cells_m, returns = ret[in_model, ], units = units)
}

# Adds the virtual units to a fit (run_race() on vs$cells / vs$returns). Point estimate and,
# for models with draws, every draw.
add_virtual = function(fit, model, vs) {
  if (is.null(vs$units) || !identical(fit$status, "ok")) return(fit)
  lg = function(x) qlogis(pmin(pmax(x, 1e-4), 1 - 1e-4))
  E = sum(vs$cells$exp_votes)
  base_s = sum(vs$cells$exp_votes * vs$cells$dem2) / max(E, 1)     # baseline Dem two-party share
  if (model == "swing") {
    pt = fit_point(fit); dr = fit$draws
    vp = project_rest(vs$units, lg(pt[["dem"]] / (pt[["dem"]] + pt[["rep"]])) - lg(base_s), pt[["tot"]] / E, noise = FALSE)
    vd = project_rest(vs$units, lg(dr$dem / (dr$dem + dr$rep)) - lg(base_s), dr$turnout / E, noise = TRUE)
    fit$point = c(dem = unname(pt[["dem"]] + vp[1, "dem"]), rep = unname(pt[["rep"]] + vp[1, "rep"]),
                  tot = unname(pt[["tot"]] + sum(vp[1, ])))
    fit$draws = dr |> mutate(dem = dem + vd[, "dem"], rep = rep + vd[, "rep"], turnout = turnout + rowSums(vd),
                             twop = dem / (dem + rep))
    ci = cred(fit$draws$twop); ct = cred(fit$draws$turnout)
    fit$race = mutate(fit$race, est_twop = mean(fit$draws$twop), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]],
                      p_dem_win = mean(fit$draws$twop > 0.5), est_turnout = mean(fit$draws$turnout),
                      lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]])
  } else if (model == "cory") {
    pr = if (is.data.frame(fit$raw)) fit$raw else fit$raw$pred
    pr = mutate(pr, party = as.character(party))
    d = sum(pr$pred[pr$party == "dem"]); r = sum(pr$pred[pr$party == "rep"])
    vp = project_rest(vs$units, lg(d / (d + r)) - lg(base_s), sum(pr$pred) / E, noise = FALSE)
    fit$virtual = vp[1, ]
    fit$race = mutate(fit$race, est_twop = unname((d + vp[1, "dem"]) / (d + r + vp[1, "dem"] + vp[1, "rep"])),
                      est_turnout = est_turnout + sum(vp[1, ]))
  }
  u = vs$units
  fit$settings = paste0(fit$settings, sprintf("; plus %d county-level virtual-precinct units (%d reported, %d projected)",
                                              nrow(u), sum(u$d + u$r + u$o > 0), sum(u$d + u$r + u$o == 0 & u$exp_base > 0)))
  fit
}

# model_cory's predicted votes by candidate, with the virtual votes added (third-party virtual
# votes split across third-party candidates in proportion to their predictions)
cory_by_candidate = function(fit) {
  pr = if (is.data.frame(fit$raw)) fit$raw else fit$raw$pred
  pr = pr |> mutate(candidate = as.character(candidate), party = as.character(party)) |>
    summarize(pred = sum(pred), .by = c(candidate, party))
  v = fit$virtual
  if (is.null(v)) return(pr)
  oth = !pr$party %in% c("dem", "rep")
  w = if (any(oth)) pr$pred[oth] / max(sum(pr$pred[oth]), 1) else numeric()
  pr$pred[pr$party == "dem"] = pr$pred[pr$party == "dem"] + v[["dem"]] / max(sum(pr$party == "dem"), 1)
  pr$pred[pr$party == "rep"] = pr$pred[pr$party == "rep"] + v[["rep"]] / max(sum(pr$party == "rep"), 1)
  if (any(oth)) pr$pred[oth] = pr$pred[oth] + v[["oth"]] * (if (sum(w) > 0) w else 1 / sum(oth))
  pr
}
