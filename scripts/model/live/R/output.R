# ========================================
## output.R -- every model's results in enightmodels' "tidy prediction data" format, for the
## dashboard. One table for all races and models:
##
##   state, election, jurisdiction, contest, district   the race (enightmodels RACE_ID)
##   candidate, party                                   one row per candidate
##   pred_type   "vote"  = predicted final votes for the candidate
##               "share" = predicted final share of ALL votes cast in the race (as
##                         enightmodels::pred_convert(to = "share") defines it)
##   time        the returns snapshot the predictions are from
##   model       "model_simpleswing", "model_cory", "model_y"
##   model_conf  how the interval was made and its level, e.g. "county_bootstrap_95";
##               NA when the model gives no interval (model_cory)
##   pct_rep     share of the race's precincts with any votes counted -- the same
##               definition for every model, so the column lines up across models
##   rep         reported so far (votes, or share of the votes counted so far)
##   pred        predicted final value
##   pred_min, pred_max   interval ends (NA without an interval)
## plus, beyond the schema (extra columns are allowed):
##   p_win       probability the candidate finishes ahead of the other major-party
##               candidate (NA without draws, and on third-party rows)
##   geo         "scraped" = the counties we scrape precinct by precinct (the models
##               themselves); "all" = statewide: scraped counties from the model plus every
##               other county from CBS's county results (counties.R). Statewide rows have
##               model_conf "..._plus_rest" and `cbs_as_of`, the time of the CBS snapshot used.
##
## Every row passes enightmodels::check_pred_data(). Races x models with no prediction
## (no votes yet, or the model errored) are not in this table; they are in the STATUS
## table, one row per race x model per run: race_id, model, time, status ("ok",
## "no_votes", "error: ..."), statewide (whether statewide rows were added, or why not),
## settings. So a broken model shows up there instead of
## silently vanishing.
##
## Margins ("margin_share", "margin_vote") are left to the dashboard: enightmodels' schema
## requires pred >= 0, so a negative margin row would fail check_pred_data().
##
##   res = predict_all(cells, returns, races = race_info(returns), models = MODELS)
##   res$pred, res$status
##   write_predictions(res)           to 26_general/model_results: a timestamped file per
##                                    run + "latest", for both tables
# ========================================

MODEL_NAMES = c(swing = "model_simpleswing", cory = "model_cory", y = "model_y")

# one row per race_id with the enightmodels race columns. The default suits the sims
# (race_id is the contest, e.g. "gov"); for live races pass a table built from the race
# dimension (CONTRACT.md section 4) with race_id, state, election, jurisdiction, contest,
# district.
race_info = function(returns) {
  returns |>
    distinct(race_id, state, election) |>
    mutate(jurisdiction = state, contest = race_id, district = NA_integer_)
}

# share of the race's precincts (in cells) with any votes counted
pct_reporting = function(cells, returns) {
  rep_geo = unique(returns$geo[returns$votes > 0])
  mean(unique(cells$geo) %in% rep_geo)
}

# reported votes per candidate, with the party of each
reported_by_candidate = function(returns) {
  returns |>
    mutate(party = if_else(party %in% c("dem", "rep"), party, if_else(party == "ind", "ind", "oth"))) |>
    summarize(rep = sum(votes), .by = c(candidate, party))
}

# fit: one model's result for one race (run_race()); returns: that race's returns
to_pred_data = function(fit, model, cells, returns, race, time, level = 0.95) {
  base = race |> select(state, election, jurisdiction, contest, district)
  name = unname(MODEL_NAMES[model])
  pct = pct_reporting(cells, returns)
  if (!identical(fit$status, "ok")) return(NULL)

  cand = reported_by_candidate(returns)
  conf = NA_character_
  if (model == "swing") {
    # point predictions by party; third-party votes split across third-party candidates
    # in proportion to their counted votes (equally if none counted yet)
    pv = c(dem = sum(fit$cells$dem), rep = sum(fit$cells$rep))
    oth_tot = sum(fit$cells$tot) - pv[["dem"]] - pv[["rep"]]
    dr = fit$draws
    tot_d = dr$turnout
    conf = sprintf("county_bootstrap_%g", 100 * level)
    q = function(x) unname(cred(x, level))
    cand = cand |>
      mutate(w = if (sum(rep[party %in% c("oth", "ind")]) > 0) rep / sum(rep[party %in% c("oth", "ind")])
                 else 1 / sum(party %in% c("oth", "ind")), .by = NULL) |>
      mutate(w = if_else(party %in% c("oth", "ind"), w, NA_real_))
    rows = lapply(seq_len(nrow(cand)), function(i) {
      p = cand$party[i]
      v = if (p == "dem") dr$dem else if (p == "rep") dr$rep else cand$w[i] * (tot_d - dr$dem - dr$rep)
      point = if (p %in% c("dem", "rep")) pv[[p]] else cand$w[i] * oth_tot
      ci_v = q(v); ci_s = q(v / tot_d)
      pw = if (p == "dem") mean(dr$dem > dr$rep) else if (p == "rep") mean(dr$rep > dr$dem) else NA_real_
      tibble(candidate = cand$candidate[i], party = p,
             pred_type = c("vote", "share"), rep = c(cand$rep[i], cand$rep[i] / max(sum(cand$rep), 1)),
             pred = c(point, point / sum(fit$cells$tot)),
             pred_min = pmax(c(ci_v[1], ci_s[1]), 0), pred_max = c(ci_v[2], ci_s[2]), p_win = pw)
    })
    out = bind_rows(rows)
  } else if (model == "cory") {
    pr = if (is.data.frame(fit$raw)) fit$raw else fit$raw$pred
    pr = pr |>
      mutate(candidate = as.character(candidate), party = as.character(party)) |>
      summarize(pred = sum(pred), .by = c(candidate, party)) |>
      left_join(select(cand, candidate, rep), by = "candidate") |>
      mutate(rep = coalesce(rep, 0))
    tot = sum(pr$pred)
    out = bind_rows(
      mutate(pr, pred_type = "vote"),
      mutate(pr, pred_type = "share", rep = rep / max(sum(rep), 1), pred = pred / tot)) |>
      mutate(pred_min = NA_real_, pred_max = NA_real_, p_win = NA_real_)
  } else {
    stop("to_pred_data() has no rule for model '", model, "' yet")
  }
  bind_cols(base[rep(1, nrow(out)), ], out) |>
    mutate(time = time, model = name, model_conf = conf, pct_rep = pct, geo = "scraped",
           election = as.Date(election), district = as.integer(district)) |>
    relocate(state, election, jurisdiction, contest, district, candidate, party, pred_type, time,
             model, model_conf, pct_rep, rep, pred, pred_min, pred_max, p_win, geo) |>
    arrange(pred_type, desc(pred))
}

# every race x model in `returns`: list(pred = prediction table, status = one row per
# race x model). A model that errors on a race is recorded in status and the rest carry on.
# cbs: optional named list, race_id -> latest CBS county snapshot (read_cbs_latest()), and
# base = county_baseline(state): adds statewide rows (geo = "all") for those races.
predict_all = function(cells, returns, races = race_info(returns), models = MODELS,
                       time = max(returns$time), level = 0.95, cbs = NULL, base = NULL) {
  res = map(sort(unique(returns$race_id)), function(r) {
    ret = filter(returns, race_id == r)
    race = filter(races, race_id == r)
    if (nrow(race) != 1) stop("races must have exactly one row for race_id ", r)
    map(models, function(m) {
      fit = tryCatch(run_race(m, cells, ret), error = function(e) list(status = paste("error:", conditionMessage(e))))
      pred = tryCatch(to_pred_data(fit, m, cells, ret, race, time, level),
                      error = function(e) { fit$status <<- paste("error in output:", conditionMessage(e)); NULL })
      st_status = NA_character_
      if (!is.null(pred) && !is.null(cbs) && !is.null(base)) {
        if (is.null(cbs[[r]])) st_status = "no CBS county snapshot yet" else {
          st = tryCatch(state_pred_data(fit, m, cells, ret, race, time, cbs[[r]], base, level),
                        error = function(e) { st_status <<- paste("error:", conditionMessage(e)); NULL })
          if (!is.null(st)) { pred = bind_rows(pred, st); st_status = "ok" }
        }
      }
      list(pred = pred,
           status = tibble(race_id = r, model = unname(MODEL_NAMES[m]), time = time,
                           status = if (is.null(fit$status)) "error: no status" else fit$status,
                           statewide = st_status,
                           settings = if (is.null(fit$settings)) NA_character_ else fit$settings))
    })
  }) |> purrr::list_flatten()
  list(pred = bind_rows(lapply(res, `[[`, "pred")), status = bind_rows(lapply(res, `[[`, "status")))
}

# Writes, for the predictions and the status table each:
#   <dir>/predictions_<YYYYmmdd_HHMMSS>.csv, <dir>/status_<...>.csv   one per run, never
#                                         overwritten, so the night's history is kept
#   <dir>/predictions_latest.csv, <dir>/status_latest.csv             overwritten each run,
#                                         via a temporary file so a reader never sees half a file
# dir defaults to 26_general/model_results in the shared Dropbox, where the dashboard reads
# from. Point test runs somewhere else (e.g. tempdir()) so they never mix with the night's.
write_predictions = function(res, dir = file.path(find_26_general(), "model_results")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stamp = format(max(res$status$time), "%Y%m%d_%H%M%S", tz = "America/New_York")
  put = function(d, name) {
    if (!is.null(d) && nrow(d)) d = mutate(d, across(any_of("time"), \(x) format(x, "%Y-%m-%dT%H:%M:%S%z")),
                                           across(any_of("election"), \(x) format(x, "%Y-%m-%d")))
    readr::write_csv(d, file.path(dir, sprintf("%s_%s.csv", name, stamp)), na = "")
    tmp = file.path(dir, sprintf(".%s_latest.csv.tmp", name))
    readr::write_csv(d, tmp, na = "")
    file.rename(tmp, file.path(dir, sprintf("%s_latest.csv", name)))
  }
  put(if (nrow(res$pred)) res$pred else tibble(), "predictions")
  put(res$status, "status")
  invisible(file.path(dir, sprintf("predictions_%s.csv", stamp)))
}
