# ========================================
## model_cory.R -- model_cory: Cory McCartan's enightmodels::model_legacy(), called as is.
##
## Moved from section D of scripts/model/tests/test_sim_night.R (2026-modeling-initial,
## commit dc7a4d4). Default configuration: model_legacy_config() (by method,
## strata = geostratum, stage_complete = 6). Point estimates only -- no interval.
##
## Input is the standard election-data format: one row per precinct x method x candidate
## with `votes` so far and the precinct's `stage` (0 / 4 / 6), plus `past`, the expected
## votes for that row = the cell's expected votes x the candidate's baseline share:
##   dem        (1 - oth) * dem2
##   rep        (1 - oth) * (1 - dem2)
##   write-in   oth * 0.15
##   any other  oth * 0.85
## The 85 / 15 third-party split is the one in Ethan's simulated nights. CHECK BEFORE LIVE
## USE: it assumes at most one non-write-in third-party candidate (two would each get 85%),
## and the candidate must be named exactly "write-in".
# ========================================

cory_past_share = function(party, candidate, dem2, oth)
  case_when(party == "dem" ~ (1 - oth) * dem2, party == "rep" ~ (1 - oth) * (1 - dem2),
            candidate == "write-in" ~ oth * 0.15, TRUE ~ oth * 0.85)

# cells:   build_cells() output
# returns: standard returns for ONE race (returns.R)
# precincts: also return per-precinct predictions (`cells`). Off by default: nothing on
#   election night uses them, and they cost a second full pass of model_legacy_total() /
#   model_legacy_cand(), which doubles the model's run time.
fit_cory = function(cells, returns, cfg = NULL, past_share = cory_past_share, precincts = FALSE) {
  if (!requireNamespace("enightmodels", quietly = TRUE))
    stop("model_cory needs Cory's enightmodels package (it is in the project's renv.lock)")
  if (is.null(cfg)) cfg = enightmodels::model_legacy_config()
  settings = paste0("enightmodels::model_legacy() with config (",
                    paste(names(cfg), vapply(cfg, \(x) paste(x, collapse = ","), ""), sep = " = ", collapse = "; "),
                    "); past = expected votes per precinct x method x candidate; no interval")

  rows = returns |>
    left_join(select(cells, geo, method, geostratum, dem2, oth, exp_votes), by = c("geo", "method")) |>
    mutate(past = exp_votes * past_share(party, candidate, dem2, oth))
  # race ids are "<STATE>_<contest>[_<district>]" live (races.R) and just the contest
  # ("gov") in Ethan's sims; enightmodels needs the contest and district on their own
  rid = returns$race_id[1]
  ri = if (grepl("^[A-Za-z]{2}_", rid)) parse_race_id(rid) else tibble(contest = rid, district = NA_integer_)
  dd = rows |>
    transmute(state, election = as.Date(election), jurisdiction = state, contest = ri$contest,
              district = ri$district, candidate, party, time, stage, method, geo, votes, geostratum, past) |>
    enightmodels::coerce_elec_data()
  out = enightmodels::model_legacy(dd, past, cfg)
  # model_legacy() returns the prediction table itself in enightmodels builds from early 2026
  # (e.g. 4944994) and list(pred =, coef =) in later ones
  pr = if (is.data.frame(out)) out else out$pred
  dem = sum(pr$pred[pr$party == "dem"]); repv = sum(pr$pred[pr$party == "rep"])

  cc = cell_counts(cells, returns)
  # per-precinct predictions, if asked for: the same two functions model_legacy() calls internally
  prec = NULL
  if (precincts) {
    d_tot = enightmodels::model_legacy_total(dd, past, cfg)
    prec = enightmodels::model_legacy_cand(dd, d_tot, past, cfg) |>
      mutate(geo = as.character(geo), party = as.character(party)) |>
      summarize(dem = sum(pred[party == "dem"]), rep = sum(pred[party == "rep"]), tot = sum(pred), .by = geo)
  }

  list(model = "cory", status = "ok", settings = settings,
       race = tibble(status = "ok", est_twop = dem / (dem + repv), lo_twop = NA_real_, hi_twop = NA_real_,
                     p_dem_win = NA_real_, est_turnout = sum(pr$pred), lo_turnout = NA_real_, hi_turnout = NA_real_,
                     raw_twop = sum(cc$c_dem) / max(sum(cc$c_dem + cc$c_rep), 1), counted_votes = sum(cc$c_tot),
                     expected_votes = sum(cells$exp_votes),
                     obs_cells = sum(cc$c_tot > 0), complete_cells = sum(cc$c_tot > 0 & cc$complete)),
       cells = prec,          # by precinct (NULL unless precincts = TRUE)
       draws = NULL,
       raw = out)
}
