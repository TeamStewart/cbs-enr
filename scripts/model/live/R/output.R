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
##   model       "model_simpleswing", "model_cory", "model_medsl"
##   model_conf  how the interval was made and its level, e.g. "county_bootstrap_95"
##               (model_simpleswing) or "posterior_95" (model_medsl);
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
## seconds (how long that model took on that race), settings. So a broken model shows up there instead of
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

MODEL_NAMES = c(swing = "model_simpleswing", cory = "model_cory", medsl = "model_medsl",
                cpois = "model_cory_pois", cprec = "model_cory_prec")

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

# predicted final votes c(dem, rep, tot) from a model with draws: its `point` if it gives
# one (model_medsl: the posterior mean), else the sum of its cells (model_simpleswing)
fit_point = function(fit) {
  if (!is.null(fit$point)) return(fit$point[c("dem", "rep", "tot")])
  c(dem = sum(fit$cells$dem), rep = sum(fit$cells$rep), tot = sum(fit$cells$tot))
}

# fit: one model's result for one race (run_race()); returns: that race's returns
# Did the fit succeed? `status` CARRIES DETAIL now -- "ok (partial prior: 2 of 9
# equations)" -- so an exact match on "ok" drops a model's entire output. That is not
# hypothetical: the first time the prior-only alarm landed, predictions went 128 -> 96 rows
# because all three call sites tested identical(fit$status, "ok") and model_medsl silently
# produced nothing, with `statewide` NA and no precinct rows either. The prefix is the
# contract (CONTRACT.md section 15.2): anything starting "ok" is a usable fit.
#
# AND THEREFORE `prior_only` EMITS NO PREDICTIONS -- deliberately, not as a side effect of
# the prefix. A prior-only fit returns the same columns, point estimates and intervals as a
# real one, so publishing them means publishing the 2024 baseline labelled as a model
# estimate. Nothing downstream could tell the difference. Section 15.2's invariant is
# satisfied either way: the race x model still appears in `status`, with the reason, so this
# is "no prediction and we say why" rather than a silent gap.
# The trade is a hole in the dashboard for an unjoined state versus a misleading number in
# it. MI is the live case (0.6% geo join, `method = "all"` with no cell). If the hole turns
# out to be worse, the change is to publish with a flag column, NOT to relax this test.
fit_ok = function(fit) !is.null(fit$status) && grepl("^ok", fit$status)

to_pred_data = function(fit, model, cells, returns, race, time, level = 0.95) {
  # race_id is carried as a permitted extra (section 15.1), NOT reconstructed downstream.
  # It is `<STATE>_<contest>[_<district>]` and COLS_PRED has no slot for it, so it gets
  # decomposed into state/contest/district -- and that decomposition is NOT INVERTIBLE:
  # parse_race_id() SYNTHESISES district = 1 for house races in at-large states, so
  # AK_house arrives as contest "house" + district 1 with no district in its id. Measured
  # on the current 12 races: paste(state, contest) recovers race_id 12 of 12 but
  # paste(state, contest, district) only 11 of 12, and a districted id like NY_house_12
  # breaks the first rule. So NO paste rule covers both shapes, and the wrong one fails
  # SILENTLY -- an anti-join reports every row missing rather than erroring. Carrying the
  # key costs one column and removes the whole class.
  base = race |> select(race_id, state, election, jurisdiction, contest, district)
  name = unname(MODEL_NAMES[model])
  pct = pct_reporting(cells, returns)
  if (!fit_ok(fit)) return(NULL)

  cand = reported_by_candidate(returns)
  conf = NA_character_
  if (model %in% c("swing", "medsl", "cprec")) {
    # point predictions by party; third-party votes split across third-party candidates
    # in proportion to their counted votes (equally if none counted yet)
    ptot = fit_point(fit)
    pv = ptot[c("dem", "rep")]
    oth_tot = ptot[["tot"]] - pv[["dem"]] - pv[["rep"]]
    dr = fit$draws
    tot_d = dr$turnout
    conf = sprintf(switch(model, swing = "county_bootstrap_%g", medsl = "posterior_%g",
                          cprec = "cred_int_%g"), 100 * level)
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
             pred = c(point, point / ptot[["tot"]]),
             pred_min = pmax(c(ci_v[1], ci_s[1]), 0), pred_max = c(ci_v[2], ci_s[2]), p_win = pw)
    })
    out = bind_rows(rows)
  } else if (model == "cory") {
    pr = cory_by_candidate(fit) |>    # with any virtual-precinct votes (virtual.R)
      left_join(select(cand, candidate, rep), by = "candidate") |>
      mutate(rep = coalesce(rep, 0))
    tot = sum(pr$pred)
    out = bind_rows(
      mutate(pr, pred_type = "vote"),
      mutate(pr, pred_type = "share", rep = rep / max(sum(rep), 1), pred = pred / tot)) |>
      mutate(pred_min = NA_real_, pred_max = NA_real_, p_win = NA_real_)
  } else if (model == "cpois") {
    # model_cory_pois: like cory in shape (per-candidate point predictions in raw$pred,
    # no draws) but it DOES carry interval ends -- split-conformal on the within-cell share
    # scale, summed over methods. So aggregate pred/pred_min/pred_max together.
    # TAKE `party` FROM THE RETURNS, NOT FROM raw$pred. fit_cpois runs model_prec_pois in
    # its `party_est = "pooled"` mode, which labels every d_cov row party = "all" so one
    # covariate frame serves all candidates -- and that label propagates into raw$pred. If
    # we aggregated on it, every candidate would come out party = "all", which is not a
    # legal COLS_PRED party and destroys the dem/rep split the dashboard differences for
    # margins. `cand` (reported_by_candidate) carries the authoritative party.
    pr = (if (is.data.frame(fit$raw)) fit$raw else fit$raw$pred) |>
      mutate(candidate = as.character(candidate)) |>
      summarize(across(c(pred, pred_min, pred_max), sum), .by = candidate) |>
      left_join(select(cand, candidate, party, rep), by = "candidate") |>
      mutate(rep = coalesce(rep, 0), party = coalesce(as.character(party), "oth"))
    tot = sum(pr$pred)
    conf = sprintf("approx_conform_%g", 100 * level)
    # The share bounds are pred_min/tot, i.e. a VOTE bound divided by the point total. That
    # is approximate for the same reason fit_cpois's lo_twop/hi_twop are: a conformal bound
    # on a candidate's votes is not a conformal bound on that candidate's share, because the
    # denominator carries no uncertainty of its own. Direction is right, width is not exact.
    # Flagged rather than hidden -- `model_conf` says `approx_`.
    out = bind_rows(
      mutate(pr, pred_type = "vote"),
      mutate(pr, pred_type = "share", rep = rep / max(sum(rep), 1),
             across(c(pred, pred_min, pred_max), ~ .x / tot))) |>
      mutate(pred_min = pmax(pred_min, 0), pred_max = pmax(pred_max, 0), p_win = NA_real_)
  } else {
    stop("to_pred_data() has no rule for model '", model, "' yet")
  }
  bind_cols(base[rep(1, nrow(out)), ], out) |>
    mutate(time = time, model = name, model_conf = conf, pct_rep = pct, geo = "scraped",
           election = as.Date(election), district = as.integer(district)) |>
    relocate(state, election, jurisdiction, contest, district, candidate, party, pred_type, time,
             model, model_conf, pct_rep, rep, pred, pred_min, pred_max, p_win, geo, race_id) |>
    arrange(pred_type, desc(pred))
}

# one race x one model: list(pred = its prediction rows (scraped, plus statewide when `cbs`
# and `base` are given), status = one row). Never stops: a model that errors is recorded in
# status and returns no rows.
# ret: that race's returns; race: its one row of `races`; cbs: that race's latest CBS county
# snapshot (read_cbs_latest()) or NULL; base: county_baseline(state) or NULL.
# ---- the per-precinct dump -----------------------------------------------------------
# Normalise each model's `cells` slot to one schema. That slot is NOT uniform: model_medsl
# returns an 18-column frame keyed by geo, model_swing a precinct x METHOD frame with
# dem/rep/tot, model_cory a precinct frame with dem/rep/tot and only when
# precincts = TRUE, and model_cory_pois nothing at all (it summarises geo away). So this is
# a per-model adapter, not a passthrough.
#
# Two defects in the pre-existing flag-gated dumps are deliberately NOT inherited:
# `n_done_modes`, which summed an ambient vector and read 10,956 on a 227-registrant
# precinct (use q_share), and the trial/swing branches writing IDENTICAL filenames with
# different schemas so one silently overwrote the other.
PREC_COLS = c("race_id", "model", "time", "geo", "county", "method",
              "turn_est", "turn_lo", "turn_hi", "dem_est", "dem_lo", "dem_hi",
              "est_dem2", "cnt_prec", "q_share", "n_obs_modes", "p_win")

to_prec_data = function(fit, model, race, time) {
  p = fit$cells
  if (!fit_ok(fit) || is.null(p) || !nrow(p)) return(NULL)
  blank = function(d, cols) { for (c in setdiff(cols, names(d))) d[[c]] = NA_real_; d }
  out = if (model == "medsl") {
    # already the richest shape; est_dem2/p_win/cnt_prec added to prec_out for this
    p |> mutate(method = NA_character_)
  } else if (model == "cprec") {
    # forecast_prec gives a frame as rich as medsl's -- turnout and share intervals,
    # est_dem2 and cnt_prec are all already there -- but under its own names, and with
    # cell counts where medsl has mode counts. Only renames, no derivation.
    # It has no q_share: that is the pin's gate in model_turnout.R and has no analogue
    # here, so it stays NA rather than being faked from n_done_cells.
    p |> mutate(method = NA_character_,
                p_win = p_dem_win,
                n_obs_modes = n_obs_cells,
                q_share = NA_real_)
  } else if (model %in% c("swing", "cory", "cpois")) {
    # dem/rep/tot, no interval.
    #
    # COLLAPSED TO PRECINCT. swing and cpois both fit at precinct x METHOD and return that
    # grain natively; cory already aggregates it away. Summing dem/rep/tot over methods
    # makes one row per precinct for every model, so the dump is comparable across models
    # and a row count per precinct is 1 rather than 1-or-3. It also matters for volume: at
    # full scale the method grain was 3x for two of four models -- 158,340 of 263,900 rows
    # per refresh, about 38M over a 12-hour night at 5-minute refreshes.
    #
    # What this discards: the per-mode split, and for swing specifically its `swing` and
    # `log_turnout_ratio` columns (the fitted county x method swing and log turnout ratio),
    # which are its most diagnostic output. If those are wanted later, the cheap route is
    # per-mode COLUMNS at precinct grain rather than a return to per-mode rows.
    p |>
      summarize(across(c(dem, rep, tot), sum), .by = any_of(c("geo", "county"))) |>
      mutate(
        method   = NA_character_,
        county   = if ("county" %in% names(p)) as.character(county) else NA_character_,
        turn_est = tot,
        dem_est  = dem / pmax(tot, 1e-9),
        est_dem2 = dem / pmax(dem + rep, 1e-9))
  } else return(NULL)
  # Identity columns FIRST, then blank the rest. blank() creates every missing PREC_COL as
  # NA, so if it ran first it would create `model` and `time` as NA columns and the mutate
  # below would read those NAs out of the data mask instead of the function arguments --
  # silently stamping every row with model = NA.
  nm = unname(MODEL_NAMES[model]); tm = time
  out |>
    mutate(race_id = race$race_id[1], model = nm, time = tm, geo = as.character(geo)) |>
    blank(PREC_COLS) |>
    select(all_of(PREC_COLS))
}

# Mirrors write_predictions() exactly -- same timestamp rule, same atomic .tmp + rename,
# same stamped-plus-`latest` pair -- but PARQUET. Volume is why: precincts x races is about
# 52.8k rows per model per refresh, so ~211k across four models and ~30M over a 12-hour
# night at 5-minute refreshes. CSV is not viable at that size.
write_precincts = function(res, dir = file.path(find_26_general(), "model_results")) {
  if (is.null(res$prec) || !nrow(res$prec)) return(invisible(NULL))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stamp = format(max(res$status$time), "%Y%m%d_%H%M%S", tz = "America/New_York")
  f = file.path(dir, sprintf("precincts_%s.parquet", stamp))
  write_pq(res$prec, f)
  tmp = file.path(dir, ".precincts_latest.parquet.tmp")
  write_pq(res$prec, tmp)
  file.rename(tmp, file.path(dir, "precincts_latest.parquet"))
  invisible(f)
}

predict_one = function(cells, ret, race, model, time, cbs = NULL, base = NULL, level = 0.95) {
  r = race$race_id[1]; m = model
  t0 = proc.time()[["elapsed"]]
  # votes reported in virtual precincts (NC's county-level early / absentee batches) are
  # fit around and added back at county level for model_simpleswing and model_cory
  # (virtual.R). The other models are left as they are: model_medsl takes those votes as
  # county x mode observations itself (bucket_counts(), returns.R).
  fit = tryCatch({
    if (m %in% VIRTUAL_MODELS) {
      vs = virtual_split(cells, ret)
      add_virtual(run_race(m, vs$cells, vs$returns), m, vs)
    } else run_race(m, cells, ret)
  }, error = function(e) list(status = paste("error:", conditionMessage(e))))
  pred = tryCatch(to_pred_data(fit, m, cells, ret, race, time, level),
                  error = function(e) { fit$status <<- paste("error in output:", conditionMessage(e)); NULL })
  st_status = NA_character_
  if (!is.null(pred) && !is.null(base)) {
    if (is.null(cbs)) st_status = "no CBS county snapshot yet" else {
      st = tryCatch(state_pred_data(fit, m, cells, ret, race, time, cbs, base, level),
                    error = function(e) { st_status <<- paste("error:", conditionMessage(e)); NULL })
      if (!is.null(st)) { pred = bind_rows(pred, st); st_status = "ok" }
    }
  }
  prec = tryCatch(to_prec_data(fit, m, race, time), error = function(e) NULL)
  list(pred = pred, prec = prec,
       status = tibble(race_id = r, model = unname(MODEL_NAMES[m]), time = time,
                       status = if (is.null(fit$status)) "error: no status" else fit$status,
                       statewide = st_status, seconds = round(proc.time()[["elapsed"]] - t0, 1),
                       settings = if (is.null(fit$settings)) NA_character_ else fit$settings))
}

# lapply(x, f), in parallel forked processes when cores > 1 (macOS / Linux; on Windows it
# runs one at a time)
run_par = function(x, f, cores = 1) {
  cores = max(1L, min(as.integer(cores), length(x)))
  if (cores > 1 && .Platform$OS.type == "unix") parallel::mclapply(x, f, mc.cores = cores, mc.preschedule = FALSE)
  else lapply(x, f)
}

# Run a list of predict_one() jobs, in parallel when cores > 1 (forked processes, so macOS
# and Linux only; on Windows it runs one at a time). Each job is a list of predict_one()'s
# arguments. Anything slow to set up (build_cells(), county_baseline(), medsl_setup()) must
# be done BEFORE this is called: a forked process's caches are thrown away when it ends.
run_jobs = function(jobs, cores = 1) {
  # model_medsl's set-up for each state x county set, here before forking (see above)
  for (j in jobs) if (identical(j$model, "medsl") && nrow(j$cells) && is.null(j$fail))
    try(fit_medsl_setup_for(j$cells, j$ret), silent = TRUE)
  one = function(j) tryCatch(if (!is.null(j$fail)) stop(j$fail) else do.call(predict_one, j[setdiff(names(j), "fail")]), error = function(e)
    list(pred = NULL, status = tibble(race_id = j$race$race_id[1], model = unname(MODEL_NAMES[j$model]), time = j$time,
                                      status = paste("error:", conditionMessage(e)), statewide = NA_character_,
                                      seconds = NA_real_, settings = NA_character_)))
  res = run_par(jobs, one, cores)
  # a forked process that died outright comes back as an error object, not a list
  res = lapply(seq_along(res), function(i) if (is.list(res[[i]]) && !is.null(res[[i]]$status)) res[[i]] else
    list(pred = NULL, status = tibble(race_id = jobs[[i]]$race$race_id[1], model = unname(MODEL_NAMES[jobs[[i]]$model]),
                                      time = jobs[[i]]$time, status = paste("error: worker failed:", as.character(res[[i]])),
                                      statewide = NA_character_, seconds = NA_real_, settings = NA_character_)))
  list(pred = bind_rows(lapply(res, `[[`, "pred")),
       prec = bind_rows(lapply(res, `[[`, "prec")),
       status = bind_rows(lapply(res, `[[`, "status")))
}

# The counties the feed covers for a race: those with at least one of their precincts in the
# race's returns (a zero file lists every precinct it will report, so this is known before any
# votes). The models fit only these counties' precincts, and every other county -- including
# counties in our precinct inputs whose live ids we don't have -- comes from CBS's county
# results in the statewide rows. (As of 2026-10-22 the crosswalks have live ids for FL 18 of
# 31 counties, MI 10 of 31, TX 4 of 32, CA none.)
feed_cells = function(cells, ret) {
  ctys = unique(cells$county[cells$geo %in% ret$geo])
  if (!length(ctys)) stop("none of the feed's precinct ids match our precinct inputs (check the crosswalk)")
  filter(cells, county %in% ctys)
}

# the predict_one() jobs for every race x model in `returns` (one state's cells)
make_jobs = function(cells, returns, races, models, time, cbs = NULL, base = NULL, level = 0.95) {
  unlist(lapply(sort(unique(returns$race_id)), function(r) {
    race = filter(races, race_id == r)
    if (nrow(race) != 1) stop("races must have exactly one row for race_id ", r)
    ret = filter(returns, race_id == r)
    cr = tryCatch(feed_cells(cells, ret), error = function(e) e)
    if (inherits(cr, "error")) return(lapply(models, function(m) list(cells = cells[0, ], ret = ret, race = race, model = m,
                                                                      time = time, cbs = NULL, base = NULL, level = level,
                                                                      fail = conditionMessage(cr))))
    lapply(models, function(m) list(cells = cr, ret = ret, race = race, model = m, time = time,
                                    cbs = if (is.null(cbs)) NULL else cbs[[r]], base = if (is.null(cbs)) NULL else base, level = level))
  }), recursive = FALSE)
}

# every race x model in `returns`: list(pred = prediction table, status = one row per
# race x model, with `seconds` each took). A model that errors on a race is recorded in
# status and the rest carry on.
# cbs: optional named list, race_id -> latest CBS county snapshot (read_cbs_latest()), and
# base = county_baseline(state): adds statewide rows (geo = "all") for those races.
# cores: run the race x model fits in parallel (see run_jobs()).
predict_all = function(cells, returns, races = race_info(returns), models = MODELS,
                       time = max(returns$time), level = 0.95, cbs = NULL, base = NULL, cores = 1) {
  run_jobs(make_jobs(cells, returns, races, models, time, cbs, base, level), cores)
}

# Writes, for the predictions and the status table each:
#   <dir>/predictions_<YYYYmmdd_HHMMSS>.csv, <dir>/status_<...>.csv   one per run, never
#                                         overwritten, so the night's history is kept
#   <dir>/predictions_latest.csv, <dir>/status_latest.csv             overwritten each run,
#                                         via a temporary file so a reader never sees half a file
# dir defaults to 26_general/model_results in the shared Dropbox, where the dashboard reads
# from. Point test runs somewhere else (e.g. tempdir()) so they never mix with the night's.
# ---- the schema gate (CONTRACT.md section 15's acceptance test) ---------------------
# enightmodels::check_pred_data() CANNOT RETURN, for any input. Its last block is
#     if (isTRUE(pred)) assert(identical(names(d), COLS_PRED), ...)
# and `pred` is not one of its arguments -- formals() is just `d`, and `pred` is not an
# object in the namespace either. It is a copy-paste from the sibling check_elec_data(d,
# full, strict), whose IDENTICAL block is guarded by `strict`, a real argument defaulting
# FALSE: `strict` was renamed to `pred` on the way across and never added to the signature.
# So every call ends `object 'pred' not found` -- AFTER all ten substantive checks have run
# and passed, because the block is last.
#
# Reported upstream. NOT patched locally: renv.lock pins enightmodels to RemoteSha
# c99797a66 and the installed copy ships no .R sources (only .rdb/.rdx), so a local patch
# would be undone by the next renv::restore() -- i.e. precisely on a fresh checkout on the
# night, which is when it would matter. Upstream needs two tokens:
#     check_pred_data = function(d, strict = FALSE)   and   if (isTRUE(strict))
# `strict` MUST default to FALSE. Defaulting it TRUE would assert identical() over ALL of
# names(d), which rejects our own output -- section 15 permits the extras p_win, geo and
# cbs_as_of -- and is almost certainly why the sibling has it off.
#
# SECOND, SEPARATE BUG, AND IT IS OURS. `district` is NA for every statewide race, and a
# column that is entirely NA carries no type in a CSV -- so readr types it `logical` on
# read-back and the package's `is.integer(d$district)` assertion fails. Measured on our own
# predictions_latest.csv: 160 of 160 rows NA, class logical, and check_pred_data() aborts on
# `district column must be a positive integer` BEFORE it ever reaches the `pred` bug. So the
# section 15 acceptance test -- check_pred_data(read_csv(predictions_latest.csv)) -- fails
# on the read, not on the data. Writing it as integer cannot help: NA_integer_ still writes
# as an empty field. So this coerces exactly that provable case and nothing else; a logical
# `district` with any non-NA value is a real violation and must still fail.
CHECK_PRED_BUG = "object 'pred' not found"

check_pred = function(d, cols = enightmodels::COLS_PRED) {
  if ("district" %in% names(d) && is.logical(d$district) && all(is.na(d$district)))
    d$district = NA_integer_                      # CSV round-trip artifact, see above
  tryCatch(enightmodels::check_pred_data(d),
           error = function(e) if (!identical(conditionMessage(e), CHECK_PRED_BUG)) stop(e))
  # what the unreachable block would have checked, scoped to the CORE columns. It cannot be
  # identical() over all of names(d) because section 15 permits extras AFTER the 16 -- which
  # also catches extras placed BEFORE them, since that reorders the core block.
  if (!identical(names(d)[seq_along(cols)], cols))
    stop("predictions: the first ", length(cols), " columns are not COLS_PRED in order.",
         "\n  expected: ", paste(cols, collapse = ", "),
         "\n  got:      ", paste(utils::head(names(d), length(cols)), collapse = ", "))
  # Narrow on purpose. A broad tryCatch would turn this gate into decoration and a schema
  # break would surface as a broken dashboard at 20:00 instead of a failed test now. It
  # also retires itself silently once upstream is fixed: the tryCatch simply stops firing.
  invisible(TRUE)
}

write_predictions = function(res, dir = file.path(find_26_general(), "model_results")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  # WARN, do not abort. If the table is malformed the dashboard is in trouble either way,
  # but one bad row -- say one race with an unexpected party string -- must not stop the
  # other eleven races' predictions from being written. The test harness calls check_pred()
  # directly and DOES abort; that is where a schema break should stop the world.
  if (!is.null(res$pred) && nrow(res$pred))
    tryCatch(check_pred(res$pred),
             error = function(e) cli::cli_alert_danger("predictions FAIL the schema gate: \
                                  {conditionMessage(e)}"))
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
