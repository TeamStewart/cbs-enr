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
##   model       the model's name (run.R: "<model>[/<demo>][/<interval>]", e.g. "swing")
##   model_conf  how the interval was made and its level (results.R), e.g.
##               "county_bootstrap_95" (swing) or "posterior_95" (pois_bayes);
##               NA when the model gives no interval (ratio_strata)
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
##   res = predict_all(cells, returns, races = race_info(returns), models = RUN)
##   res$pred, res$status
##   write_predictions(res)           to 26_general/model_results: a timestamped file per
##                                    run + "latest", for both tables
# ========================================

#' Race columns for each race in the returns
#'
#' The default suits the sims, where race_id is the contest (e.g. "gov"). For live races
#' use `races_from_priors()` instead.
#'
#' @param returns Standard returns.
#' @return Tibble, one row per race_id: race_id, state, election, jurisdiction, contest,
#'   district (NA).
race_info = function(returns) {
  returns |>
    distinct(race_id, state, election) |>
    mutate(jurisdiction = state, contest = race_id, district = NA_integer_)
}

#' Share of a race's precincts with any votes counted
#'
#' @param cells Cells (the race's precincts).
#' @param returns The race's returns.
#' @return Number in [0, 1].
pct_reporting = function(cells, returns) {
  rep_geo = unique(returns$geo[returns$votes > 0])
  mean(unique(cells$geo) %in% rep_geo)
}

#' Reported votes per candidate
#'
#' @param returns The race's returns.
#' @return Tibble: candidate, party (dem / rep / ind / oth), rep (votes counted so far).
reported_by_candidate = function(returns) {
  returns |>
    mutate(
      party = if_else(party %in% c("dem", "rep"), party, if_else(party == "ind", "ind", "oth"))
    ) |>
    summarize(rep = sum(votes), .by = c(candidate, party))
}

#' Point predictions of a fit with draws
#'
#' @param fit Model result.
#' @return Named numeric `c(dem, rep, tot)` of predicted final votes.
fit_point = function(fit) fit$point[c("dem", "rep", "tot")]

#' Did a fit succeed?
#'
#' `status` can carry detail ("ok (partial prior: 2 of 9 equations)"), so the test is the
#' prefix (CONTRACT.md section 15.2): anything starting "ok" is usable.
#'
#' A "prior_only" fit therefore publishes no predictions, deliberately: it returns the same
#' columns and intervals as a real fit, so publishing it would show the 2024 baseline
#' labelled as a model estimate. The race x model still appears in the status table with
#' the reason. MI is the live case (0.6% geo join). If the gap on the dashboard proves
#' worse, publish with a flag column rather than relax this test.
#'
#' @param fit Model result.
#' @return TRUE or FALSE.
fit_ok = function(fit) !is.null(fit$status) && grepl("^ok", fit$status)

#' Scraped-county prediction rows for one race x model
#'
#' race_id is carried as an extra column (CONTRACT.md section 15.1) rather than rebuilt
#' downstream: state / contest / district do not invert back to it (AK_house has
#' district 1 but no district in its id).
#'
#' @param fit `run_race()` result.
#' @param model Model name.
#' @param cells Cells the model fit.
#' @param returns The race's returns.
#' @param race One row of `races_from_priors()`.
#' @param time Snapshot time.
#' @param level Interval level.
#' @return Tibble in enightmodels' prediction format (one row per candidate x pred_type,
#'   "vote" and "share"), plus p_win, geo = "scraped" and race_id; NULL if the fit is not ok.
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
  name = model_label(model)
  model = model_base(model)
  pct = pct_reporting(cells, returns)
  if (!fit_ok(fit)) {
    return(NULL)
  }

  cand = reported_by_candidate(returns)
  conf = fit$conf %||% NA_character_
  if (is.null(fit$pred)) {
    # point predictions and draws by party, split among candidates by cand_split() (results.R)
    ptot = fit_point(fit)
    pt = c(
      dem = ptot[["dem"]],
      rep = ptot[["rep"]],
      oth = ptot[["tot"]] - ptot[["dem"]] - ptot[["rep"]]
    )
    dr = fit$draws
    tot_d = dr$turnout
    dv = list(dem = dr$dem, rep = dr$rep, oth = tot_d - dr$dem - dr$rep)
    q = function(x) unname(cred(x, level))
    cand = cand_split(cand)
    rows = lapply(seq_len(nrow(cand)), function(i) {
      p = cand$party[i]
      g = cand$grp[i]
      v = cand$w[i] * dv[[g]]
      point = cand$w[i] * pt[[g]]
      ci_v = q(v)
      ci_s = q(v / tot_d)
      pw = if (p == "dem") {
        mean(dr$dem > dr$rep)
      } else if (p == "rep") {
        mean(dr$rep > dr$dem)
      } else {
        NA_real_
      }
      tibble(
        candidate = cand$candidate[i],
        party = p,
        pred_type = c("vote", "share"),
        rep = c(cand$rep[i], cand$rep[i] / max(sum(cand$rep), 1)),
        pred = c(point, point / ptot[["tot"]]),
        pred_min = pmax(c(ci_v[1], ci_s[1]), 0),
        pred_max = c(ci_v[2], ci_s[2]),
        p_win = pw
      )
    })
    out = bind_rows(rows)
  } else {
    # per-candidate predictions (fit$pred), with bounds for conf_seq / conformal intervals;
    # the share version of a bound (bound / point total) is approximate
    pr = fit$pred |>
      left_join(select(cand, candidate, rep), by = "candidate") |>
      mutate(rep = coalesce(rep, 0))
    tot = sum(pr$pred)
    out = bind_rows(
      mutate(pr, pred_type = "vote"),
      mutate(
        pr,
        pred_type = "share",
        rep = rep / max(sum(rep), 1),
        across(c(pred, pred_min, pred_max), ~ .x / tot)
      )
    ) |>
      mutate(pred_min = pmax(pred_min, 0), pred_max = pmax(pred_max, 0), p_win = NA_real_)
  }
  bind_cols(base[rep(1, nrow(out)), ], out) |>
    mutate(
      time = time,
      model = name,
      model_conf = conf,
      pct_rep = pct,
      geo = "scraped",
      election = as.Date(election),
      district = as.integer(district)
    ) |>
    relocate(
      state,
      election,
      jurisdiction,
      contest,
      district,
      candidate,
      party,
      pred_type,
      time,
      model,
      model_conf,
      pct_rep,
      rep,
      pred,
      pred_min,
      pred_max,
      p_win,
      geo,
      race_id
    ) |>
    arrange(pred_type, desc(pred))
}

# ---- the per-precinct dump -----------------------------------------------------------
#' Precinct rows for one race x model
#'
#' Each model's `cells` slot is already in `PREC_COLS` shape (results.R); this adds race_id,
#' model and time.
#'
#' @param fit Model result.
#' @param model Model name.
#' @param race One row of `races_from_priors()`.
#' @param time Snapshot time.
#' @return Tibble with `PREC_COLS`, or NULL if the fit has no precinct rows.
to_prec_data = function(fit, model, race, time) {
  p = fit$cells
  if (!fit_ok(fit) || is.null(p) || !nrow(p)) {
    return(NULL)
  }
  prec_rows(p) |>
    mutate(race_id = race$race_id[1], model = model_label(model), time = time) |>
    select(all_of(PREC_COLS))
}

#' Write the precinct table
#'
#' Same timestamp rule and atomic `.tmp` + rename as `write_predictions()`, as parquet:
#' about 53k rows per model per refresh is too many for CSV over a night.
#'
#' @param res `predict_all()` / `run_jobs()` result (uses `res$prec`).
#' @param dir Output folder.
#' @return Path of the timestamped file, invisibly.
write_precincts = function(res, dir = file.path(find_26_general(), "model_results")) {
  if (is.null(res$prec) || !nrow(res$prec)) {
    return(invisible(NULL))
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  stamp = format(max(res$status$time), "%Y%m%d_%H%M%S", tz = "America/New_York")
  f = file.path(dir, sprintf("precincts_%s.parquet", stamp))
  write_pq(res$prec, f)
  tmp = file.path(dir, ".precincts_latest.parquet.tmp")
  write_pq(res$prec, tmp)
  file.rename(tmp, file.path(dir, "precincts_latest.parquet"))
  invisible(f)
}

#' One race x one model
#'
#' Never stops: a model that errors is recorded in status and returns no rows.
#'
#' @param cells Cells the model fits (`feed_cells()`).
#' @param ret That race's returns.
#' @param race Its row of `races_from_priors()`.
#' @param model Model name.
#' @param time Snapshot time.
#' @param cbs That race's latest CBS county snapshot (`read_cbs_latest()`), or NULL.
#' @param base `county_baseline(state)`, or NULL. With `cbs`, adds statewide rows.
#' @param level Interval level.
#' @return List: pred (prediction rows: scraped, plus statewide when `cbs` and `base` are
#'   given), prec (precinct rows), status (one row: race_id, model, time, status, statewide,
#'   seconds, settings).
predict_one = function(cells, ret, race, model, time, cbs = NULL, base = NULL, level = 0.95) {
  r = race$race_id[1]
  m = model
  t0 = proc.time()[["elapsed"]]
  # a by-method model on a race where only some counties' results are split by method:
  # fit it on those counties; the rest count as unscraped and come from CBS (run.R)
  note = NULL
  if (
    model_base(m) %in%
      METHOD_ONLY_MODELS &&
      nrow(ret) &&
      !returns_by_method(ret) &&
      !all(cells$method == "all")
  ) {
    mc = method_counties(ret)
    n_all = n_distinct(geo_county(ret$geo))
    if (length(mc) && any(geo_county(cells$geo) %in% mc)) {
      cells = filter(cells, geo_county(geo) %in% mc)
      ret = filter(ret, geo_county(geo) %in% mc)
      note = sprintf(
        "; fit on the %d of %d scraped counties whose results are split by vote method (the others come from CBS county results)",
        length(mc),
        n_all
      )
    }
  }
  args = if (model_base(m) == "medsl_sur") list(cbs_county = cbs) else list() # CBS county totals as constraints
  fit = tryCatch(do.call(fit_model, c(list(m, cells, ret), args)), error = function(e) {
    list(status = paste("error:", conditionMessage(e)))
  })
  if (!is.null(note) && !is.null(fit$settings) && !is.na(fit$settings)) {
    fit$settings = paste0(fit$settings, note)
  }
  pred = tryCatch(to_pred_data(fit, m, cells, ret, race, time, level), error = function(e) {
    fit$status <<- paste("error in output:", conditionMessage(e))
    NULL
  })
  st_status = NA_character_
  if (!is.null(pred) && !is.null(base)) {
    if (is.null(cbs)) {
      st_status = "no CBS county snapshot yet"
    } else {
      st = tryCatch(
        state_pred_data(fit, m, cells, ret, race, time, cbs, base, level),
        error = function(e) {
          st_status <<- paste("error:", conditionMessage(e))
          NULL
        }
      )
      if (!is.null(st)) {
        pred = bind_rows(pred, st)
        st_status = "ok"
      }
    }
  }
  prec = tryCatch(to_prec_data(fit, m, race, time), error = function(e) NULL)
  list(
    pred = pred,
    prec = prec,
    status = tibble(
      race_id = r,
      model = model_label(m),
      time = time,
      status = if (is.null(fit$status)) "error: no status" else fit$status,
      statewide = st_status,
      seconds = round(proc.time()[["elapsed"]] - t0, 1),
      settings = if (is.null(fit$settings)) NA_character_ else fit$settings
    )
  )
}

#' lapply in forked processes
#'
#' @param x List or vector.
#' @param f Function.
#' @param cores Parallel workers; on Windows it runs one at a time.
#' @return List of results, as `lapply(x, f)`.
run_par = function(x, f, cores = 1) {
  cores = max(1L, min(as.integer(cores), length(x)))
  if (cores > 1 && .Platform$OS.type == "unix") {
    parallel::mclapply(x, f, mc.cores = cores, mc.preschedule = FALSE)
  } else {
    lapply(x, f)
  }
}

#' Run a list of predict_one() jobs
#'
#' In parallel when `cores > 1` (forked processes, macOS and Linux only). Anything slow to
#' set up (`build_cells()`, `county_baseline()`, `setup_model()`) must be done before: a
#' forked process's caches are thrown away when it ends. A worker that dies is recorded in
#' status.
#'
#' @param jobs List of `predict_one()` argument lists (`make_jobs()`).
#' @param cores Parallel workers.
#' @return List: pred, prec, status, each bound across jobs.
run_jobs = function(jobs, cores = 1) {
  # each model's own set-up (MODEL_SETUP, run.R) for each state x county set, before forking
  for (j in jobs) {
    if (is.null(j$fail)) try(setup_model(j$model, j$cells, j$race$state[1]), silent = TRUE)
  }
  one = function(j) {
    tryCatch(
      if (!is.null(j$fail)) stop(j$fail) else do.call(predict_one, j[setdiff(names(j), "fail")]),
      error = function(e) {
        list(
          pred = NULL,
          status = tibble(
            race_id = j$race$race_id[1],
            model = model_label(j$model),
            time = j$time,
            status = paste("error:", conditionMessage(e)),
            statewide = NA_character_,
            seconds = NA_real_,
            settings = NA_character_
          )
        )
      }
    )
  }
  res = run_par(jobs, one, cores)
  # a forked process that died outright comes back as an error object, not a list
  res = lapply(seq_along(res), function(i) {
    if (is.list(res[[i]]) && !is.null(res[[i]]$status)) {
      res[[i]]
    } else {
      list(
        pred = NULL,
        status = tibble(
          race_id = jobs[[i]]$race$race_id[1],
          model = model_label(jobs[[i]]$model),
          time = jobs[[i]]$time,
          status = paste("error: worker failed:", as.character(res[[i]])),
          statewide = NA_character_,
          seconds = NA_real_,
          settings = NA_character_
        )
      )
    }
  })
  list(
    pred = bind_rows(lapply(res, `[[`, "pred")),
    prec = bind_rows(lapply(res, `[[`, "prec")),
    status = bind_rows(lapply(res, `[[`, "status"))
  )
}

#' Cells of the counties the feed covers
#'
#' Those with at least one precinct in the race's returns (a zero file lists every precinct
#' it will report, so this is known before any votes). The models fit only these counties;
#' every other county comes from CBS's county results in the statewide rows.
#'
#' @param cells Cells from `build_cells()`.
#' @param ret The race's returns.
#' @return The rows of `cells` in those counties.
feed_cells = function(cells, ret) {
  ctys = unique(cells$county[cells$geo %in% ret$geo])
  if (!length(ctys)) {
    stop("none of the feed's precinct ids match our precinct inputs (check the crosswalk)")
  }
  filter(cells, county %in% ctys)
}

#' predict_one() jobs for every race x model
#'
#' @param cells One state's cells.
#' @param returns Its returns.
#' @param races `races_from_priors()` rows.
#' @param models Model names.
#' @param time Snapshot time.
#' @param cbs Named list race_id -> CBS snapshot, or NULL.
#' @param base `county_baseline()`, or NULL.
#' @param level Interval level.
#' @return List of argument lists for `run_jobs()`.
make_jobs = function(cells, returns, races, models, time, cbs = NULL, base = NULL, level = 0.95) {
  unlist(
    lapply(sort(unique(returns$race_id)), function(r) {
      race = filter(races, race_id == r)
      if (nrow(race) != 1) {
        stop("races must have exactly one row for race_id ", r)
      }
      ret = filter(returns, race_id == r)
      cr = tryCatch(feed_cells(cells, ret), error = function(e) e)
      if (inherits(cr, "error")) {
        return(lapply(models, function(m) {
          list(
            cells = cells[0, ],
            ret = ret,
            race = race,
            model = m,
            time = time,
            cbs = NULL,
            base = NULL,
            level = level,
            fail = conditionMessage(cr)
          )
        }))
      }
      lapply(models, function(m) {
        list(
          cells = cr,
          ret = ret,
          race = race,
          model = m,
          time = time,
          cbs = if (is.null(cbs)) NULL else cbs[[r]],
          base = if (is.null(cbs)) NULL else base,
          level = level
        )
      })
    }),
    recursive = FALSE
  )
}

#' Every race x model in the returns
#'
#' A model that errors on a race is recorded in status and the rest carry on.
#'
#' @param cells Cells from `build_cells()`.
#' @param returns Standard returns.
#' @param races Race rows (`races_from_priors()`; `race_info()` for the sims).
#' @param models Model names.
#' @param time Snapshot time.
#' @param level Interval level.
#' @param cbs Optional named list race_id -> latest CBS county snapshot; with `base`, adds
#'   statewide rows (geo = "all").
#' @param base `county_baseline(state)`.
#' @param cores Parallel workers (`run_jobs()`).
#' @return List: pred (prediction table), prec (precinct table), status (one row per race x
#'   model, with `seconds` each took).
predict_all = function(
  cells,
  returns,
  races = race_info(returns),
  models = RUN,
  time = max(returns$time),
  level = 0.95,
  cbs = NULL,
  base = NULL,
  cores = 1
) {
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

#' Check a prediction table against enightmodels' schema
#'
#' Runs `enightmodels::check_pred_data()`, tolerating only its known bug
#' (`CHECK_PRED_BUG`), and then checks what that bug skips: the first columns are
#' `COLS_PRED` in order. Narrow on purpose, so a schema break fails here rather than on the
#' dashboard.
#'
#' @param d Prediction table.
#' @param cols Required leading columns.
#' @return TRUE, invisibly; stops on a schema break.
check_pred = function(d, cols = enightmodels::COLS_PRED) {
  if ("district" %in% names(d) && is.logical(d$district) && all(is.na(d$district))) {
    d$district = NA_integer_
  } # CSV round-trip artifact, see above
  tryCatch(enightmodels::check_pred_data(d), error = function(e) {
    if (!identical(conditionMessage(e), CHECK_PRED_BUG)) stop(e)
  })
  # what the unreachable block would have checked, scoped to the CORE columns. It cannot be
  # identical() over all of names(d) because section 15 permits extras AFTER the 16 -- which
  # also catches extras placed BEFORE them, since that reorders the core block.
  if (!identical(names(d)[seq_along(cols)], cols)) {
    stop(
      "predictions: the first ",
      length(cols),
      " columns are not COLS_PRED in order.",
      "\n  expected: ",
      paste(cols, collapse = ", "),
      "\n  got:      ",
      paste(utils::head(names(d), length(cols)), collapse = ", ")
    )
  }
  # Narrow on purpose. A broad tryCatch would turn this gate into decoration and a schema
  # break would surface as a broken dashboard at 20:00 instead of a failed test now. It
  # also retires itself silently once upstream is fixed: the tryCatch simply stops firing.
  invisible(TRUE)
}

#' Write the prediction and status tables
#'
#' Writes `predictions_<time>.csv` and `status_<time>.csv`, and replaces
#' `predictions_latest.csv` / `status_latest.csv` atomically (`.tmp` + rename). A schema
#' problem only warns: one bad row must not stop the other races from being written (the
#' tests call `check_pred()` directly and do stop).
#'
#' @param res `predict_all()` / `run_jobs()` result.
#' @param dir Output folder.
#' @return Path of the timestamped predictions file, invisibly.
write_predictions = function(res, dir = file.path(find_26_general(), "model_results")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  # WARN, do not abort. If the table is malformed the dashboard is in trouble either way,
  # but one bad row -- say one race with an unexpected party string -- must not stop the
  # other eleven races' predictions from being written. The test harness calls check_pred()
  # directly and DOES abort; that is where a schema break should stop the world.
  if (!is.null(res$pred) && nrow(res$pred)) {
    tryCatch(check_pred(res$pred), error = function(e) {
      cli::cli_alert_danger(
        "predictions FAIL the schema gate: \
                                  {conditionMessage(e)}"
      )
    })
  }
  stamp = format(max(res$status$time), "%Y%m%d_%H%M%S", tz = "America/New_York")
  put = function(d, name) {
    if (!is.null(d) && nrow(d)) {
      d = mutate(
        d,
        across(any_of("time"), \(x) format(x, "%Y-%m-%dT%H:%M:%S%z")),
        across(any_of("election"), \(x) format(x, "%Y-%m-%d"))
      )
    }
    readr::write_csv(d, file.path(dir, sprintf("%s_%s.csv", name, stamp)), na = "")
    tmp = file.path(dir, sprintf(".%s_latest.csv.tmp", name))
    readr::write_csv(d, tmp, na = "")
    file.rename(tmp, file.path(dir, sprintf("%s_latest.csv", name)))
  }
  put(if (nrow(res$pred)) res$pred else tibble(), "predictions")
  put(res$status, "status")
  invisible(file.path(dir, sprintf("predictions_%s.csv", stamp)))
}
