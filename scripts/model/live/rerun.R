# ========================================
## rerun.R -- election night: redo past model points on the dashboard.
##
## The dashboard's model time series is every model_results/predictions_*.csv stacked,
## one point per race x model x geo x time; when two files have the same point, the file
## that sorts last wins (read_model_results() in scripts/functions.R). This script reruns
## chosen models for chosen races at chosen past times, on the returns and CBS snapshot
## we had then, and publishes them as model_results/predictions_rerun_<id>.csv (plus
## turnout_rerun_<id>.csv). "rerun_" sorts after "latest" and after every timestamp, so a
## published rerun replaces those points. Nothing else is touched: no status_*, no
## precincts_*, no *_latest file, no targets store, so it is safe to run while
## automation.R is running, on any machine with the Dropbox. The dashboard picks it up on
## the next tar_make().
##
## Returns as of time T: for each jurisdiction that has a {ST}/clean/<name>_latest.parquet,
## its newest archived scrape {ST}/clean/<name>_YYYY_MM_DD_HH_MM_SS.parquet (stamped in ET)
## at or before T; if it has none by then, its _latest file if that is no newer than T
## (the zero file); otherwise it is left out. CBS county results: the newest saved
## snapshot in 26_general/cbs_api/<env>/ at or before T + --cbs-lag minutes (default 5,
## since the live run pulled CBS a little after the scrape). Each race's point time is its
## latest return, as in the pipeline, so a rerun at a history point's time lands on that
## point.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/rerun.R points --states=TX --from=20:00 --to=21:30
##       list the dashboard's points in a window (no model run)
##   Rscript scripts/model/live/rerun.R run --races=TX_sen --models='swing*' --from=20:00 --to=21:30
##       rerun every existing point in the window; staged, compared with the dashboard,
##       not published
##   Rscript scripts/model/live/rerun.R run --states=GA --at=19:45,20:15 --publish
##       rerun at these times (each snaps to the latest scrape at or before it) and publish
##   Rscript scripts/model/live/rerun.R publish <id>     publish a staged rerun
##   Rscript scripts/model/live/rerun.R unpublish <id>   take it off the dashboard again
##   Rscript scripts/model/live/rerun.R drop --races=NC_sen --models='medsl_sur*' --from=19:30 --to=19:50 --note="bad scrape"
##       hide points from the dashboard (model_results/drops.csv), for points no rerun
##       can fix; drops hide reruns in the window too
##   Rscript scripts/model/live/rerun.R undrop <row>     remove a drop (row number from list)
##   Rscript scripts/model/live/rerun.R list             staged / published reruns and drops
##
## Or in an R session:
##   source("scripts/model/live/rerun.R")
##   rerun("run", "--races=TX_sen", "--models=swing*", "--from=20:00", "--to=21:00")
##
## Options:
##   --states=TX,GA      every priors.csv race in these states ("all" = every state)
##   --races=TX_sen,...  these race_ids (combined with --states)
##   --models=a,b*       comma-separated globs against model names. Default: every model
##                       in the window's history (points / run --from) or every run = TRUE
##                       model in model_config.csv (run --at). A full model name not in the
##                       config also works, e.g. pois_bayes/strat_d/conformal
##   --from, --to        time window, inclusive (run, points, drop)
##   --at=T1,T2          explicit times (run, or one time for drop)
##                       Times are ET: "2026-11-03 20:15", or "20:15" on ELECTION_DATE,
##                       where an hour before 12 ("00:40") means the next morning
##   --geo=all|scraped   only compare/print this geo (all rows are still written)
##   --exclude=REGEX     leave out archived clean files whose name matches, e.g. a bad
##                       scrape: --exclude='TX_dallas_2026_11_03_20_1[0-5]'. The
##                       jurisdiction falls back to its previous scrape
##   --cbs-lag=5         minutes after T to still accept a CBS snapshot
##   --cbs=live          CBS feed (default MODEL_CBS_ENV, live)
##   --cores=N           fits at once (default half the cores)
##   --label=text        added to the rerun id
##   --publish           publish right after the run (run)
##   --note=text         why (drop)
##   --dir=PATH          model_results folder (default 26_general/model_results; tests)
##
## Staged reruns live in model_results/reruns/<id>/: predictions_latest.csv,
## turnout_latest.csv, status_latest.csv (one row per race x model x time: "ok" or the
## error), compare.csv (old vs new two-party-relevant shares per point), files.csv (the
## clean files each state x time used), call.txt.
# ========================================
suppressPackageStartupMessages({
  source("scripts/model/live/load.R")
  library(glue)
  source("scripts/functions.R") # read_model_results(), its drops
  source("scripts/model_targets.R") # model_state_inputs(), as the pipeline builds them
})

TZ = "America/New_York"

# ---- arguments ---------------------------------------------------------------------------
rr_opt = function(args, k, d = NA) {
  a = grep(paste0("^--", k, "(=|$)"), args, value = TRUE)
  if (!length(a)) return(d)
  if (!grepl("=", a[1])) return("TRUE")
  sub("^[^=]*=", "", a[1])
}
rr_list = function(x) if (is.na(x)) character() else trimws(strsplit(x, ",")[[1]])

# "20:15" -> ELECTION_DATE 20:15 ET (hours before 12 are the next morning);
# "2026-11-03 20:15[:00]" -> as given, ET
rr_time = function(x) {
  x = trimws(x)
  if (grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", x)) {
    h = as.integer(sub(":.*", "", x))
    x = paste(as.Date(ELECTION_DATE) + (h < 12), x)
  }
  t = suppressWarnings(lubridate::ymd_hms(x, tz = TZ, truncated = 1, quiet = TRUE))
  if (is.na(t)) stop("can't read time '", x, "' (use 20:15 or \"2026-11-03 20:15\")")
  t
}
rr_fmt = function(t) format(t, "%m-%d %H:%M:%S", tz = TZ)

# races from --states / --races
rr_races = function(args) {
  pri = races_from_priors()
  st = toupper(rr_list(rr_opt(args, "states")))
  ids = rr_list(rr_opt(args, "races"))
  if (!length(st) && !length(ids)) stop("say which races: --states=TX,GA (or all) and/or --races=TX_sen")
  bad = setdiff(ids, pri$race_id)
  if (length(bad)) stop("not in priors.csv: ", paste(bad, collapse = ", "))
  bad = setdiff(setdiff(st, "ALL"), pri$state)
  if (length(bad)) stop("no priors.csv races in: ", paste(bad, collapse = ", "))
  filter(pri, "ALL" %in% st | state %in% st | race_id %in% ids)
}

# models matching the --models globs among `pool`; full names outside it are allowed when
# they are valid models
rr_models = function(args, pool) {
  pats = rr_list(rr_opt(args, "models"))
  if (!length(pats)) return(pool)
  out = unique(unlist(lapply(pats, \(p) {
    m = grep(utils::glob2rx(p), pool, value = TRUE)
    if (!length(m) && !grepl("[*?]", p)) {
      check_models(p)
      m = p
    }
    if (!length(m)) cli::cli_alert_warning("--models: '{p}' matches nothing")
    m
  })))
  if (!length(out)) stop("no models selected")
  out
}

rr_window = function(args) {
  from = rr_opt(args, "from")
  to = rr_opt(args, "to")
  if (is.na(from) && is.na(to)) return(NULL)
  c(
    if (is.na(from)) as.POSIXct("1970-01-01", tz = TZ) else rr_time(from),
    if (is.na(to)) as.POSIXct("2100-01-01", tz = TZ) else rr_time(to)
  )
}

rr_dir = function(args) path.expand(rr_opt(args, "dir", file.path(find_26_general(), "model_results")))

# the dashboard's points (after drops) for these races x models in the window
rr_history = function(dir, races, window = NULL) {
  h = read_model_results(dir)$pred
  if (is.null(h)) return(NULL)
  h = filter(h, race_id %in% races$race_id)
  if (!is.null(window)) h = filter(h, time >= window[1], time <= window[2])
  h
}

# ---- returns and CBS as of a time ---------------------------------------------------------
# One row per jurisdiction: the clean file to use at `at` (see the header)
clean_files_at = function(state, at, d26 = find_26_general(), exclude = NA) {
  dir = file.path(d26, state, "clean")
  rx_stamp = "_\\d{4}(_\\d{2}){5}\\.parquet$"
  latest = list.files(dir, pattern = paste0("^", state, "(_.*)?_latest\\.parquet$"))
  arch = list.files(dir, pattern = paste0("^", state, ".*", rx_stamp))
  if (!is.na(exclude)) {
    skip = grepl(exclude, arch)
    if (any(skip)) cli::cli_alert_info("{state}: --exclude leaves out {sum(skip)} file{?s}: {head(arch[skip], 5)}")
    arch = arch[!skip]
    latest = latest[!grepl(exclude, latest)]
  }
  a = tibble(
    prefix = sub(rx_stamp, "", arch),
    file = arch,
    stamp = as.POSIXct(stringr::str_extract(arch, "\\d{4}(_\\d{2}){5}(?=\\.parquet$)"), format = "%Y_%m_%d_%H_%M_%S", tz = TZ)
  )
  bind_rows(lapply(sub("_latest\\.parquet$", "", latest), function(p) {
    x = filter(a, prefix == p, stamp <= at)
    if (nrow(x)) return(mutate(slice_max(x, stamp, n = 1, with_ties = FALSE), source = "archive"))
    f = file.path(dir, paste0(p, "_latest.parquet"))
    t = if (file.size(f) > 0) tryCatch(max(read_pq(f)$time, na.rm = TRUE), error = \(e) NA) else NA
    if (!is.na(t) && t <= at) {
      return(tibble(prefix = p, file = basename(f), stamp = lubridate::with_tz(t, TZ), source = "latest (no archive by then)"))
    }
    tibble(prefix = p, file = NA_character_, stamp = as.POSIXct(NA, tz = TZ), source = "none by then")
  })) |>
    mutate(state = state, at = at, path = if_else(is.na(file), NA_character_, file.path(dir, file)))
}

# ---- run -----------------------------------------------------------------------------------
rr_run = function(args) {
  dir = rr_dir(args)
  races = rr_races(args)
  d26 = find_26_general()
  env = rr_opt(args, "cbs", MODEL_CBS_ENV)
  lag = as.numeric(rr_opt(args, "cbs-lag", "5")) * 60
  cores = as.integer(rr_opt(args, "cores", max(1, parallel::detectCores() %/% 2)))
  window = rr_window(args)
  at = rr_list(rr_opt(args, "at"))
  if (is.null(window) == !length(at)) stop("give either --from/--to (redo existing points) or --at=T1,T2")

  # targets: race x model x time
  if (length(at)) {
    tt = do.call(c, lapply(at, rr_time))
    models = rr_models(args, models_from_config())
    want = tidyr::expand_grid(race_id = races$race_id, model = models, at = tt)
  } else {
    h = rr_history(dir, races, window)
    if (is.null(h) || !nrow(h)) stop("no dashboard points for those races between ", rr_fmt(window[1]), " and ", rr_fmt(window[2]))
    pool = unique(h$model)
    ok = vapply(pool, \(m) !inherits(try(check_models(m), silent = TRUE), "try-error"), TRUE)
    if (any(!ok)) cli::cli_alert_info("skipping models that no longer exist: {pool[!ok]}")
    models = rr_models(args, pool[ok])
    want = distinct(filter(h, model %in% models), race_id, model, at = time)
  }
  want = left_join(want, select(races, race_id, state), by = "race_id")
  cli::cli_h1("rerun: {nrow(want)} race x model x time point{?s}")

  # jobs, one returns build per state x time
  jobs = list()
  cbs_st = list()
  files = list()
  inputs = list()
  done = character()
  for (st in sort(unique(want$state))) {
    inputs[[st]] = model_state_inputs(st)
    if (inherits(inputs[[st]]$cells, "error")) {
      cli::cli_alert_danger("{st}: no precinct inputs: {conditionMessage(inputs[[st]]$cells)}")
      next
    }
    geos = unique(inputs[[st]]$cells$geo)
    base = if (inherits(inputs[[st]]$base, "error")) NULL else inputs[[st]]$base
    w_st = filter(want, state == st)
    for (i in seq_along(ats <- sort(unique(w_st$at)))) {
      t = ats[i]
      sel = clean_files_at(st, t, d26, exclude = rr_opt(args, "exclude"))
      files[[length(files) + 1]] = sel
      use = sel$path[!is.na(sel$path)]
      if (!length(use)) {
        cli::cli_alert_warning("{st} {rr_fmt(t)}: no scrape at or before this time")
        next
      }
      ret = tryCatch(returns_from_clean(filter(races, state == st), d26, files = use, geos = geos), error = function(e) {
        cli::cli_alert_danger("{st} {rr_fmt(t)}: {conditionMessage(e)}")
        NULL
      })
      if (is.null(ret)) next
      for (rid in unique(w_st$race_id[w_st$at == t])) {
        r = filter(ret, race_id == rid)
        if (!nrow(r)) {
          cli::cli_alert_warning("{rid} {rr_fmt(t)}: no returns by then")
          next
        }
        race = filter(races, race_id == rid)
        time_r = max(r$time, na.rm = TRUE)
        if (is.null(window) && abs(as.numeric(time_r) - as.numeric(t)) > 1) {
          cli::cli_alert_info("{rid} --at {rr_fmt(t)}: latest scrape is {rr_fmt(time_r)}")
        } else if (!is.null(window) && abs(as.numeric(time_r) - as.numeric(t)) > 1) {
          cli::cli_alert_warning("{rid} {rr_fmt(t)}: rebuilt returns end at {rr_fmt(time_r)}, so this adds a point instead of replacing one (a scrape file missing or excluded?)")
        }
        ms = unique(want$model[want$race_id == rid & want$at == t])
        key = paste(rid, ms, format(time_r))
        ms = ms[!key %in% done] # two --at times on the same scrape
        done = c(done, key[!key %in% done])
        if (!length(ms)) next
        snap = read_cbs_latest(race$state, race$contest, race$district, env = env, at = time_r + lag, dir_26 = d26)
        cbs_note = if (is.null(snap)) "rerun: no saved CBS snapshot" else paste("rerun: CBS snapshot observed", format(lubridate::with_tz(max(snap$observed_at), TZ), "%m-%d %H:%M:%S ET"))
        cbs_st[[length(cbs_st) + 1]] = tibble(race_id = rid, time = time_r, cbs = cbs_note)
        jobs = c(jobs, make_jobs(
          inputs[[st]]$cells, r, races = race, models = ms, time = time_r,
          cbs = if (is.null(base)) NULL else setNames(list(snap), rid),
          base = base
        ))
      }
    }
  }
  if (!length(jobs)) stop("nothing to run")

  cli::cli_alert_info("running {length(jobs)} fit{?s} on {cores} core{?s}")
  t0 = Sys.time()
  res = run_jobs(jobs, cores = cores)
  cli::cli_alert_success("fits done in {round(as.numeric(difftime(Sys.time(), t0, units = 'mins')), 1)} min")
  res$status = left_join(res$status, distinct(bind_rows(cbs_st)), by = c("race_id", "time"))
  for (st in names(inputs)) {
    if (inherits(inputs[[st]]$base, "error")) {
      res$status$statewide[startsWith(res$status$race_id, paste0(st, "_"))] = paste("no county history:", conditionMessage(inputs[[st]]$base))
    }
  }

  # stage
  label = gsub("[^A-Za-z0-9-]+", "-", rr_opt(args, "label", ""))
  id = paste0(format(Sys.time(), "%Y%m%d_%H%M%S", tz = TZ), if (nzchar(label)) paste0("_", label))
  stage = file.path(dir, "reruns", id)
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  write_predictions(res, stage)
  file.remove(list.files(stage, pattern = "_\\d{8}_\\d{6}\\.csv$", full.names = TRUE))
  readr::write_csv(mutate(bind_rows(files), across(c(stamp, at), \(x) format(x, "%Y-%m-%d %H:%M:%S", tz = TZ))), file.path(stage, "files.csv"), na = "")
  writeLines(c(paste("rerun.R", paste(args, collapse = " ")), paste("by", Sys.info()[["user"]], "at", format(Sys.time(), tz = TZ, usetz = TRUE))), file.path(stage, "call.txt"))

  cmp = rr_compare(dir, res)
  readr::write_csv(cmp, file.path(stage, "compare.csv"), na = "")
  bad = filter(res$status, status != "ok")
  if (nrow(bad)) {
    cli::cli_alert_danger("{nrow(bad)} fit{?s} not ok:")
    print(as.data.frame(select(bad, race_id, model, time, status)), right = FALSE)
  }
  geo = rr_opt(args, "geo")
  show = if (is.na(geo)) cmp else filter(cmp, geo == !!geo)
  cli::cli_h2("old vs new (Rep minus Dem share of all votes, points; compare.csv has every row)")
  print(as.data.frame(utils::head(show, 80)), right = FALSE, row.names = FALSE)
  if (nrow(show) > 80) cli::cli_alert_info("... {nrow(show) - 80} more rows in {.path {file.path(stage, 'compare.csv')}}")
  cli::cli_alert_success("staged {id} in {.path {stage}}")

  if (isTRUE(as.logical(rr_opt(args, "publish", "FALSE")))) {
    rr_publish(c("publish", id, grep("^--dir=", args, value = TRUE)))
  } else {
    cli::cli_alert_info("not on the dashboard yet. Publish with: Rscript scripts/model/live/rerun.R publish {id}")
  }
  invisible(id)
}

# old (dashboard now) vs new margin per race x model x geo x time
rr_compare = function(dir, res) {
  marg = function(d) {
    d |>
      filter(pred_type == "share", party %in% c("dem", "rep")) |>
      mutate(time = lubridate::with_tz(time, "UTC")) |>
      summarize(
        margin = 100 * (sum(pred[party == "rep"]) - sum(pred[party == "dem"])),
        pct_rep = first(pct_rep),
        .by = c(race_id, model, geo, time)
      )
  }
  new = marg(mutate(res$pred, time = as.POSIXct(time)))
  old = read_model_results(dir)$pred
  old = if (is.null(old)) new[0, ] else marg(semi_join(old, distinct(new, race_id, model), by = c("race_id", "model")))
  left_join(new, old, by = c("race_id", "model", "geo", "time"), suffix = c("_new", "_old")) |>
    transmute(
      race_id, model, geo,
      time = format(time, "%m-%d %H:%M:%S", tz = TZ),
      point = if_else(is.na(margin_old), "new", "replaces"),
      pct_rep = round(pct_rep_new, 3),
      old = round(margin_old, 2),
      new = round(margin_new, 2),
      change = round(margin_new - margin_old, 2)
    ) |>
    arrange(race_id, model, geo, time)
}

# ---- publish / unpublish ---------------------------------------------------------------------
rr_publish = function(args) {
  dir = rr_dir(args)
  id = args[2]
  if (is.na(id) || startsWith(id, "--")) stop("publish <id> (see: rerun.R list)")
  stage = file.path(dir, "reruns", id)
  if (!dir.exists(stage)) stop("no staged rerun ", id, " in ", file.path(dir, "reruns"))
  for (nm in c("predictions", "turnout")) {
    f = file.path(stage, paste0(nm, "_latest.csv"))
    if (!file.exists(f) || file.size(f) == 0) next
    tmp = file.path(dir, sprintf(".%s_rerun_%s.csv.tmp", nm, id))
    file.copy(f, tmp, overwrite = TRUE)
    file.rename(tmp, file.path(dir, sprintf("%s_rerun_%s.csv", nm, id)))
  }
  cli::cli_alert_success("published {id}: on the dashboard after the next tar_make(). Undo: rerun.R unpublish {id}")
  invisible(id)
}

rr_unpublish = function(args) {
  dir = rr_dir(args)
  id = args[2]
  if (is.na(id) || startsWith(id, "--")) stop("unpublish <id> (see: rerun.R list)")
  f = file.path(dir, sprintf("%s_rerun_%s.csv", c("predictions", "turnout"), id))
  if (!any(file.exists(f))) stop(id, " is not published")
  file.remove(f[file.exists(f)])
  cli::cli_alert_success("unpublished {id} (still staged in reruns/{id}); gone from the dashboard after the next tar_make()")
}

# ---- drops -----------------------------------------------------------------------------------
rr_drop = function(args) {
  dir = rr_dir(args)
  races = rr_races(args)
  window = rr_window(args)
  if (is.null(window)) {
    at = rr_list(rr_opt(args, "at"))
    if (length(at) != 1) stop("drop needs --from/--to or one --at")
    window = rep(rr_time(at), 2)
  }
  models = rr_list(rr_opt(args, "models", "*"))
  h = rr_history(dir, races, window)
  hit = if (is.null(h)) 0 else nrow(distinct(filter(h, Reduce(`|`, lapply(models, \(p) grepl(utils::glob2rx(p), model)))), race_id, model, time))
  new = tidyr::expand_grid(race_id = races$race_id, model = models) |>
    mutate(
      from = format(window[1], "%Y-%m-%dT%H:%M:%S%z"),
      to = format(window[2], "%Y-%m-%dT%H:%M:%S%z"),
      note = rr_opt(args, "note", ""),
      added = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = TZ),
      by = Sys.info()[["user"]]
    )
  f = file.path(dir, "drops.csv")
  old = if (file.exists(f)) readr::read_csv(f, col_types = readr::cols(.default = "c")) else NULL
  tmp = file.path(dir, ".drops.csv.tmp")
  readr::write_csv(bind_rows(old, new), tmp, na = "")
  file.rename(tmp, f)
  cli::cli_alert_success("dropped {hit} race x model x time point{?s} ({nrow(new)} row{?s} in {.path {f}}); off the dashboard after the next tar_make(). Undo: rerun.R list, then undrop <row>")
}

rr_undrop = function(args) {
  dir = rr_dir(args)
  f = file.path(dir, "drops.csv")
  rows = as.integer(rr_list(args[2]))
  if (!file.exists(f) || anyNA(rows)) stop("undrop <row>[,<row>] (row numbers from: rerun.R list)")
  d = readr::read_csv(f, col_types = readr::cols(.default = "c"))
  if (any(rows > nrow(d))) stop("drops.csv has ", nrow(d), " rows")
  tmp = file.path(dir, ".drops.csv.tmp")
  readr::write_csv(d[-rows, ], tmp, na = "")
  file.rename(tmp, f)
  cli::cli_alert_success("removed drop row{?s} {rows}")
}

# ---- list / points -----------------------------------------------------------------------------
rr_ls = function(args) {
  dir = rr_dir(args)
  ids = sort(basename(list.dirs(file.path(dir, "reruns"), recursive = FALSE)))
  cli::cli_h2("reruns ({.path {file.path(dir, 'reruns')}})")
  if (!length(ids)) cli::cli_text("none")
  for (id in ids) {
    s = file.path(dir, "reruns", id)
    st = if (file.exists(file.path(s, "status_latest.csv"))) readr::read_csv(file.path(s, "status_latest.csv"), col_types = readr::cols(.default = "c")) else tibble()
    pub = file.exists(file.path(dir, sprintf("predictions_rerun_%s.csv", id)))
    call = if (file.exists(file.path(s, "call.txt"))) readLines(file.path(s, "call.txt"))[1] else ""
    cli::cli_text("{if (pub) '[published]' else '[staged]   '} {id}: {nrow(st)} fit{?s}, {sum(st$status != 'ok')} not ok -- {call}")
  }
  f = file.path(dir, "drops.csv")
  cli::cli_h2("drops ({.path {f}})")
  if (file.exists(f)) print(as.data.frame(readr::read_csv(f, col_types = readr::cols(.default = "c"))), right = FALSE) else cli::cli_text("none")
}

rr_points = function(args) {
  dir = rr_dir(args)
  races = rr_races(args)
  h = rr_history(dir, races, rr_window(args))
  if (is.null(h) || !nrow(h)) return(cli::cli_alert_info("no points"))
  pats = rr_list(rr_opt(args, "models"))
  if (length(pats)) h = filter(h, Reduce(`|`, lapply(pats, \(p) grepl(utils::glob2rx(p), model))))
  geo = rr_opt(args, "geo", "all")
  h |>
    filter(geo == !!geo, pred_type == "share", party %in% c("dem", "rep")) |>
    summarize(margin = round(100 * (sum(pred[party == "rep"]) - sum(pred[party == "dem"])), 2), pct_rep = round(first(pct_rep), 3), .by = c(race_id, model, time)) |>
    arrange(race_id, model, time) |>
    mutate(time = format(time, "%m-%d %H:%M:%S", tz = TZ)) |>
    as.data.frame() |>
    print(right = FALSE, row.names = FALSE)
  cli::cli_alert_info("geo = {geo}; margin = Rep minus Dem share of all votes, points")
}

# ---- entry --------------------------------------------------------------------------------------
rerun = function(...) {
  args = c(...)
  cmd = args[1]
  f = switch(
    if (is.na(cmd)) "" else cmd,
    run = rr_run, points = rr_points, publish = rr_publish, unpublish = rr_unpublish,
    drop = rr_drop, undrop = rr_undrop, list = rr_ls,
    stop("usage: rerun.R run|points|publish|unpublish|drop|undrop|list [options]; see the top of scripts/model/live/rerun.R")
  )
  f(args)
}

if (sys.nframe() == 0L) {
  tryCatch(rerun(commandArgs(trailingOnly = TRUE)), error = function(e) {
    cli::cli_alert_danger(conditionMessage(e))
    quit(status = 1)
  })
}
