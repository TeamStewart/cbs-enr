# ========================================
## Watcher L1: deterministic checks on one pipeline cycle (B2-B11 in docs/watcher_tasks.md).
##   res = run_checks()   list(findings = list of finding(), metrics = list(...))
## `metrics` is saved by the caller as state/metrics_prev.json and passed back as `prev`
## next cycle, so each check can diff against the last one. Every check is its own
## check_<name>(ctx) returning list(findings, metrics); an error in one becomes a P2
## finding `check_error:<name>` and the rest still run.
## Needs lib.R sourced first. Read-only: Dropbox, the pushed meta and GitHub are only read.
# ========================================

suppressPackageStartupMessages({
  library(data.table)
})

# The model code (races_from_priors, build_cells, returns_from_clean, returns_dropped,
# bucket_class) and globals.R (DASHBOARD_CONTESTS, PATH_DROPBOX). load.R expects the repo
# root as the working directory.
local({
  wd = setwd(WATCH_REPO)
  on.exit(setwd(wd))
  source("scripts/model/live/load.R")
})

TRANSIENT_RX = "HTTP (error )?(405|429|5[0-9]{2})|[Tt]imeout|timed out|Could not resolve host|[Cc]onnection reset|Connection refused"
FROZEN_MIN = 30      # minutes a timestamp may sit unchanged while its state moves
CBS_STALE_MIN = 15
DASH_STALE_MIN = 15
REDIST_STALE_MIN = 30
CELLS_MAX_AGE_H = 12
MATCH_SLACK = 0.05   # unknown_geo share allowed above (1 - baseline match rate)
SHARE_JUMP = 0.05    # 5pp cycle-over-cycle rise in either dropped share
DASH_URL = "https://dqvla57lr7v2z.cloudfront.net/20261103-US-G/mit.html"

# ---- small helpers ----
# state from a target or file name: data_FL_pasco, tbl_cbs_CA_butte, model_fits_TX_ab12, FL_Alachua
name_state = function(x) stringr::str_match(x, "(?:^|_)([A-Z]{2})(?:_|$)")[, 2]
# P1 once the state's last polls have closed, P2 before. Unknown state counts as closed.
sev_close = function(state, now) if (isTRUE(polls_closed(state %||% NA_character_, now))) "P1" else "P2"
mins_since = function(t, now) as.numeric(difftime(now, t, units = "mins"))
pmeta = function(...) watch_path("pipeline-state", ...)
first_line = function(x, n = 300) substr(sub("\n.*", "", x), 1, n)

# Pushed targets meta and progress. tar_meta()/tar_progress() work on a store holding only
# meta/progress/process; fall back to reading the pipe-delimited files if they do not.
read_meta = function(ps_dir) {
  store = file.path(ps_dir, "_targets")
  tryCatch(
    as.data.table(targets::tar_meta(store = store, fields = c(name, type, data, time, error, warnings))),
    error = function(e) fread(file.path(store, "meta", "meta"), sep = "|", fill = TRUE,
                              select = c("name", "type", "data", "time", "error", "warnings"))
  )
}
read_progress = function(ps_dir) {
  store = file.path(ps_dir, "_targets")
  tryCatch(
    as.data.table(targets::tar_progress(store = store)),
    error = function(e) {
      p = fread(file.path(store, "meta", "progress"), sep = "|", fill = TRUE)
      # one row per event; the last one per target is its state this run
      unique(p[, .(name, progress)], by = "name", fromLast = TRUE)
    }
  )
}

# ---- B2: cycle steps and disk ----
check_cycle_steps = function(ctx) {
  cyc = ctx$cycle
  out = list()
  for (s in c("enightdata", "tar_make", "dashboard_push", "dashboard_s3", "sheet")) {
    v = cyc[[s]]
    if (!is.null(v) && identical(as.logical(v), FALSE)) {
      out[[length(out) + 1]] = finding(
        "cycle_step", s, paste0("Cycle ", cyc$run_time, ": step ", s, " failed (see that cycle's log/console.log)."),
        severity = if (s == "tar_make") "P0" else "P1", evidence = list(run_time = cyc$run_time)
      )
    }
  }
  disk = suppressWarnings(as.numeric(cyc$disk_free_gb %||% NA))
  if (!is.na(disk) && disk < 10) {
    out[[length(out) + 1]] = finding("cycle_step", "disk", sprintf("Scraper laptop has %.1f GB free.", disk),
                                     severity = "P1", value = disk)
  }
  list(findings = out, metrics = list(run_time = cyc$run_time, disk_free_gb = disk))
}

# ---- B3: target errors and warnings ----
check_target_errors = function(ctx) {
  m = ctx$meta(); p = ctx$progress()
  errored = p$name[p$progress == "errored"]
  msg = setNames(m$error[match(errored, m$name)], errored)
  # knock-ons: a failed data_* hands NULL to its tbl_cbs_*/summary_*, and a failed
  # timestamp_* usually fails data_* with the same message; report the root only
  root = sub("^(tbl_cbs|summary)_", "data_", errored)
  knock = grepl("^(tbl_cbs|summary)_", errored) & root %in% errored
  ts = sub("^data_", "timestamp_", errored)
  knock = knock | (grepl("^data_", errored) & ts %in% errored & msg[errored] %in% msg[ts])
  prev_counts = ctx$prev$target_errors$counts %||% list()
  counts = list()
  out = list()
  for (i in seq_along(errored)) {
    nm = errored[i]
    counts[[nm]] = (prev_counts[[nm]] %||% 0) + 1
    if (knock[i]) next
    st = name_state(nm)
    e = msg[[nm]] %||% "(no message in meta)"
    out[[length(out) + 1]] = finding(
      "target_error", nm, paste0(nm, " errored: ", first_line(e)),
      severity = sev_close(st, ctx$now), state = st, value = counts[[nm]],
      evidence = list(error = substr(e, 1, 2000), consecutive = counts[[nm]],
                      knock_on = errored[knock & (root == nm | ts == nm)]),
      transient = grepl(TRANSIENT_RX, e)
    )
  }
  # warnings of targets in this run (meta keeps a target's warnings from its last build)
  w = m[!is.na(warnings) & nzchar(warnings) & name %in% p$name]
  for (i in seq_len(nrow(w))) {
    st = name_state(w$name[i])
    out[[length(out) + 1]] = finding("target_warning", w$name[i], paste0(w$name[i], " warned: ", first_line(w$warnings[i])),
                                     severity = "P2", state = st, evidence = list(warnings = substr(w$warnings[i], 1, 2000)))
  }
  st_all = name_state(p$name)
  by_state = lapply(split(p$progress, st_all), \(x) list(n = length(x), errored = sum(x == "errored")))
  list(findings = out, metrics = list(
    counts = counts, n_targets = nrow(p), n_targets_errored = length(errored),
    n_ok = sum(p$progress != "errored"), n_warnings = nrow(w), by_state = by_state
  ))
}

# ---- B4: enightdata change with an error jump ----
check_enightdata = function(ctx) {
  f = pmeta("enightdata_sha")
  sha = if (file.exists(f)) readLines(f, warn = FALSE)[1] else NA_character_
  n = sum(ctx$progress()$progress == "errored")
  old = ctx$prev$enightdata$sha %||% NA_character_
  n_old = ctx$prev$enightdata$n_errored %||% NA_real_
  out = list()
  if (!is.na(sha) && !is.na(old) && sha != old && !is.na(n_old) && n - n_old >= max(5, 0.5 * n_old)) {
    out[[1]] = finding(
      "enightdata_jump", substr(sha, 1, 12),
      sprintf("Errored targets rose %d -> %d after enightdata moved %s -> %s. Consider reverting on GitHub.",
              as.integer(n_old), n, substr(old, 1, 7), substr(sha, 1, 7)),
      severity = "P0", value = n, evidence = list(old_sha = old, new_sha = sha, before = n_old, after = n)
    )
  }
  # keep the pre-change baseline while the same sha stays installed and nothing changed
  list(findings = out, metrics = list(sha = sha, n_errored = n))
}

# ---- B5: frozen timestamps ----
check_frozen_timestamps = function(ctx) {
  m = ctx$meta(); p = ctx$progress()
  ts = m[grepl("^timestamp_", name) & name %in% p$name, .(name, data)]
  bad = p$name[p$progress == "errored"]
  now = ctx$cycle_time
  prev = ctx$prev$frozen_timestamps$ts %||% list()
  cur = list()
  for (i in seq_len(nrow(ts))) {
    nm = ts$name[i]; pv = prev[[nm]]
    if (is.null(pv) || !identical(pv$hash, ts$data[i])) {
      # first sight is not a change: last_change stays NA until the hash actually moves
      cur[[nm]] = list(hash = ts$data[i], since = fmt_time(now),
                       last_change = if (is.null(pv)) NA_character_ else fmt_time(now))
    } else {
      cur[[nm]] = pv
    }
  }
  out = list()
  if (length(cur)) {
    d = data.table(name = names(cur),
                   since = do.call(c, lapply(cur, \(x) parse_time(x$since))),
                   last_change = do.call(c, lapply(cur, \(x) parse_time(x$last_change %||% NA_character_))))
    d[, state := name_state(name)]
    d[, moved := !is.na(last_change) & mins_since(last_change, now) < FROZEN_MIN]
    d[, frozen := mins_since(since, now) >= FROZEN_MIN & !name %in% bad]
    for (st in unique(d$state[!is.na(d$state)])) {
      if (!isTRUE(polls_closed(st, now))) next
      ds = d[state == st]
      for (i in which(ds$frozen)) {
        others = ds[-i][moved == TRUE]
        if (!nrow(others)) next
        out[[length(out) + 1]] = finding(
          "frozen_timestamp", ds$name[i],
          sprintf("%s unchanged for %d min while %d other %s jurisdiction(s) moved.",
                  ds$name[i], round(mins_since(ds$since[i], now)), nrow(others), st),
          severity = "P1", state = st, value = round(mins_since(ds$since[i], now)),
          evidence = list(unchanged_since = fmt_time(ds$since[i]), moved = head(others$name, 10))
        )
      }
    }
  }
  list(findings = out, metrics = list(ts = cur))
}

# ---- B6: scraper output in the clean files ----
check_scraper_output = function(ctx) {
  files = Sys.glob(file.path(ctx$d26, "*", "clean", "*_latest.parquet"))
  files = files[grepl("^[A-Z]{2}$", basename(dirname(dirname(files))))]
  prev = ctx$prev$scraper_output$files %||% list()
  cur = list()
  out = list()
  n_read = 0
  for (f in files) {
    key = sub("_latest\\.parquet$", "", basename(f))
    st = basename(dirname(dirname(f)))
    info = file.info(f)
    # whole seconds, so the value survives the JSON round trip exactly
    mt = round(as.numeric(info$mtime)); sz = as.numeric(info$size)
    pv = prev[[key]]
    if (sz == 0) {
      out[[length(out) + 1]] = finding("scraper_output", paste0("empty_file:", key),
                                       paste0(basename(f), " is 0 bytes (online-only in Dropbox): make the folder available offline."),
                                       severity = "P1", state = st, evidence = list(path = f))
      cur[[key]] = list(state = st, mtime = mt, size = 0, rows = pv$rows, votes = pv$votes, cands = pv$cands)
      next
    }
    if (!is.null(pv) && isTRUE(pv$mtime == mt) && isTRUE(pv$size == sz)) {
      cur[[key]] = pv
      next
    }
    x = nanoparquet::read_parquet(f, col_select = c("contest", "candidate", "votes"))
    n_read = n_read + 1
    cands = rlang::hash(sort(unique(paste(x$contest, x$candidate, sep = "|"))))
    now_m = list(state = st, mtime = mt, size = sz, rows = nrow(x), votes = sum(x$votes, na.rm = TRUE), cands = cands)
    cur[[key]] = now_m
    if (is.null(pv) || is.null(pv$rows)) next
    sev = sev_close(st, ctx$now)
    if (pv$rows > 0 && now_m$rows == 0) {
      out[[length(out) + 1]] = finding("scraper_output", paste0("rows_zero:", key),
                                       sprintf("%s dropped from %d rows to 0.", basename(f), pv$rows),
                                       severity = sev, state = st, value = 0)
    }
    if (pv$votes > 0 && now_m$votes < pv$votes * (1 - 0.005)) {
      out[[length(out) + 1]] = finding("scraper_output", paste0("votes_down:", key),
                                       sprintf("%s total votes fell %s -> %s (%.1f%%).", basename(f),
                                               format(pv$votes, big.mark = ","), format(now_m$votes, big.mark = ","),
                                               100 * (now_m$votes / pv$votes - 1)),
                                       severity = sev, state = st, value = now_m$votes,
                                       evidence = list(before = pv$votes, after = now_m$votes))
    }
    if (!identical(pv$cands, cands) && now_m$rows > 0) {
      out[[length(out) + 1]] = finding("scraper_output", paste0("candidates:", key),
                                       paste0(basename(f), ": the set of contest x candidate changed."),
                                       severity = sev, state = st)
    }
  }
  by_state = lapply(split(cur, vapply(cur, \(x) x$state, "")), \(z) list(
    n_files = length(z), rows = sum(vapply(z, \(x) as.numeric(x$rows %||% 0), 0)),
    votes = sum(vapply(z, \(x) as.numeric(x$votes %||% 0), 0))
  ))
  list(findings = out, metrics = list(files = cur, n_files = length(files), n_read = n_read, by_state = by_state))
}

# ---- B7: methods and crosswalk match ----
# Cells are static for the night: built once per state and cached for CELLS_MAX_AGE_H.
cached_cells = function(state, d26) {
  f = state_path(paste0("cells_", state, ".rds"))
  if (file.exists(f) && difftime(Sys.time(), file.mtime(f), units = "hours") < CELLS_MAX_AGE_H) return(readRDS(f))
  cells = suppressMessages(build_cells(state, d26))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(cells, f)
  cells
}

# pct_ours (matched / our_n) summed to the state. It measures our precinct ids against
# votehub geometry, the closest pre-election number to "how many ids reconcile".
match_baseline = function() {
  f = file.path(WATCH_REPO, "scripts", "model", "lookups", "precinct_geo_match_rates.csv")
  if (!file.exists(f)) return(numeric())
  d = fread(f)
  d = d[, .(rate = sum(matched) / sum(our_n)), by = state]
  setNames(d$rate, d$state)
}

check_methods_match = function(ctx) {
  races = races_from_priors(ctx$d26)
  base = match_baseline()
  prev = ctx$prev$methods_match$races %||% list()
  cur = list()
  out = list()
  timing = list()
  for (st in sort(unique(races$state))) {
    t0 = Sys.time()
    r = tryCatch({
      cells = cached_cells(st, ctx$d26)
      list(cells = cells, ret = suppressMessages(returns_from_clean(races[races$state == st, ], ctx$d26, geos = unique(cells$geo))))
    }, error = function(e) e)
    if (inherits(r, "error")) {
      # no clean files yet is B8's business; anything else is worth a line in the digest
      if (!grepl("^No \\*_latest", conditionMessage(r))) {
        out[[length(out) + 1]] = finding("methods_match", paste0("error:", st), paste0(st, ": ", first_line(conditionMessage(r))),
                                         severity = "P2", state = st)
      }
      next
    }
    known = unique(as.character(r$cells$geo))
    sev = sev_close(st, ctx$now)
    for (rid in sort(unique(r$ret$race_id))) {
      rr = r$ret[r$ret$race_id == rid, ]
      # bucket rows (off-spine geos whose label is a county/site pile) are expected and
      # handled by bucket_counts(), so they are out of both numerator and denominator
      is_bucket = !as.character(rr$geo) %in% known & !is.na(bucket_class(geo_label(rr$geo)))
      rr = rr[!is_bucket, ]
      d = returns_dropped(r$cells, rr)
      tot = attr(d, "total_votes")
      # before any votes, shares are by rows so the zero file still says something
      basis = if (tot > 0) "votes" else "rows"
      share = function(k) {
        if (!nrow(rr)) return(NA_real_)
        if (basis == "votes") sum(d$votes[d$drop == k]) / tot else sum(d$n_rows[d$drop == k]) / nrow(rr)
      }
      sg = share("unknown_geo"); sm = share("unknown_method")
      meths = sort(unique(d$method[d$drop == "unknown_method"]))
      g = attr(d, "geos")
      ev = list(
        basis = basis, unknown_geo_share = sg, unknown_method_share = sm, total_votes = tot,
        n_bucket_rows = sum(is_bucket),
        # plain data frames: subsetting the tibbles would carry returns_dropped()'s attributes
        top_geos = head(as.data.frame(g)[g$drop == "unknown_geo", c("method", "geo", "votes")], 5),
        top_methods = head(as.data.frame(d)[d$drop == "unknown_method", c("method", "n_geo", "votes")], 5)
      )
      cur[[rid]] = list(state = st, basis = basis, unknown_geo = sg, unknown_method = sm, methods = as.list(meths))
      pv = prev[[rid]]
      why = character()
      if (!is.na(base[st]) && !is.na(sg) && sg > (1 - base[st]) + MATCH_SLACK) {
        why = c(why, sprintf("unknown_geo %.1f%% vs baseline %.1f%%", 100 * sg, 100 * (1 - base[st])))
      }
      same_basis = !is.null(pv) && identical(pv$basis, basis)
      if (same_basis && !is.na(sg) && !is.null(pv$unknown_geo) && sg - pv$unknown_geo > SHARE_JUMP) {
        why = c(why, sprintf("unknown_geo up %.1fpp", 100 * (sg - pv$unknown_geo)))
      }
      if (same_basis && !is.na(sm) && !is.null(pv$unknown_method) && sm - pv$unknown_method > SHARE_JUMP) {
        why = c(why, sprintf("unknown_method up %.1fpp", 100 * (sm - pv$unknown_method)))
      }
      new_m = if (is.null(pv)) character() else setdiff(meths, unlist(pv$methods))
      if (length(new_m)) why = c(why, paste0("new unmodelled method(s): ", paste(new_m, collapse = ", ")))
      if (length(why)) {
        out[[length(out) + 1]] = finding("methods_match", rid,
                                         paste0(rid, ": ", paste(why, collapse = "; "), " (by ", basis, ")."),
                                         severity = sev, state = st, value = sg, evidence = ev)
      }
    }
    timing[[st]] = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  }
  list(findings = out, metrics = list(races = cur, seconds_by_state = timing))
}

# ---- B8: models ----
check_models = function(ctx) {
  f = file.path(ctx$d26, "model_results", "status_latest.csv")
  races = races_from_priors(ctx$d26)
  out = list()
  s = if (file.exists(f)) fread(f, colClasses = "character", na.strings = NULL) else data.table(race_id = character())
  for (i in seq_len(nrow(s))) {
    st = substr(s$race_id[i], 1, 2)
    sev = sev_close(st, ctx$now)
    k = paste0(s$race_id[i], ":", s$model[i])
    if (!grepl("^ok", s$status[i])) {
      out[[length(out) + 1]] = finding("model_status", k, sprintf("%s %s: %s", s$race_id[i], s$model[i], first_line(s$status[i])),
                                       severity = sev, state = st, evidence = list(time = s$time[i]))
    } else if (!identical(s$statewide[i], "ok")) {
      out[[length(out) + 1]] = finding("model_statewide", k, sprintf("%s %s statewide: %s", s$race_id[i], s$model[i],
                                                                     first_line(s$statewide[i] %||% "(blank)")),
                                       severity = sev, state = st, evidence = list(time = s$time[i], cbs = s$cbs[i]))
    }
  }
  # a state that fails writes no status rows at all; its race only "vanishes" if the state
  # has votes in its clean files
  by_state = ctx$cur$scraper_output$by_state %||% list()
  have_votes = names(by_state)[vapply(by_state, \(x) x$votes > 0, TRUE)]
  gone = setdiff(races$race_id[races$state %in% have_votes], unique(s$race_id))
  for (rid in gone) {
    st = substr(rid, 1, 2)
    out[[length(out) + 1]] = finding("model_vanished", rid, paste0(rid, " has votes in its clean files but no row in status_latest.csv."),
                                     severity = "P1", state = st)
  }
  list(findings = out, metrics = list(
    n_rows = nrow(s), n_ok = sum(grepl("^ok", s$status)),
    status_mtime = if (file.exists(f)) fmt_time(file.mtime(f)) else NA_character_
  ))
}

# ---- B9: CBS feed ----
check_cbs_feed = function(ctx) {
  dir = file.path(ctx$d26, "cbs_api", "live")
  fs = list.files(dir, recursive = TRUE, full.names = TRUE)
  newest = if (length(fs)) max(file.mtime(fs)) else as.POSIXct(NA)
  start = as.POSIXct(paste(Sys.getenv("CBS_ELECTION_DATE", "2026-11-03"), "19:00"), tz = WATCH_TZ)
  age = if (is.na(newest)) Inf else mins_since(newest, ctx$now)
  out = list()
  if (ctx$now >= start && age > CBS_STALE_MIN) {
    out[[1]] = finding("cbs_feed", "stale",
                       if (is.na(newest)) "No CBS snapshot in cbs_api/live/ yet." else sprintf("Newest CBS snapshot is %d min old.", round(age)),
                       severity = "P1", value = if (is.finite(age)) round(age) else NULL)
  }
  list(findings = out, metrics = list(newest = if (is.na(newest)) NA_character_ else fmt_time(newest), n_files = length(fs)))
}

# ---- B10: dashboard ----
check_dashboard = function(ctx) {
  out = list()
  f = watch_path("dashboard-live", "dashboard_data.rds")
  dd = tryCatch(readRDS(f), error = function(e) NULL)
  keys = with(DASHBOARD_CONTESTS, ifelse(is.na(district), paste0(state, "-", contest), paste0(state, "-", contest, "-", district)))
  missing = setdiff(keys, if (!is.null(dd$meta)) dd$meta$key else character())
  if (is.null(dd) || !NROW(dd$meta) || length(missing)) {
    out[[length(out) + 1]] = finding(
      "dashboard", "data",
      if (is.null(dd)) "dashboard_data.rds on dashboard-live is missing, unreadable or NULL."
      else if (!NROW(dd$meta)) "dashboard_data.rds has no meta rows."
      else paste0("dashboard_data.rds is missing contest(s): ", paste(missing, collapse = ", ")),
      severity = "P0", evidence = list(missing = missing)
    )
  }

  gh = suppressWarnings(system2("gh", c("run", "list", "-R", "TeamStewart/cbs-enr", "--workflow", "pages.yml", "-L", "1",
                                        "--json", "conclusion,status,createdAt"), stdout = TRUE, stderr = TRUE))
  run = NULL
  if (!is.null(attr(gh, "status")) && attr(gh, "status") != 0) {
    out[[length(out) + 1]] = finding("dashboard", "gh_cli", paste0("gh run list failed: ", first_line(paste(gh, collapse = " "))),
                                     severity = "P2", transient = TRUE)
  } else {
    run = tryCatch(jsonlite::fromJSON(paste(gh, collapse = "")), error = function(e) NULL)
    if (NROW(run) && identical(run$conclusion[1], "failure")) {
      out[[length(out) + 1]] = finding("dashboard", "pages_run", paste0("Last pages.yml run (", run$createdAt[1], ") failed."),
                                       severity = "P1", evidence = as.list(run[1, ]))
    }
  }

  # CloudFront copy: only meaningful while cycles are arriving. The distribution sits behind
  # Basic auth, so CBS_WATCH_DASH_AUTH="user:password" is needed; it goes to curl on stdin
  # (-K -) so it never shows in the process list.
  lm = NA
  auth = Sys.getenv("CBS_WATCH_DASH_AUTH")
  hdr = suppressWarnings(system2(
    "curl", c("-sI", "-m", "10", if (nzchar(auth)) c("-K", "-"), DASH_URL),
    input = if (nzchar(auth)) sprintf('user = "%s"', auth), stdout = TRUE, stderr = TRUE
  ))
  ok = (is.null(attr(hdr, "status")) || attr(hdr, "status") == 0) && length(hdr) && grepl(" 200", hdr[1])
  line = grep("^last-modified:", hdr, ignore.case = TRUE, value = TRUE)
  if (ok && length(line)) {
    lm = as.POSIXct(trimws(sub("^[^:]+:", "", line[1])), format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  }
  fresh = !is.na(ctx$cycle_time) && mins_since(ctx$cycle_time, ctx$now) <= DASH_STALE_MIN
  if (!ok || !length(line)) {
    out[[length(out) + 1]] = finding("dashboard", "cloudfront_head",
                                     paste0("Could not read Last-Modified of mit.html on CloudFront (",
                                            if (length(hdr)) trimws(hdr[1]) else "no response",
                                            if (!nzchar(auth)) "; CBS_WATCH_DASH_AUTH unset" else "", ")."),
                                     severity = "P2", transient = TRUE)
  } else if (fresh && mins_since(lm, ctx$now) > DASH_STALE_MIN) {
    out[[length(out) + 1]] = finding("dashboard", "cloudfront_stale",
                                     sprintf("CloudFront mit.html last modified %d min ago while cycles are fresh.", round(mins_since(lm, ctx$now))),
                                     severity = "P1", value = round(mins_since(lm, ctx$now)))
  }
  list(findings = out, metrics = list(
    n_meta = NROW(dd$meta), pages_run = if (NROW(run)) as.list(run[1, ]) else NULL,
    cloudfront_last_modified = if (is.na(lm)) NA_character_ else fmt_time(lm)
  ))
}

# ---- B11: redistricting ----
check_redistricting = function(ctx) {
  log = file.path(ctx$d26, "model_results", "redistricting", "live.log")
  st = file.path(ctx$d26, "model_results", "status_latest.csv")
  out = list()
  if (file.exists(log) && file.exists(st)) {
    lt = file.mtime(log); s = file.mtime(st)
    if (mins_since(lt, ctx$now) > REDIST_STALE_MIN && s > lt) {
      out[[1]] = finding("redistricting", "stale",
                         sprintf("redistricting live.log last written %d min ago, before the latest model run.", round(mins_since(lt, ctx$now))),
                         severity = "P2", value = round(mins_since(lt, ctx$now)))
    }
  }
  list(findings = out, metrics = list(log_mtime = if (file.exists(log)) fmt_time(file.mtime(log)) else NA_character_))
}

# ---- driver ----
# Order matters only for check_models, which reads check_scraper_output's metrics.
CHECKS = c("cycle_steps", "target_errors", "enightdata", "frozen_timestamps", "scraper_output",
           "methods_match", "models", "cbs_feed", "dashboard", "redistricting")

# the finding `check` names each check emits: a check that ran resolves its incidents that it
# no longer finds, and one that failed (check_error) resolves nothing
CHECK_EMITS = list(
  cycle_steps = "cycle_step", target_errors = c("target_error", "target_warning"),
  enightdata = "enightdata_jump", frozen_timestamps = "frozen_timestamp", scraper_output = "scraper_output",
  methods_match = "methods_match", models = c("model_status", "model_statewide", "model_vanished"),
  cbs_feed = "cbs_feed", dashboard = "dashboard", redistricting = "redistricting"
)

run_checks = function(ps_dir = watch_path("pipeline-state"), prev = read_json_file(state_path("metrics_prev.json")),
                      now = now_et(), checks = CHECKS) {
  prev = prev %||% list()
  ctx = new.env()
  ctx$ps_dir = ps_dir
  ctx$prev = prev
  ctx$now = now
  ctx$cycle = read_json_file(file.path(ps_dir, "cycle.json"), list())
  ctx$cycle_time = if (!is.null(ctx$cycle$run_time)) parse_run_time(ctx$cycle$run_time) else as.POSIXct(NA)
  if (is.na(ctx$cycle_time)) ctx$cycle_time = now
  ctx$d26 = find_26_general()
  ctx$cur = list()
  # meta and progress read once, on first use
  ctx$meta = local({ x = NULL; function() { if (is.null(x)) x <<- read_meta(ps_dir); x } })
  ctx$progress = local({ x = NULL; function() { if (is.null(x)) x <<- read_progress(ps_dir); x } })

  findings = list()
  seconds = list()
  checks_run = character()
  for (nm in checks) {
    t0 = Sys.time()
    ran = TRUE
    r = tryCatch(get(paste0("check_", nm))(ctx), error = function(e) {
      ran <<- FALSE
      list(findings = list(finding("check_error", nm, paste0("check ", nm, " failed: ", first_line(conditionMessage(e))),
                                   severity = "P2", evidence = list(error = conditionMessage(e)))),
           # keep last cycle's metrics so the next diff is still against real data
           metrics = prev[[nm]])
    })
    seconds[[nm]] = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    findings = c(findings, r$findings)
    ctx$cur[[nm]] = r$metrics
    if (ran) checks_run = c(checks_run, CHECK_EMITS[[nm]])
  }
  te = ctx$cur$target_errors
  metrics = c(ctx$cur, list(
    at = fmt_time(now), run_time = ctx$cycle$run_time,
    summary = list(
      n_targets_errored = te$n_targets_errored, n_ok = te$n_ok, n_findings = length(findings),
      by_severity = as.list(table(factor(vapply(findings, \(f) f$severity, ""), c("P0", "P1", "P2")))),
      by_state = local({
        sts = sort(unique(c(names(te$by_state), names(ctx$cur$scraper_output$by_state))))
        sts = sts[nzchar(sts) & !is.na(sts)]
        setNames(lapply(sts, \(s) list(
          targets = te$by_state[[s]]$n, errored = te$by_state[[s]]$errored,
          files = ctx$cur$scraper_output$by_state[[s]]$n_files, votes = ctx$cur$scraper_output$by_state[[s]]$votes,
          findings = sum(vapply(findings, \(f) identical(f$state, s), TRUE))
        )), sts)
      })
    ),
    seconds = seconds
  ))
  list(findings = findings, metrics = metrics, checks_run = checks_run)
}
