# ========================================
## run_night.R -- the election-night run: every race, every model, scraped counties AND
## statewide, written for the dashboard. One refresh, or one every N minutes.
##
## Each refresh:
##   1. reads the latest scraped returns (CONTRACT.md section 8 shape) and keeps the races
##      in 26_general/input_data/priors.csv
##   2. for each state: its precinct cells (build_cells(), from model_inputs/), its county
##      history (county_baseline()), and FRESH CBS county results for its races
##      (cbs_for_races(): pulls each race, falls back to the last saved snapshot)
##   3. runs every wired model on every race (predict_all()): rows with geo = "scraped"
##      (the models on our precincts) and geo = "all" (statewide: scraped counties + CBS
##      counties)
##   4. writes 26_general/model_results/predictions_<time>.csv + predictions_latest.csv and
##      status_<time>.csv + status_latest.csv (write_predictions(); format in R/output.R)
## A race or model that fails is recorded in the status table; the rest carry on.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/run_night.R                 # one refresh, from the scrapers'
##                                                         # 26_general/{ST}/clean/*_latest.parquet
##   Rscript scripts/model/live/run_night.R --every=5       # every 5 minutes until stopped
##   Rscript scripts/model/live/run_night.R --returns=<file> --candidates=<file>   # one refresh
##   Rscript scripts/model/live/run_night.R --returns=<file> --candidates=<file> --every=5
##                                                         # every 5 minutes until stopped
##   --returns     the scraped returns, CONTRACT.md section 8 (geo, race_id, candidate_id,
##                 method, votes, stage, observed_at); parquet or csv
##   --candidates  CONTRACT.md section 5 (race_id, candidate_id, name, party)
##   --cbs=test    CBS's test feed instead of live (default live)
##   --config=<file>  the model config (default 26_general/model_config.csv, R/config.R): its
##                 run = TRUE rows are the models run. RUN in R/run.R if the file is missing
##   --models=swing,pois_bayes   (overrides the config; any of MODELS); a model may carry a
##                 demographic spec (R/demos.R) and an interval (R/results.R):
##                 "<model>[/<demo>][/<interval>]", e.g. pois_bayes/strat_d/conformal
##   --states=NC,TX  only these states' races (default: every race in priors.csv)
##   --cores=N     how many race x model fits run at once (default: all cores but one).
##                 Every race x model in every state goes into one pool, so a refresh takes
##                 about as long as the slowest single fit when there are enough cores.
##                 --cores=1 runs them one at a time. macOS / Linux only; Windows runs 1.
##
## Test the whole path on one of Ethan's simulated nights, with the simulated CBS feed in
## 26_general/model_tests/statewide_sim/<scenario>/ (made by tests/sim_statewide.R):
##   Rscript scripts/model/live/run_night.R --sim=tx_base --at="2026-11-03 22:00"
## Sim runs write to 26_general/model_tests/run_night_sim/, never to model_results/.
# ========================================
source("scripts/model/live/load.R")
args = commandArgs(trailingOnly = TRUE)

#' Value of a `--key=value` command-line flag
#'
#' @param k Flag name.
#' @param d Default when the flag is absent.
#' @return The value as a string, or `d`.
opt = function(k, d = NA) {
  a = grep(paste0("^--", k, "="), args, value = TRUE)
  if (length(a)) sub("^[^=]*=", "", a[1]) else d
}
MODELS_RUN = if (!is.na(opt("models"))) {
  strsplit(opt("models"), ",")[[1]]
} else {
  f_cfg = opt("config", file.path(find_26_general(), MODEL_CONFIG_FILE))
  if (file.exists(f_cfg)) {
    models_from_config(f_cfg)
  } else {
    cli::cli_alert_warning(
      "No {.path {f_cfg}}: running RUN (R/run.R). Make the config with make_model_config.R"
    )
    RUN
  }
}
check_models(MODELS_RUN)
SIM = opt("sim")
EVERY = as.numeric(opt("every", "0"))
CBS_ENV = opt("cbs", "live")
CORES = as.integer(opt("cores", max(1L, parallel::detectCores() - 1L)))
TZ = "America/New_York"
d26 = find_26_general()
all_races = races_from_priors(d26)
if (!is.na(opt("states"))) {
  ST_RUN = toupper(strsplit(opt("states"), ",")[[1]])
  all_races = filter(all_races, state %in% ST_RUN)
  if (!nrow(all_races)) stop("--states=", opt("states"), " matches no race in priors.csv")
}

# ---- inputs that don't change during the night: loaded once per state ---------------
# Loaded here, in the main process, before any fits are farmed out: a forked worker's
# caches are thrown away when it finishes, so anything set up inside one is redone every time.
cache = new.env()

#' A state's inputs that don't change during the night
#'
#' Built once per state in the main process, before fits are forked: precinct cells
#' (`build_cells()`), county history (`county_baseline()`), and the demographic covariates
#' (`demo_covs()`, cached).
#'
#' @param st Two-letter state code.
#' @return List: cells and base, each the value or the error it raised.
state_inputs = function(st) {
  if (is.null(cache[[st]])) {
    cells = tryCatch(build_cells(st, d26), error = function(e) e)
    base = tryCatch(county_baseline(st, d26), error = function(e) e)
    try(demo_covs(st, d26), silent = TRUE) # precinct demographics (R/demos.R)
    cache[[st]] = list(cells = cells, base = base)
  }
  cache[[st]]
}
# load the packages the models use once, so each worker doesn't
for (pk in c("fixest", "enightmodels")) {
  suppressPackageStartupMessages(requireNamespace(pk, quietly = TRUE))
}

# ---- the returns for this refresh ---------------------------------------------------
if (!is.na(SIM)) {
  dir_sc = if (dir.exists(SIM)) SIM else file.path(dirname(d26), "26_sim", SIM)
  # opt() returns NA when the flag is absent, and read_sim() auto-detects on NULL -- so NA
  # has to become NULL or it looks for a file literally called NA_truth.parquet.
  ST_OPT = {
    s = opt("state")
    if (is.na(s)) NULL else toupper(s)
  }
  sim = read_sim(dir_sc, state = ST_OPT)
  dir_cbs = file.path(d26, "model_tests", "statewide_sim", basename(dir_sc))
  out_dir = file.path(d26, "model_tests", "run_night_sim", basename(dir_sc))
  # the sim's state: --state, else the truth frame's `state` column, else the scenario-name
  # prefix
  st_sim = toupper(
    if (!is.null(ST_OPT)) {
      ST_OPT
    } else if ("state" %in% names(sim$truth) && length(unique(sim$truth$state)) == 1) {
      unique(sim$truth$state)[1]
    } else {
      substr(basename(dir_sc), 1, 2)
    }
  )
  if (!st_sim %in% all_races$state) {
    stop(
      "sim state '",
      st_sim,
      "' has no races in priors.csv (have: ",
      paste(sort(unique(all_races$state)), collapse = ", "),
      "). Pass --state= to override."
    )
  }
  get_returns = function(at) {
    cells = state_inputs(st_sim)$cells
    bind_rows(lapply(sort(unique(sim$truth$contest)), \(r) {
      returns_from_sim(sim, r, at, geos = cells$geo)
    })) |>
      mutate(race_id = paste0(st_sim, "_", race_id))
  }
} else {
  f_ret = opt("returns")
  f_cand = opt("candidates")
  dir_cbs = d26
  out_dir = file.path(d26, "model_results")
  read_any = function(p) {
    if (grepl("\\.parquet$", p)) read_pq(p) else readr::read_csv(p, show_col_types = FALSE)
  }
  # default: the scrapers' clean files, 26_general/{ST}/clean/*_latest.parquet (returns.R),
  # their geo ids matched to each state's precinct inputs
  if (is.na(f_ret)) {
    get_returns = function(at) {
      geos = unlist(lapply(unique(all_races$state), function(st) {
        cl = state_inputs(st)$cells
        if (inherits(cl, "error")) NULL else unique(cl$geo)
      }))
      returns_from_clean(all_races, d26, geos = geos)
    }
  } else {
    get_returns = function(at) {
      if (is.na(f_cand)) {
        stop("--returns=<file> needs --candidates=<file> too (CONTRACT.md sections 8 and 5)")
      }
      live = read_any(f_ret)
      cand = read_any(f_cand)
      bind_rows(lapply(split(all_races, all_races$state), function(r) {
        returns_from_live(
          filter(live, race_id %in% r$race_id),
          cand,
          state = r$state[1],
          election = r$election[1],
          at = at
        )
      }))
    }
  }
}

# ---- one refresh -------------------------------------------------------------------------
#' One refresh: every race x model, scraped and statewide
#'
#' Reads the returns, pulls CBS county results per state, runs every model on every race
#' (`run_jobs()`), writes predictions, status and precinct files, and prints a summary with
#' the slowest fit per model and any race x model that is not ok.
#'
#' @param at Snapshot time for sim runs; NULL = the latest returns.
#' @return The `run_jobs()` result, invisibly.
refresh = function(at = NULL) {
  t0 = Sys.time()
  ret = get_returns(at)
  now = if (is.null(at)) max(ret$time, na.rm = TRUE) else at
  races = filter(all_races, race_id %in% unique(ret$race_id))
  by_state = split(races, races$state)
  inp = lapply(setNames(names(by_state), names(by_state)), state_inputs)
  t_inp = Sys.time()

  # CBS county results for every race, one state per worker (each pull is a web request)
  cbs = run_par(
    names(by_state),
    function(st) {
      tryCatch(
        if (is.na(SIM)) {
          cbs_for_races(by_state[[st]], env = CBS_ENV, dir_26 = dir_cbs)
        } else {
          cbs_for_races(by_state[[st]], env = "sim", at = now, dir_26 = dir_cbs)
        },
        error = function(e) NULL
      )
    },
    CORES
  )
  names(cbs) = names(by_state)
  cbs = lapply(cbs, function(x) if (is.list(x)) x else NULL)
  t_cbs = Sys.time()

  # every race x model in every state with precinct inputs, in one pool
  no_inp = list()
  jobs = unlist(
    lapply(names(by_state), function(st) {
      rs = by_state[[st]]
      if (inherits(inp[[st]]$cells, "error")) {
        no_inp[[st]] <<- tibble(
          race_id = rs$race_id,
          model = NA_character_,
          time = now,
          status = paste("no precinct inputs:", conditionMessage(inp[[st]]$cells)),
          statewide = NA_character_,
          seconds = NA_real_,
          settings = NA_character_
        )
        return(NULL)
      }
      base = if (inherits(inp[[st]]$base, "error")) NULL else inp[[st]]$base
      make_jobs(
        inp[[st]]$cells,
        filter(ret, race_id %in% rs$race_id),
        races = rs,
        models = MODELS_RUN,
        time = now,
        cbs = if (is.null(base)) NULL else cbs[[st]],
        base = base
      )
    }),
    recursive = FALSE
  )
  res = run_jobs(jobs, CORES)
  t_fit = Sys.time()

  # statewide status: why there are no statewide rows, where there aren't
  cbs_status = bind_rows(lapply(cbs, function(x) attr(x, "status")))
  res$status = bind_rows(res$status, bind_rows(no_inp))
  if (nrow(cbs_status)) {
    res$status = left_join(res$status, cbs_status, by = "race_id")
  }
  for (st in names(by_state)) {
    if (inherits(inp[[st]]$base, "error")) {
      i = res$status$race_id %in% by_state[[st]]$race_id
      res$status$statewide[i] = paste("no county history:", conditionMessage(inp[[st]]$base))
    }
  }
  write_predictions(res, out_dir)
  write_precincts(res, out_dir)
  sec = function(a, b) as.numeric(difftime(b, a, units = "secs"))
  cat(sprintf(
    "%s  %d races x %d models, %d prediction rows (%d statewide) in %.0fs [inputs %.0fs, CBS %.0fs, models %.0fs on %d cores] -> %s\n",
    format(now, "%Y-%m-%d %H:%M:%S", tz = TZ),
    nrow(races),
    length(MODELS_RUN),
    nrow(res$pred),
    sum(res$pred$geo == "all"),
    sec(t0, Sys.time()),
    sec(t0, t_inp),
    sec(t_inp, t_cbs),
    sec(t_cbs, t_fit),
    CORES,
    out_dir
  ))
  slow = res$status |>
    summarize(seconds = max(seconds, na.rm = TRUE), .by = model) |>
    filter(is.finite(seconds))
  if (nrow(slow)) {
    cat(
      "  slowest fit per model:",
      paste(sprintf("%s %.0fs", slow$model, slow$seconds), collapse = ", "),
      "\n"
    )
  }
  bad = filter(res$status, status != "ok" | is.na(statewide) | statewide != "ok")
  if (nrow(bad)) {
    print(
      as.data.frame(select(bad, race_id, model, status, statewide, any_of("cbs"))),
      right = FALSE
    )
  }
  invisible(res)
}

at = if (!is.na(opt("at"))) as.POSIXct(opt("at"), tz = TZ) else NULL
if (EVERY > 0) {
  repeat {
    tryCatch(refresh(at), error = function(e) cat("Refresh failed:", conditionMessage(e), "\n"))
    Sys.sleep(60 * EVERY)
  }
} else {
  refresh(at)
}
