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
##   Rscript scripts/model/live/run_night.R --returns=<file> --candidates=<file>   # one refresh
##   Rscript scripts/model/live/run_night.R --returns=<file> --candidates=<file> --every=5
##                                                         # every 5 minutes until stopped
##   --returns     the scraped returns, CONTRACT.md section 8 (geo, race_id, candidate_id,
##                 method, votes, stage, observed_at); parquet or csv
##   --candidates  CONTRACT.md section 5 (race_id, candidate_id, name, party)
##   --cbs=test    CBS's test feed instead of live (default live)
##   --models=swing,cory,medsl   (default: all three)
##
## Test the whole path on one of Ethan's simulated nights, with the simulated CBS feed in
## 26_general/model_tests/statewide_sim/<scenario>/ (made by tests/sim_statewide.R):
##   Rscript scripts/model/live/run_night.R --sim=tx_base --at="2026-11-03 22:00"
## Sim runs write to 26_general/model_tests/run_night_sim/, never to model_results/.
# ========================================
source("scripts/model/live/load.R")
args = commandArgs(trailingOnly = TRUE)
opt = function(k, d = NA) { a = grep(paste0("^--", k, "="), args, value = TRUE); if (length(a)) sub("^[^=]*=", "", a[1]) else d }
MODELS_RUN = strsplit(opt("models", paste(MODELS, collapse = ",")), ",")[[1]]
SIM = opt("sim"); EVERY = as.numeric(opt("every", "0")); CBS_ENV = opt("cbs", "live")
TZ = "America/New_York"
d26 = find_26_general()
all_races = races_from_priors(d26)

# ---- inputs that don't change during the night: loaded once per state ---------------
cache = new.env()
state_inputs = function(st) {
  if (is.null(cache[[st]])) {
    cells = tryCatch(build_cells(st, d26), error = function(e) e)
    base = tryCatch(county_baseline(st, d26), error = function(e) e)
    cache[[st]] = list(cells = cells, base = base)
  }
  cache[[st]]
}

# ---- the returns for this refresh ---------------------------------------------------
if (!is.na(SIM)) {
  dir_sc = if (dir.exists(SIM)) SIM else file.path(dirname(d26), "26_sim", SIM)
  sim = read_sim(dir_sc)
  dir_cbs = file.path(d26, "model_tests", "statewide_sim", basename(dir_sc))
  out_dir = file.path(d26, "model_tests", "run_night_sim", basename(dir_sc))
  st_sim = toupper(substr(basename(dir_sc), 1, 2))
  get_returns = function(at) {
    cells = state_inputs(st_sim)$cells
    bind_rows(lapply(sort(unique(sim$truth$contest)), \(r) returns_from_sim(sim, r, at, geos = cells$geo))) |>
      mutate(race_id = paste0(st_sim, "_", race_id))
  }
} else {
  f_ret = opt("returns"); f_cand = opt("candidates")
  if (is.na(f_ret) || is.na(f_cand)) stop("Give --returns=<file> and --candidates=<file> (or --sim=<scenario> to test)")
  dir_cbs = d26; out_dir = file.path(d26, "model_results")
  read_any = function(p) if (grepl("\\.parquet$", p)) read_pq(p) else readr::read_csv(p, show_col_types = FALSE)
  get_returns = function(at) {
    live = read_any(f_ret); cand = read_any(f_cand)
    bind_rows(lapply(split(all_races, all_races$state), function(r)
      returns_from_live(filter(live, race_id %in% r$race_id), cand, state = r$state[1],
                        election = r$election[1], at = at)))
  }
}

# ---- one refresh -------------------------------------------------------------------------
refresh = function(at = NULL) {
  t0 = Sys.time()
  ret = get_returns(at)
  now = if (is.null(at)) max(ret$time, na.rm = TRUE) else at
  races = filter(all_races, race_id %in% unique(ret$race_id))
  res = lapply(split(races, races$state), function(rs) {
    st = rs$state[1]
    inp = state_inputs(st)
    if (inherits(inp$cells, "error"))
      return(list(pred = NULL, status = tibble(race_id = rs$race_id, model = NA_character_, time = now,
                                               status = paste("no precinct inputs:", conditionMessage(inp$cells)),
                                               statewide = NA_character_, settings = NA_character_)))
    cbs = if (is.na(SIM)) cbs_for_races(rs, env = CBS_ENV, dir_26 = dir_cbs)
          else cbs_for_races(rs, env = "sim", at = now, dir_26 = dir_cbs)
    base = if (inherits(inp$base, "error")) NULL else inp$base
    out = predict_all(inp$cells, filter(ret, race_id %in% rs$race_id), races = rs, models = MODELS_RUN,
                      time = now, cbs = cbs, base = base)
    out$status = left_join(out$status, attr(cbs, "status"), by = "race_id")
    if (is.null(base)) out$status$statewide = paste("no county history:", conditionMessage(inp$base))
    out
  })
  res = list(pred = bind_rows(lapply(res, `[[`, "pred")), status = bind_rows(lapply(res, `[[`, "status")))
  write_predictions(res, out_dir)
  cat(sprintf("%s  %d races, %d prediction rows (%d statewide) in %.0fs -> %s\n", format(now, "%Y-%m-%d %H:%M:%S", tz = TZ),
              nrow(races), nrow(res$pred), sum(res$pred$geo == "all"), as.numeric(difftime(Sys.time(), t0, units = "secs")), out_dir))
  bad = filter(res$status, status != "ok" | is.na(statewide) | statewide != "ok")
  if (nrow(bad)) print(as.data.frame(select(bad, race_id, model, status, statewide, any_of("cbs"))), right = FALSE)
  invisible(res)
}

at = if (!is.na(opt("at"))) as.POSIXct(opt("at"), tz = TZ) else NULL
if (EVERY > 0) {
  repeat {
    tryCatch(refresh(at), error = function(e) cat("Refresh failed:", conditionMessage(e), "\n"))
    Sys.sleep(60 * EVERY)
  }
} else refresh(at)
