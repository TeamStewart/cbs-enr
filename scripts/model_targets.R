# ========================================
## Election-night models as targets: what scripts/model/live/run_night.R does in one
## refresh, split so targets runs it in parallel on the crew workers and skips any
## race x model whose inputs did not change since the last tar_make().
##
## Per state in priors.csv (static branches, model_*_{ST} in _targets.R):
##   model_inputs_{ST}   precinct cells and county history (build_cells(), county_baseline())
##   model_cbs_{ST}      fresh CBS county results for its races (cbs_for_races())
##   model_slices_{ST}   one element per race: its returns, CBS snapshot and time. Waits
##                       only on that state's scrapes, so a state models as soon as its
##                       counties are in
##   model_fits_{ST}     dynamic: cross(race, model), one predict_one() each. A branch
##                       whose race slice, model and inputs are unchanged is skipped
## Then model_out writes the usual model_results/ files (write_predictions(),
## write_precincts()) and starts the redistricting charts, as run_night.R does.
##
## The large outputs stay in the Dropbox (model_results/); the targets store keeps only
## each branch's tables.
# ========================================

# States whose races get modeled: every state in priors.csv, or the sim's state (its
# scenario name starts with the state, e.g. "tx_base").
model_states <- function(sim = MODEL_SIM) {
  if (nzchar(sim)) return(toupper(substr(basename(sim), 1, 2)))
  sort(unique(races_from_priors(find_26_general())$state))
}

# A state's inputs that don't change during the night. Errors are kept, not raised, so
# its races get a status saying why (as state_inputs() in run_night.R).
model_state_inputs <- function(state) {
  d26 = find_26_general()
  cells = tryCatch(build_cells(state, d26), error = function(e) e)
  base = tryCatch(county_baseline(state, d26), error = function(e) e)
  list(cells = cells, base = base)
}

# CBS county results for a state's races: race_id -> snapshot (NULL where none), with
# attribute "status".
model_state_cbs <- function(state, sim = MODEL_SIM, at = MODEL_SIM_AT, env = MODEL_CBS_ENV) {
  d26 = find_26_general()
  races = filter(races_from_priors(d26), state == !!state)
  if (nzchar(sim)) {
    dir_cbs = file.path(d26, "model_tests", "statewide_sim", basename(sim))
    return(cbs_for_races(races, env = "sim", at = as.POSIXct(at, tz = "America/New_York"), dir_26 = dir_cbs))
  }
  cbs_for_races(races, env = env, dir_26 = d26)
}

# One element per race with returns: race (its priors.csv row), ret (its returns), cbs
# (its CBS snapshot, or NULL) and time (its latest return, or the sim's `at`). Slicing by
# race is what lets targets skip a race whose returns and CBS results did not change.
# `feed` is the state's scrape summaries: it only orders this after them.
model_state_slices <- function(state, inputs, cbs, sim = MODEL_SIM, at = MODEL_SIM_AT, feed = NULL) {
  d26 = find_26_general()
  races = filter(races_from_priors(d26), state == !!state)
  geos = if (inherits(inputs$cells, "error")) NULL else unique(inputs$cells$geo)
  if (nzchar(sim)) {
    at = as.POSIXct(at, tz = "America/New_York")
    dir_sc = if (dir.exists(sim)) sim else file.path(dirname(d26), "26_sim", sim)
    s = read_sim(dir_sc, state = state)
    ret = bind_rows(lapply(sort(unique(s$truth$contest)), \(r) returns_from_sim(s, r, at, geos = geos))) |>
      mutate(race_id = paste0(state, "_", race_id))
  } else {
    ret = tryCatch(returns_from_clean(races, d26, geos = geos), error = function(e) {
      cli::cli_alert_warning("{state}: {conditionMessage(e)}")
      NULL
    })
  }
  # list(NULL), not list(): targets cannot branch over nothing; model_fit() skips NULL
  if (is.null(ret) || !nrow(ret)) return(list(NULL))
  races = filter(races, race_id %in% ret$race_id)
  lapply(split(races, races$race_id), function(race) {
    r = filter(ret, race_id == race$race_id)
    list(
      race = race,
      ret = r,
      cbs = cbs[[race$race_id]],
      cbs_status = if (is.null(attr(cbs, "status"))) tibble() else filter(attr(cbs, "status"), race_id == race$race_id),
      time = if (nzchar(sim)) at else max(r$time, na.rm = TRUE)
    )
  })
}

# One race x one model (predict_one() through make_jobs() / run_jobs(), which run the
# model's set-up first). Returns run_jobs()'s list: pred, turnout, prec, status.
model_fit <- function(slice, model, inputs) {
  if (is.null(slice)) return(NULL)
  race = slice$race
  if (inherits(inputs$cells, "error")) {
    return(list(status = tibble(
      race_id = race$race_id, model = model, time = slice$time,
      status = paste("no precinct inputs:", conditionMessage(inputs$cells)),
      statewide = NA_character_, seconds = NA_real_, settings = NA_character_
    )))
  }
  base = if (inherits(inputs$base, "error")) NULL else inputs$base
  jobs = make_jobs(
    inputs$cells, slice$ret, races = race, models = model, time = slice$time,
    cbs = if (is.null(base)) NULL else setNames(list(slice$cbs), race$race_id),
    base = base
  )
  res = run_jobs(jobs, cores = 1)
  if (nrow(slice$cbs_status)) res$status = left_join(res$status, slice$cbs_status, by = "race_id")
  if (inherits(inputs$base, "error")) res$status$statewide = paste("no county history:", conditionMessage(inputs$base))
  res
}

# Folder the night's model files go to: model_results/, or the sim's test folder.
model_out_dir <- function(sim = MODEL_SIM) {
  d26 = find_26_general()
  if (nzchar(sim)) file.path(d26, "model_tests", "run_night_sim", basename(sim)) else file.path(d26, "model_results")
}

# Writes every fit to model_out_dir() (write_predictions(), write_precincts()) and starts
# the redistricting charts. `fits` is a list of model_fits_{ST} (each a list of branches).
# Returns the folder, so model_results reads it after the write.
model_write <- function(fits, sim = MODEL_SIM, redistricting = MODEL_REDISTRICTING) {
  fits = unlist(fits, recursive = FALSE)
  res = list(
    pred = bind_rows(lapply(fits, `[[`, "pred")),
    turnout = bind_rows(lapply(fits, `[[`, "turnout")),
    prec = bind_rows(lapply(fits, `[[`, "prec")),
    status = bind_rows(lapply(fits, `[[`, "status"))
  )
  dir = model_out_dir(sim)
  if (!nrow(res$status)) {
    cli::cli_alert_warning("No race had returns: nothing written to {dir}")
    return(dir)
  }
  write_predictions(res, dir)
  write_precincts(res, dir)
  if (redistricting && !nzchar(sim)) start_redistricting(MODEL_CBS_ENV)
  bad = filter(res$status, status != "ok" | is.na(statewide) | statewide != "ok")
  if (nrow(bad)) print(as.data.frame(select(bad, race_id, model, status, statewide)), right = FALSE)
  dir
}

# scripts/model/reports/redistricting_compare.R --live in the background, as run_night.R:
# a slow or failed chart never holds up the models. Log in model_results/redistricting/.
start_redistricting <- function(env = MODEL_CBS_ENV) {
  d26 = find_26_general()
  f = "scripts/model/reports/redistricting_compare.R"
  dir_r = file.path(d26, "model_results", "redistricting")
  if (!file.exists(f) || !length(list.files(file.path(d26, "input_data", "redistricting"), "_county_district_weights\\.csv$"))) {
    cli::cli_alert_warning("Redistricting charts not set up (no weights in input_data/redistricting/): see BEFORE THE NIGHT in {f}")
    return(invisible(FALSE))
  }
  dir.create(dir_r, recursive = TRUE, showWarnings = FALSE)
  system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(f), "--live", paste0("--env=", env)),
    stdout = file.path(dir_r, "live.log"), stderr = file.path(dir_r, "live.log"), wait = FALSE
  )
  invisible(TRUE)
}

# The dashboard's data for the published page: written where automation.R pushes it from
# (data/dashboard/live/, gitignored on main; see push_dashboard_data() in automation.R).
export_dashboard_data <- function(dd, path = "data/dashboard/live/dashboard_data.rds") {
  fs::dir_create(fs::path_dir(path))
  tmp = paste0(path, ".tmp")
  saveRDS(dd, tmp)
  file.rename(tmp, path)
  path
}
