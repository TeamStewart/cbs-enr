# ========================================
## time_models.R -- how long each model takes per race, on THIS computer.
##
## For each state with precinct inputs, makes one fake race with half the precincts x
## methods counted, then:
##   1. times each model on it, one at a time (seconds per race per refresh)
##   2. runs every state x model at once through run_jobs(), as run_night.R does, and
##      reports the wall-clock time for the whole pool on --cores cores
## The fake returns only exercise the code; the estimates are meaningless.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/time_models.R                 # every state with inputs
##   Rscript scripts/model/live/tests/time_models.R TX FL --cores=4
# ========================================
source("scripts/model/live/load.R")
args = commandArgs(trailingOnly = TRUE)
cores = as.integer(sub("^--cores=", "", c(grep("^--cores=", args, value = TRUE),
                                          paste0("--cores=", max(1L, parallel::detectCores() - 1L)))[1]))
d26 = find_26_general()
have = sub("_precinct\\.parquet$", "", list.files(file.path(d26, "input_data", "model_inputs"), pattern = "^[A-Z]{2}_precinct\\.parquet$"))
states = toupper(grep("^--", args, value = TRUE, invert = TRUE))
if (!length(states)) states = have
for (pk in c("fixest", "enightmodels")) suppressPackageStartupMessages(requireNamespace(pk, quietly = TRUE))

fake_returns = function(cells, st) {
  set.seed(1)
  rep = runif(nrow(cells)) < 0.5
  s = plogis(qlogis(pmin(pmax(cells$dem2, 0.01), 0.99)) + 0.1 + rnorm(nrow(cells), 0, 0.1))
  v = round(cells$exp_votes * 0.95 * rep)
  bind_rows(
    tibble(geo = cells$geo, method = cells$method, candidate = "D", party = "dem", votes = round(v * (1 - cells$oth) * s)),
    tibble(geo = cells$geo, method = cells$method, candidate = "R", party = "rep", votes = round(v * (1 - cells$oth) * (1 - s))),
    tibble(geo = cells$geo, method = cells$method, candidate = "O", party = "oth", votes = round(v * cells$oth))) |>
    mutate(race_id = paste0(st, "_gov"), state = st, election = "2026-11-03",
           complete = rep[match(paste(geo, method), paste(cells$geo, cells$method))]) |>
    mutate(stage = case_when(all(complete) ~ 6L, any(votes > 0) ~ 4L, TRUE ~ 0L), .by = geo) |>
    mutate(time = as.POSIXct("2026-11-03 22:00", tz = "America/New_York"))
}

cat(sprintf("Timing %s on %d core(s) available to the pool\n\n", paste(states, collapse = ", "), cores))
jobs = list(); rows = list()
for (st in states) {
  t0 = proc.time()[["elapsed"]]
  cells = tryCatch(build_cells(st, d26), error = function(e) NULL)
  if (is.null(cells)) { cat(st, ": no precinct inputs, skipped\n"); next }
  for (m in MODELS) setup_model(m, cells, st)
  setup = proc.time()[["elapsed"]] - t0
  ret = fake_returns(cells, st)
  race = tibble(race_id = ret$race_id[1], state = st, election = as.Date("2026-11-03"), jurisdiction = st,
                contest = "gov", district = NA_integer_)
  for (m in MODELS) {
    t1 = proc.time()[["elapsed"]]
    f = tryCatch(run_race(m, cells, ret), error = function(e) list(status = paste("error:", conditionMessage(e))))
    rows[[length(rows) + 1]] = tibble(state = st, precincts = n_distinct(cells$geo), setup_once_s = round(setup, 1),
                                      model = m, seconds = round(proc.time()[["elapsed"]] - t1, 1),
                                      status = f$status)
  }
  jobs = c(jobs, make_jobs(cells, ret, race, MODELS, time = ret$time[1]))
}
tab = bind_rows(rows)
cat("One race, one model at a time (seconds per race per refresh):\n")
print(as.data.frame(tidyr::pivot_wider(select(tab, state, precincts, setup_once_s, model, seconds), names_from = model, values_from = seconds)), row.names = FALSE)
if (any(tab$status != "ok")) print(as.data.frame(filter(tab, status != "ok")), row.names = FALSE)

t2 = proc.time()[["elapsed"]]
res = run_jobs(jobs, cores)
wall = proc.time()[["elapsed"]] - t2
cat(sprintf("\nAll %d race x model fits at once on %d core(s): %.0f seconds wall clock (sum of the fits: %.0f s)\n",
            length(jobs), cores, wall, sum(res$status$seconds, na.rm = TRUE)))
