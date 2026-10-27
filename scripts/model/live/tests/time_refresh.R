# ========================================
## time_refresh.R -- how long one election-night refresh of the models takes on this computer.
##
## Replays five scrapes of one of Ethan's simulated nights (TX, both races) and, for each,
## times ONLY the step that reruns every model on the new returns -- exactly what run_night.R
## does every five minutes: all three models on every race, scraped + statewide rows, in
## parallel on all cores but one. Not timed: the one-time set-up at the start of the night
## (loading precinct inputs, medsl_sur's set-up) and reading the new returns / CBS file.
## Nothing is written.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/time_refresh.R
##   Rscript scripts/model/live/tests/time_refresh.R --cores=4      # choose the cores
##   Rscript scripts/model/live/tests/time_refresh.R --cores=1      # one fit at a time
## Needs 26_sim/tx_base and its simulated CBS feed in
## 26_general/model_tests/statewide_sim/tx_base (both in the shared Dropbox).
# ========================================
source("scripts/model/live/load.R")
args = commandArgs(trailingOnly = TRUE)
opt = function(k, d) { a = grep(paste0("^--", k, "="), args, value = TRUE); if (length(a)) sub("^[^=]*=", "", a[1]) else d }
CORES = as.integer(opt("cores", max(1L, parallel::detectCores() - 1L)))
SCEN = opt("sim", "tx_base")
TZ = "America/New_York"
# five scrapes through the night
SCRAPES = as.POSIXct(c("2026-11-03 20:30", "2026-11-03 21:30", "2026-11-03 22:30",
                       "2026-11-04 00:00", "2026-11-04 02:00"), tz = TZ)

d26 = find_26_general()
sim = read_sim(file.path(dirname(d26), "26_sim", SCEN))
dir_cbs = file.path(d26, "model_tests", "statewide_sim", SCEN)
races = races_from_priors(d26) |> filter(state == "TX")

# ---- one-time set-up (not timed in the result) -----------------------------------------
t0 = proc.time()[["elapsed"]]
cells = build_cells("TX", d26)
base = county_baseline("TX", d26)
invisible(medsl_setup("TX", d26, hybrid = TRUE))
for (pk in c("fixest", "enightmodels")) suppressPackageStartupMessages(requireNamespace(pk, quietly = TRUE))
cat(sprintf("One-time set-up for the night: %.1f s. Timing %d refreshes of %s on %d core(s) (of %d).\n\n",
            proc.time()[["elapsed"]] - t0, length(SCRAPES), paste(MODELS, collapse = ", "), CORES, parallel::detectCores()))

# ---- the five refreshes -----------------------------------------------------------------
res = lapply(seq_along(SCRAPES), function(k) {
  tm = SCRAPES[k]
  # the new scrape and the latest CBS county file (not timed)
  ret = bind_rows(lapply(c("gov", "sen"), \(r) returns_from_sim(sim, r, tm, geos = cells$geo))) |>
    mutate(race_id = paste0("TX_", race_id))
  cbs = setNames(lapply(c("gov", "sen"), \(r) read_cbs_latest("TX", r, env = "sim", at = tm, dir_26 = dir_cbs)),
                 c("TX_gov", "TX_sen"))
  # rerun every model on every race: what each five-minute refresh does
  t1 = proc.time()[["elapsed"]]
  out = run_jobs(make_jobs(cells, ret, races, MODELS, time = tm, cbs = cbs, base = base), CORES)
  secs = proc.time()[["elapsed"]] - t1
  per = out$status |> summarize(s = max(seconds, na.rm = TRUE), .by = model)
  bad = filter(out$status, status != "ok")
  cat(sprintf("Refresh %d (%s, %.0f%% of the 32 counties' votes counted): %.1f s   [slowest fit: %s]%s\n",
              k, format(tm, "%H:%M"), 100 * sum(ret$votes) / sum(sim$truth$votes[sim$truth$geo %in% cells$geo]), secs,
              paste(sprintf("%s %.0f s", per$model, per$s), collapse = ", "),
              if (nrow(bad)) paste0("  PROBLEM: ", paste(bad$model, bad$status, collapse = "; ")) else ""))
  secs
})
secs = unlist(res)
cat(sprintf("\nAverage refresh: %.1f s (fastest %.1f, slowest %.1f) for 2 races x %d models on %d core(s).\n",
            mean(secs), min(secs), max(secs), length(MODELS), CORES))
cat("On election night add the time to read the scrape and pull CBS's county results.\n")
