# ========================================
## Run the election-night models on one of Ethan's simulated nights, through run_all(), which
## fits each model as predict_one() does on the night.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/sim_night.R [scenario] [--models=swing,cory,medsl] [--out=<dir>]
## scenario defaults to tx_base (26_sim/tx_base); --models defaults to every wired model.
##
## Writes <dir>/live_sim_<scenario>.csv (default dir 26_general/model_tests): one row per race x model x
## checkpoint, with the truth alongside (truth_twop, truth_turnout, pct_votes).
# ========================================
source("scripts/model/live/load.R")
args = commandArgs(trailingOnly = TRUE)
SCENARIO = c(grep("^--", args, value = TRUE, invert = TRUE), "tx_base")[1]
MODELS_RUN = strsplit(sub("^--models=", "", c(grep("^--models=", args, value = TRUE),
                                               paste0("--models=", paste(MODELS, collapse = ",")))[1]), ",")[[1]]

DIR_26 = find_26_general()
dir_sc = if (dir.exists(SCENARIO)) SCENARIO else file.path(dirname(DIR_26), "26_sim", SCENARIO)
sim = read_sim(dir_sc)
# as on the night (predict_one()): fit the counties the feed covers, and keep the returns rows
# that match no precinct (NC's early-vote sites), which virtual.R adds back at county level
cells = feed_cells(build_cells(sim$truth$state[1], DIR_26), sim$truth)
cat(sprintf("Scenario %s: %d precincts x %d methods; models: %s\n",
            basename(dir_sc), n_distinct(cells$geo), n_distinct(cells$method), paste(MODELS_RUN, collapse = ", ")))

truth = sim$truth |>
  summarize(truth_twop = sum(votes[party == "dem"]) / sum(votes[party %in% c("dem", "rep")]),
            truth_turnout = sum(votes), .by = contest) |>
  rename(race_id = contest)

t0 = Sys.time()
res = map_dfr(sim_checkpoints(), function(tm) {
  ret = bind_rows(lapply(sort(unique(sim$truth$contest)),
                         \(r) returns_from_sim(sim, r, tm)))
  if (sum(ret$votes) == 0) return(NULL)
  cat("  ", format(tm, "%Y-%m-%d %H:%M"), "\n")
  run_all(cells, ret, models = MODELS_RUN) |>
    mutate(time = format(tm, "%Y-%m-%d %H:%M"), .after = model)
}) |>
  left_join(truth, by = "race_id") |>
  mutate(pct_votes = counted_votes / truth_turnout,
         covers = truth_twop >= lo_twop & truth_twop <= hi_twop,
         scenario = basename(dir_sc))

dir_out = sub("^--out=", "", c(grep("^--out=", args, value = TRUE), paste0("--out=", file.path(DIR_26, "model_tests")))[1])
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
out = file.path(dir_out, sprintf("live_sim_%s.csv", basename(dir_sc)))
write_csv(res, out)
cat(sprintf("\n%.1f minutes. Mean absolute error (points) once votes are in, and interval coverage:\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
res |>
  filter(status == "ok") |>
  summarize(checkpoints = n(), mean_abs_err_pts = round(100 * mean(abs(est_twop - truth_twop)), 2),
            interval_holds_truth = sum(covers, na.rm = TRUE), .by = c(race_id, model)) |>
  print(n = Inf)
cat("\nWrote", out, "\n")
