# ========================================
## Run the election-night models on one of Ethan's simulated nights, each fit through
## fit_model(), as predict_one() fits it on the night.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/sim_night.R [scenario] [--models=swing,medsl_sur] [--out=<dir>] [--cores=<n>]
## scenario defaults to tx_base (26_sim/tx_base); --models defaults to every wired model.
## "<model>[/<demo>][/<interval>]" runs a model with a demographic spec (R/demos.R) and an
## interval (R/results.R), e.g. --models=swing,swing/strat_d_cov,pois_bayes/tpsob/conformal
## Every checkpoint x race x model is one job, run in forked workers: --cores defaults to half
## the machine's cores minus one (4 on a 10-core laptop), so a run leaves the computer usable.
## Memory is about 1-4 GB per worker on TX.
##
## Writes <dir>/live_sim_<scenario>.csv (default dir 26_general/model_tests): one row per race x model x
## checkpoint, with the truth alongside (truth_twop, truth_turnout, pct_votes).
# ========================================
source("scripts/model/live/load.R")
args = commandArgs(trailingOnly = TRUE)
SCENARIO = c(grep("^--", args, value = TRUE, invert = TRUE), "tx_base")[1]

#' Value of a `--key=value` command-line flag
#'
#' @param key Flag name.
#' @param default Value when the flag is absent.
#' @return The value as a string.
opt = function(key, default) {
  sub(
    paste0("^--", key, "="),
    "",
    c(grep(paste0("^--", key, "="), args, value = TRUE), paste0("--", key, "=", default))[1]
  )
}
MODELS_RUN = strsplit(opt("models", paste(MODELS, collapse = ",")), ",")[[1]]
check_models(MODELS_RUN)
CORES = as.integer(opt("cores", max(1L, parallel::detectCores() %/% 2L - 1L)))

DIR_26 = find_26_general()
dir_sc = if (dir.exists(SCENARIO)) SCENARIO else file.path(dirname(DIR_26), "26_sim", SCENARIO)
sim = read_sim(dir_sc)
# as on the night (predict_one()): fit the counties the feed covers, and keep the returns rows
# that match no precinct (NC's early-vote sites), which virtual.R adds back at county level
cells = feed_cells(build_cells(sim$truth$state[1], DIR_26), sim$truth)
cat(sprintf(
  "Scenario %s: %d precincts x %d methods; models: %s\n",
  basename(dir_sc),
  n_distinct(cells$geo),
  n_distinct(cells$method),
  paste(MODELS_RUN, collapse = ", ")
))

truth = sim$truth |>
  summarize(
    truth_twop = sum(votes[party == "dem"]) / sum(votes[party %in% c("dem", "rep")]),
    truth_turnout = sum(votes),
    .by = contest
  ) |>
  rename(race_id = contest)

# the returns at every checkpoint with votes, one job per checkpoint x race x model
rets = lapply(sim_checkpoints(), function(tm) {
  bind_rows(lapply(sort(unique(sim$truth$contest)), \(r) returns_from_sim(sim, r, tm)))
})
rets = rets[vapply(rets, \(x) sum(x$votes) > 0, NA)]
jobs = tidyr::expand_grid(
  i = seq_along(rets),
  race_id = sort(unique(sim$truth$contest)),
  model = MODELS_RUN
)

# one-time set-up in this process, before forking, so the workers inherit it: each model's
# own set-up (MODEL_SETUP, run.R) and the covariate caches
st = sim$truth$state[1]
# (a set-up failure, e.g. medsl_sur in a state modeled without vote method, is left for its
# own jobs to report, so the other models still run)
for (m in MODELS_RUN) {
  tryCatch(setup_model(m, cells, st), error = function(e) {
    message(m, " set-up failed: ", conditionMessage(e))
  })
}
invisible(demo_covs(st))
cat(sprintf("%d checkpoints with votes, %d jobs on %d core(s)\n", length(rets), nrow(jobs), CORES))

t0 = Sys.time()

#' Run job `k`: one checkpoint x race x model
#'
#' A skipped or failed fit keeps its status, which says why.
#'
#' @param k Row of `jobs`.
#' @return One-row tibble: race_id, model, time, the fit's race row, seconds.
one = function(k) {
  j = jobs[k, ]
  ret = filter(rets[[j$i]], race_id == j$race_id)
  s0 = proc.time()[["elapsed"]]
  # a skipped fit (e.g. medsl_sur on a race without a vote-method split) has no race row:
  # keep its status, which says why
  fit = tryCatch(
    {
      f = suppressMessages(fit_model(j$model, cells, ret))
      if (is.null(f$race) || !nrow(f$race)) tibble(status = f$status) else f$race
    },
    error = function(e) tibble(status = paste("error:", conditionMessage(e)))
  )
  bind_cols(
    tibble(race_id = j$race_id, model = j$model, time = format(max(ret$time), "%Y-%m-%d %H:%M")),
    fit,
    tibble(seconds = round(proc.time()[["elapsed"]] - s0, 1))
  )
}
res = run_par(seq_len(nrow(jobs)), one, CORES)
bad = !vapply(res, is.data.frame, NA)
if (any(bad)) {
  res[bad] = lapply(which(bad), \(k) {
    tibble(
      race_id = jobs$race_id[k],
      model = jobs$model[k],
      status = paste("error: worker failed:", as.character(res[[k]]))
    )
  })
}
res = bind_rows(
  res,
  tibble(
    est_twop = numeric(),
    lo_twop = numeric(),
    hi_twop = numeric(),
    counted_votes = numeric(),
    seconds = numeric()
  )
) |> # columns even if every fit failed
  left_join(truth, by = "race_id") |>
  mutate(
    pct_votes = counted_votes / truth_turnout,
    covers = truth_twop >= lo_twop & truth_twop <= hi_twop,
    scenario = basename(dir_sc)
  )

dir_out = sub(
  "^--out=",
  "",
  c(grep("^--out=", args, value = TRUE), paste0("--out=", file.path(DIR_26, "model_tests")))[1]
)
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
out = file.path(dir_out, sprintf("live_sim_%s.csv", basename(dir_sc)))
write_csv(res, out)
cat(sprintf(
  "\n%.1f minutes. Mean absolute error (points) once votes are in, and interval coverage:\n",
  as.numeric(difftime(Sys.time(), t0, units = "mins"))
))
res |>
  filter(grepl("^ok", status)) |>
  summarize(
    checkpoints = n(),
    mean_abs_err_pts = round(100 * mean(abs(est_twop - truth_twop)), 2),
    interval_holds_truth = sum(covers, na.rm = TRUE),
    mean_seconds = round(mean(seconds), 1),
    .by = c(race_id, model)
  ) |>
  print(n = Inf)
cat("\nWrote", out, "\n")
