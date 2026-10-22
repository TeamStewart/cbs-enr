# ========================================
## One command for the model tests: run the full model on Ethan's simulated nights,
## then build one page comparing them
##
## Run from the cbs-enr folder:
##   Rscript scripts/model/tests/run_model_tests.R                    # every scenario in 26_sim
##   Rscript scripts/model/tests/run_model_tests.R tx_base tx_slow    # just these
##   Rscript scripts/model/tests/run_model_tests.R --new              # only runs with no
##       results yet, or whose sim files are newer than their results (the page still shows all)
##
## Model-choice runs (--variants=): the same scenario(s) with one model setting changed.
## Each gets its own results file, sim_<scenario>__<variant>_results.csv, and its own row
## on the page, so it never replaces the default run.
##   Rscript scripts/model/tests/run_model_tests.R tx_base --variants=nc,stopped
##   Variants:
##     nc       mode partisan offsets borrowed from NC 2024 (default: zero)
##     tx24     mode partisan offsets measured on TX 2024 (CONTRACT.md section 7)
##     stopped  a cell counts as fully counted once it has not grown in 30 minutes,
##              instead of being told by the feed (closer to what TX will give us)
##     diag     no cross-mode correlation (SIGMA_STRUCT=diag)
##     nore     no county random effect (USE_COUNTY_RE=0)
##     swing    the standard uniform swing model (feols with county fixed effects)
##     both     model_both.R -- swing-style theta prior on the precinct x mode model.
##              UNVALIDATED; present so it can be compared, not because it is recommended
##     cory     model_cory: enightmodels::model_legacy() (Cory), default configuration
##     core     the simpler model_core.R version
##   Add "default" to the list to rerun the default settings too, e.g. --variants=default,nc
##
## Other flags passed to every run of test_sim_night.R: --model=, --cells=, --complete=
## (see test_sim_night.R). --with-tx22 also reruns the older TX 2022 backtest.
## --all-runs also puts runs that only change a model setting (nc, tx24, stopped, ...) on the
## page; by default it shows one run per model (model_y, model_simpleswing, model_cory).
## --publish also copies the page to model_tests/index.html in the repo for GitHub Pages
## (commit it and merge to main to update the online page).
##
## Each run takes about 10 minutes. Writes 26_general/model_tests/sim_*_results.csv, then
## 26_general/model_tests/model_tests.html, and opens it. Stops at the first failure.
# ========================================
source("scripts/model/tests/model_test_common.R")    # for DIR_SIM, DIR_OUT
args = commandArgs(trailingOnly = TRUE)

VARIANTS = list(
  default = list(env = character(), flags = character()),
  nc      = list(env = "MODE_OFFSET_SOURCE=nc", flags = character()),
  tx24    = list(env = c("MODE_OFFSET_SOURCE=nc", "MODE_OFFSETS_TX24=1"), flags = character()),
  stopped = list(env = character(), flags = "--complete=stopped"),
  diag    = list(env = "SIGMA_STRUCT=diag", flags = character()),
  nore    = list(env = "USE_COUNTY_RE=0", flags = character()),
  core    = list(env = character(), flags = "--model=core"),
  swing   = list(env = character(), flags = "--model=swing"),
  cory    = list(env = character(), flags = "--model=cory"),
  # model_both.R: the trial model with a swing-style theta prior. UNVALIDATED -- it is
  # here so it can be stress-tested alongside the others, not because it is recommended.
  # On tx_turnout_low it fixes LATE turnout (106.5% -> 100.8% of truth) and costs share
  # (0.148 -> 0.250 pp); on the early night it barely moves, because its per-cell level
  # fit needs counted-out cells. See scripts/model/model_both.R for its acceptance tests.
  both    = list(env = character(), flags = "--model=both")
)
sim_flags = grep("^--(model|cells|complete)=", args, value = TRUE)
var_arg = grep("^--variants=", args, value = TRUE)
variants = if (length(var_arg)) strsplit(sub("^--variants=", "", var_arg[1]), ",")[[1]] else "default"
bad_v = setdiff(variants, names(VARIANTS))
if (length(bad_v)) stop("Unknown variant(s): ", paste(bad_v, collapse = ", "), ". Choose from: ", paste(names(VARIANTS), collapse = ", "))
only_new = "--new" %in% args
with_tx22 = "--with-tx22" %in% args
publish = "--publish" %in% args
all_runs = "--all-runs" %in% args
bad = setdiff(grep("^--", args, value = TRUE), c(sim_flags, var_arg, "--new", "--with-tx22", "--publish", "--all-runs"))
if (length(bad)) stop("Unknown option(s): ", paste(bad, collapse = " "))

all_sc = basename(dirname(list.files(DIR_SIM, pattern = "^TX_truth\\.parquet$", recursive = TRUE, full.names = TRUE)))
scenarios = grep("^--", args, value = TRUE, invert = TRUE)
if (!length(scenarios)) scenarios = all_sc
miss = setdiff(scenarios, all_sc)
if (length(miss)) stop("No such scenario in ", DIR_SIM, ": ", paste(miss, collapse = ", "),
                       ". Available: ", paste(all_sc, collapse = ", "))

# every (scenario, variant) pair, and whether it needs running
jobs = expand.grid(sc = scenarios, v = variants, stringsAsFactors = FALSE)
jobs$file = file.path(DIR_OUT, sprintf("sim_%s%s_results.csv", jobs$sc, ifelse(jobs$v == "default", "", paste0("__", jobs$v))))
if (only_new) {
  stale = !file.exists(jobs$file) |
    file.mtime(file.path(DIR_SIM, jobs$sc, "TX_truth.parquet")) > file.mtime(jobs$file)
  jobs = jobs[is.na(stale) | stale, ]
}

run = function(script, args = character(), env = character()) {
  cat("\n==========", paste(c(env, script, args), collapse = " "), "==========\n")
  status = system2("Rscript", c(file.path("scripts/model/tests", script), args), env = env)
  if (status != 0) stop(script, " failed (exit status ", status, "); see the messages above")
}

t0 = Sys.time()
cat(sprintf("Runs: %s\n", if (nrow(jobs)) paste(ifelse(jobs$v == "default", jobs$sc, paste(jobs$sc, jobs$v, sep = " · ")), collapse = ", ") else "none (all up to date)"))
if (nrow(jobs)) cat(sprintf("About %d minutes.\n", 10 * nrow(jobs)))
if (with_tx22) run("test_backtest_tx22.R")
for (i in seq_len(nrow(jobs))) {
  v = VARIANTS[[jobs$v[i]]]
  run("test_sim_night.R", c(jobs$sc[i], sim_flags, v$flags, if (jobs$v[i] != "default") paste0("--label=", jobs$v[i])), v$env)
}
run("make_model_test_page.R", c(if (publish) "--publish", if (all_runs) "--all-runs"))

page = file.path(DIR_OUT, "model_tests.html")
cat(sprintf("\nDone in %.1f minutes. Page: %s\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), page))
if (interactive() || Sys.getenv("OPEN_PAGE", "1") == "1") try(utils::browseURL(page), silent = TRUE)
