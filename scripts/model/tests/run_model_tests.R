# ========================================
## One command for the model tests: run the full model on Ethan's simulated nights,
## then build one page comparing them
##
## Run from the cbs-enr folder:
##   Rscript scripts/model/tests/run_model_tests.R                    # every scenario in 26_sim
##   Rscript scripts/model/tests/run_model_tests.R tx_base tx_slow    # just these
##   Rscript scripts/model/tests/run_model_tests.R --new              # only scenarios with no
##       results yet, or whose sim files are newer than their results (the page still shows all)
##   Flags passed to every run of test_sim_night.R:
##       --model=core, --cells=complete, --complete=stopped (see test_sim_night.R)
##   --with-tx22   also rerun the older TX 2022 backtest (simpler model)
##   Trial-model switches go in front as environment variables, e.g.
##   MODE_OFFSET_SOURCE=nc Rscript scripts/model/tests/run_model_tests.R
##
## Each scenario takes about 10 minutes. Writes 26_general/model_tests/sim_<scenario>_results.csv
## for each, then 26_general/model_tests/model_tests.html, and opens it. Stops at the first failure.
# ========================================
source("scripts/model/tests/model_test_common.R")    # for DIR_SIM, DIR_OUT
args = commandArgs(trailingOnly = TRUE)
sim_flags = grep("^--(model|cells|complete)=", args, value = TRUE)
only_new = "--new" %in% args
with_tx22 = "--with-tx22" %in% args
bad = setdiff(grep("^--", args, value = TRUE), c(sim_flags, "--new", "--with-tx22"))
if (length(bad)) stop("Unknown option(s): ", paste(bad, collapse = " "))

all_sc = basename(dirname(list.files(DIR_SIM, pattern = "^TX_truth\\.parquet$", recursive = TRUE, full.names = TRUE)))
scenarios = grep("^--", args, value = TRUE, invert = TRUE)
if (!length(scenarios)) scenarios = all_sc
miss = setdiff(scenarios, all_sc)
if (length(miss)) stop("No such scenario in ", DIR_SIM, ": ", paste(miss, collapse = ", "),
                       ". Available: ", paste(all_sc, collapse = ", "))
if (only_new) {
  stale = function(sc) {
    res = file.path(DIR_OUT, sprintf("sim_%s_results.csv", sc))
    !file.exists(res) || file.mtime(file.path(DIR_SIM, sc, "TX_truth.parquet")) > file.mtime(res)
  }
  to_run = Filter(stale, scenarios)
} else to_run = scenarios

run = function(script, ...) {
  cat("\n==========", script, paste(...), "==========\n")
  status = system2("Rscript", c(file.path("scripts/model/tests", script), ...))
  if (status != 0) stop(script, " failed (exit status ", status, "); see the messages above")
}

t0 = Sys.time()
cat(sprintf("Scenarios to run: %s\n", if (length(to_run)) paste(to_run, collapse = ", ") else "none (all up to date)"))
if (with_tx22) run("test_backtest_tx22.R")
for (sc in to_run) run("test_sim_night.R", sc, sim_flags)
run("make_model_test_page.R")

page = file.path(DIR_OUT, "model_tests.html")
cat(sprintf("\nDone in %.1f minutes. Page: %s\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), page))
if (interactive() || Sys.getenv("OPEN_PAGE", "1") == "1") try(utils::browseURL(page), silent = TRUE)
