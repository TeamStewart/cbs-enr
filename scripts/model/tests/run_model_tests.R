# ========================================
## One command for the whole model test: backtest, simulated night, and the page
##
## Run from the cbs-enr folder:
##   Rscript scripts/model/tests/run_model_tests.R                       # everything, scenario tx_base
##   Rscript scripts/model/tests/run_model_tests.R my_scenario           # another of Ethan's scenarios
##   Rscript scripts/model/tests/run_model_tests.R tx_base --skip-backtest
##       reuse the last backtest results (the backtest doesn't depend on the scenario,
##       so rerun it only when the model code changes)
##
## Runs, in order: test_backtest_tx22.R, test_sim_night.R, make_model_test_page.R, then
## opens 26_general/model_tests/model_test_<scenario>.html. Stops at the first failure.
# ========================================
args = commandArgs(trailingOnly = TRUE)
skip_backtest = "--skip-backtest" %in% args
scenario = setdiff(args, "--skip-backtest")
scenario = if (length(scenario) >= 1) scenario[1] else "tx_base"

run = function(script, ...) {
  cat("\n==========", script, paste(...), "==========\n")
  status = system2("Rscript", c(file.path("scripts/model/tests", script), ...))
  if (status != 0) stop(script, " failed (exit status ", status, "); see the messages above")
}

t0 = Sys.time()
if (!skip_backtest) run("test_backtest_tx22.R") else cat("Skipping the backtest; using the last backtest_tx22_results.csv\n")
run("test_sim_night.R", scenario)
run("make_model_test_page.R", scenario)

source("scripts/model/tests/model_test_common.R")   # for DIR_OUT
page = file.path(DIR_OUT, sprintf("model_test_%s.html", scenario))
cat(sprintf("\nDone in %.1f minutes. Page: %s\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), page))
if (interactive() || Sys.getenv("OPEN_PAGE", "1") == "1") try(utils::browseURL(page), silent = TRUE)
