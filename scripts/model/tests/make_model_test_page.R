# ========================================
## Build the model-test page (charts + findings) from the two test outputs
##
## Reads 26_general/model_tests/backtest_tx22_results.csv and sim_<scenario>_results.csv
## (from test_backtest_tx22.R and test_sim_night.R) and writes a self-contained page,
## 26_general/model_tests/model_test_<scenario>.html. Open it in any browser.
## The page layout lives in scripts/model/tests/model_test_page.html; findings at the top are
## computed from the data, so they update with each run.
##
## Run from the cbs-enr folder, after the two tests:
##   Rscript scripts/model/tests/make_model_test_page.R            # scenario tx_base
##   Rscript scripts/model/tests/make_model_test_page.R my_scenario
# ========================================
source("scripts/model/tests/model_test_common.R")
args = commandArgs(trailingOnly = TRUE)
SCENARIO = if (length(args) >= 1) args[1] else "tx_base"

bt = read_csv(file.path(DIR_OUT, "backtest_tx22_results.csv"), show_col_types = FALSE)
sim = read_csv(file.path(DIR_OUT, sprintf("sim_%s_results.csv", SCENARIO)), show_col_types = FALSE,
               col_types = cols(time = "c", race = "c"))
r2 = function(x) round(100 * x, 2)

bt_list = bt |>
  mutate(key = paste0(if_else(order == "county_size", "county", "random"), if_else(county_effect, "_re", "_nore"))) |>
  split(~key) |>
  lapply(\(g) unname(Map(\(a, b, c, d, e, f, n) list(a, b, c, d, e, f, n),
    r2(g$pct_votes), r2(g$est_twop), r2(g$lo_twop), r2(g$hi_twop), r2(g$raw_twop), round(g$p_dem_win, 3), g$n_counties)))

sim_list = sim |>
  split(~race) |>
  lapply(\(g) list(
    truth = r2(g$truth_twop[1]),
    rows = unname(Map(\(tm, a, b, c, d, e, f, x, y, z) list(tm, a, b, c, d, e, f, x, y, z),
      substr(g$time, 6, 16), round(100 * g$pct_votes, 1), r2(g$est_twop), r2(g$lo_twop), r2(g$hi_twop),
      r2(g$raw_twop), round(g$p_dem_win, 3), round(100 * g$pct_early), round(100 * g$pct_eday), round(100 * g$pct_mail)))
  ))

data = list(bt = bt_list, bt_truth = r2(bt$truth_twop[1]), sim = sim_list,
            scenario = SCENARIO, run = format(Sys.time(), "%b %d, %Y %H:%M"))
json = jsonlite::toJSON(data, auto_unbox = TRUE, digits = NA)

tpl = paste(readLines("scripts/model/tests/model_test_page.html", warn = FALSE), collapse = "\n")
if (!grepl("__DATA__", tpl, fixed = TRUE)) stop("Template is missing the __DATA__ placeholder")
page = paste0("<!doctype html>\n<meta charset=\"utf-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
              sub("__DATA__", json, tpl, fixed = TRUE))
out = file.path(DIR_OUT, sprintf("model_test_%s.html", SCENARIO))
writeLines(page, out)
cat("Wrote", out, "\nOpen it in a browser (double-click the file).\n")
