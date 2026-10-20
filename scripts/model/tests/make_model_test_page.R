# ========================================
## Build the model-test page (charts + findings) from the simulated-night results
##
## Only reads results files, so it never reruns the model: rerun it any time to rebuild the page.
## Reads 26_general/model_tests/sim_<scenario>_results.csv for each scenario, and
## sim_<scenario>__<label>_results.csv for runs with other model settings (both from
## test_sim_night.R), each scenario's 26_sim/<scenario>/manifest.json (to list what is
## different from tx_base), and, if present, the older backtest_tx22_results.csv. Writes
## one self-contained page, 26_general/model_tests/model_tests.html. Open it in any browser.
## The page layout lives in scripts/model/tests/model_test_page.html; findings at the top
## are computed from the data, so they update with each run.
##
## Run from the cbs-enr folder, after test_sim_night.R:
##   Rscript scripts/model/tests/make_model_test_page.R                  # every scenario with results
##   Rscript scripts/model/tests/make_model_test_page.R tx_base tx_slow  # just these scenarios
##       (with all their labelled runs)
# ========================================
source("scripts/model/tests/model_test_common.R")
args = commandArgs(trailingOnly = TRUE)
# one run = one results file; its key is "<scenario>" or "<scenario>__<label>"
have = sub("^sim_(.*)_results\\.csv$", "\\1", list.files(DIR_OUT, pattern = "^sim_.*_results\\.csv$"))
if (!length(have)) stop("No sim_*_results.csv in ", DIR_OUT, ". Run test_sim_night.R first.")
sc_of = function(k) sub("__.*$", "", k)
want = if (length(args)) args else unique(sc_of(have))
miss = setdiff(want, sc_of(have))
if (length(miss)) stop("No results for: ", paste(miss, collapse = ", "), ". Run test_sim_night.R for them first.")
want = c(intersect("tx_base", want), sort(setdiff(want, "tx_base")))
# each scenario's default run first, then its labelled runs
KEYS = unlist(lapply(want, \(sc) { k = have[sc_of(have) == sc]; c(intersect(sc, k), sort(setdiff(k, sc))) }))
r2 = function(x) round(100 * x, 2)

# ---- what each scenario changes, from its manifest ----------------------------------
read_params = function(sc) {
  f = file.path(DIR_SIM, sc, "manifest.json")
  if (!file.exists(f)) return(NULL)
  m = jsonlite::fromJSON(f, simplifyVector = FALSE)
  c(m$params, list(seed = m$seed, dark_counties = m$dark_counties))
}
show_val = function(v) {
  if (is.null(v) || !length(v)) return("none")
  if (is.list(v) && !is.null(names(v)) && length(v)) return(paste(names(v), unlist(lapply(v, show_val)), sep = " ", collapse = ", "))
  if (is.list(v)) return(paste(vapply(v, \(x) if (is.list(x)) paste(unlist(x), collapse = " ") else as.character(x), ""), collapse = "; "))
  paste(v, collapse = "–")
}
base = read_params("tx_base")
diff_text = function(sc) {
  p = read_params(sc)
  if (is.null(p)) return("no manifest found")
  if (sc == "tx_base" || is.null(base)) return(if (sc == "tx_base") "" else "tx_base manifest not found")
  keys = union(names(p), names(base))
  d = keys[!vapply(keys, \(k) identical(p[[k]], base[[k]]), TRUE)]
  if (!length(d)) return("same settings as tx_base")
  paste(sprintf("%s %s (base %s)", d, vapply(d, \(k) show_val(p[[k]]), ""), vapply(d, \(k) show_val(base[[k]]), "")), collapse = "; ")
}

# ---- one chart's rows: time, % counted, estimate, lo, hi, raw, P(Dem wins), % early/eday/mail
night_list = function(df) df |>
  split(~race) |>
  lapply(\(g) list(
    truth = r2(g$truth_twop[1]),
    truth_turn = round(g$truth_turnout[1] / 1e6, 4),
    # turnout in millions: predicted, lo, hi, counted so far
    rows = unname(Map(\(tm, a, b, c, d, e, f, x, y, z, t1, t2, t3, t4) list(tm, a, b, c, d, e, f, x, y, z, t1, t2, t3, t4),
      substr(g$time, 6, 16), round(100 * g$pct_votes, 1), r2(g$est_twop), r2(g$lo_twop), r2(g$hi_twop),
      r2(g$raw_twop), round(g$p_dem_win, 3), round(100 * g$pct_early), round(100 * g$pct_eday), round(100 * g$pct_mail),
      round(g$est_turnout / 1e6, 4), round(g$lo_turnout / 1e6, 4), round(g$hi_turnout / 1e6, 4),
      round(g$pct_votes * g$truth_turnout / 1e6, 4)))
  ))
sims = setNames(lapply(KEYS, function(k) {
  d = read_csv(file.path(DIR_OUT, sprintf("sim_%s_results.csv", k)), show_col_types = FALSE,
               col_types = cols(time = "c", race = "c"))
  mc = if ("model_changes" %in% names(d)) coalesce(as.character(d$model_changes[1]), "") else ""
  sc_diff = diff_text(sc_of(k))
  list(races = night_list(d),
       diff = paste(c(if (nzchar(sc_diff)) paste("Sim:", sc_diff), if (nzchar(mc)) paste("Model:", mc)), collapse = ". "),
       settings = paste0(if ("model" %in% names(d)) paste(d$model[1], "model; ") else "",
                         if ("settings" %in% names(d)) d$settings[1] else ""))
}), sub("__", " · ", KEYS))

# ---- the older TX 2022 backtest, if its results exist -------------------------------
p22 = file.path(DIR_OUT, "backtest_tx22_results.csv")
bt = if (file.exists(p22)) read_csv(p22, show_col_types = FALSE) else NULL
bt_list = if (is.null(bt)) NULL else bt |>
  mutate(key = paste0(if_else(order == "county_size", "county", "random"), if_else(county_effect, "_re", "_nore"))) |>
  split(~key) |>
  lapply(\(g) unname(Map(\(a, b, c, d, e, f, n) list(a, b, c, d, e, f, n),
    r2(g$pct_votes), r2(g$est_twop), r2(g$lo_twop), r2(g$hi_twop), r2(g$raw_twop), round(g$p_dem_win, 3), g$n_counties)))

data = list(sims = sims, bt = bt_list, bt_truth = if (is.null(bt)) NULL else r2(bt$truth_twop[1]),
            run = format(Sys.time(), "%b %d, %Y %H:%M"))
json = jsonlite::toJSON(data, auto_unbox = TRUE, digits = NA, null = "null")

tpl = paste(readLines("scripts/model/tests/model_test_page.html", warn = FALSE), collapse = "\n")
if (!grepl("__DATA__", tpl, fixed = TRUE)) stop("Template is missing the __DATA__ placeholder")
page = paste0("<!doctype html>\n<meta charset=\"utf-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
              sub("__DATA__", json, tpl, fixed = TRUE))
out = file.path(DIR_OUT, "model_tests.html")
writeLines(page, out)
cat("Wrote", out, "with runs:", paste(sub("__", " · ", KEYS), collapse = ", "), "\nOpen it in a browser (double-click the file).\n")
