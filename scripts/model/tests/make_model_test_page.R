# ========================================
## Build the model-test page (charts + findings) from the simulated-night results
##
## Only reads results files, so it never reruns the model: rerun it any time to rebuild the page.
## Reads 26_general/model_tests/sim_<scenario>_results.csv for each scenario, and
## sim_<scenario>__<label>_results.csv for runs with other model settings (both from
## test_sim_night.R), and each scenario's 26_sim/<scenario>/manifest.json (to list what
## is different from tx_base). Writes
## one self-contained page, 26_general/model_tests/model_tests.html. Open it in any browser.
## The page layout lives in scripts/model/tests/model_test_page.html; findings at the top
## are computed from the data, so they update with each run.
##
## Run from the cbs-enr folder, after test_sim_night.R:
##   Rscript scripts/model/tests/make_model_test_page.R                  # every scenario with results
##   Rscript scripts/model/tests/make_model_test_page.R tx_base tx_slow  # just these scenarios
##       (with all their labelled runs)
##   Rscript scripts/model/tests/make_model_test_page.R --all-runs
##       also show runs that only change a model setting (e.g. tx_base__nc)
##   Rscript scripts/model/tests/make_model_test_page.R --publish
##       also copy the page to model_tests/index.html in the repo, for GitHub Pages.
##       Commit that file and get it onto main; the page is then at
##       https://teamstewart.github.io/cbs-enr/model_tests/ (if Pages serves main).
# ========================================
source("scripts/model/tests/model_test_common.R")
args = commandArgs(trailingOnly = TRUE)
PUBLISH = "--publish" %in% args
# by default the page shows each scenario's run of each MODEL (model_medsl, model_simpleswing,
# model_cory); runs that only change a model setting (e.g. __nc, __tx24, __stopped) are left
# off unless --all-runs is given. Their results files stay in model_tests/.
ALL_RUNS = "--all-runs" %in% args
args = setdiff(args, c("--publish", "--all-runs"))
# one run = one results file; its key is "<scenario>" or "<scenario>__<label>"
have = sub("^sim_(.*)_results\\.csv$", "\\1", list.files(DIR_OUT, pattern = "^sim_.*_results\\.csv$"))
if (!length(have)) stop("No sim_*_results.csv in ", DIR_OUT, ". Run test_sim_night.R first.")
sc_of = function(k) sub("__.*$", "", k)
want = if (length(args)) args else unique(sc_of(have))
miss = setdiff(want, sc_of(have))
if (length(miss)) stop("No results for: ", paste(miss, collapse = ", "), ". Run test_sim_night.R for them first.")
want = c(intersect("tx_base", want), sort(setdiff(want, "tx_base")))
# each scenario's default run first, then its labelled runs
MODEL_RUNS = c("swing", "cory", "core")   # labels that are models, not setting changes
# model_hybrid (and its retired predecessor model_both) is a prior-re-centring VARIANT of
# model_medsl, not a separate model: its runs (sim_<scenario>__hybrid_results.csv, and the
# single-arm ablations __hyblvl / __hybcomp / __hybnocov) stay out of the default page and
# come back with --all-runs.
have = have[!grepl("__(both|hybrid|hyblvl|hybcomp|hybnocov)$", have)]
# per scenario: model_medsl, then the other models in MODEL_RUNS order, then setting changes
KEYS = unlist(lapply(want, \(sc) {
  k = have[sc_of(have) == sc]
  m = paste0(sc, "__", MODEL_RUNS)
  c(intersect(sc, k), intersect(m, k), sort(setdiff(k, c(sc, m))))
}))
if (!ALL_RUNS) KEYS = KEYS[!grepl("__", KEYS) | sub("^.*?__", "", KEYS) %in% MODEL_RUNS]
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
  f = file.path(DIR_SIM, sc, "manifest.json")
  if (file.exists(f)) {
    m = jsonlite::fromJSON(f, simplifyVector = FALSE)
    # stress scenarios (make_stress_scenario.R) say in words what they changed
    if (!is.null(m$stress)) return(paste0(paste(unlist(m$stress$changes), collapse = "; "), " (made from ", m$stress$from, ")"))
  }
  p = read_params(sc)
  if (is.null(p)) return("no manifest found")
  if (sc == "tx_base" || is.null(base)) return(if (sc == "tx_base") "" else "tx_base manifest not found")
  keys = union(names(p), names(base))
  d = keys[!vapply(keys, \(k) identical(p[[k]], base[[k]]), TRUE)]
  if (!length(d)) return("same settings as tx_base")
  paste(sprintf("%s %s (base %s)", d, vapply(d, \(k) show_val(p[[k]]), ""), vapply(d, \(k) show_val(base[[k]]), "")), collapse = "; ")
}

# ---- statewide estimate: computed in test_sim_night.R (statewide_tx.R); here we only
#      fetch the method's historical check to describe it on the page
state = NULL
p_tp = file.path(DIR_26, "input_data", "model_inputs", "TX_precinct.parquet")
if (file.exists(p_tp)) {
  source("scripts/model/tests/statewide_tx.R")
  ST = statewide_setup(read_pq(p_tp))
  if (!is.null(ST)) state = list(hist = ST$hist, gap_sd_d = ST$gap_sd_d, ratio = ST$ratio, base32 = ST$base32,
                                 rest_join = ST$rest_join)
}
has = function(g, v) v %in% names(g) && any(!is.na(g[[v]]))

# ---- one chart's rows: time, % counted, estimate, lo, hi, raw, P(Dem wins), % early/eday/mail
#      counted; turnout predicted/lo/hi/counted (millions); statewide estimate/lo/hi with
#      demographic swing, then with uniform swing
night_list = function(df) df |>
  split(~race) |>
  lapply(\(g) { st = has(g, "st_est"); list(
    truth = r2(g$truth_twop[1]),
    truth_turn = round(g$truth_turnout[1] / 1e6, 4),
    # turnout in millions: predicted, lo, hi, counted so far
    rows = unname(Map(list,
      substr(g$time, 6, 16), round(100 * g$pct_votes, 1), r2(g$est_twop), r2(g$lo_twop), r2(g$hi_twop),
      r2(g$raw_twop), round(g$p_dem_win, 3), round(100 * g$pct_early), round(100 * g$pct_eday), round(100 * g$pct_mail),
      round(g$est_turnout / 1e6, 4), round(g$lo_turnout / 1e6, 4), round(g$hi_turnout / 1e6, 4),
      round(g$pct_votes * g$truth_turnout / 1e6, 4),
      if (st) r2(g$st_est) else NA, if (st) r2(g$st_lo) else NA, if (st) r2(g$st_hi) else NA,
      if (st) r2(g$stu_est) else NA, if (st) r2(g$stu_lo) else NA, if (st) r2(g$stu_hi) else NA)),
    st_exact = if (st) r2(g$st_exact[1]) else NULL, stu_exact = if (st) r2(g$stu_exact[1]) else NULL
  ) })
# display names: model_medsl = the full precinct model (model_turnout.R), model_simpleswing =
# the standard uniform swing model, model_cory = enightmodels::model_legacy(),
# model_core = the simpler model_core.R version
MODEL_NAMES = c(trial = "model_medsl", model_y = "model_medsl", model_medsl = "model_medsl", swing = "model_simpleswing",
                model_simpleswing = "model_simpleswing", core = "model_core", model_core = "model_core",
                cory = "model_cory", model_cory = "model_cory")
model_of = function(d) {
  m = if ("model" %in% names(d)) as.character(d$model[1]) else "trial"
  coalesce(unname(MODEL_NAMES[m]), m)
}
run_name = function(k, d) {
  m = model_of(d)
  lab = if (grepl("__", k)) sub("^.*?__", "", k) else ""
  paste(c(sc_of(k), m, if (nzchar(lab) && !lab %in% c("swing", "core", "cory")) lab), collapse = " · ")
}
NAMES = character()
sims = setNames(lapply(KEYS, function(k) {
  d = read_csv(file.path(DIR_OUT, sprintf("sim_%s_results.csv", k)), show_col_types = FALSE,
               col_types = cols(time = "c", race = "c"))
  NAMES[[k]] <<- run_name(k, d)
  mc = if ("model_changes" %in% names(d)) coalesce(as.character(d$model_changes[1]), "") else ""
  sc_diff = diff_text(sc_of(k))
  lab = if (grepl("__", k)) sub("^.*?__", "", k) else ""
  list(races = night_list(d), scenario = sc_of(k), model = model_of(d),
       variant = if (nzchar(lab) && !lab %in% MODEL_RUNS) lab else "",
       diff = paste(c(if (nzchar(sc_diff)) paste("Sim:", sc_diff), if (nzchar(mc)) paste("Model:", mc)), collapse = ". "),
       settings = if ("settings" %in% names(d)) d$settings[1] else "")
}), KEYS)
names(sims) = NAMES[KEYS]

# ---- calibration over many synthetic nights (run_calibration.R), if it has run ----------
p_cal = file.path(DIR_OUT, "calibration", "calibration_summary.csv")
calib = if (file.exists(p_cal)) {
  cs = read_csv(p_cal, show_col_types = FALSE, col_types = cols(time = "c", group = "c", model = "c"))
  list(rows = unname(Map(\(g, t, m, n, rn, c50, c80, c95, w, e, wd, pv) list(g = g, t = t, m = m, n = n, rn = rn, c50 = c50, c80 = c80, c95 = c95, w = w, e = e, wd = wd, pv = pv),
                         cs$group, cs$time, cs$model, cs$nights, cs$race_nights, round(100 * cs$cov50, 1), round(100 * cs$cov80, 1),
                         round(100 * cs$cov95, 1), round(100 * cs$wrong, 1), round(100 * cs$err, 2), round(100 * cs$width, 2), round(100 * cs$pct_votes))),
       updated = format(file.mtime(p_cal), "%b %d, %Y %H:%M"))
} else NULL

# ---- statewide test nights (live/tests/sim_statewide.R), if any have run ----------------
# SIM_statewide_<scenario>[_offset<x>][_<race>].csv: the models' statewide rows (scraped
# counties + simulated CBS counties) and scraped-only rows, with the statewide truth
sw_files = list.files(DIR_OUT, pattern = "^SIM_statewide_.*\\.csv$", full.names = TRUE)
statewide = if (length(sw_files)) {
  sw = bind_rows(lapply(sw_files, function(f)
    read_csv(f, show_col_types = FALSE, col_types = cols(time = "c", .default = col_guess())) |>
      mutate(key = sub("_(gov|sen)$", "", sub("^SIM_statewide_(.*)\\.csv$", "\\1", basename(f)))))) |>
    filter(pred_type == "share", party == "dem") |>
    distinct(key, contest, model, geo, time, .keep_all = TRUE) |>
    mutate(t = format(as.POSIXct(time, tz = "America/New_York"), "%m-%d %H:%M"))
  late = function(t) t > "11-04 04:00"
  phase = function(t) case_when(t < "11-03 21:00" ~ "8-9pm", t < "11-03 23:00" ~ "9-11pm", !late(t) ~ "11pm-4am", TRUE ~ "Nov 4-10")
  r2 = function(x) round(100 * x, 2)
  scen = lapply(split(sw, sw$key), function(k) {
    off = unique(k$rest_offset)
    list(label = if (grepl("_offset", k$key[1])) sprintf("stress: rest of state %+.2f logit", as.numeric(sub("^.*_offset", "", k$key[1])))
                 else "base", 
         races = lapply(split(k, k$contest), function(r) {
           list(truth = r2(r$truth[r$geo == "all"][1]), truth32 = r2(r$truth[r$geo == "scraped"][1]),
                offset = round(r$rest_offset[1], 3),
                models = lapply(split(r, r$model), function(m) {
                  a = filter(m, geo == "all") |> arrange(time); s32 = filter(m, geo == "scraped")
                  a = left_join(a, select(s32, time, e32 = pred, lo32 = pred_min, hi32 = pred_max), by = "time")
                  unname(Map(\(t, pc, e, lo, hi, e32, lo32, hi32) list(t, pc, e, lo, hi, e32, lo32, hi32),
                             a$t, round(100 * a$pct_counted, 1), r2(a$pred), r2(a$pred_min), r2(a$pred_max),
                             r2(a$e32), r2(a$lo32), r2(a$hi32)))
                }))
         }))
  })
  summ = sw |>
    mutate(phase = phase(t), err = 100 * abs(pred - truth), width = 100 * (pred_max - pred_min)) |>
    summarize(n = n(), mae = round(mean(err), 2), width = round(mean(width), 2),
              cov = if (all(is.na(covers))) NA_real_ else round(100 * mean(covers, na.rm = TRUE)),
              pct = round(100 * mean(pct_counted)), .by = c(key, contest, model, geo, phase)) |>
    mutate(phase = factor(phase, c("8-9pm", "9-11pm", "11pm-4am", "Nov 4-10"))) |>
    arrange(key, contest, model, desc(geo), phase) |> mutate(phase = as.character(phase))
  list(scen = scen, summ = summ, updated = format(max(file.mtime(sw_files)), "%b %d, %Y %H:%M"))
} else NULL

data = list(sims = sims, state = state, calib = calib, statewide = statewide,
            run = format(Sys.time(), "%b %d, %Y %H:%M"))
json = jsonlite::toJSON(data, auto_unbox = TRUE, digits = NA, null = "null", na = "null")

tpl = paste(readLines("scripts/model/tests/model_test_page.html", warn = FALSE), collapse = "\n")
if (!grepl("__DATA__", tpl, fixed = TRUE)) stop("Template is missing the __DATA__ placeholder")
page = paste0("<!doctype html>\n<meta charset=\"utf-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
              sub("__DATA__", json, tpl, fixed = TRUE))
out = file.path(DIR_OUT, "model_tests.html")
writeLines(page, out)
if (PUBLISH) {
  dir.create("model_tests", showWarnings = FALSE)
  file.copy(out, "model_tests/index.html", overwrite = TRUE)
  cat("Copied to model_tests/index.html for GitHub Pages: commit it and merge to main.\n")
}
cat("Wrote", out, "with runs:", paste(NAMES[KEYS], collapse = ", "), "\nOpen it in a browser (double-click the file).\n")
