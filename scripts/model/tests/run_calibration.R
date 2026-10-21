# ========================================
## Calibration: how often is each model's interval wrong over many synthetic nights?
##
## 1. makes N synthetic nights (make_calibration_nights.R; existing nights are kept)
## 2. runs model_y and model_simpleswing on each night at 7 checkpoints
##    (8:30, 9:00, 9:30, 10:30pm, midnight, 4am, Nov 10), without the statewide step
## 3. summarises: at each checkpoint, the share of nights where the 50%, 80% and 95% intervals
##    held the true 32-county result, how often the 95% interval was entirely on the wrong
##    side of 50% (confidently wrong), average error and width -- overall and by how strong
##    that night's within-county Hispanic tilt was
## 4. rebuilds model_tests.html, which gets a calibration section
##
## Safe to stop and restart: finished night x model runs are skipped, so rerunning picks up
## where it left off. A run that fails is logged and skipped; the rest continue.
## About 2-3 minutes per night for model_y and 1-2 for model_simpleswing, so 100 nights is
## roughly 6 hours. Leave the computer awake and plugged in.
##
## Run from the cbs-enr folder:
##   RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript scripts/model/tests/run_calibration.R 100
##   ... run_calibration.R 100 --models=swing      # one model only
##   ... run_calibration.R --summary-only          # just re-summarise what has run so far
# ========================================
source("scripts/model/tests/model_test_common.R")
args = commandArgs(trailingOnly = TRUE)
N = as.integer(c(grep("^[0-9]+$", args, value = TRUE), "100")[1])
MODELS = strsplit(sub("^--models=", "", c(grep("^--models=", args, value = TRUE), "--models=trial,swing")[1]), ",")[[1]]
MODELS = sub("^y$", "trial", MODELS)
stopifnot("--models must be trial and/or swing" = all(MODELS %in% c("trial", "swing")))
SUMMARY_ONLY = "--summary-only" %in% args
# check packages up front, so an overnight run doesn't fail on every night
need = c("nanoparquet", "jsonlite", if ("swing" %in% MODELS) "fixest")
miss = need[!vapply(need, requireNamespace, TRUE, quietly = TRUE)]
if (length(miss)) stop("Install first, in R: install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))")
DIR_CAL = file.path(DIR_OUT, "calibration")
DIR_NIGHTS = file.path(DIR_CAL, "nights"); DIR_RES = file.path(DIR_CAL, "results")
dir.create(DIR_RES, showWarnings = FALSE, recursive = TRUE)
log_file = file.path(DIR_CAL, "run_log.txt")
logit_line = function(...) { msg = paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S "), ...); cat(msg, "\n"); cat(msg, "\n", file = log_file, append = TRUE) }

if (!SUMMARY_ONLY) {
  # ---- 1. nights --------------------------------------------------------------------
  st = system2("Rscript", c("scripts/model/tests/make_calibration_nights.R", N))
  if (st != 0) stop("make_calibration_nights.R failed")

  # ---- 2. models ----------------------------------------------------------------------
  jobs = expand.grid(night = seq_len(N), model = MODELS, stringsAsFactors = FALSE)
  jobs$out = file.path(DIR_RES, sprintf("night_%03d__%s.csv", jobs$night, jobs$model))
  todo = jobs[!file.exists(jobs$out), ]
  logit_line(sprintf("calibration: %d nights x %s; %d runs to do, %d already done",
                     N, paste(MODELS, collapse = "+"), nrow(todo), nrow(jobs) - nrow(todo)))
  t0 = Sys.time()
  for (i in seq_len(nrow(todo))) {
    j = todo[i, ]
    t1 = Sys.time()
    st = system2("Rscript", c("scripts/model/tests/test_sim_night.R", paste0("--model=", j$model),
                              paste0("--sim_dir=", file.path(DIR_NIGHTS, sprintf("night_%03d", j$night))),
                              paste0("--out=", j$out), "--checkpoints=calib", "--no_statewide"),
                 stdout = FALSE, stderr = FALSE)
    el = as.numeric(difftime(Sys.time(), t0, units = "mins"))
    logit_line(sprintf("night %03d %-6s %s in %.1f min | %d/%d done, about %.0f min left", j$night, j$model,
                       if (st == 0 && file.exists(j$out)) "ok" else "FAILED",
                       as.numeric(difftime(Sys.time(), t1, units = "mins")), i, nrow(todo), el / i * (nrow(todo) - i)))
  }
}

# ---- 3. summary -------------------------------------------------------------------------
files = list.files(DIR_RES, pattern = "^night_[0-9]+__(trial|swing)\\.csv$", full.names = TRUE)
if (!length(files)) stop("No calibration results yet in ", DIR_RES)
man = lapply(list.files(DIR_NIGHTS, pattern = "^night_[0-9]+$", full.names = TRUE), function(d) {
  f = file.path(d, "manifest.json"); if (!file.exists(f)) return(NULL)
  m = jsonlite::fromJSON(f)
  tibble(night = m$night, tilt_hisp = m$tilt_hisp, leftover = m$leftover_pattern)
}) |> bind_rows()
res = bind_rows(lapply(files, function(f) {
  read_csv(f, show_col_types = FALSE, col_types = cols(time = "c", race = "c", model = "c")) |>
    mutate(night = as.integer(sub("^night_([0-9]+)__.*$", "\\1", basename(f))))
})) |>
  left_join(man, by = "night") |>
  mutate(model = recode(model, trial = "model_y", swing = "model_simpleswing"),
         sd = (hi_twop - lo_twop) / (2 * qnorm(0.975)),
         cov50 = abs(est_twop - truth_twop) <= qnorm(0.75) * sd,
         cov80 = abs(est_twop - truth_twop) <= qnorm(0.90) * sd,
         cov95 = truth_twop >= lo_twop & truth_twop <= hi_twop,
         wrong = (truth_twop > 0.5 & hi_twop < 0.5) | (truth_twop < 0.5 & lo_twop > 0.5),
         err = abs(est_twop - truth_twop), width = hi_twop - lo_twop,
         close = abs(truth_twop - 0.5) < 0.03,
         tilt_bin = cut(tilt_hisp, c(-Inf, -0.05, 0.05, Inf), labels = c("Hispanic tilt toward Rep (< -5)", "little tilt (-5 to +5)", "Hispanic tilt toward Dem (> +5)")))
write_csv(res, file.path(DIR_CAL, "calibration_by_night.csv"))
smry = function(d, ...) d |>
  summarize(race_nights = n(), nights = n_distinct(night), cov50 = mean(cov50), cov80 = mean(cov80), cov95 = mean(cov95),
            wrong = mean(wrong), err = mean(err), width = mean(width), pct_votes = mean(pct_votes), .by = c(model, ...))
out = bind_rows(
  smry(res, time) |> mutate(group = "by checkpoint"),
  smry(res) |> mutate(group = "all checkpoints", time = "all"),
  smry(filter(res, close)) |> mutate(group = "close races (true result within 3 pts of 50%)", time = "all"),
  smry(res, tilt_bin) |> mutate(group = "by Hispanic tilt", time = as.character(tilt_bin)) |> select(-tilt_bin)
)
write_csv(out, file.path(DIR_CAL, "calibration_summary.csv"))
cat("\nCoverage of the 95% interval (target 95%) and confidently-wrong rate, all checkpoints:\n")
print(as.data.frame(smry(res) |> mutate(across(c(cov50, cov80, cov95, wrong), \(x) round(100 * x, 1)),
                                        across(c(err, width), \(x) round(100 * x, 2)))), row.names = FALSE)
cat("\nWrote", file.path(DIR_CAL, "calibration_summary.csv"), "\n")

# ---- 4. page ---------------------------------------------------------------------------
system2("Rscript", "scripts/model/tests/make_model_test_page.R")
