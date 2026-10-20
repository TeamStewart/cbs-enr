## Shared helpers for the model test scripts (test_backtest_tx22.R, test_sim_night.R).
## Run those from the cbs-enr folder; they source this file.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(stringr)
})
source("scripts/model/model_core.R")   # fit_conj_re, draw_beta_re, sig_of, logit, invlogit

# Same Dropbox lookup as prep_tx_model_inputs.R. If it fails, add to ~/.Renviron:
#   CBS_26_GENERAL="/full/path/to/26_general"
find_26_general = function() {
  cands = c(
    Sys.getenv("CBS_26_GENERAL", unset = ""),
    if (exists("PATH_DROPBOX") && nzchar(PATH_DROPBOX)) file.path(PATH_DROPBOX, "26_general"),
    path.expand("~/Dropbox/2024 Election Results/26_general"),
    Sys.glob(path.expand("~/MIT Dropbox/*/CBS-MIT Election Data/26_general")),
    Sys.glob(path.expand("~/Library/CloudStorage/Dropbox*/*/CBS-MIT Election Data/26_general")),
    Sys.glob(path.expand("~/Library/CloudStorage/Dropbox*/*/*/2024 Election Results/26_general")),
    Sys.glob(path.expand("~/Dropbox*/*/CBS-MIT Election Data/26_general"))
  )
  cands = cands[nzchar(cands) & dir.exists(cands)]
  if (length(cands) == 0) stop('Can\'t find 26_general. Add CBS_26_GENERAL="/path/to/26_general" to ~/.Renviron and restart R.')
  normalizePath(cands[1])
}
DIR_26  = find_26_general()
DIR_SIM = file.path(dirname(DIR_26), "26_sim")
DIR_OUT = file.path(DIR_26, "model_tests")
dir.create(DIR_OUT, showWarnings = FALSE)

read_pq = function(p) {
  if (!file.exists(p)) stop("Missing: ", p)
  if (file.size(p) == 0) stop(p, " is 0 bytes: make the folder available offline in Dropbox")
  as_tibble(nanoparquet::read_parquet(p))
}

# 95% interval from draws: mean +/- z * sd, the same thing enightmodels::cred_int()
# returns with param = TRUE (its fit_t() falls back to a normal)
cred = function(x, level = 0.95) {
  z = qnorm(1 - (1 - level) / 2)
  c(lower = mean(x) - z * sd(x), upper = mean(x) + z * sd(x))
}

ICC = 0.302          # county share of residual variance (Young-Kyung's, from the 2020 Senate race)
PRIOR_N = 3          # prior strength in precinct-equivalents, as in tx_backtest.R
