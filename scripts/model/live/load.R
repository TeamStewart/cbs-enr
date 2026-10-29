# ========================================
## load.R -- load the election-night models. From the cbs-enr folder:
##   source("scripts/model/live/load.R")
## Loads packages and the files in R/. Nothing is read or run at load time.
# ========================================
# Single-threaded BLAS, set before any matrix work. run_jobs() forks via mclapply, and on
# macOS a forked child segfaults on its next matrix multiply if the parent has already
# warmed the BLAS thread pool (medsl_setup() does, in the parent). Many workers each running
# a multithreaded BLAS would oversubscribe the CPU anyway.
for (v in c("VECLIB_MAXIMUM_THREADS", "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS")) {
  if (!nzchar(Sys.getenv(v))) do.call(Sys.setenv, setNames(list("1"), v))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
})

# Shared repo config (paths, ELECTION_DATE, ELECTION_FOLDER), before paths.R so that
# find_26_general() can use PATH_DROPBOX when a machine sets it.
local({
  g = "scripts/util/globals.R"
  if (file.exists(g)) {
    source(g)
  } else {
    cli::cli_alert_warning("{.path {g}} not found -- PATH_DROPBOX / ELECTION_FOLDER unset")
  }
})

local({
  here = "scripts/model/live/R"
  if (!dir.exists(here)) {
    stop("Run from the cbs-enr folder (scripts/model/live/R not found from ", getwd(), ")")
  }
  for (f in c(
    "paths.R",
    "vote_modes.R",
    "cells.R",
    "returns.R",
    "demos.R",
    "results.R",
    "model_swing.R",
    "model_ratio_strata.R",
    "model_medsl.R",
    "model_lm_bayes.R",
    "model_pois_bayes.R",
    "run.R",
    "config.R",
    "output.R",
    "counties.R",
    "virtual.R",
    "races.R",
    "cbs_county_pull.R"
  )) {
    source(file.path(here, f))
  }
})
