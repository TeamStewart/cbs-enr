# ========================================
## load.R -- load the election-night models. From the cbs-enr folder:
##   source("scripts/model/live/load.R")
## Loads packages and the files in R/. Nothing is read or run at load time.
# ========================================
# SINGLE-THREADED BLAS, SET BEFORE ANY MATRIX WORK. run_jobs() forks via mclapply, and on
# macOS a forked child inherits an invalid Accelerate/vecLib thread state if the parent has
# already warmed the BLAS thread pool -- which model_medsl does on purpose, since
# medsl_setup() runs in the parent so each worker does not redo it. The child then segfaults
# on its next matrix multiply.
#   MEASURED 2026-10-26: 8 jobs (2 races x 4 models) on 9 cores -> model_medsl dies on BOTH
#   races with `*** caught segfault *** address 0x110, cause 'invalid permissions'` inside
#   draw_beta_re(...) %*% t(cbind(X, CTY_MAT)), reported upstream only as
#   "error: worker failed:" with an empty message. With these set, all 8 jobs pass at 9 and
#   at 2 cores. It does not reproduce at 1 job, which is why calling the model directly
#   never showed it.
# Single-threaded is also what you want regardless: N concurrent workers each running a
# multithreaded BLAS oversubscribes the CPU.
for (v in c("VECLIB_MAXIMUM_THREADS", "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS"))
  if (!nzchar(Sys.getenv(v))) do.call(Sys.setenv, setNames(list("1"), v))

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(stringr)
})

# Shared repo config (paths, ELECTION_DATE, ELECTION_FOLDER), BEFORE paths.R so that
# find_26_general() can use PATH_DROPBOX when a machine sets it. Sourced rather than
# duplicated: races.R used to carry its own ELECTION_DATE, which differed from this one in
# both value and type, and whichever file was sourced second would have won silently.
local({
  g = "scripts/util/globals.R"
  if (file.exists(g)) source(g) else
    cli::cli_alert_warning("{.path {g}} not found -- PATH_DROPBOX / ELECTION_FOLDER unset")
})

local({
  here = "scripts/model/live/R"
  if (!dir.exists(here)) stop("Run from the cbs-enr folder (scripts/model/live/R not found from ", getwd(), ")")
  for (f in c("paths.R", "vote_modes.R", "cells.R", "returns.R", "model_swing.R", "model_cory.R", "model_medsl.R", "model_cpois.R", "model_cprec.R", "run.R", "output.R", "counties.R", "races.R", "cbs_county_pull.R"))
    source(file.path(here, f))
})
