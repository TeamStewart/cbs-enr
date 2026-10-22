# ========================================
## load.R -- load the election-night models. From the cbs-enr folder:
##   source("scripts/model/live/load.R")
## Loads packages and the files in R/. Nothing is read or run at load time.
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(stringr)
})

local({
  here = "scripts/model/live/R"
  if (!dir.exists(here)) stop("Run from the cbs-enr folder (scripts/model/live/R not found from ", getwd(), ")")
  # listed explicitly: model_y.R is a script copy, not functions, and must not be sourced yet
  for (f in c("paths.R", "cells.R", "returns.R", "model_swing.R", "model_cory.R", "run.R", "output.R", "counties.R", "races.R"))
    source(file.path(here, f))
})
