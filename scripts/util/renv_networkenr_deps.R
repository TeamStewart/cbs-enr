# networkenr is in renv ignored.packages (each machine installs its own checkout), so
# renv::snapshot() does not follow its dependencies. This file lists networkenr's Imports so
# the lockfile keeps them. Never sourced; keep in sync with its DESCRIPTION.
if (FALSE) {
  library(cbsapi)
  library(cli)
  library(dplyr)
  library(glue)
  library(httr2)
  library(jsonlite)
  library(lubridate)
  library(purrr)
  library(rlang)
  library(stringr)
  library(tibble)
  library(tidyr)
}
