# ========================================
## paths.R -- where the shared Dropbox data lives, and small read/write helpers.
## If it fails on your machine, add this line to ~/.Renviron and restart R:
##   CBS_26_GENERAL="/full/path/to/26_general"
# ========================================

#' Find the shared 26_general folder
#'
#' Looks in order at the `CBS_26_GENERAL` environment variable, `PATH_DROPBOX` (this
#' session's, else what scripts/util/globals.R sets for this machine, read without touching
#' the caller's environment), then the usual Dropbox install locations.
#'
#' @return Normalized path to `26_general`. Stops if none exists; the fix is to add
#'   `CBS_26_GENERAL="/path/to/26_general"` to ~/.Renviron.
find_26_general = function() {
  # PATH_DROPBOX: this session's if set, else what globals.R says for this machine (the
  # PATH_DROPBOX env var, then its per-user list), read without touching the caller's env
  pd = if (exists("PATH_DROPBOX") && nzchar(PATH_DROPBOX)) {
    PATH_DROPBOX
  } else {
    local({
      e = new.env()
      if (file.exists("scripts/util/globals.R")) {
        try(suppressMessages(sys.source("scripts/util/globals.R", envir = e)), silent = TRUE)
      }
      if (exists("PATH_DROPBOX", envir = e, inherits = FALSE)) e$PATH_DROPBOX else ""
    })
  }
  cands = c(
    Sys.getenv("CBS_26_GENERAL", unset = ""),
    if (nzchar(pd)) path.expand(file.path(pd, "26_general")),
    path.expand("~/Dropbox/2024 Election Results/26_general"),
    Sys.glob(path.expand("~/MIT Dropbox/*/CBS-MIT Election Data/26_general")),
    Sys.glob(path.expand("~/Library/CloudStorage/Dropbox*/*/CBS-MIT Election Data/26_general")),
    Sys.glob(path.expand("~/Library/CloudStorage/Dropbox*/*/*/2024 Election Results/26_general")),
    Sys.glob(path.expand("~/Dropbox*/*/CBS-MIT Election Data/26_general"))
  )
  cands = cands[nzchar(cands) & dir.exists(cands)]
  if (length(cands) == 0) {
    stop(
      'Can\'t find 26_general. Add CBS_26_GENERAL="/path/to/26_general" to ~/.Renviron and restart R.'
    )
  }
  normalizePath(cands[1])
}

#' Find the shared Dropbox folder
#'
#' The folder that holds 26_general, 24_general, 26_sim, ... (what older scripts call
#' `PATH_DROPBOX`). Scripts that need only this can call it without the other helpers:
#' `PATH_DROPBOX = local({ source("scripts/model/live/R/paths.R", local = TRUE); find_dropbox() })`.
#'
#' @return Path to the parent folder of `26_general`.
find_dropbox = function() dirname(find_26_general())

#' Read a parquet file as a tibble
#'
#' @param p Path to the file.
#' @return A tibble. Stops if the file is missing or 0 bytes (an online-only Dropbox file).
read_pq = function(p) {
  if (!file.exists(p)) {
    stop("Missing: ", p)
  }
  if (file.size(p) == 0) {
    stop(p, " is 0 bytes: make the folder available offline in Dropbox")
  }
  tibble::as_tibble(nanoparquet::read_parquet(p))
}

#' Write a data frame to parquet
#'
#' @param d Data frame.
#' @param p Output path.
#' @return Called for its side effect.
write_pq = function(d, p) nanoparquet::write_parquet(d, p)

#' Normal-approximation interval from draws
#'
#' Mean +/- z * sd, as `enightmodels::cred_int()` returns with `param = TRUE`.
#'
#' @param x Numeric vector of draws.
#' @param level Interval level.
#' @return Named numeric vector `c(lower, upper)`.
cred = function(x, level = 0.95) {
  z = qnorm(1 - (1 - level) / 2)
  c(lower = mean(x) - z * sd(x), upper = mean(x) + z * sd(x))
}
