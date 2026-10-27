# ========================================
## paths.R -- where the shared Dropbox data lives, and small read/write helpers.
## If it fails on your machine, add this line to ~/.Renviron and restart R:
##   CBS_26_GENERAL="/full/path/to/26_general"
# ========================================

find_26_general = function() {
  # PATH_DROPBOX: this session's if set, else what globals.R says for this machine (the
  # PATH_DROPBOX env var, then its per-user list), read without touching the caller's env
  pd = if (exists("PATH_DROPBOX") && nzchar(PATH_DROPBOX)) PATH_DROPBOX else local({
    e = new.env()
    if (file.exists("scripts/util/globals.R"))
      try(suppressMessages(sys.source("scripts/util/globals.R", envir = e)), silent = TRUE)
    if (exists("PATH_DROPBOX", envir = e, inherits = FALSE)) e$PATH_DROPBOX else ""
  })
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
  if (length(cands) == 0) stop('Can\'t find 26_general. Add CBS_26_GENERAL="/path/to/26_general" to ~/.Renviron and restart R.')
  normalizePath(cands[1])
}

# the shared folder that holds 26_general, 24_general, 26_sim, ... (what older scripts
# call PATH_DROPBOX). Scripts that only need this can call it without the helpers below:
#   PATH_DROPBOX = local({ source("scripts/model/live/R/paths.R", local = TRUE); find_dropbox() })
find_dropbox = function() dirname(find_26_general())

read_pq = function(p) {
  if (!file.exists(p)) stop("Missing: ", p)
  if (file.size(p) == 0) stop(p, " is 0 bytes: make the folder available offline in Dropbox")
  tibble::as_tibble(nanoparquet::read_parquet(p))
}
write_pq = function(d, p) nanoparquet::write_parquet(d, p)

# interval from draws: mean +/- z * sd (what enightmodels::cred_int() returns with
# param = TRUE)
cred = function(x, level = 0.95) {
  z = qnorm(1 - (1 - level) / 2)
  c(lower = mean(x) - z * sd(x), upper = mean(x) + z * sd(x))
}
