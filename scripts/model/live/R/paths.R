# ========================================
## paths.R -- where the shared Dropbox data lives, and small read/write helpers.
## Same lookup as scripts/model/tests/model_test_common.R and prep_tx_model_inputs.R.
## If it fails on your machine, add this line to ~/.Renviron and restart R:
##   CBS_26_GENERAL="/full/path/to/26_general"
# ========================================

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

read_pq = function(p) {
  if (!file.exists(p)) stop("Missing: ", p)
  if (file.size(p) == 0) stop(p, " is 0 bytes: make the folder available offline in Dropbox")
  tibble::as_tibble(nanoparquet::read_parquet(p))
}
write_pq = function(d, p) nanoparquet::write_parquet(d, p)

# interval from draws: mean +/- z * sd (same as cred() in model_test_common.R, and what
# enightmodels::cred_int() returns with param = TRUE)
cred = function(x, level = 0.95) {
  z = qnorm(1 - (1 - level) / 2)
  c(lower = mean(x) - z * sd(x), upper = mean(x) + z * sd(x))
}
