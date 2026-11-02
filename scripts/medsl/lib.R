##################################################
## Project: MEDSL ENR
## Script purpose: Helpers used only by _targets_medsl.R and automation_medsl.R
## Date: November 2026
## Author: Joseph R. Loffredo
##################################################

# Explicit, not networkenr:::the$networks: `the` is unexported, and a package change should not
# silently reshape the pipeline graph.
MEDSL_NETWORKS = c(
  "ap", "msnow", "nyt", "fox", "politico", "nbc", "cnn", "abc", "cbs", "hill"
)

# The work is network-bound, so this can exceed the core count. The floor keeps the laptop
# matching the CBS pipeline; medsl.mit.edu picks up more on its own.
MEDSL_WORKERS = local({
  n <- suppressWarnings(as.integer(Sys.getenv("MEDSL_WORKERS", "")))
  if (!is.na(n) && n > 0) return(n)
  max(12L, parallel::detectCores(logical = TRUE) - 2L)
})

# create_dirs() without its aws.s3::put_folder() call. That call runs at pipeline parse time,
# so sourcing _targets.R reaches S3 even on a no-upload run.
create_dirs_medsl <- function(meta) {
  create_dir <- function(state) {
    fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw"))
    fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/clean"))
    fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/summaries"))
  }
  purrr::walk(unique(meta$state), create_dir)

  for (net in MEDSL_NETWORKS) {
    fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/networks/{net}/raw"))
    fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/networks/{net}/clean"))
  }

  return(NULL)
}

# write_local() only writes parquet when the path ends in ".csv" (enightdata/R/io.R:25); a
# ".parquet" path with csv = FALSE writes nothing and returns normally.
medsl_write <- function(d, path_local, latest = NULL) {
  if (is.null(d) || nrow(d) == 0) return(FALSE)
  enightdata::write_local(d, path_local, csv = FALSE)
  if (!is.null(latest)) enightdata::write_local(d, latest)
  TRUE
}

# networkenr does no I/O, so the pipeline writes its objects. Returns counts, not the tables,
# to keep the targets store small.
save_network <- function(net, timestamp) {
  res <- networkenr::get_results(net, timestamp = timestamp)
  raw <- tibble::as_tibble(res$raw)
  ts <- attr(res$results, "timestamp")
  if (is.null(ts) || is.na(ts)) ts <- timestamp

  # Aborts on a schema or duplicate-key problem. "No contests returned" only warns, which is
  # the correct state for every network before results publish.
  networkenr::verify_enr(res$results)

  medsl_write(raw, glue("networks/{net}/raw/{net}_{ts}.csv"),
              glue("networks/{net}/raw/{net}_latest.csv"))
  medsl_write(res$results$contests,
              glue("networks/{net}/clean/{net}_contests_{ts}.csv"),
              glue("networks/{net}/clean/{net}_contests_latest.csv"))
  medsl_write(res$results$candidates,
              glue("networks/{net}/clean/{net}_candidates_{ts}.csv"),
              glue("networks/{net}/clean/{net}_candidates_latest.csv"))

  tibble::tibble(
    network = net,
    timestamp = ts,
    entries = nrow(raw),
    errors = if (nrow(raw)) sum(!is.na(raw$error)) else 0L,
    contests = nrow(res$results$contests),
    candidates = nrow(res$results$candidates),
    fetched_at = attr(res$results, "fetched_at")
  )
}

# Install from GitHub when the branch moved since the last install. A clean clone outside
# Dropbox is reset to it, documented (a pushed NAMESPACE can be stale) and installed. Any error
# keeps the installed version.
update_pkg_from_git <- function(pkg, repo, branch, dir) {
  dir <- path.expand(dir)
  stamp <- paste0(dir, ".installed")
  git <- function(..., at = dir) {
    out <- suppressWarnings(system2("git", c("-C", shQuote(at), ...), stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
      stop("git ", paste(c(...), collapse = " "), ": ", paste(out, collapse = "\n"))
    }
    out
  }

  sha <- sub("\t.*", "", git("ls-remote", shQuote(repo), paste0("refs/heads/", branch), at = "."))
  if (!nzchar(sha)) stop("no branch ", branch, " on ", repo)
  if (file.exists(stamp) && readLines(stamp, warn = FALSE)[1] == sha &&
      requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(FALSE))
  }

  if (!dir.exists(file.path(dir, ".git"))) {
    git("clone", "--quiet", shQuote(repo), shQuote(dir), at = ".")
  }
  git("fetch", "--quiet", "origin", branch)
  git("reset", "--hard", "--quiet", sha)
  git("clean", "-fdxq")
  devtools::document(dir)
  renv::install(dir, prompt = FALSE)
  writeLines(sha, stamp)
  message("Installed ", pkg, " ", substr(sha, 1, 7), " from GitHub ", branch)
  invisible(TRUE)
}
