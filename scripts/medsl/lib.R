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

# Name a response by what it actually contains. Content type lies: AP serves .json.gz and
# reports application/x-gzip, and a cbsapi body arrives already parsed.
medsl_raw_ext <- function(body) {
  if (startsWith(body, "{") || startsWith(body, "[")) return("json")
  if (startsWith(body, "<?xml")) return("xml")
  if (startsWith(body, "<")) return("html")
  "txt"
}

# One archive per network per cycle, holding each response's bytes exactly as fetched plus a
# manifest of the request metadata. A body escaped into a table cell was unusable and huge:
# NBC was 146 MB as CSV against ~10 MB here.
medsl_write_raw <- function(raw, root, rel) {
  if (nrow(raw) == 0) return(FALSE)
  dir <- fs::path(tempfile("medsl_raw_"))
  fs::dir_create(dir)
  on.exit(fs::dir_delete(dir), add = TRUE)

  files <- rep(NA_character_, nrow(raw))
  for (i in seq_len(nrow(raw))) {
    body <- raw$body[[i]]
    if (is.na(body) || !nzchar(body)) next
    name <- raw$name[[i]]
    if (is.na(name) || !nzchar(name)) name <- paste0("entry_", i)
    stem <- str_replace_all(name, "[^A-Za-z0-9._-]+", "_")
    files[[i]] <- paste0(stem, ".", medsl_raw_ext(body))
    con <- file(fs::path(dir, files[[i]]), "wb")
    writeBin(charToRaw(body), con)
    close(con)
  }

  # Row-wise rather than apply(), which coerces the frame to a character matrix and would
  # quote status and reformat fetched_at.
  manifest <- raw |>
    dplyr::select(-"body") |>
    dplyr::mutate(file = files)
  lines <- purrr::map_chr(seq_len(nrow(manifest)), \(i) {
    as.character(jsonlite::toJSON(as.list(manifest[i, ]), auto_unbox = TRUE, na = "null"))
  })
  writeLines(lines, fs::path(dir, "manifest.jsonl"))

  archive <- fs::path(root, rel)
  fs::dir_create(fs::path_dir(archive))
  system2("tar", c("-czf", shQuote(archive), "-C", shQuote(dir), "."))
  TRUE
}

# networkenr does no I/O, so the pipeline writes its objects. Returns counts, not the tables,
# to keep the targets store small.
save_network <- function(net, timestamp, root) {
  res <- networkenr::get_results(net, timestamp = timestamp)
  ts <- attr(res$results, "timestamp")
  if (is.null(ts) || is.na(ts)) ts <- timestamp

  raw <- tibble::as_tibble(res$raw)
  medsl_write_raw(raw, root, glue("networks/{net}/raw/{net}_{ts}.tar.gz"))
  medsl_write(res$results$contests,
              glue("networks/{net}/clean/{net}_contests_{ts}.csv"),
              glue("networks/{net}/clean/{net}_contests_latest.csv"))
  medsl_write(res$results$candidates,
              glue("networks/{net}/clean/{net}_candidates_{ts}.csv"),
              glue("networks/{net}/clean/{net}_candidates_latest.csv"))

  # Checked after writing: Fox once served a duplicate candidate mid-update, and aborting
  # here cost the whole cycle's Fox data. "No contests returned" only warns, which is the
  # correct state for every network before results publish.
  problem <- tryCatch(
    {
      networkenr::verify_enr(res$results)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )

  tibble::tibble(
    network = net,
    timestamp = ts,
    entries = nrow(raw),
    errors = if (nrow(raw)) sum(!is.na(raw$error)) else 0L,
    contests = nrow(res$results$contests),
    candidates = nrow(res$results$candidates),
    problem = problem,
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
  # roxygen2 rather than devtools: devtools pulls 43 more packages, 15 of them compiled
  # (shiny, pak, ragg, systemfonts), and the medsl server builds everything from source.
  roxygen2::roxygenise(dir)
  renv::install(dir, prompt = FALSE)
  writeLines(sha, stamp)
  message("Installed ", pkg, " ", substr(sha, 1, 7), " from GitHub ", branch)
  invisible(TRUE)
}
