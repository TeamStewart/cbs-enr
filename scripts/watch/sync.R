# ========================================
## Watcher: fetch the scraper laptop's pushed branches (B1).
## pipeline-state (targets meta, cycle logs, cycle.json, enightdata_sha) and dashboard-live
## (dashboard_data.rds) are force-pushed by automation.R each cycle. They are fetched into a
## side repo at $WATCH_HOME/repo (fetch only, never pushed from) and unpacked into
## $WATCH_HOME/pipeline-state/ and $WATCH_HOME/dashboard-live/.
## Needs lib.R sourced first.
# ========================================

SYNC_BRANCHES = c("pipeline-state", "dashboard-live")

sync_git = function(..., dir = watch_path("repo")) {
  out = suppressWarnings(system2("git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    stop("git ", paste(c(...), collapse = " "), ": ", paste(out, collapse = "\n"))
  }
  out
}

# Unpack a fetched branch into `dest`, replacing what is there. It is extracted next to
# `dest` and renamed into place, so a reader never sees a half-written folder.
materialize_branch = function(branch, dest) {
  tmp = paste0(dest, ".new")
  old = paste0(dest, ".old")
  unlink(c(tmp, old), recursive = TRUE)
  dir.create(tmp, recursive = TRUE)
  tarball = tempfile(fileext = ".tar")
  on.exit(unlink(tarball), add = TRUE)
  sync_git("archive", "--format=tar", "-o", shQuote(tarball), paste0("refs/remotes/origin/", branch))
  utils::untar(tarball, exdir = tmp)
  if (dir.exists(dest) && !file.rename(dest, old)) stop("could not move ", dest, " aside")
  if (!file.rename(tmp, dest)) {
    if (dir.exists(old)) file.rename(old, dest)
    stop("could not rename ", tmp, " to ", dest)
  }
  unlink(old, recursive = TRUE)
  invisible(dest)
}

# Fetch both branches and unpack them. Never throws: a failure returns ok = FALSE and the
# error, and the previous unpacked copies stay in place. `changed` is TRUE when the
# pipeline-state commit differs from the last successful sync.
sync_pipeline_state = function() {
  res = list(ok = FALSE, run_time = NA_character_, commit_time = as.POSIXct(NA), changed = FALSE,
             sha = NA_character_, dashboard_ok = FALSE, error = NULL)
  tryCatch({
    repo = watch_path("repo")
    if (!dir.exists(file.path(repo, ".git"))) {
      url = system2("git", c("-C", shQuote(WATCH_REPO), "remote", "get-url", "origin"), stdout = TRUE)
      dir.create(repo, recursive = TRUE, showWarnings = FALSE)
      sync_git("init", "--quiet")
      sync_git("remote", "add", "origin", shQuote(url))
    }
    refspecs = paste0("+refs/heads/", SYNC_BRANCHES, ":refs/remotes/origin/", SYNC_BRANCHES)
    # dashboard-live is fetched separately so a missing or broken one never blocks the
    # pipeline state
    sync_git("fetch", "--quiet", "--no-tags", "origin", refspecs[1])
    res$dashboard_ok = tryCatch({
      sync_git("fetch", "--quiet", "--no-tags", "origin", refspecs[2])
      materialize_branch("dashboard-live", watch_path("dashboard-live"))
      TRUE
    }, error = function(e) {
      res$error <<- paste("dashboard-live:", conditionMessage(e))
      FALSE
    })

    ref = "refs/remotes/origin/pipeline-state"
    sha = sync_git("rev-parse", ref)[1]
    res$sha = sha
    res$commit_time = as.POSIXct(sync_git("log", "-1", "--format=%ct", ref)[1] |> as.numeric(),
                                 origin = "1970-01-01", tz = WATCH_TZ)
    last = read_json_file(state_path("sync.json"), list())
    res$changed = !identical(last$sha, sha)
    if (res$changed || !dir.exists(watch_path("pipeline-state"))) {
      materialize_branch("pipeline-state", watch_path("pipeline-state"))
    }
    cyc = read_json_file(watch_path("pipeline-state", "cycle.json"), list())
    res$run_time = cyc$run_time %||% NA_character_
    write_json_file(list(sha = sha, at = fmt_time(), run_time = res$run_time), state_path("sync.json"))
    res$ok = TRUE
  }, error = function(e) {
    res$error <<- conditionMessage(e)
  })
  res
}
