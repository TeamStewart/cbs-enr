##################################################
## Project: CBS ENR
## Script purpose: Automate tar_make()
## Date: March 2024
## Author: Joseph R. Loffredo and Mason Reece
##################################################

INTERVAL = 5 # minutes

# Production settings. A rehearsal leftover (e.g. CBS_MODEL_SIM) would put test numbers on the
# live dashboard, so refuse to start unless CBS_AUTOMATION_TEST=1.
local({
  bad = c(
    CBS_MODEL_SIM = Sys.getenv("CBS_MODEL_SIM") != "",
    CBS_ELECTION_FOLDER = !Sys.getenv("CBS_ELECTION_FOLDER") %in% c("", "26_general"),
    CBS_MODEL_CBS_ENV = !Sys.getenv("CBS_MODEL_CBS_ENV") %in% c("", "live")
  )
  if (any(bad) && Sys.getenv("CBS_AUTOMATION_TEST") != "1") {
    stop("Not production settings: ", paste(names(bad)[bad], collapse = ", "),
         ". Unset them, or set CBS_AUTOMATION_TEST=1 for a rehearsal.")
  }
})
# tar_make() (and _targets.R, which reads these) runs in a callr process that inherits them:
# skip the in-pipeline dashboard render (mit.html is rendered below) and the local-checkout
# enightdata install (update_enightdata() installs from GitHub instead)
Sys.setenv(CBS_DASH_RENDER = "FALSE", CBS_AUTOMATION = "1")

# One Google token for both: Drive's scope covers Sheets. A non-interactive Rscript can't open
# the browser, so cache it once first:
#   Rscript -e 'options(rlang_interactive = TRUE); googledrive::drive_auth()'
googledrive::drive_auth(email = TRUE)
googlesheets4::gs4_auth(token = googledrive::drive_token())

# The published dashboard's data: tar_make() writes it here (dashboard_export in _targets.R)
DASH_DATA = "data/dashboard/live/dashboard_data.rds"
# Branch the data is pushed to. It is one commit with only dashboard_data.rds, replaced by a
# force-push each run, so neither main's history nor the Dropbox copy of .git grows over the
# night, and the automation never has to merge with teammates' pushes to main.
DASH_BRANCH = "dashboard-live"
# Side repo the data branch is built in: outside Dropbox, so its objects never sync
DASH_PUSH_DIR = path.expand(Sys.getenv("CBS_DASH_PUSH_DIR", "~/.cache/cbs-enr-dashboard-live"))
# CBS copy of the same page, rendered here from DASH_DATA and served by CloudFront
# (https://dqvla57lr7v2z.cloudfront.net/20261103-US-G/mit.html). The page fetches precinct
# geometry relative to itself, so the geo folder sits next to it in the bucket.
DASH_S3_BUCKET = "cbsn-elections-external-models" # PATH_CBS_S3 in scripts/util/globals.R
DASH_S3_DIR = "20261103-US-G"
DASH_S3_HTML = "data/dashboard/live/mit.html"
# What the watcher on another laptop reads (docs/watcher_plan.md): targets metadata (not the
# objects) and the last cycles' logs, force-pushed each run like DASH_BRANCH
STATE_BRANCH = "pipeline-state"
STATE_LOGS = 24 # cycles of logs/automation_*.log kept on the branch
# enightdata comes from GitHub, never from anyone's working copy: teammates push scraper fixes
# to ENIGHTDATA_BRANCH. A clean clone outside Dropbox is reset to it and installed when it moves.
ENIGHTDATA_REPO = Sys.getenv("CBS_ENIGHTDATA_REPO", "git@github.com:TeamStewart/enightdata.git")
ENIGHTDATA_BRANCH = Sys.getenv("CBS_ENIGHTDATA_BRANCH", "main")
ENIGHTDATA_DIR = path.expand(Sys.getenv("CBS_ENIGHTDATA_DIR", "~/.cache/cbs-enr-enightdata"))
ENIGHTDATA_STAMP = paste0(ENIGHTDATA_DIR, ".installed") # sha of the installed commit

# Function to execute the R script
execute_script <- function() {
  message(sprintf("%s: Running tar_make", format(Sys.time(), format="%Y-%m-%d %H:%M:%S")))
  targets::tar_make()
}

git <- function(..., dir = DASH_PUSH_DIR, input = NULL, env = character()) {
  out = suppressWarnings(system2("git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = TRUE, input = input, env = env))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    stop("git ", paste(c(...), collapse = " "), ": ", paste(out, collapse = "\n"))
  }
  out
}

# Install enightdata from GitHub when ENIGHTDATA_BRANCH moved since the last install: reset the clean
# clone to it, document (a pushed NAMESPACE can be stale) and install. Any error keeps the
# installed version.
update_enightdata <- function() {
  sha = sub("\t.*", "", git("ls-remote", shQuote(ENIGHTDATA_REPO), paste0("refs/heads/", ENIGHTDATA_BRANCH), dir = "."))
  if (file.exists(ENIGHTDATA_STAMP) && readLines(ENIGHTDATA_STAMP, warn = FALSE)[1] == sha &&
      requireNamespace("enightdata", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  if (!dir.exists(file.path(ENIGHTDATA_DIR, ".git"))) {
    git("clone", "--quiet", shQuote(ENIGHTDATA_REPO), shQuote(ENIGHTDATA_DIR), dir = ".")
  }
  if (!nzchar(sha)) stop("no branch ", ENIGHTDATA_BRANCH, " on ", ENIGHTDATA_REPO)
  git("fetch", "--quiet", "origin", ENIGHTDATA_BRANCH, dir = ENIGHTDATA_DIR)
  git("reset", "--hard", "--quiet", sha, dir = ENIGHTDATA_DIR)
  git("clean", "-fdxq", dir = ENIGHTDATA_DIR)
  devtools::document(ENIGHTDATA_DIR)
  renv::install(ENIGHTDATA_DIR, prompt = FALSE)
  writeLines(sha, ENIGHTDATA_STAMP)
  message("Installed enightdata ", substr(sha, 1, 7), " from GitHub ", ENIGHTDATA_BRANCH)
  invisible(TRUE)
}

# Force-push `files` as the only contents of `branch`: one commit, replaced each run, so the
# branch never grows. Names of `files` are their paths on the branch, values the local files.
push_files <- function(files, branch, msg) {
  if (!dir.exists(file.path(DASH_PUSH_DIR, ".git"))) {
    fs::dir_create(DASH_PUSH_DIR)
    git("init", "--quiet")
    git("remote", "add", "origin", system2("git", c("remote", "get-url", "origin"), stdout = TRUE))
  }
  # a throwaway index, so the branch can hold nested paths
  idx = paste0("GIT_INDEX_FILE=", shQuote(file.path(DASH_PUSH_DIR, ".git", paste0("index-", branch))))
  git("read-tree", "--empty", env = idx)
  for (f in names(files)) {
    blob = git("hash-object", "-w", shQuote(normalizePath(files[[f]])))
    git("update-index", "--add", "--cacheinfo", shQuote(paste0("100644,", blob, ",", f)), env = idx)
  }
  tree = git("write-tree", env = idx)
  commit = git("-c", "user.name='cbs-enr automation'", "-c", "user.email=noreply@github.com",
               "commit-tree", tree, "-m", shQuote(msg))
  git("push", "--force", "--quiet", "origin", paste0(commit, ":refs/heads/", branch))
  git("gc", "--prune=now", "--quiet") # drop the previous runs' blobs
  invisible(TRUE)
}

# Force-push DASH_DATA as the only file on DASH_BRANCH, then ask GitHub Actions to render
# and deploy the page from it (.github/workflows/pages.yml)
push_dashboard_data <- function(run_time) {
  if (!file.exists(DASH_DATA)) {
    message("No ", DASH_DATA, " (did dashboard_export build?); nothing pushed")
    return(invisible(FALSE))
  }
  push_files(c(dashboard_data.rds = DASH_DATA), DASH_BRANCH, glue::glue("dashboard data {run_time}"))
  gh = suppressWarnings(system2(
    "gh", c("workflow", "run", "pages.yml", "-R", "TeamStewart/cbs-enr", "--ref", "main", "-f", "data=live"),
    stdout = TRUE, stderr = TRUE
  ))
  if (!is.null(attr(gh, "status")) && attr(gh, "status") != 0) {
    message("Pushed the data but could not start the Pages build (gh): ", paste(gh, collapse = "\n"))
  }
  invisible(TRUE)
}

# Render index.qmd from rds (default DASH_DATA, the data GitHub Pages renders from) and upload it to
# DASH_S3_DIR/mit.html. As in 2024/2025: one self-contained file, text/html, and no-cache
# so CloudFront serves each new upload.
upload_dashboard_s3 <- function(rds = DASH_DATA) {
  if (!file.exists(rds)) return(invisible(FALSE))
  out = suppressWarnings(system2(
    "quarto", c("render", "index.qmd", "--output", "mit.html"),
    env = paste0("CBS_DASH_RDS=", shQuote(normalizePath(rds))), stdout = TRUE, stderr = TRUE
  ))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    stop("quarto render: ", paste(tail(out, 20), collapse = "\n"))
  }
  file.rename("mit.html", DASH_S3_HTML)
  aws.s3::put_object(
    file = DASH_S3_HTML,
    object = file.path(DASH_S3_DIR, "mit.html"),
    bucket = DASH_S3_BUCKET,
    multipart = TRUE,
    headers = list("Content-Type" = "text/html", "Cache-Control" = "no-cache, max-age=0")
  )
}

# Precinct maps' geometry (data/dashboard/geo, static for the night), next to mit.html
upload_dashboard_geo <- function(dir = "data/dashboard/geo") {
  for (f in list.files(dir, pattern = "\\.json$", full.names = TRUE)) {
    aws.s3::put_object(
      file = f,
      object = file.path(DASH_S3_DIR, f),
      bucket = DASH_S3_BUCKET,
      headers = list("Content-Type" = "application/json")
    )
  }
}

# STATE_BRANCH: targets metadata, the last STATE_LOGS cycle logs, logs/console.log (when
# automation.R is started with `>> logs/console.log 2>&1`), the installed enightdata sha and
# this cycle's step results (cycle.json)
# Approved watcher commands (scripts/watch/apply.R): branch CMD_BRANCH holds commands.json, a
# list of {id, action = "invalidate", targets}. Each id runs once on this laptop, between
# tar_make() runs, and only for jurisdiction targets. Returns the ids run this cycle.
CMD_BRANCH = "pipeline-commands"
CMD_DONE = path.expand("~/.cache/cbs-enr-commands.done")
CMD_TARGET_RX = "^(timestamp|data|tbl_cbs|summary)_[A-Z]{2}_"

run_watch_commands <- function() {
  heads = git("ls-remote", "origin", paste0("refs/heads/", CMD_BRANCH))
  if (!length(heads) || !nzchar(heads[1])) return(character())
  git("fetch", "--quiet", "origin", CMD_BRANCH)
  cmds = jsonlite::fromJSON(paste(git("show", "FETCH_HEAD:commands.json"), collapse = "\n"), simplifyVector = FALSE)
  done = if (file.exists(CMD_DONE)) readLines(CMD_DONE, warn = FALSE) else character()
  ran = character()
  for (cmd in cmds) {
    if (cmd$id %in% done || !identical(cmd$action, "invalidate")) next
    tg = unlist(cmd$targets)
    tg = tg[grepl(CMD_TARGET_RX, tg)]
    if (length(tg)) targets::tar_invalidate(names = dplyr::any_of(tg))
    message("Watcher command ", cmd$id, " (by ", cmd$by, "): invalidated ", paste(tg, collapse = ", "))
    cat(cmd$id, "\n", sep = "", file = CMD_DONE, append = TRUE)
    ran = c(ran, cmd$id)
  }
  ran
}

push_pipeline_state <- function(run_time, steps) {
  meta = file.path("_targets", "meta", c("meta", "progress", "process"))
  logs = tail(sort(list.files("logs", "^automation_.*\\.log$", full.names = TRUE)), STATE_LOGS)
  extra = c("logs/console.log", ENIGHTDATA_STAMP)
  cycle = tempfile(fileext = ".json")
  # free space on the volume holding the repo, for the watcher's disk check
  df = suppressWarnings(system2("df", c("-k", "."), stdout = TRUE))
  free_gb = round(as.numeric(strsplit(trimws(df[length(df)]), "[[:space:]]+")[[1]][4]) / 1024^2, 1)
  jsonlite::write_json(c(list(run_time = run_time, finished = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                              disk_free_gb = free_gb, commands_done = as.list(COMMANDS_DONE)), steps),
                       cycle, auto_unbox = TRUE)
  files = c(meta, logs, extra)
  files = setNames(files, replace(files, files == ENIGHTDATA_STAMP, "enightdata_sha"))
  files = files[file.exists(files)]
  push_files(c(files, cycle.json = cycle), STATE_BRANCH, glue::glue("pipeline state {run_time}"))
}

tryCatch(upload_dashboard_geo(), error = function(e) message("Dashboard geo upload failed: ", conditionMessage(e)))

# Main loop to schedule and execute the script every INTERVAL minutes
while (TRUE) {
  print(glue::glue(
    "Started at {format(Sys.time(), format='%Y-%m-%d %H:%M:%S')}, waiting..."
  ))

  start_time <- Sys.time()

  run_time <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%S") |>
    stringr::str_replace_all("-|:| ", "_")

  fs::dir_create("logs/")

  # each step: TRUE, or FALSE after logging why; nothing here may stop the loop
  step <- function(what, expr) {
    tryCatch({ force(expr); TRUE }, error = function(e) { message(what, " failed: ", conditionMessage(e)); FALSE })
  }

  con <- file(glue::glue("logs/automation_{run_time}.log"))
  sink(con, append = TRUE)
  sink(con, append = TRUE, type = "message")

  # this laptop's dead-man ping (healthchecks.io), for when the watcher laptop can't see it
  if (nzchar(Sys.getenv("CBS_HEALTHCHECK_URL"))) {
    try(system2("curl", c("-fsS", "-m", "10", "-o", "/dev/null", shQuote(Sys.getenv("CBS_HEALTHCHECK_URL")))), silent = TRUE)
  }
  COMMANDS_DONE = character()
  steps = list(
    enightdata = step("enightdata update (kept the installed version)", update_enightdata()),
    commands = step("Watcher commands", COMMANDS_DONE <- run_watch_commands()),
    tar_make = step("tar_make", execute_script())
  )

  # Restore output to console
  sink()
  sink(type = "message")
  close(con)

  steps$dashboard_push = step("Dashboard push", push_dashboard_data(run_time))
  steps$dashboard_s3 = step("Dashboard S3 upload", upload_dashboard_s3())

  # read here, used by the next cycle's tar_make(); a failed read keeps the last metadata.csv
  steps$sheet = step("Google Sheet read (kept the last metadata.csv)", {
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/1zWKr_cXeDHnw6sbxzAPpGG92E-Di6nmB4Q4fa-OWuic/edit?gid=0#gid=0",
      sheet = "Sheet1",
      col_types = "cccclclllc"
    ) |>
      enightdata::write_local("metadata.csv")
  })

  step("Pipeline state push", push_pipeline_state(run_time, steps))

  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  sleep_time <- max(0, 60 * INTERVAL - elapsed)

  print("Finished... Zzzzz")

  Sys.sleep(sleep_time)
}
