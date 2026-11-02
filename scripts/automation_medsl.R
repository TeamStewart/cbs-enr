##################################################
## Project: MEDSL ENR
## Script purpose: Automate tar_make() for the MEDSL pipeline
## Date: November 2026
## Author: Joseph R. Loffredo
##
## Rscript scripts/automation_medsl.R >> logs/medsl_console.log 2>&1
##
## Scrape and process only. It never dispatches pages.yml, which is the only thing in this repo
## that triggers a render.
##################################################

INTERVAL = 10 # minutes

# Host-specific values are env vars, so medsl.mit.edu only has to set them; the defaults are
# the laptop's layout.
MEDSL_DROPBOX = Sys.getenv("MEDSL_DROPBOX", "~/MIT Dropbox/Joseph Loffredo/medsl-enr")
MEDSL_ENIGHTDATA = Sys.getenv("MEDSL_ENIGHTDATA", "~/Desktop/GitHub/enightdata")
MEDSL_LOGDIR = Sys.getenv("MEDSL_LOGDIR", "logs")
# Clarity needs Python and is 57 of the 218 rows. Unset, reticulate's discovery dies on
# whatever stale python is first on PATH. Empty skips it, for a host whose default works.
MEDSL_PYTHON = Sys.getenv(
  "MEDSL_PYTHON",
  "~/Desktop/GitHub/cbs-enr/renv/python/virtualenvs/renv-python-3.9/bin/python3"
)

# tar_make() runs in a callr process that inherits these. PATH_DROPBOX and PATH_ENIGHTDATA must
# be set together, or globals.R's username map points this run at the CBS folder.
# ENIGHTDATA_CONFIG keeps this pipeline's paths out of the shared per-user config.
Sys.setenv(
  TAR_PROJECT = "medsl",
  PATH_DROPBOX = path.expand(MEDSL_DROPBOX),
  PATH_ENIGHTDATA = path.expand(MEDSL_ENIGHTDATA),
  ENIGHTDATA_CONFIG = path.expand(Sys.getenv("MEDSL_CONFIG", "~/.cache/medsl-enr-config")),
  CBS_AUTOMATION = "1"
)
if (nzchar(MEDSL_PYTHON)) Sys.setenv(RETICULATE_PYTHON = path.expand(MEDSL_PYTHON))

# One Google token for both: Drive's scope covers Sheets. A non-interactive Rscript can't open
# the browser, so cache it once first:
#   Rscript -e 'options(rlang_interactive = TRUE); googledrive::drive_auth()'
googledrive::drive_auth(email = TRUE)
googlesheets4::gs4_auth(token = googledrive::drive_token())

METADATA_SHEET = "https://docs.google.com/spreadsheets/d/1zWKr_cXeDHnw6sbxzAPpGG92E-Di6nmB4Q4fa-OWuic/edit?gid=0#gid=0"

# HTTPS, as both checkouts use. Over SSH the server needs a deploy key; over HTTPS a private
# repo needs GITHUB_PAT in ~/.Renviron.
ENIGHTDATA_REPO = Sys.getenv("MEDSL_ENIGHTDATA_REPO", "https://github.com/TeamStewart/enightdata.git")
ENIGHTDATA_BRANCH = Sys.getenv("MEDSL_ENIGHTDATA_BRANCH", "medsl")
ENIGHTDATA_DIR = Sys.getenv("MEDSL_ENIGHTDATA_DIR", "~/.cache/medsl-enr-enightdata")
NETWORKENR_REPO = Sys.getenv("MEDSL_NETWORKENR_REPO", "https://github.com/TeamStewart/networkenr.git")
NETWORKENR_BRANCH = Sys.getenv("MEDSL_NETWORKENR_BRANCH", "main")
NETWORKENR_DIR = Sys.getenv("MEDSL_NETWORKENR_DIR", "~/.cache/medsl-enr-networkenr")

source("scripts/medsl/lib.R")

# _targets_medsl.R and scripts/medsl/lib.R need no restart: tar_make() re-reads them every
# cycle. This file is the running process, so a new version of it cannot take effect in
# place -- the cycle exits 7 and scripts/medsl/run_medsl.sh starts the new one.
SELF = "scripts/automation_medsl.R"
self_hash <- function() unname(tools::md5sum(SELF))
SELF_HASH = self_hash()

# --ff-only: a checkout with local commits should stop and be looked at, not merged blind.
update_repo <- function() {
  out <- suppressWarnings(system2("git", c("pull", "--ff-only", "--quiet"),
                                  stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    stop("git pull: ", paste(out, collapse = " "))
  }
  invisible(TRUE)
}

# Read at the top of the cycle, not the bottom as in automation.R: metadata.csv resolves under
# the MEDSL folder, so a fresh one has none and the first tar_make() would build nothing. A
# failed read keeps the previous file.
pull_metadata <- function() {
  googlesheets4::read_sheet(ss = METADATA_SHEET, sheet = "Sheet1", col_types = "cccclclllc") |>
    enightdata::write_local("metadata.csv")
}

while (TRUE) {
  print(glue::glue(
    "Started at {format(Sys.time(), format='%Y-%m-%d %H:%M:%S')}, waiting..."
  ))

  start_time <- Sys.time()

  run_time <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%S") |>
    stringr::str_replace_all("-|:| ", "_")

  fs::dir_create(MEDSL_LOGDIR)

  # each step: TRUE, or FALSE after logging why; nothing here may stop the loop
  step <- function(what, expr) {
    tryCatch({ force(expr); TRUE }, error = function(e) { message(what, " failed: ", conditionMessage(e)); FALSE })
  }

  con <- file(fs::path(MEDSL_LOGDIR, glue::glue("medsl_{run_time}.log")))
  sink(con, append = TRUE)
  sink(con, append = TRUE, type = "message")

  steps = list(
    # first, so a pulled _targets_medsl.R or lib.R is what this cycle's tar_make() reads
    repo = step("cbs-enr pull (kept the current checkout)", update_repo()),
    sheet = step("Google Sheet read (kept the last metadata.csv)", pull_metadata()),
    enightdata = step("enightdata update (kept the installed version)", update_pkg_from_git(
      "enightdata", ENIGHTDATA_REPO, ENIGHTDATA_BRANCH, ENIGHTDATA_DIR
    )),
    networkenr = step("networkenr update (kept the installed version)", update_pkg_from_git(
      "networkenr", NETWORKENR_REPO, NETWORKENR_BRANCH, NETWORKENR_DIR
    )),
    tar_make = step("tar_make", targets::tar_make())
  )

  sink()
  sink(type = "message")
  close(con)

  print(glue::glue(
    "Cycle {run_time}: {paste(names(steps), unlist(steps), sep='=', collapse=' ')}"
  ))

  # Hand off before sleeping, so a pushed fix starts working now rather than in ten minutes.
  if (!identical(self_hash(), SELF_HASH)) {
    print("automation_medsl.R changed; exiting 7 for run_medsl.sh to start the new one")
    quit(save = "no", status = 7)
  }

  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  sleep_time <- max(0, 60 * INTERVAL - elapsed)

  print("Finished... Zzzzz")

  Sys.sleep(sleep_time)
}
