##################################################
## Project: CBS ENR
## Script purpose: Automate tar_make()
## Date: March 2024
## Author: Joseph R. Loffredo and Mason Reece
##################################################

INTERVAL = 5 # minutes
googlesheets4::gs4_auth(email = TRUE)
googledrive::drive_auth(email = TRUE)

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

# Function to execute the R script
execute_script <- function() {
  message(sprintf("%s: Running tar_make", format(Sys.time(), format="%Y-%m-%d %H:%M:%S")))
  targets::tar_make()
}

git <- function(..., dir = DASH_PUSH_DIR, input = NULL) {
  out = suppressWarnings(system2("git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = TRUE, input = input))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    stop("git ", paste(c(...), collapse = " "), ": ", paste(out, collapse = "\n"))
  }
  out
}

# Force-push DASH_DATA as the only file on DASH_BRANCH, then ask GitHub Actions to render
# and deploy the page from it (.github/workflows/pages.yml)
push_dashboard_data <- function(run_time) {
  if (!file.exists(DASH_DATA)) {
    message("No ", DASH_DATA, " (did dashboard_export build?); nothing pushed")
    return(invisible(FALSE))
  }
  if (!dir.exists(file.path(DASH_PUSH_DIR, ".git"))) {
    fs::dir_create(DASH_PUSH_DIR)
    git("init", "--quiet")
    git("remote", "add", "origin", system2("git", c("remote", "get-url", "origin"), stdout = TRUE))
  }
  blob = git("hash-object", "-w", shQuote(normalizePath(DASH_DATA)))
  tree = git("mktree", input = sprintf("100644 blob %s\tdashboard_data.rds", blob))
  commit = git("-c", "user.name='cbs-enr automation'", "-c", "user.email=noreply@github.com",
               "commit-tree", tree, "-m", shQuote(glue::glue("dashboard data {run_time}")))
  git("push", "--force", "--quiet", "origin", paste0(commit, ":refs/heads/", DASH_BRANCH))
  git("gc", "--prune=now", "--quiet") # drop the previous runs' blobs
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

  # pull down latest `enightdata`
  cred <- git2r::cred_token()
  git2r::pull("../enightdata/", credentials = cred)

  con <- file(glue::glue("logs/automation_{run_time}.log"))
  sink(con, append = TRUE)
  sink(con, append = TRUE, type = "message")

  # a failed run must not leave output sunk or stop the loop
  tryCatch(execute_script(), error = function(e) message("tar_make failed: ", conditionMessage(e)))

  # Restore output to console
  sink()
  sink(type = "message")

  tryCatch(push_dashboard_data(run_time), error = function(e) message("Dashboard push failed: ", conditionMessage(e)))
  tryCatch(upload_dashboard_s3(), error = function(e) message("Dashboard S3 upload failed: ", conditionMessage(e)))

  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1zWKr_cXeDHnw6sbxzAPpGG92E-Di6nmB4Q4fa-OWuic/edit?gid=0#gid=0",
    sheet = "Sheet1",
    col_types = "cccclclllc"
  ) |>
    enightdata::write_local("metadata.csv") |>
    invisible()

  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  sleep_time <- max(0, 60 * INTERVAL - elapsed)

  print("Finished... Zzzzz")

  Sys.sleep(sleep_time)
}
