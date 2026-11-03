# ========================================
## MEDSL TARGETS PIPELINE
## Scrape and process only: no upload, no modeling, no Quarto render.
## TAR_PROJECT=medsl selects this (_targets.yaml); the store is _targets_medsl/.
# ========================================
rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(glue)
})

source("scripts/functions.R")
source("scripts/medsl/lib.R")

# globals.R only skips its username map when PATH_DROPBOX and PATH_ENIGHTDATA are both set,
# and that map points PATH_DROPBOX at the CBS folder. Setting one alone is how this run would
# quietly write into CBS's tree.
if (!nzchar(PATH_DROPBOX) || grepl("CBS-MIT Election Data", PATH_DROPBOX)) {
  cli::cli_abort(c(
    "PATH_DROPBOX is {.path {PATH_DROPBOX}}, which is not a MEDSL output folder.",
    i = "Set PATH_DROPBOX and PATH_ENIGHTDATA together; see scripts/automation_medsl.R."
  ))
}

if (Sys.getenv("CBS_AUTOMATION") != "1") local({
  stamp = file.path(tools::R_user_dir("cbs-enr", "cache"), "enightdata_installed_medsl")
  git = \(...) suppressWarnings(system2("git", c("-C", shQuote(path.expand(PATH_ENIGHTDATA)), ...), stdout = TRUE, stderr = TRUE))
  state = rlang::hash(c(git("rev-parse", "HEAD"), git("status", "--porcelain"), git("diff", "HEAD")))
  if (!requireNamespace("enightdata", quietly = TRUE) || !file.exists(stamp) || readLines(stamp, warn = FALSE)[1] != state) {
    roxygen2::roxygenise(PATH_ENIGHTDATA)
    renv::install(PATH_ENIGHTDATA, prompt = FALSE)
    dir.create(dirname(stamp), recursive = TRUE, showWarnings = FALSE)
    writeLines(state, stamp)
  }
})

options(
  readr.show_col_types = FALSE,
  gargle_oauth_email = TRUE,
  scipen = 999
)

tar_option_set(
  packages = c(
    "data.table",
    # the verbs, not the metapackage: nothing here plots, and ggplot2 and its font stack
    # would be built from source on the server
    "dplyr",
    "tidyr",
    "stringr",
    "readr",
    "purrr",
    "tibble",
    "lubridate",
    "glue",
    "fs",
    "reticulate",
    "enightmodels",
    "enightdata", # scrapers and timestamps
    "networkenr" # news network results
  ),
  imports = c("enightdata", "networkenr"),
  error = "null",
  controller = crew::crew_controller_local(workers = MEDSL_WORKERS)
)

# Persisted to a config file so the crew workers resolve the same paths: each loads enightdata
# fresh and seeds the$path_dropbox in .onLoad. ENIGHTDATA_CONFIG keeps it out of CBS's copy.
enightdata::set_all(
  dropbox = PATH_DROPBOX,
  gdrive = PATH_GDRIVE,
  election_folder = ELECTION_FOLDER,
  s3_bucket = PATH_CBS_S3,
  election_date = ELECTION_DATE
)

# Every jurisdiction with a verified link, not just the ones CBS gets. Column meanings are in
# _targets.R; `upload` is read and ignored.
metadata = enightdata::read_local("metadata.csv", colClasses=list(character=c(1:4, 6), logical=c(5, 7, 8, 9))) |>
  dplyr::mutate(
    path_timestamp = dplyr::na_if(path_timestamp, ""),
    path_timestamp = dplyr::coalesce(path_timestamp, path),
    county = dplyr::na_if(county, "")
  ) |>
  dplyr::filter(build, updated)

create_dirs_medsl(metadata)

# ========================================
## PIPELINE
# ========================================
map_single = tar_map(
  metadata,
  tar_target(timestamp, get_timestamp(state, county, path_timestamp), cue = tar_cue(mode = "always")),
  tar_target(data, get_data(state, county, timestamp, path, trust_stage)),
  tar_target(summary, make_summary(data, state, county)),
  names = c(state, county)
)

# One target per network: networkenr holds its per-host rate limits inside a single fetch call,
# so splitting a network would let crew workers race past its own throttle.
map_networks = tar_map(
  tibble::tibble(net = MEDSL_NETWORKS),
  tar_target(net_timestamp, networkenr::get_timestamp(net), cue = tar_cue(mode = "always")),
  tar_target(net_data, save_network(net, net_timestamp, medsl_root)),
  names = net
)

list(
  # An explicit target so the crew workers get it; enightdata's own copy of the path lives in
  # its config file and is not reachable from here.
  tar_target(medsl_root, fs::path(path.expand(PATH_DROPBOX), ELECTION_FOLDER)),
  map_single,
  map_networks,
  tar_combine(net_report, map_networks[["net_data"]], command = dplyr::bind_rows(!!!.x)),
  # MEDSL's live plots (hdurham/enr_scripts, cloned to ENR_SCRIPTS_DIR): pulls its latest main,
  # then one pass after this cycle's summaries and network results, in its own R process so
  # ggplot2 stays out of this pipeline. A failed pull is a warning and the pass runs the last
  # copy; a pass that fails or runs past 5 minutes is this target's error, and error = "null"
  # keeps the rest of the cycle going. Returns when the plots last drew.
  tar_combine(
    live_plots,
    map_single[["summary"]], map_networks[["net_data"]],
    command = {
      list(!!!.x) # wait for every summary and network result
      dir = path.expand(Sys.getenv("ENR_SCRIPTS_DIR", "~/enr_scripts"))
      pulled = system2("git", c("-C", dir, "pull", "--ff-only", "--quiet"), timeout = 60)
      if (pulled != 0) warning("git pull in ", dir, " failed (exit ", pulled, "); ran the last copy")
      callr::rscript("live/run_live.R", cmdargs = "--once", wd = dir, timeout = 300, show = FALSE)
      Sys.time()
    },
    cue = tar_cue(mode = "always")
  )
)
