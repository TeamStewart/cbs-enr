PATH_GDRIVE = "https://drive.google.com/drive/u/0/folders/1ByjqU2Cq-qnBR8NYpC6r_JOE8CB1hJnL"
PATH_CBS_S3 = "cbsn-elections-external-models"
PATH_LOCAL = ""
ELECTION_DATE = "2026-11-03"
ELECTION_FOLDER = Sys.getenv("CBS_ELECTION_FOLDER", "26_general")
PATH_DROPBOX = ""
PATH_ENIGHTDATA = ""

# Election-night models in the targets pipeline (run_models() in scripts/functions.R; which
# models run is {ELECTION_FOLDER}/model_config.csv). MODEL_SIM = "" runs them on the real
# scraped returns and writes to model_results/. A 26_sim scenario name (e.g. "tx_base") runs
# them on that simulated night at MODEL_SIM_AT instead, written to
# model_tests/run_night_sim/<scenario>/ and never to model_results/.
MODEL_SIM = Sys.getenv("CBS_MODEL_SIM", "")
MODEL_SIM_AT = Sys.getenv("CBS_MODEL_SIM_AT", "2026-11-03 22:00")

local({
  user = Sys.info()["user"]

  # ENV VARS FIRST, so a machine can STATE its paths instead of being recognised by
  # username. The username map below only ever worked for the three people in it and
  # cli_abort()ed for everyone else -- which made this file unsourceable from
  # scripts/model/live/load.R, and would have taken down run_night.R on any machine whose
  # user is not one of those three. Stating the path is also the same route
  # find_26_general() already prefers (CBS_26_GENERAL).
  if (nzchar(Sys.getenv("PATH_DROPBOX")))    PATH_DROPBOX    <<- Sys.getenv("PATH_DROPBOX")
  if (nzchar(Sys.getenv("PATH_ENIGHTDATA"))) PATH_ENIGHTDATA <<- Sys.getenv("PATH_ENIGHTDATA")
  if (nzchar(PATH_DROPBOX) && nzchar(PATH_ENIGHTDATA)) return(invisible(NULL))

  if (user == "josephloffredo"){
    PATH_DROPBOX <<- "~/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data"
    PATH_ENIGHTDATA <<- "/Users/josephloffredo/Desktop/GitHub/enightdata"
  } else if (user == "mason") {
    PATH_DROPBOX <<- "~/Dropbox (MIT)/Research/CBS-MIT Election Data"
    PATH_ENIGHTDATA <<- "~/Documents/github/enightdata"
  } else if (user == "stewart") {
    PATH_DROPBOX <<- "/Users/stewart/MIT Dropbox/Mason Reece/Research/CBS-MIT Election Data"
    PATH_ENIGHTDATA <<- "~/Documents/enightdata"
  } else {
    # WARN, not abort. An unrecognised user is a missing CONFIG, not a broken repo: the
    # consumers of these paths all guard on nzchar() (find_26_general() skips an empty
    # PATH_DROPBOX and falls through to its own globs and CBS_26_GENERAL), so leaving them
    # empty degrades rather than dies. Aborting here meant one unlisted username could stop
    # the entire election-night pipeline from even loading.
    cli::cli_alert_warning(
      "globals.R: user {.val {user}} is not in the known list, so PATH_DROPBOX and \
       PATH_ENIGHTDATA are unset. Set them in ~/.Renviron if anything needs them \
       (find_26_general() has its own lookup and does not).")
  }

})

# ========================================
## DASHBOARD
# ========================================
# State x Contest selections shown in the dashboard dropdown (index.qmd).
# - state: all caps abbreviation
# - contest: contest code as it appears in the clean data (e.g. "sen", "gov", "house")
# - district: integer district for district-level contests, NA for statewide
# - label: dropdown text; NA -> "{state} {contest}[-{district}]"
# - hist_contest: contest to compare against in {STATE}_history.parquet; NA -> same contest
DASHBOARD_CONTESTS = tibble::tribble(
  ~state, ~contest, ~district, ~label, ~hist_contest,
  "GA", "sen", NA_integer_, "GA Senate", NA_character_,
  "GA", "gov", NA_integer_, "GA Governor", NA_character_,
  "NC", "sen", NA_integer_, "NC Senate", NA_character_,
  "NE", "sen", NA_integer_, "NE Senate", NA_character_,
  "PA", "gov", NA_integer_, "PA Governor", NA_character_,
  "TX", "house", 33L, NA_character_, NA_character_,
)

DASHBOARD_TITLE = local({
  yr = paste0("20", stringr::str_sub(ELECTION_FOLDER, 1, 2))
  kind = stringr::str_to_title(stringr::str_remove(ELECTION_FOLDER, "^\\d+_"))
  dt = lubridate::ymd(ELECTION_DATE)
  paste0(yr, " ", kind, " · ", format(dt, "%b"), " ", lubridate::day(dt))
})
