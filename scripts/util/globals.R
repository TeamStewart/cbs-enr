PATH_GDRIVE = "https://drive.google.com/drive/u/0/folders/1ByjqU2Cq-qnBR8NYpC6r_JOE8CB1hJnL"
PATH_CBS_S3 = "cbsn-elections-external-models"
PATH_LOCAL = ""
ELECTION_DATE = "2026-11-03"
ELECTION_FOLDER = Sys.getenv("CBS_ELECTION_FOLDER", "26_general")
PATH_DROPBOX = ""
PATH_ENIGHTDATA = ""

local({
  user = Sys.info()["user"]
  
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
    cli::cli_abort("User {user} not recognized, cannot set PATH_DROPBOX or PATH_ENIGHTDATA")
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
