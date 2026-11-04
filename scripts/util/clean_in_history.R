# ========================================
## clean_in_history.R -- Indiana's 2024 General ENR export, president at precinct level,
## into {ELECTION_FOLDER}/history/IN_history.parquet, which read_history() reads.
##
##   Rscript scripts/util/clean_in_history.R "~/Downloads/AllOfficeResults (2).csv"
##
## 50 of the 92 counties report only at county level in 2024 and are dropped: this covers 42
## counties and 1.70M of the 2.94M presidential votes.
# ========================================
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(data.table)
})
source("scripts/util/globals.R")

OFFICE = "US President & Vice President"
TIMESTAMP = "2024_11_05_00_00_00"

args = commandArgs(trailingOnly = TRUE)
SRC = path.expand(args[1] %||% "")
if (!fs::file_exists(SRC)) cli::cli_abort("Pass the 2024 AllOfficeResults CSV.")

suppressMessages(devtools::load_all(PATH_ENIGHTDATA, quiet = TRUE))

# finish_general() stamps `election` from the config; assigning in place keeps 2024 out of the
# shared config file the live pipeline reads.
the = asNamespace("enightdata")$the
old_date = the$election_date
the$election_date = "2024-11-05"
on.exit(the$election_date <- old_date, add = TRUE)

# Patterns are the ticket's top-of-ballot surname; parties are the feed's own, as
# general_party() resolves them. finish_general() does not read "(W/I)" as a write-in.
.in24_pres = tibble::tribble(
  ~pattern, ~candidate, ~party, ~contest, ~district,
  "\\btrump\\b", "trump, donald j.", "rep", "pres", NA_integer_,
  "\\bharris\\b", "harris, kamala d.", "dem", "pres", NA_integer_,
  "\\bkennedy\\b", "kennedy, robert f., jr.", "oth", "pres", NA_integer_,
  "\\boliver\\b", "oliver, chase", "oth", "pres", NA_integer_,
  "\\bsonski\\b", "sonski, peter", "oth", "pres", NA_integer_,
  "\\bde la cruz\\b", "de la cruz, claudia", "oth", "pres", NA_integer_,
  "\\bwest\\b", "west, cornel", "ind", "pres", NA_integer_,
  "\\bhoefling\\b", "hoefling, tom", "ind", "pres", NA_integer_,
  "\\bboring\\b", "boring, paij", "ind", "pres", NA_integer_,
  "\\bnalbach\\b", "nalbach, william cody", "ind", "pres", NA_integer_,
  "\\bjenkins\\b", "jenkins, doug", "ind", "pres", NA_integer_,
  "\\basher\\b", "asher, carol", "ind", "pres", NA_integer_,
  "\\bmcneil\\b", "mcneil, andre ramon, sr.", "ind", "pres", NA_integer_,
  "\\bjohnson\\b", "johnson, nala baozun scott, jr.", "oth", "pres", NA_integer_,
  "\\birving\\b", "irving, shondra", "ind", "pres", NA_integer_,
  "\\bfox\\b", "fox, cherunda lynn", "ind", "pres", NA_integer_,
  "\\bbuchser\\b", "buchser, susan maude", "oth", "pres", NA_integer_,
)

# Same columns and intermediate shape as parse_in(); the export adds a leading `Election`.
raw = fread(
  SRC,
  colClasses = "character", showProgress = FALSE,
  select = c(
    "JurisdictionName", "ReportingCountyName", "DataEntryJurisdictionName",
    "DataEntryLevelName", "Office", "NameonBallot", "PoliticalParty", "TotalVotes"
  )
) |>
  as_tibble() |>
  filter(.data$DataEntryLevelName == "Precinct", .data$Office == OFFICE)

if (nrow(raw) == 0) cli::cli_abort("No precinct-level {.val {OFFICE}} rows in {.file {SRC}}.")

clean = raw |>
  transmute(
    county = .data$ReportingCountyName,
    precinct_id = str_squish(.data$DataEntryJurisdictionName),
    race_name = str_squish(paste(.data$Office, .data$JurisdictionName)),
    candidate_raw = str_squish(.data$NameonBallot),
    party_raw = .data$PoliticalParty,
    method = "Total",
    votes = as.numeric(.data$TotalVotes)
  ) |>
  mutate(stage = stage_votes(.data$votes), .by = c("county", "precinct_id")) |>
  # general_contest() has no `pres` branch: 2026 is a midterm.
  mutate(contest = "pres") |>
  finish_general("IN", TIMESTAMP, .in24_pres)

verify_scrape(clean)

# `method` is left out so read_history() fills it with NA and the dashboard joins on geo alone.
# A method of "all" would match none of the live eday/mail/early rows.
hist = clean |>
  transmute(
    geo = as.character(.data$geo),
    contest = as.character(.data$contest),
    district = NA_integer_,
    candidate = as.character(.data$candidate),
    party = as.character(.data$party),
    year = 2024L,
    county = str_extract(.data$geo, "^in_(\\d{3})", group = 1),
    votes = .data$votes
  )

out = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "history", "IN_history.parquet")
fs::dir_create(fs::path_dir(out))
nanoparquet::write_parquet(hist, out)

cli::cli_alert_success(
  "{nrow(hist)} rows, {dplyr::n_distinct(hist$geo)} precincts, {sum(hist$votes)} votes -> {.file {out}}"
)
