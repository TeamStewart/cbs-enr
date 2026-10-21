# ========================================
## Pull CBS county-level results for one race (2026 general)
##
## Fetches county results from the CBS News Elections API (via the cbsapi
## package) and writes an append-only snapshot to the shared Dropbox folder,
## so counties we don't scrape can be filled from CBS on election night.
##
##   26_general/cbs_api/{env}/{ST}_{contest}[_{district}]/
##     {ST}_{contest}_{YYYYmmdd_HHMMSS}.parquet   one snapshot per run
##
## One row per county x candidate. Columns:
##   state, contest, district, county_fips (5-digit text), county_cbs (3-digit,
##   matches the `geo` prefix tx_{county_cbs}_...), candidate, party, votes
##   (cumulative, all methods), county_total_vote, county_pct_exp_vote,
##   county_precincts_in, county_precincts_of, county_ts (CBS's per-county
##   update time, UTC), race_last_updated, race_pct_in, race_call, env,
##   observed_at (when this script ran, UTC).
##
## Use county_ts, not race_last_updated, for as-of joins: cbsapi copies the
## race-level timestamp and stage onto every county row. Snapshots are
## cumulative, so never sum across snapshots (data contract §13).
##
## Run from the cbs-enr folder:
##   Rscript scripts/model/cbs_county_pull.R TX gov            # test feed
##   Rscript scripts/model/cbs_county_pull.R TX house 7        # House needs a district
##   Rscript scripts/model/cbs_county_pull.R TX gov --live     # live feed
## or source() it in RStudio/Positron after editing the defaults below.
##
## One-time setup (each person):
##   1. CBS API key in ~/.Renviron:      CBS_API_KEY="..."   then restart R
##   2. cbsapi is a private repo, so pak needs a GitHub token that can read
##      cbs-data-desk-ext (gitcreds::gitcreds_set(), or GITHUB_PAT in ~/.Renviron):
##        pak::pak("cbs-data-desk-ext/cbsapi")
##   3. Dropbox 26_general found automatically; if not, set CBS_26_GENERAL in
##      ~/.Renviron (see find_26_general below).
## Needs: cbsapi, tidyverse, glue, cli, nanoparquet, enightmodels
# ========================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
})

# ---- settings: defaults, overridden by command-line arguments --------------------
STATE = "TX"
CONTEST = "gov"          # pres, sen, house, gov, ag, ltgov, stsen, sthouse, supcrt, mayor, prop
DISTRICT = NA_integer_   # NA for statewide; required for house/stsen/sthouse
ENV = "test"             # "test" until election night

args = commandArgs(trailingOnly = TRUE)
if ("--live" %in% args) ENV = "live"
args = setdiff(args, "--live")
if (length(args) >= 1) STATE = toupper(args[1])
if (length(args) >= 2) CONTEST = tolower(args[2])
if (length(args) >= 3) DISTRICT = as.integer(args[3])
ELECTION = as.Date("2026-11-03")

# ---- 0. checks and paths ----------------------------------------------------------
if (!requireNamespace("cbsapi", quietly = TRUE)) {
  cli::cli_abort(c("Package {.pkg cbsapi} is not installed.",
                   "i" = 'Run {.code pak::pak("cbs-data-desk-ext/cbsapi")} (needs a GitHub token with access to cbs-data-desk-ext).'))
}
if (!nzchar(Sys.getenv("CBS_API_KEY"))) {
  cli::cli_abort(c("CBS_API_KEY is not set.",
                   "i" = 'Add CBS_API_KEY="..." to ~/.Renviron (usethis::edit_r_environ()) and restart R.'))
}

# Same lookup order as prep_tx_model_inputs.R: (1) CBS_26_GENERAL, (2) PATH_DROPBOX
# if set by globals.R, (3) known Dropbox install locations.
find_26_general = function() {
  cands = c(
    Sys.getenv("CBS_26_GENERAL", unset = ""),
    if (exists("PATH_DROPBOX") && nzchar(PATH_DROPBOX)) file.path(PATH_DROPBOX, "26_general"),
    path.expand("~/Dropbox/2024 Election Results/26_general"),
    Sys.glob(path.expand("~/MIT Dropbox/*/CBS-MIT Election Data/26_general")),
    Sys.glob(path.expand("~/Library/CloudStorage/Dropbox*/*/CBS-MIT Election Data/26_general")),
    Sys.glob(path.expand("~/Dropbox*/*/CBS-MIT Election Data/26_general"))
  )
  cands = cands[nzchar(cands) & dir.exists(cands)]
  if (length(cands) == 0) {
    cli::cli_abort(c("Can't find the Dropbox 26_general folder.",
                     "i" = 'Add CBS_26_GENERAL="/full/path/to/26_general" to ~/.Renviron and restart R.'))
  }
  normalizePath(cands[1])
}
DIR_26 = find_26_general()
race_tag = if (is.na(DISTRICT)) glue("{STATE}_{CONTEST}") else glue("{STATE}_{CONTEST}_{DISTRICT}")
DIR_OUT = file.path(DIR_26, "cbs_api", ENV, race_tag)
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

observed_at = lubridate::now(tzone = "UTC")
cli::cli_h1("CBS county pull: {race_tag} ({ENV} feed) at {format(observed_at, '%Y-%m-%d %H:%M:%S')} UTC")

# ---- 1. county results ------------------------------------------------------------
cty_raw = cbsapi::cbs_election_data(ELECTION, STATE, CONTEST, district = DISTRICT,
                                    county = TRUE, addl = TRUE, env = ENV)
if (nrow(cty_raw) == 0) {
  cli::cli_abort("CBS returned no county results for {race_tag} on the {ENV} feed (not published yet, or wrong contest/district).")
}

# ---- 2. FIPS -> county_cbs ----------------------------------------------------------
# county_cbs is assumed to be the 3-digit county part of the FIPS code (e.g. Kaufman,
# TX = 48257 -> tx_257_*). Checked against enightmodels::cbs_counties below; the
# script stops rather than write rows that would not join to the precinct spine.
cty = cty_raw |>
  transmute(
    state, contest, district,
    county_fips = str_pad(geo, 5, pad = "0"),
    county_cbs = str_sub(county_fips, 3, 5),
    candidate, party, votes,
    county_total_vote = as.numeric(totalVote),
    county_pct_exp_vote = as.numeric(pctExpVote),
    county_precincts_in = as.numeric(precinctsIn),
    county_precincts_of = as.numeric(precinctsOf),
    county_ts = lubridate::ymd_hms(ts, tz = "UTC", quiet = TRUE),
    race_last_updated = time,
    race_pct_in = pctIn,
    race_call = call,
    env = ENV,
    observed_at = observed_at
  )

known = enightmodels::cbs_counties |>
  filter(state == STATE) |>
  transmute(county_cbs = str_pad(county_cbs, 3, pad = "0"), county_name = county)
unmatched = setdiff(unique(cty$county_cbs), known$county_cbs)
if (length(unmatched) > 0) {
  cli::cli_abort(c("{length(unmatched)} CBS FIPS code(s) don't map to enightmodels::cbs_counties for {STATE}: {head(unmatched, 10)}",
                   "i" = "The FIPS -> county_cbs rule doesn't hold for this state; add an explicit lookup before using these rows."))
}
cty = left_join(cty, known, by = "county_cbs") |> relocate(county_name, .after = county_cbs)

# ---- 3. checks ----------------------------------------------------------------------
n_cty = n_distinct(cty$county_cbs)
cli::cli_alert_info("{n_cty} counties returned; {nrow(known)} counties in cbs_counties for {STATE}")
if (n_cty < nrow(known)) cli::cli_alert_warning("Missing counties: {head(setdiff(known$county_name, cty$county_name), 10)}")

dupes = count(cty, county_cbs, candidate, party) |> filter(n > 1)
if (nrow(dupes) > 0) cli::cli_abort("Duplicate county x candidate rows in the CBS response ({nrow(dupes)}).")

# county sums should equal the race-level totals when both feeds are in sync
race = cbsapi::cbs_election_data(ELECTION, STATE, CONTEST, district = DISTRICT,
                                 county = FALSE, env = ENV)
sum_check = cty |>
  summarise(county_sum = sum(votes, na.rm = TRUE), .by = c(candidate, party)) |>
  full_join(select(race, candidate, party, race_total = votes), by = c("candidate", "party")) |>
  mutate(diff = county_sum - race_total)
print(sum_check)
if (any(is.na(sum_check$diff) | sum_check$diff != 0)) {
  cli::cli_alert_warning("County sums differ from race totals (feeds out of sync, or counties missing). Snapshot still saved.")
} else {
  cli::cli_alert_success("County sums match race totals.")
}

cli::cli_h2("Most-reported counties")
cty |>
  distinct(county_name, county_cbs, county_ts, county_pct_exp_vote, county_precincts_in, county_precincts_of) |>
  arrange(desc(county_pct_exp_vote)) |>
  print(n = 10)

# ---- 4. save append-only snapshot -------------------------------------------------
f = file.path(DIR_OUT, glue("{race_tag}_{format(observed_at, '%Y%m%d_%H%M%S')}.parquet"))
nanoparquet::write_parquet(cty, f)
cli::cli_alert_success("Saved {nrow(cty)} rows to {f}")
