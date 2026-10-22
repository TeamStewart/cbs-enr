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
##   state, contest, district, county_fips (5-digit text), county_cbs (3-digit CBS
##   county number, alphabetical -- NOT the FIPS county code; matches the `geo` prefix
##   tx_{county_cbs}_...), candidate, party (dem / rep / oth), party_cbs (CBS's own
##   label), votes
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
##   Rscript scripts/model/cbs_county_pull.R TX_gov             # one race, test feed
##   Rscript scripts/model/cbs_county_pull.R TX_gov FL_sen      # several
##   Rscript scripts/model/cbs_county_pull.R --all              # every race in priors.csv
##   Rscript scripts/model/cbs_county_pull.R --all --live       # live feed
##   Rscript scripts/model/cbs_county_pull.R TX house 7         # old form: STATE CONTEST [DISTRICT]
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

# ---- which races --------------------------------------------------------------------
# A race is given as a race_id from 26_general/input_data/priors.csv ("TX_gov",
# "AK_house", ...), or as STATE CONTEST [DISTRICT]. --all pulls every race in priors.csv.
# House races in at-large states (AK, DE, ND, SD, VT, WY) are district 1, as in
# enightmodels' schema.
ENV = "test"             # "test" until election night
ELECTION = as.Date("2026-11-03")
AT_LARGE = c("AK", "DE", "ND", "SD", "VT", "WY")

parse_race_id = function(id) {
  p = strsplit(id, "_")[[1]]
  st = toupper(p[1]); ct = tolower(p[2])
  dist = if (length(p) >= 3) as.integer(p[3]) else if (ct == "house" && st %in% AT_LARGE) 1L else NA_integer_
  tibble(race_id = id, state = st, contest = ct, district = dist)
}

args = commandArgs(trailingOnly = TRUE)
if ("--live" %in% args) ENV = "live"
ALL = "--all" %in% args
args = setdiff(args, c("--live", "--all"))

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
races = if (ALL) {
  pr = readr::read_csv(file.path(DIR_26, "input_data", "priors.csv"), show_col_types = FALSE)
  bind_rows(lapply(pr$race_id, parse_race_id))
} else if (length(args) == 0) {
  parse_race_id("TX_gov")
} else if (length(args) == 1 || grepl("_", args[1])) {
  bind_rows(lapply(args, parse_race_id))
} else {
  tibble(race_id = paste(toupper(args[1]), tolower(args[2]), sep = "_"), state = toupper(args[1]),
         contest = tolower(args[2]), district = if (length(args) >= 3) as.integer(args[3]) else NA_integer_)
}

# ---- one race -----------------------------------------------------------------------
# Returns a one-row summary; stops with an informative error if the race can't be pulled.
pull_race = function(STATE, CONTEST, DISTRICT, ENV) {
  race_tag = if (is.na(DISTRICT)) glue("{STATE}_{CONTEST}") else glue("{STATE}_{CONTEST}_{DISTRICT}")
  DIR_OUT = file.path(DIR_26, "cbs_api", ENV, race_tag)
  observed_at = lubridate::now(tzone = "UTC")
  cli::cli_h1("CBS county pull: {race_tag} ({ENV} feed) at {format(observed_at, '%Y-%m-%d %H:%M:%S')} UTC")

  # ---- 1. county results ------------------------------------------------------------
  cty_raw = cbsapi::cbs_election_data(ELECTION, STATE, CONTEST, district = DISTRICT,
                                      county = TRUE, addl = TRUE, env = ENV)
  if (nrow(cty_raw) == 0) {
    cli::cli_abort("CBS returned no county results for {race_tag} on the {ENV} feed (not published yet, or wrong contest/district).")
  }

  # ---- 2. FIPS -> county_cbs ----------------------------------------------------------
  # CBS numbers a state's counties in alphabetical order (enightmodels::cbs_counties: TX
  # Anderson = 1 ... Zavala = 254), and the precinct ids use that number (tx_015_* is
  # Bexar, FIPS 48029). It is NOT the FIPS county code. Census assigned county FIPS codes
  # alphabetically too, so a county's CBS number = its rank among the state's FIPS codes
  # (TX: (FIPS county + 1) / 2). Two checks before anything is written:
  #   - CBS must return every county in cbs_counties, or the ranks would shift
  #   - where our model inputs exist, the rank must match the county prefix of the precinct
  #     ids for every county we have (TX: the 32 scraped counties)
  known = enightmodels::cbs_counties |>
    filter(state == STATE) |>
    transmute(county_cbs = str_pad(county_cbs, 3, pad = "0"), county_name = county)
  fips = sort(unique(str_pad(cty_raw$geo, 5, pad = "0")))
  if (length(fips) != nrow(known)) {
    cli::cli_abort(c("CBS returned {length(fips)} counties for {STATE} but cbs_counties has {nrow(known)}.",
                     "i" = "The FIPS -> CBS number rule needs the full county list; add an explicit lookup for this state."))
  }
  fips_map = tibble(county_fips = fips, county_cbs = str_pad(seq_along(fips), 3, pad = "0"))
  p_inp = file.path(DIR_26, "input_data", "model_inputs", glue("{STATE}_precinct.parquet"))
  if (file.exists(p_inp)) {
    chk = nanoparquet::read_parquet(p_inp, col_select = c("geo", "county_fips")) |>
      distinct(county_fips = str_pad(as.character(county_fips), 5, pad = "0"),
               prefix = str_extract(as.character(geo), "(?<=_)[0-9]{3}(?=_)")) |>
      left_join(fips_map, by = "county_fips")
    bad = filter(chk, is.na(county_cbs) | county_cbs != prefix)
    if (nrow(bad)) cli::cli_abort(c("FIPS -> CBS number rule disagrees with the precinct ids for {nrow(bad)} county(ies): {head(bad$county_fips, 10)}",
                                    "i" = "Add an explicit lookup for {STATE} before using these rows."))
    cli::cli_alert_success("FIPS -> CBS county number checked against the precinct ids of {nrow(chk)} counties")
  }

  # CBS's county feed labels parties d / r / lb ..., its race feed dem / rep / lib. Both
  # go to enightmodels' codes (dem, rep, oth), as in lookups/party_map.csv; the original
  # label is kept in party_cbs.
  cbs_party = function(p) {
    p = tolower(trimws(as.character(p)))
    case_when(p %in% c("d", "dem", "democrat", "democratic") ~ "dem",
              p %in% c("r", "rep", "gop", "republican") ~ "rep",
              TRUE ~ "oth")
  }

  cty = cty_raw |>
    transmute(
      state, contest, district,
      county_fips = str_pad(geo, 5, pad = "0"),
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
    ) |>
    mutate(party_cbs = party, party = cbs_party(party), .after = party) |>
    left_join(fips_map, by = "county_fips") |>
    left_join(known, by = "county_cbs") |>
    relocate(county_cbs, county_name, .after = county_fips)

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
    full_join(race |> mutate(party = cbs_party(party)) |> select(candidate, party, race_total = votes),
              by = c("candidate", "party")) |>
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
  dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)
  f = file.path(DIR_OUT, glue("{race_tag}_{format(observed_at, '%Y%m%d_%H%M%S')}.parquet"))
  nanoparquet::write_parquet(cty, f)
  cli::cli_alert_success("Saved {nrow(cty)} rows to {f}")
  tibble(counties = n_cty, of = nrow(known), rows = nrow(cty),
         pct_exp_vote = paste(range(cty$county_pct_exp_vote, na.rm = TRUE), collapse = "-"),
         candidates = paste(unique(paste0(cty$candidate, " (", cty$party, ")")), collapse = "; "),
         file = basename(f))
}

# ---- run ------------------------------------------------------------------------------
out = lapply(seq_len(nrow(races)), function(i) {
  r = races[i, ]
  res = tryCatch(pull_race(r$state, r$contest, r$district, ENV),
                 error = function(e) tibble(error = conditionMessage(e)))
  bind_cols(select(r, race_id, district), res)
}) |> bind_rows()
if (nrow(races) > 1) {
  cli::cli_h1("Summary ({ENV} feed)")
  for (cl in c("counties", "of", "pct_exp_vote", "candidates", "error")) if (!cl %in% names(out)) out[[cl]] = NA
  out |>
    transmute(race_id, district, result = if_else(is.na(error), "ok", "FAILED"),
              counties = if_else(is.na(error), paste0(counties, " of ", of), ""),
              pct_exp_vote = coalesce(pct_exp_vote, ""),
              detail = if_else(is.na(error), candidates,
                               str_trunc(str_squish(str_replace_all(error, "\\n|!|\u2139", " ")), 110))) |>
    as.data.frame() |> print(right = FALSE, row.names = FALSE)
}
if ("error" %in% names(out) && any(!is.na(out$error))) quit(status = 1)
