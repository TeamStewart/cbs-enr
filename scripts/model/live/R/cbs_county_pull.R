# ========================================
## cbs_county_pull.R -- CBS county results for our races, pulled fresh at every model run.
##
## Fetches county results from the CBS News Elections API (the cbsapi package) and saves
## an append-only snapshot to the shared Dropbox folder, so the statewide rows
## (counties.R) use CBS's latest counts for the counties we don't scrape:
##
##   26_general/cbs_api/{env}/{ST}_{contest}[_{district}]/
##     {ST}_{contest}_{YYYYmmdd_HHMMSS}.parquet   one snapshot per pull
##
## Functions (loaded by load.R; nothing runs at load time):
##   pull_cbs_race("TX", "gov", env = "live")   pull, check, save; returns the snapshot
##   cbs_for_races(races, env = "live")         (races.R) pulls every race, falling back
##                                              to the last saved snapshot if a pull fails
##
## From the command line (to test the feed, or pull without running the models), from the
## cbs-enr folder:
##   Rscript scripts/model/live/R/cbs_county_pull.R TX_gov             # one race, test feed
##   Rscript scripts/model/live/R/cbs_county_pull.R TX_gov FL_sen      # several
##   Rscript scripts/model/live/R/cbs_county_pull.R --all              # every race in priors.csv
##   Rscript scripts/model/live/R/cbs_county_pull.R --all --live       # live feed
##
## One row per county x candidate. Columns:
##   state, contest, district, county_fips (5-digit text), county_cbs (3-digit CBS
##   county number, alphabetical -- NOT the FIPS county code; matches the `geo` prefix
##   tx_{county_cbs}_...), county_name, candidate, party (dem / rep / oth), party_cbs
##   (CBS's own label), votes (cumulative, all methods), county_total_vote,
##   county_pct_exp_vote, county_precincts_in, county_precincts_of, county_ts (CBS's
##   per-county update time, UTC), race_last_updated, race_pct_in, race_call, env,
##   observed_at (when the pull ran, UTC).
## Use county_ts, not race_last_updated, for as-of joins: cbsapi copies the race-level
## timestamp onto every county row. Snapshots are cumulative: never sum across them.
##
## One-time setup (each person who runs the models):
##   1. CBS API key in ~/.Renviron:   CBS_API_KEY="..."   then restart R
##   2. cbsapi is a private repo: a GitHub token that can read cbs-data-desk-ext, then
##        remotes::install_github("cbs-data-desk-ext/cbsapi")
## Without these the models still run, using the last saved snapshots (cbs_for_races()).
# ========================================

CBS_ELECTION = as.Date("2026-11-03")

# CBS's county feed labels parties d / r / lb ..., its race feed dem / rep / lib. Both go
# to enightmodels' codes (dem, rep, oth), as in lookups/party_map.csv.
cbs_party = function(p) {
  p = tolower(trimws(as.character(p)))
  case_when(p %in% c("d", "dem", "democrat", "democratic") ~ "dem",
            p %in% c("r", "rep", "gop", "republican") ~ "rep",
            TRUE ~ "oth")
}

# can this machine pull from CBS? (cbsapi installed and a key set) -- "" if yes, else why not
cbs_pull_unavailable = function() {
  if (!requireNamespace("cbsapi", quietly = TRUE))
    return('cbsapi is not installed (remotes::install_github("cbs-data-desk-ext/cbsapi"))')
  if (!nzchar(Sys.getenv("CBS_API_KEY"))) return("CBS_API_KEY is not set in ~/.Renviron")
  ""
}

# FIPS -> CBS county number. CBS numbers a state's counties alphabetically by ITS names
# (enightmodels::cbs_counties: TX Anderson = 1 ... Zavala = 254; FL files Miami-Dade under
# D), and the precinct ids use that number (tx_015_* is Bexar). It is NOT the FIPS county
# code, and FIPS order is not safe either (Miami-Dade's FIPS is 12086). The lookup is
# model_inputs/{ST}_counties.csv from prep_model_inputs.R (names matched between L2 and
# cbs_counties; AK = 40 election districts, FIPS 02001-02040). Without that file, FIPS
# order is used, and only if CBS returns exactly the cbs_counties count.
cbs_county_lookup = function(state, fips, dir_26) {
  n_known = sum(enightmodels::cbs_counties$state == state)
  p_cty = file.path(dir_26, "input_data", "model_inputs", paste0(state, "_counties.csv"))
  if (file.exists(p_cty)) {
    m = readr::read_csv(p_cty, col_types = readr::cols(.default = "c")) |> select(county_fips, county_cbs)
    miss = setdiff(fips, m$county_fips)
    if (length(miss)) cli::cli_abort(c("{length(miss)} CBS FIPS code(s) are not in {basename(p_cty)}: {head(miss, 10)}",
                                       "i" = "Rerun prep_model_inputs.R {state}, or check what CBS uses as geo for {state}."))
    return(m)
  }
  if (length(fips) != n_known)
    cli::cli_abort(c("CBS returned {length(fips)} counties for {state} but cbs_counties has {n_known}, and there is no {basename(p_cty)}.",
                     "i" = "Run prep_model_inputs.R {state} to write the county lookup."))
  cli::cli_alert_warning("No {basename(p_cty)}: CBS county numbers taken from FIPS order. Run prep_model_inputs.R {state}.")
  tibble(county_fips = fips, county_cbs = stringr::str_pad(seq_along(fips), 3, pad = "0"))
}

# Pull one race, check it, save the snapshot, return it. Stops with an informative error
# if the race can't be pulled. verbose = FALSE for the per-refresh pulls.
pull_cbs_race = function(state, contest, district = NA, env = "live", dir_26 = find_26_general(), verbose = TRUE) {
  why = cbs_pull_unavailable()
  if (nzchar(why)) stop(why)
  tag = if (is.na(district)) paste0(state, "_", contest) else paste0(state, "_", contest, "_", district)
  observed_at = lubridate::now(tzone = "UTC")
  if (verbose) cli::cli_h1("CBS county pull: {tag} ({env} feed) at {format(observed_at, '%Y-%m-%d %H:%M:%S')} UTC")

  raw = cbsapi::cbs_election_data(CBS_ELECTION, state, contest, district = district,
                                  county = TRUE, addl = TRUE, env = env)
  if (nrow(raw) == 0)
    cli::cli_abort("CBS returned no county results for {tag} on the {env} feed (not published yet, or wrong contest/district).")

  fips = sort(unique(stringr::str_pad(raw$geo, 5, pad = "0")))
  fips_map = cbs_county_lookup(state, fips, dir_26)
  known = enightmodels::cbs_counties |>
    filter(state == !!state) |>
    transmute(county_cbs = stringr::str_pad(county_cbs, 3, pad = "0"), county_name = county)

  # where we have precinct inputs, the county numbers must match the precinct ids
  p_inp = file.path(dir_26, "input_data", "model_inputs", paste0(state, "_precinct.parquet"))
  if (file.exists(p_inp)) {
    chk = nanoparquet::read_parquet(p_inp, col_select = c("geo", "county_fips")) |>
      distinct(county_fips = stringr::str_pad(as.character(county_fips), 5, pad = "0"),
               prefix = stringr::str_extract(as.character(geo), "(?<=_)[0-9]{3}(?=_)")) |>
      left_join(fips_map, by = "county_fips")
    bad = filter(chk, is.na(county_cbs) | county_cbs != prefix)
    if (nrow(bad)) cli::cli_abort(c("County numbers disagree with the precinct ids for {nrow(bad)} county(ies): {head(bad$county_fips, 10)}",
                                    "i" = "Rerun prep_model_inputs.R {state} before using these rows."))
  }

  cty = raw |>
    transmute(
      state, contest, district,
      county_fips = stringr::str_pad(geo, 5, pad = "0"),
      candidate, party, votes,
      county_total_vote = as.numeric(totalVote),
      county_pct_exp_vote = as.numeric(pctExpVote),
      county_precincts_in = as.numeric(precinctsIn),
      county_precincts_of = as.numeric(precinctsOf),
      county_ts = lubridate::ymd_hms(ts, tz = "UTC", quiet = TRUE),
      race_last_updated = time, race_pct_in = pctIn, race_call = call,
      env = env, observed_at = observed_at) |>
    mutate(party_cbs = party, party = cbs_party(party), .after = party) |>
    left_join(fips_map, by = "county_fips") |>
    left_join(known, by = "county_cbs") |>
    relocate(county_cbs, county_name, .after = county_fips)

  dupes = count(cty, county_cbs, candidate, party) |> filter(n > 1)
  if (nrow(dupes) > 0) cli::cli_abort("Duplicate county x candidate rows in the CBS response ({nrow(dupes)}).")

  if (verbose) {
    n_cty = n_distinct(cty$county_cbs)
    cli::cli_alert_info("{n_cty} counties returned; {nrow(known)} counties in cbs_counties for {state}")
    if (n_cty < nrow(known)) cli::cli_alert_warning("Missing counties: {head(setdiff(known$county_name, cty$county_name), 10)}")
    # county sums should equal the race-level totals when both feeds are in sync
    race = cbsapi::cbs_election_data(CBS_ELECTION, state, contest, district = district, county = FALSE, env = env)
    sum_check = cty |>
      summarize(county_sum = sum(votes, na.rm = TRUE), .by = c(candidate, party)) |>
      full_join(race |> mutate(party = cbs_party(party)) |> select(candidate, party, race_total = votes),
                by = c("candidate", "party")) |>
      mutate(diff = county_sum - race_total)
    print(sum_check)
    if (any(is.na(sum_check$diff) | sum_check$diff != 0)) {
      cli::cli_alert_warning("County sums differ from race totals (feeds out of sync, or counties missing). Snapshot still saved.")
    } else cli::cli_alert_success("County sums match race totals.")
  }

  dir_out = file.path(dir_26, "cbs_api", env, tag)
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  f = file.path(dir_out, paste0(tag, "_", format(observed_at, "%Y%m%d_%H%M%S"), ".parquet"))
  nanoparquet::write_parquet(cty, f)
  if (verbose) cli::cli_alert_success("Saved {nrow(cty)} rows to {f}")
  cty
}

# ---- command line ------------------------------------------------------------------
# Runs only when this file is executed with Rscript, never when load.R sources it.
if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(purrr); library(readr); library(stringr)
  })
  for (f in c("paths.R", "races.R", "counties.R")) source(file.path("scripts/model/live/R", f))
  args = commandArgs(trailingOnly = TRUE)
  env = if ("--live" %in% args) "live" else "test"
  all = "--all" %in% args
  args = setdiff(args, c("--live", "--all"))
  d26 = find_26_general()
  races = if (all) races_from_priors(d26) else bind_rows(lapply(if (length(args)) args else "TX_gov", parse_race_id))
  out = lapply(seq_len(nrow(races)), function(i) {
    r = races[i, ]
    res = tryCatch({
      x = pull_cbs_race(r$state, r$contest, r$district, env = env, dir_26 = d26, verbose = TRUE)
      tibble(result = "ok", counties = n_distinct(x$county_cbs),
             pct_exp_vote = paste(range(x$county_pct_exp_vote, na.rm = TRUE), collapse = "-"),
             detail = paste(unique(paste0(x$candidate, " (", x$party, ")")), collapse = "; "))
    }, error = function(e) tibble(result = "FAILED", counties = NA_integer_, pct_exp_vote = "",
                                  detail = str_trunc(str_squish(str_replace_all(conditionMessage(e), "\\n|!|\u2139", " ")), 110)))
    bind_cols(select(r, race_id), res)
  }) |> bind_rows()
  if (nrow(races) > 1) {
    cli::cli_h1("Summary ({env} feed)")
    print(as.data.frame(out), right = FALSE, row.names = FALSE)
  }
  if (any(out$result != "ok")) quit(status = 1)
}
