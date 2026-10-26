# ========================================
## make_reporting_levels.R -- build lookups/reporting_levels.csv: for every
## (state, county, method), does that method report at PRECINCT level or at COUNTY/SITE
## level?
##
## Why this exists. cell_counts() (live/R/returns.R) joins FROM the precinct spine, so a
## returns row on a non-precinct geo is dropped. Two states send a third of their vote on
## such geos:
##   NC 2024 Governor   1,743,117 of 5,591,547 votes (31.2%) on virtual precincts --
##                      early-voting SITE buckets (nc_032_EV DUKE) and county ABSENTEE
##                      buckets (nc_003_ABSENTEE).
##   AK Aug-2026 primary  153,478 of 493,117 votes (31.1%) on the per-district
##                      NN-EARLY / NN-ABSENTEE / NN-QUESTION buckets.
## returns_dropped() detects that at runtime. This table is the PRE-ELECTION EXPECTATION
## it is checked against, so a mismatch on the night is a known deviation rather than a
## discovery.
##
## Usage, from the cbs-enr folder:
##   Rscript scripts/model/make_reporting_levels.R            # all six states
##   Rscript scripts/model/make_reporting_levels.R NC AK      # just these
##
## Writes scripts/model/lookups/reporting_levels.csv and prints a per-state summary.
## Read scripts/model/lookups/README.md for the column and provenance semantics.
# ========================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
})
source("scripts/model/live/R/paths.R")

STATES = c("AK", "CA", "FL", "MI", "NC", "TX")
YEAR   = 2026
OUT    = "scripts/model/lookups/reporting_levels.csv"

# A state's filed counties can stand in for the ones that filed nothing only if enough of
# them filed. See README: with 6 of 254 TX counties on file, the other 248 are `unknown`,
# not "precinct".
MIN_STATE_COVERAGE = 1 / 3   # share of a state's counties needing direct evidence
MIN_METHOD_SHARE   = 2 / 3   # share of evidence counties carrying a method, to infer it
MIN_LEVEL_SHARE    = 2 / 3   # agreement among them on the level, to infer it

# ---- the bucket-label vocabulary --------------------------------------------------
# A reporting bucket is a geo whose precinct id is not a precinct: an early-voting site,
# a county mail/absentee pile, a provisional pile, a transfer pile. Matching order is the
# one lookups/README.md already fixes for vote_mode_map.csv -- mail -> prov -> early ->
# eday -- so `EV MAIL` reaches mail and `EARLY VOTING PROVISIONAL` reaches prov.
#
# VALIDATED, not guessed: scored against `virtual_precinct` on all 2,908 distinct
# (county, precinct_id) labels in NC 2024 Governor it gets 249 of 250 virtual labels
# (missing only Hoke's `ROCKFISH`, an EV site named after a place) with 2 false positives
# (Caswell `PROVI` and Henderson `EV`, both of which look like buckets and are flagged
# virtual = FALSE upstream). Re-score it with the block at the bottom of this file after
# any edit.
BUCKET_RX = c(
  mail     = "(^|[^A-Z])(ABS|ABSEN[A-Z]*|MAIL|BY[ -]?MAIL)([^A-Z]|$)",
  prov     = "(^|[^A-Z])(PROV[A-Z]*|PROVSIONAL|QUESTION[A-Z]*|CHALLENGE[A-Z]*)([^A-Z]|$)",
  early    = "(^|[^A-Z])(EV|EARLY[A-Z]*|ONE ?STOP|BOE|CURBSIDE)([^A-Z]|$)|^EV[A-Z]{2,4}$",
  transfer = "(^|[^A-Z])(TRANS[A-Z]*)([^A-Z]|$)"
)

# NA for anything that looks like a precinct.
bucket_class = function(label) {
  u = toupper(label)
  case_when(str_detect(u, BUCKET_RX[["mail"]])     ~ "mail",
            str_detect(u, BUCKET_RX[["prov"]])     ~ "prov",
            str_detect(u, BUCKET_RX[["early"]])    ~ "early",
            str_detect(u, BUCKET_RX[["transfer"]]) ~ "transfer",
            TRUE ~ NA_character_)
}

# 2024 clean `vote_mode` -> canonical method. DROP is not a vote (CONTRACT.md section 7).
MODE_2024 = c("Election Day" = "eday", "Early Voting" = "early", "Absentee/Mail" = "mail",
              "Provisional" = "prov", "Overvote/Undervote" = NA_character_)

# ---- paths ------------------------------------------------------------------------
dir_26 = find_26_general()
dir_24 = file.path(dirname(dir_26), "24_general")
dir_in = file.path(dir_26, "input_data", "model_inputs")

# county universe: counties.csv is the complete list; the geo prefix is county_cbs,
# zero-padded to 3 (verified against {ST}_precinct.parquet for all six states).
county_frame = function(state) {
  readr::read_csv(file.path(dir_in, paste0(state, "_counties.csv")), show_col_types = FALSE) |>
    transmute(state = state, county_fips = str_pad(as.character(county_fips), 5, pad = "0"),
              county_cbs = as.integer(county_cbs), county_name = toupper(county_name),
              pref = sprintf("%03d", as.integer(county_cbs)))
}

spine_prefixes = function(state) {
  p = file.path(dir_in, paste0(state, "_precinct.parquet"))
  if (!file.exists(p)) return(character())
  unique(str_match(as.character(read_pq(p)$geo), "^[a-z]{2}_([0-9]+)_")[, 2])
}

# ---- source 1: the 2026 zero files ------------------------------------------------
# 26_general/{ST}/clean/{ST}*_latest.parquet. Schema: state, election, jurisdiction,
# contest, district, candidate, party, time, stage, method, geo, votes. Nominally all
# zeros, but AK / MI / FL *_latest now carry real primary votes, so vote mass is recorded
# where it exists and is NA where it does not. The level verdict never depends on it: the
# zero files are a geo-vocabulary preview, and a bucket geo is a bucket geo at zero votes.
read_zero_2026 = function(state) {
  fs = Sys.glob(file.path(dir_26, state, "clean", paste0(state, "*_latest.parquet")))
  if (length(fs) == 0) return(NULL)
  z = map_dfr(fs, \(f) read_pq(f) |>
                mutate(across(c(geo, method), as.character)) |>
                select(geo, method, votes)) |>
    mutate(pref  = str_match(geo, "^[a-z]{2}_([0-9]+)_")[, 2],
           label = str_replace(geo, "^[a-z]{2}_[0-9]+_", ""),
           bk    = bucket_class(label))
  if (anyNA(z$pref)) stop(state, ": ", sum(is.na(z$pref)), " zero-file geos have no county prefix")

  geo_level = z |> distinct(pref, geo, label, bk)
  # votes are crossed over every method, so a bucket's own method is its label's class
  v = z |> summarize(votes = sum(votes, na.rm = TRUE), .by = c(pref, geo, method))

  # (a) a method reports at county/site level in a county when that county has a bucket
  #     geo of that class. `transfer` is carried as its own method value: it is not in
  #     enightmodels::VOTE_METHODS, its votes land in eday, and NC 2024 measures eday as
  #     100% precinct-allocated -- so a transfer pile must not make eday look site-level.
  buckets = geo_level |> filter(!is.na(bk)) |>
    summarize(n_geo_bucket = n_distinct(geo),
              bucket_labels = paste(sort(unique(label)), collapse = "; "),
              .by = c(pref, bk)) |>
    rename(method = bk)
  # (b) every (county, method) the file actually carries, plus any transfer row from (a)
  carried = z |> filter(is.na(bk)) |> summarize(n_geo_precinct = n_distinct(geo),
                                                votes_file = sum(votes, na.rm = TRUE),
                                                .by = c(pref, method))
  bucket_votes = v |> inner_join(select(geo_level, geo, bk), by = "geo") |>
    filter(!is.na(bk), method == bk) |>
    summarize(votes_bucket_file = sum(votes), .by = c(pref, bk)) |>
    rename(method = bk)

  full_join(carried, buckets, by = c("pref", "method")) |>
    left_join(bucket_votes, by = c("pref", "method")) |>
    mutate(n_geo_bucket = coalesce(n_geo_bucket, 0L),
           level_zero_2026 = if_else(n_geo_bucket > 0, "county_site", "precinct"))
}

# ---- source 2: the 2024 actuals ---------------------------------------------------
# 24_general/{ST}/clean/{ST}*_latest.parquet -- a DIFFERENT schema from 2026, carrying
# `virtual_precinct` (dropped by the 2026 normalization) plus vote_mode / precinct_total /
# precinct_id / jurisdiction. `virtual_precinct` is the ground truth for site-vs-precinct,
# and it comes with vote mass, so here the rule is the measured share: a county x method
# is site-reporting when more than half its votes sit on virtual precincts. On NC that is
# a clean binary -- 67 counties at 0.0%, 33 at 100.0%, nothing in between -- so the
# threshold is not doing any work it could get wrong.
read_actual_2024 = function(state, cty) {
  fs = Sys.glob(file.path(dir_24, state, "clean", paste0(state, "*_latest.parquet")))
  if (length(fs) == 0) return(NULL)
  d = map_dfr(fs, \(f) read_pq(f) |>
                select(jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total))
  unk = setdiff(unique(d$vote_mode), names(MODE_2024))
  if (length(unk)) stop(state, " 2024: unmapped vote_mode(s): ", paste(unk, collapse = ", "))
  d = d |>
    mutate(method = unname(MODE_2024[vote_mode]), jkey = toupper(jurisdiction)) |>
    filter(!is.na(method))
  miss = setdiff(unique(d$jkey), cty$county_name)
  if (length(miss)) stop(state, " 2024: jurisdictions not in counties.csv: ",
                         paste(head(miss, 5), collapse = ", "))
  d |>
    summarize(votes_bucket = sum(precinct_total[virtual_precinct], na.rm = TRUE),
              votes_precinct = sum(precinct_total[!virtual_precinct], na.rm = TRUE),
              labels_24 = paste(sort(unique(precinct_id[virtual_precinct])), collapse = "; "),
              .by = c(jkey, method)) |>
    mutate(tot = votes_bucket + votes_precinct,
           share_bucket_2024 = if_else(tot > 0, votes_bucket / tot, NA_real_),
           level_actual_2024 = case_when(is.na(share_bucket_2024) ~ NA_character_,
                                         share_bucket_2024 > 0.5 ~ "county_site",
                                         TRUE ~ "precinct")) |>
    select(county_name = jkey, method, votes_precinct, votes_bucket, share_bucket_2024,
           level_actual_2024, labels_24)
}

# ---- build one state --------------------------------------------------------------
build_state = function(state) {
  cty = county_frame(state)
  sp  = spine_prefixes(state)
  z   = read_zero_2026(state)
  a24 = read_actual_2024(state, cty)

  # the UNION of the two sources, not the newest: NC's two sources disagree in BOTH
  # directions, and a county that filed no 2026 zero file can still have 2024 evidence.
  d26 = if (is.null(z)) tibble(county_name = character(), method = character(),
                               level_zero_2026 = character(), n_geo_precinct = integer(),
                               n_geo_bucket = integer(), bucket_labels = character(),
                               votes_file = numeric(), votes_bucket_file = numeric())
        else z |> inner_join(select(cty, pref, county_name), by = "pref") |>
          select(county_name, method, level_zero_2026, n_geo_precinct, n_geo_bucket,
                 bucket_labels, votes_file, votes_bucket_file)
  d24 = if (is.null(a24)) tibble(county_name = character(), method = character(),
                                 level_actual_2024 = character(), votes_precinct = numeric(),
                                 votes_bucket = numeric(), share_bucket_2024 = numeric(),
                                 labels_24 = character())
        else select(a24, county_name, method, level_actual_2024, votes_precinct,
                    votes_bucket, share_bucket_2024, labels_24)
  direct = full_join(d26, d24, by = c("county_name", "method"))

  ev_counties = unique(direct$county_name)
  coverage = length(ev_counties) / nrow(cty)

  # ---- source 3: the state's modal pattern among the counties that did file --------
  modal = direct |>
    filter(!is.na(level_zero_2026) | !is.na(level_actual_2024)) |>
    mutate(lv = coalesce(level_zero_2026, level_actual_2024)) |>
    summarize(n_cty = n_distinct(county_name),
              lv_mode = names(sort(table(lv), decreasing = TRUE))[1],
              lv_share = max(table(lv)) / n(), .by = method) |>
    mutate(method_share = n_cty / length(ev_counties)) |>
    filter(method_share >= MIN_METHOD_SHARE, lv_share >= MIN_LEVEL_SHARE)

  use_modal = coverage >= MIN_STATE_COVERAGE && nrow(modal) > 0
  gap = setdiff(cty$county_name, ev_counties)
  inferred = if (length(gap) == 0) NULL else if (use_modal) {
    tidyr::expand_grid(county_name = gap, select(modal, method, lv_mode)) |>
      transmute(county_name, method, level_modal = lv_mode)
  } else {
    tibble(county_name = gap, method = "*", level_modal = NA_character_)
  }

  merged = bind_rows(direct, inferred)
  if (!"level_modal" %in% names(merged)) merged$level_modal = NA_character_
  out = merged |>
    mutate(
      level_priority = coalesce(level_zero_2026, level_actual_2024, level_modal, "unknown"),
      level = case_when(
        level_zero_2026 == "county_site" | level_actual_2024 == "county_site" ~ "county_site",
        !is.na(level_zero_2026) | !is.na(level_actual_2024) ~ "precinct",
        TRUE ~ coalesce(level_modal, "unknown")),
      provenance = case_when(!is.na(level_zero_2026)   ~ "zero_2026",
                             !is.na(level_actual_2024) ~ "actual_2024",
                             !is.na(level_modal)       ~ "state_modal",
                             TRUE ~ "unknown"),
      sources = case_when(
        !is.na(level_zero_2026) & !is.na(level_actual_2024) ~ "zero_2026+actual_2024",
        !is.na(level_zero_2026) ~ "zero_2026",
        !is.na(level_actual_2024) ~ "actual_2024",
        !is.na(level_modal) ~ "state_modal",
        TRUE ~ "none"),
      disagree = !is.na(level_zero_2026) & !is.na(level_actual_2024) &
        level_zero_2026 != level_actual_2024) |>
    left_join(select(cty, county_name, county_fips, county_cbs, pref), by = "county_name") |>
    transmute(state = state, year = YEAR, county_fips, county_cbs, county_name, method,
              level, provenance, level_priority, level_zero_2026, level_actual_2024,
              sources, disagree,
              n_geo_precinct = coalesce(as.integer(n_geo_precinct), NA_integer_),
              n_geo_bucket = coalesce(as.integer(n_geo_bucket), NA_integer_),
              votes_precinct_2026 = votes_file, votes_bucket_2026 = votes_bucket_file,
              votes_precinct_2024 = votes_precinct, votes_bucket_2024 = votes_bucket,
              share_bucket_2024 = round(share_bucket_2024, 4),
              in_spine = pref %in% sp,
              state_evidence_coverage = round(coverage, 4),
              bucket_labels = str_trunc(coalesce(bucket_labels, labels_24, ""), 300)) |>
    arrange(county_cbs, method)

  attr(out, "coverage") = coverage
  attr(out, "use_modal") = use_modal
  out
}

# ---- run ---------------------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)
states = if (length(args)) toupper(args) else STATES
bad = setdiff(states, STATES)
if (length(bad)) stop("Not a precinct-input state: ", paste(bad, collapse = ", "),
                      " (NY is statewide-only and has no precinct inputs)")

tabs = set_names(states) |> map(\(st) { cli::cli_h2(st); build_state(st) })
res = bind_rows(tabs)

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(res, OUT, na = "")
cli::cli_alert_success("Wrote {nrow(res)} rows to {OUT}")

cli::cli_h1("rows by state x provenance")
print(res |> count(state, provenance) |>
        pivot_wider(names_from = provenance, values_from = n, values_fill = 0L) |>
        as.data.frame())
cli::cli_h1("rows by state x level")
print(res |> count(state, level) |>
        pivot_wider(names_from = level, values_from = n, values_fill = 0L) |>
        as.data.frame())
cli::cli_h1("county/site-reporting (state, method) counts")
print(res |> filter(level == "county_site") |> count(state, method) |> as.data.frame())
cli::cli_h1("counties with level == unknown")
unk = res |> filter(level == "unknown") |> distinct(state, county_name, in_spine)
print(unk |> count(state, in_spine) |> as.data.frame())
for (st in states) {
  u = filter(unk, state == st)
  if (nrow(u)) cli::cli_alert_info(
    "{st}: {nrow(u)} unknown ({sum(u$in_spine)} of them modelled, i.e. in the spine): {paste(head(u$county_name, 12), collapse = ', ')}{if (nrow(u) > 12) ', ...' else ''}")
}
cli::cli_h1("sources disagree (2026 zero file vs 2024 actuals)")
print(res |> filter(disagree) |>
        select(state, county_name, method, level_zero_2026, level_actual_2024) |>
        as.data.frame())

# ---- re-score the bucket classifier against 2024 virtual_precinct ------------------
# Run after any edit to BUCKET_RX. NC is the only state with both a statewide 2024 clean
# file and site-level reporting, so it is the only place this can be scored.
score_classifier = function() {
  p = file.path(dir_24, "NC", "clean", "NC_latest.parquet")
  if (!file.exists(p)) return(invisible(NULL))
  lab = read_pq(p) |> filter(race_name == "Governor") |>
    distinct(jurisdiction, precinct_id, virtual_precinct) |>
    mutate(flag = !is.na(bucket_class(precinct_id)))
  cli::cli_h1("BUCKET_RX scored on NC 2024 virtual_precinct")
  print(table(virtual = lab$virtual_precinct, flagged = lab$flag))
  cli::cli_alert_info("missed: {paste(lab$precinct_id[lab$virtual_precinct & !lab$flag], collapse = ', ')}")
  cli::cli_alert_info("false positives: {paste(lab$precinct_id[!lab$virtual_precinct & lab$flag], collapse = ', ')}")
}
if ("NC" %in% states) score_classifier()
