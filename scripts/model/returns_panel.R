# ========================================
## General timestamped election-night returns panels, multi-state
##
## Generalizes `tx24_returns_panel.R`. Produces, per state, one row per
## (geo, method, timestamp) with dem / rep / oth votes -- the real release cadence.
##
## WHY THIS MATTERS: the unreported-set prior (the variance component that fixes late
## coverage) and delta_m are both computed ENTIRELY WITHIN a single state's scrape --
## counted vs final within (county, mode). Neither needs votehub or L2, so neither is
## limited by the precinct-id reconciliation that caps the covariate join at 17% in NC
## and 20% in OH. That is why these panels are built separately from backtest_panel.R.
##
## TWO INPUT SHAPES
##  A. {ST}/clean/*.csv -- ALREADY NORMALIZED by the pipeline's cleaning step:
##       state, race_id, race_name, candidate_name, candidate_party, jurisdiction,
##       precinct_id, virtual_precinct, timestamp, vote_mode, precinct_total
##     `vote_mode` is already canonical (Early Voting / Election Day / Absentee/Mail /
##     Provisional / Overvote-Undervote) and `candidate_party` is spelled out
##     ("Democrat"). Available for AZ, GA, MI, NC, PA.
##  B. {ST}/raw/*.csv -- the county-native shape TX uses:
##       race_id, race_name, vote_mode, precinct_id, precinct_total, candidate_name,
##       candidate_party, jurisdiction, timestamp
##     Raw county mode labels and party spellings, so it needs the (state, year)-keyed
##     lookups in scripts/model/lookups/. Available for AR, CO, SC, TX, WV.
##     NOTE TX's clean/ is EMPTY while NC's has 80 files -- the TX cleaning step was
##     never run. That pipeline hole is why TX needed hand-built lookups at all.
##
## Timestamps: clean/ stores UTC with a Z suffix; raw/ stores local with an offset.
## Both are converted to the state's LOCAL wall clock, because poll-close reasoning
## ("what was known at 19:30") is local.
##
## Output: scripts/model/panels/{ST}_returns.parquet
##         scripts/model/panels/returns_coverage.csv
##
## Run:  Rscript scripts/model/returns_panel.R           # every state with usable data
##       STATE=GA Rscript scripts/model/returns_panel.R
##       RACE=Governor STATE=NC Rscript scripts/model/returns_panel.R
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
})

# PATH_DROPBOX env var if set, else this machine's Dropbox (live/R/paths.R)
PATH_DROPBOX <- local({ source("scripts/model/live/R/paths.R", local = TRUE); find_dropbox() })
GEN    <- path.expand(file.path(PATH_DROPBOX, "24_general"))
OUTDIR <- "scripts/model/panels"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

# Default contest. President is the only one present in every scraped state, so it is
# the comparable choice across states. TX's own tx24_returns_panel.R uses US Senate;
# that panel is kept as-is and this one is additive.
RACE <- Sys.getenv("RACE", unset = "President")
YEAR <- 2024

# Local time zone per state, for converting the clean/ UTC timestamps
TZ <- c(AR = "America/Chicago",  AZ = "America/Phoenix", CA = "America/Los_Angeles",
        CO = "America/Denver",   GA = "America/New_York", MI = "America/Detroit",
        NC = "America/New_York", PA = "America/New_York", SC = "America/New_York",
        TX = "America/Chicago",  WV = "America/New_York")

# clean/ canonical vote_mode -> model method. "Overvote/Undervote" is ballot-level
# metadata, not votes, and is dropped (the archived scrapers bucketed it explicitly
# rather than letting it vanish silently).
MODE_CLEAN <- c("Early Voting" = "early", "Election Day" = "eday",
                "Absentee/Mail" = "mail", "Provisional" = "prov",
                "Overvote/Undervote" = "DROP")
# clean/ candidate_party -> dem / rep / oth. Anything unlisted is a real third-party or
# write-in vote and lands in `oth`; it is NEVER dropped, because dropping it would short
# the turnout denominator and bias every share upward.
PARTY_CLEAN <- c("Democrat" = "dem", "Democratic" = "dem", "DEM" = "dem",
                 "Republican" = "rep", "REP" = "rep")

norm_prec <- function(x) {
  x |> toupper() |> trimws() |>
    str_remove("^(PRECINCT|PCT)\\.?\\s*") |>
    str_remove_all("[^0-9A-Z]")
}

cli_h1("Multi-state timestamped returns panels -- {RACE} {YEAR}")

# ---- shape A: clean/ --------------------------------------------------------------
read_clean <- function(st) {
  fs <- list.files(file.path(GEN, st, "clean"), pattern = "\\.csv$", full.names = TRUE)
  if (!length(fs)) return(NULL)
  cli_inform("{st}: reading {length(fs)} clean file(s)")
  d <- map(fs, \(f) suppressWarnings(read_csv(
         f, col_types = cols(.default = "c"), progress = FALSE))) |> bind_rows()
  req <- c("race_name", "candidate_party", "jurisdiction", "precinct_id",
           "timestamp", "vote_mode", "precinct_total")
  if (!all(req %in% names(d))) {
    cli_alert_warning("{st}: clean/ missing {setdiff(req, names(d))} -- skipped")
    return(NULL)
  }
  d |>
    mutate(across(everything(), ~ replace_na(as.character(.x), ""))) |>
    transmute(
      jurisdiction, precinct_id, race_name,
      raw_mode  = vote_mode,
      raw_party = candidate_party,
      cand      = if ("candidate_name" %in% names(d)) candidate_name else "",
      votes     = suppressWarnings(as.numeric(precinct_total)),
      ts_utc    = timestamp,
      shape     = "clean"
    )
}

# ---- shape B: raw/, the county-native TX shape -------------------------------------
read_raw <- function(st) {
  fs <- list.files(file.path(GEN, st, "raw"), pattern = "\\.csv$", full.names = TRUE)
  if (!length(fs)) return(NULL)
  cli_inform("{st}: reading {length(fs)} raw file(s)")
  one <- function(f) {
    d <- suppressWarnings(read_csv(f, col_types = cols(.default = "c"), progress = FALSE))
    req <- c("race_name", "vote_mode", "precinct_id", "precinct_total",
             "candidate_name", "candidate_party", "jurisdiction", "timestamp")
    if (!all(req %in% names(d))) return(NULL)       # AZ's raw is a different wide shape
    select(d, all_of(req))
  }
  d <- map(fs, one) |> compact() |> bind_rows()
  if (!nrow(d)) return(NULL)
  d |>
    mutate(across(everything(), ~ replace_na(as.character(.x), ""))) |>
    transmute(jurisdiction, precinct_id, race_name,
              raw_mode = vote_mode, raw_party = candidate_party,
              cand = candidate_name,
              votes = suppressWarnings(as.numeric(precinct_total)),
              ts_utc = timestamp, shape = "raw")
}

# ---- lookups for shape B ----------------------------------------------------------
mode_map  <- read_csv("scripts/model/lookups/vote_mode_map.csv",
                      col_types = cols(.default = "c"))
party_map <- read_csv("scripts/model/lookups/party_map.csv",
                      col_types = cols(.default = "c"))

map_mode <- function(d, st) {
  if (d$shape[1] == "clean") {
    d |> mutate(method = unname(MODE_CLEAN[trimws(raw_mode)]))
  } else {
    mm <- mode_map |> filter(state == st, year == as.character(YEAR))
    d |> left_join(select(mm, raw_mode = raw_label, method), by = "raw_mode")
  }
}

map_party <- function(d, st) {
  if (d$shape[1] == "clean") {
    d |> mutate(party = unname(PARTY_CLEAN[trimws(raw_party)]))
  } else {
    pm <- party_map |> filter(state == st, year == as.character(YEAR))
    pv <- setNames(pm$party, pm$raw_label)
    d |> mutate(
      party = coalesce(
        unname(pv[trimws(raw_party)]),
        unname(pv[str_extract(cand, "^\\s*\\([A-Za-z]\\)")]),
        unname(pv[str_extract(cand, "^\\s*[A-Za-z]{3}(?=\\s)")])))
    }
}

# ---- per-state build ---------------------------------------------------------------
RE_RACE <- regex(RACE, ignore_case = TRUE)

build_one <- function(st) {
  d <- read_clean(st)
  if (is.null(d)) d <- read_raw(st)
  if (is.null(d) || !nrow(d)) { cli_alert_warning("{st}: no usable input"); return(NULL) }

  n_all <- nrow(d)
  d <- d |> filter(str_detect(race_name, RE_RACE))
  if (!nrow(d)) {
    cli_alert_warning("{st}: no rows matching race {.val {RACE}} \\
                       (have: {paste(head(unique(read_clean(st)$race_name), 5), collapse=' / ')})")
    return(NULL)
  }
  # a blank precinct_id is a county-level aggregate, not a precinct
  d <- d |> filter(trimws(precinct_id) != "", trimws(cand) != "" | shape == "clean")

  d <- map_mode(d, st)
  unmapped <- d |> filter(is.na(method)) |> count(raw_mode, sort = TRUE)
  d <- map_party(d, st)
  d <- d |> mutate(party = coalesce(party, "oth"),
                   votes = coalesce(votes, 0))

  d <- d |> filter(!is.na(method), method != "DROP")
  if (!nrow(d)) { cli_alert_warning("{st}: every row dropped on mode"); return(NULL) }

  # ---- MODE-REPLICATION GUARD ----------------------------------------------------
  # Some counties report the precinct's TOTAL on every mode row instead of splitting it.
  # Measured in GA: 64 of 159 counties do this (e.g. Stephens County 1 shows Trump =
  # 10,632 for Election Day AND Absentee/Mail AND Provisional AND Early Voting), while
  # 95 split correctly (Appling 1B: eday 23 / mail 10 / prov 0 / early 75). Summing the
  # replicated ones across modes inflates them 4x -- GA's raw statewide sum came to
  # 14.1M against an actual turnout of ~5.27M, and the per-candidate totals came out
  # near-equal, which is how it was caught.
  # Rule: a county whose (precinct, candidate) groups are mostly all-modes-identical is
  # collapsed to a single `all` row carrying the value ONCE. That keeps its turnout and
  # share usable while removing the inflation, and mode work simply filters out `all`
  # rather than silently consuming garbage.
  # Evaluate on each jurisdiction's LAST snapshot only. Pooling snapshots defeats the
  # test: the four mode rows do not all appear in every snapshot, so a replicating
  # county's per-mode SUMS differ even though its per-snapshot values are identical.
  # (Diagnosed the hard way -- the pooled version scored 0 of 159 GA counties while a
  # single-snapshot check scored 64.)
  rep_cty <- d |>
    filter(ts_utc == max(ts_utc), .by = jurisdiction) |>
    summarize(v = sum(votes), .by = c(jurisdiction, precinct_id, cand, method)) |>
    filter(max(v) > 0, .by = c(jurisdiction, precinct_id, cand)) |>
    summarize(replicated = n_distinct(v) == 1 & n() > 1,
              .by = c(jurisdiction, precinct_id, cand)) |>
    summarize(frac_rep = mean(replicated), .by = jurisdiction) |>
    mutate(mode_reliable = frac_rep <= 0.5)
  cli_inform("{st}: replication check -- {nrow(rep_cty)} counties scored, \\
              {sum(!rep_cty$mode_reliable)} flagged mode-UNRELIABLE \\
              (mean frac_rep {round(mean(rep_cty$frac_rep), 3)})")
  d <- d |> left_join(select(rep_cty, jurisdiction, mode_reliable), by = "jurisdiction") |>
    mutate(mode_reliable = coalesce(mode_reliable, TRUE))
  # EXCLUDE the unreliable counties rather than trying to repair them.
  # I first tried collapsing them to one `all` row via max() over the modes. That made
  # things WORSE, and the numbers say so: GA's two-party Dem was 48.9% uncollapsed
  # (matching the actual result) and 41.62% after collapsing, with the vote total moving
  # from 2x too high to 17% too low. So the replicated rows are not simply "the total
  # repeated four times" -- their structure is not yet understood well enough to repair,
  # and a wrong repair is worse than a smaller panel.
  # Excluding leaves a panel that is internally consistent on both turnout and share,
  # covering fewer counties. `jur_mode_bad` records what was dropped.
  n_before <- nrow(d)
  d <- d |> filter(mode_reliable)
  if (nrow(d) < n_before)
    cli_alert_info("{st}: excluded {format(n_before - nrow(d), big.mark=',')} row(s) in \\
                    {sum(!rep_cty$mode_reliable)} mode-unreliable county(ies)")
  if (!nrow(d)) { cli_alert_warning("{st}: no mode-reliable counties"); return(NULL) }

  tzs <- TZ[[st]]; if (is.null(tzs)) tzs <- "UTC"
  panel <- d |>
    mutate(
      # clean/ is UTC with a Z; raw/ carries an explicit offset. Both -> local wall clock.
      ts = as.POSIXct(str_replace(ts_utc, "Z$", ""), tz = "UTC",
                      format = if (str_detect(ts_utc[1], "T")) "%Y-%m-%dT%H:%M:%S"
                               else "%Y-%m-%d %H:%M:%S"),
      ts_local = as.POSIXct(format(ts, tz = tzs), tz = "UTC"),
      geo = paste(toupper(trimws(jurisdiction)), norm_prec(precinct_id), sep = "|")
    ) |>
    filter(!is.na(ts_local)) |>
    summarize(votes = sum(votes),
              .by = c(geo, jurisdiction, method, mode_reliable, ts_local, party)) |>
    pivot_wider(names_from = party, values_from = votes, values_fill = 0)
  for (p in c("dem", "rep", "oth")) if (!p %in% names(panel)) panel[[p]] <- 0
  panel <- panel |>
    transmute(state = st, geo, jurisdiction, method, mode_reliable,
              timestamp = format(ts_local, "%Y-%m-%d %H:%M:%S"), dem, rep, oth) |>
    arrange(geo, method, timestamp)

  # election-night window, in local time
  N0 <- as.POSIXct("2024-11-05 17:00:00", tz = "UTC")
  N1 <- as.POSIXct("2024-11-06 08:00:00", tz = "UTC")
  tsl <- as.POSIXct(panel$timestamp, tz = "UTC")
  night <- panel[tsl >= N0 & tsl <= N1, ]
  fin <- panel |> mutate(j = jurisdiction) |> filter(timestamp == max(timestamp), .by = j)

  cov <- tibble(
    state = st, shape = d$shape[1], race = RACE,
    rows_in = n_all, panel_rows = nrow(panel),
    jurisdictions = n_distinct(panel$jurisdiction),
    precincts = n_distinct(panel$geo),
    timestamps = n_distinct(panel$timestamp),
    night_timestamps = n_distinct(night$timestamp),
    night_jurisdictions = n_distinct(night$jurisdiction),
    modes = paste(sort(unique(panel$method)), collapse = "/"),
    unmapped_modes = nrow(unmapped),
    # counties whose mode split is trustworthy -- the only ones usable for mode work
    jur_mode_ok = n_distinct(panel$jurisdiction[panel$mode_reliable]),
    jur_mode_bad = n_distinct(panel$jurisdiction[!panel$mode_reliable]),
    final_votes = sum(fin$dem + fin$rep + fin$oth),
    final_twop_dem = round(100 * sum(fin$dem) / pmax(sum(fin$dem) + sum(fin$rep), 1), 2)
  )
  if (nrow(unmapped)) {
    cli_alert_warning("{st}: {nrow(unmapped)} unmapped mode label(s): \\
                       {paste(head(unmapped$raw_mode, 5), collapse=' / ')}")
  }
  list(panel = panel, cov = cov)
}

want   <- Sys.getenv("STATE", unset = "")
states <- if (nzchar(want)) want else
  sort(list.dirs(GEN, recursive = FALSE, full.names = FALSE) |>
         keep(\(x) nchar(x) == 2 & x == toupper(x)))
cli_inform("candidate states: {paste(states, collapse=' ')}")

res <- map(states, \(st) tryCatch(build_one(st), error = function(e) {
  cli_alert_danger("{st}: {conditionMessage(e)}"); NULL
})) |> compact()

cov_tbl <- bind_rows(map(res, "cov"))
cli_h2("returns panels")
print(as.data.frame(cov_tbl), row.names = FALSE)

for (r in res) {
  write_parquet(r$panel, file.path(OUTDIR, paste0(r$cov$state, "_returns.parquet")))
}
write_csv(cov_tbl, file.path(OUTDIR, "returns_coverage.csv"))
cli_alert_success("wrote {length(res)} returns panel(s) to {OUTDIR}")
cli_inform(c(
  "`night_timestamps` is what makes a state usable for cadence work -- a state with 0",
  "has only pre- or post-election scrapes. `night_jurisdictions` bounds how much of the",
  "state was actually reporting on the night."
))
