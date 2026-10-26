# ========================================
## returns.R -- what has been counted so far, in one standard shape for every model.
##
## STANDARD RETURNS ("long"): one row per race x precinct x method x candidate
##   race_id, state, election, geo, method, candidate, party, votes, stage, complete, time
##   party     "dem", "rep", or anything else (treated as third party)
##   votes     votes counted so far (integer)
##   stage     the feed's reporting stage for the precinct, 0 / 4 / 6 (CONTRACT.md section 8)
##   complete  TRUE once this precinct x method cell is fully counted
## Every precinct x method x candidate the race will ever have should be present, with 0
## votes where nothing is in yet (as in a zero file).
##
## Two ways in:
##   returns_from_sim()   a checkpoint of one of Ethan's simulated nights (26_sim/<scenario>)
##   returns_from_live()  the live feed in the CONTRACT.md section 8 shape
## and one way down to the precinct x method counts the swing model uses:
##   cell_counts()
## plus the drop diagnostic that watches it:
##   returns_dropped()  /  report_dropped()
# ========================================

# ---- simulated nights -------------------------------------------------------------
# sim: list(truth = TX_truth.parquet, sched = TX_schedule.parquet), read by read_sim().
# A cell's counted votes are its final votes x the share of the cell counted by `time`
# (cum_frac); censored precincts never report. Same rules as counted_at() / frac_at() in
# scripts/model/tests/test_sim_night.R.
#
# round_by: the original harness rounds counted votes in two different ways --
#   "candidate"  round each candidate's votes (what model_cory got; also what a real feed
#                looks like, since every candidate's count is a whole number). Default.
#   "party"      add up dem / rep / third-party first, then round (what the swing model
#                got). Only differs when a party has more than one candidate (e.g. a
#                minor-party candidate plus write-ins), by at most 1 vote per cell.
read_sim = function(dir_sc, tz = "America/New_York") {
  list(
    truth = read_pq(file.path(dir_sc, "TX_truth.parquet")) |>
      mutate(across(c(geo, county_cbs), as.character)),
    sched = read_pq(file.path(dir_sc, "TX_schedule.parquet")) |>
      mutate(geo = as.character(geo), t = lubridate::with_tz(t, tz))
  )
}

# the checkpoints the sim tests refit at: every 15 minutes 8pm-4am ET, then four later
# times (same as scripts/model/tests/test_sim_night.R)
sim_checkpoints = function(tz = "America/New_York") {
  c(seq(as.POSIXct("2026-11-03 20:00", tz = tz), as.POSIXct("2026-11-04 04:00", tz = tz), by = "15 min"),
    as.POSIXct(c("2026-11-04 12:00", "2026-11-05 12:00", "2026-11-07 12:00", "2026-11-10 12:00"), tz = tz))
}

sim_frac_at = function(sim, time) {
  landed = filter(sim$sched, !is.na(t), !is.na(cum_frac), t <= time)
  if (nrow(landed) == 0) return(tibble(geo = character(), method = character(), frac = numeric()))
  summarize(landed, frac = max(cum_frac), .by = c(geo, method))
}

returns_from_sim = function(sim, race_id, time, geos = NULL, round_by = c("candidate", "party")) {
  round_by = match.arg(round_by)
  tr = filter(sim$truth, contest == race_id)
  if (!is.null(geos)) tr = filter(tr, geo %in% geos)
  if (round_by == "party") {
    tr = tr |>
      mutate(party = if_else(party %in% c("dem", "rep"), party, "oth")) |>
      summarize(votes = sum(votes), .by = c(state, election, contest, geo, method, party)) |>
      mutate(candidate = party)
  }
  censored = distinct(sim$truth, geo, censored)
  fr = sim_frac_at(sim, time) |>
    right_join(distinct(tr, geo, method), by = c("geo", "method")) |>
    left_join(censored, by = "geo") |>
    mutate(frac = if_else(coalesce(censored, FALSE), 0, coalesce(frac, 0))) |>
    # precinct-level stage, as a feed reports it: 6 when every method is fully counted,
    # 4 when anything is in, 0 otherwise
    mutate(stage = case_when(all(frac >= 1) ~ 6L, any(frac > 0) ~ 4L, TRUE ~ 0L), .by = geo) |>
    select(geo, method, frac, stage)
  tr |>
    left_join(fr, by = c("geo", "method")) |>
    transmute(race_id = contest, state, election, geo, method, candidate, party,
              votes = round(votes * frac), stage, complete = frac >= 1, frac, time = time)
}

# ---- the live feed ----------------------------------------------------------------
# live: CONTRACT.md section 8 -- geo, race_id, candidate_id, method, votes, stage,
#       observed_at; append-only, one snapshot per scrape.
# candidates: CONTRACT.md section 5 -- race_id, candidate_id, name, party.
# Takes each row's latest value at or before `at` (default: the latest scrape).
# `geo` must already be reconciled to the precinct spine and `method` already canonical
# (lookups/vote_mode_map.csv); this function does neither.
# NOT YET TESTED AGAINST A REAL FEED: as of CONTRACT.md 2026-10-18 the TX live ids join
# to the spine at 0.8%, so the id crosswalk has to be fixed upstream first.
returns_from_live = function(live, candidates, state, election, at = NULL, complete_stage = 6L) {
  if (!is.null(at)) live = filter(live, observed_at <= at)
  live |>
    slice_max(observed_at, n = 1, with_ties = FALSE, by = c(geo, race_id, candidate_id, method)) |>
    left_join(select(candidates, race_id, candidate_id, candidate = name, party),
              by = c("race_id", "candidate_id")) |>
    transmute(race_id, state = state, election = election, geo, method, candidate, party,
              votes, stage, complete = stage >= complete_stage, frac = NA_real_, time = observed_at)
}

# ---- drop detection ---------------------------------------------------------------
# cell_counts() joins FROM `cells`, so every returns row whose (geo, method) is not a cell
# is discarded. Two distinct loss modes, and before this diagnostic both were silent:
#
#   unknown_geo     the `geo` is not in `cells` at all. Either a non-precinct reporting
#                   bucket -- an early-voting SITE (`nc_032_EV DUKE`, `ak_004_04-early`)
#                   or a county-level mail/provisional bucket (`nc_003_ABSENTEE`,
#                   `ak_001_01-ABSENTEE`) -- or a precinct id that was never reconciled to
#                   the spine. scripts/model/lookups/reporting_levels.csv is the
#                   pre-election expectation of which (state, county, method) send these.
#   unknown_method  the `geo` is a known precinct but the method is not one of its cells.
#                   Every state's feed carries `prov` (and CA/MI/FL/TX carry `all`) while
#                   build_cells() defaults to early/eday/mail, so these are votes on
#                   precincts we do model, in a bucket we did not ask for.
#
# This is not an edge case and not a rounding error. Measured on NC 2024 Governor,
# 1,743,117 of 5,591,547 votes -- 31.2% -- sat on non-precinct geo ids; on the AK August
# 2026 primary (26_general/AK/clean/AK_latest.parquet, all three contests), 153,478 of
# 493,117 -- 31.1% -- sat on the per-district `NN-EARLY`, `NN-ABSENTEE` and `NN-QUESTION`
# buckets, which is AK's ENTIRE early, mail and provisional vote. Both vanished with no
# counter and no warning.
#
# Returns one row per (drop, method): n_geo, n_rows, votes, share of all returns votes.
# `attr(., "geos")` carries the distinct dropped geos (drop, method, geo, votes) so a
# caller can name them; `attr(., "total_votes")` the denominator.
returns_dropped = function(cells, returns) {
  known_geo = unique(as.character(cells$geo))
  known_cell = paste0(as.character(cells$geo), "\r", as.character(cells$method))
  r = returns |>
    transmute(geo = as.character(geo), method = as.character(method), votes,
              drop = case_when(!geo %in% known_geo ~ "unknown_geo",
                               !paste0(geo, "\r", method) %in% known_cell ~ "unknown_method",
                               TRUE ~ NA_character_)) |>
    filter(!is.na(drop))
  tot = sum(returns$votes, na.rm = TRUE)
  geos = r |> summarize(votes = sum(votes, na.rm = TRUE), .by = c(drop, method, geo)) |>
    arrange(drop, method, desc(votes))
  out = r |>
    summarize(n_geo = n_distinct(geo), n_rows = n(), votes = sum(votes, na.rm = TRUE),
              .by = c(drop, method)) |>
    mutate(share = if (tot > 0) votes / tot else NA_real_) |>
    arrange(drop, desc(votes))
  structure(out, geos = geos, total_votes = tot)
}

# Prints nothing when nothing is dropped. Deliberately NOT behind a flag or a verbosity
# option: the failure mode this exists to catch is a silent 31% loss, so the only way it
# can do its job is to be on by default.
report_dropped = function(d, label = NULL, n_show = 6) {
  if (nrow(d) == 0) return(invisible(d))
  tot = attr(d, "total_votes")
  v = sum(d$votes)
  tag = if (is.null(label)) "" else paste0(" [", label, "]")
  n_geo = n_distinct(attr(d, "geos")$geo[attr(d, "geos")$drop == "unknown_geo"])
  n_cell = sum(d$n_rows[d$drop == "unknown_method"])
  pct = function(x) if (is.na(x)) "n/a" else sprintf("%.1f%%", 100 * x)
  cli::cli_alert_warning(paste0(
    "cell_counts() dropped {prettyNum(v, big.mark = ',')} of ",
    "{prettyNum(tot, big.mark = ',')} returns votes ",
    "({pct(if (tot > 0) v / tot else NA_real_)})", tag, ": ",
    "{n_geo} geo{?s} absent from cells, {n_cell} row{?s} on an unmodelled method."))
  for (i in seq_len(nrow(d))) {
    cli::cli_alert_info(paste0(
      "  {d$drop[i]} / {d$method[i]}: {d$n_geo[i]} geo{?s}, ",
      "{prettyNum(d$votes[i], big.mark = ',')} votes ",
      "({pct(d$share[i])} of all returns votes so far)"))
  }
  ex = attr(d, "geos") |> filter(drop == "unknown_geo") |> head(n_show)
  if (nrow(ex)) cli::cli_alert_info("  e.g. {.val {ex$geo}}")
  invisible(d)
}

# ---- down to precinct x method counts ---------------------------------------------
# One race's returns -> one row per row of `cells`, in the same order:
#   c_dem, c_rep, c_oth, c_tot, complete
# Cells with no returns rows get zeros and complete = FALSE.
# Returns rows that are NOT a cell are dropped -- see returns_dropped() above; the shape
# and column names of the return value are unchanged, other models depend on them.
cell_counts = function(cells, returns) {
  if (n_distinct(returns$race_id) > 1) stop("cell_counts() takes one race at a time")
  tt = suppressWarnings(max(returns$time, na.rm = TRUE))
  report_dropped(returns_dropped(cells, returns),
                 label = paste0(returns$race_id[1],
                                if (is.finite(tt)) paste0(" @ ", format(tt)) else ""))
  cc = returns |>
    summarize(c_dem = sum(votes[party == "dem"]), c_rep = sum(votes[party == "rep"]),
              c_oth = sum(votes[!party %in% c("dem", "rep")]), complete = all(complete),
              .by = c(geo, method))
  select(cells, geo, method) |>
    left_join(cc, by = c("geo", "method")) |>
    mutate(across(c(c_dem, c_rep, c_oth), \(x) coalesce(x, 0)),
           complete = coalesce(complete, FALSE),
           c_tot = c_dem + c_rep + c_oth)
}
