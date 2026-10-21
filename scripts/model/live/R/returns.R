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

# ---- down to precinct x method counts ---------------------------------------------
# One race's returns -> one row per row of `cells`, in the same order:
#   c_dem, c_rep, c_oth, c_tot, complete
# Cells with no returns rows get zeros and complete = FALSE.
cell_counts = function(cells, returns) {
  if (n_distinct(returns$race_id) > 1) stop("cell_counts() takes one race at a time")
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
