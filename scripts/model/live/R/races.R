# ========================================
## races.R -- the races we cover on election night, from 26_general/input_data/priors.csv.
##
##   races = races_from_priors()        one row per race: race_id ("TX_gov", "AK_house",
##                                      ...), state, election, jurisdiction, contest,
##                                      district -- the race columns of enightmodels'
##                                      prediction format. Pass it to predict_all(races = ).
##   cbs = cbs_for_races(races, env = "live")
##                                      named list race_id -> latest CBS county snapshot
##                                      (NULL where none has been pulled yet), for
##                                      predict_all(cbs = ).
## race_id is <STATE>_<contest>[_<district>]. House races in at-large states are district
## 1, as in enightmodels' schema; scripts/model/live/R/cbs_county_pull.R uses the same rule, so
## its folders (26_general/cbs_api/<env>/<STATE>_<contest>[_<district>]) line up.
# ========================================

AT_LARGE = c("AK", "DE", "ND", "SD", "VT", "WY")
ELECTION_DATE = as.Date("2026-11-03")

parse_race_id = function(id) {
  p = strsplit(id, "_")[[1]]
  st = toupper(p[1]); ct = tolower(p[2])
  dist = if (length(p) >= 3) as.integer(p[3]) else if (ct == "house" && st %in% AT_LARGE) 1L else NA_integer_
  tibble(race_id = id, state = st, election = ELECTION_DATE, jurisdiction = st, contest = ct, district = dist)
}

races_from_priors = function(dir_26 = find_26_general()) {
  pr = readr::read_csv(file.path(dir_26, "input_data", "priors.csv"), show_col_types = FALSE)
  bind_rows(lapply(pr$race_id, parse_race_id))
}

# Pulls fresh CBS county results for every race (pull_cbs_race(), cbs_county_pull.R) and
# returns the newest snapshot per race. If a pull fails -- or this machine can't pull (no
# cbsapi or CBS_API_KEY) -- the last saved snapshot is used instead, so the models still
# run. attr(result, "status") says what happened for each race.
# at: use saved snapshots as of this time (for testing; no pulling).
cbs_for_races = function(races, env = "live", pull = is.null(at), at = NULL, dir_26 = find_26_general()) {
  why = if (pull) cbs_pull_unavailable() else ""
  if (pull && nzchar(why)) message("Not pulling from CBS (", why, "); using the last saved snapshots")
  st = character(nrow(races))
  out = lapply(seq_len(nrow(races)), function(i) {
    r = races[i, ]
    snap = NULL; s = "not pulled"
    if (pull && !nzchar(why)) {
      snap = tryCatch(pull_cbs_race(r$state, r$contest, r$district, env = env, dir_26 = dir_26, verbose = FALSE),
                      error = function(e) { s <<- paste("pull failed:", conditionMessage(e)); NULL })
      if (!is.null(snap)) s = "pulled"
    }
    if (is.null(snap)) {
      snap = read_cbs_latest(r$state, r$contest, r$district, env = env, at = at, dir_26 = dir_26)
      s = paste0(s, if (is.null(snap)) "; no saved snapshot" else paste0("; using the snapshot saved ", format(as.POSIXct(max(snap$observed_at), tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC")))
    }
    st[i] <<- s
    snap
  })
  out = setNames(out, races$race_id)
  attr(out, "status") = tibble(race_id = races$race_id, cbs = st)
  out
}
