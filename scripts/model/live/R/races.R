# ========================================
## races.R -- the races we cover on election night, from 26_general/input_data/priors.csv.
##
##   races = races_from_priors()        one row per race: race_id ("TX_gov", "AK_house",
##                                      ...), state, election, jurisdiction, contest,
##                                      district -- the race columns of enightmodels'
##                                      prediction format, READ FROM priors.csv, not
##                                      inferred. Pass it to predict_all(races = ).
##   cbs = cbs_for_races(races, env = "live")
##                                      named list race_id -> latest CBS county snapshot
##                                      (NULL where none has been pulled yet), for
##                                      predict_all(cbs = ).
## race_id is <STATE>_<contest>[_<district>], and priors.csv now states state / contest /
## district explicitly rather than having them parsed back out of it. House races in
## at-large states are district 1, as in enightmodels' schema.
##
## CORRECTION, measured 2026-10-26: this used to claim the CBS snapshot folders
## (26_general/cbs_api/<env>/<STATE>_<contest>[_<district>], built by cbs_county_pull.R
## from the decomposed fields) "line up" with race_id. They do NOT, for exactly one of the
## 12 races: AK_house has district 1, so its folder is `AK_house_1` while its race_id is
## `AK_house`. It is harmless in operation -- cbs_for_races() both writes and reads through
## the same decomposed fields, so they agree with each other, and the returned list is keyed
## by race_id -- but anyone looking for cbs_api/<env>/AK_house/ will not find it.
# ========================================

AT_LARGE = c("AK", "DE", "ND", "SD", "VT", "WY")
# NO ELECTION_DATE HERE. It used to be `as.Date("2026-11-03")`, which collided by name with
# scripts/util/globals.R's `ELECTION_DATE = "20260526"` -- the MAY PRIMARY date, and a
# character rather than a Date. Disjoint code paths today (globals.R is the primaries
# config, ELECTION_FOLDER = "26_primaries", and live/load.R does not source it), but two
# definitions of one name differing in both value and type is a trap, and whichever file
# was sourced second would have won silently.
# It is removable because priors.csv now STATES `election` and races_from_priors() reads it,
# and nothing consumes parse_race_id()'s election field: model_cory.R and model_cpois.R take
# only ri$contest / ri$district (their `election` comes from the returns), and
# cbs_county_pull.R's CLI takes only state/contest/district.

# parse_race_id() INFERS the race dimension from the id string. priors.csv now STATES it,
# and races_from_priors() reads it, so nothing in the night path infers any more. This is
# kept for the CLI paths that genuinely only have a string to go on -- cbs_county_pull.R's
# positional arguments, and model_cory.R / model_cpois.R when handed a bare returns
# race_id -- and for the round-trip assertion below.
#
# WHY THE INFERENCE HAD TO GO. It is not invertible: `district` is SYNTHESISED as 1 for
# house races in at-large states, so AK_house comes back as contest "house" with district 1
# and nothing in the result records that the id carried no district. Measured on the 12
# current races, paste(state, contest) recovers race_id 12 of 12 while
# paste(state, contest, district) recovers 11 of 12, and a districted id like NY_house_12
# breaks the first rule -- so no single rule reconstructs the key, and the CBS snapshot
# folder for AK_house is `AK_house_1` while its race_id is `AK_house`, which the comment at
# the top of this file used to claim "line up". They do not.
parse_race_id = function(id) {
  p = strsplit(id, "_")[[1]]
  st = toupper(p[1]); ct = tolower(p[2])
  dist = if (length(p) >= 3) as.integer(p[3]) else if (ct == "house" && st %in% AT_LARGE) 1L else NA_integer_
  # election is NA, not a hardcoded date: priors.csv is the only source for it. The column
  # is kept so the shape matches races_from_priors(), and an NA is an obvious wrong answer
  # if anything ever starts reading it, where a stale date would not be.
  tibble(race_id = id, state = st, election = as.Date(NA), jurisdiction = st,
         contest = ct, district = dist)
}

# The race dimension, READ not derived. priors.csv is the single source of truth for which
# races we cover and what each one is; this function does no parsing, applies no default,
# and has no fallback -- a missing column or a blank value is an abort, because the
# alternative is a silently invented district or a stale hardcoded election date.
#
# `district` is the one column allowed to be empty, and only because NA is its real value
# for a statewide race. It must be stated as empty rather than omitted.
RACE_COLS = c("race_id", "state", "election", "jurisdiction", "contest", "district")
races_from_priors = function(dir_26 = find_26_general()) {
  f = file.path(dir_26, "input_data", "priors.csv")
  pr = readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(
    race_id = "c", state = "c", election = "D", jurisdiction = "c",
    contest = "c", district = "i"))
  miss = setdiff(RACE_COLS, names(pr))
  if (length(miss))
    cli::cli_abort(c("{.path {f}} is missing the race column(s) {.val {miss}}.",
      "i" = "priors.csv states the race dimension; it is no longer inferred from race_id.",
      ">" = "Add the column(s). parse_race_id() shows what the old inference produced."))
  for (cl in setdiff(RACE_COLS, "district")) {
    bad = which(is.na(pr[[cl]]) | (is.character(pr[[cl]]) & !nzchar(trimws(pr[[cl]]))))
    if (length(bad))
      cli::cli_abort(c("{.path {f}} has a blank {.field {cl}} on row(s) {bad}.",
        "i" = "Every race column except `district` must be filled; `district` is empty \
               only because NA is its real value for a statewide race."))
  }
  # A house race MUST state its district. This is the one invariant the round-trip check
  # below cannot enforce: AK_house with a blank district round-trips cleanly (the 2-part
  # shape matches the id), so a blank would silently drop the `district = 1` that
  # enightmodels' schema wants for an at-large seat -- which is precisely the value the old
  # inference used to synthesise. Stating it is the whole point; permitting it blank would
  # put the fallback back.
  hb = which(pr$contest == "house" & is.na(pr$district))
  if (length(hb))
    cli::cli_abort(c("{.path {f}}: house race(s) {.val {pr$race_id[hb]}} have no district.",
      "i" = "An at-large seat is district 1, as in enightmodels' schema -- state it."))
  if (anyDuplicated(pr$race_id))
    cli::cli_abort("{.path {f}} has duplicate race_id(s): \
                    {.val {unique(pr$race_id[duplicated(pr$race_id)])}}")
  # The id is still the key everything else is keyed by, so it must agree with the columns
  # wherever its shape can express them. This is the check the old inference made impossible
  # to write: it could only ever agree with itself.
  shape = paste(pr$state, pr$contest, sep = "_")
  shape = ifelse(!is.na(pr$district) & grepl("_[0-9]+$", pr$race_id),
                 paste(shape, pr$district, sep = "_"), shape)
  off = which(shape != pr$race_id)
  if (length(off))
    cli::cli_abort(c("{.path {f}}: race_id disagrees with state/contest/district.",
      "x" = "{paste(sprintf('%s != %s', pr$race_id[off], shape[off]), collapse = ', ')}"))
  pr[, RACE_COLS]
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
