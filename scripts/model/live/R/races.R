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
## The CBS snapshot folders (26_general/cbs_api/<env>/) are named from state / contest /
## district, so AK_house's folder is `AK_house_1` while its race_id is `AK_house`.
# ========================================

AT_LARGE = c("AK", "DE", "ND", "SD", "VT", "WY")

#' Infer the race columns from a race id
#'
#' The night path reads them from priors.csv instead (`races_from_priors()`). This is kept
#' for the CLI paths that only have a string -- cbs_county_pull.R's positional arguments,
#' and model_ratio_strata.R when handed a bare returns race_id -- and for the round-trip
#' check in `races_from_priors()`. Not invertible: district is set to 1 for house races in
#' at-large states, so AK_house comes back with district 1.
#'
#' @param id Race id, `<STATE>_<contest>[_<district>]`.
#' @return One-row tibble with `RACE_COLS`; election is NA.
parse_race_id = function(id) {
  p = strsplit(id, "_")[[1]]
  st = toupper(p[1])
  ct = tolower(p[2])
  dist = if (length(p) >= 3) {
    as.integer(p[3])
  } else if (ct == "house" && st %in% AT_LARGE) {
    1L
  } else {
    NA_integer_
  }
  # election is NA, not a hardcoded date: priors.csv is the only source for it. The column
  # is kept so the shape matches races_from_priors(), and an NA is an obvious wrong answer
  # if anything ever starts reading it, where a stale date would not be.
  tibble(
    race_id = id,
    state = st,
    election = as.Date(NA),
    jurisdiction = st,
    contest = ct,
    district = dist
  )
}

# the race columns of enightmodels' prediction format, as priors.csv states them
RACE_COLS = c("race_id", "state", "election", "jurisdiction", "contest", "district")

#' The races we cover, read from priors.csv
#'
#' priors.csv is the single source of truth for which races we cover and what each one is.
#' No parsing, defaults or fallbacks: a missing column or a blank value stops, except
#' `district`, which is NA for a statewide race. A house race must state its district (an
#' at-large seat is district 1, as in enightmodels' schema).
#'
#' @param dir_26 Path to 26_general.
#' @return Tibble, one row per race, with `RACE_COLS`: race_id, state, election,
#'   jurisdiction, contest, district. Pass it to `predict_all(races = )`.
races_from_priors = function(dir_26 = find_26_general()) {
  f = file.path(dir_26, "input_data", "priors.csv")
  pr = readr::read_csv(
    f,
    show_col_types = FALSE,
    col_types = readr::cols(
      race_id = "c",
      state = "c",
      election = "c",
      jurisdiction = "c",
      contest = "c",
      district = "i"
    )
  )
  # ISO dates, or M/D/YY(YY) as Excel writes them when it re-saves the file
  el = trimws(pr$election)
  pr$election = as.Date(el, "%Y-%m-%d")
  for (fmt in c("%m/%d/%Y", "%m/%d/%y")) {
    i = is.na(pr$election) & grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", el) == (fmt == "%m/%d/%y")
    pr$election[i] = as.Date(el[i], fmt)
  }
  unread = which(is.na(pr$election) & !is.na(el) & nzchar(el))
  if (length(unread)) {
    cli::cli_abort(c(
      "{.path {f}} has an election date that is not a date on row(s) {unread}: {.val {el[unread]}}.",
      "i" = "Write it as 2026-11-03 (11/3/26 and 11/3/2026 are read too)."
    ))
  }
  miss = setdiff(RACE_COLS, names(pr))
  if (length(miss)) {
    cli::cli_abort(c(
      "{.path {f}} is missing the race column(s) {.val {miss}}.",
      "i" = "priors.csv states the race dimension; it is no longer inferred from race_id.",
      ">" = "Add the column(s). parse_race_id() shows what the old inference produced."
    ))
  }
  for (cl in setdiff(RACE_COLS, "district")) {
    bad = which(is.na(pr[[cl]]) | (is.character(pr[[cl]]) & !nzchar(trimws(pr[[cl]]))))
    if (length(bad)) {
      cli::cli_abort(c(
        "{.path {f}} has a blank {.field {cl}} on row(s) {bad}.",
        "i" = "Every race column except `district` must be filled; `district` is empty \
               only because NA is its real value for a statewide race."
      ))
    }
  }
  # A house race must state its district (an at-large seat is district 1 in enightmodels'
  # schema); the round-trip check below cannot catch a blank one.
  hb = which(pr$contest == "house" & is.na(pr$district))
  if (length(hb)) {
    cli::cli_abort(c(
      "{.path {f}}: house race(s) {.val {pr$race_id[hb]}} have no district.",
      "i" = "An at-large seat is district 1, as in enightmodels' schema -- state it."
    ))
  }
  if (anyDuplicated(pr$race_id)) {
    cli::cli_abort(
      "{.path {f}} has duplicate race_id(s): \
                    {.val {unique(pr$race_id[duplicated(pr$race_id)])}}"
    )
  }
  # The id is still the key everything else is keyed by, so it must agree with the columns
  # wherever its shape can express them. This is the check the old inference made impossible
  # to write: it could only ever agree with itself.
  shape = paste(pr$state, pr$contest, sep = "_")
  shape = ifelse(
    !is.na(pr$district) & grepl("_[0-9]+$", pr$race_id),
    paste(shape, pr$district, sep = "_"),
    shape
  )
  off = which(shape != pr$race_id)
  if (length(off)) {
    cli::cli_abort(c(
      "{.path {f}}: race_id disagrees with state/contest/district.",
      "x" = "{paste(sprintf('%s != %s', pr$race_id[off], shape[off]), collapse = ', ')}"
    ))
  }
  pr[, RACE_COLS]
}

#' Latest CBS county results for every race
#'
#' Pulls fresh results for each race (`pull_cbs_race()`, cbs_county_pull.R) and returns the
#' newest snapshot. If a pull fails -- or this machine can't pull (no cbsapi or
#' CBS_API_KEY) -- the last saved snapshot is used, so the models still run.
#'
#' @param races Races (`races_from_priors()`).
#' @param env CBS feed: "live", "test", or "sim".
#' @param pull Pull fresh results; FALSE reads saved snapshots only.
#' @param at Use saved snapshots as of this time (testing; no pulling).
#' @param dir_26 Path to 26_general (the cbs_api folder's parent).
#' @return Named list race_id -> snapshot (NULL where none exists). Attribute `status`: one
#'   row per race saying what happened.
cbs_for_races = function(
  races,
  env = "live",
  pull = is.null(at),
  at = NULL,
  dir_26 = find_26_general()
) {
  why = if (pull) cbs_pull_unavailable() else ""
  if (pull && nzchar(why)) {
    message("Not pulling from CBS (", why, "); using the last saved snapshots")
  }
  st = character(nrow(races))
  out = lapply(seq_len(nrow(races)), function(i) {
    r = races[i, ]
    snap = NULL
    s = "not pulled"
    if (pull && !nzchar(why)) {
      snap = tryCatch(
        pull_cbs_race(r$state, r$contest, r$district, env = env, dir_26 = dir_26, verbose = FALSE),
        error = function(e) {
          s <<- paste("pull failed:", conditionMessage(e))
          NULL
        }
      )
      if (!is.null(snap)) s = "pulled"
    }
    if (is.null(snap)) {
      snap = read_cbs_latest(r$state, r$contest, r$district, env = env, at = at, dir_26 = dir_26)
      s = paste0(
        s,
        if (is.null(snap)) {
          "; no saved snapshot"
        } else {
          paste0(
            "; using the snapshot saved ",
            format(as.POSIXct(max(snap$observed_at), tz = "UTC"), "%Y-%m-%d %H:%M:%S UTC")
          )
        }
      )
    }
    st[i] <<- s
    snap
  })
  out = setNames(out, races$race_id)
  attr(out, "status") = tibble(race_id = races$race_id, cbs = st)
  out
}
