## Shared setup for the sim scripts: where things live, and the per-state settings.
##
## Paths: 26_general is found by find_26_general() (scripts/model/live/R/paths.R), which
## tries CBS_26_GENERAL, then PATH_DROPBOX (env var, or globals.R's per-user value), then
## the usual Dropbox locations. Sims go to $SIM_OUT if set, else 26_sim next to
## 26_general -- the same folder run_night.R, sim_night.R etc. read from.
##
## Per-state settings, both in scripts/sim/:
##   sim_state_params.csv  poll_close, poll_close_late, pres24_dem2 (optional override)
##   sim_candidates.csv    contest x role (dem, rep, minor, writein) -> candidate, party.
##                         Spelt as the zero files spell them; a race with several minor
##                         candidates gets one "other"; writein only where the feed has one.
## Both refuse to guess: a state with no row aborts with a message saying what to add.

source("scripts/util/globals.R")
source("scripts/model/live/R/paths.R")

DIR_26     = find_26_general()
DIR_INPUTS = file.path(DIR_26, "input_data", "model_inputs")
DIR_SIMS   = Sys.getenv("SIM_OUT", unset = file.path(dirname(DIR_26), "26_sim"))

# state from --state, else the scenario name's prefix ("nc_base" -> "NC")
sim_state_from = function(state, scenario) {
  s = if (is.null(state) || is.na(state) || !nzchar(state)) substr(scenario, 1, 2) else state
  toupper(s)
}

# poll close (Eastern) and the optional 2024 baseline override for one state.
# poll_close: "HH:MM" on election day, or "YYYY-MM-DD HH:MM" for a close after midnight
#   ET (AK). poll_close_late: "COUNTY=HH:MM;COUNTY=HH:MM", county_name as in
#   {ST}_precinct.parquet. Returned as a named character vector.
sim_state_params = function(state, file = "scripts/sim/sim_state_params.csv") {
  sp = read.csv(file, colClasses = "character")
  sp = sp[sp$state == state, , drop = FALSE]
  if (!nrow(sp) || !nzchar(trimws(sp$poll_close[1])))
    cli::cli_abort(c("{.path {file}} has no poll_close for {state}.",
                     "i" = "Fill it in EASTERN time. It is a legal fact per state and in several \\
                            states per county; guessing it correlates reporting time with \\
                            partisanship, which is the bias these scenarios exist to detect.",
                     "x" = "Refusing to invent a poll close."))
  late = trimws(sp$poll_close_late[1])
  late = if (is.na(late) || !nzchar(late)) c() else {
    kv = strsplit(strsplit(late, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
    if (any(lengths(kv) != 2)) cli::cli_abort("bad poll_close_late for {state}: {late}")
    setNames(trimws(sapply(kv, `[`, 2)), trimws(sapply(kv, `[`, 1)))
  }
  pres = suppressWarnings(as.numeric(sp$pres24_dem2[1]))
  site = trimws(strsplit(coalesce_chr(sp$site_methods[1]), "|", fixed = TRUE)[[1]])
  list(poll_close = trimws(sp$poll_close[1]), poll_close_late = late,
       pres24_dem2 = if (is.na(pres)) NULL else pres,
       site_methods = site[nzchar(site)])
}
coalesce_chr = function(x) if (is.null(x) || is.na(x)) "" else x

# which counties report a method by site / county bucket rather than by precinct, and the
# bucket labels they use (lookups/reporting_levels.csv, built from the zero files and the
# 2024 results). One row per county_cbs x method x label, for the methods asked for.
sim_site_buckets = function(state, methods, file = "scripts/model/lookups/reporting_levels.csv") {
  if (!length(methods)) return(tibble::tibble(county_cbs = character(), method = character(), label = character()))
  source("scripts/model/live/R/vote_modes.R", local = TRUE)
  rl = read.csv(file, colClasses = "character")
  rl = rl[rl$state == state & rl$method %in% methods & rl$level == "county_site", , drop = FALSE]
  if (!nrow(rl)) cli::cli_abort("{.path {file}} has no county_site rows for {state} {.val {methods}}.")
  rl |>
    tibble::as_tibble() |>
    dplyr::transmute(county_cbs = stringr::str_pad(county_cbs, 3, pad = "0"), method,
                     label = strsplit(bucket_labels, ";\\s*")) |>
    tidyr::unnest_longer(label, keep_empty = TRUE) |>
    dplyr::mutate(label = trimws(label),
                  fold = unname(BUCKET_FOLD[bucket_class(label)])) |>
    # keep the labels that ARE this method's buckets; a county whose labels name none
    # (or that has none) gets one generic bucket
    dplyr::mutate(keep = !is.na(fold) & fold == method, .by = c(county_cbs, method)) |>
    dplyr::mutate(label = if (any(keep)) label else
                    c(early = "EARLY VOTING", mail = "ABSENTEE", eday = "PROVISIONAL")[method[1]],
                  keep = if (any(keep)) keep else seq_along(keep) == 1,
                  .by = c(county_cbs, method)) |>
    dplyr::filter(keep) |>
    dplyr::distinct(county_cbs, method, label)
}

# the candidate table make_sim.R draws votes for: one row per contest x role
sim_candidates = function(state, file = "scripts/sim/sim_candidates.csv") {
  x = read.csv(file, colClasses = "character")
  x = tibble::as_tibble(x[x$state == state, c("contest", "role", "candidate", "party"), drop = FALSE])
  if (!nrow(x))
    cli::cli_abort(c("{.path {file}} has no candidates for {state}.",
                     "i" = "Add one row per contest x role (dem, rep, minor, writein), with the \\
                            candidate named as the clean files name them."))
  # writein is optional: a contest whose feed has no write-in row gets none, and its
  # third-party vote all goes to `minor`
  bad = x |> dplyr::summarize(ok = all(c("dem", "rep", "minor") %in% role), .by = contest) |> dplyr::filter(!ok)
  roles = x |> dplyr::filter(!role %in% c("dem", "rep", "minor", "writein"))
  if (nrow(bad) || nrow(roles) || anyDuplicated(x[c("contest", "role")]))
    cli::cli_abort("{.path {file}}: {state} needs one dem, rep and minor row per contest (writein optional).")
  x
}

# counties with a zero file: the CBS county numbers in the geos of every
# 26_general/{ST}/clean/*_latest.parquet (geo is st_{county_cbs}_{precinct}). Used by
# make_sim.R --counties=zero, so a state whose feeds cover only some counties is simulated
# for those counties alone.
sim_zero_counties = function(state) {
  fs = Sys.glob(file.path(DIR_26, state, "clean", paste0(state, "*_latest.parquet")))
  if (!length(fs)) cli::cli_abort("No zero files in {.path {file.path(DIR_26, state, 'clean')}}.")
  geo = unlist(lapply(fs, \(f) as.character(nanoparquet::read_parquet(f, col_select = "geo")$geo)))
  sort(unique(substr(geo, 4, 6)))
}
