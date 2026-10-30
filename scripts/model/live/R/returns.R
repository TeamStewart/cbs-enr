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
#' Read one of Ethan's simulated nights
#'
#' @param dir_sc Scenario directory (26_sim/<scenario>).
#' @param tz Time zone for the schedule times.
#' @param state Two-letter state code; NULL auto-detects from the scenario's single
#'   `{ST}_truth.parquet`.
#' @return List: truth (`{ST}_truth.parquet`: final votes per geo x method x candidate) and
#'   sched (`{ST}_schedule.parquet`: cumulative share counted, `cum_frac`, by time `t`).
read_sim = function(dir_sc, tz = "America/New_York", state = NULL) {
  if (is.null(state)) {
    f = list.files(dir_sc, pattern = "_truth\\.parquet$")
    if (length(f) != 1) {
      stop(
        "expected exactly one *_truth.parquet in ",
        dir_sc,
        ", found ",
        length(f),
        if (length(f)) paste0(": ", paste(f, collapse = ", ")) else ""
      )
    }
    state = sub("_truth\\.parquet$", "", f)
  }
  p = function(suffix) {
    f = file.path(dir_sc, paste0(state, suffix))
    if (!file.exists(f)) {
      stop("sim scenario is missing ", basename(f))
    }
    f
  }
  list(
    truth = read_pq(p("_truth.parquet")) |>
      mutate(across(c(geo, county_cbs), as.character)),
    sched = read_pq(p("_schedule.parquet")) |>
      mutate(geo = as.character(geo), t = lubridate::with_tz(t, tz))
  )
}

#' Checkpoints the sim tests refit at
#'
#' @param tz Time zone.
#' @return POSIXct vector: every 15 minutes 8pm-4am ET on election night, then noon on Nov 4,
#'   5, 7 and 10.
sim_checkpoints = function(tz = "America/New_York") {
  c(
    seq(
      as.POSIXct("2026-11-03 20:00", tz = tz),
      as.POSIXct("2026-11-04 04:00", tz = tz),
      by = "15 min"
    ),
    as.POSIXct(
      c("2026-11-04 12:00", "2026-11-05 12:00", "2026-11-07 12:00", "2026-11-10 12:00"),
      tz = tz
    )
  )
}

#' Share of each cell counted by a time
#'
#' @param sim `read_sim()` list.
#' @param time POSIXct.
#' @return Tibble: geo, method, frac (the largest `cum_frac` landed by `time`).
sim_frac_at = function(sim, time) {
  landed = filter(sim$sched, !is.na(t), !is.na(cum_frac), t <= time)
  if (nrow(landed) == 0) {
    return(tibble(geo = character(), method = character(), frac = numeric()))
  }
  summarize(landed, frac = max(cum_frac), .by = c(geo, method))
}

#' Standard returns at one checkpoint of a simulated night
#'
#' A cell's counted votes are its final votes x the share counted by `time`; censored
#' precincts never report. `stage` is precinct-level, as a feed reports it: 6 when every
#' method is fully counted, 4 when anything is in, else 0.
#'
#' By default the returns look like the live feed's: `get_data()` (scripts/functions.R)
#' rewrites stage 6 to 5 unless `trust_stage`, so on the night `complete` is never TRUE
#' and the models fall back to `turnout_done()`. Here stage 6 becomes 5 and `complete` and
#' `frac` are dropped the same way. SIM_TRUST_COMPLETE=1 (or `trust_complete = TRUE`) keeps
#' the sim's exact completion instead, as a feed with a trustworthy stage 6 would give.
#'
#' @param sim `read_sim()` list.
#' @param race_id The sim's contest (e.g. "gov").
#' @param time POSIXct checkpoint.
#' @param geos Keep only these geos (e.g. the cells' precincts); NULL keeps all.
#' @param round_by "candidate" (default) rounds each candidate's counted votes, as a real
#'   feed reports them; "party" sums dem / rep / third party first, then rounds. They differ
#'   by at most 1 vote per cell, and only when a party has more than one candidate.
#' @param trust_complete Keep the sim's stage 6 and `complete` (see above).
#' @return Standard returns (file header), with `frac` the share of the cell counted (NA
#'   unless `trust_complete`).
returns_from_sim = function(
  sim,
  race_id,
  time,
  geos = NULL,
  round_by = c("candidate", "party"),
  trust_complete = Sys.getenv("SIM_TRUST_COMPLETE", unset = "0") == "1"
) {
  round_by = match.arg(round_by)
  tr = filter(sim$truth, contest == race_id)
  if (!is.null(geos)) {
    tr = filter(tr, geo %in% geos)
  }
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
    transmute(
      race_id = contest,
      state,
      election,
      geo,
      method,
      candidate,
      party,
      votes = round(votes * frac),
      stage = if (trust_complete) stage else pmin(stage, 5L),
      complete = trust_complete & frac >= 1,
      frac = if (trust_complete) frac else NA_real_,
      time = time
    )
}

# ---- the live feed ----------------------------------------------------------------
#' Standard returns from a CONTRACT.md section 8 feed
#'
#' Takes each row's latest value at or before `at`. `geo` must already be reconciled to the
#' precinct spine and `method` already canonical (lookups/vote_mode_map.csv); this function
#' does neither. Used by `run_night.R --returns=`.
#'
#' @param live The feed: geo, race_id, candidate_id, method, votes, stage, observed_at;
#'   append-only, one snapshot per scrape.
#' @param candidates CONTRACT.md section 5: race_id, candidate_id, name, party.
#' @param state,election Filled into every row.
#' @param at POSIXct; NULL = the latest scrape.
#' @param complete_stage A cell is complete once its precinct's stage reaches this.
#' @return Standard returns.
returns_from_live = function(live, candidates, state, election, at = NULL, complete_stage = 6L) {
  if (!is.null(at)) {
    live = filter(live, observed_at <= at)
  }
  live |>
    slice_max(observed_at, n = 1, with_ties = FALSE, by = c(geo, race_id, candidate_id, method)) |>
    left_join(
      select(candidates, race_id, candidate_id, candidate = name, party),
      by = c("race_id", "candidate_id")
    ) |>
    transmute(
      race_id,
      state = state,
      election = election,
      geo,
      method,
      candidate,
      party,
      votes,
      stage,
      complete = stage >= complete_stage,
      frac = NA_real_,
      time = observed_at
    )
}

# ---- the scrapers' clean files ------------------------------------------------------
#' Match feed geo ids to our precinct ids
#'
#' A feed id found in `geos` as written is kept; otherwise it is matched after
#' `norm_live_geo()` on both sides. Unmatched ids are kept as written.
#'
#' @param geo Feed geo ids.
#' @param geos Our precinct ids; NULL returns `geo` unchanged.
#' @return Character vector the length of `geo`.
match_live_geo = function(geo, geos = NULL) {
  geo = as.character(geo)
  if (is.null(geos)) {
    return(geo)
  }
  geos = unique(as.character(geos))
  key = setNames(geos, norm_live_geo(geos))
  key = key[!duplicated(names(key))]
  alt = unname(key[norm_live_geo(geo)])
  if_else(geo %in% geos | is.na(alt), geo, alt)
}

#' Normalize a geo id for matching
#'
#' The precinct part is upper-cased, space-squished, and loses a trailing ".0" and leading
#' zeros. Ids not in `{st}_{ddd}_...` form are returned as they are.
#'
#' @param geo Geo ids.
#' @return Character vector.
norm_live_geo = function(geo) {
  parts = stringr::str_match(as.character(geo), "^([a-z]{2}_\\d{3})_(.*)$")
  id = stringr::str_squish(toupper(parts[, 3]))
  id = stringr::str_remove(stringr::str_remove(id, "\\.0$"), "^0+(?=\\d)")
  if_else(is.na(parts[, 1]), as.character(geo), paste0(parts[, 2], "_", id))
}

#' Standard returns from the scrapers' clean files
#'
#' Reads `26_general/{ST}/clean/{ST}[_<county>]_latest.parquet`, in enightmodels'
#' election-data format (state, election, jurisdiction, contest, district, candidate, party,
#' time, stage, method, geo, votes): the zero file before polls close, the latest scrape
#' after. Online-only (0-byte) Dropbox files are skipped with a warning, so one county
#' cannot stop the refresh. Rows whose geo matches no precinct are kept: `virtual.R` adds
#' them at county level for swing and ratio_strata, and `bucket_counts()` passes them to
#' medsl_sur.
#'
#' @param races Races to keep (`races_from_priors()`).
#' @param dir_26 Path to 26_general.
#' @param files Files to read; NULL finds every state's `*_latest.parquet`.
#' @param complete_stage A cell is complete once its precinct's stage reaches this.
#' @param geos Our precinct ids, for `match_live_geo()`.
#' @return Standard returns. race_id is `{ST}_{contest}[_{district}]` as in priors.csv
#'   (at-large House seats have no district).
returns_from_clean = function(
  races,
  dir_26 = find_26_general(),
  files = NULL,
  complete_stage = 6L,
  geos = NULL
) {
  if (is.null(files)) {
    files = unlist(lapply(unique(races$state), function(st) {
      list.files(
        file.path(dir_26, st, "clean"),
        pattern = paste0("^", st, "(_.*)?_latest\\.parquet$"),
        full.names = TRUE
      )
    }))
  }
  if (!length(files)) {
    stop(
      "No *_latest.parquet in 26_general/<ST>/clean for ",
      paste(unique(races$state), collapse = ", ")
    )
  }
  # a 0-byte file is online-only in Dropbox: skip it (with a warning) rather than stop the
  # whole refresh, so one county's file can't take down every race
  empty = files[file.size(files) == 0]
  if (length(empty)) {
    cli::cli_alert_warning(
      "Skipping {length(empty)} online-only (0-byte) file{?s}; make {?it/them} available offline in Dropbox: {head(basename(empty), 8)}{if (length(empty) > 8) ', ...' else ''}"
    )
    files = setdiff(files, empty)
  }
  if (!length(files)) {
    stop(
      "Every *_latest.parquet for ",
      paste(unique(races$state), collapse = ", "),
      " is online-only in Dropbox; make 26_general available offline"
    )
  }
  x = bind_rows(lapply(files, function(f) {
    read_pq(f) |> mutate(across(where(is.factor), as.character), district = as.integer(district))
  }))
  x |>
    mutate(
      state = toupper(state),
      race_id = if_else(
        is.na(district) | (contest == "house" & state %in% AT_LARGE),
        paste(state, contest, sep = "_"),
        paste(state, contest, district, sep = "_")
      )
    ) |>
    filter(race_id %in% races$race_id) |>
    transmute(
      race_id,
      state,
      election = as.character(election),
      geo = match_live_geo(geo, geos),
      method,
      candidate,
      party,
      votes,
      stage,
      time
    ) |>
    summarize(
      votes = sum(votes),
      stage = max(stage),
      time = max(time),
      .by = c(race_id, state, election, geo, method, candidate, party)
    ) |>
    mutate(complete = stage >= complete_stage, frac = NA_real_)
}

# ---- drop detection ---------------------------------------------------------------
#' Returns rows that `cell_counts()` would drop
#'
#' `cell_counts()` joins from `cells`, so a returns row whose (geo, method) is not a cell is
#' discarded. Two loss modes:
#' * unknown_geo: the geo is not in `cells` at all. Either a non-precinct reporting bucket
#'   -- an early-voting site (`nc_032_EV DUKE`, `ak_004_04-early`) or a county mail /
#'   provisional pile (`nc_003_ABSENTEE`) -- or a precinct id never reconciled to the spine.
#'   lookups/reporting_levels.csv is the pre-election expectation of which (state, county,
#'   method) send these.
#' * unknown_method: the geo is a known precinct but the method is not one of its cells
#'   (`prov`, or `all` in CA / MI / FL / TX feeds).
#'
#' Size: 31% of the vote in NC 2024 Governor and in the AK 2026 primary sat on non-precinct
#' geo ids.
#'
#' @param cells Cells (geo, method).
#' @param returns One race's returns.
#' @return Tibble, one row per (drop, method): n_geo, n_rows, votes, share of all returns
#'   votes. Attributes: `geos` (drop, method, geo, votes) and `total_votes`.
returns_dropped = function(cells, returns) {
  known_geo = unique(as.character(cells$geo))
  known_cell = paste0(as.character(cells$geo), "\r", as.character(cells$method))
  r = returns |>
    transmute(
      geo = as.character(geo),
      method = as.character(method),
      votes,
      drop = case_when(
        !geo %in% known_geo ~ "unknown_geo",
        !paste0(geo, "\r", method) %in% known_cell ~ "unknown_method",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(drop))
  tot = sum(returns$votes, na.rm = TRUE)
  geos = r |>
    summarize(votes = sum(votes, na.rm = TRUE), .by = c(drop, method, geo)) |>
    arrange(drop, method, desc(votes))
  out = r |>
    summarize(
      n_geo = n_distinct(geo),
      n_rows = n(),
      votes = sum(votes, na.rm = TRUE),
      .by = c(drop, method)
    ) |>
    mutate(share = if (tot > 0) votes / tot else NA_real_) |>
    arrange(drop, desc(votes))
  structure(out, geos = geos, total_votes = tot)
}

#' Print the drop diagnostic
#'
#' Prints nothing when nothing is dropped; always on.
#'
#' @param d `returns_dropped()` output.
#' @param label Tag for the message (race and time).
#' @param n_show How many dropped geos to name.
#' @return `d`, invisibly.
report_dropped = function(d, label = NULL, n_show = 6) {
  if (nrow(d) == 0) {
    return(invisible(d))
  }
  tot = attr(d, "total_votes")
  v = sum(d$votes)
  tag = if (is.null(label)) "" else paste0(" [", label, "]")
  n_geo = n_distinct(attr(d, "geos")$geo[attr(d, "geos")$drop == "unknown_geo"])
  n_cell = sum(d$n_rows[d$drop == "unknown_method"])
  pct = function(x) if (is.na(x)) "n/a" else sprintf("%.1f%%", 100 * x)
  cli::cli_alert_warning(paste0(
    "cell_counts() dropped {prettyNum(v, big.mark = ',')} of ",
    "{prettyNum(tot, big.mark = ',')} returns votes ",
    "({pct(if (tot > 0) v / tot else NA_real_)})",
    tag,
    ": ",
    "{n_geo} geo{?s} absent from cells, {n_cell} row{?s} on an unmodelled method."
  ))
  for (i in seq_len(nrow(d))) {
    cli::cli_alert_info(paste0(
      "  {d$drop[i]} / {d$method[i]}: {d$n_geo[i]} geo{?s}, ",
      "{prettyNum(d$votes[i], big.mark = ',')} votes ",
      "({pct(d$share[i])} of all returns votes so far)"
    ))
  }
  ex = attr(d, "geos") |> filter(drop == "unknown_geo") |> head(n_show)
  if (nrow(ex)) {
    cli::cli_alert_info("  e.g. {.val {ex$geo}}")
  }
  invisible(d)
}

# ---- county x mode observations from the bucket rows -------------------------------
#' County x mode observations from bucket rows
#'
#' The rows `returns_dropped()` calls unknown_geo carry 31% of AK's and NC's vote. This turns
#' them into one observation per (county, mode). Deliberately narrow, because every
#' qualifier has a counterexample and a mistake produces an up-only turnout error:
#' * bucket rows only: the geo must be absent from `cells` AND its label must classify as a
#'   bucket (`bucket_class()`). Caswell `PROVI` and Henderson `EV` are real NC precincts and
#'   stay in `cell_counts()`.
#' * never `all` or `adv`: an aggregate is never summed beside its parts.
#' * keyed on the geo's county code (`geo_county()`), never `cells$county`: in AK those are
#'   different partitions.
#' * one race at a time (stops otherwise).
#' * folded to the model's modes (`BUCKET_FOLD`: prov and transfer -> eday).
#'
#' @param cells Cells (geo), the known precincts.
#' @param returns One race's returns.
#' @return Tibble, one row per (county, mode): c_dem, c_rep, c_oth, c_tot, n_geo, complete.
#'   Attribute `unmatched`: unknown geos that do not look like buckets (unreconciled
#'   precinct ids), so "no buckets" and "buckets we failed to parse" look different.
bucket_counts = function(cells, returns) {
  if (n_distinct(returns$race_id) > 1) {
    stop("bucket_counts() takes one race at a time")
  }
  known = unique(as.character(cells$geo))
  r = returns |>
    mutate(geo = as.character(geo), method = as.character(method)) |>
    filter(!geo %in% known)
  empty = tibble(
    county = character(),
    mode = character(),
    c_dem = numeric(),
    c_rep = numeric(),
    c_oth = numeric(),
    c_tot = numeric(),
    n_geo = integer(),
    complete = logical()
  )
  if (!nrow(r)) {
    return(structure(empty, unmatched = r[0, ]))
  }
  r = r |>
    mutate(county = geo_county(geo), label = geo_label(geo), bk = bucket_class(label))
  # An unknown geo whose label does NOT look like a bucket is a SPINE failure -- a precinct
  # id that was never reconciled -- not a county-level report. Absorbing it into a county
  # total would silently convert a join bug into extra votes, so it is returned separately.
  ok = !is.na(r$bk) & !is.na(r$county)
  unmatched = r[!ok, ]
  r = r[ok, ] |> mutate(mode = unname(BUCKET_FOLD[bk]))
  if (any(is.na(r$mode))) {
    stop(
      "bucket_class() returned a class with no BUCKET_FOLD entry: ",
      paste(unique(r$bk[is.na(r$mode)]), collapse = ", ")
    )
  }
  out = r |>
    summarize(
      c_dem = sum(votes[party == "dem"], na.rm = TRUE),
      c_rep = sum(votes[party == "rep"], na.rm = TRUE),
      c_oth = sum(votes[!party %in% c("dem", "rep")], na.rm = TRUE),
      n_geo = n_distinct(geo),
      complete = all(complete),
      .by = c(county, mode)
    ) |>
    mutate(c_tot = c_dem + c_rep + c_oth) |>
    arrange(county, mode)
  structure(out, unmatched = unmatched)
}

#' Print the bucket summary
#'
#' Prints nothing when there are no buckets, but always reports unmatched geos.
#'
#' @param b `bucket_counts()` output.
#' @param cells Cells; if given, flags bucket counties with no modeled precinct.
#' @param label Tag for the message.
#' @return `b`, invisibly.
report_buckets = function(b, cells = NULL, label = NULL) {
  um = attr(b, "unmatched")
  tag = if (is.null(label)) "" else paste0(" [", label, "]")
  if (nrow(b)) {
    cli::cli_alert_info(paste0(
      "bucket_counts{tag}: {prettyNum(sum(b$c_tot), big.mark = ',')} votes on ",
      "{sum(b$n_geo)} county/site geo{?s} -> {nrow(b)} county x mode observation{?s} ",
      "({n_distinct(b$county)} county(ies))"
    ))
    for (m in sort(unique(b$mode))) {
      z = b[b$mode == m, ]
      cli::cli_alert_info(paste0(
        "  {m}: {nrow(z)} county(ies), ",
        "{prettyNum(sum(z$c_tot), big.mark = ',')} votes, ",
        "{sum(z$complete)} flagged complete"
      ))
    }
    if (!is.null(cells)) {
      miss = setdiff(unique(b$county), geo_county(cells$geo))
      if (length(miss)) {
        cli::cli_alert_danger(paste0(
          "  {length(miss)} bucket county(ies) are not ",
          "in `cells` ({.val {miss}}) -- their votes belong to no modelled ",
          "precinct and must NOT be used as a county total."
        ))
      }
    }
  }
  if (nrow(um)) {
    g = unique(um$geo)
    cli::cli_alert_warning(paste0(
      "bucket_counts{tag}: {prettyNum(sum(um$votes, na.rm = TRUE), big.mark = ',')} votes ",
      "on {length(g)} unknown geo{?s} that do NOT look like buckets -- these are ",
      "unreconciled precinct ids, not county reports: {.val {utils::head(g, 6)}}"
    ))
  }
  invisible(b)
}

# ---- down to precinct x method counts ---------------------------------------------
#' One race's returns as precinct x method counts
#'
#' Rows that are not a cell are dropped and reported (`returns_dropped()`). Cells with no
#' returns get zeros and complete = FALSE.
#'
#' @param cells Cells (geo, method).
#' @param returns One race's returns (stops on more than one).
#' @return Tibble, one row per row of `cells` in the same order: geo, method, c_dem, c_rep,
#'   c_oth, complete, c_tot.
cell_counts = function(cells, returns) {
  if (n_distinct(returns$race_id) > 1) {
    stop("cell_counts() takes one race at a time")
  }
  tt = suppressWarnings(max(returns$time, na.rm = TRUE))
  report_dropped(
    returns_dropped(cells, returns),
    label = paste0(returns$race_id[1], if (is.finite(tt)) paste0(" @ ", format(tt)) else "")
  )
  cc = returns |>
    summarize(
      c_dem = sum(votes[party == "dem"]),
      c_rep = sum(votes[party == "rep"]),
      c_oth = sum(votes[!party %in% c("dem", "rep")]),
      complete = all(complete),
      .by = c(geo, method)
    )
  select(cells, geo, method) |>
    left_join(cc, by = c("geo", "method")) |>
    mutate(
      across(c(c_dem, c_rep, c_oth), \(x) coalesce(x, 0)),
      complete = coalesce(complete, FALSE),
      c_tot = c_dem + c_rep + c_oth
    )
}

# Fallback turnout-completeness test: a cell counts as counted out once its votes reach
# COMPLETE_FRAC x its expected votes. The same env var and default as the medsl model
# (medsl/model_turnout.R, where the threshold is measured and the 0.6 sweep is written up).
CELL_COMPLETE_FRAC = as.numeric(Sys.getenv("COMPLETE_FRAC", unset = "0.6"))

#' Cells counted out, for the turnout fits
#'
#' The feed's `complete` when it carries information, else the COMPLETE_FRAC ratio test, as
#' `complete_usable()` does for medsl_sur. A `complete` column that is never TRUE is the
#' production state (`get_data()` rewrites stage 6 to 5 unless `trust_stage`), and taken
#' at face value it leaves every turnout fit with no rows, so turnout is the prior all
#' night. The flag is trusted once it is TRUE for any cell of the race, or always with
#' COMPLETE_TRUST=1.
#'
#' Only for FITTING turnout. A cell admitted by the ratio test may still be counting, so
#' callers pin a cell's final votes at its count only when `counts$complete` is TRUE.
#' Admitting a cell at counted fraction g biases its log turnout ratio down by at most
#' log(COMPLETE_FRAC).
#'
#' @param counts `cell_counts()` output.
#' @param exp_votes Expected votes per cell, in the same order.
#' @return Logical per cell.
turnout_done = function(counts, exp_votes) {
  if (any(counts$complete) || Sys.getenv("COMPLETE_TRUST", unset = "0") == "1") {
    return(counts$complete & counts$c_tot > 0)
  }
  counts$c_tot > 0 & counts$c_tot >= CELL_COMPLETE_FRAC * exp_votes
}
