## Snapshots of a simulated election night
##
## A sim is two tables written by scripts/sim/make_sim_*.R:
##   truth:    final canvass, one row per geo x method x contest x candidate
##             (state, election, county_name, county_cbs, geo, contest, candidate,
##             party, method, votes, censored)
##   schedule: one row per geo x method x batch (geo, county_cbs, method, t, cum_frac).
##             Once batch time `t` passes, `cum_frac` of that cell's final votes are
##             counted. `t = NA` means the batch never lands during the sim.
##
## sim_snapshot(truth, schedule, time) returns what the clean files would hold at
## `time`: one row per candidate x method x geo, in the enightmodels schema.
##   - votes = round(final * largest cum_frac landed by `time`), so counts never fall
##   - stage per geo: 0 nothing counted, 4 some counted, 6 every batch landed
##   - time  = the county's last posting at or before `time` (counties post on their
##             own clocks, as the source timestamps do); before any posting, the
##             county's zero file at `zero_post`
##   - censored precincts (ids the crosswalk will not match) are dropped
##
## Needs dplyr, lubridate, and enightmodels.

SIM_TZ = "America/New_York"

# POSIXct, "HH:MM" on the election day, "YYYY-MM-DD HH:MM[:SS]", or a
# "YYYY_MM_DD_HH_MM_SS" timestamp string; all read as Eastern
sim_time = function(x, election) {
  if (inherits(x, "POSIXt")) return(lubridate::with_tz(x, SIM_TZ))
  x = as.character(x)
  x = ifelse(grepl("^\\d{1,2}:\\d{2}$", x), paste(election, x), x)
  lubridate::parse_date_time(x, c("Ymd HMS", "Ymd HM"), tz = SIM_TZ)
}

sim_snapshot = function(truth, schedule, time, zero_post = "19:00") {
  out = sim_snapshot_rows(truth, schedule, time, zero_post)
  enightmodels::coerce_elec_data(dplyr::select(out, dplyr::all_of(enightmodels::COLS_ELEC)))
}

sim_snapshot_rows = function(truth, schedule, time, zero_post) {
  election = truth$election[1]
  time = sim_time(time, election)
  stopifnot(length(time) == 1, !is.na(time))
  zero_time = min(time, sim_time(zero_post, election))

  landed = dplyr::filter(schedule, !is.na(t), t <= time)
  if (nrow(landed) > 0) {
    frac = dplyr::summarize(landed, frac = max(cum_frac), .by = c(geo, method))
    posted = dplyr::summarize(landed, time = max(t), .by = county_cbs)
  } else {
    frac = dplyr::tibble(geo = character(), method = character(), frac = numeric())
    posted = dplyr::tibble(county_cbs = character(), time = as.POSIXct(character(), tz = SIM_TZ))
  }
  stage = schedule |>
    dplyr::summarize(
      stage = dplyr::case_when(
        all(!is.na(t) & t <= time) ~ 6L,
        any(!is.na(t) & t <= time & cum_frac > 0) ~ 4L,
        .default = 0L
      ),
      .by = geo
    )

  truth |>
    dplyr::filter(!censored) |>
    dplyr::left_join(frac, by = c("geo", "method")) |>
    dplyr::left_join(stage, by = "geo") |>
    dplyr::left_join(posted, by = "county_cbs") |>
    dplyr::mutate(
      jurisdiction = tolower(state),
      district = NA_integer_,
      votes = round(votes * dplyr::coalesce(frac, 0)),
      time = dplyr::coalesce(.data$time, zero_time)
    )
}

# Write clean files the way cbs-enr does, into {dir}/{ST}/clean/:
#   statewide feed  {ST}_{timestamp}.parquet each time any county posts, plus
#                   {ST}_latest.parquet (AK, GA, IA, MN, NC: metadata.csv has one row for
#                   the state, with no county)
#   county feeds    {ST}_{county}_{timestamp}.parquet each time that county posts, plus
#                   {ST}_{county}_latest.parquet, with the county spelt as metadata.csv
#                   spells it ("MI_St. Clair", "TX_el paso")
# metadata: a data frame or path; NULL uses 26_general/metadata.csv when sim_config.R has
# set DIR_26. With no metadata row for the state, county feeds with lowercase names.
# A statewide file's timestamp is its latest county posting.
sim_write_clean = function(truth, schedule, times, dir, zero_post = "19:00", metadata = NULL) {
  st = truth$state[1]
  out_dir = file.path(dir, st, "clean")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(metadata) && exists("DIR_26")) metadata = file.path(DIR_26, "metadata.csv")
  if (is.character(metadata)) metadata = if (file.exists(metadata)) utils::read.csv(metadata, colClasses = "character") else NULL
  meta = if (is.null(metadata)) NULL else metadata[metadata$state == st, , drop = FALSE]
  statewide = !is.null(meta) && nrow(meta) > 0 && all(is.na(meta$county) | meta$county == "")
  key = function(x) gsub("[^A-Z]", "", toupper(x))
  file_county = function(county_name) {
    m = if (is.null(meta)) NA_character_ else meta$county[match(key(county_name), key(meta$county))]
    if (is.na(m)) tolower(county_name) else m
  }
  write_one = function(d, prefix, ts) {
    d = enightmodels::coerce_elec_data(dplyr::select(d, dplyr::all_of(enightmodels::COLS_ELEC)))
    nanoparquet::write_parquet(d, file.path(out_dir, paste0(prefix, "_", ts, ".parquet")))
    nanoparquet::write_parquet(d, file.path(out_dir, paste0(prefix, "_latest.parquet")))
  }
  stamp = function(t) format(lubridate::with_tz(t, SIM_TZ), "%Y_%m_%d_%H_%M_%S")

  times = sort(sim_time(times, truth$election[1]))
  last = character()
  for (i in seq_along(times)) {
    snap = sim_snapshot_rows(truth, schedule, times[i], zero_post)
    if (statewide) {
      ts = stamp(max(snap$time))
      if (identical(last[st], ts)) next
      last[st] = ts
      write_one(snap, st, ts)
      next
    }
    for (d in split(snap, snap$county_name)) {
      county = file_county(d$county_name[1])
      ts = stamp(d$time[1])
      if (identical(last[county], ts)) next
      last[county] = ts
      write_one(d, paste0(st, "_", county), ts)
    }
  }
  invisible(out_dir)
}
