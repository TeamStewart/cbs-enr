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

# Write per-county clean files the way cbs-enr does:
# {dir}/{ST}/clean/{ST}_{county}_{timestamp}.parquet, one per county each time its
# posting time changes, plus {ST}_{county}_latest.parquet. County names are lowercase,
# as in metadata.csv.
sim_write_clean = function(truth, schedule, times, dir, zero_post = "19:00") {
  st = truth$state[1]
  out_dir = file.path(dir, st, "clean")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  times = sort(sim_time(times, truth$election[1]))
  last = character()
  for (i in seq_along(times)) {
    snap = sim_snapshot_rows(truth, schedule, times[i], zero_post)
    for (d in split(snap, snap$county_name)) {
      county = tolower(d$county_name[1])
      ts = format(lubridate::with_tz(d$time[1], SIM_TZ), "%Y_%m_%d_%H_%M_%S")
      if (identical(last[county], ts)) next
      last[county] = ts
      d = enightmodels::coerce_elec_data(dplyr::select(d, dplyr::all_of(enightmodels::COLS_ELEC)))
      nanoparquet::write_parquet(d, file.path(out_dir, paste0(st, "_", county, "_", ts, ".parquet")))
      nanoparquet::write_parquet(d, file.path(out_dir, paste0(st, "_", county, "_latest.parquet")))
    }
  }
  invisible(out_dir)
}
