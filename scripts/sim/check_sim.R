## Checks for a simulated election night written by make_sim.R
##
## Usage (from the repo root):
##   Rscript scripts/sim/check_sim.R [scenario] [state] [--contest=sen]
## Reads {SIM_OUT or 26_sim}/{scenario}/{ST}_truth.parquet and _schedule.parquet (see
## sim_config.R). state defaults to the one truth file in the folder; contest defaults to
## the first in the truth (gov in TX). Times are read off the state's poll close, so the
## mid-night and late checks fall 2 and 5 hours after it (22:00 and 01:00 in TX).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(nanoparquet)
  library(cli)
})
source("scripts/sim/sim_snapshot.R")
source("scripts/sim/sim_config.R")
source("scripts/model/live/R/vote_modes.R")

args_all = commandArgs(trailingOnly = TRUE)
args = args_all[!grepl("^--", args_all)]
SCENARIO = if (length(args) >= 1) args[1] else "tx_base"
DIR = file.path(DIR_SIMS, SCENARIO)
STATE = if (length(args) >= 2) toupper(args[2]) else {
  f = list.files(DIR, pattern = "_truth\\.parquet$")
  if (length(f) != 1) cli_abort("Expected one *_truth.parquet in {.path {DIR}}, found {length(f)}; give the state.")
  sub("_truth\\.parquet$", "", f)
}

truth    = read_parquet(file.path(DIR, paste0(STATE, "_truth.parquet"))) |> as_tibble()
schedule = read_parquet(file.path(DIR, paste0(STATE, "_schedule.parquet"))) |> as_tibble() |>
  mutate(t = with_tz(t, SIM_TZ))
spine = read_parquet(file.path(DIR_INPUTS, paste0(STATE, "_precinct.parquet")))
CONTEST = sub("^--contest=", "", grep("^--contest=", args_all, value = TRUE)[1])
if (is.na(CONTEST)) CONTEST = unique(truth$contest)[1]
if (!CONTEST %in% truth$contest) cli_abort("No {.val {CONTEST}} in the truth: {.val {unique(truth$contest)}}.")
cli_h1("check_sim.R: {STATE} / {SCENARIO} / {CONTEST}")

ok = function(cond, msg) if (isTRUE(cond)) cli_alert_success(msg) else cli_alert_danger(msg)

# snapshot grid: before the zero files, then every 15 minutes to 7 hours after poll
# close, then days out
el = truth$election[1]
CLOSE = sim_time(sim_state_params(STATE)$poll_close, el)
T_MID  = format(CLOSE + hours(2), "%m-%d %H:%M")
T_LATE = format(CLOSE + hours(5), "%m-%d %H:%M")
times = c(
  sim_time("18:00", el),
  seq(sim_time("19:00", el), CLOSE + hours(7), by = "15 min"),
  sim_time(c("2026-11-04 12:00", "2026-11-05 12:00", "2026-11-07 12:00", "2026-11-10 12:00"), el)
)
snaps = lapply(times, \(t) sim_snapshot(truth, schedule, t))
names(snaps) = format(times, "%m-%d %H:%M")

cli_h2("Schema")
ok(all(vapply(snaps, \(d) identical(names(d), enightmodels::COLS_ELEC), TRUE)), "every snapshot has COLS_ELEC, in order")
for (i in c(2, which(names(snaps) == T_MID), length(snaps))) {
  res = tryCatch({ enightmodels::check_elec_data(snaps[[i]]); "passes" }, error = \(e) conditionMessage(e))
  ok(res == "passes", "check_elec_data({names(snaps)[i]}): {res}")
}
# a geo outside the spine must be a site / county bucket (site_methods in sim_state_params.csv)
off_spine = setdiff(as.character(unique(snaps[[1]]$geo)), spine$geo)
ok(all(!is.na(bucket_class(geo_label(off_spine)))), "every geo is in {STATE}_precinct.parquet or is a bucket ({length(off_spine)} bucket geos)")

cli_h2("Zero files")
z = snaps[["11-03 19:00"]]
ok(sum(z$votes) == 0 && all(z$stage == 0L), "19:00: {n_distinct(z$geo)} precincts, all 0 votes, stage 0")

cli_h2("Monotone")
key = c("geo", "contest", "candidate", "method")
pairs = Map(\(a, b) inner_join(a, b, by = key, suffix = c("_a", "_b")), snaps[-length(snaps)], snaps[-1])
ok(all(vapply(pairs, \(p) all(p$votes_b >= p$votes_a), TRUE)), "votes never decrease")
ok(all(vapply(pairs, \(p) all(p$stage_b >= p$stage_a), TRUE)), "stage never decreases")
ok(all(vapply(pairs, \(p) all(p$time_b >= p$time_a), TRUE)), "county posting times never go back")

cli_h2("Final vs truth")
last = snaps[[length(snaps)]]
reported = filter(truth, !censored) |>
  semi_join(filter(schedule, !is.na(t)), by = "geo") |>
  inner_join(select(last, all_of(key), votes_snap = votes) |> mutate(across(where(is.factor), as.character)), by = key)
ok(all(reported$votes == reported$votes_snap), "last snapshot equals truth for every reporting precinct")
cli_text("Dark (never report): {.val {unique(truth$county_name[truth$geo %in% schedule$geo[is.na(schedule$t)]])}}")
cli_text("Censored: {n_distinct(truth$geo[truth$censored])} of {n_distinct(truth$geo)} precincts")

cli_h2("Turnout (T1) and shares")
ctv = filter(truth, contest == CONTEST) |> summarize(votes = sum(votes), .by = geo) |>
  left_join(select(spine, geo, est_turnout, n), by = "geo")
cli_text("{CONTEST} votes {format(sum(ctv$votes), big.mark = ',')} vs est_turnout {format(round(sum(ctv$est_turnout, na.rm = TRUE)), big.mark = ',')}; max votes / n {round(max(ctv$votes / ctv$n, na.rm = TRUE), 2)}")
truth |>
  summarize(votes = sum(votes), .by = c(contest, method, party)) |>
  pivot_wider(names_from = party, values_from = votes) |>
  mutate(total = dem + rep + oth, dem2 = round(dem / (dem + rep), 3), method_share = round(total / sum(total), 3), .by = contest) |>
  print()

cli_h2("Mode replication (T2)")
rep_frac = snaps[[T_MID]] |>
  summarize(replicated = n() > 1 & n_distinct(votes) == 1 & all(votes > 0), .by = c(geo, contest, candidate)) |>
  summarize(frac = mean(replicated))
ok(rep_frac$frac < 0.01, "cells with identical nonzero votes across methods: {round(100 * rep_frac$frac, 2)}%")

cli_h2("Reporting curve")
expected = filter(truth, !censored, contest == CONTEST) |> summarize(final = sum(votes), .by = method)
curve = bind_rows(lapply(names(snaps), \(nm) {
  snaps[[nm]] |>
    filter(contest == CONTEST) |>
    summarize(votes = sum(votes), .by = method) |>
    mutate(time = nm, stage4 = mean(snaps[[nm]]$stage >= 4L), stage6 = mean(snaps[[nm]]$stage == 6L))
})) |>
  left_join(expected, by = "method") |>
  mutate(pct = round(100 * votes / final)) |>
  select(time, method, pct, stage4, stage6) |>
  pivot_wider(names_from = method, values_from = pct) |>
  mutate(across(c(stage4, stage6), \(x) round(100 * x)))
print(filter(curve, time %in% names(snaps)[c(1:3, seq(4, length(snaps) - 4, by = 4), (length(snaps) - 3):length(snaps))]), n = Inf)

# by CBS reporting unit (county_cbs): a county everywhere but AK, where it is a house
# district and is labelled with its first borough
# election-day votes, or every vote in a state that reports no method (MN)
EDAY = if ("eday" %in% truth$method) "eday" else "all"
cli_h2("By county at {T_MID} and {T_LATE}: last posting, % of election-day votes counted")
units = distinct(truth, geo, county_cbs, county_name) |>
  mutate(county_name = first(county_name), .by = county_cbs)
eday_final = filter(truth, !censored, contest == CONTEST, method == EDAY) |>
  summarize(final = sum(votes), .by = county_cbs)
by_cty = function(nm) snaps[[nm]] |>
  filter(contest == CONTEST) |>
  mutate(geo = as.character(geo)) |>
  left_join(units, by = "geo") |>
  summarize(posted = format(max(time), "%H:%M", tz = SIM_TZ), eday = sum(votes[method == EDAY]), .by = c(county_cbs, county_name)) |>
  left_join(eday_final, by = "county_cbs") |>
  transmute(county_cbs, county_name, posted, pct_eday = round(100 * eday / final))
print(full_join(by_cty(T_MID), by_cty(T_LATE), by = c("county_cbs", "county_name"), suffix = c("_mid", "_late")) |>
        arrange(county_cbs) |> select(-county_cbs), n = Inf)
