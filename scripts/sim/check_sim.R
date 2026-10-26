## Checks for a simulated election night written by make_sim_*.R
##
## Usage (from the repo root): Rscript scripts/sim/check_sim.R [scenario] [state]
## Reads {SIM_OUT or Dropbox/26_sim}/{scenario}/{ST}_truth.parquet and _schedule.parquet.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(nanoparquet)
  library(cli)
})
source("scripts/sim/sim_snapshot.R")

args = commandArgs(trailingOnly = TRUE)
SCENARIO = if (length(args) >= 1) args[1] else "tx_base"
STATE    = if (length(args) >= 2) args[2] else "TX"

PATH_DROPBOX = Sys.getenv("PATH_DROPBOX", unset = "~/Library/CloudStorage/Dropbox/2024 Election Results")
DIR = file.path(Sys.getenv("SIM_OUT", unset = file.path(PATH_DROPBOX, "26_sim")), SCENARIO)
truth    = read_parquet(file.path(DIR, paste0(STATE, "_truth.parquet"))) |> as_tibble()
schedule = read_parquet(file.path(DIR, paste0(STATE, "_schedule.parquet"))) |> as_tibble() |>
  mutate(t = with_tz(t, SIM_TZ))
spine = read_parquet(file.path(PATH_DROPBOX, "26_general", "input_data", "model_inputs",
                               paste0(STATE, "_precinct.parquet")))

ok = function(cond, msg) if (isTRUE(cond)) cli_alert_success(msg) else cli_alert_danger(msg)

# snapshot grid: before the zero files, then every 15 minutes to 03:00, then days out
el = truth$election[1]
times = c(
  sim_time("18:00", el),
  seq(sim_time("19:00", el), sim_time("19:00", el) + hours(8), by = "15 min"),
  sim_time(c("2026-11-04 12:00", "2026-11-05 12:00", "2026-11-07 12:00", "2026-11-10 12:00"), el)
)
snaps = lapply(times, \(t) sim_snapshot(truth, schedule, t))
names(snaps) = format(times, "%m-%d %H:%M")

cli_h2("Schema")
ok(all(vapply(snaps, \(d) identical(names(d), enightmodels::COLS_ELEC), TRUE)), "every snapshot has COLS_ELEC, in order")
for (i in c(2, which(names(snaps) == "11-03 22:00"), length(snaps))) {
  res = tryCatch({ enightmodels::check_elec_data(snaps[[i]]); "passes" }, error = \(e) conditionMessage(e))
  ok(res == "passes", "check_elec_data({names(snaps)[i]}): {res}")
}
ok(all(as.character(unique(snaps[[1]]$geo)) %in% spine$geo), "every geo is in {STATE}_precinct.parquet")

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
gov = filter(truth, contest == "gov") |> summarize(votes = sum(votes), .by = geo) |>
  left_join(select(spine, geo, est_turnout, n), by = "geo")
cli_text("gov votes {format(sum(gov$votes), big.mark = ',')} vs est_turnout {format(round(sum(gov$est_turnout)), big.mark = ',')}; max votes / n {round(max(gov$votes / gov$n), 2)}")
truth |>
  summarize(votes = sum(votes), .by = c(contest, method, party)) |>
  pivot_wider(names_from = party, values_from = votes) |>
  mutate(total = dem + rep + oth, dem2 = round(dem / (dem + rep), 3), method_share = round(total / sum(total), 3), .by = contest) |>
  print()

cli_h2("Mode replication (T2)")
rep_frac = snaps[["11-03 22:00"]] |>
  summarize(replicated = n_distinct(votes) == 1 & all(votes > 0), .by = c(geo, contest, candidate)) |>
  summarize(frac = mean(replicated))
ok(rep_frac$frac < 0.01, "cells with identical nonzero votes across methods: {round(100 * rep_frac$frac, 2)}%")

cli_h2("Reporting curve")
expected = filter(truth, !censored, contest == "gov") |> summarize(final = sum(votes), .by = method)
curve = bind_rows(lapply(names(snaps), \(nm) {
  snaps[[nm]] |>
    filter(contest == "gov") |>
    summarize(votes = sum(votes), .by = method) |>
    mutate(time = nm, stage4 = mean(snaps[[nm]]$stage >= 4L), stage6 = mean(snaps[[nm]]$stage == 6L))
})) |>
  left_join(expected, by = "method") |>
  mutate(pct = round(100 * votes / final)) |>
  select(time, method, pct, stage4, stage6) |>
  pivot_wider(names_from = method, values_from = pct) |>
  mutate(across(c(stage4, stage6), \(x) round(100 * x)))
print(filter(curve, time %in% names(snaps)[c(1:3, seq(4, length(snaps) - 4, by = 4), (length(snaps) - 3):length(snaps))]), n = Inf)

cli_h2("By county at 22:00 and 01:00: last posting, % of election-day votes counted")
eday_final = filter(truth, !censored, contest == "gov", method == "eday") |>
  summarize(final = sum(votes), .by = county_name)
by_cty = function(nm) snaps[[nm]] |>
  filter(contest == "gov") |>
  mutate(geo = as.character(geo)) |>
  left_join(distinct(truth, geo, county_name), by = "geo") |>
  summarize(posted = format(max(time), "%H:%M", tz = SIM_TZ), eday = sum(votes[method == "eday"]), .by = county_name) |>
  left_join(eday_final, by = "county_name") |>
  transmute(county_name, posted, pct_eday = round(100 * eday / final))
print(full_join(by_cty("11-03 22:00"), by_cty("11-04 01:00"), by = "county_name", suffix = c("_22", "_01")) |> arrange(county_name), n = Inf)
