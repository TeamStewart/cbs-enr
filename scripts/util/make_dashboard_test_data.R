# ========================================
## Build test data for the dashboard (index.qmd) from the simulated nights
##
## Live results: every 26_sim/{st}_base scenario (truth + schedule, see
## scripts/sim/sim_snapshot.R) is replayed at the sim test checkpoints (every 15 min
## 8pm-4am ET, then Nov 4/5/7/10 noon) through make_summary() into
## {TEST_FOLDER}/{STATE}/summaries, one statewide feed per state.
## Models: live_sim_{st}_base.csv in 26_general/model_tests/coverage/{main,specs_conformal}
## (scripts/model/live/tests/sim_night.R; model names as in model_config.csv, which is
## copied alongside) is converted to enightmodels predictions/status csvs in
## {TEST_FOLDER}/model_results.
## live_sim has only the two-party share and turnout, so per-party shares split the
## counted two-party share of the vote at that checkpoint, and votes = share x est_turnout.
## Also writes fake history (with vote methods, except GA), copies the sim states'
## crosswalks and L2 files from 26_general/input_data, and saves the dashboard data rds.
## Only DASHBOARD_CONTESTS rows with a sim are shown.
##
## Usage (from project root):
##   CBS_ELECTION_FOLDER=26_dashtest Rscript scripts/util/make_dashboard_test_data.R
##   CBS_ELECTION_FOLDER=26_dashtest CBS_DASH_RDS=data/dashboard/dashtest_data.rds quarto render index.qmd --output dashtest.html
# ========================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
})

source("scripts/functions.R")
source("scripts/sim/sim_snapshot.R")

stopifnot(ELECTION_FOLDER != "26_primaries", ELECTION_FOLDER != "26_general")

SIM_DIR = fs::path(PATH_DROPBOX, "26_sim")
# sim_night.R runs: main has each model's default fits, specs_conformal the other
# demographic specs with the conformal interval; stacked, a model in both read from the first
TEST_DIRS = fs::path(PATH_DROPBOX, "26_general", "model_tests", "coverage", c("main", "specs_conformal"))
read_live_sim <- function(scenario) {
  files = fs::path(TEST_DIRS, glue("live_sim_{scenario}.csv"))
  files = files[fs::file_exists(files)]
  map(files, \(f) readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE) |>
        mutate(source = fs::path_rel(f, fs::path(PATH_DROPBOX, "26_general")))) |>
    bind_rows() |>
    filter(source == first(source), .by = model)
}
SCENARIOS = fs::path_file(fs::dir_ls(SIM_DIR, regexp = "_base$", type = "directory"))
SCENARIOS = SCENARIOS[fs::file_exists(fs::path(TEST_DIRS[1], glue("live_sim_{SCENARIOS}.csv")))]
STATES = toupper(str_sub(SCENARIOS, 1, 2))
# same checkpoints as sim_checkpoints() in scripts/model/live/R/returns.R
CHECKPOINTS = c(
  seq(as.POSIXct("2026-11-03 20:00", tz = SIM_TZ), as.POSIXct("2026-11-04 04:00", tz = SIM_TZ), by = "15 min"),
  as.POSIXct(c("2026-11-04 12:00", "2026-11-05 12:00", "2026-11-07 12:00", "2026-11-10 12:00"), tz = SIM_TZ)
)

set.seed(1)
dash = list()
counted = list() # counted dem / rep / total per state x contest x checkpoint, for the model rows
cands = list()

for (i in seq_along(SCENARIOS)) {
  s = STATES[i]
  fs::dir_delete(fs::path(PATH_DROPBOX, ELECTION_FOLDER, s, "summaries")) |> try(silent = TRUE)
  truth = nanoparquet::read_parquet(fs::path(SIM_DIR, SCENARIOS[i], glue("{s}_truth.parquet"))) |>
    as_tibble() |>
    mutate(across(c(geo, county_cbs), as.character))
  sched = nanoparquet::read_parquet(fs::path(SIM_DIR, SCENARIOS[i], glue("{s}_schedule.parquet"))) |>
    as_tibble() |>
    mutate(geo = as.character(geo))
  cands[[s]] = distinct(truth, contest, party, candidate) |> mutate(state = s)

  for (tm in as.list(CHECKPOINTS)) {
    d = sim_snapshot(truth, sched, tm)
    counted[[length(counted) + 1]] = d |>
      summarise(dem = sum(votes[party == "dem"]), rep = sum(votes[party == "rep"]), total = sum(votes), .by = contest) |>
      mutate(state = s, time = tm)
    res = make_summary(d, s, NA)
    if (is.null(dash[[res$key]]) || res$time >= dash[[res$key]]$time) dash[[res$key]] = res
  }
  cli::cli_inform("{s}: {SCENARIOS[i]}, {n_distinct(truth$geo)} geos, {length(CHECKPOINTS)} checkpoints")
}
counted = bind_rows(counted)
cands = bind_rows(cands)

TEST_CONTESTS = semi_join(DASHBOARD_CONTESTS, distinct(cands, state, contest), by = c("state", "contest")) |>
  filter(is.na(district))
missing = anti_join(distinct(cands, state, contest), TEST_CONTESTS, by = c("state", "contest"))
if (nrow(missing) > 0) cli::cli_warn("sim contests not in DASHBOARD_CONTESTS (not shown): {paste(missing$state, missing$contest)}")

# fake history from the final count: GA without methods, the rest with methods
fake_history <- function(state, with_method) {
  p = dash[[state]]$prec |> filter(party %in% c("dem", "rep"))
  grp = c("geo", "contest", "district", "party", if (with_method) "method")
  p |>
    summarise(votes = sum(votes), .by = all_of(grp)) |>
    mutate(
      votes = round(votes * runif(n(), 0.7, 1.3) * if_else(party == "dem", 0.9, 1.1)),
      year = 2022L,
      county = str_extract(geo, "^[a-z]{2}_(\\d{3})", group = 1)
    )
}

# crosswalks + L2 for read_l2()
for (sub in c("crosswalks", "l2")) {
  to = fs::dir_create(fs::path(PATH_DROPBOX, ELECTION_FOLDER, "input_data", sub))
  fs::dir_ls(fs::path(PATH_DROPBOX, "26_general", "input_data", sub), regexp = glue("/({paste(tolower(STATES), collapse = '|')})(\\.csv|_\\d{{8}}_precincts\\.parquet)$")) |>
    fs::file_copy(to, overwrite = TRUE)
}

hist_dir = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "history")
fs::dir_delete(hist_dir) |> try(silent = TRUE)
fs::dir_create(hist_dir)
for (s in STATES) nanoparquet::write_parquet(fake_history(s, s != "GA"), fs::path(hist_dir, glue("{s}_history.parquet")))

# the night's model names and their spec / interval parts (model_parts() reads the copy)
fs::file_copy(fs::path(PATH_DROPBOX, "26_general", "model_config.csv"), fs::path(PATH_DROPBOX, ELECTION_FOLDER, "model_config.csv"), overwrite = TRUE)
parts = model_parts(unlist(map(SCENARIOS, \(sc) unique(read_live_sim(sc)$model))))
# model_conf labels as scripts/model/live/R/results.R writes them (95% intervals)
CONF_LABEL = c(bootstrap = "county_bootstrap_95", credible = "posterior_95", conf_seq = "conf_seq_95", conformal = "approx_conform_95")

# live_sim -> enightmodels predictions; time is the checkpoint (Eastern), geo = "scraped"
live_sim_models <- function(scenario, state) {
  runs = read_live_sim(scenario) |>
    mutate(
      time = lubridate::ymd_hm(time, tz = SIM_TZ),
      across(c(est_twop, lo_twop, hi_twop, est_turnout, pct_votes), as.numeric),
      status = coalesce(status, "no result (status missing in live_sim)")
    ) |>
    rename(contest = race_id)

  status = runs |>
    transmute(
      race_id = paste(state, contest, sep = "_"), model, time, status,
      statewide = "scraped counties only",
      settings = paste0("26_general/", source)
    )

  pred = runs |>
    filter(str_starts(status, "ok"), !is.na(est_twop)) |>
    mutate(state = state) |>
    inner_join(counted, by = c("state", "contest", "time")) |>
    filter(total > 0) |>
    mutate(two = (dem + rep) / total, has_int = !is.na(lo_twop) & !is.na(hi_twop)) |>
    rowwise() |>
    reframe(
      state, contest, model, time, pct_rep = pct_votes, est_turnout, has_int,
      party = c("dem", "rep", "oth"),
      share = c(est_twop, 1 - est_twop, NA) * two,
      share_min = c(lo_twop, 1 - hi_twop, NA) * two,
      share_max = c(hi_twop, 1 - lo_twop, NA) * two,
      n_rep = c(dem, rep, total - dem - rep)
    ) |>
    mutate(
      share = coalesce(share, 1 - sum(share, na.rm = TRUE)),
      share_rep = n_rep / sum(n_rep),
      .by = c(contest, model, time)
    ) |>
    left_join(cands |> summarise(candidate = first(candidate), .by = c(state, contest, party)), by = c("state", "contest", "party"))

  pred = bind_rows(
    transmute(pred, time, state, contest, model, has_int, party, candidate, pred_type = "share", pct_rep,
              rep = share_rep, pred = share, pred_min = share_min, pred_max = share_max),
    transmute(pred, time, state, contest, model, has_int, party, candidate, pred_type = "vote", pct_rep,
              rep = n_rep, pred = share * est_turnout, pred_min = share_min * est_turnout, pred_max = share_max * est_turnout)
  ) |>
    mutate(
      election = ELECTION_DATE, jurisdiction = state, district = NA_integer_, geo = "scraped",
      race_id = paste(state, contest, sep = "_"),
      model_conf = if_else(has_int, CONF_LABEL[parts$model_interval[match(model, parts$model)]], NA_character_)
    ) |>
    select(-has_int)
  list(pred = pred, status = status)
}

model_dir = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "model_results")
fs::dir_delete(model_dir) |> try(silent = TRUE)
fs::dir_create(model_dir)
fm = map2(SCENARIOS, STATES, live_sim_models)
fm_pred = map(fm, "pred") |> bind_rows()
fm_status = map(fm, "status") |> bind_rows()
iso = \(t) format(t, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
stamp = \(t) format(t, "%Y%m%d_%H%M%S", tz = "UTC")
for (t in sort(unique(c(fm_pred$time, fm_status$time)))) {
  t = as.POSIXct(t, tz = "UTC")
  readr::write_csv(mutate(filter(fm_pred, time == t), time = iso(time)), fs::path(model_dir, glue("predictions_{stamp(t)}.csv")), na = "")
  readr::write_csv(mutate(filter(fm_status, time == t), time = iso(time)), fs::path(model_dir, glue("status_{stamp(t)}.csv")), na = "")
}
cli::cli_inform("model results: {n_distinct(fm_pred$time)} runs, {n_distinct(fm_pred$race_id)} races in {model_dir}")

dd = build_dashboard_data(unname(dash), contests = TEST_CONTESTS, models = read_model_results(model_dir))
saveRDS(dd, fs::path(PATH_DROPBOX, ELECTION_FOLDER, "dashboard_data.rds"))
# committed copy; pushing it to main redeploys GitHub Pages (.github/workflows/pages.yml)
saveRDS(dd, "data/dashboard/dashtest_data.rds")

print(dd$meta, width = Inf)
cli::cli_inform("ts rows: {nrow(dd$ts)}; prec rows: {nrow(dd$prec)}; model rows: {nrow(dd$models)}")
