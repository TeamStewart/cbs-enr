# ========================================
## Build fake general-election test data for the dashboard (index.qmd)
##
## Reads the 26_primaries clean snapshots, merges party primaries into one
## Dem-vs-Rep contest (sen_dem + sen_rep -> sen), replays every snapshot through
## make_summary() into {TEST_FOLDER}/{STATE}/summaries, writes fake history files
## for GA (no methods) and NC (with methods), copies the dashboard states' crosswalks
## and L2 files from 26_general/input_data, writes fake enightmodels runs to
## {TEST_FOLDER}/model_results, and saves the dashboard data rds.
##
## Usage (from project root):
##   CBS_ELECTION_FOLDER=26_dashtest Rscript scripts/util/make_dashboard_test_data.R
##   CBS_ELECTION_FOLDER=26_dashtest CBS_DASH_RDS=".../26_dashtest/dashboard_data.rds" quarto render index.qmd
# ========================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
})

source("scripts/functions.R")

stopifnot(ELECTION_FOLDER != "26_primaries", ELECTION_FOLDER != "26_general")

SOURCE_FOLDER = "26_primaries"
STATES = unique(DASHBOARD_CONTESTS$state)
# late post-primary snapshots cause big jumps in the time series; TX keeps them (runoff day)
DROP_DATE = "2026_05_26"

to_general <- function(d) {
  d |>
    mutate(
      contest = str_remove(as.character(contest), "_(dem|rep)$"),
      party = as.character(party)
    )
}

# 26_general geos for a state's dashboard contests; empty until general files exist
general_geos <- function(state) {
  f = fs::dir_ls(fs::path(PATH_DROPBOX, "26_general", state, "clean"), regexp = "_latest\\.parquet$", fail = FALSE)
  if (length(f) == 0) return(character())
  map(f, nanoparquet::read_parquet) |>
    bind_rows() |>
    mutate(contest = as.character(contest), geo = as.character(geo)) |>
    semi_join(filter(DASHBOARD_CONTESTS, .data$state == .env$state), by = c("contest", "district")) |>
    pull(geo) |>
    unique()
}

# primary geo -> general geo; matches ignore case, unmatched general geos copy a
# random same-county primary precinct, and primary-only geos are dropped
geo_map <- function(prim, gen) {
  direct = tibble(geo = prim, geo_new = gen[match(tolower(prim), tolower(gen))]) |> filter(!is.na(geo_new))
  cty = \(x) str_extract(tolower(x), "^[a-z]{2}_[^_]+")
  left = setdiff(gen, direct$geo_new)
  donors = map_chr(left, \(g) {
    cand = direct$geo[cty(direct$geo) == cty(g)]
    sample(if (length(cand) > 0) cand else direct$geo, 1)
  })
  cli::cli_inform("{length(gen)} general geos: {nrow(direct)} matched, {length(left)} borrowed; {length(setdiff(prim, direct$geo))} primary geos dropped")
  bind_rows(direct, tibble(geo = donors, geo_new = left))
}

set.seed(1)
dash = list()

for (s in STATES) {
  fs::dir_delete(fs::path(PATH_DROPBOX, ELECTION_FOLDER, s, "summaries")) |> try(silent = TRUE)

  files = tibble(
    path = fs::dir_ls(fs::path(PATH_DROPBOX, SOURCE_FOLDER, s, "clean"), regexp = "_\\d{4}(_\\d{2}){5}\\.parquet$")
  ) |>
    mutate(
      prefix = str_match(fs::path_file(path), "^(.*)_\\d{4}(_\\d{2}){5}\\.parquet$")[, 2],
      county = na_if(str_remove(prefix, glue("^{s}_?")), "")
    ) |>
    filter(s == "TX" | !str_detect(fs::path_file(path), glue("_{DROP_DATE}_"))) |>
    arrange(path)

  gen = general_geos(s)
  if (length(gen) > 0) {
    prim = map(files$path, \(f) as.character(nanoparquet::read_parquet(f, col_select = "geo")$geo)) |> unlist() |> unique()
    xw = geo_map(prim, gen)
  }

  for (i in seq_len(nrow(files))) {
    d = nanoparquet::read_parquet(files$path[i]) |> to_general()
    if (length(gen) > 0) {
      d = mutate(d, geo = as.character(geo)) |>
        inner_join(xw, by = "geo", relationship = "many-to-many") |>
        mutate(geo = geo_new, .keep = "unused")
    }
    if (nrow(d) == 0) next
    res = make_summary(d, s, files$county[i])
    if (is.null(dash[[res$key]]) || res$time >= dash[[res$key]]$time) dash[[res$key]] = res
  }
  cli::cli_inform("{s}: {nrow(files)} snapshot{?s}")
}

# fake history: GA without methods, NC with methods
fake_history <- function(state, with_method) {
  p = bind_rows(map(keep(dash, \(x) x$state == state), "prec")) |>
    filter(party %in% c("dem", "rep"))
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

fs::dir_create(fs::path(PATH_DROPBOX, ELECTION_FOLDER, "history"))
nanoparquet::write_parquet(fake_history("GA", FALSE), fs::path(PATH_DROPBOX, ELECTION_FOLDER, "history", "GA_history.parquet"))
nanoparquet::write_parquet(fake_history("NC", TRUE), fs::path(PATH_DROPBOX, ELECTION_FOLDER, "history", "NC_history.parquet"))

dd = build_dashboard_data(unname(dash))

# fake enightmodels output (one predictions/status csv per summary time), shaped like
# 26_general/model_tests/run_night_sim: GA-sen has scraped + all, NC-sen scraped only,
# PA-gov scraped only with model_medsl failing; other contests get no models
FAKE_MODELS = tribble(
  ~model, ~conf_scraped, ~conf_all, ~width,
  "model_simpleswing", "county_bootstrap_95", "county_bootstrap_95_plus_rest", 0.04,
  "model_cory", NA, NA, NA,
  "model_medsl", "posterior_95", "posterior_95_plus_rest", 0.06,
  "model_cory_pois", "approx_conform_95", NA, 0.15
)
FAKE_RACES = tribble(
  ~key, ~geos, ~failed,
  "GA-sen", c("scraped", "all"), NA,
  "NC-sen", "scraped", NA,
  "PA-gov", "scraped", "model_medsl"
)

fake_models <- function(key, geos, failed) {
  m = filter(dd$meta, .data$key == .env$key)
  tot = filter(dd$ts, .data$key == .env$key) |>
    summarise(across(c(dem, rep, total), sum), .by = time) |>
    filter(total > 0) |>
    arrange(time)
  fin = slice_tail(tot, n = 1)
  cand = c(dem = str_to_lower(m$dem_name), rep = str_to_lower(m$rep_name), oth = "write-in")
  mods = filter(FAKE_MODELS, !model %in% failed)

  pred = expand_grid(tot, mods, geo = geos) |>
    mutate(
      # "all" adds unscraped counties: less reported, final result a bit more Rep
      cover = if_else(geo == "all", 0.75, 0.97),
      pct_rep = pmin(total / fin$total * cover, 1),
      final_dem = fin$dem / fin$total - if_else(geo == "all", 0.03, 0) + rnorm(n(), 0, 0.004),
      final_rep = fin$rep / fin$total + if_else(geo == "all", 0.03, 0) + rnorm(n(), 0, 0.004),
      final_n = fin$total / cover * runif(n(), 0.97, 1.03),
      err = (1 - pct_rep) * rnorm(n(), 0, 0.03),
      half = coalesce(width, 0) * (1 - pct_rep) + 0.004,
      conf = if_else(geo == "all", conf_all, conf_scraped)
    ) |>
    rowwise() |>
    reframe(
      time, model, geo, pct_rep,
      model_conf = conf, party = c("dem", "rep", "oth"),
      share_rep = c(dem, rep, total - dem - rep) / total,
      share = c(final_dem + err, final_rep - err, 1 - final_dem - final_rep),
      half = half * c(1, 1, 0.2),
      n = final_n, n_rep = total * c(dem, rep, total - dem - rep) / total
    ) |>
    mutate(candidate = cand[party])

  pred = bind_rows(
    transmute(pred, time, model, model_conf, geo, party, candidate, pred_type = "share", pct_rep,
              rep = share_rep, pred = share, pred_min = share - half, pred_max = share + half),
    transmute(pred, time, model, model_conf, geo, party, candidate, pred_type = "vote", pct_rep,
              rep = n_rep, pred = share * n, pred_min = (share - half) * n, pred_max = (share + half) * n)
  ) |>
    mutate(
      state = m$state, election = ELECTION_DATE, jurisdiction = m$state, contest = m$contest, district = m$district,
      race_id = paste(m$state, m$contest, sep = "_"),
      across(c(pred_min, pred_max), \(x) if_else(is.na(model_conf), NA_real_, x))
    )

  status = expand_grid(time = tot$time, model = FAKE_MODELS$model) |>
    mutate(
      race_id = paste(m$state, m$contest, sep = "_"),
      status = if_else(model %in% failed, "error: fake failure for testing", "ok"),
      statewide = if_else("all" %in% geos, "fake county snapshot", "no CBS county snapshot yet"),
      settings = "fake model output from make_dashboard_test_data.R"
    )
  list(pred = pred, status = status)
}

model_dir = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "model_results")
fs::dir_delete(model_dir) |> try(silent = TRUE)
fs::dir_create(model_dir)
fm = pmap(FAKE_RACES, fake_models)
fm_pred = map(fm, "pred") |> bind_rows()
fm_status = map(fm, "status") |> bind_rows()
iso = \(t) format(t, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
stamp = \(t) format(t, "%Y%m%d_%H%M%S", tz = "UTC")
for (t in sort(unique(fm_pred$time))) {
  t = as.POSIXct(t, tz = "UTC")
  readr::write_csv(mutate(filter(fm_pred, time == t), time = iso(time)), fs::path(model_dir, glue("predictions_{stamp(t)}.csv")), na = "")
  readr::write_csv(mutate(filter(fm_status, time == t), time = iso(time)), fs::path(model_dir, glue("status_{stamp(t)}.csv")), na = "")
}
cli::cli_inform("fake model results: {n_distinct(fm_pred$time)} runs in {model_dir}")

dd = build_dashboard_data(unname(dash), models = read_model_results(model_dir))
saveRDS(dd, fs::path(PATH_DROPBOX, ELECTION_FOLDER, "dashboard_data.rds"))
# committed copy; pushing it to main redeploys GitHub Pages (.github/workflows/pages.yml)
saveRDS(dd, "data/dashboard/dashtest_data.rds")

print(dd$meta, width = Inf)
cli::cli_inform("ts rows: {nrow(dd$ts)}; prec rows: {nrow(dd$prec)}")
