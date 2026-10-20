# ========================================
## Build fake general-election test data for the dashboard (index.qmd)
##
## Reads the 26_primaries clean snapshots, merges party primaries into one
## Dem-vs-Rep contest (sen_dem + sen_rep -> sen), replays every snapshot through
## make_summary() into {TEST_FOLDER}/{STATE}/summaries, writes fake history files
## for GA (no methods) and NC (with methods), copies the dashboard states' crosswalks
## and L2 files from 26_general/input_data, and saves the dashboard data rds.
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
saveRDS(dd, fs::path(PATH_DROPBOX, ELECTION_FOLDER, "dashboard_data.rds"))
# committed copy; pushing it to main redeploys GitHub Pages (.github/workflows/pages.yml)
saveRDS(dd, "data/dashboard/dashtest_data.rds")

print(dd$meta, width = Inf)
cli::cli_inform("ts rows: {nrow(dd$ts)}; prec rows: {nrow(dd$prec)}")
