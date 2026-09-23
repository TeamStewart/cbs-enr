# ========================================
## Build fake general-election test data for the dashboard (index.qmd)
##
## Reads the 26_primaries clean snapshots, merges party primaries into one
## Dem-vs-Rep contest (sen_dem + sen_rep -> sen), replays every snapshot through
## make_summary() into {TEST_FOLDER}/{STATE}/summaries, writes fake history files
## for GA (no methods) and NC (with methods), and saves the dashboard data rds.
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

to_general <- function(d) {
  d |>
    mutate(
      contest = str_remove(as.character(contest), "_(dem|rep)$"),
      party = as.character(party)
    )
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
    arrange(path)

  for (i in seq_len(nrow(files))) {
    d = nanoparquet::read_parquet(files$path[i]) |> to_general()
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

fs::dir_create(fs::path(PATH_DROPBOX, ELECTION_FOLDER, "history"))
nanoparquet::write_parquet(fake_history("GA", FALSE), fs::path(PATH_DROPBOX, ELECTION_FOLDER, "history", "GA_history.parquet"))
nanoparquet::write_parquet(fake_history("NC", TRUE), fs::path(PATH_DROPBOX, ELECTION_FOLDER, "history", "NC_history.parquet"))

dd = build_dashboard_data(unname(dash))
saveRDS(dd, fs::path(PATH_DROPBOX, ELECTION_FOLDER, "dashboard_data.rds"))

print(dd$meta, width = Inf)
cli::cli_inform("ts rows: {nrow(dd$ts)}; prec rows: {nrow(dd$prec)}")
