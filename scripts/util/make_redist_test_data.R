# ========================================
## Redistricting tab test data: adds `redist` to data/dashboard/dashtest_data.rds (after
## scripts/util/make_dashboard_test_data.R builds it). Votes: the sim's Senate (and TX governor)
## ENR precinct votes, and the CBS test feed's county results for every Senate, governor and
## House race (redist_cbs_votes(): Senate / governor pulled now; House read from the pulls
## saved under 26_general/cbs_api/test/redistricting/ -- pull them first with
##   Rscript scripts/model/reports/redistricting_compare.R --live --env=test --out=<tmp folder>
## ). Model estimates: the fake precinct projections make_dashboard_test_data.R writes to
## {ELECTION_FOLDER}/model_results/precincts_latest.parquet, regrouped into districts as on the night.
## Needs the prep's files (redistricting_compare.R without --live) and CBS_API_KEY in ~/.Renviron.
## make_dashboard_test_data.R runs this at its end; run it alone to refresh only `redist`:
##
##   CBS_ELECTION_FOLDER=26_dashtest Rscript scripts/util/make_redist_test_data.R
# ========================================
suppressPackageStartupMessages({library(tidyverse); library(glue)})
source("scripts/functions.R")
source("scripts/model/live/load.R")
source("scripts/model_targets.R")
source("scripts/model/reports/redistricting_lib.R")
source("scripts/redistricting_targets.R")
f = "data/dashboard/dashtest_data.rds"
d = readRDS(f)
files = redist_files()
tm = d$meta |> distinct(key, time)
enr = d$prec |>
  filter(str_detect(key, "^(NC|FL|TX|OH|CA)-")) |>
  summarise(dem = sum(dem), rep = sum(rep), total = sum(total), .by = c(key, geo)) |>
  left_join(tm, by = "key") |>
  transmute(state = substr(key, 1, 2), contest = sub("^..-", "", key), district = NA_integer_, geo, dem, rep, total, time)
cbs = bind_rows(lapply(REDIST_STATES, \(s) redist_cbs_votes(s, env = "test", sim = "", enabled = TRUE, pull_house = FALSE)))
print(count(cbs, state, contest))
# the night passes model_results; build_redist_data() reads only its precinct projections
mp = read_model_precincts()
if (is.null(mp)) warning("No precincts_latest.parquet in ", ELECTION_FOLDER, "/model_results: no model estimates")
r = build_redist_data(files, cbs, list(prec = mp), enr)
print(r$src |> select(state, src, p_prec, note) |> mutate(note = substr(note, 1, 110)), n = 100)
print(count(r$src, state, kind))
d$redist = r
saveRDS(d, f)
