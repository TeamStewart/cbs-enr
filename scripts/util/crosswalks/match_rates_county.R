# ========================================
## Crosswalk match rates BY COUNTY, for the L2 and VoteHub maps, against the current feed.
##
##   Rscript scripts/util/crosswalks/match_rates_county.R            # threshold 0.85
##   Rscript scripts/util/crosswalks/match_rates_county.R 0.9
##
## L2 is measured the way the full files join it: an ENR precinct counts as matched when
## l2_precinct_map() in scripts/util/full_join.R returns L2 covariates for it (frame
## crosswalk, then l2_direct/, then the CA consolidated members files). VoteHub: the
## precinct has a unit_id in crosswalks/votehub/{st}.csv. Both key on geo_key()
## (match_rates.R's xw_county() / xw_prec()).
##
## Reporting units that are not places (absentee, early voting, provisional, transfer,
## AK "-question", CA "VBM-AV", ...; NONPRECINCT below) can never match and are left out of
## the rates; their count is reported as n_nonprecinct.
##
## Writes, under 26_general/input_data/crosswalks/match_report/:
##   county_match_{stamp}.csv   state, map, county, n_feed, n_match, rate, n_nonprecinct,
##                              unmatched_feed (up to 8 examples)
##   state_match_{stamp}.csv    state, map, n_feed, n_match, rate, n_zero_counties,
##                              n_counties_below
## and prints the states and counties below the threshold. Read-only otherwise.
# ========================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(glue)
  library(purrr)
})
source("scripts/util/globals.R")
source("scripts/util/full_join.R") # l2_precinct_map(), geo_key(); sources match_rates.R
source("scripts/model/live/R/paths.R")

args = commandArgs(trailingOnly = TRUE)
thr = if (length(args)) as.numeric(args[1]) else 0.85

d26 = find_26_general()
if (!nzchar(PATH_DROPBOX)) PATH_DROPBOX = dirname(d26) # full_join.R's input_path()

NONPRECINCT = paste0(
  "(?i)(^|[-\\s])(absentee|provisional|transfer|curbside|spare|uocava|overseas)( .*)?$",
  "|^early voting|^ev\\s|^one stop|vbm|-early$|-question$"
)

files = clean_files(d26)
geos = lapply(split(files, names(files)), \(ff) unique(unlist(lapply(ff, \(x) as.character(nanoparquet::read_parquet(x, col_select = "geo")$geo)))))
ex = \(v, k = 8) paste(head(sort(unique(v)), k), collapse = " | ")

county_rows = bind_rows(lapply(sort(names(geos)), function(st) {
  cf = enightmodels::cbs_counties[enightmodels::cbs_counties$state == st, ]
  m = str_match(geos[[st]], "^[a-z]{2}_(\\d+)_(.*)$")
  fd = tibble(geo = geos[[st]], cbs = as.integer(m[, 2]), p = m[, 3]) |>
    filter(!is.na(cbs)) |>
    mutate(county = toupper(cf$county)[match(cbs, as.integer(cf$county_cbs))],
           county = coalesce(county, paste0("cbs ", cbs)),
           gkey = geo_key(st, geo),
           nonprecinct = str_detect(p, NONPRECINCT))
  maps = list()
  lm = tryCatch(l2_precinct_map(st), error = function(e) {
    cli::cli_alert_warning("{st} L2 map failed: {conditionMessage(e)}")
    NULL
  })
  if (!is.null(lm)) maps$l2 = lm$gkey
  vh = file.path(d26, "input_data", "crosswalks", "votehub", paste0(tolower(st), ".csv"))
  if (file.exists(vh)) {
    v = data.table::fread(vh, colClasses = "character", select = c("county", "precinct_enr", "unit_id"))
    v = v[!is.na(unit_id) & nzchar(unit_id)]
    maps$votehub = paste(xw_county(st, v$county), xw_prec(v$precinct_enr))
  }
  bind_rows(lapply(names(maps), function(nm) {
    fd |>
      mutate(hit = gkey %in% maps[[nm]]) |>
      summarise(
        state = st, map = nm,
        n_feed = sum(!nonprecinct), n_match = sum(hit & !nonprecinct),
        rate = round(n_match / pmax(n_feed, 1), 4),
        n_nonprecinct = sum(nonprecinct),
        unmatched_feed = ex(p[!hit & !nonprecinct]),
        .by = county
      )
  }))
})) |>
  relocate(state, map) |>
  arrange(state, map, rate, desc(n_feed))

state_rows = county_rows |>
  summarise(
    n_zero_counties = sum(n_match == 0 & n_feed > 0),
    n_counties_below = sum(rate < thr & n_feed > 0),
    n_nonprecinct = sum(n_nonprecinct),
    n_feed = sum(n_feed), n_match = sum(n_match),
    rate = round(n_match / pmax(n_feed, 1), 4),
    .by = c(state, map)
  ) |>
  relocate(state, map, n_feed, n_match, rate)

stamp = format(Sys.time(), "%Y%m%d_%H%M")
dir_out = file.path(d26, "input_data", "crosswalks", "match_report")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
readr::write_csv(county_rows, file.path(dir_out, paste0("county_match_", stamp, ".csv")))
readr::write_csv(state_rows, file.path(dir_out, paste0("state_match_", stamp, ".csv")))

options(width = 200)
cat(sprintf("\n== States (threshold %.0f%%; non-precinct units excluded) ==\n", 100 * thr))
print(as.data.frame(state_rows))
cat(sprintf("\n== Counties below %.0f%% (most unmatched first) ==\n", 100 * thr))
print(as.data.frame(county_rows |>
  filter(rate < thr, n_feed > 0) |>
  arrange(desc(n_feed - n_match)) |>
  select(state, map, county, n_feed, n_match, rate)))
cat("\nWrote", dir_out, paste0("{county,state}_match_", stamp, ".csv"), "\n")
