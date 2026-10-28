# ========================================
## Make a stress-test scenario from one of Ethan's simulated nights
##
## Reads 26_sim/<from>/ (TX_truth.parquet, TX_schedule.parquet, manifest.json), applies one
## or more distortions that a uniform swing cannot absorb, and writes 26_sim/<name>/ in the
## same format, so scripts/model/live/tests/sim_night.R runs it like any of Ethan's scenarios. Ethan's code
## and files are not touched.
##
## Distortions (combine freely):
##   --hisp_swing=<pts>   Hispanic voters swing toward the Democrat by <pts> of the two-party
##                        vote, on top of the base scenario. Each precinct x method x race
##                        moves by (its Hispanic share of voters) x <pts>, with the cell's
##                        two-party total held fixed (votes move from Rep to Dem). Hispanic
##                        share of voters = YouGov's yg_pred_his from TX_precinct.parquet.
##                        Default when the name contains "hispanic": 0.211, from Trump winning
##                        TX Latinos by ~10 in 2024 (Dem two-party ~45%) and 2026 polling of
##                        roughly 60-63% Dem to 30-33% Rep (Dem two-party ~66%).
##   --urban_first        within each county, Election Day votes report in order of the
##                        precinct's Harris 2024 share, most Democratic first (a proxy for
##                        urban cores reporting first: the frame has no urbanicity measure).
##                        Each county keeps its own set of report times; only which precinct
##                        gets which time changes. Default when the name contains "urban".
##
## Run from the cbs-enr folder:
##   Rscript scripts/sim/make_stress_scenario.R tx_hispanic_swing
##   Rscript scripts/sim/make_stress_scenario.R tx_urbanfirst
##   Rscript scripts/sim/make_stress_scenario.R tx_both --hisp_swing=0.211 --urban_first
##   (--from=<scenario> to start from something other than tx_base)
# ========================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("scripts/sim/sim_config.R")   # DIR_26, DIR_SIMS, read_pq(), write_pq()
args = commandArgs(trailingOnly = TRUE)
NAME = grep("^--", args, value = TRUE, invert = TRUE)[1]
if (is.na(NAME) || !grepl("^[A-Za-z0-9_]+$", NAME)) stop("Give the new scenario a name (letters, digits, _), e.g. tx_hispanic_swing")
opt = function(key, default = NA) {
  a = grep(paste0("^--", key, "(=|$)"), args, value = TRUE)
  if (!length(a)) return(default)
  if (!grepl("=", a[1])) return(TRUE)
  sub("^[^=]*=", "", a[1])
}
FROM = opt("from", "tx_base")
HISP = as.numeric(opt("hisp_swing", if (grepl("hispanic", NAME)) 0.211 else 0))
URBAN = isTRUE(opt("urban_first", grepl("urban", NAME)))
if (HISP == 0 && !URBAN) stop("No distortion chosen: use --hisp_swing=<pts> and/or --urban_first")

dir_from = file.path(DIR_SIMS, FROM); dir_to = file.path(DIR_SIMS, NAME)
if (!dir.exists(dir_from)) stop("No scenario ", FROM, " in ", DIR_SIMS)
dir.create(dir_to, showWarnings = FALSE, recursive = TRUE)
truth = read_pq(file.path(dir_from, "TX_truth.parquet"))
sched = read_pq(file.path(dir_from, "TX_schedule.parquet"))
manifest = jsonlite::fromJSON(file.path(dir_from, "manifest.json"), simplifyVector = FALSE)
inp = read_pq(file.path(DIR_26, "input_data", "model_inputs", "TX_precinct.parquet")) |>
  transmute(geo = as.character(geo), hisp = coalesce(yg_pred_his, p_hisp), dem2)
notes = character()

# ---- Hispanic swing ------------------------------------------------------------------
if (HISP != 0) {
  cells = truth |>
    mutate(geo = as.character(geo)) |>
    summarize(d = sum(votes[party == "dem"]), r = sum(votes[party == "rep"]), .by = c(contest, geo, method)) |>
    left_join(inp, by = "geo") |>
    mutate(two = d + r,
           s_new = if_else(two > 0, pmin(pmax(d / pmax(two, 1) + coalesce(hisp, 0) * HISP, 0.005), 0.995), NA_real_),
           d_new = if_else(two > 0, round(s_new * two), d), r_new = two - d_new) |>
    select(contest, geo, method, d_new, r_new)
  truth = truth |>
    mutate(geo_c = as.character(geo)) |>
    left_join(cells, by = c("contest", "geo_c" = "geo", "method")) |>
    # one Dem and one Rep candidate per race in the sim
    mutate(votes = case_when(party == "dem" ~ d_new, party == "rep" ~ r_new, TRUE ~ votes)) |>
    select(-geo_c, -d_new, -r_new)
  notes = c(notes, sprintf("Hispanic voters swing %+.1f pts of the two-party vote toward the Democrat (precinct shift = Hispanic share of voters x %.3f)", 100 * HISP, HISP))
}

# ---- urban cores report Election Day first -------------------------------------------
if (URBAN) {
  ed = sched |> mutate(geo_c = as.character(geo)) |> filter(method == "eday", !is.na(t)) |>
    left_join(select(inp, geo, dem2), by = c("geo_c" = "geo"))
  if (any(ed |> count(geo_c, method) |> pull(n) > 1))
    stop("Election Day has more than one batch per precinct in ", FROM, "; update make_stress_scenario.R")
  reassigned = ed |>
    mutate(t_new = sort(t)[rank(-coalesce(dem2, 0), ties.method = "first")], .by = county_cbs) |>
    select(geo_c, t_new)
  sched = sched |> mutate(geo_c = as.character(geo)) |>
    left_join(reassigned, by = "geo_c") |>
    mutate(t = if_else(method == "eday" & !is.na(t_new), t_new, t)) |>
    select(-geo_c, -t_new)
  notes = c(notes, "Election Day votes report most-Democratic precincts first within each county (proxy for urban cores first)")
}

# ---- write, with a manifest saying what changed ----------------------------------------
write_pq(truth, file.path(dir_to, "TX_truth.parquet"))
write_pq(sched, file.path(dir_to, "TX_schedule.parquet"))
manifest$scenario = NAME
manifest$created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
manifest$stress = list(from = FROM, made_by = "scripts/sim/make_stress_scenario.R", changes = as.list(notes))
manifest$params$stress_hisp_swing = HISP
manifest$params$stress_urban_first = URBAN
tot = truth |> summarize(dem = sum(votes[party == "dem"]), rep = sum(votes[party == "rep"]), .by = contest)
manifest$races = lapply(manifest$races, function(r) {
  x = tot[tot$contest == r$contest, ]
  if (nrow(x)) { r$dem = x$dem; r$rep = x$rep; r$dem2 = x$dem / (x$dem + x$rep) }
  r
})
writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA),
           file.path(dir_to, "manifest.json"))
cat(sprintf("Wrote %s from %s:\n  %s\n", dir_to, FROM, paste(notes, collapse = "\n  ")))
print(as.data.frame(mutate(tot, dem2 = round(100 * dem / (dem + rep), 2))), row.names = FALSE)
cat("Next: Rscript scripts/model/live/tests/sim_night.R", NAME, "\n")
