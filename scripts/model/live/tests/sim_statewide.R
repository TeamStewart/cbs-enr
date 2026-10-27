# ========================================
## A simulated election night at STATE level: our models on the scraped counties (one of
## Ethan's simulated nights) + a simulated CBS county feed for every other county, run
## through the same code as on the night (predict_all() with cbs_for_races()-style
## snapshots), compared with the statewide truth at every checkpoint.
##
## Ethan's sims cover only the counties we scrape (TX: 32). The other counties' results
## are simulated here, consistent with the sim night:
##   final result   county c's two-party Dem share
##                    = logit^-1( logit(Harris 2024 share in c) + swing + offset + e_c )
##                  swing  = the sim night's swing in the scraped counties vs Harris 2024
##                  offset = the rest of the state swinging differently (--offset, points
##                           of logit; default a draw from N(0, 0.05) per race)
##                  e_c    ~ N(0, 0.08) per county, shared by the races
##                  turnout = 2022 governor votes x the sim night's turnout ratio in the
##                  scraped counties x exp(N(0, 0.10)); third-party share = 2024's
##   reporting      each county starts between 8:00 and 9:30pm ET and counts along
##                  1 - exp(-(t - start) / tau), tau 40-150 minutes, up to 97% by 4am ET,
##                  then 98 / 99 / 99.5 / 100% at the Nov 4 noon / Nov 5 / Nov 7 / Nov 10
##                  checkpoints. CBS's pct_exp_vote is that share, rounded down.
##                  What is counted first is not representative: the counted two-party
##                  share is off by b_c x (1 - share counted), b_c ~ N(0, 0.15) logit
##                  (about +/- 4 points early on), as early and Election Day votes differ.
## The CBS-format snapshots go to
##   26_general/model_tests/statewide_sim/<scenario>[_offset<x>]/cbs_api/sim/TX_<contest>/
## (one per checkpoint, named by its UTC time), with truth_statewide.csv and
## truth_counties.csv beside them, and are read back with read_cbs_latest(), exactly as
## live snapshots are.
##
## NOTE: the rest of the state is simulated with the same kind of model counties.R
## assumes (a common swing plus independent county noise), so this tests the plumbing
## and how precision builds over the night, not whether that assumption holds.
## --offset=0.15 (or -0.15) makes the rest of the state swing differently from the
## scraped counties by much more than the model allows for.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/sim_statewide.R [scenario] [--models=swing,ratio_strata,medsl_sur]
##           [--offset=<logit>] [--seed=<n>] [--checkpoints=all|calib] [--races=gov]
##           [--snapshots-only]   (write the simulated CBS feed + truth, don't run the models)
## Writes 26_general/model_tests/SIM_statewide_<scenario>[_offset<x>][_<race>].csv
# ========================================
source("scripts/model/live/load.R")
args = commandArgs(trailingOnly = TRUE)
opt = function(k, d) { a = grep(paste0("^--", k, "="), args, value = TRUE); if (length(a)) sub("^[^=]*=", "", a[1]) else d }
SCENARIO = c(grep("^--", args, value = TRUE, invert = TRUE), "tx_base")[1]
MODELS_RUN = strsplit(opt("models", paste(MODELS, collapse = ",")), ",")[[1]]
SEED = as.integer(opt("seed", "2026"))
OFFSET = opt("offset", NA)
CHECKS = opt("checkpoints", "all")
RACES_RUN = opt("races", NA)        # e.g. --races=gov to run one race (then the output file is per race)
STATE = "TX"

d26 = find_26_general()
dir_sc = if (dir.exists(SCENARIO)) SCENARIO else file.path(dirname(d26), "26_sim", SCENARIO)
sim = read_sim(dir_sc)
cells = build_cells(STATE, d26)
base = county_baseline(STATE, d26)
p_cty = file.path(d26, "input_data", "model_inputs", paste0(STATE, "_counties.csv"))
if (!file.exists(p_cty)) stop("Missing ", p_cty, ": run scripts/model/prep_model_inputs.R ", STATE)
lookup = readr::read_csv(p_cty, col_types = readr::cols(.default = "c"))
scraped = scraped_counties(cells)
races_all = sort(unique(sim$truth$contest))
lg = function(x) qlogis(pmin(pmax(x, 1e-4), 1 - 1e-4))
tz = "America/New_York"
times = if (CHECKS == "calib") {
  as.POSIXct(c("2026-11-03 20:30", "2026-11-03 21:00", "2026-11-03 21:30", "2026-11-03 22:30",
               "2026-11-04 00:00", "2026-11-04 04:00", "2026-11-10 12:00"), tz = tz)
} else sim_checkpoints(tz)

# ---- 1. the other counties' final results ------------------------------------------------
set.seed(SEED)
rest = lookup |>
  filter(!county_cbs %in% scraped) |>
  inner_join(select(base, county_fips, h24, oth24, exp_base), by = "county_fips") |>
  mutate(e = rnorm(n(), 0, 0.08), u = rnorm(n(), 0, 0.10),
         start = as.POSIXct("2026-11-03 20:00", tz = tz) + runif(n(), 0, 90) * 60,
         tau = runif(n(), 40, 150), b = rnorm(n(), 0, 0.15))
b32 = filter(base, county_fips %in% lookup$county_fips[lookup$county_cbs %in% scraped])
h32 = sum(b32$pres24_dem) / sum(b32$pres24_dem + b32$pres24_rep); e32 = sum(b32$exp_base)

truth32 = sim$truth |> filter(geo %in% cells$geo) |>
  summarize(dem = sum(votes[party == "dem"]), rep = sum(votes[party == "rep"]),
            oth = sum(votes[!party %in% c("dem", "rep")]), .by = contest)
offsets = setNames(if (is.na(OFFSET)) rnorm(length(races_all), 0, 0.05) else rep(as.numeric(OFFSET), length(races_all)), races_all)
races = if (is.na(RACES_RUN)) races_all else intersect(races_all, strsplit(RACES_RUN, ",")[[1]])
# the simulated feed lives in one folder per scenario (both races); results per run
tag_cbs = paste0(basename(dir_sc), if (!is.na(OFFSET)) paste0("_offset", OFFSET) else "")
tag_run = paste0(tag_cbs, if (!is.na(RACES_RUN)) paste0("_", paste(races, collapse = "_")) else "")
SNAP_ONLY = "--snapshots-only" %in% args
rest_final = map_dfr(races_all, function(r) {
  t32 = filter(truth32, contest == r)
  swing = lg(t32$dem / (t32$dem + t32$rep)) - lg(h32)
  ratio = (t32$dem + t32$rep + t32$oth) / e32
  rest |> mutate(contest = r, s = plogis(lg(h24) + swing + offsets[[r]] + e),
                 T = exp_base * ratio * exp(u), oth = oth24)
})
truth = rest_final |>
  summarize(dem = sum(T * (1 - oth) * s), rep = sum(T * (1 - oth) * (1 - s)), oth = sum(T * oth), .by = contest) |>
  bind_rows(truth32) |>
  summarize(across(c(dem, rep, oth), sum), .by = contest) |>
  mutate(geo = "all") |>
  bind_rows(mutate(truth32, geo = "scraped")) |>
  mutate(tot = dem + rep + oth)
cat(sprintf("Scenario %s; rest of state offset %s (logit); statewide truth:\n", basename(dir_sc),
            paste(sprintf("%s %+.3f", names(offsets), offsets), collapse = ", ")))
print(as.data.frame(truth |> transmute(contest, geo, dem_share = round(dem / tot, 4), rep_share = round(rep / tot, 4),
                                       two_party = round(dem / (dem + rep), 4), total = round(tot))))

# ---- 2. CBS-format snapshots, one per checkpoint ---------------------------------------------
dir_cbs = file.path(d26, "model_tests", "statewide_sim", tag_cbs)
for (r in races) unlink(file.path(dir_cbs, "cbs_api", "sim", paste0(STATE, "_", r)), recursive = TRUE)
cap_at = function(tm) {
  h = as.numeric(difftime(tm, as.POSIXct("2026-11-04 04:00", tz = tz), units = "hours"))
  if (h <= 0) 0.97 else if (h <= 8) 0.98 else if (h <= 32) 0.99 else if (h <= 80) 0.995 else 1
}
cand_names = sim$truth |> distinct(contest, party, candidate) |>
  mutate(party = if_else(party %in% c("dem", "rep"), party, "oth")) |>
  slice_head(n = 1, by = c(contest, party))
for (tm in as.list(times)) {
  pc = pmin(pmax(1 - exp(-pmax(as.numeric(difftime(tm, rest$start, units = "mins")), 0) / rest$tau), 0), 1) * cap_at(tm)
  for (r in races) {
    f = filter(rest_final, contest == r)
    sc = plogis(lg(f$s) + f$b * (1 - pc))                 # counted share, not yet representative
    tot = round(pc * f$T)
    snap = tibble(county_fips = f$county_fips, county_cbs = f$county_cbs, county_name = f$county_name,
                  dem = round(tot * (1 - f$oth) * sc), rep = round(tot * (1 - f$oth) * (1 - sc)),
                  county_pct_exp_vote = floor(100 * pc), prec_of = pmax(round(f$exp_base / 800), 1)) |>
      mutate(oth = tot - dem - rep) |>
      pivot_longer(c(dem, rep, oth), names_to = "party", values_to = "votes") |>
      left_join(filter(cand_names, contest == r) |> select(party, candidate), by = "party") |>
      transmute(state = STATE, contest = r, district = NA_integer_, county_fips, county_cbs, county_name,
                candidate, party, party_cbs = party, votes,
                county_total_vote = NA_real_, county_pct_exp_vote,
                county_precincts_in = floor(prec_of * county_pct_exp_vote / 100), county_precincts_of = prec_of,
                county_ts = lubridate::with_tz(tm, "UTC"), race_last_updated = lubridate::with_tz(tm, "UTC"),
                race_pct_in = NA_real_, race_call = FALSE, env = "sim", observed_at = lubridate::with_tz(tm, "UTC"))
    # CBS lists every county: the scraped ones too (their rows are not used for the
    # projection, but they place the scraped counties' history -- see counties.R)
    snap = bind_rows(snap, lookup |> filter(county_cbs %in% scraped) |>
      tidyr::crossing(filter(cand_names, contest == r) |> select(party, candidate)) |>
      transmute(state = STATE, contest = r, district = NA_integer_, county_fips, county_cbs, county_name,
                candidate, party, party_cbs = party, votes = 0, county_total_vote = NA_real_,
                county_pct_exp_vote = 0, county_precincts_in = 0, county_precincts_of = 1,
                county_ts = lubridate::with_tz(tm, "UTC"), race_last_updated = lubridate::with_tz(tm, "UTC"),
                race_pct_in = NA_real_, race_call = FALSE, env = "sim", observed_at = lubridate::with_tz(tm, "UTC")))
    tag = paste0(STATE, "_", r)
    dd = file.path(dir_cbs, "cbs_api", "sim", tag)
    dir.create(dd, recursive = TRUE, showWarnings = FALSE)
    write_pq(snap, file.path(dd, paste0(tag, "_", format(tm, "%Y%m%d_%H%M%S", tz = "UTC"), ".parquet")))
  }
}

# truth, for anyone using the feed: statewide and scraped-county totals per race, and each
# simulated county's final result
dir.create(dir_cbs, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(truth |> filter(contest %in% races) |> mutate(rest_offset = offsets[contest]),
                 file.path(dir_cbs, paste0("truth_statewide", if (!is.na(RACES_RUN)) paste0("_", paste(races, collapse = "_")) else "", ".csv")))
readr::write_csv(rest_final |> filter(contest %in% races) |>
                   transmute(contest, county_fips, county_cbs, county_name, final_votes = round(T),
                             dem2 = s, oth_share = oth, report_start = format(start, "%Y-%m-%d %H:%M", tz = tz), tau_min = tau),
                 file.path(dir_cbs, paste0("truth_counties", if (!is.na(RACES_RUN)) paste0("_", paste(races, collapse = "_")) else "", ".csv")))
cat("Simulated CBS feed and truth in", dir_cbs, "\n")
if (SNAP_ONLY) quit(save = "no")

# ---- 3. the night --------------------------------------------------------------------------
t0 = Sys.time()
res = map_dfr(as.list(times), function(tm) {
  ret = bind_rows(lapply(races, \(r) returns_from_sim(sim, r, tm, geos = cells$geo)))
  cbs = setNames(lapply(races, \(r) read_cbs_latest(STATE, r, env = "sim", at = tm, dir_26 = dir_cbs)), races)
  out = predict_all(cells, ret, models = MODELS_RUN, time = tm, cbs = cbs, base = base)
  rest_in = map_dfr(races, \(r) if (is.null(cbs[[r]])) tibble(contest = r, rest_counted = 0) else
                      tibble(contest = r, rest_counted = sum(cbs[[r]]$votes)))
  sim_in = ret |> summarize(sim_counted = sum(votes), .by = race_id) |> rename(contest = race_id)
  cat(sprintf("  %s  %s\n", format(tm, "%m-%d %H:%M"), paste(unique(out$status$statewide), collapse = "/")))
  if (!nrow(out$pred)) return(NULL)
  out$pred |>
    filter(party %in% c("dem", "rep")) |>
    left_join(rest_in, by = "contest") |> left_join(sim_in, by = "contest") |>
    mutate(time = format(tm, "%Y-%m-%d %H:%M"))
})
res = res |>
  left_join(truth |> select(contest, geo, t_dem = dem, t_rep = rep, t_tot = tot), by = c("contest", "geo")) |>
  mutate(truth = case_when(pred_type == "vote" & party == "dem" ~ t_dem, pred_type == "vote" ~ t_rep,
                           party == "dem" ~ t_dem / t_tot, TRUE ~ t_rep / t_tot),
         pct_counted = if_else(geo == "all", (sim_counted + rest_counted) / t_tot, sim_counted / t_tot),
         covers = truth >= pred_min & truth <= pred_max, scenario = basename(dir_sc),
         rest_offset = offsets[contest]) |>
  select(-t_dem, -t_rep, -t_tot)
out_f = file.path(d26, "model_tests", sprintf("SIM_statewide_%s.csv", tag_run))
readr::write_csv(mutate(res, election = format(election), time = as.character(time)), out_f, na = "")

cat(sprintf("\n%.1f minutes. Dem share of all votes, statewide (geo = all) vs the scraped counties only:\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
res |>
  filter(pred_type == "share", party == "dem") |>
  summarize(checkpoints = n(), mean_abs_err_pts = round(100 * mean(abs(pred - truth)), 2),
            max_abs_err_pts = round(100 * max(abs(pred - truth)), 2),
            mean_width_pts = round(100 * mean(pred_max - pred_min), 2),
            interval_holds_truth = if (all(is.na(covers))) NA_character_ else paste0(sum(covers, na.rm = TRUE), "/", sum(!is.na(covers))),
            .by = c(contest, geo, model)) |>
  arrange(contest, geo, model) |>
  print(n = Inf)
cat("\nWrote", out_f, "\n")
