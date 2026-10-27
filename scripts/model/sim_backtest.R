# ========================================
## Run the election-night model against Ethan's simulated TX night
##
## Demonstrates the model end-to-end on `scripts/sim/` output, using the PRODUCTION
## covariate file (model_inputs/TX_precinct.parquet, built by prep_tx_model_inputs.R)
## rather than a backtest-specific panel. The sim's truth is drawn from the same file,
## so this exercises the live join path, not a parallel one.
##
## WHY THIS SUITE RATHER THAN model_turnout.R's own make_stages(): the sim's schedule is
## one row per (geo, method, BATCH) with a cumulative fraction, so cells accumulate
## PARTIALLY. make_stages reveals cells all-or-nothing and therefore cannot exercise the
## partial-count machinery at all. The sim also emits `stage` in the real 0 / 4 / 6
## vocabulary -- and measuring it showed stage 6 is UNREACHABLE on the night (late mail
## keeps landing for days), so the run below is made with NO completeness flag at all.
## See the STAGE_TRUST note in as_of() for the numbers.
##
## WHAT IT DOES NOT SHOW: the sim's truth is generated from `dem2`, which the model also
## uses as a prior, so accuracy here is partly by construction. It is a plumbing and
## regression check, not evidence about reality -- the TX 2024 real-cadence backtest
## remains the only out-of-sample gate. Its mode lean also uses NC 2024 values, and TX
## 2024 measured the opposite sign (election day runs 2.6pp MORE Democratic than early),
## so the red mirage it produces is not Texas's.
##
## Run:  PATH_DROPBOX=... SIM_OUT=... Rscript scripts/model/sim_backtest.R [contest]
## Env:  CONTEST (gov|sen, default gov), SCENARIO (default tx_base), PLOT=0 to skip
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr); library(lubridate); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
})
source("scripts/sim/sim_snapshot.R")

CONTEST  <- Sys.getenv("CONTEST",  unset = "gov")
STAGE_TRUST <- Sys.getenv("STAGE_TRUST", unset = "1") == "1"
SCENARIO <- Sys.getenv("SCENARIO", unset = "tx_base")
# PATH_DROPBOX env var if set, else this machine's Dropbox (live/R/paths.R)
PATH_DROPBOX <- local({ source("scripts/model/live/R/paths.R", local = TRUE); find_dropbox() })
DIR_IN  <- path.expand(file.path(PATH_DROPBOX, "26_general/input_data/model_inputs"))
DIR_SIM <- file.path(Sys.getenv("SIM_OUT", unset = file.path(path.expand(PATH_DROPBOX), "26_sim")), SCENARIO)
SP      <- Sys.getenv("SIM_WORK", unset = tempdir())

cli_h1("Model vs simulated TX night -- scenario {SCENARIO}, contest {CONTEST}")

# ---- 1. adapt the production covariate file to load_covs()'s shape ----------------
# load_covs expects one row per (precinct, party) with `est`; TX_precinct.parquet is wide.
prec_wide <- read_parquet(file.path(DIR_IN, "TX_precinct.parquet")) |> as_tibble()
covs <- prec_wide |>
  transmute(
    geo, county = county_name, geostratum, stratum_demo, n,
    turnout22, turnout24, p_male, p_age_65_up, p_black, p_hisp, p_col,
    dem2,
    tp_raw = oth_base,                       # third-party prior, supplied directly
    est_dem = est_turnout * dem2, est_rep = est_turnout * (1 - dem2)) |>
  pivot_longer(c(est_dem, est_rep), names_to = "party", values_to = "est",
               names_prefix = "est_") |>
  mutate(method = "all")
f_covs <- file.path(SP, "sim_covs.parquet"); write_parquet(covs, f_covs)

# ---- 2. mode history -> the schema derive_mode_priors() expects -------------------
# The production file is (geo, election, method, voters). derive_mode_priors needs
# party-split votes; splitting each mode's total at the precinct's own lean carries the
# TURNOUT composition and no partisan differential, which is what we want -- delta_m is
# set separately by MODE_OFFSET_SOURCE.
mh <- read_parquet(file.path(DIR_IN, "TX_precinct_mode_history.parquet")) |> as_tibble()
MODE_YEAR <- Sys.getenv("MODE_YEAR", unset = "2022_general")
mh2 <- mh |> filter(election == MODE_YEAR) |>
  inner_join(select(prec_wide, geo, dem2), by = "geo") |>
  transmute(geo, method, dem = round(voters * dem2), rep = voters - round(voters * dem2)) |>
  pivot_longer(c(dem, rep), names_to = "party", values_to = "votes") |>
  mutate(election = MODE_YEAR, contest = CONTEST) |>
  select(geo, election, contest, method, party, votes)
f_mh <- file.path(SP, "sim_mode_history.parquet"); write_parquet(mh2, f_mh)

Sys.setenv(
  SOURCE_ONLY = "1", PATH_COVS = f_covs, PATH_MODE_HIST = f_mh,
  MODE_HIST_PREFER = MODE_YEAR, MODE_OFFSET_SOURCE = "zero",
  COVS_TURN  = "log_n,turnout22,turnout24,p_male,p_age_65_up,p_black,p_hisp,p_col,dem2",
  COVS_SHARE = "log_n,turnout22,turnout24,p_male,p_age_65_up,p_black,p_hisp,p_col,dem2",
  COVS_TP    = "log_n,p_male,p_age_65_up,dem2,tp_prior_raw")
source("scripts/model/model_turnout.R")   # config, cells, prec, CTY_MAT, fit_stage

# ---- 3. the sim ------------------------------------------------------------------
truth_all <- read_parquet(file.path(DIR_SIM, "TX_truth.parquet")) |> as_tibble()
sched     <- read_parquet(file.path(DIR_SIM, "TX_schedule.parquet")) |> as_tibble()
truth <- truth_all |> filter(contest == CONTEST)

# truth over the precincts the model actually carries
tru_cell <- truth |> filter(geo %in% prec$geo) |>
  summarize(votes = sum(votes), .by = c(geo, method, party)) |>
  pivot_wider(names_from = party, values_from = votes, values_fill = 0)
for (p0 in c("dem", "rep", "oth")) if (!p0 %in% names(tru_cell)) tru_cell[[p0]] <- 0
TT <- tru_cell |> summarize(dem = sum(dem), rep = sum(rep), oth = sum(oth))
T_turn <- TT$dem + TT$rep + TT$oth
T_dem <- TT$dem / T_turn; T_2p <- TT$dem / (TT$dem + TT$rep)
cli_inform(c(
  "model carries {format(nrow(prec), big.mark=',')} of \\
   {format(n_distinct(truth$geo), big.mark=',')} sim precincts",
  "TRUTH: Dem {round(100*T_dem,3)}% of all votes, two-party {round(100*T_2p,3)}%, \\
   turnout {format(T_turn, big.mark=',')}"))

# A snapshot is already in the enightmodels schema; map it to fit_stage's returns shape.
# `complete` would come from the feed's stage == 6 -- but stage 6 is unreachable on
# the night, so in practice the night runs with no completeness flag. See STAGE_TRUST.
as_of <- function(tt) {
  sn <- sim_snapshot_rows(truth, sched, tt, zero_post = "19:00")
  sn <- filter(sn, geo %in% prec$geo)
  # the feed's own completeness, in the real 0 / 4 / 6 vocabulary: 6 = every batch landed
  st <- sn |> summarize(sim_stage = max(stage), .by = c(geo, method))
  sn |>
    mutate(party = if_else(party %in% c("dem", "rep"), party, "oth")) |>
    summarize(votes = sum(votes), .by = c(geo, method, party)) |>
    pivot_wider(names_from = party, values_from = votes, values_fill = 0) |>
    { \(x) { for (p0 in c("dem","rep","oth")) if (!p0 %in% names(x)) x[[p0]] <- 0; x } }() |>
    left_join(st, by = c("geo", "method")) |>
    transmute(geo, mode = method,
              stage = if_else(dem + rep + oth > 0, STAGE_DONE, 0L),
              # STAGE_TRUST=0 withholds the feed's completeness entirely. In production
              # `stage` reaches 6 days late, differs by state, is downgraded by our own
              # loader, and has been observed moving BACKWARDS, so the switch exists to
              # test rather than assert that the aggregate needs no completeness flag.
              #
              # MEASURED (2026-10-19), and the answer is that the flag is unreachable on
              # the night rather than merely untrusted. stage 6 means EVERY batch landed,
              # and the sim's late mail arrives over late_mail_days = c(1, 6):
              #     21:00  stage6 =    0      11-05 12:00  stage6 =  371
              #     23:00  stage6 =    0      11-07 12:00  stage6 = 1468
              #     11-04 06:00  stage6 = 0   11-10 12:00  stage6 = 5565
              # So `complete` is FALSE at every election-night stage in BOTH arms. Run
              # with identical env, the two arms differ on exactly ONE line of the stage
              # table -- the terminal canvass stage, the only one where the flag is
              # reachable:
              #     STAGE_TRUST=1   11-10 12:00  err +0.06  width 0.42  turn 103.9
              #     STAGE_TRUST=0   11-10 12:00  err +0.02  width 0.59  turn 105.0
              # Trusting it tightens the final interval, because 502 censored precincts
              # plus dark PARKER county leave 9.8% of the vote unreported even at the
              # end, and the flag decides which cells the TURNOUT equation fits on.
              #
              # The headline 13/13 at 0.21pp is the STAGE_TRUST=0 arm: earned with NO
              # completeness information, which is the realistic condition rather than a
              # flattering one. That is what the mixture was built for -- rem = T -
              # counted pins a finished cell without needing a classifier.
              complete = if (STAGE_TRUST) coalesce(sim_stage, 0L) >= 6L else FALSE,
              votes_dem = if_else(stage >= STAGE_DONE, as.numeric(dem), NA_real_),
              votes_rep = if_else(stage >= STAGE_DONE, as.numeric(rep), NA_real_),
              votes_oth = if_else(stage >= STAGE_DONE, as.numeric(oth), NA_real_))
}

STAGES <- c("20:00","20:15","20:30","21:00","21:30","22:00","22:30","23:00","23:30",
            "2026-11-04 00:30","2026-11-04 02:00","2026-11-04 06:00","2026-11-10 12:00")

run_stage <- function(tt) {
  r <- as_of(tt)
  cnt <- r |> filter(stage >= STAGE_DONE)
  ctot <- sum(cnt$votes_dem + cnt$votes_rep + cnt$votes_oth, na.rm = TRUE)
  cd <- sum(cnt$votes_dem, na.rm = TRUE); cr <- sum(cnt$votes_rep, na.rm = TRUE)
  f <- fit_stage(cells, r)
  ci <- cred_int(f$S_dem, level = LEVEL); ci2 <- cred_int(f$S_2p, level = LEVEL)
  tibble(stage = tt, ts = sim_time(tt, truth$election[1]),
         cells = nrow(cnt), pct_votes = 100 * ctot / T_turn,
         naive_2p = if (cd + cr > 0) 100 * cd / (cd + cr) else NA_real_,
         est = 100*mean(f$S_dem), lo = 100*ci[["lower"]], hi = 100*ci[["upper"]],
         err = 100*(mean(f$S_dem) - T_dem),
         covers = T_dem >= ci[["lower"]] && T_dem <= ci[["upper"]],
         est_2p = 100*mean(f$S_2p), lo_2p = 100*ci2[["lower"]], hi_2p = 100*ci2[["upper"]],
         err_2p = 100*(mean(f$S_2p) - T_2p),
         cov_2p = T_2p >= ci2[["lower"]] && T_2p <= ci2[["upper"]],
         turn = 100 * mean(f$turn) / T_turn)
}

res <- map_dfr(STAGES, run_stage)
cli_h2("Two-party Dem share -- truth {round(100*T_2p,3)}%")
res |> transmute(stage, pct_votes = round(pct_votes,1), naive_2p = round(naive_2p,2),
                 est_2p = round(est_2p,2), err_2p = round(err_2p,2), cov_2p,
                 width = round(hi_2p - lo_2p, 2), turn = round(turn,1)) |>
  as.data.frame() |> print(row.names = FALSE)
cli_inform(c("coverage (two-party): {sum(res$cov_2p)}/{nrow(res)}",
             "mean |err|: {round(mean(abs(res$err_2p)),2)}pp   \\
              max |err|: {round(max(abs(res$err_2p)),2)}pp"))

if (Sys.getenv("PLOT", unset = "1") != "0") {
  source("scripts/model/plot_night.R")
  T_2p_sim <- T_2p
  plot_night(res, arm = CONTEST, truth_2p = T_2p_sim,
             tag = paste0("sim-", SCENARIO),
             title = sprintf("Simulated TX 2026 %s: model vs simulated election night [%s]",
                             c(gov = "Governor", sen = "US Senate")[CONTEST] |>
                               coalesce(toupper(CONTEST)), SCENARIO))
}
saveRDS(res, file.path(SP, sprintf("sim_res_%s_%s.rds", SCENARIO, CONTEST)))
cli_alert_success("results -> {file.path(SP, sprintf('sim_res_%s_%s.rds', SCENARIO, CONTEST))}")
