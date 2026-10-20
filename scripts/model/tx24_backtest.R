# ========================================
## TX 2024 US Senate backtest -- the REAL election-night release cadence
##
## The two reporting orders tested so far were both invented:
##   ORDER=random      representative      -> coverage 8/8, |err| <= 0.36pp (too benign)
##   ORDER=county_size small rural first   -> coverage 2/7, errors to -16.5pp
##                                            (adversarial in the GEOGRAPHIC dimension)
## The real TX curve is neither. All 16 counties report within minutes of poll close, so
## geographic coverage is near-complete from the start, but the first batches are ~97%
## EARLY VOTE and they read far more Democratic than the final. The naive
## count-what-is-in estimate is +8.6pp too Dem at 18:45 and still +1.4pp at 19:15 with
## 53% of the vote in.
##
## This driver runs the SAME fit_stage() the live model uses -- it sources
## tx_gov_trial.R with SOURCE_ONLY=1, which builds the config, `cells`, `prec` and
## `CTY_MAT` from the backtest panel and stops before the synthetic run. Re-implementing
## the fitting loop (as the M=1 tx_backtest.R does) would reintroduce the drift
## model_core.R was extracted to prevent.
##
## AS-OF SEMANTICS. Counties are scraped INDEPENDENTLY and snapshots are cumulative
## WITHIN a county, so at wall-clock t each county contributes its own latest snapshot at
## or before t. A single statewide timestamp cut is wrong.
##
## A (geo, mode) cell counts as reported only with > 0 votes in that snapshot: a zero
## eday cell at 19:00 means "polls just closed, nothing counted", not "counted zero".
## fit_stage()'s own obs test (tau > 0 & tot2 > 0) already agrees, so no special case.
##
## Inputs:  scripts/model/tx24_backtest_panel.parquet   (pre-2024 covariates)
##          scripts/model/tx24_returns_panel.parquet    (the real cadence)
##          scripts/model/tx24_truth.parquet            (final canvass)
##          scripts/model/tx24_mode_history_{2020,2022,2024}.parquet
##
## Run:   Rscript scripts/model/tx24_backtest.R
## Arms:  ARM=delta0 | nc | oracle          (default delta0)
##        HIST=2020 | 2022                  (turnout-split analogue; default 2020)
## Pass-through to the model: SEED, USE_CROSS_MODE, USE_COUNTY_RE, PRIOR_N_SHARE, ...
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
})

ARM  <- Sys.getenv("ARM",  unset = "delta0")
HIST <- Sys.getenv("HIST", unset = "2020")
P_RET   <- "scripts/model/tx24_returns_panel.parquet"
P_TRUTH <- "scripts/model/tx24_truth.parquet"

# ---- arm -> (which mode history, where delta_m comes from) -----------------------
# delta0  what ships today: real turnout split, delta_m = 0
# nc      real turnout split, delta_m borrowed from NC 2024 (the shipped sensitivity)
# oracle  the ceiling: delta_m derived from the TX 2024 final itself. LOOK-AHEAD, and
#         only meaningful as an upper bound on what delta_m could ever be worth.
arm_cfg <- switch(ARM,
  delta0 = list(hist = HIST, off = "zero"),
  nc     = list(hist = HIST, off = "nc"),
  oracle = list(hist = "2024", off = "history"),
  cli_abort("unknown ARM {ARM} (delta0 | nc | oracle)"))

Sys.setenv(
  SOURCE_ONLY = "1",
  PATH_COVS   = "scripts/model/tx24_backtest_panel.parquet",
  PATH_MODE_HIST = sprintf("scripts/model/tx24_mode_history_%s.parquet", arm_cfg$hist),
  MODE_HIST_PREFER = sprintf("%s_general", arm_cfg$hist),
  MODE_OFFSET_SOURCE = arm_cfg$off,
  # turnout24 would be look-ahead for a 2024 target; turnout18/turnout22 replace it.
  # Names are the panel's own, which is why COVS_* is env-overridable.
  COVS_TURN  = "log_n,turnout18,turnout22,p_male,p_age65,p_black,p_hisp,p_col,dem2",
  COVS_SHARE = "log_n,turnout18,turnout22,p_male,p_age65,p_black,p_hisp,p_col,dem2",
  COVS_TP    = "log_n,p_male,p_age65,dem2,tp_prior_raw"
)
# ICC defaults in tx_gov_trial.R are TX 2022 measurements (turnout 0.200 from L2, share
# 0.269 from MEDSL) -- post-2022 but PRE-2024, so legitimate here.

cli_h1("TX 2024 Senate backtest -- ARM={ARM}, mode history {arm_cfg$hist}, \\
        delta_m from '{arm_cfg$off}'")
source("scripts/model/tx_gov_trial.R")   # gives config, cells, prec, CTY_MAT, fit_stage

# ---- returns on the real cadence -------------------------------------------------
ret <- read_parquet(P_RET) |> as_tibble() |>
  filter(geo %in% prec$geo, method %in% MODES) |>
  mutate(cty = str_sub(geo, 1, 5), ts = as.POSIXct(timestamp, tz = "UTC"))
truth_cells <- read_parquet(P_TRUTH) |> as_tibble() |> filter(geo %in% prec$geo)

TRUTH <- truth_cells |> summarize(dem = sum(dem), rep = sum(rep), oth = sum(oth))
T_turn <- TRUTH$dem + TRUTH$rep + TRUTH$oth
T_dem  <- TRUTH$dem / T_turn
T_2p   <- TRUTH$dem / (TRUTH$dem + TRUTH$rep)
cli_inform(c(
  "universe: {format(nrow(prec), big.mark=',')} precincts in \\
   {n_distinct(prec$county)} counties",
  "TRUTH: Dem {round(100*T_dem, 3)}% of all votes, two-party {round(100*T_2p, 3)}%, \\
   turnout {format(T_turn, big.mark=',')}"
))

# county-level snapshot index, for the as-of join
cty_ts <- ret |> distinct(cty, ts) |> arrange(cty, ts)

# as_of(t) -> the returns table fit_stage expects (geo, mode, stage, votes_*, complete),
# built from each county's latest snapshot at or before t.
#
# `complete` is the turnout-observation flag fit_stage needs (production reads it from
# the feed's pct_reported; see nyc_enr_io.R). There is no pct_reported in the 2024 TX
# scrape, so it is derived here from the snapshot history AVAILABLE AT t: a cell counts
# as done when its total has not moved across that county's last two intervals. This is
# unbiased with respect to the cell's final size, which the prior-based fallback is not,
# and it converges -- by the end of the night every cell has plateaued, so turnout pins
# to the counted total exactly.
# Requiring TWO static intervals rather than one guards against a cell that merely
# pauses between batches.
as_of <- function(t) {
  pick <- cty_ts |> filter(ts <= t) |> slice_max(ts, n = 3, by = cty) |>
    mutate(lag = dense_rank(desc(ts)), .by = cty)       # 1 = current, 2/3 = previous
  if (!nrow(pick)) return(NULL)
  h <- ret |> inner_join(pick, by = c("cty", "ts")) |>
    transmute(geo, mode = method, lag, v = dem + rep + oth, dem, rep, oth) |>
    pivot_wider(names_from = lag, values_from = c(v, dem, rep, oth), names_sep = "")
  for (cl in c("v2", "v3")) if (!cl %in% names(h)) h[[cl]] <- NA_real_
  h |>
    transmute(geo, mode,
              stage = if_else(v1 > 0, STAGE_DONE, 0L),
              # plateaued across both available intervals; a county with fewer than
              # three snapshots so far cannot qualify, which is the conservative side
              complete = v1 > 0 & !is.na(v2) & !is.na(v3) & v1 <= v2 & v2 <= v3,
              votes_dem = if_else(stage >= STAGE_DONE, dem1, NA_real_),
              votes_rep = if_else(stage >= STAGE_DONE, rep1, NA_real_),
              votes_oth = if_else(stage >= STAGE_DONE, oth1, NA_real_))
}

# ---- the stage grid --------------------------------------------------------------
# Wall-clock marks bracketing the 16% -> 53% -> 70% jump that does the damage, plus the
# long election-day tail. `final` is each county's last snapshot, which is the truth.
STAGES <- as.POSIXct(c(
  "2024-11-05 18:45:00", "2024-11-05 19:00:00", "2024-11-05 19:15:00",
  "2024-11-05 19:30:00", "2024-11-05 20:00:00", "2024-11-05 20:30:00",
  "2024-11-05 21:00:00", "2024-11-05 21:30:00", "2024-11-05 22:00:00",
  "2024-11-05 22:30:00", "2024-11-05 23:00:00", "2024-11-05 23:59:00",
  "2024-11-06 01:00:00", "2024-11-06 03:00:00"), tz = "UTC")
STAGES <- c(STAGES, max(ret$ts))

run_stage <- function(t) {
  r <- as_of(t)
  if (is.null(r)) return(NULL)
  counted <- r |> filter(stage >= STAGE_DONE)
  cd <- sum(counted$votes_dem); cr <- sum(counted$votes_rep); co <- sum(counted$votes_oth)
  ctot <- cd + cr + co
  f <- fit_stage(cells, r)
  ci   <- cred_int(f$S_dem, level = LEVEL)
  ci2p <- cred_int(f$S_2p,  level = LEVEL)
  tibble(
    stage = format(t, "%m-%d %H:%M"),
    ts = t,
    cty_rep = n_distinct(str_sub(counted$geo, 1, 5)),
    cells = nrow(counted),
    pct_votes = 100 * ctot / T_turn,
    # the benchmark the model has to beat: just count what is in
    naive_2p = if (cd + cr > 0) 100 * cd / (cd + cr) else NA_real_,
    est    = 100 * mean(f$S_dem),
    sd     = 100 * sd(f$S_dem),
    lo     = 100 * ci[["lower"]], hi = 100 * ci[["upper"]],
    err    = 100 * (mean(f$S_dem) - T_dem),
    covers = T_dem >= ci[["lower"]] && T_dem <= ci[["upper"]],
    est_2p = 100 * mean(f$S_2p),
    lo_2p  = 100 * ci2p[["lower"]], hi_2p = 100 * ci2p[["upper"]],
    err_2p = 100 * (mean(f$S_2p) - T_2p),
    cov_2p = T_2p >= ci2p[["lower"]] && T_2p <= ci2p[["upper"]],
    turn   = mean(f$turn) / T_turn * 100
  )
}


source("scripts/model/plot_night.R")

res <- map_dfr(STAGES, run_stage)

cli_h2("Dem share of ALL votes -- truth {round(100*T_dem, 3)}%")
res |>
  select(stage, cty_rep, cells, pct_votes, naive_2p, est, sd, lo, hi, err, covers) |>
  mutate(width = hi - lo,
         across(c(pct_votes, naive_2p, est, sd, lo, hi, err, width), ~ round(.x, 2))) |>
  as.data.frame() |> print(row.names = FALSE)

cli_h2("Two-party Dem share -- truth {round(100*T_2p, 3)}%")
res |>
  select(stage, pct_votes, naive_2p, est_2p, err_2p, cov_2p, turn) |>
  mutate(naive_err = naive_2p - 100 * T_2p,
         across(c(pct_votes, naive_2p, naive_err, est_2p, err_2p, turn), ~ round(.x, 2))) |>
  as.data.frame() |> print(row.names = FALSE)

cli_inform(c(
  "ARM={ARM}  hist={arm_cfg$hist}  delta_m='{arm_cfg$off}'  \\
   SEED={Sys.getenv('SEED', unset='20260303')}",
  "coverage (share of all votes): {sum(res$covers)}/{nrow(res)}",
  "coverage (two-party):          {sum(res$cov_2p)}/{nrow(res)}",
  "max |err|: {round(max(abs(res$err)), 2)}pp   \\
   mean |err|: {round(mean(abs(res$err)), 2)}pp",
  "naive max |err| (two-party): \\
   {round(max(abs(res$naive_2p - 100*T_2p), na.rm=TRUE), 2)}pp"
))

if (Sys.getenv("PLOT", unset = "1") != "0") plot_night(res)

out <- Sys.getenv("OUT_RES", unset = "")
if (nzchar(out)) {
  write_parquet(mutate(res, arm = ARM, hist = arm_cfg$hist,
                       seed = Sys.getenv("SEED", unset = "20260303")), out)
  cli_alert_success("wrote {out}")
}
