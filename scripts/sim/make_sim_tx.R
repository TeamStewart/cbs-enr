## Simulated election night: TX governor + senate
##
## Draws a final canvass ("truth") and a reporting schedule from the model inputs in
## 26_general/input_data/model_inputs, and writes them to 26_sim/{scenario}/.
## Snapshots at any time come from sim_snapshot() in scripts/sim/sim_snapshot.R.
##
## Usage (from the repo root):
##   Rscript scripts/sim/make_sim_tx.R [scenario] [seed]
## Checks: Rscript scripts/sim/check_sim.R [scenario]

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(nanoparquet)
  library(cli)
})
source("scripts/sim/sim_snapshot.R")

### Setting knobs
SCENARIO = if (length(args) >= 1) args[1] else "turnout_partisan_R0.1"
PATH_DROPBOX = Sys.getenv("PATH_DROPBOX", unset = "/Users/dskinnion/MIT Dropbox/Dominic Skinnion/2024 Election Results")
Sys.setenv(TURNOUT_DEMO = "lean=-0.1")


args = commandArgs(trailingOnly = TRUE)
# SCENARIO = if (length(args) >= 1) args[1] else "tx_base"
SEED     = if (length(args) >= 2) as.integer(args[2]) else 2026L

# PATH_DROPBOX = Sys.getenv("PATH_DROPBOX", unset = "~/Library/CloudStorage/Dropbox/2024 Election Results")
DIR_IN   = file.path(PATH_DROPBOX, "26_general", "input_data", "model_inputs")
DIR_OUT  = file.path(Sys.getenv("SIM_OUT", unset = file.path(PATH_DROPBOX, "26_sim")), SCENARIO)
STATE    = "TX"
ELECTION = ymd("2026-11-03")
MODES    = c("mail", "early", "eday")


# ========================================
## Parameters (logit-scale SDs unless noted)
# ========================================
P = list(
  # The prior's two-party share vs. the 2024 presidential result that dem2 is built
  # from sets a uniform logit swing on dem2, plus a statewide miss drawn per race.
  pres24_dem2       = 42.46 / (42.46 + 56.14),  # TX 2024 Harris two-party share
  # MISS_SD=0 removes the per-race statewide partisan miss, so the model's share prior
  # (dem2, swung to the target) is correct by construction. Diagnostic use: if a share
  # bias introduced by the turnout machinery vanishes here, that machinery is AMPLIFYING
  # an existing prior error rather than creating one -- bounded by prior quality, and a
  # very different problem from a bias it manufactures on its own.
  miss_sd           = as.numeric(Sys.getenv("MISS_SD", unset = "0.10")),
  county_swing_sd   = 0.10,
  precinct_sd       = 0.08,
  race_precinct_sd  = 0.05,   # ticket splitting between gov and sen
  # turnout: est_turnout x county x precinct noise (log scale), capped at cap x n,
  # the model's KAPPA ceiling
  # TURNOUT_RATIO scales the whole turnout level away from est_turnout. At the default
  # of 1 the simulated truth IS the prior (times exp(sigma^2/2) ~ 1.008), so a model that
  # passively echoes est_turnout scores near-perfectly and the sim cannot distinguish it
  # from one that actually learns turnout. The real TX 2024 backtest shows turnout
  # under-predicted early and over-predicted late, which a correct-by-construction prior
  # can never reproduce. Set it away from 1 to make the night a real test.
  turnout_ratio     = as.numeric(Sys.getenv("TURNOUT_RATIO", unset = "1")),
  # TURNOUT_DEMO makes the turnout error STRUCTURED rather than uniform: turnout is
  # multiplied by exp(sum_k coef_k * (x_k - weighted mean)), then rescaled so the
  # statewide total is preserved. So est_turnout stays right ON AVERAGE and is wrong
  # per-precinct in a way that tracks demographics.
  # This is the discriminating test between the two model families. A county x method
  # fixed effect (the uniform swing model) can absorb a uniform miss and county-level
  # structure, but NOT within-county demographic structure. The precinct x mode model
  # carries pred.his, pred.bla, age and col in COVS_TURN, so it should. TURNOUT_RATIO
  # is the case that flatters the simple model; this is the case that should not.
  # Env: TURNOUT_DEMO="his=-0.4,col=0.2" (log scale per unit share, omitted = 0).
  turnout_demo      = {
    z = c(his = 0, bla = 0, col = 0, age = 0, lean = 0)
    spec = Sys.getenv("TURNOUT_DEMO", unset = "")
    if (nzchar(spec)) for (kv in strsplit(spec, ",")[[1]]) {
      p2 = strsplit(trimws(kv), "=")[[1]]
      if (length(p2) != 2 || !p2[1] %in% names(z)) stop("bad TURNOUT_DEMO entry: ", kv)
      z[p2[1]] = as.numeric(p2[2])
    }
    z
  },
  county_turnout_sd = 0.08,
  precinct_turnout_sd = 0.10,
  turnout_cap       = 1.25,
  rolloff           = c(gov = 1, sen = 0.99),
  # method split: this election's mode history, with county log-noise
  mode_election     = "2022_general",
  mode_share_sd     = 0.15,
  # MODE_SHIFT tilts the mode COMPOSITION without touching the precinct TOTAL: shares
  # are multiplied by exp(shift_m) and then renormalised per precinct, so `ballots` is
  # unchanged by construction. This is the scenario TURNOUT_RATIO cannot produce -- a
  # 2020->2024-style change in HOW people vote with no change in HOW MANY. It is the
  # case that should break any model inferring the total by grossing a completed mode
  # up through a stale composition prior, and the case a fitted composition should
  # survive. Env: MODE_SHIFT="early=0.25,eday=-0.5" (log scale, omitted modes = 0).
  mode_shift        = {
    z = setNames(rep(0, length(MODES)), MODES)
    spec = Sys.getenv("MODE_SHIFT", unset = "")
    if (nzchar(spec)) for (kv in strsplit(spec, ",")[[1]]) {
      p2 = strsplit(trimws(kv), "=")[[1]]
      if (length(p2) != 2 || !p2[1] %in% MODES) stop("bad MODE_SHIFT entry: ", kv)
      z[p2[1]] = as.numeric(p2[2])
    }
    z
  },
  # Dem lean by method, delta = a + b * (logit(dem2) - mean). NC 2024 values from
  # CONTRACT.md section 7. Set a and b to 0 for no method lean.
  # MODE_LEAN=0 zeroes the per-mode partisan differential, so modes differ only in SIZE
  # and not in how they vote. Diagnostic use: if a share bias that appears under a
  # composition shift vanishes when the leans are zeroed, the bias is coming from the
  # mode x partisanship coupling rather than from the turnout machinery itself.
  mode_lean = {
    .z = Sys.getenv("MODE_LEAN", unset = "") == "0"
    tibble(method = c("mail", "early", "eday"),
           a = if (.z) c(0, 0, 0) else c(0.660, -0.029, -0.025),
           b = if (.z) c(0, 0, 0) else c(-0.297, 0.041, -0.089))
  },
  # third party: oth_base with noise, split between the minor candidate and write-ins
  oth_sd            = 0.20,
  writein_frac      = 0.15,
  # censoring: precinct ids the crosswalk won't match, Beta drop rate per county
  censor_mean       = 0.10,
  censor_conc       = 20,
  n_dark            = 1,      # counties that never report
  # timing (Eastern)
  poll_close        = "20:00",
  # POLL_CLOSE_LATE=0 drops the late close. Diagnostic: EL PASO is Mountain time AND
  # heavily Hispanic/Democratic, so a late close correlates reporting TIME with
  # PARTISANSHIP. Any mechanism that weights precincts by how early they finish will
  # inherit that correlation as a share bias.
  poll_close_late   = if (Sys.getenv("POLL_CLOSE_LATE", unset = "") == "0") c()
                      else c(`EL PASO` = "21:00"),  # Mountain time
  zero_post         = "19:00",  # zero files posted before close
  cycles            = c(15, 30),  # county posting cycle, minutes
  speed_sd          = 0.40,   # county multiplier on election-night delays
  speed_override    = c(),    # e.g. c(HARRIS = 2) for a slow county
  early_mins        = 25,     # mean delay of the early + first-mail dump
  mail_first        = c(0.75, 0.95),  # range of the share of mail in that dump
  eday_hours        = 2.5,    # mean delay of election-day precinct posts (gamma, shape 2)
  late_mail_days    = c(1, 6)
)

CANDIDATES = tribble(
  ~contest, ~role,     ~candidate,         ~party,
  "gov",    "dem",     "hinojosa, gina",   "dem",
  "gov",    "rep",     "abbott, greg",     "rep",
  "gov",    "minor",   "dixon, pat",       "oth",
  "gov",    "writein", "write-in",         "oth",
  "sen",    "dem",     "talarico, james",  "dem",
  "sen",    "rep",     "paxton, ken",      "rep",
  "sen",    "minor",   "brown, ted",       "oth",
  "sen",    "writein", "write-in",         "oth"
)

set.seed(SEED)

# named-vector lookup with a default for missing names (and for a NULL vector)
lookup = function(x, key, default) {
  v = if (is.null(x)) rep(NA, length(key)) else unname(x[key])
  if_else(is.na(v), default, v)
}

# ========================================
## 1. Inputs
# ========================================
prec = read_parquet(file.path(DIR_IN, "TX_precinct.parquet")) |>
  select(geo, county_name, county_cbs, n, est_turnout, dem2, oth_base,
         yg_pred_his, yg_pred_bla, p_col, p_age_65_up)

prior = read.csv(file.path(DIR_IN, "TX_race_prior.csv")) |>
  transmute(contest = sub("^TX_", "", race_id), target = prior_dem / (prior_dem + prior_rep))

hist = read_parquet(file.path(DIR_IN, "TX_precinct_mode_history.parquet")) |>
  filter(election == P$mode_election) |>
  left_join(select(prec, geo, county_cbs), by = "geo")

# precinct method shares; precincts with no history (or no voters) take the county's
geo_share = hist |>
  mutate(share = voters / sum(voters), .by = geo) |>
  filter(!is.nan(share)) |>
  select(geo, method, share)
cty_share = hist |>
  summarize(voters = sum(voters), .by = c(county_cbs, method)) |>
  mutate(cshare = voters / sum(voters), .by = county_cbs) |>
  select(county_cbs, method, cshare)
state_share = hist |>
  summarize(voters = sum(voters), .by = method) |>
  transmute(method, sshare = voters / sum(voters))

# ========================================
## 2. County and precinct draws
# ========================================
cty = distinct(prec, county_name, county_cbs) |>
  arrange(county_cbs) |>
  mutate(
    swing       = rnorm(n(), 0, P$county_swing_sd),
    turn        = rnorm(n(), 0, P$county_turnout_sd),
    censor_rate = rbeta(n(), P$censor_mean * P$censor_conc, (1 - P$censor_mean) * P$censor_conc),
    dark        = row_number() %in% sample.int(n(), P$n_dark),
    speed       = exp(rnorm(n(), 0, P$speed_sd)),
    speed       = speed * lookup(P$speed_override, county_name, 1),
    cycle       = sample(P$cycles, n(), replace = TRUE)
  )

lz_mean = weighted.mean(qlogis(prec$dem2), prec$est_turnout)
geos = prec |>
  left_join(cty, by = c("county_name", "county_cbs")) |>
  mutate(
    lz       = qlogis(dem2) - lz_mean,
    lean     = qlogis(dem2) + swing + rnorm(n(), 0, P$precinct_sd),
    # structured demographic tilt, centred on the est_turnout-weighted mean so it
    # redistributes turnout rather than changing its level
    demo_lp  = P$turnout_demo[["his"]] * (yg_pred_his - weighted.mean(yg_pred_his, est_turnout)) +
               P$turnout_demo[["bla"]] * (yg_pred_bla - weighted.mean(yg_pred_bla, est_turnout)) +
               P$turnout_demo[["col"]] * (p_col       - weighted.mean(p_col,       est_turnout)) +
               P$turnout_demo[["age"]] * (p_age_65_up - weighted.mean(p_age_65_up, est_turnout)) +
               P$turnout_demo[["lean"]] * (lz - weighted.mean(lz, est_turnout)),
    demo_m   = exp(demo_lp) / weighted.mean(exp(demo_lp), est_turnout),   # preserves the total
    ballots  = pmin(est_turnout * P$turnout_ratio * demo_m *
                      exp(turn + rnorm(n(), 0, P$precinct_turnout_sd)),
                    P$turnout_cap * n),
    censored = runif(n()) < censor_rate
  )

# method cells: county noise on the shares, then a Dem lean by method, centered
# within the precinct so it moves votes between methods, not the precinct's lean
cty_mode_noise = expand_grid(county_cbs = cty$county_cbs, method = MODES) |>
  mutate(eps = rnorm(n(), 0, P$mode_share_sd))
cells = expand_grid(geo = geos$geo, method = MODES) |>
  left_join(select(geos, geo, county_cbs, lz, ballots), by = "geo") |>
  left_join(geo_share, by = c("geo", "method")) |>
  left_join(cty_share, by = c("county_cbs", "method")) |>
  left_join(state_share, by = "method") |>
  left_join(cty_mode_noise, by = c("county_cbs", "method")) |>
  left_join(P$mode_lean, by = "method") |>
  mutate(
    share = coalesce(share, cshare, sshare) * exp(eps + P$mode_shift[method]),
    share = share / sum(share),
    delta = a + b * lz,
    delta = delta - sum(share * delta),
    ballots_m = ballots * share,
    .by = geo
  ) |>
  select(geo, method, ballots_m, delta)

# ========================================
## 3. Final votes
# ========================================
races = prior |>
  mutate(swing = qlogis(target) - qlogis(P$pres24_dem2) + rnorm(n(), 0, P$miss_sd))

race_geo = cross_join(select(races, contest, rswing = swing), select(geos, geo, lean, oth_base)) |>
  mutate(
    x  = lean + rswing + rnorm(n(), 0, P$race_precinct_sd),
    tp = plogis(qlogis(oth_base) + rnorm(n(), 0, P$oth_sd))
  )

truth = race_geo |>
  inner_join(cells, by = "geo", relationship = "many-to-many") |>
  mutate(
    total   = ballots_m * P$rolloff[contest],
    p_dem   = plogis(x + delta),
    dem     = total * (1 - tp) * p_dem,
    rep     = total * (1 - tp) * (1 - p_dem),
    minor   = total * tp * (1 - P$writein_frac),
    writein = total * tp * P$writein_frac
  ) |>
  select(contest, geo, method, dem, rep, minor, writein) |>
  pivot_longer(c(dem, rep, minor, writein), names_to = "role", values_to = "votes") |>
  left_join(CANDIDATES, by = c("contest", "role")) |>
  left_join(select(geos, geo, county_name, county_cbs, censored), by = "geo") |>
  transmute(
    state = STATE, election = ELECTION, county_name, county_cbs, geo,
    contest, candidate, party, method, votes = round(votes), censored
  ) |>
  arrange(county_cbs, geo, contest, candidate, method)

# ========================================
## 4. Reporting schedule
# ========================================
# Each county posts on its own grid (every 15 or 30 minutes from poll close). The
# early vote and most mail land in one dump after close; election-day precincts post
# one by one; late mail lands days later. Dark counties never post.
round_up = function(t, origin, mins) origin + ceiling(as.numeric(difftime(t, origin, units = "mins")) / mins) * mins * 60

cty_sched = cty |>
  mutate(
    close = sim_time(lookup(P$poll_close_late, county_name, P$poll_close), ELECTION),
    dump  = round_up(close + speed * rexp(n(), 1 / P$early_mins) * 60, close, cycle),
    mail_frac = runif(n(), P$mail_first[1], P$mail_first[2]),
    late  = round_up(close + runif(n(), P$late_mail_days[1], P$late_mail_days[2]) * 86400, close, cycle)
  )

geo_sched = geos |>
  select(geo, county_cbs) |>
  left_join(cty_sched, by = "county_cbs") |>
  mutate(
    eday = close + speed * rgamma(n(), shape = 2, rate = 2 / P$eday_hours) * 3600,
    eday = round_up(pmax(eday, dump), close, cycle)
  )

schedule = bind_rows(
  transmute(geo_sched, geo, county_cbs, dark, method = "early", t = dump, cum_frac = 1),
  transmute(geo_sched, geo, county_cbs, dark, method = "mail",  t = dump, cum_frac = mail_frac),
  transmute(geo_sched, geo, county_cbs, dark, method = "mail",  t = late, cum_frac = 1),
  transmute(geo_sched, geo, county_cbs, dark, method = "eday",  t = eday, cum_frac = 1)
) |>
  mutate(t = if_else(dark, as.POSIXct(NA, tz = SIM_TZ), t)) |>
  select(-dark) |>
  arrange(county_cbs, geo, method, t)

# ========================================
## 5. Write
# ========================================
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)
write_parquet(truth, file.path(DIR_OUT, paste0(STATE, "_truth.parquet")))
write_parquet(schedule, file.path(DIR_OUT, paste0(STATE, "_schedule.parquet")))

realized = truth |>
  summarize(votes = sum(votes), .by = c(contest, party)) |>
  pivot_wider(names_from = party, values_from = votes) |>
  mutate(dem2 = dem / (dem + rep), oth = oth / (dem + rep + oth)) |>
  left_join(select(races, contest, target, swing), by = "contest")

manifest = list(
  scenario  = SCENARIO,
  mode_shift = as.list(P$mode_shift),
  turnout_demo = as.list(P$turnout_demo),
  seed      = SEED,
  created   = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  git       = system("git rev-parse --short HEAD", intern = TRUE),
  inputs    = list.files(DIR_IN, pattern = "^TX_"),
  files     = c(truth = paste0(STATE, "_truth.parquet"), schedule = paste0(STATE, "_schedule.parquet")),
  usage     = "source('scripts/sim/sim_snapshot.R'); sim_snapshot(truth, schedule, '2026-11-03 22:00')",
  params    = P,
  races     = realized,
  dark_counties = cty$county_name[cty$dark],
  censored_precincts = sum(geos$censored),
  counties  = select(cty_sched, county_name, county_cbs, speed, cycle, censor_rate, dark,
                     close, dump, late) |>
    mutate(across(c(close, dump, late), \(x) format(x, "%Y-%m-%d %H:%M %Z")))
)
jsonlite::write_json(manifest, file.path(DIR_OUT, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

cli_alert_success("Wrote {.path {DIR_OUT}}: {nrow(truth)} truth rows, {nrow(schedule)} schedule rows")
cli_text("Dark: {.val {manifest$dark_counties}}; censored precincts: {manifest$censored_precincts} of {nrow(geos)}")
print(realized)
