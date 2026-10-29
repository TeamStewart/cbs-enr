## Simulated election night, any state we model
##
## Draws a final canvass ("truth") and a reporting schedule from the model inputs in
## 26_general/input_data/model_inputs, and writes them to 26_sim/{scenario}/
## ({ST}_truth.parquet, {ST}_schedule.parquet, manifest.json), which
## live/R/returns.R read_sim() auto-detects. Snapshots at any time come from
## sim_snapshot() in scripts/sim/sim_snapshot.R.
##
## Usage (from the repo root):
##   Rscript scripts/sim/make_sim.R [--state=NC] [--contests=sen] [--counties=zero] [scenario] [seed]
## --state is optional; without it the state comes from the scenario name prefix
## (nc_base -> NC). make_sim_tx.R is the same thing with the state fixed to TX.
## --contests keeps only those sim_candidates.csv contests (comma-separated; default all).
## --counties=zero keeps only counties with a zero file in 26_general/{ST}/clean
## (sim_zero_counties()); the default keeps every county in the model frame.
## Checks: Rscript scripts/sim/check_sim.R [scenario]
##
## Per state (see sim_config.R):
##   inputs        {ST}_precinct.parquet, {ST}_race_prior.csv,
##                 {ST}_precinct_mode_history.parquet, from prep_model_inputs.R
##   CANDIDATES    sim_candidates.csv. The contests drawn are the ones listed there,
##                 and each needs a filled prior in {ST}_race_prior.csv.
##   MODES         lookups/mode_scope.csv -- the model's modes for that state, after prov
##                 folds into eday. CA is mail|eday, not mail|early|eday.
##   pres24_dem2   the TRUE STATEWIDE 2024 two-party Dem share: sim_state_params.csv if
##                 given there, else derived from county_baseline(state). NOT the
##                 modelled frame's own dem2: it is used as swing = qlogis(target) -
##                 qlogis(pres24_dem2), where `target` is a statewide prior, so both
##                 sides must be statewide. TX's modelled counties sit at 0.493 against a
##                 statewide 0.4306 -- using the frame value would bake a 6pp urban skew
##                 into the swing and the simulated truth would miss the prior by that much.
##   poll_close    sim_state_params.csv, in EASTERN time. NOT derived: poll close is a
##                 legal fact per state and in several states per county, and getting it
##                 wrong silently correlates reporting time with partisanship, which is
##                 exactly the bias these scenarios exist to detect. Unfilled rows abort.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(nanoparquet)
  library(cli)
})
source("scripts/sim/sim_snapshot.R")
source("scripts/sim/sim_config.R")

args_all = commandArgs(trailingOnly = TRUE)
opt = function(k, d = NA) {
  a = grep(paste0("^--", k, "="), args_all, value = TRUE)
  if (length(a)) sub("^[^=]*=", "", a[1]) else d
}
args = args_all[!grepl("^--", args_all)]
# make_sim_tx.R sets SIM_STATE before sourcing this file
STATE    = sim_state_from(if (exists("SIM_STATE")) SIM_STATE else opt("state"),
                          if (length(args) >= 1) args[1] else "")
SCENARIO = if (length(args) >= 1) args[1] else paste0(tolower(STATE), "_base")
SEED     = if (length(args) >= 2) as.integer(args[2]) else 2026L
if (!nzchar(STATE)) cli_abort("Give a scenario name (e.g. nc_base) or --state=NC.")

DIR_IN   = DIR_INPUTS
DIR_OUT  = file.path(DIR_SIMS, SCENARIO)
ELECTION = ymd(ELECTION_DATE)
in_file  = function(suffix) file.path(DIR_IN, paste0(STATE, suffix))

SP = sim_state_params(STATE)
CANDIDATES = sim_candidates(STATE)
CONTESTS = opt("contests")
if (!is.na(CONTESTS)) {
  CONTESTS = trimws(strsplit(CONTESTS, ",")[[1]])
  if (length(bad <- setdiff(CONTESTS, CANDIDATES$contest)))
    cli_abort("--contests {.val {bad}} not in sim_candidates.csv for {STATE}.")
  CANDIDATES = filter(CANDIDATES, contest %in% CONTESTS)
}
COUNTIES = opt("counties", "all")
if (!COUNTIES %in% c("all", "zero")) cli_abort("--counties must be all or zero, not {COUNTIES}.")

# the model's modes for this state
ms = read.csv("scripts/model/lookups/mode_scope.csv", colClasses = "character")
ms = ms[ms$state == STATE, , drop = FALSE]
if (!nrow(ms))
  cli_abort(c("No {.path lookups/mode_scope.csv} row for {STATE}.",
              "i" = "Add one -- the sim must draw the same modes the model fits."))
MODES    = trimws(strsplit(ms$modes[1], "|", fixed = TRUE)[[1]])

# statewide 2024 two-party share, over EVERY county (see header)
PRES24 = SP$pres24_dem2 %||% local({
  source("scripts/model/live/R/counties.R")
  base = county_baseline(STATE, DIR_26)
  sum(base$pres24_dem) / sum(base$pres24_dem + base$pres24_rep)
})
if (!is.finite(PRES24) || PRES24 <= 0 || PRES24 >= 1)
  cli_abort("pres24_dem2 for {STATE} is {PRES24}, which is not a share.")

cli_h1("make_sim.R: {STATE} / {SCENARIO}")
cli_inform(c("*" = "contests: {paste(unique(CANDIDATES$contest), collapse = ', ')}; modes: {paste(MODES, collapse = ', ')}",
             "*" = "statewide 2024 two-party Dem: {round(PRES24, 4)}",
             "*" = "poll close (ET): {SP$poll_close}{if (length(SP$poll_close_late)) paste0('; late: ', paste(names(SP$poll_close_late), SP$poll_close_late, sep = '=', collapse = ', ')) else ''}"))

# ========================================
## Parameters (logit-scale SDs unless noted)
# ========================================
P = list(
  # The prior's two-party share vs. the 2024 presidential result that dem2 is built
  # from sets a uniform logit swing on dem2, plus a statewide miss drawn per race.
  # STATEWIDE share (see header), from sim_state_params.csv or county_baseline().
  pres24_dem2       = PRES24,
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
    z = c(his = 0, bla = 0, col = 0, age = 0)
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
  rolloff           = c(gov = 1, sen = 0.99),   # contests not listed: 1
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
  # timing (Eastern), from sim_state_params.csv
  poll_close        = SP$poll_close,
  # POLL_CLOSE_LATE=0 drops the late close. Diagnostic: in TX, EL PASO is Mountain time
  # AND heavily Hispanic/Democratic, so a late close correlates reporting TIME with
  # PARTISANSHIP. Any mechanism that weights precincts by how early they finish will
  # inherit that correlation as a share bias.
  poll_close_late   = if (Sys.getenv("POLL_CLOSE_LATE", unset = "") == "0") c()
                      else SP$poll_close_late,
  zero_post         = "19:00",  # zero files posted before close
  cycles            = c(15, 30),  # county posting cycle, minutes
  speed_sd          = 0.40,   # county multiplier on election-night delays
  speed_override    = c(),    # e.g. c(HARRIS = 2) for a slow county
  early_mins        = 25,     # mean delay of the early + first-mail dump
  mail_first        = c(0.75, 0.95),  # range of the share of mail in that dump
  eday_hours        = 2.5,    # mean delay of election-day precinct posts (gamma, shape 2)
  late_mail_days    = c(1, 6)
)

WRITEIN_CONTESTS = CANDIDATES$contest[CANDIDATES$role == "writein"]

set.seed(SEED)

# named-vector lookup with a default for missing names (and for a NULL vector)
lookup = function(x, key, default) {
  v = if (is.null(x)) rep(NA, length(key)) else unname(x[key])
  if_else(is.na(v), default, v)
}

# ========================================
## 1. Inputs
# ========================================
COLS_PREC = c("geo", "county_name", "county_cbs", "n", "est_turnout", "dem2", "oth_base",
              "yg_pred_his", "yg_pred_bla", "p_col", "p_age_65_up")
prec = read_pq(in_file("_precinct.parquet"))
if (length(miss <- setdiff(COLS_PREC, names(prec))))
  cli_abort("{STATE}_precinct.parquet is missing {.val {miss}}.")
prec = select(prec, all_of(COLS_PREC))
if (COUNTIES == "zero") {
  zc = sim_zero_counties(STATE)
  cli_inform(c("*" = "--counties=zero: {sum(unique(prec$county_cbs) %in% zc)} of {n_distinct(prec$county_cbs)} model counties have a zero file"))
  prec = filter(prec, county_cbs %in% zc)
  if (!nrow(prec)) cli_abort("No model precincts in {STATE}'s zero-file counties.")
}
# these feed every precinct's votes: one bad value would be NA or Inf in the truth
bad = with(prec, is.na(n) | is.na(est_turnout) | !(dem2 > 0 & dem2 < 1) | !(oth_base > 0 & oth_base < 1))
if (any(bad))
  cli_abort(c("{sum(bad)} precinct{?s} in {STATE}_precinct.parquet have NA n/est_turnout or a \
               dem2/oth_base outside (0, 1).",
              "i" = "First few: {.val {head(prec$geo[bad], 5)}}. Fix the prep, not the sim."))

# the contests drawn are the ones sim_candidates.csv lists; each needs a filled prior
prior = read.csv(in_file("_race_prior.csv")) |>
  transmute(contest = sub(paste0("^", STATE, "_"), "", race_id), target = prior_dem / (prior_dem + prior_rep))
prior = left_join(distinct(CANDIDATES, contest), prior, by = "contest")
if (any(no_prior <- is.na(prior$target)))
  cli_abort(c("No prior_dem / prior_rep for {STATE} {.val {prior$contest[no_prior]}}.",
              "i" = "Fill 26_general/input_data/priors.csv and rerun prep_model_inputs.R {STATE}."))

hist = read_pq(in_file("_precinct_mode_history.parquet")) |>
  filter(election == P$mode_election) |>
  left_join(select(prec, geo, county_cbs), by = "geo")
# a state whose feed reports no vote method (mode_scope.csv modes = all; MN) gets one
# "all" cell per precinct: the history collapses to it, and it has no per-mode lean
if (identical(MODES, "all"))
  hist = summarize(hist, voters = sum(voters), .by = c(geo, election, county_cbs)) |> mutate(method = "all")

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
# counties are CBS reporting units, keyed by county_cbs. In AK those are the 40 house
# districts, which cut across boroughs, so (county_name, county_cbs) is many-to-many;
# a district takes its first borough's name for the lookups below. 1:1 elsewhere.
cty = distinct(prec, county_name, county_cbs) |>
  arrange(county_cbs) |>
  distinct(county_cbs, .keep_all = TRUE) |>
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
centre = function(x, w) coalesce(x - weighted.mean(x, w, na.rm = TRUE), 0)
geos = prec |>
  left_join(select(cty, -county_name), by = "county_cbs") |>
  mutate(
    lz       = qlogis(dem2) - lz_mean,
    lean     = qlogis(dem2) + swing + rnorm(n(), 0, P$precinct_sd),
    # structured demographic tilt, centred on the est_turnout-weighted mean so it
    # redistributes turnout rather than changing its level. A precinct missing a
    # covariate sits at the mean (no tilt): one NA yg_pred_his used to make every
    # precinct's demo_lp NA, and with it every vote in the truth.
    demo_lp  = P$turnout_demo[["his"]] * centre(yg_pred_his, est_turnout) +
               P$turnout_demo[["bla"]] * centre(yg_pred_bla, est_turnout) +
               P$turnout_demo[["col"]] * centre(p_col,       est_turnout) +
               P$turnout_demo[["age"]] * centre(p_age_65_up, est_turnout),
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
    delta = coalesce(a, 0) + coalesce(b, 0) * lz,
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
    total   = ballots_m * lookup(P$rolloff, contest, 1),
    p_dem   = plogis(x + delta),
    dem     = total * (1 - tp) * p_dem,
    rep     = total * (1 - tp) * (1 - p_dem),
    # a contest with no write-in row in sim_candidates.csv gives all of tp to minor
    wf      = if_else(contest %in% WRITEIN_CONTESTS, P$writein_frac, 0),
    minor   = total * tp * (1 - wf),
    writein = total * tp * wf
  ) |>
  select(contest, geo, method, dem, rep, minor, writein) |>
  pivot_longer(c(dem, rep, minor, writein), names_to = "role", values_to = "votes") |>
  left_join(CANDIDATES, by = c("contest", "role")) |>
  filter(!is.na(candidate)) |>
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

# checked against every county, not just the modelled ones: a late-close county outside
# the frame is fine, a misspelt one silently closes on time
late_unknown = setdiff(names(P$poll_close_late), read.csv(in_file("_counties.csv"))$county_name)
if (length(late_unknown))
  cli_abort(c("poll_close_late names {.val {late_unknown}}, not a county_name in {STATE}_counties.csv.",
              "i" = "Fix the spelling in {.path scripts/sim/sim_state_params.csv}."))
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
  transmute(geo_sched, geo, county_cbs, dark, method = "eday",  t = eday, cum_frac = 1),
  # an "all" precinct posts once, everything in, when its election-day count would
  transmute(geo_sched, geo, county_cbs, dark, method = "all",   t = eday, cum_frac = 1)
) |>
  filter(method %in% MODES) |>
  mutate(t = if_else(dark, as.POSIXct(NA, tz = SIM_TZ), t)) |>
  select(-dark) |>
  arrange(county_cbs, geo, method, t)

# ========================================
## 4b. Site-level reporting
# ========================================
# Where a county reports a method by site or county bucket instead of by precinct (NC's
# one-stop early sites), the feed carries that method's votes on bucket geos,
# {st}_{county_cbs}_{label}, labelled as lookups/reporting_levels.csv lists them. Each
# precinct's votes go to one of its county's sites at random; the precinct's own row
# stays, at 0, as in the zero files. Sites post with their county's batch for the method.
# Drawn after everything above, so a state with no site_methods is unchanged.
# A "bucket" label that is really one of the county's precincts is dropped, and a county
# left with none reports by precinct: Henderson NC's only early label, `EV`, is a precinct
# (the false positive vote_modes.R notes; 2024 has it at precinct level too).
SITES = sim_site_buckets(STATE, intersect(SP$site_methods, MODES)) |>
  filter(!paste0(tolower(STATE), "_", county_cbs, "_", label) %in% prec$geo)
if (nrow(SITES)) {
  site_of = select(geos, geo, county_cbs) |>
    inner_join(SITES, by = "county_cbs", relationship = "many-to-many") |>
    summarize(site = paste0(tolower(STATE), "_", county_cbs[1], "_", label[sample.int(n(), 1)]),
              .by = c(geo, method))
  site_truth = inner_join(truth, site_of, by = c("geo", "method")) |>
    summarize(votes = sum(votes), .by = c(state, election, county_cbs, site, contest, candidate, party, method)) |>
    left_join(select(cty, county_cbs, county_name), by = "county_cbs") |>
    transmute(state, election, county_name, county_cbs, geo = site, contest, candidate, party,
              method, votes, censored = FALSE)
  truth = truth |>
    left_join(transmute(site_of, geo, method, moved = TRUE), by = c("geo", "method")) |>
    mutate(votes = if_else(coalesce(moved, FALSE), 0, votes)) |>
    select(-moved) |>
    bind_rows(site_truth) |>
    arrange(county_cbs, geo, contest, candidate, method)
  site_sched = schedule |>
    inner_join(site_of, by = c("geo", "method")) |>
    distinct(geo = site, county_cbs, method, t, cum_frac)
  schedule = bind_rows(schedule, site_sched) |>
    arrange(county_cbs, geo, method, t)
}

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
  counties_scope = COUNTIES,
  mode_shift = as.list(P$mode_shift),
  turnout_demo = as.list(P$turnout_demo),
  seed      = SEED,
  created   = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  git       = system("git rev-parse --short HEAD", intern = TRUE),
  state     = STATE,
  inputs    = list.files(DIR_IN, pattern = paste0("^", STATE, "_")),
  files     = c(truth = paste0(STATE, "_truth.parquet"), schedule = paste0(STATE, "_schedule.parquet")),
  usage     = "source('scripts/sim/sim_snapshot.R'); sim_snapshot(truth, schedule, '2026-11-03 22:00')",
  params    = P,
  races     = realized,
  dark_counties = cty$county_name[cty$dark],
  censored_precincts = sum(geos$censored),
  site_methods = SP$site_methods,
  site_geos = if (nrow(SITES)) count(distinct(site_of, county_cbs = substr(site, 4, 6), method), method) else NULL,
  counties  = select(cty_sched, county_name, county_cbs, speed, cycle, censor_rate, dark,
                     close, dump, late) |>
    mutate(across(c(close, dump, late), \(x) format(x, "%Y-%m-%d %H:%M %Z")))
)
jsonlite::write_json(manifest, file.path(DIR_OUT, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

cli_alert_success("Wrote {.path {DIR_OUT}}: {nrow(truth)} truth rows, {nrow(schedule)} schedule rows")
cli_text("Dark: {.val {manifest$dark_counties}}; censored precincts: {manifest$censored_precincts} of {nrow(geos)}")
print(realized)
