# ========================================
## model_turnout.R -- the medsl_sur engine: precinct x vote-method model of turnout and
## vote share. Loaded once per state by live/R/model_medsl.R (medsl_setup()), which
## supplies STATE, covs_long and mode_hist and then sources this file, model_core.R and
## model_hybrid.R into one environment. Nothing here reads a file of its own except
## lookups/mode_scope.csv.
##
## Three-equation (SUR-style) model per precinct x mode:
##     log(tau_im)                 ~ X_i    tau = ALL votes in the cell
##     logit(tp_im)                ~ X_i    tp  = third-party share OF ALL votes
##     logit(s_im)                 ~ X_i    s   = Dem share of the TWO-PARTY vote
## so v_dem = tau(1-tp)s, v_rep = tau(1-tp)(1-s), v_oth = tau*tp -- a bijection, so the
## three shares of ALL votes sum to 1 in [0,1] by construction. HEADLINE OUTPUT is the
## share of all votes; the two-party share is reported alongside since it decides the race.
## fit with a conjugate g-prior placed on FITTED VALUES (pseudo-row augmentation,
## via enightmodels::lm_bayes), and a joint 3M x 3M residual covariance Sigma that
## lets an observed mode inform the unobserved modes of the SAME precinct.
##
## Why this factorization rather than one model per candidate:
##   (tau, tp, s) is a bijection of (votes_dem, votes_rep, votes_oth), so nothing is
##   lost, but the Gaussian form keeps conjugacy (=> real posterior draws, fast refits),
##   keeps each quantity separately priored and interpretable, and keeps the three shares
##   of all votes summing to 1 in [0,1] WITHOUT clamping or renormalizing. Per-candidate
##   models plus post-hoc normalization instead deflate predictive variance (~29% of sd
##   at K=2) and induce the cross-candidate correlation rather than estimating it.
##
## Why Sigma rather than independent fits:
##   statewide share S = sum(s*tau)/sum(tau) is nonlinear in BOTH outcomes, so its
##   predictive variance depends on their correlation. Sigma also carries
##   share<->share correlation ACROSS modes, which is what lets early/mail returns
##   update the election-day prediction for that same precinct.
# ========================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(cli)
  library(nanoparquet)
  library(enightmodels) # lm_bayes, log_s, cred_int, conf_seq, logit_shift
})
# fitting primitives (fit_conj_re(), draw_beta_re(), ...)
source("scripts/model/live/medsl/model_core.R", local = TRUE)
# geo_county() (live/R/vote_modes.R, loaded by live/load.R) maps each precinct to the
# county code bucket_counts() keys on. CTY_BOUND must use that same rule: in AK the geo
# code and the borough in `cells$county` are different partitions.

set.seed(as.integer(Sys.getenv("SEED", unset = "20260303")))

# ---- CONFIG ---------------------------------------------------------------------
# STATE, covs_long and mode_hist come from the caller (live/R/model_medsl.R).
DRAWS = 500 # posterior draws
# Prior strength, in PRECINCT-EQUIVALENTS (not per-cell). Env-settable so the turnout
# equation's dependence on its prior can be ablated: the TURNOUT_RATIO scenarios show the
# model echoing est_turnout almost exactly early on, and this is the first thing to rule
# in or out as the cause.
PRIOR_N_TURN = as.numeric(Sys.getenv("PRIOR_N_TURN", unset = "3.0"))
PRIOR_N_SHARE = 3.0 # keep FIXED across the night; relative weight decays on its own
STAGE_DONE = 2L # a mode cell counts as observed at this reporting stage
# Support gate: the minimum reported cells, and reporting counties, before a MODE's
# returns are used at all. Below it the mode stays at its prior. See the gate in
# fit_stage() for the TX 2024 measurement that motivates it.
# Lowered from the 60/3 stopgap once the obs/obs_turn split landed: the turnout blow-up
# it existed to prevent is now handled properly, so the gate only catches degenerate
# cases rather than discarding legitimate share information.
# Below this many reported cells an equation is held at its prior LOCATION outright
# (see the prior-only branch in fit_stage's mu construction).
MIN_FIT_OBS = as.integer(Sys.getenv("MIN_FIT_OBS", unset = "30"))
MIN_MODE_CELLS = as.integer(Sys.getenv("MIN_MODE_CELLS", unset = "10"))
MIN_MODE_CTY = as.integer(Sys.getenv("MIN_MODE_CTY", unset = "1"))
# Fallback completeness test for TURNOUT: a cell's turnout counts as observed once
# counted >= COMPLETE_FRAC * prior_tau. Used whenever the feed's own `complete` flag is
# unusable (see complete_usable() below), which in production is always.
#
# MEASURED 2026-10-30, scripts/model/prep/measure_complete_frac.R, TX, per-cell median of
# prior_tau / truth -- and therefore of what a FINISHED cell reads as tau/prior_tau:
#
#                        prior/true        finished cell reads
#   2018->2022  early       1.07                 0.93        <- the 2026 analogue:
#   (midterm ->  mail       1.12                 0.90           a 2022 prior on a midterm
#    midterm)    eday       0.85                 1.18
#   2022->2024  early       0.85                 1.18        <- midterm -> presidential
#               mail        1.20                 0.83
#               eday        1.54                 0.65
#
# THE THRESHOLD IS REALLY A PER-MODE COUNTED-FRACTION GATE. A cell counted to fraction g
# passes iff g >= COMPLETE_FRAC * (prior/true). On the 2026 analogue at the old default
# of 1.1 that is 1.18 for early and 1.23 for mail: over 100%, i.e. a correctly FINISHED
# cell of those modes could never pass, at any point in the night. Two of three modes were
# structurally excluded, which is the real defect -- a threshold error, not a prior error.
# 0.8 is the largest value leaving every mode reachable on that analogue (0.86 / 0.89 /
# 0.68), and it is what the "strict settings never pin mail" note at obs_turn describes.
#
# SUPERSEDES an earlier comment here that read "off by ~1.7x for eday and ~0.5x for mail
# on TX 2024". The eday figure holds; the mail one is wrong in DIRECTION and does not
# reproduce. prior_tau / truth is (est_tot/T) * (p_m/q_m) -- ONE common level factor times
# a per-mode share ratio -- so for mail to read 0.5x while eday read 1.7x, p/q would have
# to differ between them by 3.4x. On TX it differs by 1.31x, and no level factor
# reconciles the pair. Mail and eday priors both run high; early runs the other way.
#
# WHAT THIS DOES NOT FIX: admitting a cell at counted fraction g enters it in the fit as
# if final, contributing log(g) -- a DOWNWARD bias, bounded by the threshold itself
# (worst admitted cell on the analogue sits at g = 0.68, so -0.39 log points). That is the
# opposite sign to the gross-up's 1/q, which fabricates votes without bound. The
# principled fix is right-censoring (USE_CENSOR; CONTRACT.md 8), which needs no threshold
# at all; this is the bounded stopgap.
COMPLETE_FRAC = as.numeric(Sys.getenv("COMPLETE_FRAC", unset = "0.8"))

#' Is a returns frame's `complete` column carrying any information?
#'
#' A column that is PRESENT but never TRUE is not evidence that nothing is counted out --
#' it is an absent signal wearing the shape of a present one, and taking it at face value
#' silently disables every component gated on `obs_turn`. That is the production state:
#' `complete = stage >= 6` (live/R/returns.R), `functions.R` rewrites a reported 6 to 5
#' unless `trust_stage = TRUE`, and `trust_stage` is FALSE for all 381 rows of
#' 26_general/metadata.csv. Measured 2026-10-30 across 171 clean files (1,202,773 rows):
#' the only stage values present are 0, 2 and 5 -- zero rows at 6.
#'
#' So the column is trusted only once it has fired at least once. Early in a real night
#' nothing is complete and this is FALSE, which is correct: the ratio test then admits
#' nothing either, because cells are barely counted. Set COMPLETE_TRUST=1 to force the
#' old behaviour (trust the column whenever it exists).
#'
#' @param returns Returns frame, with or without a `complete` column.
#' @return TRUE when `complete` exists and is TRUE for at least one row.
complete_usable = function(returns) {
  if (!"complete" %in% names(returns)) {
    return(FALSE)
  }
  if (Sys.getenv("COMPLETE_TRUST", unset = "0") == "1") {
    return(TRUE)
  }
  any(returns$complete, na.rm = TRUE)
}
# Wide prior on the unreported set's share offset, in logit units, shared within a
# (county, mode). Measured on TX 2024 at 0.26-0.45 raw / 0.15-0.30 vote-weighted; 0.25 is
# the starting point. SD_UNREP=0 disables it (ablation). See the block in fit_stage.
SD_UNREP = as.numeric(Sys.getenv("SD_UNREP", unset = "0.25"))
# COUNTY TURNOUT LOWER BOUND (CTY_BOUND=0 to ablate; inert unless fit_stage() is given
# `county_obs`).
# 31% of AK's and NC's vote never lands on a precinct: AK reports mail, early and
# provisional at house-district level for all 402 precincts (153,478 of 493,117 in the 2026
# primary file), NC reports mail at county level in 1,351 of 2,633 precincts and early at
# one-stop-site level in 1,105 (3,518,519 of 7,772,169 in 2024). `cell_counts()` joins FROM
# `cells`, so every one of those rows is dropped and the model's ONLY channel for that 31%
# is `mode_turn_share`, a 2022 L2 registration split that is never checked against tonight.
# CA, FL, MI and TX have ZERO bucket vote mass, so this is an AK + NC change by construction
# and must be bit-identical elsewhere.
#
# WHAT IT IS, and what it deliberately is not. A partial county x mode total is a LOWER
# BOUND on that county's final total, not an estimate of it -- turnout is censored, share is
# sampled, and this is the censored half. So it enters as a one-sided constraint on the
# draw, the same argument as T_draw = pmax(T_draw, cnt_tot), and it needs NO completeness
# gate: a partial total is a valid bound at 20:38 as much as at 23:49. That is the whole
# difference from model_hybrid.R's HY_COUNTY arm, which treats a county total as an EXACT
# constraint gated at CTY_MIN_PCT = 99 and is therefore inert through the entire broadcast
# window (NC's early vote is 1.1% in at 19:57, 53.0% at 21:02, 95.0% only at 22:20).
#
# HOW. Per county c, C_c = votes observed in c = (precinct-level counted, already in
# cnt_tot) + (bucket votes on no precinct, from live/R/returns.R::bucket_counts). If the
# draw's county total falls short, solve for one common logit shift delta_c with
#     sum_{i in c} kappa*n_i*invlogit(theta_id + delta_c) = C_c
# exactly as the statewide constraint does. AGGREGATE on purpose: allocating C_cm down to
# precincts by the prior mode weight would import precisely the prior error this exists to
# correct. The cost is that it cannot say WHICH precinct was short -- it spreads the deficit
# by a common shift, right on average and wrong per precinct.
#
# WHERE THE INCREMENT LANDS, stated because it is not where you would want it. The bound
# raises T_draw; `rem_tot = T_draw - cnt_tot` is then split across modes by `short`, which
# is pmax(T_draw * share - cnt_tau, 0) and so hands part of the increment to modes that are
# already counted OUT. That leak is deliberate and measured: on TX at 60% reporting, 1,865
# of 7,360 counted-out cells (25.3%) already carry positive `rem`, 25,833 votes = 0.86% of
# their counted total. With the completeness flag TRUTHFUL, zeroing `short` on mask_turn is
# perfect (total cell error 25,833 -> 0); with the flag LYING at 85% in, zeroing is 2.5x
# WORSE (180,333 -> 451,124), because the leak then covers 60% of the missing votes.
# Production lives in the lying regime through the broadcast window, so `short` is left
# alone. Consequence to keep in view: with per-mode partisan offsets switched on, the leak
# moves mass OUT of the still-counting bucket modes and INTO counted-out election day, which
# in NC (mail at +0.707 logit) is a Republican-leaning shift. That is the gate on the share
# half of this add-on, and it is not implemented here.
#
# WHAT IT COSTS, MEASURED 2026-10-26, and it is NOT free. A one-sided constraint on a noisy
# draw biases the mean UP: the increment is E[max(0, C_c - T_c)], so the bias is about
# 0.4 x the model's own per-county turnout error dispersion. Synthetic AK and NC nights
# (truth drawn from the composition, then the BUCKET modes' truth scaled by g so the prior
# is wrong by construction; precinct modes to precinct geos, county_site modes to bucket
# geos), statewide turnout as % of truth with the piles 90% / 100% in:
#   g      AK off      AK on          NC off      NC on
#   0.7    111.4       111.9 / 112.2    --          --
#   1.0    100.1       102.5 / 103.9   97.1       100.7 / 103.0
#   1.3     91.0        97.5 / 100.8   88.4        98.3 / 102.0
# So when the prior UNDERSTATES the bucket modes it removes 6-10pp of error, when the prior
# is RIGHT it adds 2-6pp, and when the prior OVERSTATES them it can do nothing (a lower
# bound cannot pull an estimate down) and adds ~0.5pp. Mean |error| over the three AK arms:
# 6.8 off, 5.6 on. It also never moves two-party share the wrong way (AK |err| 2.84 -> 2.76,
# 3.74 -> 3.53, 4.48 -> 4.11 pp). At 20% of the pile in, on vs off agree to 0.1pp -- the
# bound is correctly silent while the count is still well below the prediction.
# This cost is exactly the atom PIN_DONE exists to remove, one level up: the bound floors a
# county at its count without ever substituting it, so where it binds on nearly every draw
# the county total becomes an ATOM and the statewide turnout interval collapses (AK g=1.3,
# pile 100%: 95% width 0.66% of truth off, 0.25% on). The county-level analogue of PIN_DONE
# -- substitute, do not floor, once the county's piles are flagged `complete` -- would
# remove it, and `bucket_counts()` already carries that flag. Deliberately NOT done here:
# gating the BOUND on completeness is what makes HY_COUNTY inert, and the two must not be
# conflated. Raise it as its own change.
CTY_BOUND = Sys.getenv("CTY_BOUND", unset = "1") == "1"
# COUNTY-LEVEL ANALOGUE OF PIN_DONE (CTY_SUBSTITUTE=0 to ablate).
# The bound above is ONE-SIDED, and that is what costs it 2-6pp when the prior is already
# right: E[max(0, C_c - T_c)] > 0 even when E[T_c] = C_c, so flooring a noisy county total
# biases the mean up and puts an ATOM on the county. It is the same defect PIN_DONE exists
# to remove one level down -- "a precinct with every mode counted out has a KNOWN total:
# substitute it rather than flooring, or the draw contributes one-sided excess on a quantity
# with no uncertainty left" -- so the fix is the same: once a county's total is KNOWN,
# substitute it, in both directions, instead of flooring it.
#
# A county's total is known when (a) every pile it reports is flagged complete AND (b) every
# mode it reports AT PRECINCT level is counted out in every one of its precincts. (b) is
# checked on the modes that actually have precinct-level votes rather than on the mode
# vocabulary, because BUCKET_FOLD sends provisional piles into `eday` -- so in AK all three
# of MODES have a pile and a vocabulary test would vacuously pass while election day was
# still counting.
#
# (b) IS VACUOUS EARLY, and closing that is what the third condition is for. Before any
# precinct-level votes arrive there are no precinct-reported modes to check, so (a) alone
# would substitute a county down to its piles while election day had not started. So (c):
# the county must HAVE precinct-level votes. At 20:30 an AK county with its piles in and no
# election-day votes fails (c) and is floored, not substituted.
#
# A RATIO GATE IS DELIBERATELY *NOT* THE COMPLETENESS TEST, and the first draft of this got
# it wrong. Requiring the county's observed total to reach 0.95 of its expected total looks
# like the county-level analogue of PIN_MIN_RPRIOR, but it conflates "still counting" with
# "turnout came in below expectation" -- which are indistinguishable from a ratio and have
# opposite correct responses. Measured: with the bucket truth at 0.7x the prior, the ratio
# is 0.90, a 0.95 gate refuses, and substitution does nothing in precisely the regime where
# it is the only thing that can help (a lower bound cannot pull an estimate DOWN). So
# CTY_SUB_MIN_RPRIOR is loose and exists only to catch a county whose observed total is
# wildly below its expectation, which is an id-join failure rather than low turnout.
#
# Below the gate nothing changes: the county is floored exactly as before. So this can only
# ever REPLACE a floor with an equality, never introduce a correction of its own.
CTY_SUBSTITUTE = Sys.getenv("CTY_SUBSTITUTE", unset = "1") == "1"
CTY_SUB_MIN_RPRIOR = as.numeric(Sys.getenv("CTY_SUB_MIN_RPRIOR", unset = "0.5"))
# PIN A FINISHED PRECINCT AT ITS COUNT (PIN_DONE=0 to ablate).
# The prediction step floors the total: T = pmax(kappa*n*invlogit(theta), cnt_prec). For a
# precinct still counting that is right. For one that has FINISHED it is not -- the total
# is KNOWN, and flooring keeps every draw that lands above it, so a finished precinct
# carries systematic positive excess and never negative. It is the same misspecification
# the censoring atom addresses in the LIKELIHOOD (a finished unit sits exactly AT its
# bound, which a continuous draw gives probability zero) left unaddressed in the
# PREDICTION.
# Measured at precinct level on tx_turnout_demo at 04:00, with nearly everything counted:
# median |log turnout error| 0.0189 for this model against 0.0085 for the uniform swing
# model, whose only advantage there is that it substitutes the count exactly. It is also
# the precinct-level face of turnout reading 103.9% at 100% counted, identical across
# scenarios whose truth differed by 35%.
# Touches only the prediction, never the fit, so it cannot alter what is learned.
# Gated on q_share, NOT all_done. Gating it on all_done made it completely INERT:
# measured on tx_turnout_demo at 04:00, ZERO of 6,134 precincts had all three modes done
# (mail does not finish on election night) while 5,023 were over 95% counted. all_done is
# load-bearing in three places now -- fit_tau, the gross-up's exact-sum branch, and this
# -- and it is never true while results are being reported.
# q_share >= PIN_MIN_SHARE instead: the modes that ARE counted out cover effectively the
# whole precinct. At TX shares early+eday is 0.956, so ~0.95 pins the precincts that are
# essentially finished and leaves genuinely incomplete ones alone. It inherits the
# composition prior's reliability, but fails SAFE in a way the gross-up did not --
# substituting cnt_prec can only move the estimate DOWN toward the observed count, never
# fabricate votes above it.
# STATEWIDE TURNOUT CONSTRAINT (STATE_TURNOUT unset = off).
# Every bound in this model is PER PRECINCT -- 0 < T_i < kappa*n_i, and T_i >= C_i --
# and kappa = 1.25 binds on about 1% of them. NOTHING constrains sum_i T_i, which is the
# only quantity anyone reports.
# That is the wrong place for the bounds. Independent precinct errors cancel in a sum;
# COMMON-MODE errors do not, and a wrong est_turnout is exactly common-mode -- all 6,134
# precincts high together, and the sum preserves it perfectly. It is also why every
# precinct-level fix failed: when theta's equation is empty, sum_i T_i = sum_i est_tot_i
# EXACTLY, and no per-precinct machinery can see that.
# A statewide turnout prior is also far easier to come by than a precinct one: historical
# midterm turnout, registration growth, early-vote totals as a share of final.
#
# HOW. Per draw, sample a statewide total from the prior, floor it at what has actually
# been counted statewide, then solve for the single logit-scale shift delta that makes
# the precinct totals sum to it:
#     sum_i kappa*n_i*invlogit(theta_id + delta_d) = S_d
# A COMMON shift, so relative precinct structure is untouched; on the logit scale, so
# every precinct stays inside (0, kappa*n_i) however large delta is. The counted-votes
# floor is re-applied afterwards, which is why S_d is floored first -- otherwise the
# constraint and the floor fight and the floor wins silently.
# As counting proceeds the floor increasingly binds and the constraint does less, which
# is the correct behaviour: the data should take over from the prior.
STATE_TURNOUT = suppressWarnings(as.numeric(Sys.getenv("STATE_TURNOUT", unset = NA)))
STATE_TURNOUT_SD = as.numeric(Sys.getenv("STATE_TURNOUT_SD", unset = "0.05")) # log scale
PIN_DONE = Sys.getenv("PIN_DONE", unset = "1") == "1"
PIN_MIN_SHARE = as.numeric(Sys.getenv("PIN_MIN_SHARE", unset = "0.95"))
# CORROBORATION. q_share rests on the completeness flag, which in production is a plateau
# proxy -- three snapshots with no growth. A mode that merely PAUSED clears it, so a
# precinct can reach q_share >= 0.95 with real votes still outstanding, and pinning then
# DISCARDS them. Measured per precinct on the real TX 2024 cadence:
#   23:00  fires on   78 of 2,415 (3.2%)   turnout err 0.0343 -> 0.0537  WORSE
#   03:00  fires on  328 of 2,415 (13.6%)  turnout err 0.0450 -> 0.0045  10x better
# So the pin is harmful in broadcast hours and excellent late, and the aggregate hid it
# because 78 precincts cannot move a statewide number.
# r_prior = cnt_prec / est_tot needs NO flag: it asks whether the precinct has actually
# counted its expected vote. Requiring both means a paused mode no longer qualifies on
# its own. PIN_MIN_RPRIOR = 0 restores the flag-only behaviour.
PIN_MIN_RPRIOR = as.numeric(Sys.getenv("PIN_MIN_RPRIOR", unset = "0.95"))
# Right-censoring of the turnout equation (USE_CENSOR=0 to ablate). See the block in the
# fit loop and the EM in model_core.R::fit_conj_re.
# DEFAULT OFF on the headline metric, but the picture is genuinely split -- see the log.
# Censoring FIXES early turnout (184% -> 99% at 20:00, because giving the election-day
# equation every reporting cell instead of only counted-out ones stabilises its sigma and
# stops the exp(mu + sigma^2/2) blow-up) and BREAKS late turnout (100% -> 124%, because
# E[tau | tau >= C] > C strictly, so every still-censored cell is pushed above its bound).
# Net on two-party share: 15/15 and 0.49pp mean |err| OFF, 12/15 and 0.65pp ON.
# The misspecification is that pure Tobit gives a FINISHED cell zero probability of
# sitting exactly at its bound, when tau = C exactly is the common case late on. The fix
# is a mixture likelihood with an atom at the bound, not a different threshold.
USE_CENSOR = Sys.getenv("USE_CENSOR", unset = "0") == "1"
# ATOM AT THE BOUND (USE_ATOM=1; needs USE_CENSOR=1 to do anything). Pure Tobit gives a
# FINISHED precinct zero probability of sitting exactly at its bound, which is what makes
# censoring break late turnout -- E[T | T >= C] > C strictly, so every still-censored
# precinct is pushed above its count even when counting has plainly stopped. The mixture
# puts a point mass at C with probability p_atom (see model_core.R::fit_conj_re).
#
# WHERE p_atom COMES FROM. The evidence that a precinct has finished is how much of it is
# already flagged counted-out, measured in EXPECTED VOTES rather than in modes:
#     q_share_i = sum_{m counted out} mode_turn_share_im
# A precinct with mail+early in (q ~ 0.70 on TX 2022 shares) is probably done; one with
# only mail in (q ~ 0.04) is plainly not. This makes the completeness flag SOFT EVIDENCE
# rather than a hard gate, which matters because in production it arrives late, varies by
# state, and moves backwards. ATOM_FLOOR/ATOM_CEIL keep p off 0 and 1 so neither arm of
# the mixture is ever asserted with certainty.
# Which evidence p_atom is built from. `q_share` depends on the completeness FLAG, which
# in production is unreliable (downgraded 6 -> 5 by our own loader unless the county is
# on the trust list) and which cannot be validated historically at all -- no stage column
# exists in any 2024 scrape, and there is no 2022 scrape. `r_prior` needs no flag.
# DEFAULT IS "prior", i.e. FLAG-FREE, and that is not a precaution -- it measures as well
# or better. On the real TX 2024 cadence, two-party coverage 11/15 (prior) vs 10/15
# (both or flag), mean |err| 0.48 vs 0.47 / 0.60, and late turnout within 0.1pp of
# "both" at every checkpoint. The flag signal earns nothing and costs a checkpoint.
# NOTE this is flag-free, not assumption-free: r_prior leans on prior_tau, whose per-cell
# median ratio to truth is early 1.07 / mail 1.12 / eday 0.85 on the 2018->2022 midterm
# analogue (measure_complete_frac.R, 2026-10-30). The difference is that the prior's error
# is measurable and the flag's is not.
ATOM_SOURCE = Sys.getenv("ATOM_SOURCE", unset = "prior") # prior | flag | both
stopifnot("ATOM_SOURCE must be both, flag or prior" = ATOM_SOURCE %in% c("both", "flag", "prior"))
# PARTIAL-PRECINCT TOTALS. The reference slot's outcome is theta = logit(T/cap), and the
# strict reading of "the precinct total" is every mode counted out. But the per-mode ratio
# of a completed cell decomposes exactly:
#   log(tau_im / prior_tau_im) = log(T_i/est_tot_i) + log(p_im/prior_p_im)
#                                 \___ level (theta) ___/   \__ composition (eta) __/
# ONE completed mode identifies the level given a composition; TWO identify both. So
# eta's gate (this mode AND the reference) is already right, and only theta's is too
# strict. MIN_MODE_SHARE = 1 is the old all-modes rule; below that a precinct
# qualifies once the modes that ARE done cover that share of its expected vote, and the
# composition supplies the rest:  That = sum_{m done} tau_im / q_share.
# Weighted q_share^Q_SHARE_WT_EXP: That multiplies the observed partial by 1/q, so
# its variance scales as 1/q^2 -- an early-only precinct (q~0.65, 1.5x) is strong
# evidence, a mail-only one (q~0.04, 23x) is nearly worthless.
# CAVEAT: with one mode the composition is ASSUMED, so a composition shift reads as a
# level shift. That assumption is already present in prior_tau; the old rule simply
# declined to use the information rather than avoiding the assumption.
# COMPOSITION UPDATE (needs MIN_MODE_SHARE < 1 to matter). q_share divides counted
# votes by the share of the precinct its finished modes are expected to carry, and that
# share comes from `mode_turn_share` -- a PAST-election composition that is never
# revisited. If the mode mix shifts (a 2020->2024-style mail collapse), the gross-up is
# biased and STAYS biased, because the one quantity that knows better -- eta -- is never
# consulted.
# It is not circular to fix: eta_m is identified on precincts where mode m AND the
# reference are both counted out, and that pools ACROSS precincts, so a statewide shift
# is estimable before theta is fitted. Measure the vote-weighted mean of
#   observed log(c_m / c_ref)  -  prior log(p0_m / p0_ref)
# over those precincts, tilt the prior composition by it, renormalise, and use the
# tilted shares in q_share. Reference mode's tilt is 0 by definition.
# A statewide (not per-precinct) tilt is deliberate: mode mix moves for administrative
# reasons that are common to a state or county, and a per-precinct tilt would just be
# the gross-up's own noise fed back into itself.
COMPOSITION_UPDATE = Sys.getenv("COMPOSITION_UPDATE", unset = "0") == "1"
# LEVEL UPDATE. `r_prior` = cnt_prec / est_tot is the default evidence behind p_atom,
# i.e. P(this precinct has finished counting). But est_tot is the vendor turnout score
# and is never reassigned anywhere after section 2, so if it is 15% high then r_prior
# reads 15% low, the model concludes precincts are less finished than they are, and the
# atom under-weights the bound -- the atom's evidence is corrupted by exactly the level
# error the atom exists to help with, in the direction that makes it worse.
# The same decomposition that gives the composition tilt isolates the level:
#   log(c_im / tau0_im) = log(T_i/E_i) + log(p_im/p0_im)   [level] + [composition]
# so pooling counted-out cells and subtracting the already-estimated delta_m leaves the
# statewide level correction. Non-circular: it needs no fit, only counted-out cells.
LEVEL_UPDATE = Sys.getenv("LEVEL_UPDATE", unset = "0") == "1"
LEVEL_MIN_CELL = as.integer(Sys.getenv("LEVEL_MIN_CELL", unset = "100"))
COMP_MIN_PREC = as.integer(Sys.getenv("COMP_MIN_PREC", unset = "50")) # min precincts to trust a tilt
MIN_MODE_SHARE = as.numeric(Sys.getenv("MIN_MODE_SHARE", unset = "1"))
Q_SHARE_WT_EXP = as.numeric(Sys.getenv("Q_SHARE_WT_EXP", unset = "2"))
stopifnot("MIN_MODE_SHARE must be in (0, 1]" = MIN_MODE_SHARE > 0 && MIN_MODE_SHARE <= 1)
USE_ATOM = Sys.getenv("USE_ATOM", unset = "0") == "1"
ATOM_FLOOR = as.numeric(Sys.getenv("ATOM_FLOOR", unset = "0.02"))
ATOM_CEIL = as.numeric(Sys.getenv("ATOM_CEIL", unset = "0.98"))
# ---- BOUNDED TURNOUT: precinct total x mode composition -----------------------------
# The three per-mode turnout equations used to be unconstrained log-normals fitted
# independently, so nothing tied them to each other or to anything physical. Turnout read
# 184% of truth at 20:00 and 107.8% at 100% counted. Reparameterized:
#     theta_i = logit(T_i / (KAPPA * n_i))   precinct TOTAL, bounded in (0, KAPPA*n_i)
#     eta_im  = log(p_im / p_i,ref)          composition, m != REF_MODE
#     tau_im  = T_i * softmax(eta)_im        so sum_m tau_im = T_i exactly
# Measured: the precinct total is predicted at corr(log) 0.962 / sd(log) 0.246 while the
# mode split is at sd(log) 0.698 for eday -- so the well-known quantity now gets its own
# equation instead of being multiplied by the badly-known one.
# A cap only bites on the TOTAL: per-mode caps must exceed each mode's observed max share
# of registrants, which sum to 4.66x n against an actual total of 0.654x n (7.1x actual,
# useless). On the total, kappa=1.25 binds on 1.16% of precincts and allows 1.91x actual.
# USE_COMPOSE=0 restores the old independent-log-normal turnout equations.
USE_COMPOSE = Sys.getenv("USE_COMPOSE", unset = "1") == "1"
KAPPA = as.numeric(Sys.getenv("KAPPA", unset = "1.25"))
# ---- PER-STATE MODE SCOPE -------------------------------------------------------
# REF_MODE carries theta (the precinct TOTAL); every other mode is a log-ratio to it. So
# the reference must be a mode that is OBSERVED, early and everywhere, in this state. The
# old hardcoded "early" is a TX fact (65.1% of 2022 voters) and is wrong elsewhere:
# measured on the live path, NC's ref_share is 0 at the median and the non-reference
# y_tau_prior sits at ~13 (= log(share / 1e-6), the floor), because NC's mode history says
# early = 0. MI is 0.0% early in 2022 and CA 1.6%, with no `early` in CA's feed at all.
#
# Rule: the largest mode in the 2022 midterm, the same analogue MODE_TURN_SHARE uses.
# NC is a documented exception -- see the note column in the lookup.
# PRECEDENCE: env var > lookup row for (STATE, year) > the legacy TX default. The env
# override is what the backtests set, so they are unaffected.
MODE_SCOPE_PATH = Sys.getenv("MODE_SCOPE_PATH", unset = "scripts/model/lookups/mode_scope.csv")
MODE_SCOPE_YEAR = as.integer(Sys.getenv("MODE_SCOPE_YEAR", unset = "2026"))
mode_scope = local({
  if (!file.exists(MODE_SCOPE_PATH)) {
    return(NULL)
  }
  d = utils::read.csv(MODE_SCOPE_PATH, colClasses = "character")
  r = d[d$state == STATE & d$year == as.character(MODE_SCOPE_YEAR), , drop = FALSE]
  if (!nrow(r)) {
    return(NULL)
  }
  list(
    ref_mode = trimws(r$ref_mode[1]),
    modes = trimws(strsplit(r$modes[1], "|", fixed = TRUE)[[1]])
  )
})
if (is.null(mode_scope)) {
  cli_alert_warning(
    "No {.path {basename(MODE_SCOPE_PATH)}} row for {STATE}/{MODE_SCOPE_YEAR} \\
                     -- falling back to the legacy TX scope (ref early, mail/early/eday). \\
                     Add a row rather than relying on this."
  )
}
REF_MODE = Sys.getenv("REF_MODE", unset = if (is.null(mode_scope)) "early" else mode_scope$ref_mode)
# Prior sd on the STATEWIDE LEVEL, in logit units, decoupled from PRIOR_N_*.
# MEASURED (calibrate_swing.R, 9 states x 37 contest pairs): the statewide two-party Dem
# share moves 0.22-0.26 logit between a prior contest and its target, against the ~0.098
# the g-prior implicitly assumed -- so the bundled prior was ~2.3x too tight.
# 0.22 is sen20->pres24, the closest analogue to what the model actually does.
# NOTE this cannot be validated by a single-election backtest: TX 2024's own swing was
# 0.085, unusually small, so the tight prior did no harm there. This is insurance against
# a typical-swing election the backtest does not contain.
# DEFAULT 0 (bundled, i.e. OFF) on the strength of the backtest. Measured on TX 2024:
#   SD_SYS_LEVEL=0     15/15 coverage, max |err| 1.40pp, mean 0.49pp
#   SD_SYS_LEVEL=0.22  15/15 coverage, max |err| 2.23pp, mean 0.63pp
# Loosening the level prior makes POINT ESTIMATES WORSE, and the reason matters: a looser
# prior lets the data pull the statewide level, and the early-night data is BIASED (the
# first batches read 8.9pp more Democratic than the final), not merely noisy. The tight
# bundled prior was doing protective work against sample bias that the swing measurement
# does not by itself justify removing.
# So the 0.22 finding is real but it sizes the wrong thing: interval WIDTH should reflect
# level uncertainty (0.22), while SHRINKAGE against a biased early sample is a separate
# job. Conflating them is what this ablation exposed. Setting SD_SYS_LEVEL > 0 does both
# at once, which is why it is off by default pending a design that separates them --
# most likely a mean-zero statewide offset added to the PREDICTIVE draw (as SD_UNREP is)
# rather than a loosening of the posterior shrinkage.
SD_SYS_LEVEL = as.numeric(Sys.getenv("SD_SYS_LEVEL", unset = "0"))
LEVEL = 0.95
USE_CROSS_MODE = TRUE # FALSE => independent residuals (ablation, for comparison)

# Where the 2M x 2M residual covariance comes from.
#   "historical" -- PREFERRED. Sigma is a structural quantity: estimate it ONCE from a
#                   past election where every mode eventually reported, then just use
#                   it tonight. See note below for why this is not optional.
#   "pairwise"   -- estimate from tonight's returns pairwise (blocks with no overlap
#                   are left at 0), then PSD-repair. Usable but degrades early.
#   "diagonal"   -- independent residuals; disables cross-mode conditioning.
#
# WHY NOT complete-case estimation from tonight's data: modes report in SEQUENCE
# (mail, then early, then election day), so the number of precincts with all modes
# counted is ~0 until the count is essentially over. The mail<->eday correlation --
# the single most useful block, the one that lets early returns move the election-day
# prediction -- is NEVER estimable from tonight's data. It must come from history.
# "measured" = build Sigma as D %*% C %*% D, with D the per-equation sigmas from tonight's
#   own fits and C the correlation matrix MEASURED on real returns (below). Preferred,
#   because the cross-mode share blocks are not estimable from TX data at all and the
#   alternative is calibrating on synthetic data.
# "historical" / "pairwise" / "diagonal" as before.
SIGMA_SOURCE = Sys.getenv("SIGMA_SOURCE", unset = "measured")

# MEASURED correlation structure. Sources, all from real returns:
#   within-mode rho(tau, s): NC 2024, 1,118 precincts
#        mail +0.204 (z=6.9, REAL) | early +0.052 (ns) | eday -0.140 (z=-4.7, REAL)
#        -- real, modest, and SIGN-VARYING: a mail surge goes with a more-Dem mail
#           electorate, an election-day surge with a less-Dem one.
#   cross-mode rho(s, s):    NC 2024
#        mail<->early +0.232 (z=7.9, REAL) | mail<->eday -0.050 (ns) | early<->eday +0.039 (ns)
#        -- NOTE this is ATTENUATED by sampling noise: in the largest precincts, where
#           noise is smallest, mail<->early reads 0.34-0.39, so the structural value is
#           plausibly ~0.4. RHO_ME_CORRECTED=1 uses 0.40 instead of 0.232.
#   cross-mode rho(tau, tau): ~0 (L2 TX, 5,362 precincts, |rho| < 0.2 in both 2022 and 2024)
#   anything involving tp:    unmeasured, set to 0
# These are BORROWED FROM NC for the share blocks -- TX has no mode x party history, so
# there is no alternative but to borrow or to assume zero, and zero is not the safer choice
# (see the memo's discussion of why an "uninformative" value does not exist here).
RHO_TAU_S = c(mail = 0.204, early = 0.052, eday = -0.140) # within mode
RHO_S_CROSS = if (Sys.getenv("RHO_ME_CORRECTED") == "1") 0.40 else 0.232 # mail<->early

#' Prior correlation matrix among the equations
#'
#' Within-mode tau <-> s correlations (`RHO_TAU_S`) and the mail <-> early share correlation
#' (`RHO_S_CROSS`), both measured on NC 2024; every other pair is 0.
#'
#' @param keys Equation names, `<mode>_<outcome>`.
#' @return Correlation matrix (PSD-repaired).
measured_corr = function(keys) {
  K = length(keys)
  C = diag(K)
  dimnames(C) = list(keys, keys)
  idx = function(m, o) which(keys == paste(m, o, sep = "_"))
  for (m in MODES) {
    # within-mode tau<->s
    a = idx(m, "tau")
    b = idx(m, "s")
    C[a, b] = C[b, a] = RHO_TAU_S[[m]]
  }
  a = idx("mail", "s")
  b = idx("early", "s") # the one real cross-mode pair
  C[a, b] = C[b, a] = RHO_S_CROSS
  psd_repair(C)
}
# Cross-mode share<->share correlation. NOT a neutral default at any value:
#   rho > 0 widens the aggregate interval (precinct errors reinforce across modes) and
#           lets observed modes move unobserved ones;
#   rho = 0 narrows it and ignores that signal -- anti-conservative on BOTH counts.
# Measured for TURNOUT from L2 (5,362 TX precincts) it is ~0 (|rho| < 0.2 both years).
# For SHARE it is unmeasured: no mode x party history. Set RHO_CROSS to a number to
# override the calibrated blocks, or "draw" to integrate over it per posterior draw.
RHO_CROSS = Sys.getenv("RHO_CROSS", unset = "calibrated")
# Structure imposed on Sigma. Decomposition of what the 9x9 actually contains:
#   diagonal           -- per-equation residual variance. Needed, but ALREADY supplied by
#                         each lm_bayes fit's post$sigma_scale; Sigma is redundant for it.
#   within-mode blocks -- tau<->tp, tau<->s, tp<->s in the SAME cell. These DO affect
#                         Var(aggregate), because the aggregate is nonlinear in all three
#                         jointly within each cell. Estimable from precinct-level past
#                         results with NO mode x party data needed.
#   cross-mode blocks  -- measured ~0 for turnout (L2, 5,362 TX precincts, |rho|<0.2 both
#                         years) and unidentified for share. Measurably irrelevant to the
#                         statewide number.
# "block" => block-diagonal by mode: keeps what matters, drops what is unmeasurable, and
# makes the conditional-MVN step a no-op (independent blocks carry no cross-mode signal).
# MEASURED on NC 2024 (1,118 precincts), share-residual correlations:
#   WITHIN mode, rho(tau, s):  mail +0.204 (z=6.9, REAL) | early +0.052 (ns) | eday -0.140
#                              (z=-4.7, REAL) -- real, modest, and SIGN-VARYING by mode.
#   CROSS mode, rho(s, s):     mail<->early +0.232 (z=7.9, REAL) | mail<->eday -0.050 (ns)
#                              | early<->eday +0.039 (ns)
#   CROSS mode, rho(tau, tau): ~0 from L2 TX (5,362 precincts, |rho|<0.2 both years)
# Propagated, the cross-mode blocks move aggregate sd by 0.09% (statewide) to 0.86%
# (30-precinct district). And the one real cross-mode pair (mail<->early) is the pair that
# reports closest together, so there is little time gap to exploit -- while mail<->eday,
# which the conditioning machinery was built for, is indistinguishable from zero.
# => "block" is the evidence-based default: keep the measured within-mode blocks, drop the
#    cross-mode ones. This also makes the conditional-MVN step a no-op across modes.
# => "diag" is the default. Measured, with the generator matched to NC's error structure,
#    full / block / diag give statewide sd 3.071e-2 / 3.070e-2 / 3.070e-2 at 30% reported.
#    Correlation structure is second-order at every aggregate scale because the residual
#    term is only O(1/sqrt(k)) of total variance. Diagonal also removes the conditional-MVN
#    path entirely, which was the most intricate part of the model for no measured gain.
#    "block" and "full" remain available for sub-state work or if a future election shows a
#    materially larger cross-mode share correlation than NC 2024 did.
# RE-ENABLED 2026-10-18 for PRECINCT-LEVEL output. The evidence that justified "diag"
# was about AGGREGATES: correlation structure moves a statewide interval by 0.09% and even
# a 30-precinct district by 0.86%, because the residual term is only O(1/sqrt(k)) of total
# variance there. At CELL level there is no averaging, so the correlation IS the effect --
# the measured within-precinct sd inflation from it is 4.7%, and conditioning on an
# observed mode is the only way one mode informs another. So: "full" when you want
# precinct-level predictions, "diag" if you only publish aggregates.
SIGMA_STRUCT = Sys.getenv("SIGMA_STRUCT", unset = "full") # full | block | diag

# ---- COUNTY RANDOM EFFECT -------------------------------------------------------
# The dominant uncertainty term, by a wide margin. Residuals are NOT independent across
# precincts: precincts in a county err together. Var(sum of N terms) =
# N*s2*(1 + (N-1)*rhobar), so even rhobar = 0.001 roughly DOUBLES a statewide interval,
# while everything else examined in this model moved it by <1%.
#
# MEASURED, and REPLICATED across nine states and two cycles so it is not one state's
# quirk. Design: a top-of-ticket statewide office regressed on another statewide office on
# the same ballot (vote-weighted, plus log size), then a variance decomposition by county.
# MEDSL official precinct returns.
#
#   state    2022 ICC   2024 ICC      sd inflation (22 / 24)
#   TX        0.269      0.196          3.07x / 2.72x      <- the target state
#   NC          --       0.430            --   / 3.47x
#   GA        0.295        --           2.37x /   --
#   MI        0.381      0.149          4.61x / 2.90x
#   PA        0.091      0.091          3.38x / 3.52x
#   WI        0.114      0.144          2.37x / 2.66x
#   AZ        0.089      0.227          3.19x / 5.12x
#   OH        0.193      0.131          4.31x / 3.70x
#   NV        0.106      0.142          2.81x / 3.22x
#
# ICC ranges 0.09-0.43, but the sd INFLATION is tightly clustered at 2.4x-5.1x everywhere,
# because inflation depends on ICC x precincts-per-county and states trade off between the
# two (PA has a low ICC but 127 precincts per county). So: any precinct model treating
# precincts as independent is overconfident by roughly a factor of three, essentially
# everywhere. 2022 ICCs generally exceed 2024 -- midterms look more county-clustered than
# presidentials -- and 2026 is a MIDTERM, so the 2022 column is the right analogue.
#
# Defaults are the TEXAS 2022 values, so no cross-state borrowing on the parameter that
# matters most:
#   share   0.269  (MEDSL TX 2022, governor ~ attorney general, 7,529 precincts/233 cty)
#   turnout 0.200  (L2 TX, log(vote22) ~ registration + demographics + 2018 turnout,
#                   5,433 precincts/32 counties; county sd 0.044, within 0.088)
#
# Implemented as a ridge-equivalent random effect: county indicator columns are appended
# to the design and shrunk toward zero by pseudo-rows of weight lambda = (1-ICC)/ICC.
# That is algebraically a random effect u_c ~ N(0, sigma^2/lambda) but stays a single
# weighted least-squares solve, so conjugacy and the closed-form draws survive.
# A county with NO reported precincts keeps the full random-effect variance, which is
# exactly where the widening comes from.
USE_COUNTY_RE = Sys.getenv("USE_COUNTY_RE", unset = "1") == "1"
# Which grouping the random effect uses. "county" is the default and what the measurements
# above support. "cory" reproduces the TX primary pipeline's choice --
# (1|stratum_demo) + (1|geostratum), i.e. 5 and 6 levels -- for comparison. NC says a
# coarse stratification recovers only 3-7% of the county effect (R^2 of the county effect
# on 5 or 6 lean strata = 0.045 / 0.034), so this is expected to under-cover.
# "stratum_demo" alone is there for the live wrapper's demographic specs (live/R/demos.R).
RE_GROUPING = Sys.getenv("RE_GROUPING", unset = "county") # county | cory | geostratum | stratum_demo
ICC_TURN = as.numeric(Sys.getenv("ICC_TURN", unset = "0.200")) # TX 2022, L2
ICC_SHARE = as.numeric(Sys.getenv("ICC_SHARE", unset = "0.269")) # TX 2022, MEDSL
ICC_TP = as.numeric(Sys.getenv("ICC_TP", unset = "0.269")) # unmeasured; share proxy

#' Covariate list from an environment variable
#'
#' @param var Variable name (comma-separated list).
#' @param default Value when unset.
#' @return Character vector.
cov_env = function(var, default) {
  v = Sys.getenv(var, unset = "")
  if (!nzchar(v)) {
    return(default)
  }
  trimws(strsplit(v, ",", fixed = TRUE)[[1]])
}
COVS_TURN = cov_env(
  "COVS_TURN",
  c("log_n", "turnout22", "turnout24", "age", "male", "pred.bla", "pred.his", "dem2")
)
COVS_SHARE = cov_env(
  "COVS_SHARE",
  c("log_n", "turnout22", "turnout24", "age", "male", "pred.bla", "pred.his", "dem2")
)
# Third-party share is a weak, stable signal -- keep its design small or it overfits.
COVS_TP = cov_env("COVS_TP", c("log_n", "age", "male", "dem2", "tp_prior_raw"))
# Covariates DERIVED in section 1 rather than read from the frame. Everything else named
# by a COVS_* list is a raw frame column and gets standardized there.
COVS_DERIVED = c("log_n", "dem2", "tp_prior_raw")

# Three outcomes per mode. The composition is FACTORED, not flattened:
#     tau = ALL votes in the cell
#     tp  = third-party share OF ALL votes
#     s   = Dem share of the TWO-PARTY vote
# so that v_dem = tau(1-tp)s, v_rep = tau(1-tp)(1-s), v_oth = tau*tp. That map is a
# bijection to (v_dem, v_rep, v_oth), so the three shares of all votes sum to 1 and sit
# in [0,1] BY CONSTRUCTION -- no clamping, no renormalizing, and none of the variance
# deflation that per-candidate models plus post-hoc normalization produce.
# It also keeps s (what decides the race, and what the dem2 prior already expresses)
# on its own scale rather than buried in a K-way composition.
OUTCOMES = c("tau", "tp", "s")
K_OUT = length(OUTCOMES)
PRIOR_N_TP = 2.0 # prior strength for the third-party equation
TP_SHRINK_N = 2000 # registrants at which the precinct tp signal gets half weight
TP_RANGE = c(0.001, 0.15) # clamp for the third-party prior

# ---- MODE PRIORS ----------------------------------------------------------------
# Canonical mode keys from enightmodels::VOTE_METHODS. THESE ARE THE MODEL'S MODES, i.e.
# what survives folding -- `prov` folds into `eday` and is never its own equation, and a
# state whose feed carries no `early` (CA) drops it rather than carrying a dead slot whose
# ref_share would be 0. Set per state in lookups/mode_scope.csv; MODES= overrides.
# M = length(MODES) flows into Sigma, eps, the conditional-MVN grouping and the softmax,
# so changing it changes the equation count: 3 modes x 3 outcomes = 9 equations, 2 x 3 = 6.
MODES = local({
  if (nzchar(Sys.getenv("MODES"))) {
    return(trimws(strsplit(Sys.getenv("MODES"), ",")[[1]]))
  }
  if (is.null(mode_scope)) c("mail", "early", "eday") else mode_scope$modes
})
stopifnot("REF_MODE must be one of MODES" = REF_MODE %in% MODES)
#
# mode_hist (geo, method, party, votes; from {ST}_precinct_mode_history.parquet or
# {ST}_precinct_mode_results.parquet) gives each precinct's turnout split
# (derive_mode_priors() below). The constants here are fallbacks for when it is NULL.
#
# MODE_TURN_SHARE: TX's 2022 midterm split from L2 (mail 4.2 / early 63.8 / eday 32.0).
# 2026 is a midterm; presidential years run far less election-day (TX 2024: 20.2%).
MODE_TURN_SHARE = c(mail = 0.042, early = 0.638, eday = 0.320)
#
# MODE_SHARE_OFFSET: the mode partisan differential, as a LINEAR function of the
# precinct's own lean, not a constant:
#     delta_im = a_m + b_m * logit(dem2_i)
#
# MEASURED 2026-10-18 on NC 2024 (1,118 precincts with >=25 mail and >=50 early/eday
# votes; MEDSL official returns, the only state with complete precinct x mode x party):
#
#   mode    mean delta   slope on lean    t
#   mail      +0.660        -0.297      -20.8     <- large, and strongly lean-dependent
#   early     -0.029        +0.041       17.5
#   eday      -0.025        -0.089      -11.8
#
# A CONSTANT offset is therefore misspecified, not approximately right: mail's Democratic
# skew is largest in Republican-leaning precincts and shrinks as the precinct gets more
# Democratic. (The LIKELIHOOD already handles this, since each mode gets its own beta^s
# with dem2 in the design; it is the PRIOR that was wrong.)
#
# Defaults stay at zero for TX because the magnitudes should NOT be transferred from NC:
# NC has broad no-excuse mail access while TX restricts mail to 65+/disabled/absent, so
# the TX mail electorate is older, smaller (~4% of votes vs NC's ~5%) and differently
# composed. Use NC to establish that a nonzero, lean-dependent offset is needed -- not to
# import its size. Set USE_NC_DELTA=1 to borrow them anyway for a sensitivity run.
MODE_SHARE_OFFSET = rbind(
  mail = c(a = 0.0, b = 0.0),
  early = c(a = 0.0, b = 0.0),
  eday = c(a = 0.0, b = 0.0)
)
NC_DELTA = rbind(
  # measured above; borrow only deliberately
  mail = c(a = 0.660, b = -0.297),
  early = c(a = -0.029, b = 0.041),
  eday = c(a = -0.025, b = -0.089)
)
if (Sys.getenv("USE_NC_DELTA") == "1") {
  MODE_SHARE_OFFSET = NC_DELTA
  cli_alert_warning("Using NC-measured mode offsets for TX -- sensitivity run only.")
}
# Where delta_m comes from when a mode-history FILE is present (it overrides the
# constants above). "history" derives it from the file, "zero" and "nc" keep the file's
# turnout split but replace its partisan offset. See derive_mode_priors().
MODE_OFFSET_SOURCE = Sys.getenv("MODE_OFFSET_SOURCE", unset = "history")

# --------------------------------------------------------------------------------

# The fallback constants above are written for the legacy mail/early/eday scope. When a
# state's MODES differ (CA drops `early`), subset and RENORMALISE so the fallback still
# sums to 1, rather than tripping the assertion below with a scope mismatch. Only the
# fallback is touched -- a derived per-precinct split (derive_mode_priors) is renormalised
# per geo in the `cells` block regardless.
if (!setequal(names(MODE_TURN_SHARE), MODES)) {
  miss = setdiff(MODES, names(MODE_TURN_SHARE))
  if (length(miss)) {
    cli_abort("MODES names {miss} with no MODE_TURN_SHARE fallback value.")
  }
  dropped = setdiff(names(MODE_TURN_SHARE), MODES)
  MODE_TURN_SHARE = MODE_TURN_SHARE[MODES]
  MODE_TURN_SHARE = MODE_TURN_SHARE / sum(MODE_TURN_SHARE)
  MODE_SHARE_OFFSET = MODE_SHARE_OFFSET[MODES, , drop = FALSE]
  cli_alert_info(
    "{STATE}: mode scope drops {dropped}; fallback MODE_TURN_SHARE \\
                  renormalised to {paste(sprintf('%s %.3f', MODES, MODE_TURN_SHARE), collapse = ' ')}"
  )
}

stopifnot(
  "MODE_TURN_SHARE must sum to 1" = abs(sum(MODE_TURN_SHARE) - 1) < 1e-9,
  "MODE_TURN_SHARE must be named for MODES" = setequal(names(MODE_TURN_SHARE), MODES),
  "MODE_SHARE_OFFSET must be a MODES x (a,b) matrix" = setequal(
    rownames(MODE_SHARE_OFFSET),
    MODES
  ) &&
    identical(colnames(MODE_SHARE_OFFSET), c("a", "b"))
)

# ---- 1. precinct covariates --------------------------------------------------------
# covs_long is supplied by the caller (live/R/model_medsl.R's medsl_inputs()): one row per
# precinct x party, with the structural columns below and every column the active COVS_*
# lists name.
local({
  req = unique(c(
    "geo",
    "county",
    "geostratum",
    "stratum_demo",
    "n",
    "party",
    "est",
    "method",
    setdiff(c(COVS_TURN, COVS_SHARE, COVS_TP), COVS_DERIVED),
    "dem2"
  ))
  miss = setdiff(req, names(covs_long))
  if (length(miss)) cli_abort("covs_long is missing required columns: {miss}")
})

# ---- 1b. mode history -> per-precinct mode priors -------------------------------
#' Mode priors from the precinct x mode history
#'
#' Turns `mode_hist` into (a) per-precinct mode turnout shares (replacing
#' `MODE_TURN_SHARE`) and (b) per-mode logit share offsets (replacing
#' `MODE_SHARE_OFFSET`), each shrunk toward the county / statewide value. The offsets
#' follow `MODE_OFFSET_SOURCE`: "history" (derived), "zero", or "nc" (`NC_DELTA`).
#'
#' @param hist Mode history: geo, method, party, votes.
#' @param prec Precinct frame.
#' @param k_turn Shrinkage of the turnout split toward the county, in votes.
#' @param k_share Shrinkage of the share offsets.
#' @return List: turn_share, offset, shrink_share; NULL when `hist` is NULL.
derive_mode_priors = function(hist, prec, k_turn = 40, k_share = 60) {
  if (is.null(hist)) {
    return(NULL)
  }
  cty = prec |> select(geo, county) |> mutate(geo = as.character(geo))
  h = hist |>
    pivot_wider(names_from = party, values_from = votes, values_fill = 0) |>
    inner_join(cty, by = "geo")
  if (!nrow(h)) {
    cli_abort(
      "mode history did not join to any precinct `geo`. \\
                            Check the id format (tx_{{county_cbs}}_{{precinct}})."
    )
  }
  cli_inform("mode history joined {n_distinct(h$geo)} of {nrow(prec)} precincts")

  # (a) turnout split: precinct mode share, shrunk to its county's mode share
  tot = h |> mutate(v = dem + rep) |> summarize(v = sum(v), .by = c(geo, county, method))
  cty_sh = tot |>
    summarize(v = sum(v), .by = c(county, method)) |>
    mutate(cty_share = v / sum(v), .by = county) |>
    select(county, method, cty_share)
  sw_sh = tot |> summarize(v = sum(v), .by = method) |> mutate(sw = v / sum(v))
  turn_share = tot |>
    mutate(n_prec = sum(v), .by = geo) |>
    left_join(cty_sh, by = c("county", "method")) |>
    left_join(select(sw_sh, method, sw), by = "method") |>
    mutate(
      cty_share = coalesce(cty_share, sw),
      share = (v + k_turn * cty_share) / (n_prec + k_turn) # Beta-ish shrinkage
    ) |>
    mutate(share = share / sum(share), .by = geo) |>
    select(geo, method, mode_turn_share = share)

  # (b) partisan offset: logit_shift so each mode's weighted aggregate matches that
  #     mode's observed historical two-party Dem share
  obs_mode = h |>
    summarize(d = sum(dem), r = sum(rep), .by = method) |>
    mutate(dem2_mode = d / (d + r))
  w = tot |> select(geo, method, v)
  # Estimate a_m and b_m by regressing the precinct's observed mode differential on its
  # own lean, weighted by that mode's votes. Falls back to (0,0) on thin data.
  off = t(vapply(
    MODES,
    function(m) {
      hj = h |>
        filter(method == m) |>
        inner_join(select(prec, geo, dem2), by = "geo") |>
        mutate(n2 = dem + rep, s_m = dem / pmax(n2, 1)) |>
        filter(n2 >= 25, is.finite(dem2), dem2 > 0, dem2 < 1, s_m > 0, s_m < 1)
      if (nrow(hj) < 100) {
        return(c(a = 0, b = 0))
      }
      x = logit(hj$dem2)
      y = logit(hj$s_m) - x
      f = stats::lm.wfit(cbind(1, x), y, hj$n2)
      c(a = unname(f$coefficients[1]), b = unname(f$coefficients[2]))
    },
    numeric(2)
  ))
  rownames(off) = MODES
  # The turnout split and the partisan offset come from the same file but are NOT equally
  # trustworthy: a history built from registration-by-mode carries a real turnout split
  # and a misleading partisan one. MODE_OFFSET_SOURCE lets the two be sourced separately.
  if (MODE_OFFSET_SOURCE == "zero") {
    off[] = 0
    cli_alert_info(
      "MODE_OFFSET_SOURCE=zero -- keeping the derived turnout split, \\
                    forcing delta_m to 0"
    )
  } else if (MODE_OFFSET_SOURCE == "nc") {
    off = NC_DELTA[MODES, , drop = FALSE]
    cli_alert_info(
      "MODE_OFFSET_SOURCE=nc -- keeping the derived turnout split, \\
                    borrowing NC-measured delta_m"
    )
  } else if (MODE_OFFSET_SOURCE != "history") {
    cli_abort(
      "unknown MODE_OFFSET_SOURCE {MODE_OFFSET_SOURCE} \\
               (history | zero | nc)"
    )
  }
  cli_inform(c(
    "derived mode priors:",
    paste0(
      "  statewide turnout split: ",
      paste(sprintf("%s %.3f", sw_sh$method, sw_sh$sw), collapse = "  ")
    ),
    paste0(
      "  logit share offsets:     ",
      paste(sprintf("%s %+.3f", names(off), off), collapse = "  ")
    )
  ))
  list(turn_share = turn_share, offset = off, shrink_share = k_share)
}

# mode_hist (geo, method, party, votes) is supplied by the caller; derive_mode_priors() runs after `prec`

# one row per precinct: party-long -> wide, and recover total expected turnout
prec = covs_long |>
  filter(method == "all") |>
  select(-method) |>
  pivot_wider(names_from = party, values_from = est, names_prefix = "est_") |>
  mutate(
    est_tot = est_dem + est_rep,
    log_n = log(n),
    # third-party prior from the frame's residual 1 - pr_harris - pr_trump. Measured
    # n-weighted 1.64% for TX vs ~1.4% actual 2024 "other", so it is signal -- but the
    # precinct range is 0% to 20.4%, so shrink hard toward the statewide value.
    # A panel may instead supply `tp_raw` directly -- a backtest must, since
    # pr_harris/pr_trump are calibrated to 2024 returns and so are look-ahead for 2024.
    tp_prior_raw = if ("tp_raw" %in% names(covs_long)) {
      pmin(pmax(tp_raw, 0), 0.25)
    } else {
      pmin(pmax(1 - pr_harris - pr_trump, 0), 0.25)
    },
    # standardize every raw frame covariate the active COVS_* lists name (with the
    # default lists this is turnout22/turnout24/age/male/pred.bla/pred.his, as before)
    across(
      all_of(setdiff(unique(c(COVS_TURN, COVS_SHARE, COVS_TP)), COVS_DERIVED)),
      ~ as.numeric(scale(.x))
    )
  ) |>
  filter(est_tot > 0, is.finite(dem2), is.finite(tp_prior_raw)) |>
  mutate(
    tp_sw = stats::weighted.mean(tp_prior_raw, est_tot),
    w_tp = n / (n + TP_SHRINK_N),
    tp_prior = pmin(pmax(w_tp * tp_prior_raw + (1 - w_tp) * tp_sw, TP_RANGE[1]), TP_RANGE[2])
  )

cli_inform(c(
  "precincts: {nrow(prec)}",
  "baseline expected turnout: {format(round(sum(prec$est_tot)), big.mark = ',')}",
  "baseline Dem two-party share: {round(weighted.mean(prec$dem2, prec$est_tot), 4)}"
))

#' Warn when the mode priors fall back to constants
#'
#' @param mode_priors `derive_mode_priors()` output.
#' @return NULL, invisibly; warns only when `mode_priors` is NULL.
warn_uncalibrated_modes = function(mode_priors) {
  if (!is.null(mode_priors)) {
    return(invisible())
  }
  if (all(MODE_SHARE_OFFSET == 0)) {
    cli_alert_warning(
      "No mode history, and MODE_SHARE_OFFSET is all zero: the prior \\
                       assumes every mode votes like its precinct overall. Early-night \\
                       output will carry the full red-mirage/blue-shift bias."
    )
  } else {
    cli_alert_warning("Using hardcoded MODE_* constants (no mode history found).")
  }
}

# ---- 2. expand to precinct x mode cells, build mode-specific priors -------------
mode_priors = derive_mode_priors(mode_hist, prec)
warn_uncalibrated_modes(mode_priors)

cells = tidyr::expand_grid(prec, mode = MODES) |>
  {
    \(x) {
      if (is.null(mode_priors)) {
        mutate(
          x,
          mode_turn_share = MODE_TURN_SHARE[mode],
          off_a = MODE_SHARE_OFFSET[mode, "a"],
          off_b = MODE_SHARE_OFFSET[mode, "b"]
        )
      } else {
        x |>
          left_join(rename(mode_priors$turn_share, mode = method), by = c("geo", "mode")) |>
          mutate(
            # precincts the history did not cover fall back to the statewide constants
            mode_turn_share = coalesce(mode_turn_share, MODE_TURN_SHARE[mode]),
            off_a = mode_priors$offset[mode, "a"],
            off_b = mode_priors$offset[mode, "b"]
          )
      }
    }
  }() |>
  mutate(
    mode_turn_share = mode_turn_share / sum(mode_turn_share),
    .by = geo
  ) |>
  mutate(
    prior_tau = est_tot * mode_turn_share,
    # delta_im = a_m + b_m * logit(dem2_i)  -- lean-dependent, per the NC measurement
    prior_s = invlogit(logit(dem2) + off_a + off_b * logit(dem2)),
    prior_tp = tp_prior, # no mode differential assumed for third party
    y_tp_prior = logit(prior_tp),
    y_s_prior = logit(prior_s),
    # registrant-based ceiling on the PRECINCT total
    cap = KAPPA * pmax(n, 1),
    # share of this precinct's prior composition carried by the reference mode
    ref_share = mode_turn_share[match(REF_MODE, mode)],
    .by = geo
  ) |>
  mutate(
    # The per-mode `tau` SLOT is reinterpreted rather than restructured, so every
    # downstream index (Sigma, eps, the conditional-MVN block, the aggregation offsets)
    # keeps working unchanged:
    #   reference mode's slot -> theta, the precinct TOTAL on the capped logit scale
    #   other modes' slots    -> eta_m, the log-ratio of that mode's share to the ref's
    y_tau_prior = if (!USE_COMPOSE) {
      log(prior_tau + 0.1)
    } else {
      if_else(
        mode == REF_MODE,
        logit(pmin(pmax(est_tot / cap, 1e-4), 1 - 1e-4)),
        log(pmax(mode_turn_share, 1e-6) / pmax(ref_share, 1e-6))
      )
    }
  )
N_PREC = nrow(prec)
M = length(MODES)
# county indicator matrix for the random effect (one column per county, in prec order)
CTY_MAT = if (!USE_COUNTY_RE) {
  matrix(0, nrow(prec), 0)
} else {
  mk = function(v, pre) {
    f = factor(v)
    m = stats::model.matrix(~ f - 1)
    colnames(m) = paste0(pre, levels(f))
    m
  }
  switch(
    RE_GROUPING,
    county = mk(prec$county, "cty_"),
    geostratum = mk(prec$geostratum, "geo_"),
    stratum_demo = mk(prec$stratum_demo, "sd_"),
    cory = cbind(mk(prec$stratum_demo, "sd_"), mk(prec$geostratum, "geo_")),
    cli_abort("unknown RE_GROUPING {RE_GROUPING}")
  )
}
# Congressional-district indicators, added to the random-effect block of the TURNOUT (tau)
# equations only: a county with few reported precincts borrows from its 2026 district
# before the statewide level. Their ridge weight is not the counties' ICC_TURN: it is
# sigma^2 / tau^2, both estimated in each fit from the counted-out precincts (re_shrink(),
# live/R/demos.R: within-district residual variance and between-district variance of y -
# prior; tau^2 = 0 makes the weight huge, i.e. no district effect). Ansolabehere et al. (2024), "Forecasting
# Turnout" (HDSR 6.4): turnout is far more predictable by district than by county. Needs
# `cd` on covs_long (live/R/model_medsl.R's medsl_inputs()); USE_CD_TURN=0 turns it off.
USE_CD_TURN = Sys.getenv("USE_CD_TURN", unset = "1") == "1"
CD_MAT = if (USE_COUNTY_RE && USE_CD_TURN && "cd" %in% names(prec) && length(unique(prec$cd)) > 1) {
  m = stats::model.matrix(~ f - 1, data.frame(f = factor(prec$cd)))
  colnames(m) = paste0("cd_", levels(factor(prec$cd)))
  m
} else {
  matrix(0, nrow(prec), 0)
}
#' The random-effect block of an equation: groups, plus the districts for turnout
#'
#' @param out Outcome ("tau", "tp" or "s").
#' @return Indicator matrix, one row per precinct.
re_mat = function(out) if (out == "tau") cbind(CTY_MAT, CD_MAT) else CTY_MAT
if (USE_COUNTY_RE) {
  cli_inform(
    "random effect ON [{RE_GROUPING}{if (ncol(CD_MAT)) paste0(' + ', ncol(CD_MAT), ' districts in turnout') else ''}]: {ncol(CTY_MAT)} levels, \\
              lambda turn {round((1-ICC_TURN)/ICC_TURN,2)} / share {round((1-ICC_SHARE)/ICC_SHARE,2)}"
  )
}
cli_inform("cells: {nrow(cells)} ({N_PREC} precincts x {M} modes)")

# ---- 3. fit --------------------------------------------------------------------
# One lm_bayes per (mode, outcome) = K_OUT * M = 3M equations (9 for three modes). All
# share a design, so these coefficients equal the full-SUR ones; Sigma is estimated from
# residuals. Caveat: the equivalence needs the same regressors on the SAME ROWS, and the
# observed sets differ by mode on the night (early reports before eday), so this is an
# unbalanced SUR and equation-by-equation is not strictly efficient. Unmeasured.

#' Fit the model to one snapshot of returns
#'
#' One conjugate regression per mode x outcome (`fit_conj_re()`), a joint residual
#' covariance Sigma across the 3M equations, and draws of every cell's final votes,
#' conditioning unobserved modes on observed ones within a precinct. Below the support
#' gate (`MIN_MODE_CELLS`, `MIN_MODE_CTY`) or `MIN_FIT_OBS` an equation stays at its prior.
#'
#' @param cells Precinct x mode cells with priors (`cells` built in section 2).
#' @param returns One row per precinct x mode: geo, mode, stage, complete, votes_dem,
#'   votes_rep, votes_oth (NA until reported).
#' @param draws Number of draws.
#' @param cross_mode Condition unobserved modes on observed ones.
#' @param Sigma_hist A fixed Sigma instead of estimating one.
#' @param level Interval level for the precinct rows.
#' @param county_obs County x mode observations for votes on no precinct, as
#'   `bucket_counts()` returns them (`county` the 3-digit geo code, `c_tot`). They bound each
#'   county's drawn turnout from below (`CTY_BOUND`). NULL leaves the bound inert. Only
#'   `county` and `c_tot` are read.
#' @return List: S_dem, S_rep, S_oth, S_2p (statewide share draws), turn (turnout draws),
#'   Sigma, resid, n_obs_cells, prior_only (equations held at prior; `all_prior` TRUE when
#'   the fit is all prior), cell_s, cell_obs, cell_votes (per-cell final votes and fitted
#'   shares, for conformal), prec (per-precinct estimates), cty_bound (NULL when the county
#'   bound never ran, 0 rows when it ran and never bound).
fit_stage = function(
  cells,
  returns,
  draws = DRAWS,
  cross_mode = USE_CROSS_MODE,
  Sigma_hist = NULL,
  level = 0.95,
  county_obs = NULL
) {
  has_complete = complete_usable(returns)
  if (!has_complete) {
    cli_alert_warning(
      "`complete` is {if ('complete' %in% names(returns)) 'present but never TRUE' else 'absent'} \\
                       -- turnout completeness falls back to COMPLETE_FRAC ({COMPLETE_FRAC}) \\
                       * prior_tau (see the COMPLETE_FRAC block)"
    )
  }
  d = cells |>
    left_join(returns, by = c("geo", "mode")) |>
    mutate(
      complete = if (has_complete) coalesce(complete, FALSE) else FALSE,
      tot2 = votes_dem + votes_rep,
      tau = tot2 + votes_oth, # ALL votes in the cell
      obs = !is.na(votes_dem) & stage >= STAGE_DONE & tau > 0 & tot2 > 0,
      # TWO observation flags, matching enightmodels::forecast_prec(obs, obs_turn):
      # a cell's SHARES become usable as soon as it reports (they are a sample of that
      # cell's electorate, weighted by counted ballots below), but its TURNOUT is only
      # usable once the cell is essentially COUNTED OUT. Real snapshots are cumulative
      # partial counts: measured on TX 2024, when an election-day cell first shows votes
      # the median cell has 5.4% of its final count in, and mail sits at 66% of final at
      # 19:00. Treating those as final understated statewide turnout by 12.5pp at 22:30
      # and biased the share estimate toward the under-counted mode's opponent.
      # `pct_reported` from the live feed is the RIGHT input here (that is what
      # nyc_enr_io.R uses, at >= 0.98); COMPLETE_FRAC is the fallback for feeds that do
      # not carry it, and it is only as good as prior_tau.
      # Preference order for the turnout flag:
      #  1. `complete` supplied in the returns -- the PRODUCTION path. nyc_enr_io.R sets
      #     it from the feed's pct_reported >= 0.98; the TX backtest sets it from whether
      #     the cell has stopped growing across that county's last two snapshots. Both
      #     are unbiased with respect to the cell's final size.
      #  2. COMPLETE_FRAC * prior_tau -- the fallback, and in production the ONLY branch
      #     that runs, because `complete` is present but never TRUE (complete_usable()).
      #     It inherits prior_tau's error, which acts as a per-mode counted-fraction gate:
      #     g >= COMPLETE_FRAC * (prior/true). The old default of 1.1 put that over 100%
      #     for two of three modes, so a correctly finished cell of those modes could
      #     never pass -- which is exactly the measured symptom here, "strict settings
      #     never pin mail" (turnout ends the night at 113%). Loose settings have the
      #     opposite failure: they treat 5%-counted eday cells as final (87%). 0.8 is the
      #     largest value leaving every mode reachable; see the COMPLETE_FRAC block.
      obs_turn = obs & if (has_complete) complete else tau >= COMPLETE_FRAC * prior_tau,
      # RAW observed values -- used to substitute counted cells in the aggregation.
      # o_tau is gated on obs_turn, the shares on obs.
      o_tau = if_else(obs_turn, tau, NA_real_),
      # CENSORING FLOOR: whatever is counted in a cell is a hard LOWER BOUND on its
      # final turnout, whether or not the cell is counted out. This needs no threshold
      # and no completeness classifier, so it carries the cases the classifier gets
      # wrong -- which in production is ALL of them, since `complete` never fires and the
      # ratio test admits a cell only once it is roughly g >= COMPLETE_FRAC * (prior/true)
      # counted.
      f_tau = if_else(obs, tau, 0),
      f_dem = if_else(obs, votes_dem, 0),
      f_rep = if_else(obs, votes_rep, 0),
      f_oth = if_else(obs, votes_oth, 0),
      o_tp = if_else(obs, votes_oth / pmax(tau, 1), NA_real_),
      o_s = if_else(obs, votes_dem / pmax(tot2, 1), NA_real_),
      # CONTINUITY-CORRECTED values -- used to FIT, so a cell with zero third-party
      # votes (very common) gives a finite logit instead of -Inf
      y_tau = NA_real_, # filled below; meaning depends on USE_COMPOSE
      y_tp = if_else(obs, logit((votes_oth + 0.5) / (tau + 1)), NA_real_),
      y_s = if_else(obs, logit((votes_dem + 0.5) / (tot2 + 1)), NA_real_)
    )

  # ---- precinct-level quantities for the total x composition parameterization ------
  # The per-mode `tau` SLOT is REINTERPRETED (see the cells block): the reference mode's
  # slot carries the precinct TOTAL on the capped logit scale, the others carry their
  # log-ratio to the reference. Doing it this way rather than restructuring the key
  # layout keeps Sigma, eps, the conditional-MVN grouping and the aggregation offsets
  # working unchanged.
  d = d |>
    mutate(
      cnt_prec = sum(if_else(obs, tau, 0), na.rm = TRUE), # counted so far, precinct
      all_done = all(obs_turn), # every mode counted out
      T_final = sum(if_else(obs_turn, tau, 0), na.rm = TRUE),
      # SECOND, independent signal that counting has stopped: how much of the precinct's
      # PRIOR expected total is already in. q_share is only as good as the completeness
      # flag; this one needs no flag at all. A precinct that has already counted its
      # whole expected vote is done whatever the flag says.
      tau_ref = {
        i <- match(REF_MODE, mode)
        if_else(rep(obs_turn[i], n()), tau[i], NA_real_)
      },
      ref_done = {
        i <- match(REF_MODE, mode)
        rep(obs_turn[i], n())
      },
      .by = geo
    )

  # ---- statewide composition tilt (see COMPOSITION_UPDATE) -------------------------
  # delta_m = vote-weighted mean of [ observed log(c_m/c_ref) - prior log(p0_m/p0_ref) ]
  # over precincts where BOTH mode m and the reference are counted out.
  comp_tilt = setNames(rep(0, length(MODES)), MODES)
  if (COMPOSITION_UPDATE) {
    for (m in setdiff(MODES, REF_MODE)) {
      z = d |> filter(mode == m, obs_turn, ref_done, !is.na(tau_ref), tau_ref > 0, tau > 0)
      if (nrow(z) >= COMP_MIN_PREC) {
        # for a NON-reference mode, y_tau_prior IS log(p0_m / p0_ref) by construction
        # (see the y_tau_prior block), so it is the prior log-ratio we need here.
        obs_lr = log(z$tau / z$tau_ref)
        comp_tilt[m] = stats::weighted.mean(obs_lr - z$y_tau_prior, z$tau + z$tau_ref)
      }
    }
    cli_inform(
      "composition tilt: {paste(sprintf('%s %+.3f', names(comp_tilt), comp_tilt), collapse = '  ')}"
    )
  }
  # ---- statewide LEVEL correction (see LEVEL_UPDATE) -------------------------------
  # lambda = vote-weighted mean of [ log(c_im / tau0_im) - delta_m ] over counted-out
  # cells. With COMPOSITION_UPDATE off, delta_m is 0 and lambda absorbs whatever
  # composition drift exists as well -- still better than no correction, but noisier.
  level_tilt = 0
  if (LEVEL_UPDATE) {
    zc = d |> filter(obs_turn, tau > 0, prior_tau > 0)
    if (nrow(zc) >= LEVEL_MIN_CELL) {
      level_tilt = stats::weighted.mean(
        log(zc$tau / zc$prior_tau) - comp_tilt[zc$mode],
        zc$tau
      )
      cli_inform(
        "level tilt: {sprintf('%+.3f', level_tilt)} \
                  ({round(100 * (exp(level_tilt) - 1), 1)}% on est_tot, \
                  from {nrow(zc)} counted-out cells)"
      )
    }
  }

  # tilted composition: p_m proportional to p0_m * exp(delta_m), reference tilt = 0
  d = d |>
    mutate(p_tilt = mode_turn_share * exp(comp_tilt[mode])) |>
    mutate(p_tilt = p_tilt / sum(p_tilt), .by = geo) |>
    mutate(
      # share of the precinct's expected votes already flagged counted-out. Uses the
      # TILTED composition when COMPOSITION_UPDATE is on, the raw prior otherwise.
      q_share = sum(if_else(obs_turn, p_tilt, 0), na.rm = TRUE),
      # counted votes over the expected total, LEVEL-CORRECTED when LEVEL_UPDATE is on.
      # exp(level_tilt) is 1 when it is off, so this is behaviour-preserving by default.
      # Must live HERE, not in the earlier mutate: level_tilt is not defined until the
      # counted-out cells have been pooled above.
      r_prior = cnt_prec / pmax(sum(prior_tau) * exp(level_tilt), 1),
      .by = geo
    ) |>
    mutate(
      is_ref = mode == REF_MODE,
      # WHICH ROWS THE TURNOUT SLOT CAN BE FITTED ON:
      #   total      -- needs every mode of the precinct counted out, else T is unknown
      #   log-ratio  -- needs this mode AND the reference mode counted out
      # `all_done |` is not redundant: q_share sums normalized shares, so an all-modes
      # precinct can land at 0.9999999999999999 and fail a bare `>= 1`.
      fit_tau = if (!USE_COMPOSE) {
        obs_turn
      } else {
        if_else(
          is_ref,
          all_done | (q_share >= MIN_MODE_SHARE & T_final > 0),
          obs_turn & ref_done & !is.na(tau_ref) & tau_ref > 0
        )
      },
      # exact sum when every mode is in; otherwise the composition supplies the rest
      T_hat = if_else(all_done, T_final, T_final / pmax(q_share, 1e-6)),
      y_tau = if (!USE_COMPOSE) {
        if_else(obs_turn, log(tau + 0.1), NA_real_)
      } else {
        if_else(
          is_ref,
          logit(pmin(pmax(T_hat / cap, 1e-4), 1 - 1e-4)),
          log(pmax(tau, 0.5) / pmax(tau_ref, 0.5))
        )
      },
      w_tau = if_else(is_ref, q_share^Q_SHARE_WT_EXP, 1),
      y_tau = if_else(fit_tau, y_tau, NA_real_),
      # The TOTAL is right-censored for any reporting precinct: T_i >= what is counted.
      # This is a far better home for censoring than the cell level, where it fixed the
      # early blow-up and broke the late one -- the total is the quantity with the good
      # prior (corr(log) 0.962) and the bound is directly observable.
      cens_tau = USE_COMPOSE & is_ref & !fit_tau & cnt_prec > 0,
      lb_tau = logit(pmin(pmax(cnt_prec / cap, 1e-4), 1 - 1e-4)),
      # P(this precinct has finished counting). Only the reference slot is ever
      # censored, so this is only read there.
      # Either signal on its own is sufficient evidence, so take the max: most modes
      # flagged done, OR the expected vote already counted. Erring HIGH here is the safe
      # direction -- a too-high p_atom under-predicts T, and the counted-votes floor
      # catches that, whereas a too-low p_atom over-predicts and the floor cannot.
      p_raw = switch(
        ATOM_SOURCE,
        both = pmax(q_share, pmin(r_prior, 1)),
        flag = q_share,
        prior = pmin(r_prior, 1)
      ),
      p_atom = pmin(pmax(p_raw, ATOM_FLOOR), ATOM_CEIL)
    )

  n_obs_cells = sum(d$obs)
  # ---- DEGRADATION LEDGER (see `prior_only` in the return value) --------------------
  # A fit that is ENTIRELY its own prior produces numbers indistinguishable from a real
  # one: same columns, same intervals, no flag. It has to be counted and carried OUT,
  # because fit_medsl() runs this function under suppressMessages() -- so every
  # cli_alert_info below is swallowed and no operator ever sees it. The ledger is read and
  # re-emitted by the caller.
  # cli_warn, not cli_alert_info, for the same reason: warnings survive suppressMessages.
  gated_modes = character(0) # held at prior by the MODE SUPPORT GATE
  prior_eqs = character(0) # equations below MIN_FIT_OBS, so sitting at prior_loc
  if (n_obs_cells < 30) {
    cli_warn(
      "only {n_obs_cells} observed cell(s) -- this fit IS its prior, not a \
              forecast. Every number it returns comes from the 2024 baseline."
    )
  }

  # --- 4a. per-(mode, outcome) conjugate fits ---
  fits = list()
  n_fit = list()
  prior_loc_mat = list()
  resid_mat = matrix(NA_real_, N_PREC, K_OUT * M)
  colnames(resid_mat) = paste(rep(MODES, each = K_OUT), OUTCOMES, sep = "_")
  geo_idx = setNames(seq_len(N_PREC), prec$geo)
  covs_for = function(out) switch(out, tau = COVS_TURN, tp = COVS_TP, s = COVS_SHARE)
  prior_n_for = function(out) switch(out, tau = PRIOR_N_TURN, tp = PRIOR_N_TP, s = PRIOR_N_SHARE)

  for (m in MODES) {
    dm = filter(d, mode == m)
    # --- MODE SUPPORT GATE ---------------------------------------------------------
    # A mode whose only reported cells come from a handful of precincts in one or two
    # counties is not evidence about that mode statewide, and treating it as such is
    # actively harmful. Measured on the real TX 2024 cadence: at 20:00, the ONLY county
    # with any election-day cells was Montague (15 precincts, rural, and still mid-count
    # -- its own count ran to 20:52). Fitting log(tau_eday) on those 15 partial cells
    # gave a residual sd of 1.15 on the log scale, and exp(mu + eps) then put the
    # statewide turnout draw at 10^11 against a truth of 3.6M.
    # Note this gates the counted-cell SUBSTITUTION too, deliberately: an early snapshot
    # of a cell is a PARTIAL count, so substituting it as if final understates turnout
    # (the standing "partial reporting is censored for turnout" caveat). Below the gate
    # the mode stays entirely at its prior.
    n_c = sum(dm$obs)
    n_cty = n_distinct(dm$county[dm$obs])
    if (n_c > 0 && (n_c < MIN_MODE_CELLS || n_cty < MIN_MODE_CTY)) {
      cli_alert_info(
        "mode {m}: only {n_c} cell(s) in {n_cty} county(ies) \\
                      -- below the support gate ({MIN_MODE_CELLS} cells / \\
                      {MIN_MODE_CTY} counties), holding the mode at its prior"
      )
      gated_modes = c(gated_modes, m)
      dm$obs = FALSE
      dm$obs_turn = FALSE
      dm$o_tau = dm$o_tp = dm$o_s = NA_real_
    }
    for (out in OUTCOMES) {
      ycol = paste0("y_", out)
      pcol = paste0("y_", out, "_prior")
      covs = covs_for(out)
      pn = prior_n_for(out)
      form = as.formula(paste(ycol, "~", paste(sprintf("`%s`", covs), collapse = " + ")))
      dm2 = mutate(dm, !!ycol := coalesce(.data[[ycol]], .data[[pcol]])) # placeholder; subset gates fitting
      # FINDING (NC 2024, 1,118 precincts): most of the share residual variance is
      # SAMPLING noise, not unpredictability -- log(resid^2) regressed on log(ballots)
      # has slope -0.72 (mail), -0.62 (early), -0.84 (eday), against -1 for pure
      # binomial. So weight the share equations by counted ballots. Var(logit p-hat)
      # ~ 1/(n p(1-p)), so precision ~ n. Turnout is observed exactly once a cell
      # reports, so its residual is model error, not sampling error -- no weights there.
      # which observation flag this equation uses: turnout needs a counted-out cell,
      # shares only need a reporting one (see the two flags built in `d` above)
      ob = if (out == "tau") dm$fit_tau else dm$obs
      # TURNOUT IS RIGHT-CENSORED. A cell that is reporting but not known counted out
      # tells us its final turnout is AT LEAST what has been counted -- true even at 5%
      # counted, which is the median election-day cell when it first reports. Feeding
      # that bound in (rather than dropping the cell, or pretending the partial count is
      # final) is what makes the completeness flag non-load-bearing, which matters
      # because in production `stage` reaches "complete" days late, uses a different
      # vocabulary per state, is downgraded by our own loader, and moves BACKWARDS.
      # Shares are NOT censored -- a partial count is a noisy SAMPLE of the cell's share,
      # not a bound on it, and that is already handled by the ballot weights plus the
      # mixture in the aggregation.
      cs = if (out == "tau") dm$cens_tau else NULL
      lb = if (out == "tau") dm$lb_tau else NULL
      pa = if (out == "tau" && USE_ATOM) dm$p_atom else NULL
      wt = if (out == "tau") {
        # default (MIN_MODE_SHARE = 1) keeps the turnout equation unweighted, as before
        if (!USE_COMPOSE || MIN_MODE_SHARE >= 1) {
          NULL
        } else {
          raw = dm$w_tau
          raw[!dm$fit_tau] = 1
          mu = if (any(dm$fit_tau)) mean(raw[dm$fit_tau]) else 1
          raw / mu
        }
      } else {
        raw = pmax(dm$tau, 1) # ballots counted in the cell
        raw[!dm$obs] = 1 # unused (not in subset), keep finite
        # normalize to mean 1 over the OBSERVED rows, so prior_n keeps its
        # "precinct-equivalents" meaning instead of being swamped by raw counts
        mu = if (any(dm$obs)) mean(raw[dm$obs]) else 1
        raw / mu
      }
      if (USE_COUNTY_RE) {
        icc = switch(out, tau = ICC_TURN, tp = ICC_TP, s = ICC_SHARE)
        lam = (1 - icc) / icc
        if (out == "tau" && ncol(CD_MAT)) {
          # district ridge weight sigma^2 / tau^2 from this fit's counted-out precincts
          rw = if (is.null(wt)) rep(1, nrow(dm)) else wt
          rs = re_shrink(
            (dm[[ycol]] - dm[[pcol]])[ob],
            rw[ob],
            prec$cd[ob],
            mu = 0,
            weights = "precision"
          )
          t2 = attr(rs, "tau2")
          lam_cd = if (is.finite(t2) && t2 > 0) attr(rs, "sigma2") / t2 else 1e8
          lam = c(rep(lam, ncol(CTY_MAT)), rep(lam_cd, ncol(CD_MAT)))
        }
        Xcov = cbind(1, as.matrix(prec[, covs, drop = FALSE]))
        fit = fit_conj_re(
          y = dm[[ycol]],
          Xcov = Xcov,
          Cty = re_mat(out),
          obs = ob,
          prior_loc = dm[[pcol]],
          prior_eff = pn / nrow(dm),
          lambda = lam,
          wts = wt,
          sd_level = if (SD_SYS_LEVEL > 0) SD_SYS_LEVEL else NULL,
          cens = if (USE_CENSOR) cs else NULL,
          y_lb = lb,
          p_atom = pa
        )
        if (is.null(fit)) cli_abort("county-RE design is rank deficient for {m}/{out}")
      } else {
        fit = lm_bayes(
          form,
          data = dm2,
          subset = ob,
          weights = wt,
          prior_yhat_loc = dm[[pcol]],
          prior_yhat_eff = pn / nrow(dm)
        )
      }
      fits[[paste(m, out, sep = "_")]] = fit
      n_fit[[paste(m, out, sep = "_")]] = sum(ob)
      pl = rep(NA_real_, N_PREC)
      pl[geo_idx[dm$geo]] = dm[[pcol]]
      prior_loc_mat[[paste(m, out, sep = "_")]] = pl
      r = rep(NA_real_, N_PREC)
      r[geo_idx[dm$geo[ob]]] = fit$residuals
      resid_mat[, paste(m, out, sep = "_")] = r
    }
  }

  # --- 4b. joint residual covariance across all 2M outcomes ---
  # Source order: historical (preferred) > pairwise from tonight > diagonal.
  # Complete-case estimation from tonight is deliberately NOT offered: with
  # sequential mode reporting there are ~0 complete cases until the count is over.
  Sigma = if (!cross_mode) {
    diag(vapply(fits, sig_of, 0)^2, K_OUT * M)
  } else if (SIGMA_SOURCE == "measured") {
    sdv = vapply(fits, sig_of, 0) # tonight's own per-equation sigmas
    C = measured_corr(colnames(resid_mat))
    diag(sdv) %*% C %*% diag(sdv)
  } else if (!is.null(Sigma_hist)) {
    stopifnot(all(dim(Sigma_hist) == K_OUT * M))
    Sigma_hist[colnames(resid_mat), colnames(resid_mat), drop = FALSE]
  } else if (SIGMA_SOURCE == "pairwise") {
    cov_pairwise(resid_mat)
  } else {
    cli_alert_info(
      "no historical Sigma supplied -- using diagonal (no cross-mode \\
                    conditioning). Set SIGMA_SOURCE or pass Sigma_hist."
    )
    diag(vapply(fits, sig_of, 0)^2, K_OUT * M)
  }
  dimnames(Sigma) = list(colnames(resid_mat), colnames(resid_mat))

  # --- impose structure on Sigma ---
  if (SIGMA_STRUCT != "full") {
    if (SIGMA_STRUCT == "diag") {
      Sigma = diag(diag(Sigma), nrow(Sigma))
    } else if (SIGMA_STRUCT == "block") {
      keep = matrix(FALSE, nrow(Sigma), ncol(Sigma))
      for (mi in seq_along(MODES)) {
        ix = K_OUT * (mi - 1) + seq_len(K_OUT)
        keep[ix, ix] = TRUE
      }
      Sigma[!keep] = 0
      Sigma = psd_repair(Sigma)
    }
    dimnames(Sigma) = list(colnames(resid_mat), colnames(resid_mat))
  }

  # --- optional override of the s<->s CROSS-MODE blocks (sensitivity analysis) ---
  if (RHO_CROSS != "calibrated") {
    s_idx = which(grepl("_s$", colnames(Sigma))) # the three share equations
    rho = if (RHO_CROSS == "draw") NA_real_ else as.numeric(RHO_CROSS)
    if (!is.na(rho)) {
      sdv = sqrt(diag(Sigma))
      for (a in s_idx) {
        for (b in s_idx) {
          if (a != b) {
            Sigma[a, b] = rho * sdv[a] * sdv[b]
          }
        }
      }
      Sigma = psd_repair(Sigma)
      dimnames(Sigma) = list(colnames(resid_mat), colnames(resid_mat))
    }
  }

  # --- 4c. draws: linear predictor + jointly-drawn residuals ---
  # mu[[k]] is draws x N_PREC for outcome k, in the column order of resid_mat
  keys = colnames(resid_mat)
  mu = lapply(keys, function(k) {
    parts = strsplit(k, "_")[[1]]
    out = parts[2]
    covs = covs_for(out)
    X = cbind(1, as.matrix(prec[, covs, drop = FALSE]))
    # An equation with (almost) no reported cells must sit at its PRIOR LOCATION, not at
    # a posterior mean drawn from a g-prior worth 3 precinct-equivalents. With no data
    # that posterior is extremely diffuse -- var of a fitted value is ~sigma^2 * p /
    # prior_n -- and for the turnout equations it is then EXPONENTIATED, which put the
    # statewide turnout draw at 1e26 the first time the obs/obs_turn split left eday and
    # mail with no counted-out cells. Holding the mode at its prior has to mean the
    # prior, not an unconstrained fit to nothing.
    if (n_fit[[k]] < MIN_FIT_OBS) {
      prior_eqs <<- c(prior_eqs, k) # <<- : mu is built inside lapply()
      return(matrix(prior_loc_mat[[k]], draws, N_PREC, byrow = TRUE))
    }
    if (USE_COUNTY_RE) {
      # design now carries the county block; counties with no reported precincts get
      # u_c shrunk to ~0 but keep the full random-effect variance in the draws
      draw_beta_re(fits[[k]], draws) %*% t(cbind(X, re_mat(sub("^.*_", "", k))))
    } else {
      draw_beta(fits[[k]], draws) %*% t(X)
    }
  })
  names(mu) = keys

  # observation mask per precinct x mode (both outcomes of a mode observed together)
  obs_mode = d |>
    select(geo, mode, obs) |>
    pivot_wider(names_from = mode, values_from = obs) |>
    arrange(match(geo, prec$geo))
  mask = as.matrix(obs_mode[, MODES, drop = FALSE]) # N_PREC x M logical
  # second mask, for TURNOUT substitution only: a reporting-but-still-counting cell
  # contributes its shares but NOT its turnout (see the two flags in `d`)
  turn_mode = d |>
    select(geo, mode, obs_turn) |>
    pivot_wider(names_from = mode, values_from = obs_turn) |>
    arrange(match(geo, prec$geo))
  mask_turn = as.matrix(turn_mode[, MODES, drop = FALSE]) # N_PREC x M logical
  # THIRD mask, for the turnout SLOT's observed-ness in the conditional-MVN block.
  # It is not the same as mask_turn once the slot holds a total / log-ratio: a non-ref
  # cell can be counted out while its REFERENCE mode is not, in which case its log-ratio
  # does not exist. Conditioning on mask_turn there reads an NA y_tau -- which is exactly
  # what the stopifnot below caught.
  fit_mode = d |>
    select(geo, mode, fit_tau) |>
    pivot_wider(names_from = mode, values_from = fit_tau) |>
    arrange(match(geo, prec$geo))
  mask_fit = as.matrix(fit_mode[, MODES, drop = FALSE]) # N_PREC x M logical
  obs_vals = d |>
    select(geo, mode, y_tau, y_tp, y_s) |>
    pivot_wider(
      names_from = mode,
      values_from = c(y_tau, y_tp, y_s),
      names_glue = "{mode}_{str_remove(.value, 'y_')}"
    ) |>
    arrange(match(geo, prec$geo))
  obs_vals = as.matrix(obs_vals[, keys, drop = FALSE]) # N_PREC x K_OUT*M
  obs_raw = d |>
    select(geo, mode, o_tau, o_tp, o_s) |>
    pivot_wider(
      names_from = mode,
      values_from = c(o_tau, o_tp, o_s),
      names_glue = "{mode}_{str_remove(.value, 'o_')}"
    ) |>
    arrange(match(geo, prec$geo))
  obs_raw = as.matrix(obs_raw[, keys, drop = FALSE]) # N_PREC x K_OUT*M
  cnt_mat = function(col) {
    m = d |>
      select(geo, mode, all_of(col)) |>
      pivot_wider(names_from = mode, values_from = all_of(col)) |>
      arrange(match(geo, prec$geo))
    m = as.matrix(m[, MODES, drop = FALSE])
    m[is.na(m)] = 0
    m
  }
  cnt_tau = cnt_mat("f_tau")
  cnt_dem = cnt_mat("f_dem")
  cnt_rep = cnt_mat("f_rep")
  cnt_oth = cnt_mat("f_oth")

  # residual draws, conditioning on observed OUTCOMES of the same precinct.
  # The pattern is per (mode, outcome), not per mode: since the obs/obs_turn split a
  # reporting-but-still-counting cell has its shares observed and its TURNOUT not, so
  # conditioning on all three outcomes of an "observed" mode would read an NA y_tau.
  obs_out = matrix(FALSE, N_PREC, K_OUT * M, dimnames = list(NULL, keys))
  for (mi in seq_along(MODES)) {
    k = K_OUT * (mi - 1)
    obs_out[, k + 1] = mask_fit[, mi] # turnout slot: where y_tau actually exists
    obs_out[, k + 2] = mask[, mi] # tp
    obs_out[, k + 3] = mask[, mi] # s
  }
  stopifnot(!any(is.na(obs_vals[obs_out])))
  eps = array(0, c(draws, N_PREC, K_OUT * M))
  pat = apply(obs_out, 1, function(z) paste(as.integer(z), collapse = ""))
  for (p in unique(pat)) {
    rows = which(pat == p)
    o = which(strsplit(p, "")[[1]] == "1")
    u = setdiff(seq_len(K_OUT * M), o)
    if (!length(u)) {
      next
    }
    # With a diagonal Sigma, Sigma_uo = 0, so conditioning is an exact no-op. Skip it
    # rather than multiplying by zeros -- this is the simplification `diag` buys.
    if (length(o) && cross_mode && any(Sigma[u, o, drop = FALSE] != 0)) {
      A = Sigma[u, o, drop = FALSE] %*% solve(Sigma[o, o, drop = FALSE])
      Scu = psd_repair(Sigma[u, u, drop = FALSE] - A %*% Sigma[o, u, drop = FALSE])
      # observed residual = observed y - its own linear predictor (draw-specific)
      for (j in seq_along(rows)) {
        i = rows[j]
        ro = vapply(o, function(k) obs_vals[i, k] - mu[[k]][, i], numeric(draws)) # draws x |o|
        shift = ro %*% t(A) # draws x |u|
        eps[, i, u] = shift + matrix(rnorm(draws * length(u)), draws) %*% chol(Scu)
      }
    } else {
      Su = psd_repair(Sigma[u, u, drop = FALSE])
      for (i in rows) {
        eps[, i, u] = matrix(rnorm(draws * length(u)), draws) %*% chol(Su)
      }
    }
  }

  # --- 4c-bis. WIDE PRIOR ON THE UNREPORTED SET -----------------------------------
  # The cells still to be counted inside a (county, mode) differ from that county's
  # already-counted ones by more than any existing term can express. Measured on TX 2024,
  # the logit gap between the remaining and the counted share within (county, mode):
  #   as-of  20:30  21:30  22:30  23:00  01:00
  #   mean   -0.10  -0.06  +0.04  +0.02  +0.13     <- MEAN ~ 0
  #   sd      0.42   0.45   0.34   0.32   0.26     <- large and stable
  #   leave-one-county-out sd 0.337-0.395 against a full-sample 0.373
  # Mean ~ 0 with large stable dispersion is a VARIANCE component, not a predictable
  # offset -- and the predictor route is separately refuted (within county x mode,
  # pre-election covariates explain 4.6% of election-day lateness, and lateness -> share
  # is t = -1.4). So: one shared mean-zero offset per (county, mode, draw) on the
  # UNREPORTED cells' share.
  #
  # Why nothing already covers it: per-cell residuals average over the unreported set at
  # 1/sqrt(k), which with k in the tens-to-hundreds is ~0.02 on the logit scale, two
  # orders below the measured 0.3. And u_c for a (county, mode) is pinned by that
  # county's REPORTED cells, so it cannot express a reported-vs-unreported difference.
  #
  # Being mean-zero, this must NOT move the point estimate -- only the width. That is the
  # sharpest check on this block, and the backtest's error curve is where it is verified.
  if (SD_UNREP > 0) {
    cf = factor(prec$county)
    ci = as.integer(cf)
    n_cty = nlevels(cf)
    for (mi in seq_along(MODES)) {
      k = K_OUT * (mi - 1)
      # Keyed on mask_turn, NOT mask: a cell that is reporting but still counting has its
      # share SUBSTITUTED only once counted out (see 4d), so the cells whose final share
      # is still uncertain are exactly the not-counted-out ones. Keying this on `mask`
      # instead left the late stages untouched -- at 92.7% reported the width moved
      # 0.59 -> 0.60pp -- because by then almost every cell has SOME report, while the
      # remaining VOTES still sat inside half-counted cells.
      unrep = !mask_turn[, mi]
      if (!any(unrep)) {
        next
      }
      z = matrix(rnorm(draws * n_cty, 0, SD_UNREP), draws, n_cty)
      eps[, unrep, k + 3] = eps[, unrep, k + 3] + z[, ci[unrep], drop = FALSE]
    }
  }

  # --- 4d. invert, substitute observed, aggregate WITHIN each draw ---
  num_d = num_r = num_o = den = numeric(draws)
  cell_s_hat = matrix(NA_real_, N_PREC, M, dimnames = list(NULL, MODES))
  # per cell (precinct x mode), means over draws: final votes by party (fin_*) and the
  # model's fitted share of all votes by party (fit_*, before any counted votes are mixed
  # in). Returned as `cell_votes` for the live wrapper's conformal interval; nothing here
  # reads them.
  cell_votes = lapply(
    setNames(nm = c("fin_dem", "fin_rep", "fin_oth", "fit_dem", "fit_rep", "fit_oth")),
    \(x) matrix(NA_real_, N_PREC, M, dimnames = list(NULL, MODES))
  )
  # PER-PRECINCT accumulators (draws x N_PREC), for precinct-level output.
  # pnum_r is accumulated so the precinct dump can report a TWO-PARTY share. Without it
  # only `dem_est` (Dem share of ALL votes) is available, which is a different quantity
  # from every headline and every `covers` check in the harnesses -- so the old dump's
  # estimate and its own `truth_dem2` column were on different scales.
  pnum_d = matrix(0, draws, N_PREC)
  pnum_r = matrix(0, draws, N_PREC)
  pden = matrix(0, draws, N_PREC)
  # ---- reconstruct turnout from the TOTAL and the COMPOSITION ---------------------
  # tau_im = T_i * p_im with sum_m p_im = 1, so sum_m tau_im = T_i EXACTLY and
  # T_i < KAPPA * n_i by construction. Phantom ballots in one mode can no longer inflate
  # the precinct total, and 184%-of-truth turnout is arithmetically impossible rather
  # than something detected afterwards.
  ref_i = match(REF_MODE, MODES)
  # Filled by the county bound below and returned, so a harness can test WHERE it bound
  # instead of parsing the log. NULL = the bound never ran (switch off, no county_obs, or no
  # bucket votes at all -- the TX/CA/FL/MI case, where bucket_counts() returns 0 rows); a
  # 0-row frame = it ran over real county observations and never bound, which is the correct
  # state for a model that is already predicting at least what has been counted.
  cty_bound_log = NULL
  if (CTY_BOUND && !is.null(county_obs) && nrow(county_obs) && !USE_COMPOSE) {
    cli_alert_warning(
      "CTY_BOUND needs USE_COMPOSE -- there is no precinct total to bound \\
                       in the per-cell parameterization. County observations ignored."
    )
  }
  if (USE_COMPOSE) {
    k_ref = K_OUT * (ref_i - 1)
    capm = matrix(KAPPA * pmax(prec$n, 1), draws, N_PREC, byrow = TRUE) # cap lives on `cells`
    T_draw = capm * invlogit(mu[[k_ref + 1]] + eps[,, k_ref + 1])
    # softmax over modes; the reference mode's log-ratio is 0 by definition
    ex = vector("list", M)
    den_p = matrix(0, draws, N_PREC)
    for (mi in seq_along(MODES)) {
      k = K_OUT * (mi - 1)
      ex[[mi]] = if (mi == ref_i) {
        matrix(1, draws, N_PREC)
      } else {
        exp(pmin(mu[[k + 1]] + eps[,, k + 1], 20))
      }
      den_p = den_p + ex[[mi]]
    }
    # THE FLOOR MUST ACT ON THE PRECINCT TOTAL, NOT PER CELL.
    # Flooring each cell at its own counted value (`pmax(tau, ct)`) destroys the
    # adding-up property this parameterization exists for: sum_m tau_im could then exceed
    # T_i again. Measured when I got this wrong: late turnout went 106.3% -> 114.4% at
    # 99.2% counted, worse than the unconstrained model it replaced.
    # Instead: floor the TOTAL at the precinct's counted total, then hand the remainder
    # only to the modes that are short of their composition-implied share, in proportion
    # to how short they are. Counted stays a hard floor per cell AND sum_m v_im = T_i.
    cnt_tot = matrix(rowSums(cnt_tau), draws, N_PREC, byrow = TRUE)
    T_draw = pmax(T_draw, cnt_tot)
    # A precinct with every mode counted out has a KNOWN total: substitute it rather than
    # flooring, or the draw contributes one-sided excess on a quantity with no
    # uncertainty left. mask_turn is the N_PREC x M obs_turn matrix built above.
    if (PIN_DONE) {
      # q_share in prec order; all_done implies q_share == 1, so this is a superset
      qs_prec = d |> distinct(geo, q_share) |> arrange(match(geo, prec$geo)) |> pull(q_share)
      rp_prec = d |> distinct(geo, r_prior) |> arrange(match(geo, prec$geo)) |> pull(r_prior)
      # all_done is exact and needs no corroboration; the q_share route does.
      done_prec = (rowSums(mask_turn) == M) |
        (qs_prec >= PIN_MIN_SHARE & rp_prec >= PIN_MIN_RPRIOR)
      if (any(done_prec)) {
        # direct assignment, not dplyr::if_else -- vctrs rejects matrices
        T_draw[, done_prec] <- cnt_tot[, done_prec, drop = FALSE]
      }
    }
    # ---- STATEWIDE TURNOUT CONSTRAINT (see STATE_TURNOUT) --------------------------
    if (!is.na(STATE_TURNOUT) && STATE_TURNOUT > 0) {
      capv = capm[1, ] # kappa*n_i, same every draw
      n_hit <- 0L
      for (dd in seq_len(draws)) {
        cnt_d <- cnt_tot[dd, ]
        # the target: a draw from the statewide prior, floored at what is already
        # counted (an unreachable target would just be eaten by the floor below)
        S_d <- max(STATE_TURNOUT * exp(stats::rnorm(1, 0, STATE_TURNOUT_SD)), sum(cnt_d))
        th_d <- qlogis(pmin(pmax(T_draw[dd, ] / capv, 1e-9), 1 - 1e-9))
        f <- function(del) sum(capv * plogis(th_d + del)) - S_d
        # bracket: +/-10 on the logit scale spans essentially (0, kappa*n) statewide
        lo <- -10
        hi <- 10
        if (f(lo) > 0 || f(hi) < 0) {
          n_hit <- n_hit + 1L
          next
        } # unreachable, leave as-is
        del <- stats::uniroot(f, c(lo, hi), tol = 1e-8)$root
        T_draw[dd, ] <- pmax(capv * plogis(th_d + del), cnt_d) # floor re-applied
      }
      if (n_hit > 0) {
        cli_alert_warning(
          "statewide constraint unreachable on {n_hit} of {draws} draws \
                           -- left unconstrained"
        )
      }
      cnt_tot_chk <- rowSums(cnt_tau)
      cli_inform(
        "statewide constraint: target {format(round(STATE_TURNOUT), big.mark = ',')} \
                  (sd {STATE_TURNOUT_SD} log), counted \
                  {format(round(cnt_tot_chk[1] * 0 + sum(cnt_tau)), big.mark = ',')}, \
                  mean drawn total {format(round(mean(rowSums(T_draw))), big.mark = ',')}"
      )
    }
    # ---- COUNTY TURNOUT LOWER BOUND (see CTY_BOUND) --------------------------------
    # ORDERED LAST, and the order is not cosmetic. The statewide block above ends with
    # pmax(capv * plogis(th_d + del), cnt_d), which re-floors at the PER-PRECINCT counted
    # votes only and knows nothing about bucket votes -- so a county bound applied before
    # it is solved away without trace. Same for PIN_DONE, which substitutes cnt_tot.
    # Running last means a county's observed total can push that draw's statewide total
    # above its own S_d. That is correct, not a violation: S_d is a draw from a PRIOR and a
    # county's counted votes are an OBSERVATION, and a prior must not override an
    # observation. (It also means the statewide block's floor, max(STATE_TURNOUT * ...,
    # sum(cnt_d)), understates what has actually been counted by up to 31% in AK and NC,
    # since cnt_d excludes bucket votes. Both want the fuller count -- raise it with
    # whoever owns that block rather than changing it here.)
    #
    # Stated as a floor on the county's REMAINDER, which is what it really is:
    #   sum_{i in c} (T_i - c_i) >= B_c,   B_c = bucket votes in c
    # so the target for the common shift is sum_{i in c} c_i + B_c and no precinct-level
    # counted total is ever re-derived from the county figure.
    if (CTY_BOUND && !is.null(county_obs) && nrow(county_obs)) {
      if (!all(c("county", "c_tot") %in% names(county_obs))) {
        cli_abort(
          "county_obs needs `county` (3-digit geo code) and `c_tot`; \\
                   got {.val {names(county_obs)}}"
        )
      }
      # KEYED ON THE GEO'S CODE, NEVER prec$county. In AK those are different partitions:
      # 40 geo codes against 30 borough names, code 037 spanning 7 boroughs, 11 boroughs
      # spanning more than one code, and no containment in either direction (measured on
      # AK_precinct.parquet). prec$county is a NAME; bucket_counts() keys on the code.
      cty_v = geo_county(prec$geo)
      capv = capm[1, ] # kappa*n_i, same every draw
      cnt_v = cnt_tot[1, ] # precinct-level counted, same every draw
      # A precinct whose geo carries no county code belongs to no county here, so it is
      # simply never in a county's free set (which == drops NA) and never receives the
      # bound. Warned rather than fatal: going inert statewide over a few malformed ids
      # would throw away the bound in every well-formed county too.
      if (anyNA(cty_v)) {
        cli_warn(
          "county bound: {sum(is.na(cty_v))} precinct geo{?s} carry no 3-digit \\
                  county code ({.val {utils::head(prec$geo[is.na(cty_v)], 5)}}) -- they are \\
                  excluded from every county total, so their counties' bounds understate"
        )
      }
      # ONE SOURCE PER COUNTY. bucket_counts() returns one row per (county, mode) by
      # construction; re-asserted because a duplicated key is the one way the same votes
      # could enter C_c twice. The other route -- summing bucket geos AND the CBS county
      # feed, which are the SAME votes -- is prevented by never reading the CBS feed here.
      key = if ("mode" %in% names(county_obs)) {
        paste0(county_obs$county, "\r", county_obs$mode)
      } else {
        county_obs$county
      }
      if (anyDuplicated(key)) {
        cli_abort(
          "county_obs has {sum(duplicated(key))} duplicated (county, mode) key{?s} \\
                   -- the same votes would be counted twice in the county total"
        )
      }
      # DOUBLE-COUNT SIGNATURE. reporting_levels.csv assigns each (county, method) to ONE
      # level, so a county reporting a mode as a pile should have no precinct-level votes in
      # that same mode. Both present means either a mixed-level county or the feed sending
      # the same votes twice, and C_c cannot tell them apart. Reported, not fatal: refusing
      # to model the state over it would be worse than modelling it with the warning shown.
      # AK trips this legitimately on eday, and it is worth knowing: AK's `NN-question`
      # provisional piles fold to eday (BUCKET_FOLD) while eday itself reports per precinct,
      # so all 40 AK districts show both. They are different votes, so the county total is
      # right -- but the same line would appear if the feed were duplicating, which is why it
      # names the pairs and the magnitudes rather than just counting them.
      # Raised as a WARNING, not a cli message: fit_stage() runs under suppressMessages() in
      # live/R/model_medsl.R, which would silently eat it.
      if ("mode" %in% names(county_obs)) {
        dbl = Filter(
          Negate(is.null),
          lapply(which(county_obs$mode %in% MODES), function(k) {
            ov = sum(
              cnt_tau[cty_v == county_obs$county[k], match(county_obs$mode[k], MODES)],
              na.rm = TRUE
            )
            if (ov > 0) {
              tibble(
                county = county_obs$county[k],
                mode = county_obs$mode[k],
                bucket = county_obs$c_tot[k],
                precinct = ov
              )
            } else {
              NULL
            }
          })
        )
        if (length(dbl)) {
          z = bind_rows(dbl)
          cli_warn(
            "county bound: {nrow(z)} (county, mode) pair{?s} have BOTH a \\
            county pile and precinct-level votes ({prettyNum(sum(z$bucket), big.mark = ',')} \\
            bucket + {prettyNum(sum(z$precinct), big.mark = ',')} precinct votes) -- if the \\
            feed is reporting the same votes at both levels the county total is inflated: \\
            {.val {paste0(z$county, '/', z$mode)}}"
          )
        }
      }
      obs_c = county_obs |> summarize(B = sum(c_tot, na.rm = TRUE), .by = county)
      # A bucket county with no modelled precinct has votes belonging to no precinct the
      # shift can move; using it as a county total would make the solve unreachable and
      # look exactly like a binding bound. Dropped loudly -- it is an id-reconciliation
      # failure, not a modelling one.
      miss = setdiff(obs_c$county, cty_v)
      if (length(miss)) {
        mv = sum(obs_c$B[obs_c$county %in% miss])
        cli_warn(
          "county bound: {length(miss)} bucket county(ies) have no precinct \\
          in `cells` ({.val {miss}}), {prettyNum(mv, big.mark = ',')} votes -- DROPPED, \\
          not used as a county total"
        )
        obs_c = obs_c |> filter(!county %in% miss)
      }
      obs_c = obs_c |> filter(B > 0)
      # EXEMPT THE PINNED SET. PIN_DONE substitutes T_draw = cnt_tot outright for a
      # finished precinct -- an EQUALITY on a quantity with no uncertainty left -- and a
      # later common logit shift would move it again, so the pinned precincts are HELD and
      # their mass is subtracted from the target instead (which is what using sum(cnt_v)
      # over the FREE set only amounts to). The gate is recomputed here rather than threaded
      # out of the PIN_DONE block above because that block has UNCOMMITTED changes in the
      # working tree (the PIN_MIN_RPRIOR corroboration, from the 2026-10-21 session); the
      # equality check below is what catches the two drifting apart once they land.
      pin_v = rep(FALSE, N_PREC)
      if (PIN_DONE) {
        qs_p = d |> distinct(geo, q_share) |> arrange(match(geo, prec$geo)) |> pull(q_share)
        # PIN_MIN_RPRIOR and `r_prior` are the corroboration half of the pin's gate and
        # are uncommitted above; defaulted so this block works whether or not they are
        # present (no r_prior => the flag-only gate, which is what the pin was before).
        rp_min = if (exists("PIN_MIN_RPRIOR")) PIN_MIN_RPRIOR else 0
        rp_p = rep(Inf, N_PREC)
        if ("r_prior" %in% names(d)) {
          rp_p = d |> distinct(geo, r_prior) |> arrange(match(geo, prec$geo)) |> pull(r_prior)
        }
        pin_v = (rowSums(mask_turn) == M) | (qs_p >= PIN_MIN_SHARE & rp_p >= rp_min)
        # A pinned precinct must be sitting exactly at its count. It will NOT be if the
        # statewide constraint ran -- that block re-shifts every precinct and only re-floors
        # at cnt_d, so it destroys the pin's equality itself -- so the check is only
        # meaningful with that block off.
        if (
          any(pin_v) &&
            !(!is.na(STATE_TURNOUT) && STATE_TURNOUT > 0) &&
            max(abs(T_draw[, pin_v, drop = FALSE] - cnt_tot[, pin_v, drop = FALSE])) > 1e-6
        ) {
          cli_warn(
            "county bound: the recomputed PIN_DONE gate does not match the \\
            pin actually applied -- the exemption is on the wrong precincts. Reconcile with \\
            the PIN_DONE block before trusting the bound."
          )
        }
      }
      # Also held: a precinct already counted at or above its own cap kappa*n_i. The common
      # shift cannot raise it (capv * plogis(.) < capv <= cnt_v, and the floor then puts it
      # back), so leaving it in the free set only makes the target unreachable.
      fixed_v = pin_v | (cnt_v >= capv)
      # ---- which counties have a KNOWN total (see CTY_SUBSTITUTE) --------------------
      sub_ok = rep(FALSE, nrow(obs_c))
      if (CTY_SUBSTITUTE && nrow(obs_c)) {
        piles_done = county_obs |> summarize(ok = all(complete), .by = county)
        # each precinct's own expected total, in prec order, for the r_prior gate
        exp_v = d |>
          summarize(E = sum(prior_tau, na.rm = TRUE), .by = geo) |>
          arrange(match(geo, prec$geo)) |>
          pull(E)
        for (ci in seq_len(nrow(obs_c))) {
          cc = obs_c$county[ci]
          j = match(cc, piles_done$county)
          if (is.na(j) || !isTRUE(piles_done$ok[j])) {
            next
          } # a pile still arriving
          idx = which(cty_v == cc)
          # Modes this county reports AT PRECINCT level = those that actually have
          # precinct-level votes. Read from the votes rather than from the mode vocabulary
          # because BUCKET_FOLD sends provisional piles into `eday`, so in AK all three of
          # MODES have a pile and a vocabulary test would pass vacuously.
          pm = which(colSums(cnt_tau[idx, , drop = FALSE]) > 0)
          # NO precinct-level votes at all => nothing has been counted here except the
          # piles. Substituting would pin the county to its piles alone.
          if (!length(pm)) {
            next
          }
          if (!all(mask_turn[idx, pm, drop = FALSE])) {
            next
          }
          E = sum(exp_v[idx])
          r = (sum(cnt_v[idx]) + obs_c$B[ci]) / max(E, 1)
          sub_ok[ci] = is.finite(r) && r >= CTY_SUB_MIN_RPRIOR
        }
      }
      n_unreach = 0L
      logs = list()
      refused = character()
      n_floorfight = 0L
      for (ci in seq_len(nrow(obs_c))) {
        idx = which(cty_v == obs_c$county[ci])
        free = idx[!fixed_v[idx]]
        targ = sum(cnt_v[free]) + obs_c$B[ci]
        if (!length(free)) {
          # REFUSE rather than pick one. Every precinct in the county is pinned or capped,
          # so there is nowhere for the bucket votes to go that does not contradict an
          # equality we have already asserted. Silently shifting a pinned precinct would
          # make the bound look as if it had worked.
          refused = c(refused, obs_c$county[ci])
          next
        }
        cf = capv[free]
        cnf = cnt_v[free]
        hit = 0L
        short_v = numeric(0)
        for (dd in seq_len(draws)) {
          th = qlogis(pmin(pmax(T_draw[dd, free] / cf, 1e-9), 1 - 1e-9))
          # `cur` is read back through the clamp rather than from T_draw directly, so that
          # f(0) = cur - targ EXACTLY. Taking it from T_draw instead leaves a precinct whose
          # ratio hit the 1e-9 clamp disagreeing with f(0) by a hair, and uniroot() then
          # errors on a bracket it was told was valid.
          cur = sum(cf * plogis(th))
          # ONE-SIDED unless the county's total is KNOWN, in which case solve in both
          # directions -- that is the whole of the PIN_DONE analogue.
          if (cur >= targ && !sub_ok[ci]) {
            next
          }
          f = function(del) sum(cf * plogis(th + del)) - targ
          lo = if (sub_ok[ci]) -50 else 0
          # f is increasing. +/-50 on the logit scale spans the clamp (logit 1e-9 = -20.7)
          # to the cap, so an unreachable target means the county total is inconsistent with
          # this county's registrants, not that the bracket is too tight.
          if (f(50) < 0 || f(lo) > 0) {
            n_unreach = n_unreach + 1L
            next
          }
          del = stats::uniroot(f, c(lo, 50), tol = 1e-8)$root
          Tn = cf * plogis(th + del)
          # The per-precinct counted floor still wins: a precinct cannot finish below its
          # own count. For a SUBSTITUTED county with del < 0 that can push the county back
          # above its known total -- the same fight the statewide block describes, where
          # "the constraint and the floor fight and the floor wins silently". It is counted
          # here rather than left silent, because a county that cannot reach its own known
          # total means the deficit sits in precincts that are individually already over it.
          if (del < 0 && any(Tn < cnf - 1e-9)) {
            n_floorfight = n_floorfight + 1L
          }
          T_draw[dd, free] <- pmax(Tn, cnf)
          hit = hit + 1L
          short_v = c(short_v, targ - cur)
        }
        if (hit) {
          logs[[length(logs) + 1L]] = tibble(
            county = obs_c$county[ci],
            n_free = length(free),
            n_fixed = length(idx) - length(free),
            bucket = obs_c$B[ci],
            cnt_prec = sum(cnt_v[idx]),
            target = targ,
            draws_bound = hit,
            mean_short = mean(short_v),
            max_short = max(short_v)
          )
        }
      }
      cty_bound_log = if (length(logs)) {
        bind_rows(logs) |> arrange(desc(mean_short))
      } else {
        tibble(
          county = character(),
          n_free = integer(),
          n_fixed = integer(),
          bucket = numeric(),
          cnt_prec = numeric(),
          target = numeric(),
          draws_bound = integer(),
          mean_short = numeric(),
          max_short = numeric()
        )
      }
      if (n_floorfight > 0) {
        cli_warn(
          "county bound: the per-precinct counted floor overrode a SUBSTITUTED \
          county total on {n_floorfight} county-draw(s) -- those counties finish above \
          their own observed total because individual precincts are already past it"
        )
      }
      if (length(refused)) {
        cli_warn(
          "county bound: REFUSED on {length(refused)} county(ies) with \\
          bucket votes and no unpinned, uncapped precinct ({.val {refused}}) -- the bound \\
          would have had to move a precinct whose total is already asserted known"
        )
      }
      if (n_unreach > 0) {
        cli_warn(
          "county bound unreachable on {n_unreach} (county, draw) pair{?s} \\
                           -- the county's observed votes exceed kappa * sum(n_i) there; \\
                           left unconstrained (suspect an id join, not real votes)"
        )
      }
      # EVERY binding county, with how much. A bound that binds on a join error looks
      # identical to one that binds on real votes, so the only way to tell them apart is to
      # read the counties and the magnitudes -- which means printing them, not a count.
      cli_inform(
        "county bound: {nrow(cty_bound_log)} of {nrow(obs_c)} county(ies) bind, \\
                  {prettyNum(round(sum(obs_c$B)), big.mark = ',')} bucket votes observed"
      )
      for (i in seq_len(nrow(cty_bound_log))) {
        z = cty_bound_log[i, ]
        cli_inform(
          "  {z$county}: +{round(z$mean_short)} votes mean \\
                    (max {round(z$max_short)}) on {z$draws_bound}/{draws} draws; \\
                    {z$n_free} free + {z$n_fixed} held precincts, counted \\
                    {prettyNum(round(z$cnt_prec), big.mark = ',')} + bucket \\
                    {prettyNum(round(z$bucket), big.mark = ',')} = target \\
                    {prettyNum(round(z$target), big.mark = ',')}"
        )
      }
    }
    rem_tot = T_draw - cnt_tot
    short = vector("list", M)
    short_sum = matrix(0, draws, N_PREC)
    for (mi in seq_along(MODES)) {
      cti = matrix(cnt_tau[, mi], draws, N_PREC, byrow = TRUE)
      short[[mi]] = pmax(T_draw * (ex[[mi]] / den_p) - cti, 0)
      short_sum = short_sum + short[[mi]]
    }
    # degenerate case: every mode already at or above its share -- fall back to the
    # composition itself so the remainder still goes somewhere proportionate
    flat = short_sum <= 1e-9
    for (mi in seq_along(MODES)) {
      short[[mi]][flat] = (ex[[mi]] / den_p)[flat]
      short_sum[flat] = 1
    }
  }
  for (mi in seq_along(MODES)) {
    k = K_OUT * (mi - 1)
    tau = if (USE_COMPOSE) {
      NULL
    } else {
      tt = exp(mu[[k + 1]] + eps[,, k + 1]) - 0.1
      tt[tt < 0] = 0
      tt
    }
    tp = invlogit(mu[[k + 2]] + eps[,, k + 2])
    s = invlogit(mu[[k + 3]] + eps[,, k + 3])
    # ---- PARTIAL COUNTS AS A MIXTURE, not substitute-or-predict -------------------
    # A cell's final votes = what is already counted (KNOWN exactly) + what remains
    # (predicted). So:
    #   tau   >= counted           (censoring floor; counted is a hard lower bound)
    #   rem    = tau - counted     (ballots still to count in this cell)
    #   dem    = counted_dem + rem*(1-tp)*s
    # This replaces the binary rule it grew out of, which substituted a reporting cell's
    # share as EXACT once flagged complete and otherwise PREDICTED it, discarding the
    # observed share entirely. That was wrong in both directions: a 5%-counted cell got
    # its partial share asserted as final, and a 99%-counted-but-unflagged cell had
    # near-perfect information thrown away -- which is why the interval at 100% counted
    # came out 4.81pp wide instead of collapsing.
    # The mixture needs NO completeness classifier: a counted-out cell has rem ~ 0 so its
    # share is pinned automatically, and a barely-started cell is mostly predicted. That
    # removes the dependence on the plateau proxy (and on a feed `pct_reported`) for the
    # aggregate, which is a strictly better place to be.
    ct = matrix(cnt_tau[, mi], draws, N_PREC, byrow = TRUE)
    cd = matrix(cnt_dem[, mi], draws, N_PREC, byrow = TRUE)
    cr = matrix(cnt_rep[, mi], draws, N_PREC, byrow = TRUE)
    co = matrix(cnt_oth[, mi], draws, N_PREC, byrow = TRUE)
    if (USE_COMPOSE) {
      rem = rem_tot * (short[[mi]] / short_sum) # this mode's share of what is left
      tau = ct + rem # sums to T_i across modes
    } else {
      tau = pmax(tau, ct)
      rem = tau - ct
    }
    # Substitute a cell's values as EXACT only once it is counted out. A half-counted
    # cell still informs the FIT -- its share enters weighted by counted ballots -- but
    # its final share is not yet known, so pinning it at the partial share asserts
    # certainty the data does not support. Measured on TX 2024: when an election-day cell
    # first reports, the median cell has 5.4% of its final count in; at 23:00 the still-
    # uncounted votes were +7.76pp more Democratic than the counted ones. This is the
    # same partial-reporting defect as on the turnout side, in the share equation.
    # (enightmodels::forecast_prec substitutes shares on its `obs` flag, but nyc_enr_io.R
    # sets that flag to `total > turn_early * 1.2` -- a cell with a substantial count --
    # not "any votes at all", which is what this reproduces.)
    rem_two = rem * (1 - tp)
    v_dem = cd + rem_two * s
    v_rep = cr + rem_two * (1 - s)
    v_oth = co + rem * tp
    cell_s_hat[, mi] = colMeans(v_dem / pmax(v_dem + v_rep, 1e-9))
    cell_votes$fin_dem[, mi] = colMeans(v_dem)
    cell_votes$fin_rep[, mi] = colMeans(v_rep)
    cell_votes$fin_oth[, mi] = colMeans(v_oth)
    cell_votes$fit_dem[, mi] = colMeans((1 - tp) * s)
    cell_votes$fit_rep[, mi] = colMeans((1 - tp) * (1 - s))
    cell_votes$fit_oth[, mi] = colMeans(tp)
    num_d = num_d + rowSums(v_dem)
    num_r = num_r + rowSums(v_rep)
    num_o = num_o + rowSums(v_oth)
    den = den + rowSums(v_dem + v_rep + v_oth)
    pnum_d = pnum_d + v_dem
    pnum_r = pnum_r + v_rep
    pden = pden + v_dem + v_rep + v_oth
  }
  # precinct-level Dem share of ALL votes, summarized across draws
  pshare = pnum_d / pmax(pden, 1e-9)
  # q_share per precinct, in prec order -- the pin's gate, so downstream diagnostics can
  # measure how often it fires instead of inferring it from the stricter all_done counts
  qs_out = d |> distinct(geo, q_share) |> arrange(match(geo, prec$geo)) |> pull(q_share)
  # counted votes per precinct, in prec order. Exported rather than left in `d` because
  # every harness was otherwise re-deriving its own `c_tot` from its own returns -- which is
  # how the `n_done_modes` defect got in (test_sim_night.R summed an ambient vector and read
  # 10,956 on a 227-registrant precinct). One source, computed once.
  cnt_out = d |> distinct(geo, cnt_prec) |> arrange(match(geo, prec$geo)) |> pull(cnt_prec)
  # TWO-PARTY share per precinct, the scale every headline and `covers` check uses.
  p2 = pnum_d / pmax(pnum_d + pnum_r, 1e-9)
  # interval ends from `level` rather than hardcoded 0.025/0.975 -- the rest of the codebase
  # threads a level (output.R, cred()), and a dump whose interval silently ignored it could
  # not be compared against anything else.
  qlo = 0.5 * (1 - level)
  qhi = 0.5 * (1 + level)
  prec_out = tibble(
    geo = prec$geo,
    county = prec$county,
    q_share = qs_out,
    n_obs_modes = rowSums(mask),
    cnt_prec = cnt_out,
    turn_est = colMeans(pden),
    # turnout had a point estimate and no interval while share had all four, even though
    # pden was already in hand.
    # EXPECTED, do not "fix": turn_est can sit slightly ABOVE turn_hi. The counted-votes
    # floor (T_draw = pmax(T_draw, cnt_tot)) puts an ATOM at cnt_prec holding more than
    # 97.5% of the draws, so both quantiles land exactly on the floor while the mean is
    # pulled up by the thin tail above it. Measured on tx_base at 22:00: a handful of
    # precincts, max gap 0.47 votes. turn_est stays the MEAN so it matches how the race
    # level reports est_turnout; consistency beats a bracketing guarantee that the atom
    # makes impossible.
    turn_sd = apply(pden, 2, sd),
    turn_lo = apply(pden, 2, quantile, qlo),
    turn_hi = apply(pden, 2, quantile, qhi),
    dem_est = colMeans(pshare), # Dem share of ALL votes
    dem_sd = apply(pshare, 2, sd),
    dem_lo = apply(pshare, 2, quantile, qlo),
    dem_hi = apply(pshare, 2, quantile, qhi),
    est_dem2 = colMeans(p2), # Dem share of the TWO-PARTY vote
    dem2_lo = apply(p2, 2, quantile, qlo),
    dem2_hi = apply(p2, 2, quantile, qhi),
    p_win = colMeans(p2 > 0.5), # P(Dem leads two-party) in this precinct
    level = level
  )
  den = pmax(den, 1e-9)
  list(
    # HEADLINE: shares of ALL votes. These sum to 1 by construction.
    S_dem = num_d / den,
    S_rep = num_r / den,
    S_oth = num_o / den,
    # two-party share, which is what decides the race
    S_2p = num_d / pmax(num_d + num_r, 1e-9),
    turn = den,
    Sigma = Sigma,
    resid = resid_mat,
    n_obs_cells = n_obs_cells,
    # How much of this fit is its own prior. `all_prior` TRUE means the output carries no
    # information from tonight at all, and the caller must not present it as a forecast.
    prior_only = list(
      n_obs_cells = n_obs_cells,
      n_cells = nrow(d),
      eq_total = length(keys),
      eq_prior = length(unique(prior_eqs)),
      eq_prior_names = sort(unique(prior_eqs)),
      gated_modes = sort(unique(gated_modes)),
      all_prior = n_obs_cells < 30 || length(unique(prior_eqs)) == length(keys)
    ),
    cell_s = cell_s_hat,
    cell_obs = mask,
    cell_votes = cell_votes,
    prec = prec_out,
    # NULL when the county bound never ran, a 0-row frame when it ran and never bound.
    # The distinction is what the TX/CA/FL/MI inertness check needs: those states have zero
    # bucket vote mass, so "ran and never bound" is the expected state there, and a count
    # alone could not tell it from "was never wired in".
    cty_bound = cty_bound_log
  )
}

# ---- 4. prior-only draws (reference measure for conf_seq) ----------------------
#' Prior predictive draws of the race totals
#'
#' As if nothing were counted. `cells` should carry the model's own priors, not priors
#' re-centred on tonight's returns (`recentre_prior()`): a reference measure that has seen
#' the data is not a reference measure. Used for conf_seq.
#'
#' @param cells Cells with prior_tau, prior_s, prior_tp.
#' @param draws Number of draws.
#' @param sd_sys_s,sd_sys_tau Statewide (shared) prior sd of the share and log turnout.
#' @param sd_idio_s,sd_idio_tau Precinct-level prior sd.
#' @return Tibble, one row per draw: dem, rep, oth, turnout (all votes).
prior_totals = function(
  cells,
  draws = DRAWS,
  sd_sys_s = 0.10,
  sd_sys_tau = 0.12,
  sd_idio_s = 0.40,
  sd_idio_tau = 0.30
) {
  # CRITICAL: the systematic terms are what make this a usable reference measure.
  # Idiosyncratic per-cell noise averages out over ~13.5k cells (1/sqrt(n)), so a
  # prior built from it alone is ABSURDLY tight at the statewide level -- tighter
  # than the posterior, which makes conf_seq's quadratic unbounded and returns
  # (-Inf, Inf). The real prior uncertainty is about the statewide SWING, which is
  # perfectly correlated across precincts and does not average away.
  sys_s = rnorm(draws, 0, sd_sys_s)
  sys_tau = rnorm(draws, 0, sd_sys_tau)
  sys_tp = rnorm(draws, 0, sd_sys_s)
  dem = rep = oth = numeric(draws)
  for (m in MODES) {
    cm = filter(cells, mode == m)
    nc = nrow(cm)
    tau = matrix(cm$prior_tau, draws, nc, byrow = TRUE) *
      exp(sys_tau + matrix(rnorm(draws * nc, 0, sd_idio_tau), draws))
    s = invlogit(
      matrix(logit(cm$prior_s), draws, nc, byrow = TRUE) +
        sys_s +
        matrix(rnorm(draws * nc, 0, sd_idio_s), draws)
    )
    tp = invlogit(
      matrix(logit(cm$prior_tp), draws, nc, byrow = TRUE) +
        sys_tp +
        matrix(rnorm(draws * nc, 0, sd_idio_s), draws)
    )
    dem = dem + rowSums(tau * (1 - tp) * s)
    rep = rep + rowSums(tau * (1 - tp) * (1 - s))
    oth = oth + rowSums(tau * tp)
  }
  tibble::tibble(dem = dem, rep = rep, oth = oth, turnout = dem + rep + oth)
}

#' Prior predictive draws of the Dem share of all votes
#'
#' The same draws as `prior_totals()`, summarised.
#'
#' @param cells,draws,... As `prior_totals()`.
#' @return Numeric vector of draws.
prior_draws = function(cells, draws = DRAWS, ...) {
  p = prior_totals(cells, draws = draws, ...)
  p$dem / pmax(p$turnout, 1e-9)
}
