# ========================================
## TX Governor 2026 -- TRIAL precinct model: turnout x vote share, by vote mode
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
##
## Input:  {PATH_DROPBOX}/26_general/input_data/model_inputs/TX_precinct.parquet
##         (was TX_prec_covs.parquet; PATH_COVS overrides the path)
##         Built by scripts/model/prep_tx_model_inputs.R, which supersedes the removed
##         prep_tx_covs.R / check_live_ids.R / inspect_l2.R (Bridget, 2026-10-19).
##         NOTE two changes there that affect joins and covariates:
##           - `geo` drops LEADING ZEROS (`tx_257_1`, not `tx_257_0001`), because live
##             feeds write ids without them -- Harris matched 15.8% before stripping and
##             99.6% after.
##           - `n` is now the YouGov frame's registrant count (complete for every
##             precinct); L2's is carried alongside as `l2_n`, and L2 gaps are filled
##             with the county registrant-weighted mean flagged `l2_imputed`.
##         Schema (long by party):
##           geo, county, geostratum, stratum_demo, cd_main, cd_main_share, n_cd, n,
##           turnout20, turnout22, turnout24, turnoutPred, age, male,
##           pred.whi, pred.bla, pred.his, pred.asi, pred.oth,
##           pr_harris, pr_trump, vote_imputed, <l2 cols>, dem2,
##           party ("dem"/"rep"), est, method ("all")
##
## !! THE COVARIATE INPUT HAS NO VOTE-MODE SPLIT (method == "all" only). !!
##    Mode priors are therefore built in the MODE PRIORS config block below. Status
##    as of 2026-10-18:
##      MODE_TURN_SHARE   -- SOLVED. 2022 midterm split read from the L2 precinct file
##        (l2/tx_20261011_precincts.parquet, vote22_{mail,early,eday}); available PER
##        PRECINCT, joinable via crosswalks/tx.csv `precinct_l2`. Constant below is the
##        statewide fallback.
##      MODE_SHARE_OFFSET -- STILL 0. L2 records HOW people voted, never FOR WHOM.
##        Reachable by EI (see that block) or from real mode x party results.
##      Sigma cross-mode share blocks -- STILL BLOCKED on real mode x party results.
##
## Output: console summary + optional parquet of draws-level statewide summaries.
##
## Run from the cbs-enr folder:
##   Rscript scripts/model/tx_gov_trial.R            # synthetic returns, all stages
##   Rscript scripts/model/tx_gov_trial.R 0.35       # single stage at 35% reported
# ========================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(cli)
  library(nanoparquet)
  library(enightmodels)   # lm_bayes, log_s, cred_int, conf_seq, logit_shift
})
# SEED overridable so coverage can be tested across replications
# Fitting primitives live in model_core.R so the backtests cannot drift from the model.
# Run from the cbs-enr folder, or set MODEL_CORE to an explicit path.
source(Sys.getenv("MODEL_CORE", unset = "scripts/model/model_core.R"))

set.seed(as.integer(Sys.getenv("SEED", unset = "20260303")))

# ---- CONFIG ---------------------------------------------------------------------
STATE          = "TX"
ELECTION_FOLDER = "26_general"
PATH_DROPBOX   = Sys.getenv(
  "PATH_DROPBOX",
  unset = "~/Library/CloudStorage/Dropbox-MIT/Young-Kyung Kim/Contracting/2024 Election Results"
)
# PATH_COVS override lets a backtest driver swap in a panel whose covariates are all
# pre-treatment for its target election (see tx24_backtest.R).
path_covs      = Sys.getenv(
  "PATH_COVS",
  unset = file.path(PATH_DROPBOX, ELECTION_FOLDER, "input_data",
                    paste0(STATE, "_prec_covs.parquet")))
# Historical precinct x mode x party returns. Supplies ALL THREE mode-dependent
# inputs: the turnout split, the partisan offset, and Sigma. See the MODE PRIORS
# block for the required schema. Drop the file here and everything below uses it.
path_mode_hist = Sys.getenv(
  "PATH_MODE_HIST",                       # override for testing
  unset = file.path(PATH_DROPBOX, ELECTION_FOLDER, "input_data",
                    paste0(STATE, "_mode_history.parquet"))
)

DRAWS          = 500      # posterior draws
PRIOR_N_TURN   = 3.0      # prior strength, in PRECINCT-EQUIVALENTS (not per-cell)
PRIOR_N_SHARE  = 3.0      # keep FIXED across the night; relative weight decays on its own
STAGE_DONE     = 2L       # a mode cell counts as observed at this reporting stage
# Support gate: the minimum reported cells, and reporting counties, before a MODE's
# returns are used at all. Below it the mode stays at its prior. See the gate in
# fit_stage() for the TX 2024 measurement that motivates it.
# Lowered from the 60/3 stopgap once the obs/obs_turn split landed: the turnout blow-up
# it existed to prevent is now handled properly, so the gate only catches degenerate
# cases rather than discarding legitimate share information.
# Below this many reported cells an equation is held at its prior LOCATION outright
# (see the prior-only branch in fit_stage's mu construction).
MIN_FIT_OBS    = as.integer(Sys.getenv("MIN_FIT_OBS", unset = "30"))
MIN_MODE_CELLS = as.integer(Sys.getenv("MIN_MODE_CELLS", unset = "10"))
MIN_MODE_CTY   = as.integer(Sys.getenv("MIN_MODE_CTY",   unset = "1"))
# Fallback completeness test for TURNOUT when the feed carries no `pct_reported`:
# a cell's turnout counts as observed once counted >= COMPLETE_FRAC * prior_tau.
# It is only as good as prior_tau, which on TX 2024 is off by ~1.7x for eday and ~0.5x
# for mail -- so prefer a real pct_reported field wherever the feed has one.
COMPLETE_FRAC  = as.numeric(Sys.getenv("COMPLETE_FRAC", unset = "1.1"))
# Wide prior on the unreported set's share offset, in logit units, shared within a
# (county, mode). Measured on TX 2024 at 0.26-0.45 raw / 0.15-0.30 vote-weighted; 0.25 is
# the starting point. SD_UNREP=0 disables it (ablation). See the block in fit_stage.
SD_UNREP       = as.numeric(Sys.getenv("SD_UNREP", unset = "0.25"))
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
USE_CENSOR     = Sys.getenv("USE_CENSOR", unset = "0") == "1"
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
USE_COMPOSE    = Sys.getenv("USE_COMPOSE", unset = "1") == "1"
KAPPA          = as.numeric(Sys.getenv("KAPPA", unset = "1.25"))
REF_MODE       = Sys.getenv("REF_MODE", unset = "early")
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
SD_SYS_LEVEL   = as.numeric(Sys.getenv("SD_SYS_LEVEL", unset = "0"))
LEVEL          = 0.95
USE_CROSS_MODE = TRUE     # FALSE => independent residuals (ablation, for comparison)

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
RHO_TAU_S   = c(mail = 0.204, early = 0.052, eday = -0.140)   # within mode
RHO_S_CROSS = if (Sys.getenv("RHO_ME_CORRECTED") == "1") 0.40 else 0.232  # mail<->early
measured_corr = function(keys) {
  K = length(keys); C = diag(K); dimnames(C) = list(keys, keys)
  idx = function(m, o) which(keys == paste(m, o, sep = "_"))
  for (m in MODES) {                                   # within-mode tau<->s
    a = idx(m, "tau"); b = idx(m, "s")
    C[a, b] = C[b, a] = RHO_TAU_S[[m]]
  }
  a = idx("mail", "s"); b = idx("early", "s")          # the one real cross-mode pair
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
SIGMA_STRUCT = Sys.getenv("SIGMA_STRUCT", unset = "full")   # full | block | diag

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
RE_GROUPING = Sys.getenv("RE_GROUPING", unset = "county")   # county | cory | geostratum
ICC_TURN  = as.numeric(Sys.getenv("ICC_TURN",  unset = "0.200"))   # TX 2022, L2
ICC_SHARE = as.numeric(Sys.getenv("ICC_SHARE", unset = "0.269"))   # TX 2022, MEDSL
ICC_TP    = as.numeric(Sys.getenv("ICC_TP",    unset = "0.269"))   # unmeasured; share proxy
path_sigma   = file.path(PATH_DROPBOX, ELECTION_FOLDER, "input_data",
                         paste0(STATE, "_gov_sigma.rds"))

# Covariates. Deliberately SMALL: with hundreds of races you cannot hand-curate.
# Same set in both equations for now -> Sigma does not change beta (SUR == OLS when
# designs match); it only fixes the joint predictive distribution, which is the point.
# To also gain SUR efficiency, give the two equations NON-NESTED sets (see TODO below).
# Overridable from the environment (comma-separated) so a backtest can drop a covariate
# that would be look-ahead for ITS target year -- e.g. turnout24 for a 2024 backtest.
cov_env = function(var, default) {
  v = Sys.getenv(var, unset = "")
  if (!nzchar(v)) return(default)
  trimws(strsplit(v, ",", fixed = TRUE)[[1]])
}
COVS_TURN  = cov_env("COVS_TURN",
             c("log_n", "turnout22", "turnout24", "age", "male",
               "pred.bla", "pred.his", "dem2"))
COVS_SHARE = cov_env("COVS_SHARE",
             c("log_n", "turnout22", "turnout24", "age", "male",
               "pred.bla", "pred.his", "dem2"))
# Third-party share is a weak, stable signal -- keep its design small or it overfits.
COVS_TP    = cov_env("COVS_TP", c("log_n", "age", "male", "dem2", "tp_prior_raw"))
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
K_OUT    = length(OUTCOMES)
PRIOR_N_TP   = 2.0        # prior strength for the third-party equation
TP_SHRINK_N  = 2000       # registrants at which the precinct tp signal gets half weight
TP_RANGE     = c(0.001, 0.15)   # clamp for the third-party prior

# ---- MODE PRIORS ----------------------------------------------------------------
# Three canonical modes (keys from enightmodels::VOTE_METHODS).
MODES = c("mail", "early", "eday")
#
# If `path_mode_hist` exists, the turnout split, the partisan offset and Sigma are all
# DERIVED from it (derive_mode_priors() below) and the constants here are ignored.
#
# REQUIRED SCHEMA for {STATE}_mode_history.parquet -- one row per precinct x mode x
# party, FINAL CANVASS numbers (not an election-night snapshot):
#     geo      chr  live-scraper format: tx_{county_cbs 3-digit}_{precinct_id}, with
#                   precinct_id trimmed, upper-cased and LEADING ZEROS DROPPED
#                   (tx_257_1, not tx_257_0001) -- see CONTRACT.md §0 and
#                   prep_tx_model_inputs.R
#     election chr  e.g. "2022_general", "2024_general"
#     method   chr  one of mail / early / eday  (map provisional+limited explicitly;
#                   say whether they are folded into eday or dropped)
#     party    chr  "dem" / "rep"
#     votes    dbl  final votes for that party in that precinct and mode
# Optional but valuable: `contest` (so we can prefer governor/senate over president),
# and `boundary_vintage` (which precinct map the geo ids are on).
#
# Fallback constants, used only when the file is absent.
#
# MODE_TURN_SHARE: the 2022 MIDTERM split, read DIRECTLY from the L2 precinct file
# (26_general/input_data/l2/tx_20261011_precincts.parquet, refreshed 2026-10-16), which
# carries vote{16,18,20,22,24}_{mail,early,eday} -- per-election, per-mode voter counts
# at precinct level. 9,591 precinct rows, vote22 populated in 99.7% of them.
#
#   election         mail    early    eday       total
#   2018 midterm     4.8%    67.9%   27.3%   6,882,402
#   2020 pres        7.2%    79.5%   13.3%   9,802,123
#   2022 midterm     4.2%    63.8%   32.0%   7,515,288   <- used here
#   2024 pres        3.5%    76.4%   20.2%  10,963,552
#
# MIDTERMS RUN ~27-32% ELECTION-DAY, PRESIDENTIALS 13-20%. 2026 is a midterm, so 2022
# is the analogue. Do NOT use the coalesced "most recent general" composite in
# yougov_precinct_frame_method.csv (72.9/21.6/5.5): it is dominated by 2024 and
# under-weights election day by ~10pp. Election day is both the mode with the most
# distinctive partisan lean and the one that reports last, so that error feeds straight
# into red-mirage bias.
#
# THESE ARE AVAILABLE PER PRECINCT -- the statewide constants below are only a fallback.
# BUILT ARTIFACT (Bridget, 2026-10-19): TX_precinct_mode_history.parquet covers the
# 2016-2024 generals for 5,537 precincts with an L2 match; in the 32 scope counties the
# 2022 split is mail 4.4 / early 65.0 / eday 30.5, and the mode columns cover 99.8% of
# 2022 voters. Point PATH_MODE_HIST at it rather than re-deriving the join.
# Join path, if rebuilding: L2 `precinct_cbs` -> crosswalks/tx.csv `precinct_l2`
# (90.3% of precincts, 98.2% of REGISTRANTS -- the weighted figure is the relevant one)
# -> `precinct_yougov` -> the covariate frame. See derive_mode_priors() for the shape.
# DO NOT backfill crosswalk gaps by normalizing ids: for 92 TX precincts a normalized
# match exists but the pairs are different places sharing a number, with registrant
# counts differing by a median 35%. The blanks are correct (CONTRACT.md §0).
#
# Caveat: still a stock of CURRENT registrants who voted in 2022 by each mode, so
# survivorship-biased (2022 voters since died/moved are absent). But it is the right
# election and the right modes, which the composite was not.
MODE_TURN_SHARE   = c(mail = 0.042, early = 0.638, eday = 0.320)
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
  mail  = c(a = 0.0, b = 0.0),
  early = c(a = 0.0, b = 0.0),
  eday  = c(a = 0.0, b = 0.0)
)
NC_DELTA = rbind(                      # measured above; borrow only deliberately
  mail  = c(a =  0.660, b = -0.297),
  early = c(a = -0.029, b =  0.041),
  eday  = c(a = -0.025, b = -0.089)
)
if (Sys.getenv("USE_NC_DELTA") == "1") {
  MODE_SHARE_OFFSET = NC_DELTA
  cli_alert_warning("Using NC-measured mode offsets for TX -- sensitivity run only.")
}
# Where delta_m comes from when a mode-history FILE is present (it overrides the
# constants above). "history" derives it from the file, "zero" and "nc" keep the file's
# turnout split but replace its partisan offset. See derive_mode_priors().
MODE_OFFSET_SOURCE = Sys.getenv("MODE_OFFSET_SOURCE", unset = "history")
#
# Which past election to lean on. 2026 is a MIDTERM, so 2022 is the better analogue
# for mode composition and for the mode partisan differential; 2024 has better
# coverage and a fresher precinct partisan lean. Preference order, first match wins.
# Env-overridable (comma-separated) so a backtest can name an analogue appropriate to
# ITS target year -- 2020 for a presidential target, since TX mode composition tracks
# turnout type rather than calendar.
MODE_HIST_PREFER = cov_env("MODE_HIST_PREFER", c("2022_general", "2024_general"))
# --------------------------------------------------------------------------------



stopifnot(
  "MODE_TURN_SHARE must sum to 1"   = abs(sum(MODE_TURN_SHARE) - 1) < 1e-9,
  "MODE_TURN_SHARE must be named for MODES" = setequal(names(MODE_TURN_SHARE), MODES),
  "MODE_SHARE_OFFSET must be a MODES x (a,b) matrix" =
    setequal(rownames(MODE_SHARE_OFFSET), MODES) &&
    identical(colnames(MODE_SHARE_OFFSET), c("a","b"))
)

# ---- 1. load precinct covariates ------------------------------------------------
# Falls back to a synthetic frame with the documented schema so this script is
# runnable before the real file lands. Synthetic mode is loudly labelled.
load_covs = function(path) {
  if (file.exists(path)) {
    cli_alert_success("Reading {.path {path}}")
    d = read_parquet(path)
    # Structural columns, plus whatever the ACTIVE covariate lists name. Checking the
    # active set rather than a fixed list is what lets a backtest supply turnout20 in
    # place of turnout24 without carrying a dead column.
    req = unique(c("geo", "county", "geostratum", "stratum_demo", "n", "party", "est",
                   "method", setdiff(c(COVS_TURN, COVS_SHARE, COVS_TP), COVS_DERIVED),
                   "dem2"))
    miss = setdiff(req, names(d))
    if (length(miss)) cli_abort("Input is missing required columns: {miss}")
    if (!all(d$method == "all")) {
      cli_alert_info("Input already has a mode split ({unique(d$method)}) -- \\
                      replace the MODE PRIORS block with those columns.")
    }
    return(d)
  }
  cli_alert_warning("{.path {path}} not found -- generating SYNTHETIC covariates. \\
                     Results are a plumbing test only, not a forecast.")
  np = 4500
  tibble(
    geo          = sprintf("tx_%03d_%04d", rep(1:150, length.out = np), 1:np),
    county       = sprintf("county_%03d", rep(1:150, length.out = np)),
    geostratum   = paste0("TX-", rep(1:6, length.out = np)),
    stratum_demo = sample(c("white_noncol", "white_col", "hisp", "black", "mixed"), np, TRUE),
    n            = round(runif(np, 300, 4000)),
    turnout22    = runif(np, 0.25, 0.65),
    turnout24    = runif(np, 0.40, 0.80),
    turnoutPred  = runif(np, 0.35, 0.70),
    age          = runif(np, 35, 60),
    male         = runif(np, 0.42, 0.55),
    `pred.bla`   = runif(np, 0, 0.5),
    `pred.his`   = runif(np, 0, 0.7)
  ) |>
    mutate(
      dem2 = invlogit(rnorm(np, 0, 0.9)),
      # third-party residual, matched to the real frame: n-weighted ~1.6%, right-skewed
      pr_other = pmin(pmax(rgamma(np, shape = 1.6, rate = 95), 0), 0.25)
    ) |>
    mutate(pr_harris = dem2 * (1 - pr_other), pr_trump = (1 - dem2) * (1 - pr_other)) |>
    select(-pr_other) |>
    mutate(est_tot = turnoutPred * n, est_dem = est_tot * dem2, est_rep = est_tot * (1 - dem2)) |>
    select(-est_tot) |>
    pivot_longer(c(est_dem, est_rep), names_to = "party", values_to = "est",
                 names_prefix = "est_") |>
    mutate(method = "all")
}

covs_long = load_covs(path_covs)

# ---- 1b. mode history -> per-precinct mode priors -------------------------------
# Turns the colleague's precinct x mode x party table into:
#   (a) per-precinct mode turnout shares   (replaces MODE_TURN_SHARE)
#   (b) per-mode logit share offsets        (replaces MODE_SHARE_OFFSET)
# Both are shrunk toward the county / statewide value so thin precincts behave.
load_mode_history = function(path) {
  if (!file.exists(path)) return(NULL)
  d = read_parquet(path) |> as_tibble()
  req = c("geo", "election", "method", "party", "votes")
  miss = setdiff(req, names(d))
  if (length(miss)) cli_abort("{.path {path}} missing columns: {miss}")

  bad_m = setdiff(unique(d$method), MODES)
  if (length(bad_m)) cli_abort("unexpected `method` values: {bad_m}. Map them to \\
                                {MODES} (and state where provisional went).")
  bad_p = setdiff(unique(d$party), c("dem", "rep"))
  if (length(bad_p)) cli_alert_warning("dropping non-two-party rows: {bad_p}")
  d = filter(d, party %in% c("dem", "rep"))

  elec = MODE_HIST_PREFER[MODE_HIST_PREFER %in% unique(d$election)]
  if (!length(elec)) cli_abort("no election in {MODE_HIST_PREFER}; have \\
                                {unique(d$election)}")
  if ("contest" %in% names(d)) {
    pref = c("governor", "senate", "president")
    hit = pref[pref %in% tolower(unique(d$contest))]
    if (length(hit)) d = filter(d, tolower(contest) == hit[1])
    cli_inform("mode history: using contest {.val {if (length(hit)) hit[1] else 'all'}}")
  }
  cli_alert_success("Mode history: {.path {basename(path)}}, using {.val {elec[1]}}")
  filter(d, election == elec[1]) |>
    summarize(votes = sum(votes), .by = c(geo, method, party))
}

derive_mode_priors = function(hist, prec, k_turn = 40, k_share = 60) {
  if (is.null(hist)) return(NULL)
  cty = prec |> select(geo, county) |> mutate(geo = as.character(geo))
  h = hist |>
    pivot_wider(names_from = party, values_from = votes, values_fill = 0) |>
    inner_join(cty, by = "geo")
  if (!nrow(h)) cli_abort("mode history did not join to any precinct `geo`. \\
                            Check the id format (tx_{{county_cbs}}_{{precinct}}).")
  cli_inform("mode history joined {n_distinct(h$geo)} of {nrow(prec)} precincts")

  # (a) turnout split: precinct mode share, shrunk to its county's mode share
  tot = h |> mutate(v = dem + rep) |> summarize(v = sum(v), .by = c(geo, county, method))
  cty_sh = tot |> summarize(v = sum(v), .by = c(county, method)) |>
    mutate(cty_share = v / sum(v), .by = county) |> select(county, method, cty_share)
  sw_sh = tot |> summarize(v = sum(v), .by = method) |> mutate(sw = v / sum(v))
  turn_share = tot |>
    mutate(n_prec = sum(v), .by = geo) |>
    left_join(cty_sh, by = c("county", "method")) |>
    left_join(select(sw_sh, method, sw), by = "method") |>
    mutate(
      cty_share = coalesce(cty_share, sw),
      share = (v + k_turn * cty_share) / (n_prec + k_turn)      # Beta-ish shrinkage
    ) |>
    mutate(share = share / sum(share), .by = geo) |>
    select(geo, method, mode_turn_share = share)

  # (b) partisan offset: logit_shift so each mode's weighted aggregate matches that
  #     mode's observed historical two-party Dem share
  obs_mode = h |> summarize(d = sum(dem), r = sum(rep), .by = method) |>
    mutate(dem2_mode = d / (d + r))
  w = tot |> select(geo, method, v)
  # Estimate a_m and b_m by regressing the precinct's observed mode differential on its
  # own lean, weighted by that mode's votes. Falls back to (0,0) on thin data.
  off = t(vapply(MODES, function(m) {
    hj = h |> filter(method == m) |> inner_join(select(prec, geo, dem2), by = "geo") |>
      mutate(n2 = dem + rep, s_m = dem / pmax(n2, 1)) |>
      filter(n2 >= 25, is.finite(dem2), dem2 > 0, dem2 < 1, s_m > 0, s_m < 1)
    if (nrow(hj) < 100) return(c(a = 0, b = 0))
    x = logit(hj$dem2); y = logit(hj$s_m) - x
    f = stats::lm.wfit(cbind(1, x), y, hj$n2)
    c(a = unname(f$coefficients[1]), b = unname(f$coefficients[2]))
  }, numeric(2)))
  rownames(off) = MODES
  # The turnout split and the partisan offset come from the same file but are NOT equally
  # trustworthy: a history built from registration-by-mode carries a real turnout split
  # and a misleading partisan one. MODE_OFFSET_SOURCE lets the two be sourced separately.
  if (MODE_OFFSET_SOURCE == "zero") {
    off[] = 0
    cli_alert_info("MODE_OFFSET_SOURCE=zero -- keeping the derived turnout split, \\
                    forcing delta_m to 0")
  } else if (MODE_OFFSET_SOURCE == "nc") {
    off = NC_DELTA[MODES, , drop = FALSE]
    cli_alert_info("MODE_OFFSET_SOURCE=nc -- keeping the derived turnout split, \\
                    borrowing NC-measured delta_m")
  } else if (MODE_OFFSET_SOURCE != "history") {
    cli_abort("unknown MODE_OFFSET_SOURCE {MODE_OFFSET_SOURCE} \\
               (history | zero | nc)")
  }
  cli_inform(c("derived mode priors:",
    paste0("  statewide turnout split: ",
           paste(sprintf("%s %.3f", sw_sh$method, sw_sh$sw), collapse = "  ")),
    paste0("  logit share offsets:     ",
           paste(sprintf("%s %+.3f", names(off), off), collapse = "  "))))
  list(turn_share = turn_share, offset = off, shrink_share = k_share)
}

mode_hist = load_mode_history(path_mode_hist)   # derive_mode_priors() runs after `prec`

# one row per precinct: party-long -> wide, and recover total expected turnout
prec = covs_long |>
  filter(method == "all") |>
  select(-method) |>
  pivot_wider(names_from = party, values_from = est, names_prefix = "est_") |>
  mutate(
    est_tot = est_dem + est_rep,
    log_n   = log(n),
    # third-party prior from the frame's residual 1 - pr_harris - pr_trump. Measured
    # n-weighted 1.64% for TX vs ~1.4% actual 2024 "other", so it is signal -- but the
    # precinct range is 0% to 20.4%, so shrink hard toward the statewide value.
    # A panel may instead supply `tp_raw` directly -- a backtest must, since
    # pr_harris/pr_trump are calibrated to 2024 returns and so are look-ahead for 2024.
    tp_prior_raw = if ("tp_raw" %in% names(covs_long)) pmin(pmax(tp_raw, 0), 0.25)
                   else pmin(pmax(1 - pr_harris - pr_trump, 0), 0.25),
    # standardize every raw frame covariate the active COVS_* lists name (with the
    # default lists this is turnout22/turnout24/age/male/pred.bla/pred.his, as before)
    across(all_of(setdiff(unique(c(COVS_TURN, COVS_SHARE, COVS_TP)), COVS_DERIVED)),
           ~ as.numeric(scale(.x)))
  ) |>
  filter(est_tot > 0, is.finite(dem2), is.finite(tp_prior_raw)) |>
  mutate(
    tp_sw    = stats::weighted.mean(tp_prior_raw, est_tot),
    w_tp     = n / (n + TP_SHRINK_N),
    tp_prior = pmin(pmax(w_tp * tp_prior_raw + (1 - w_tp) * tp_sw,
                         TP_RANGE[1]), TP_RANGE[2])
  )

cli_h1("{STATE} Governor 2026 -- trial precinct x mode model")
cli_inform(c(
  "precincts: {nrow(prec)}",
  "baseline expected turnout: {format(round(sum(prec$est_tot)), big.mark = ',')}",
  "baseline Dem two-party share: {round(weighted.mean(prec$dem2, prec$est_tot), 4)}"
))
warn_uncalibrated_modes = function(mode_priors) {
  if (!is.null(mode_priors)) return(invisible())
  if (all(MODE_SHARE_OFFSET == 0)) {
    cli_alert_warning("No mode history, and MODE_SHARE_OFFSET is all zero: the prior \\
                       assumes every mode votes like its precinct overall. Early-night \\
                       output will carry the full red-mirage/blue-shift bias.")
  } else {
    cli_alert_warning("Using hardcoded MODE_* constants (no mode history found).")
  }
}

# ---- 2. expand to precinct x mode cells, build mode-specific priors -------------
mode_priors = derive_mode_priors(mode_hist, prec)
warn_uncalibrated_modes(mode_priors)

cells = tidyr::expand_grid(prec, mode = MODES) |>
  { \(x) if (is.null(mode_priors)) {
      mutate(x, mode_turn_share = MODE_TURN_SHARE[mode],
                off_a = MODE_SHARE_OFFSET[mode, "a"], off_b = MODE_SHARE_OFFSET[mode, "b"])
    } else {
      x |>
        left_join(rename(mode_priors$turn_share, mode = method), by = c("geo", "mode")) |>
        mutate(
          # precincts the history did not cover fall back to the statewide constants
          mode_turn_share = coalesce(mode_turn_share, MODE_TURN_SHARE[mode]),
          off_a = mode_priors$offset[mode, "a"],
          off_b = mode_priors$offset[mode, "b"]
        )
    } }() |>
  mutate(
    mode_turn_share = mode_turn_share / sum(mode_turn_share), .by = geo
  ) |>
  mutate(
    prior_tau = est_tot * mode_turn_share,
    # delta_im = a_m + b_m * logit(dem2_i)  -- lean-dependent, per the NC measurement
    prior_s   = invlogit(logit(dem2) + off_a + off_b * logit(dem2)),
    prior_tp  = tp_prior,            # no mode differential assumed for third party
    y_tp_prior  = logit(prior_tp),
    y_s_prior   = logit(prior_s),
    # registrant-based ceiling on the PRECINCT total
    cap = KAPPA * pmax(n, 1),
    # share of this precinct's prior composition carried by the reference mode
    ref_share = mode_turn_share[match(REF_MODE, mode)], .by = geo
  ) |>
  mutate(
    # The per-mode `tau` SLOT is reinterpreted rather than restructured, so every
    # downstream index (Sigma, eps, the conditional-MVN block, the aggregation offsets)
    # keeps working unchanged:
    #   reference mode's slot -> theta, the precinct TOTAL on the capped logit scale
    #   other modes' slots    -> eta_m, the log-ratio of that mode's share to the ref's
    y_tau_prior = if (!USE_COMPOSE) log(prior_tau + 0.1) else if_else(
      mode == REF_MODE,
      logit(pmin(pmax(est_tot / cap, 1e-4), 1 - 1e-4)),
      log(pmax(mode_turn_share, 1e-6) / pmax(ref_share, 1e-6)))
  )
N_PREC = nrow(prec); M = length(MODES)
# county indicator matrix for the random effect (one column per county, in prec order)
CTY_MAT = if (!USE_COUNTY_RE) matrix(0, nrow(prec), 0) else {
  mk = function(v, pre) {
    f = factor(v); m = stats::model.matrix(~ f - 1)
    colnames(m) = paste0(pre, levels(f)); m
  }
  switch(RE_GROUPING,
    county     = mk(prec$county, "cty_"),
    geostratum = mk(prec$geostratum, "geo_"),
    cory       = cbind(mk(prec$stratum_demo, "sd_"), mk(prec$geostratum, "geo_")),
    cli_abort("unknown RE_GROUPING {RE_GROUPING}"))
}
if (USE_COUNTY_RE)
  cli_inform("random effect ON [{RE_GROUPING}]: {ncol(CTY_MAT)} levels, \\
              lambda turn {round((1-ICC_TURN)/ICC_TURN,2)} / share {round((1-ICC_SHARE)/ICC_SHARE,2)}")
cli_inform("cells: {nrow(cells)} ({N_PREC} precincts x {M} modes)")

# ---- 3. synthetic returns (replace with the live feed) --------------------------
# Truth is drawn ONCE with a deliberate mode differential and correlated precinct
# shocks, so the cross-mode conditioning has something real to find.
make_truth = function(cells) {
  # Error structure now MATCHES the NC 2024 measurements rather than my initial guesses:
  #   within-mode rho(tau, s):  mail +0.204, early +0.052, eday -0.140  (sign-varying)
  #   cross-mode rho:           ~0 for turnout and for mail<->eday / early<->eday
  # The previous version drew u_tau / u_s at PRECINCT level, shared across modes, which
  # implied near-perfect cross-mode correlation -- the opposite of what NC shows -- and
  # drew tau and s independently, so it had no within-mode correlation to find either.
  # Consequence: full / block / diag Sigma were indistinguishable for reasons that were
  # artefacts of the generator. Shocks are now drawn PER CELL with mode-specific
  # within-cell correlation. (This slightly understates the one real cross-mode pair,
  # mail<->early at +0.232; representing that would need a factor structure.)
  rho_in  = c(mail = 0.204, early = 0.052, eday = -0.140)
  sd_tau  = 0.18; sd_s = 0.22; sd_tp = 0.30
  # COUNTY-level component, at the NC-measured ICC. Without this the generator has
  # independent precincts and cannot test whether the county random effect is calibrated
  # -- it would only show that the intervals widen.
  cty <- unique(cells$county)
  cshock <- tibble(
    county = cty,
    c_tau = rnorm(length(cty), 0, sd_tau * sqrt(ICC_TURN  / (1 - ICC_TURN))),
    c_s   = rnorm(length(cty), 0, sd_s   * sqrt(ICC_SHARE / (1 - ICC_SHARE))),
    c_tp  = rnorm(length(cty), 0, sd_tp  * sqrt(ICC_TP    / (1 - ICC_TP)))
  )
  true_a = c(mail =  0.660, early = -0.029, eday = -0.025)
  true_b = c(mail = -0.297, early =  0.041, eday = -0.089)
  cells |>
    left_join(cshock, by = "county") |>
    mutate(
      .r  = rho_in[mode],
      .z1 = rnorm(n()), .z2 = rnorm(n()),
      e_tau = sd_tau * .z1 + c_tau,
      e_s   = sd_s * (.r * .z1 + sqrt(1 - .r^2) * .z2) + c_s,   # within-cell corr + county
      e_tp  = sd_tp * rnorm(n()) + c_tp,
      tau_true = pmax(round(prior_tau * exp(e_tau)), 0),
      s_true   = invlogit(logit(dem2) + true_a[mode] + true_b[mode] * logit(dem2) + e_s),
      tp_true  = invlogit(logit(prior_tp) + e_tp)
    ) |>
    select(geo, mode, tau_true, s_true, tp_true)
}

# reporting order: mode-driven (mail -> early -> eday), with county-level jitter
make_stages = function(cells, truth, frac) {
  ord = c(mail = 0.0, early = 0.33, eday = 0.66)
  cells |>
    select(geo, county, mode) |>
    mutate(
      pos = ord[mode] + 0.34 * (dense_rank(county) / n_distinct(county)) + runif(n(), 0, 0.03),
      stage = if_else(pos <= frac, STAGE_DONE, 0L)
    ) |>
    select(geo, mode, stage) |>
    left_join(truth, by = c("geo", "mode")) |>
    mutate(
      v_two = tau_true * (1 - tp_true),
      votes_dem = if_else(stage >= STAGE_DONE, round(v_two * s_true), NA_real_),
      votes_rep = if_else(stage >= STAGE_DONE, round(v_two * (1 - s_true)), NA_real_),
      votes_oth = if_else(stage >= STAGE_DONE, round(tau_true * tp_true), NA_real_)
    ) |>
    select(geo, mode, stage, votes_dem, votes_rep, votes_oth)
}

# ---- 4. fit --------------------------------------------------------------------
# One lm_bayes per (mode, outcome) = K_OUT * M = 3M equations (9 for three modes). All
# share a design, so these coefficients equal the full-SUR ones; Sigma is estimated from
# residuals. Caveat: the equivalence needs the same regressors on the SAME ROWS, and the
# observed sets differ by mode on the night (early reports before eday), so this is an
# unbalanced SUR and equation-by-equation is not strictly efficient. Unmeasured.

# Conjugate Gaussian fit with a g-prior on the covariate block and a ridge-equivalent
# random effect on the county block. One weighted QR; same posterior form as lm_bayes.
#   rows: [g-prior pseudo-rows | reported data rows | county ridge rows]



# Pairwise covariance from an incomplete residual matrix. Blocks with too little
# overlap are left at zero (conservative: no conditioning through that block).

fit_stage = function(cells, returns, draws = DRAWS, cross_mode = USE_CROSS_MODE,
                     Sigma_hist = NULL) {
  has_complete = "complete" %in% names(returns)
  if (!has_complete)
    cli_alert_warning("returns carry no `complete` flag -- falling back to \\
                       COMPLETE_FRAC * prior_tau, which is known to be unreliable \\
                       (see the note in fit_stage)")
  d = cells |>
    left_join(returns, by = c("geo", "mode")) |>
    mutate(
      complete = if (has_complete) coalesce(complete, FALSE) else FALSE,
      tot2  = votes_dem + votes_rep,
      tau   = tot2 + votes_oth,                       # ALL votes in the cell
      obs   = !is.na(votes_dem) & stage >= STAGE_DONE & tau > 0 & tot2 > 0,
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
      #  2. COMPLETE_FRAC * prior_tau -- last-resort fallback, and a poor one: it
      #     inherits prior_tau's error, which on TX 2024 is ~0.5x for mail and ~1.7x for
      #     eday, so no single threshold works. Strict settings never pin mail (turnout
      #     ends the night at 113%); loose ones treat 5%-counted eday cells as final
      #     (turnout ends at 87%). Supply `complete` instead wherever possible.
      obs_turn = obs & if (has_complete) complete else tau >= COMPLETE_FRAC * prior_tau,
      # RAW observed values -- used to substitute counted cells in the aggregation.
      # o_tau is gated on obs_turn, the shares on obs.
      o_tau = if_else(obs_turn, tau, NA_real_),
      # CENSORING FLOOR: whatever is counted in a cell is a hard LOWER BOUND on its
      # final turnout, whether or not the cell is counted out. This needs no threshold
      # and no completeness classifier, so it carries the cases the classifier gets
      # wrong -- which on TX 2024 is most of them, since prior_tau is off by ~0.5x for
      # mail and ~1.7x for eday and no single COMPLETE_FRAC can serve both.
      f_tau = if_else(obs, tau, 0),
      f_dem = if_else(obs, votes_dem, 0),
      f_rep = if_else(obs, votes_rep, 0),
      f_oth = if_else(obs, votes_oth, 0),
      o_tp  = if_else(obs, votes_oth / pmax(tau, 1), NA_real_),
      o_s   = if_else(obs, votes_dem / pmax(tot2, 1), NA_real_),
      # CONTINUITY-CORRECTED values -- used to FIT, so a cell with zero third-party
      # votes (very common) gives a finite logit instead of -Inf
      y_tau = NA_real_,                 # filled below; meaning depends on USE_COMPOSE
      y_tp  = if_else(obs, logit((votes_oth + 0.5) / (tau + 1)),  NA_real_),
      y_s   = if_else(obs, logit((votes_dem + 0.5) / (tot2 + 1)), NA_real_)
    )

  # ---- precinct-level quantities for the total x composition parameterization ------
  # The per-mode `tau` SLOT is REINTERPRETED (see the cells block): the reference mode's
  # slot carries the precinct TOTAL on the capped logit scale, the others carry their
  # log-ratio to the reference. Doing it this way rather than restructuring the key
  # layout keeps Sigma, eps, the conditional-MVN grouping and the aggregation offsets
  # working unchanged.
  d = d |>
    mutate(
      cnt_prec  = sum(if_else(obs, tau, 0), na.rm = TRUE),   # counted so far, precinct
      all_done  = all(obs_turn),                             # every mode counted out
      T_final   = sum(if_else(obs_turn, tau, 0), na.rm = TRUE),
      tau_ref   = { i <- match(REF_MODE, mode); if_else(rep(obs_turn[i], n()), tau[i], NA_real_) },
      ref_done  = { i <- match(REF_MODE, mode); rep(obs_turn[i], n()) },
      .by = geo) |>
    mutate(
      is_ref = mode == REF_MODE,
      # WHICH ROWS THE TURNOUT SLOT CAN BE FITTED ON:
      #   total      -- needs every mode of the precinct counted out, else T is unknown
      #   log-ratio  -- needs this mode AND the reference mode counted out
      fit_tau = if (!USE_COMPOSE) obs_turn
                else if_else(is_ref, all_done, obs_turn & ref_done & !is.na(tau_ref) & tau_ref > 0),
      y_tau = if (!USE_COMPOSE) if_else(obs_turn, log(tau + 0.1), NA_real_)
              else if_else(is_ref,
                           logit(pmin(pmax(T_final / cap, 1e-4), 1 - 1e-4)),
                           log(pmax(tau, 0.5) / pmax(tau_ref, 0.5))),
      y_tau = if_else(fit_tau, y_tau, NA_real_),
      # The TOTAL is right-censored for any reporting precinct: T_i >= what is counted.
      # This is a far better home for censoring than the cell level, where it fixed the
      # early blow-up and broke the late one -- the total is the quantity with the good
      # prior (corr(log) 0.962) and the bound is directly observable.
      cens_tau = USE_COMPOSE & is_ref & !fit_tau & cnt_prec > 0,
      lb_tau   = logit(pmin(pmax(cnt_prec / cap, 1e-4), 1 - 1e-4))
    )

  n_obs_cells = sum(d$obs)
  if (n_obs_cells < 30) {
    cli_alert_info("only {n_obs_cells} observed cells -- returning prior only")
  }

  # --- 4a. per-(mode, outcome) conjugate fits ---
  fits = list(); n_fit = list(); prior_loc_mat = list()
  resid_mat = matrix(NA_real_, N_PREC, K_OUT * M)
  colnames(resid_mat) = paste(rep(MODES, each = K_OUT), OUTCOMES, sep = "_")
  geo_idx = setNames(seq_len(N_PREC), prec$geo)
  covs_for = function(out) switch(out, tau = COVS_TURN, tp = COVS_TP, s = COVS_SHARE)
  prior_n_for = function(out) switch(out, tau = PRIOR_N_TURN, tp = PRIOR_N_TP,
                                     s = PRIOR_N_SHARE)

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
    n_c = sum(dm$obs); n_cty = n_distinct(dm$county[dm$obs])
    if (n_c > 0 && (n_c < MIN_MODE_CELLS || n_cty < MIN_MODE_CTY)) {
      cli_alert_info("mode {m}: only {n_c} cell(s) in {n_cty} county(ies) \\
                      -- below the support gate ({MIN_MODE_CELLS} cells / \\
                      {MIN_MODE_CTY} counties), holding the mode at its prior")
      dm$obs = FALSE; dm$obs_turn = FALSE
      dm$o_tau = dm$o_tp = dm$o_s = NA_real_
    }
    for (out in OUTCOMES) {
      ycol  = paste0("y_", out)
      pcol  = paste0("y_", out, "_prior")
      covs  = covs_for(out)
      pn    = prior_n_for(out)
      form  = as.formula(paste(ycol, "~", paste(sprintf("`%s`", covs), collapse = " + ")))
      dm2   = mutate(dm, !!ycol := coalesce(.data[[ycol]], .data[[pcol]]))  # placeholder; subset gates fitting
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
      wt = if (out == "tau") NULL else {
        raw = pmax(dm$tau, 1)                         # ballots counted in the cell
        raw[!dm$obs] = 1                              # unused (not in subset), keep finite
        # normalise to mean 1 over the OBSERVED rows, so prior_n keeps its
        # "precinct-equivalents" meaning instead of being swamped by raw counts
        mu = if (any(dm$obs)) mean(raw[dm$obs]) else 1
        raw / mu
      }
      if (USE_COUNTY_RE) {
        icc = switch(out, tau = ICC_TURN, tp = ICC_TP, s = ICC_SHARE)
        lam = (1 - icc) / icc
        Xcov = cbind(1, as.matrix(prec[, covs, drop = FALSE]))
        fit = fit_conj_re(
          y = dm[[ycol]], Xcov = Xcov, Cty = CTY_MAT, obs = ob,
          prior_loc = dm[[pcol]], prior_eff = pn / nrow(dm),
          lambda = lam, wts = wt,
          sd_level = if (SD_SYS_LEVEL > 0) SD_SYS_LEVEL else NULL,
          cens = if (USE_CENSOR) cs else NULL, y_lb = lb
        )
        if (is.null(fit)) cli_abort("county-RE design is rank deficient for {m}/{out}")
      } else {
        fit = lm_bayes(
          form, data = dm2, subset = ob, weights = wt,
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
    sdv = vapply(fits, sig_of, 0)                      # tonight's own per-equation sigmas
    C = measured_corr(colnames(resid_mat))
    diag(sdv) %*% C %*% diag(sdv)
  } else if (!is.null(Sigma_hist)) {
    stopifnot(all(dim(Sigma_hist) == K_OUT * M))
    Sigma_hist[colnames(resid_mat), colnames(resid_mat), drop = FALSE]
  } else if (SIGMA_SOURCE == "pairwise") {
    cov_pairwise(resid_mat)
  } else {
    cli_alert_info("no historical Sigma supplied -- using diagonal (no cross-mode \\
                    conditioning). Set SIGMA_SOURCE or pass Sigma_hist.")
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
    s_idx = which(grepl("_s$", colnames(Sigma)))          # the three share equations
    rho = if (RHO_CROSS == "draw") NA_real_ else as.numeric(RHO_CROSS)
    if (!is.na(rho)) {
      sdv = sqrt(diag(Sigma))
      for (a in s_idx) for (b in s_idx) if (a != b)
        Sigma[a, b] = rho * sdv[a] * sdv[b]
      Sigma = psd_repair(Sigma)
      dimnames(Sigma) = list(colnames(resid_mat), colnames(resid_mat))
    }
  }

  # --- 4c. draws: linear predictor + jointly-drawn residuals ---
  # mu[[k]] is draws x N_PREC for outcome k, in the column order of resid_mat
  keys = colnames(resid_mat)
  mu = lapply(keys, function(k) {
    parts = strsplit(k, "_")[[1]]; out = parts[2]
    covs = covs_for(out)
    X    = cbind(1, as.matrix(prec[, covs, drop = FALSE]))
    # An equation with (almost) no reported cells must sit at its PRIOR LOCATION, not at
    # a posterior mean drawn from a g-prior worth 3 precinct-equivalents. With no data
    # that posterior is extremely diffuse -- var of a fitted value is ~sigma^2 * p /
    # prior_n -- and for the turnout equations it is then EXPONENTIATED, which put the
    # statewide turnout draw at 1e26 the first time the obs/obs_turn split left eday and
    # mail with no counted-out cells. Holding the mode at its prior has to mean the
    # prior, not an unconstrained fit to nothing.
    if (n_fit[[k]] < MIN_FIT_OBS) {
      return(matrix(prior_loc_mat[[k]], draws, N_PREC, byrow = TRUE))
    }
    if (USE_COUNTY_RE) {
      # design now carries the county block; counties with no reported precincts get
      # u_c shrunk to ~0 but keep the full random-effect variance in the draws
      draw_beta_re(fits[[k]], draws) %*% t(cbind(X, CTY_MAT))
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
  mask = as.matrix(obs_mode[, MODES, drop = FALSE])            # N_PREC x M logical
  # second mask, for TURNOUT substitution only: a reporting-but-still-counting cell
  # contributes its shares but NOT its turnout (see the two flags in `d`)
  turn_mode = d |>
    select(geo, mode, obs_turn) |>
    pivot_wider(names_from = mode, values_from = obs_turn) |>
    arrange(match(geo, prec$geo))
  mask_turn = as.matrix(turn_mode[, MODES, drop = FALSE])      # N_PREC x M logical
  # THIRD mask, for the turnout SLOT's observed-ness in the conditional-MVN block.
  # It is not the same as mask_turn once the slot holds a total / log-ratio: a non-ref
  # cell can be counted out while its REFERENCE mode is not, in which case its log-ratio
  # does not exist. Conditioning on mask_turn there reads an NA y_tau -- which is exactly
  # what the stopifnot below caught.
  fit_mode = d |>
    select(geo, mode, fit_tau) |>
    pivot_wider(names_from = mode, values_from = fit_tau) |>
    arrange(match(geo, prec$geo))
  mask_fit = as.matrix(fit_mode[, MODES, drop = FALSE])        # N_PREC x M logical
  obs_vals = d |>
    select(geo, mode, y_tau, y_tp, y_s) |>
    pivot_wider(names_from = mode, values_from = c(y_tau, y_tp, y_s),
                names_glue = "{mode}_{str_remove(.value, 'y_')}") |>
    arrange(match(geo, prec$geo))
  obs_vals = as.matrix(obs_vals[, keys, drop = FALSE])         # N_PREC x K_OUT*M
  obs_raw = d |>
    select(geo, mode, o_tau, o_tp, o_s) |>
    pivot_wider(names_from = mode, values_from = c(o_tau, o_tp, o_s),
                names_glue = "{mode}_{str_remove(.value, 'o_')}") |>
    arrange(match(geo, prec$geo))
  obs_raw = as.matrix(obs_raw[, keys, drop = FALSE])           # N_PREC x K_OUT*M
  cnt_mat = function(col) {
    m = d |> select(geo, mode, all_of(col)) |>
      pivot_wider(names_from = mode, values_from = all_of(col)) |>
      arrange(match(geo, prec$geo))
    m = as.matrix(m[, MODES, drop = FALSE]); m[is.na(m)] = 0; m
  }
  cnt_tau = cnt_mat("f_tau"); cnt_dem = cnt_mat("f_dem")
  cnt_rep = cnt_mat("f_rep"); cnt_oth = cnt_mat("f_oth")

  # residual draws, conditioning on observed OUTCOMES of the same precinct.
  # The pattern is per (mode, outcome), not per mode: since the obs/obs_turn split a
  # reporting-but-still-counting cell has its shares observed and its TURNOUT not, so
  # conditioning on all three outcomes of an "observed" mode would read an NA y_tau.
  obs_out = matrix(FALSE, N_PREC, K_OUT * M, dimnames = list(NULL, keys))
  for (mi in seq_along(MODES)) {
    k = K_OUT * (mi - 1)
    obs_out[, k + 1] = mask_fit[, mi]       # turnout slot: where y_tau actually exists
    obs_out[, k + 2] = mask[, mi]           # tp
    obs_out[, k + 3] = mask[, mi]           # s
  }
  stopifnot(!any(is.na(obs_vals[obs_out])))
  eps = array(0, c(draws, N_PREC, K_OUT * M))
  pat = apply(obs_out, 1, function(z) paste(as.integer(z), collapse = ""))
  for (p in unique(pat)) {
    rows = which(pat == p)
    o = which(strsplit(p, "")[[1]] == "1")
    u = setdiff(seq_len(K_OUT * M), o)
    if (!length(u)) next
    # With a diagonal Sigma, Sigma_uo = 0, so conditioning is an exact no-op. Skip it
    # rather than multiplying by zeros -- this is the simplification `diag` buys.
    if (length(o) && cross_mode && any(Sigma[u, o, drop = FALSE] != 0)) {
      A   = Sigma[u, o, drop = FALSE] %*% solve(Sigma[o, o, drop = FALSE])
      Scu = psd_repair(Sigma[u, u, drop = FALSE] - A %*% Sigma[o, u, drop = FALSE])
      # observed residual = observed y - its own linear predictor (draw-specific)
      for (j in seq_along(rows)) {
        i  = rows[j]
        ro = vapply(o, function(k) obs_vals[i, k] - mu[[k]][, i], numeric(draws))  # draws x |o|
        shift = ro %*% t(A)                                                        # draws x |u|
        eps[, i, u] = shift + matrix(rnorm(draws * length(u)), draws) %*% chol(Scu)
      }
    } else {
      Su = psd_repair(Sigma[u, u, drop = FALSE])
      for (i in rows) eps[, i, u] = matrix(rnorm(draws * length(u)), draws) %*% chol(Su)
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
    cf = factor(prec$county); ci = as.integer(cf); n_cty = nlevels(cf)
    for (mi in seq_along(MODES)) {
      k = K_OUT * (mi - 1)
      # Keyed on mask_turn, NOT mask: a cell that is reporting but still counting has its
      # share SUBSTITUTED only once counted out (see 4d), so the cells whose final share
      # is still uncertain are exactly the not-counted-out ones. Keying this on `mask`
      # instead left the late stages untouched -- at 92.7% reported the width moved
      # 0.59 -> 0.60pp -- because by then almost every cell has SOME report, while the
      # remaining VOTES still sat inside half-counted cells.
      unrep = !mask_turn[, mi]
      if (!any(unrep)) next
      z = matrix(rnorm(draws * n_cty, 0, SD_UNREP), draws, n_cty)
      eps[, unrep, k + 3] = eps[, unrep, k + 3] + z[, ci[unrep], drop = FALSE]
    }
  }

  # --- 4d. invert, substitute observed, aggregate WITHIN each draw ---
  num_d = num_r = num_o = den = numeric(draws)
  cell_s_hat = matrix(NA_real_, N_PREC, M, dimnames = list(NULL, MODES))
  # PER-PRECINCT accumulators (draws x N_PREC), for precinct-level output
  pnum_d = matrix(0, draws, N_PREC); pden = matrix(0, draws, N_PREC)
  # ---- reconstruct turnout from the TOTAL and the COMPOSITION ---------------------
  # tau_im = T_i * p_im with sum_m p_im = 1, so sum_m tau_im = T_i EXACTLY and
  # T_i < KAPPA * n_i by construction. Phantom ballots in one mode can no longer inflate
  # the precinct total, and 184%-of-truth turnout is arithmetically impossible rather
  # than something detected afterwards.
  ref_i = match(REF_MODE, MODES)
  if (USE_COMPOSE) {
    k_ref = K_OUT * (ref_i - 1)
    capm  = matrix(KAPPA * pmax(prec$n, 1), draws, N_PREC, byrow = TRUE)  # cap lives on `cells`
    T_draw = capm * invlogit(mu[[k_ref + 1]] + eps[, , k_ref + 1])
    # softmax over modes; the reference mode's log-ratio is 0 by definition
    ex = vector("list", M); den_p = matrix(0, draws, N_PREC)
    for (mi in seq_along(MODES)) {
      k = K_OUT * (mi - 1)
      ex[[mi]] = if (mi == ref_i) matrix(1, draws, N_PREC)
                 else exp(pmin(mu[[k + 1]] + eps[, , k + 1], 20))
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
    T_draw  = pmax(T_draw, cnt_tot)
    rem_tot = T_draw - cnt_tot
    short = vector("list", M); short_sum = matrix(0, draws, N_PREC)
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
    tau = if (USE_COMPOSE) NULL else {
      tt = exp(mu[[k + 1]] + eps[, , k + 1]) - 0.1; tt[tt < 0] = 0; tt
    }
    tp  = invlogit(mu[[k + 2]] + eps[, , k + 2])
    s   = invlogit(mu[[k + 3]] + eps[, , k + 3])
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
      rem = rem_tot * (short[[mi]] / short_sum)     # this mode's share of what is left
      tau = ct + rem                                 # sums to T_i across modes
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
    num_d = num_d + rowSums(v_dem)
    num_r = num_r + rowSums(v_rep)
    num_o = num_o + rowSums(v_oth)
    den   = den   + rowSums(v_dem + v_rep + v_oth)
    pnum_d = pnum_d + v_dem
    pden   = pden + v_dem + v_rep + v_oth
  }
  # precinct-level Dem share of ALL votes, summarised across draws
  pshare = pnum_d / pmax(pden, 1e-9)
  prec_out = tibble(
    geo      = prec$geo,
    county   = prec$county,
    n_obs_modes = rowSums(mask),
    turn_est = colMeans(pden),
    dem_est  = colMeans(pshare),
    dem_sd   = apply(pshare, 2, sd),
    dem_lo   = apply(pshare, 2, quantile, 0.025),
    dem_hi   = apply(pshare, 2, quantile, 0.975)
  )
  den = pmax(den, 1e-9)
  list(
    # HEADLINE: shares of ALL votes. These sum to 1 by construction.
    S_dem = num_d / den, S_rep = num_r / den, S_oth = num_o / den,
    # two-party share, which is what decides the race
    S_2p  = num_d / pmax(num_d + num_r, 1e-9),
    turn  = den, Sigma = Sigma, resid = resid_mat, n_obs_cells = n_obs_cells,
    cell_s = cell_s_hat, cell_obs = mask,
    prec = prec_out
  )
}

# Estimate Sigma from a PAST election in which every mode eventually reported.
# This is the real calibration step: run it once per state offline on 2022/2024 and
# cache to {STATE}_gov_sigma.rds; on election night Sigma is an input, not a fit.
calibrate_sigma = function(cells, returns_full) {
  f = fit_stage(cells, returns_full, draws = 2, cross_mode = FALSE)
  R = f$resid
  cc = stats::complete.cases(R)
  if (sum(cc) < 50) cli_abort("calibration needs >=50 fully-reported precincts, got {sum(cc)}")
  S = psd_repair(stats::cov(R[cc, , drop = FALSE]))
  dimnames(S) = dimnames(R)[c(2, 2)]
  cli_alert_success("Sigma calibrated on {sum(cc)} fully-reported precincts")
  S
}

# ---- 5. prior-only draws (reference measure for conf_seq) ----------------------
prior_draws = function(cells, draws = DRAWS,
                       sd_sys_s = 0.10, sd_sys_tau = 0.12,
                       sd_idio_s = 0.40, sd_idio_tau = 0.30) {
  # CRITICAL: the systematic terms are what make this a usable reference measure.
  # Idiosyncratic per-cell noise averages out over ~13.5k cells (1/sqrt(n)), so a
  # prior built from it alone is ABSURDLY tight at the statewide level -- tighter
  # than the posterior, which makes conf_seq's quadratic unbounded and returns
  # (-Inf, Inf). The real prior uncertainty is about the statewide SWING, which is
  # perfectly correlated across precincts and does not average away.
  sys_s   = rnorm(draws, 0, sd_sys_s)
  sys_tau = rnorm(draws, 0, sd_sys_tau)
  sys_tp  = rnorm(draws, 0, sd_sys_s)
  num = den = numeric(draws)
  for (m in MODES) {
    cm  = filter(cells, mode == m)
    nc  = nrow(cm)
    tau = matrix(cm$prior_tau, draws, nc, byrow = TRUE) *
          exp(sys_tau + matrix(rnorm(draws * nc, 0, sd_idio_tau), draws))
    s   = invlogit(matrix(logit(cm$prior_s), draws, nc, byrow = TRUE) +
                   sys_s + matrix(rnorm(draws * nc, 0, sd_idio_s), draws))
    tp  = invlogit(matrix(logit(cm$prior_tp), draws, nc, byrow = TRUE) +
                   sys_tp + matrix(rnorm(draws * nc, 0, sd_idio_s), draws))
    num = num + rowSums(tau * (1 - tp) * s); den = den + rowSums(tau)
  }
  num / pmax(den, 1e-9)   # prior predictive for the DEM SHARE OF ALL VOTES
}

# ---- 6. run --------------------------------------------------------------------
# SOURCE_ONLY=1 stops here: sections 1-5 are definitions plus the real `cells`, `prec`,
# `CTY_MAT` and config, which is exactly what a backtest driver needs. The driver then
# calls the SAME fit_stage() the live model uses, so the two cannot drift -- the reason
# model_core.R was extracted in the first place. The closing brace is at end of file.
if (nzchar(Sys.getenv("SOURCE_ONLY"))) {
  cli_alert_info("SOURCE_ONLY set -- definitions and cells built; \\
                  skipping the synthetic run")
} else {

args  = commandArgs(trailingOnly = TRUE)
fracs = if (length(args)) as.numeric(args[1]) else c(0.10, 0.30, 0.50, 0.70, 0.90, 1.00)

# Sigma: load the cached historical one, else calibrate on a synthetic PAST election
# (an independent draw from the same structure -- never tonight's own realization).
Sigma_hist = NULL
if (SIGMA_SOURCE == "historical") {
  if (file.exists(path_sigma)) {
    Sigma_hist = readRDS(path_sigma)
    cli_alert_success("Loaded historical Sigma from {.path {path_sigma}}")
  } else {
    cli_alert_warning("No cached Sigma at {.path {path_sigma}} -- calibrating on a \\
                       SYNTHETIC past election (trial only).")
    Sigma_hist = calibrate_sigma(cells, make_stages(cells, make_truth(cells), frac = 1.0))
  }
}

truth = make_truth(cells)
TRUTH = with(truth, {
  tot = sum(tau_true); two = tau_true * (1 - tp_true)
  list(dem = sum(two * s_true) / tot, rep = sum(two * (1 - s_true)) / tot,
       oth = sum(tau_true * tp_true) / tot,
       twoparty = sum(two * s_true) / sum(two), turn = tot)
})
d0 = prior_draws(cells)

cli_h2("Truth: Dem {round(100*TRUTH$dem, 2)}%  Rep {round(100*TRUTH$rep, 2)}%  \
        Oth {round(100*TRUTH$oth, 2)}%  (two-party Dem {round(100*TRUTH$twoparty, 2)}%)  \
        turnout {format(round(TRUTH$turn), big.mark = ',')}")

res = map_dfr(fracs, function(fr) {
  ret = make_stages(cells, truth, fr)
  f   = fit_stage(cells, ret, cross_mode = USE_CROSS_MODE, Sigma_hist = Sigma_hist)
  ci  = cred_int(f$S_dem, level = LEVEL)              # headline: Dem share of ALL votes
  cs  = conf_seq(d0, f$S_dem, level = LEVEL)
  tibble(
    frac = fr, obs_cells = f$n_obs_cells,
    dem = mean(f$S_dem), rep = mean(f$S_rep), oth = mean(f$S_oth),
    sum = mean(f$S_dem + f$S_rep + f$S_oth),          # must be exactly 1
    sd = sd(f$S_dem),
    cred_lo = ci[["lower"]], cred_hi = ci[["upper"]],
    cs_lo = cs[["lower"]], cs_hi = cs[["upper"]],
    covers = TRUTH$dem >= ci[["lower"]] && TRUTH$dem <= ci[["upper"]],
    two_party = mean(f$S_2p), turn = mean(f$turn)
  )
})
if (nzchar(Sys.getenv("COVLINE"))) {
  for (i in seq_len(nrow(res))) cat(sprintf(
    "COV %s %.6f %.6f %.6f %.6f %d\n", res$frac[i], TRUTH$dem,
    res$dem[i], res$cred_lo[i], res$cred_hi[i], as.integer(res$covers[i])))
}
cli_inform("Headline = Dem share of ALL votes; `sum` must be exactly 1.")
print(as.data.frame(mutate(res, across(where(is.numeric), ~ signif(.x, 4)))), row.names = FALSE)

# ---- precinct-level output -------------------------------------------------------
# This is what SIGMA_STRUCT="full" is for: at cell level there is no averaging, so the
# correlation structure and the conditioning are the dominant terms.
cli_h2("Precinct-level predictions at 50% reported")
pp = fit_stage(cells, make_stages(cells, truth, 0.50), cross_mode = USE_CROSS_MODE,
               Sigma_hist = Sigma_hist)$prec
tru_p = truth |>
  summarize(turn = sum(tau_true),
            dem = sum(tau_true * (1 - tp_true) * s_true) / pmax(sum(tau_true), 1), .by = geo)
pj = inner_join(pp, tru_p, by = "geo", suffix = c("", "_true"))
pj = mutate(pj, err = dem_est - dem, covers = dem >= dem_lo & dem <= dem_hi,
            width = dem_hi - dem_lo)
cli_inform(c(
  "precincts: {nrow(pj)}",
  "by number of modes reported:"))
pj |>
  summarize(n = n(), rmse = sqrt(mean(err^2)), width = mean(width),
            coverage = mean(covers), .by = n_obs_modes) |>
  arrange(n_obs_modes) |>
  mutate(across(c(rmse, width, coverage), ~ signif(.x, 3))) |>
  as.data.frame() |> print(row.names = FALSE)

# ablation: what the cross-mode conditioning is worth
if (USE_CROSS_MODE) {
  cli_h2("Ablation at 50% reported: cross-mode Sigma vs independent residuals")
  ret = make_stages(cells, truth, 0.50)
  fa = fit_stage(cells, ret, cross_mode = TRUE,  Sigma_hist = Sigma_hist)
  fb = fit_stage(cells, ret, cross_mode = FALSE)

  # (i) statewide aggregate -- expect LITTLE gain, and the reason matters:
  #     per-cell residual noise averages out at 1/sqrt(13.5k), so statewide width is
  #     set almost entirely by COEFFICIENT uncertainty (which moves every precinct
  #     together). Sigma shrinks per-cell noise, which was already negligible here.
  cli_inform(c(
    "statewide Dem share of all votes:  with cross-mode sd {signif(sd(fa$S_dem), 3)}  |  \\
     without sd {signif(sd(fb$S_dem), 3)}  |  ratio {round(sd(fa$S_dem)/sd(fb$S_dem), 3)}"
  ))

  # (ii) CELL-level share prediction for UNOBSERVED mode cells -- this is where
  #      conditioning on an observed mode of the same precinct actually pays, and
  #      it is what any sub-state aggregate (CD, legislative district) inherits.
  tr = truth |>
    mutate(mi = match(mode, MODES), pi = match(geo, prec$geo)) |>
    filter(!is.na(pi))
  unobs = !fa$cell_obs
  idx = tr |> filter(unobs[cbind(pi, mi)])
  rmse = function(f) sqrt(mean((f$cell_s[cbind(idx$pi, idx$mi)] - idx$s_true)^2))
  cli_inform(c(
    "unobserved-cell share RMSE ({nrow(idx)} cells):",
    "  with    cross-mode: {signif(rmse(fa), 4)}",
    "  without cross-mode: {signif(rmse(fb), 4)}",
    "  ratio {round(rmse(fa)/rmse(fb), 3)}  (<1 => observed modes informed unobserved ones)"
  ))
}

}   # end of the SOURCE_ONLY guard opened at section 6

# ---- TODO before this is more than a trial -------------------------------------
# 0. THIRD-PARTY VOTE: now modelled (the `tp` equation), so `tau` is ALL votes and the
#    three shares of all votes sum to 1 by construction. Remaining caveats:
#    (a) the tp prior is the frame residual 1 - pr_harris - pr_trump (n-weighted 1.64%,
#        but precinct range 0-20.4%, hence the heavy shrinkage). votehub
#        estimated_past_results.csv `gov22_oth/gov22_total` is strictly better -- mean
#        1.43%, median 1.36%, p95 2.69% -- but needs per-county id reconciliation
#        (BEXAR 88% matched, KAUFMAN 38%, BELL 0%). Swap it in once crosswalked.
#    (b) no mode differential is assumed for tp; the synthetic truth has one, so the
#        model must learn it from returns.
#    (c) make sure write-ins / "scattered" land in `other` rather than being dropped,
#        or tau is short and every share is biased high.
#    (d) if a serious independent appears (>~10%), promote them to a 4th outcome --
#        the fit loop is generic over OUTCOMES -- rather than hiding them in `other`.
# 1. CALIBRATE MODE_TURN_SHARE and MODE_SHARE_OFFSET from 2022/2024 mode-level
#    results (recipe in the config block). This is the dominant source of error.
# 2. PARTIAL REPORTING. A half-counted cell is a *weighted* observation for the
#    share equation but a *censored* one (tau >= counted) for turnout. Here a cell
#    is all-or-nothing at STAGE_DONE. Pass `weights` to lm_bayes for share; treat
#    turnout as censored or leave unobserved.
# 3. MODE DENOMINATORS. For an unreported mode we assume MODE_TURN_SHARE; in reality
#    "500 mail votes counted" rarely tells you how many mail ballots exist.
# 4. NON-NESTED DESIGNS. COVS_TURN == COVS_SHARE today, so Sigma does not improve
#    beta (SUR == OLS under matching designs). Moving past-turnout terms to turnout
#    only and past-partisanship to share only would add an efficiency gain -- but it
#    is a real exclusion restriction, so verify by backtest first.
# 5. CROSS-PRECINCT correlation is still absent. Independent precinct errors wash out
#    at 1/sqrt(n); correlated ones do not, and that difference sets the true width of
#    the statewide band. Add a geostratum random effect or spatial term.
# 6. BACKTEST on real 2022/2024 reporting order (enightdata timestamps), not the
#    synthetic order here, and check interval coverage as a function of % reported.
