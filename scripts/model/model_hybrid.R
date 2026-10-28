# ========================================
## model_hybrid -- swing-style re-centring of the precinct x mode model's PRIORS
##
## STATUS. Measured, not scaffold. All four arms run; what each is worth is in MEASURED
## at the bottom, including the two places the earlier `model_both` numbers were wrong
## about themselves. Headline: the level arm recovers tx_turnout_low's true statewide
## level to within 0.2% and fixes the 3.5%-counted overshoot (131% -> 118%, below the
## base model's 119%); the share arm is live on real returns from 19:00, two hours before
## the turnout arm has enough counted-out cells to fit; the county-constraint arm
## reproduces CBS county x method totals to 2e-12 but only fires on counted-out counties,
## which makes it a late-night correction.
##
## WHY. model_turnout.R's turnout TOTAL equation is fitted on precincts where every mode
## is counted out (`fit_tau = all_done`). Mail does not finish on election night, so that
## is satisfied for ZERO precincts across the whole broadcast -- measured on the real TX
## 2024 cadence: 0% eligible for the first eight checkpoints, 11.6% at 99.2% counted,
## 17.7% at 100%. The equation has no data rows, the solve returns its prior, and turnout
## is est_turnout echoed back. On a simulated night with truth at 0.85x the prior the
## model reported 119% of truth from first checkpoint to last -- which is 1/0.85.
##
## The uniform swing model does not have this problem: its turnout fit is per-CELL
## (`frac >= 1`), so it has completed early-vote cells from ~20:25, and it uses
## county x method FIXED effects, so it never extrapolates across counties and
## reporting-order selection cannot bias it. But it carries no covariates, so it cannot
## distinguish a 70%-Hispanic precinct from a 10%-Hispanic one inside the same county.
##
## Measured on tx_turnout_demo (turnout misspecified by demographics, statewide total
## preserved), precinct-level median |log turnout error| at 04:00, UNREPORTED precincts:
##
##   prior (est_turnout)        0.126   cor(err, hisp) +0.87
##   uniform swing model        0.102   cor(err, hisp) +0.45
##   precinct x mode + covars   0.083   cor(err, hisp) +0.10
##
## So each half fixes what the other cannot. This file combines them. Those three figures
## are from the ADDITIVE `| county + mode` fit; re-measured against `county^mode` they
## become 0.1278 / 0.1018 / 0.0713 -- the covariate half gets BETTER, not worse. See
## MEASURED (d), which also shows the interaction alone doing almost none of that work.
##
## THE SPECIFICATION. On counted-out cells,
##
##     log(c_im / prior_tau_im) ~ covariates | county^mode
##
## INTERACTION, not `| county + mode`. Only the interaction can say "county A is 80%
## early, county B is 40%", which is the whole content of a county-level composition.
## The FE count is modest because our frames are county SUBSETS: TX 32x4 = 128,
## NC 100x4 = 400, FL 31x5 = 155, MI 31x4 = 124, AK 30x4 = 120, CA 17x3 = 51.
##
## The decomposition log(c_im/tau0_im) = log(T_i/E_i) + log(p_im/p0_im) says what each
## term absorbs, and the interaction means both halves now have a home:
##   covariates          the within-county precinct structure a county effect cannot see
##                       -- and ONLY that: the term is CENTRED (see below), so it
##                       redistributes turnout and never moves the statewide level
##   county^mode FE      county turnout level x that county's mode composition, UNSHRUNK.
##                       Split post-hoc into a per-county LEVEL and a per-county x mode
##                       COMPOSITION offset (see split_fe()), which go to theta's prior
##                       and eta's prior respectively.
##
## Per-cell, so never starved. County effects FIXED rather than shrunk is precisely why
## the swing model avoids the selection bias that killed the gross-up (measured there:
## cor(precinct turnout delta, dem2) = -0.36 BETWEEN counties, +0.06 within).
##
## WHY THE 3.5%-COUNTED OVERSHOOT HAPPENED, AND THE TWO FIXES. They are independent
## defects and the measurements separate them cleanly.
##   (a) THE GATE WAS GLOBAL. ">= 200 counted-out cells ANYWHERE" was satisfied at 20:15
##       by 252 cells sitting in three or four fast counties, and the fit was then applied
##       to all 32. Measured on tx_turnout_low at 20:15: the additive fit returns an
##       expected-vote-weighted level of +0.0962 -- the WRONG SIGN, against a tau-weighted
##       mean log ratio of -0.1796 on its own fit rows -- and turnout reads 131% of truth.
##       The gate is now PER COUNTY x MODE: below ST_MIN_CM counted-out cells a county x
##       mode is dropped from the fitted effects entirely and keeps its prior. At 20:15
##       that leaves 6 trusted groups, the level becomes -0.0108, and turnout reads 117.9%
##       -- just under the 119% prior echo, which is the right place to be when there is
##       almost no data. This one fix IS the overshoot fix.
##   (b) THE COVARIATE TERM WAS UNCENTRED. The FEs carry the intercept, so `X %*% b` has
##       no defined zero: with log_n ~ 7.9 and dem2 ~ 0.45 un-standardised (COVS_DERIVED
##       is not scaled in model_turnout.R section 1), that term adds an arbitrary
##       statewide constant. It happens to be small on the sim frame -- centring changes
##       the 20:15 figure by nothing at all -- and it is catastrophic on the TX 2024
##       panel, where it put statewide turnout at 2.18x truth with the level clamp pinned
##       at +0.5 for over half of precincts. The term is now centred at the
##       expected-vote-weighted statewide mean and the constant is REATTACHED to the FE
##       (fit_cm()), where it belongs: so the covariates redistribute turnout with zero
##       statewide level effect, and the level comes only from the gated FEs.
## The interaction makes thin data WORSE (more FEs on the same early cells), which is why
## the gate had to MOVE rather than just be raised.
##
## HOW THE BOUNDS SURVIVE. None of this becomes theta, eta or s. It sets their PRIORS:
##     est_tot_i   <- est_tot_i * exp(level_i)
##     mode_turn_share_im <- renormalise( mode_turn_share_im * exp(comp_cm) )
##     prior_s_im  <- invlogit(logit(prior_s_im) + share_offset_im)
##     prior_tau / y_tau_prior / y_s_prior then follow by their own definitions
## Everything downstream is model_turnout.R's, untouched: T = kappa*n*invlogit(theta),
## softmax(eta), the partial-count mixture, adding-up, the counted-votes floor and pin.
## When theta's own equation is empty -- which is all night -- the solve now returns a
## prior that has LEARNED FROM THE RETURNS rather than the vendor score.
##
## LEVEL AND COMPOSITION COMPOSE EXACTLY, and this is provable rather than hoped for:
## mode_turn_share is renormalised PER PRECINCT, so sum_m prior_tau_im = est_tot_i
## whatever the composition offsets are. The level offset therefore sets each precinct's
## total and the composition offsets only split it. There is no interaction term and
## nothing to double-count.
##
## WHAT THE SHARE RE-CENTRING ADDS, AND WHERE IT BITES. Sibling estimator on REPORTING
## cells (not counted-out ones): logit(s_obs) - logit(prior_s) ~ COVS_SHARE | county^mode,
## weighted by counted two-party ballots -- the same rationale as fit_stage's own share
## weights (NC 2024: log(resid^2) on log(ballots) has slope -0.62 to -0.84 against -1 for
## pure binomial, so most share residual variance is sampling noise). Shares are NOT
## censored: a partial count is a noisy SAMPLE of a cell's share, not a bound. So this
## estimator runs on data from ~20:15 where the turnout arm needs ~21:00.
## It changes the POSTERIOR mainly where there is no data to overwhelm it, which is the
## point and also the bound on the harm: prior_yhat_eff is PRIOR_N_SHARE / nrow(cells),
## i.e. 3 precinct-equivalents out of thousands, so a re-centred prior is invisible once
## a mode's own equation has data. It is decisive in exactly one place -- a mode below
## MIN_FIT_OBS (30 observed cells) is held at prior_loc_mat EXACTLY (model_turnout.R's mu
## block), and early in the night that is mail and election day. Those modes now inherit
## the covariate slopes learned from the mode that IS reporting.
## Third party (tp) is deliberately NOT re-centred: weakest signal, smallest quantity,
## more double-counting surface for no measurable gain.
##
## COUNTY-LEVEL RETURNS ARE AN EXACT AGGREGATE CONSTRAINT, OUTSIDE THE REGRESSION.
## A complete county x mode total C_cm is not a thin sample to be shrunk -- it is the
## whole county, and it states sum_{i in c} tau_im = C_cm. It is applied by solving for
## the county's level offset and its one-dimensional-per-mode composition offsets that
## reproduce C_cm from the prior (solve_county(), IPF), NOT by adding rows to the
## regression. Source: the CBS county feed via live/R/cbs_county_pull.R (schema_version 2,
## one row per county x method x candidate). Three traps, all handled in county_targets():
##   * CBS's per-method rows DO NOT ACCOUNT FOR THE WHOLE VOTE. Measured on the TX gov
##     test feed 2026-10-26: 243 of 254 counties disagree and the method rows carry
##     7,141,818 against a county total of 7,988,244 -- 846,426 votes (10.6%) attributed
##     to NO method, unevenly (ratio 0.881 in early+eday+mail counties, 0.974 in
##     adv+eday ones). So an "exact" constraint is not exact as filed. Handled by taking
##     the LEVEL from county_total_vote (which the candidates' race totals reconcile to
##     exactly) and only the COMPOSITION from the method rows, i.e. scaling the method
##     totals up by 1/coverage per county. A per-county scale is exactly right if the
##     unattributed votes are spread like that county's method mix, and the measured
##     unevenness is BETWEEN counties, which per-county scaling removes. Counties below
##     CTY_MIN_COVER coverage are skipped rather than scaled.
##   * `adv` is a PARTIAL AGGREGATE of early+mail. A county files either adv+eday or
##     early+eday+mail, never both, so within a county it is clean -- but per-method
##     totals are NOT comparable across counties and `adv` must never be summed beside
##     `early`/`mail`. Handled by constraining GROUPS of modes: adv constrains
##     {early, mail} jointly and leaves their internal split at the prior.
##   * A PARTIAL county total is a LOWER BOUND, not a constraint. Applying one as a target
##     would understate turnout by exactly the uncounted share. So a county is only
##     constrained once county_pct_exp_vote >= CTY_MIN_PCT and precincts_in ==
##     precincts_of. Per-mode gross-ups from rep_method / exp_method are deliberately NOT
##     used: cbs_county_pull.R records that exp_method is FLOORED to rep_method when CBS
##     files no expectation, so a fraction of exactly 1.0 means "no expectation", not
##     "counted out", and there is no way to tell them apart from the snapshot.
##
## DO NOT COMBINE WITH LEVEL_UPDATE=1 OR COMPOSITION_UPDATE=1. Each applies the same
## correction this file applies, arrived at from a different direction, and together they
## apply it TWICE:
##   LEVEL_UPDATE        r_prior = cnt_prec / (sum(prior_tau) * exp(level_tilt))
##   HY_LEVEL            prior_tau = est_tot * exp(level_i) * mode_turn_share
##     -- so sum(prior_tau) already carries exp(level_i) and level_tilt multiplies again.
##   COMPOSITION_UPDATE  p_tilt = mode_turn_share * exp(comp_tilt[mode]), statewide
##   HY_COMP             mode_turn_share <- renormalise(mode_turn_share * exp(comp_cm))
##     -- so the tilt is re-derived from returns the composition offset already absorbed.
## Both of model_turnout.R's switches are the NARROWER fix (LEVEL_UPDATE corrects only
## r_prior; COMPOSITION_UPDATE is statewide where this is per county) and this file's is
## the general one. They should be REMOVED rather than left as traps -- a silent
## double-correction is exactly the kind of thing that surfaces months later as an
## unexplained bias. fit_stage_hybrid() ABORTS if it sees either.
##
## q_share: the LEVEL cancels out of it (mode_turn_share is normalised), so PIN_DONE's
## gate is unaffected by HY_LEVEL -- but the COMPOSITION offsets DO move q_share, and
## deliberately: q_share is "how much of this precinct's expected vote is flagged counted
## out", and a better composition is a better answer to that question. HY_COMP=0 restores
## the old behaviour exactly.
##
## NOT A FORK. This sources model_turnout.R with SOURCE_ONLY=1 and overrides one step.
## scripts/model/live/R/model_medsl.R is already a COPY of model_turnout.R and its own
## header warns about drift; a second copy would be worse. A change to the model is
## picked up here with no edits.
##
## Run:  Rscript scripts/model/model_hybrid.R                    (prints the self-check)
##       Not wired into live/ and its sim harness (tests/test_sim_night.R) is retired, so the
##       acceptance tests (b) and (d) at the bottom need a new harness to run.
## Use:  source this file, then call
##         fit_stage_hybrid(cells, returns, cbs_county = <snapshot or NULL>)
##       wherever fit_stage(cells, returns) is called today.
# ========================================

# Source the model only if it is not already loaded (a harness that loads model_turnout.R
# by its own parse-and-eval would otherwise have its `cells` and `prec` clobbered).
if (!exists("fit_stage", mode = "function")) {
  Sys.setenv(SOURCE_ONLY = "1")
  source("scripts/model/model_turnout.R")
}
stopifnot("needs fixest" = requireNamespace("fixest", quietly = TRUE))

# ---- switches -----------------------------------------------------------------------
# Four independent arms, each ablatable. With all four at 0 this file is EXACTLY
# model_turnout.R: recentre_prior() returns `cells` unmodified (asserted in the
# self-check below), so fit_stage_hybrid() and fit_stage() agree bit for bit.
HY_LEVEL  = Sys.getenv("HY_LEVEL",  unset = "1") == "1"   # theta's prior   (was model_both)
HY_COMP   = Sys.getenv("HY_COMP",   unset = "1") == "1"   # eta's prior     (new)
HY_SHARE  = Sys.getenv("HY_SHARE",  unset = "1") == "1"   # s's prior       (new)
HY_COUNTY = Sys.getenv("HY_COUNTY", unset = "1") == "1"   # county aggregate constraints

# How many counted-out cells before the TURNOUT level/composition model is fitted at all.
# This is now only a "can the covariate slopes be estimated" gate, not the level gate:
# below it nothing is applied and the result is exactly model_turnout.R.
ST_MIN_CELLS  = as.integer(Sys.getenv("ST_MIN_CELLS", unset = "200"))
# ...and how many distinct gated-in county x mode groups, for the same reason. One
# county's worth of cells identifies a county, not a covariate slope.
ST_MIN_GRP    = as.integer(Sys.getenv("ST_MIN_GRP", unset = "5"))
# PER COUNTY x MODE gate: below this many counted-out cells that county x mode's fitted
# effect is DISCARDED and it keeps its prior. This is the overshoot fix; see the header.
ST_MIN_CM     = as.integer(Sys.getenv("ST_MIN_CM", unset = "10"))
# Clamp on the fitted level, in log points. exp(+/-0.5) is roughly a 1.65x / 0.61x swing
# in statewide turnout -- far outside anything real, so this is a guard against a
# degenerate fit on thin early data, not a modelling choice.
ST_MAX_LEVEL  = as.numeric(Sys.getenv("ST_MAX_LEVEL", unset = "0.5"))
# Clamp on a composition offset, in log points. Looser than the level on purpose: mode
# MIX genuinely moves by a factor of two between counties (80% early vs 40% early is
# 0.69 in log points) where the turnout LEVEL does not.
ST_MAX_COMP   = as.numeric(Sys.getenv("ST_MAX_COMP", unset = "1.0"))

# Share arm, mirroring the turnout gates. Separate constants because the share estimator
# runs on REPORTING cells rather than counted-out ones, so it has far more rows, earlier.
SH_MIN_CELLS  = as.integer(Sys.getenv("SH_MIN_CELLS", unset = "200"))
SH_MIN_GRP    = as.integer(Sys.getenv("SH_MIN_GRP", unset = "5"))
SH_MIN_CM     = as.integer(Sys.getenv("SH_MIN_CM", unset = "10"))
# Clamp in LOGIT points on the two-party share prior. 0.25 logit is ~6pp at 50-50, which
# is already a very large statewide share surprise (calibrate_swing.R: 9 states x 37
# contest pairs move 0.22-0.26 logit between a prior contest and its target).
SH_MAX_SHIFT  = as.numeric(Sys.getenv("SH_MAX_SHIFT", unset = "0.25"))

# County-constraint arm. The snapshot normally arrives as fit_stage_hybrid()'s
# `cbs_county` argument; CTY_SNAPSHOT is a file-path fallback for testing, pointing at a
# parquet written by live/R/cbs_county_pull.R (schema_version 2). With neither, inert.
CTY_SNAPSHOT  = Sys.getenv("CTY_SNAPSHOT", unset = "")
# minimum share of a county's total vote that its per-method rows account for before the
# method split is believed at all (see the 10.6% shortfall in the header)
CTY_MIN_COVER = as.numeric(Sys.getenv("CTY_MIN_COVER", unset = "0.80"))
# a county's total is a CONSTRAINT only once it is counted out; below this it is a lower
# bound and using it as a target would understate turnout by the uncounted share
CTY_MIN_PCT   = as.numeric(Sys.getenv("CTY_MIN_PCT", unset = "99"))
# Clamp on a CONSTRAINT-derived level. Wider than ST_MAX_LEVEL because a constraint is
# data, not a fit -- so if it binds, that is a unit or join error worth seeing, and the
# warning says so.
CTY_MAX_LEVEL = as.numeric(Sys.getenv("CTY_MAX_LEVEL", unset = "1.0"))

# COVARIATE SETS FOR THE RE-CENTRING ESTIMATORS, overridable independently of the
# model's own COVS_TURN / COVS_SHARE so the ablation changes ONE thing. Set either to
# "none" for the swing-only estimator (`lr ~ 1 | county^mode`, no covariates), which is
# the comparison the covariate claim has to beat: on tx_turnout_demo the uniform swing
# model scored median |log turnout error| 0.102 against 0.083 for precinct x mode plus
# covariates -- but that was with `| county + mode`, and `county^mode` absorbs far more
# variation, so the gain has to be RE-MEASURED rather than assumed to survive.
# HY_COVS_TURN=none / HY_COVS_SHARE=none is the swing-only arm ("" cannot mean it:
# cov_env() reads an empty env var as "not set" and falls back to the default).
hy_covs = function(var, default)
  if (identical(Sys.getenv(var), "none")) character(0) else cov_env(var, default)
HY_COVS_TURN  = hy_covs("HY_COVS_TURN",  COVS_TURN)
HY_COVS_SHARE = hy_covs("HY_COVS_SHARE", COVS_SHARE)

# CBS county-feed method -> the model modes it constrains. `adv` is early+mail COMBINED
# and `all` is every mode, so each constrains a GROUP and leaves the split inside the
# group at the prior. `prov` must already be gone (reassign_prov = TRUE in the pull).
cbs_method_group = function(method) {
  g = switch(method,
    eday  = "eday", early = "early", mail = "mail",
    adv   = intersect(c("early", "mail"), MODES),
    all   = MODES,
    cli_abort(c("CBS method {.val {method}} has no mapping to the model's modes.",
                "i" = "`prov` should be absorbed upstream by reassign_prov = TRUE.",
                "x" = "Add a mapping in cbs_method_group() rather than guessing here.")))
  g = intersect(g, MODES)
  # An empty group means CBS is reporting a method this state's MODES does not model --
  # e.g. `adv` against the degenerate MODES = "all" path. Silently dropping it would
  # throw away the county's votes; silently keeping it would break the IPF.
  if (!length(g))
    cli_abort(c("CBS method {.val {method}} maps to none of this state's MODES \
                 ({paste(MODES, collapse = '/')}).",
                "x" = "Reconcile lookups/mode_scope.csv with the CBS vocabulary before \
                       constraining on this feed."))
  g
}

# ---- shared machinery ---------------------------------------------------------------
# Expected-vote-weighted statewide mean of the design, so a covariate term built from it
# has ZERO statewide level effect by construction (header, fix (b)).
covs_centre = function(covs, p = prec) {
  if (!length(covs)) return(numeric(0))
  X = as.matrix(p[, covs, drop = FALSE])
  as.vector(apply(X, 2, stats::weighted.mean, w = p$est_tot))
}

# centred design, zero-width when covs is empty (the swing-only ablation)
covs_dev = function(covs, p = prec) {
  if (!length(covs)) return(matrix(0, nrow(p), 0))
  sweep(as.matrix(p[, covs, drop = FALSE]), 2, covs_centre(covs, p), "-")
}

# One fixest fit on `lr ~ covs | county^mode`, with the per-county x mode gate applied to
# the fitted effects. Returns NULL when the fit cannot be trusted at all.
#   fr    rows to fit on, carrying lr, county, mode and the weight column
#   wcol  name of the weight column in fr
fit_cm = function(fr, covs, wcol, min_cells, min_grp, min_cm, tag) {
  if (nrow(fr) < min_cells) {
    cli_alert_info("hybrid/{tag}: only {nrow(fr)} usable cells (< {min_cells}) \\
                    -- offsets held at 0")
    return(NULL)
  }
  fr = dplyr::mutate(fr, .cm = paste(county, mode, sep = "\r"))
  n_cm = table(fr$.cm)
  keep = names(n_cm)[n_cm >= min_cm]
  if (length(keep) < min_grp) {
    cli_alert_info("hybrid/{tag}: only {length(keep)} county x mode group(s) with \\
                    >= {min_cm} cells (< {min_grp}) -- offsets held at 0")
    return(NULL)
  }
  form = stats::as.formula(paste("lr ~",
    if (length(covs)) paste(sprintf("`%s`", covs), collapse = " + ") else "1", "| .cm"))
  fit = tryCatch(fixest::feols(form, data = fr, weights = fr[[wcol]],
                               notes = FALSE, warn = FALSE),
                 error = function(e) NULL)
  if (is.null(fit)) {
    cli_alert_warning("hybrid/{tag}: fit failed -- offsets held at 0")
    return(NULL)
  }
  b = if (length(covs)) stats::coef(fit)[covs] else numeric(0)
  b[!is.finite(b)] = 0
  fe = tryCatch(fixest::fixef(fit, notes = FALSE)$.cm, error = function(e) NULL)
  if (is.null(fe)) {
    cli_alert_warning("hybrid/{tag}: no county x mode fixed effects recovered \\
                       -- offsets held at 0")
    return(NULL)
  }
  # THE GATE. A county x mode under min_cm cells is dropped entirely rather than shrunk:
  # it keeps its prior, which is the same principle as a county with no data borrowing
  # nothing, and it is what stops a 5-cell group from setting a statewide level.
  fe = fe[intersect(names(fe), keep)]
  # EVALUATE THE FIXED EFFECT AT THE MEAN COVARIATE VECTOR. fixest fits lr = X b + a_cm
  # with no intercept, so a_cm is the group effect at X = 0 -- and X = 0 is meaningless
  # here (log_n ~ 7.9, dem2 ~ 0.45; COVS_DERIVED is not standardised in
  # model_turnout.R section 1). The quantity we want is the group's level at the AVERAGE
  # precinct, a_cm + Xbar b, because the covariate term is applied CENTRED. Dropping this
  # constant instead of reattributing it is a pure level error: measured on the TX 2024
  # panel against the final canvass it put statewide turnout at 2.18x truth.
  fe = fe + sum(covs_centre(covs) * b)
  g = tibble::tibble(.cm = names(fe), fe = unname(fe)) |>
    tidyr::separate_wider_delim(.cm, delim = "\r", names = c("county", "mode")) |>
    dplyr::left_join(dplyr::count(fr, county, mode, name = "n_cells"),
                     by = c("county", "mode"))
  cli_inform("hybrid/{tag}: {nrow(fr)} cells, {nrow(g)} of {length(n_cm)} \\
              county x mode groups kept (>= {min_cm} cells)")
  list(b = b, g = g, n_rows = nrow(fr))
}

# Split the fitted county^mode effects into a per-county LEVEL and a per-county x mode
# COMPOSITION offset. g_cm ~ L_c + D_m (+ interaction): D_m is estimated by pooling
# ACROSS counties, which is what lets a county with only `early` counted out separate
# "this county turns out high" from "early is running ahead of its prior share". With
# every mode of a county gated in, L_c is just the composition-weighted mean and
# D_m drops out -- so the pooling only matters where it has to.
# Returns level (named by county) and comp (county, mode, off). Modes NOT gated in get no
# comp row, i.e. offset 0: a county x mode with no data keeps its prior share.
split_fe = function(g, a_cm, sweeps = 25L) {
  d = dplyr::left_join(g, a_cm, by = c("county", "mode")) |>
    dplyr::mutate(w = dplyr::coalesce(a, 0) + 1e-9)
  D = setNames(rep(0, length(MODES)), MODES)
  L = setNames(rep(0, length(unique(d$county))), unique(d$county))
  for (it in seq_len(sweeps)) {
    d$resid = d$fe - unname(L[d$county])
    Dn = d |> dplyr::summarize(v = stats::weighted.mean(resid, w), .by = mode)
    D[] = 0; D[Dn$mode] = Dn$v
    # centre D so the grand mean lives in the LEVEL, where it belongs: a statewide
    # turnout shift is a level, not a composition
    wm = d |> dplyr::summarize(w = sum(w), .by = mode)
    D = D - stats::weighted.mean(D[wm$mode], wm$w)
    d$resid = d$fe - unname(D[d$mode])
    Ln = d |> dplyr::summarize(v = stats::weighted.mean(resid, w), .by = county)
    L[] = 0; L[Ln$county] = Ln$v
  }
  comp = d |> dplyr::transmute(county, mode, off = fe - unname(L[county]))
  list(level = L, comp = comp)
}

# prior county x mode expected votes, the natural weight for both the split and the
# county constraints: A_cm = sum_{i in c} est_tot_i * mode_turn_share_im
prior_cm = function(cells) {
  cells |> dplyr::summarize(a = sum(est_tot * mode_turn_share), .by = c(county, mode))
}

# ---- the per-cell TURNOUT level + composition model ----------------------------------
# Returns: lvl  per-precinct log level offset, named by prec$geo
#          comp tibble(county, mode, off) of log composition offsets (missing = 0)
estimate_level = function(cells, returns, covs = HY_COVS_TURN) {
  zero = list(lvl = setNames(rep(0, nrow(prec)), prec$geo),
              comp = tibble::tibble(county = character(), mode = character(),
                                    off = numeric()),
              b = setNames(rep(0, length(covs)), covs), covs = covs)
  if (!HY_LEVEL && !HY_COMP) return(zero)
  # Same completeness preference order as fit_stage: the feed's own flag, else the
  # COMPLETE_FRAC * prior_tau fallback (poor, because it inherits prior_tau's error --
  # the very thing this estimator exists to correct -- but better than refusing to run).
  if (!"complete" %in% names(returns))
    returns = dplyr::mutate(returns, complete = NA)
  d = cells |>
    dplyr::left_join(returns, by = c("geo", "mode")) |>
    dplyr::mutate(
      tau  = dplyr::coalesce(votes_dem, 0) + dplyr::coalesce(votes_rep, 0) +
             dplyr::coalesce(votes_oth, 0),
      done = !is.na(votes_dem) & tau > 0 & prior_tau > 0 &
             dplyr::if_else(is.na(complete), tau >= COMPLETE_FRAC * prior_tau,
                            dplyr::coalesce(complete, FALSE)),
      lr   = log(tau / prior_tau)
    )
  fit = fit_cm(dplyr::filter(d, done), covs, "tau",
               ST_MIN_CELLS, ST_MIN_GRP, ST_MIN_CM, "turnout")
  if (is.null(fit)) return(zero)
  sp = split_fe(fit$g, prior_cm(cells))

  # Covariate part, for EVERY precinct including ones that have reported nothing. This
  # is the half the swing model cannot do, and it extrapolates on OBSERVABLES rather
  # than on an unobserved county level inferred from whoever reported first. CENTRED, so
  # its expected-vote-weighted statewide mean is 0 and it cannot move the level.
  Xc = covs_dev(covs)
  lvl = as.vector(Xc %*% fit$b)
  # County level, unshrunk. A county with no gated-in county x mode gets 0 -- it keeps
  # its prior level rather than borrowing one, which is the point.
  lvl = lvl + dplyr::coalesce(unname(sp$level[as.character(prec$county)]), 0)
  lvl = pmin(pmax(lvl, -ST_MAX_LEVEL), ST_MAX_LEVEL)
  comp = dplyr::mutate(sp$comp, off = pmin(pmax(off, -ST_MAX_COMP), ST_MAX_COMP))
  if (!HY_LEVEL) lvl[] = 0
  if (!HY_COMP)  comp = zero$comp
  if (HY_LEVEL)
    cli_inform("hybrid/turnout: level median {sprintf('%+.3f', stats::median(lvl))} \\
                ({sprintf('%+.1f%%', 100 * (exp(stats::median(lvl)) - 1))} on est_tot), \\
                range {sprintf('%+.3f', min(lvl))} to {sprintf('%+.3f', max(lvl))}")
  if (HY_COMP && nrow(comp))
    cli_inform("hybrid/comp: {nrow(comp)} county x mode offsets, \\
                median |off| {sprintf('%.3f', stats::median(abs(comp$off)))}, \\
                range {sprintf('%+.3f', min(comp$off))} to {sprintf('%+.3f', max(comp$off))}")
  list(lvl = setNames(lvl, prec$geo), comp = comp, b = fit$b, covs = covs)
}

# ---- the per-cell SHARE model --------------------------------------------------------
# logit(s_obs) - logit(prior_s) ~ COVS_SHARE | county^mode, on REPORTING cells, weighted
# by counted two-party ballots. Returns tibble(geo, mode, off) in LOGIT points.
estimate_share = function(cells, returns, covs = HY_COVS_SHARE) {
  zero = tibble::tibble(geo = character(), mode = character(), off = numeric())
  if (!HY_SHARE) return(zero)
  d = cells |>
    dplyr::left_join(returns, by = c("geo", "mode")) |>
    dplyr::mutate(
      tot2 = dplyr::coalesce(votes_dem, 0) + dplyr::coalesce(votes_rep, 0),
      tau  = tot2 + dplyr::coalesce(votes_oth, 0),
      # the same `obs` test fit_stage uses -- a cell's SHARES are usable as soon as it
      # reports, because a partial count is a sample of its electorate, not a bound
      done = !is.na(votes_dem) & stage >= STAGE_DONE & tau > 0 & tot2 > 0,
      # continuity-corrected, matching fit_stage's y_s, so a 0-Dem cell is finite
      lr   = logit((dplyr::coalesce(votes_dem, 0) + 0.5) / (tot2 + 1)) - logit(prior_s),
      w2   = pmax(tot2, 1)
    )
  fit = fit_cm(dplyr::filter(d, done, is.finite(lr)), covs, "w2",
               SH_MIN_CELLS, SH_MIN_GRP, SH_MIN_CM, "share")
  if (is.null(fit)) return(zero)
  xb = setNames(as.vector(covs_dev(covs) %*% fit$b), prec$geo)
  off = cells |>
    dplyr::select(geo, county, mode) |>
    dplyr::left_join(dplyr::select(fit$g, county, mode, fe), by = c("county", "mode")) |>
    dplyr::transmute(geo, mode,
                     off = pmin(pmax(unname(xb[geo]) + dplyr::coalesce(fe, 0),
                                     -SH_MAX_SHIFT), SH_MAX_SHIFT))
  cli_inform("hybrid/share: offset median {sprintf('%+.3f', stats::median(off$off))} logit, \\
              range {sprintf('%+.3f', min(off$off))} to {sprintf('%+.3f', max(off$off))}")
  off
}

# ---- county-level returns as exact aggregate constraints ------------------------------
# Reads a cbs_county_pull.R snapshot (schema_version 2) and returns, per county, the
# counted-out per-mode-GROUP vote totals to be reproduced. See the header for the three
# traps this function exists to handle.
county_targets = function(cells, snap = NULL) {
  none = tibble::tibble(county = character(), grp = character(), target = numeric())
  if (!HY_COUNTY) return(none)
  if (is.null(snap)) {
    # No snapshot passed and no path set: the arm is INERT, which is the right default
    # for a harness with no county feed. Production passes the frame explicitly --
    # live/R/counties.R's read_cbs_latest() is where it comes from.
    if (!nzchar(CTY_SNAPSHOT)) return(none)
    if (!file.exists(CTY_SNAPSHOT))
      cli_abort("CTY_SNAPSHOT={.path {CTY_SNAPSHOT}} does not exist.")
    snap = tibble::as_tibble(nanoparquet::read_parquet(CTY_SNAPSHOT))
  }
  snap = tibble::as_tibble(snap)
  need = c("county_cbs", "method", "votes", "county_total_vote",
           "county_pct_exp_vote", "county_precincts_in", "county_precincts_of")
  miss = setdiff(need, names(snap))
  if (length(miss))
    cli_abort(c("CBS county snapshot is missing {miss}.",
                "i" = "cbs_county_pull.R writes schema_version 2; a v1 snapshot is \\
                       mode-blind and cannot be used here."))
  if (!is.null(snap$schema_version) && any(snap$schema_version < 2L, na.rm = TRUE))
    cli_abort("CBS county snapshot is schema_version 1 (mode-blind); re-pull with by_method = TRUE.")

  # KEY ON THE PRECINCT ID's CBS COUNTY NUMBER, not the county name. The precinct ids are
  # tx_{county_cbs}_*, and cbs_county_pull.R already asserts that its county_cbs agrees
  # with that prefix -- so this is the one join that is checked at both ends.
  key = cells |>
    dplyr::distinct(county, geo) |>
    dplyr::mutate(county_cbs = stringr::str_extract(geo, "(?<=_)[0-9]{3}(?=_)")) |>
    dplyr::distinct(county, county_cbs) |>
    dplyr::filter(!is.na(county_cbs))
  if (!nrow(key) || !any(key$county_cbs %in% snap$county_cbs)) {
    cli_alert_warning("hybrid/county: no precinct id matches a CBS county_cbs \\
                       ({nrow(key)} county keys derived) -- county constraints skipped")
    return(none)
  }

  # COUNTY COMPLETENESS GATE. A partial county total is a lower bound, not a target.
  # county_pct_exp_vote is 0-100 in schema_version 2 (CBS files an integer percent). If
  # that ever becomes a 0-1 fraction, `pct >= 99` stops firing and this arm goes SILENTLY
  # inert, which is the worst failure mode available -- so check the scale, don't assume.
  if (any(is.finite(snap$county_pct_exp_vote)) &&
      max(snap$county_pct_exp_vote, na.rm = TRUE) <= 1.5)
    cli_abort(c("county_pct_exp_vote tops out at \
                 {max(snap$county_pct_exp_vote, na.rm = TRUE)}, which looks like a 0-1 \
                 fraction, not the 0-100 percent CTY_MIN_PCT={CTY_MIN_PCT} expects.",
                "x" = "Fix the scale here (and in cbs_county_pull.R) rather than letting \
                       the completeness gate never fire."))
  cty = snap |>
    dplyr::summarize(total = dplyr::first(county_total_vote),
                     pct = dplyr::first(county_pct_exp_vote),
                     pin = dplyr::first(county_precincts_in),
                     pof = dplyr::first(county_precincts_of),
                     method_votes = sum(votes, na.rm = TRUE),
                     .by = county_cbs) |>
    dplyr::mutate(cover = method_votes / pmax(total, 1),
                  done = is.finite(pct) & pct >= CTY_MIN_PCT &
                         is.finite(pin) & is.finite(pof) & pof > 0 & pin >= pof,
                  # a BAND, not a floor: on the TX gov test feed one county's method rows
                  # carry 1.37x its total, which is an over-count, not a shortfall, and
                  # scaling it down would silently launder a feed inconsistency
                  ok = done & is.finite(cover) &
                       cover >= CTY_MIN_COVER & cover <= 1 / CTY_MIN_COVER)
  n_in = sum(cty$county_cbs %in% key$county_cbs)
  n_ok = sum(cty$ok & cty$county_cbs %in% key$county_cbs)
  n_cov = sum(cty$done & !cty$ok & cty$county_cbs %in% key$county_cbs)
  # The completeness gate is STRICT on purpose, and it makes this a LATE-night
  # correction: a partially-counted county total is a lower bound, and using it as a
  # target understates turnout by exactly the uncounted share. The version that fires
  # earlier needs a trustworthy per-mode counted fraction, and cbs_county_pull.R records
  # why we do not have one -- exp_method is floored to rep_method when CBS files no
  # expectation, so a fraction of 1.0 cannot be told apart from "counted out".
  cli_inform("hybrid/county: {n_in} CBS counties in frame, {n_ok} counted out and \\
              >= {round(100*CTY_MIN_COVER)}% method coverage \\
              (median coverage {sprintf('%.3f', stats::median(cty$cover))}; \\
              {n_cov} counted out but outside the \
              [{CTY_MIN_COVER}, {round(1/CTY_MIN_COVER, 2)}] coverage band, skipped)")
  if (!n_ok) return(none)

  # Per method-GROUP totals, SCALED to the county total: the level comes from
  # county_total_vote (authoritative) and only the split from the method rows.
  tg = snap |>
    dplyr::inner_join(dplyr::filter(cty, ok) |> dplyr::select(county_cbs, cover),
                      by = "county_cbs") |>
    dplyr::summarize(v = sum(votes, na.rm = TRUE),
                     cover = dplyr::first(cover), .by = c(county_cbs, method)) |>
    dplyr::mutate(target = v / cover)
  grp = lapply(unique(tg$method), function(m)
    tibble::tibble(method = m, grp = paste(sort(cbs_method_group(m)), collapse = "+")))
  tg = tg |>
    dplyr::left_join(dplyr::bind_rows(grp), by = "method") |>
    dplyr::summarize(target = sum(target), .by = c(county_cbs, grp)) |>
    dplyr::inner_join(key, by = "county_cbs") |>
    dplyr::select(county, grp, target)
  # Overlapping groups inside one county would double-count (the `adv` trap). The pull
  # already aborts on it; assert here too, because this is where it would do damage.
  ov = tg |>
    dplyr::mutate(m = strsplit(grp, "+", fixed = TRUE)) |>
    tidyr::unnest(m) |>
    dplyr::count(county, m) |>
    dplyr::filter(n > 1)
  if (nrow(ov))
    cli_abort(c("{nrow(ov)} county x mode(s) are covered by more than one CBS method group.",
                "i" = "`adv` is early+mail combined and `all` is every mode; summing \\
                       either beside a per-method row double-counts.",
                "x" = "Offending: {paste(ov$county, ov$m, collapse = ', ')}"))
  tg
}

# Solve one county's level and composition offsets so the prior reproduces its targets.
# Level is closed form (it is one unknown against one equation, after the covariate term);
# composition is iterative proportional fitting over the method GROUPS, which is the
# multi-dimensional version of "one multiplicative offset per group".
#   E   per-precinct expected votes ALREADY carrying the covariate term
#   P   precinct x mode prior composition (rows sum to 1)
#   tg  named vector of group targets; names are "+"-joined mode sets
solve_county = function(E, P, tg, maxit = 100L, tol = 1e-10) {
  grp = lapply(names(tg), function(g) strsplit(g, "+", fixed = TRUE)[[1]])
  names(grp) = names(tg)
  # A target of exactly 0 (a complete county that filed no votes for a mode) would send
  # that group's offset to -Inf and the next iteration's ratio to NaN. Half a vote is
  # below any rounding anyone will ever see and keeps the solve finite.
  tg = pmax(tg, 0.5)
  covered = unlist(grp, use.names = FALSE)
  A = colSums(E * P)
  T_c = sum(tg) + sum(A[setdiff(colnames(P), covered)])
  lam = log(T_c / max(sum(A), 1e-9))
  Ea = E * exp(lam)
  o = setNames(rep(0, length(grp)), names(grp))
  for (it in seq_len(maxit)) {
    W = P
    for (g in names(grp)) W[, grp[[g]]] = W[, grp[[g]], drop = FALSE] * exp(o[[g]])
    W = W / rowSums(W)
    ach = vapply(grp, function(ms) sum(colSums(Ea * W)[ms]), 0)
    step = log(tg / pmax(ach, 1e-9))
    o = o + step
    if (max(abs(step)) < tol) break
  }
  off = setNames(rep(0, ncol(P)), colnames(P))
  for (g in names(grp)) off[grp[[g]]] = o[[g]]
  list(level = lam, off = off, iters = it, gap = max(abs(step)))
}

# Fold the county constraints into the regression-derived offsets. A constrained county's
# level and composition come ENTIRELY from its constraint -- a complete county total is
# data about that county and a fitted FE is an estimate of the same thing, so applying
# both would double-count. The covariate term is kept (it redistributes WITHIN the county
# and the level is solved around it), so the aggregate stays exact.
# NOTE this arm supplies a level even with HY_LEVEL=0, deliberately: HY_LEVEL switches off
# the REGRESSION-derived level, and a complete county total is not a regression.
apply_county = function(cells, off, tg) {
  if (!nrow(tg)) return(off)
  covs = off$covs
  Xc = covs_dev(covs)
  # the covariate-only part of the level, which a constrained county KEEPS (it
  # redistributes within the county; the constraint's level is solved around it)
  xb = pmin(pmax(as.vector(Xc %*% off$b), -ST_MAX_LEVEL), ST_MAX_LEVEL)
  lvl = off$lvl; comp = off$comp
  wide = cells |>
    dplyr::select(geo, mode, mode_turn_share) |>
    tidyr::pivot_wider(names_from = mode, values_from = mode_turn_share) |>
    dplyr::arrange(match(geo, prec$geo))
  P_all = as.matrix(wide[, MODES, drop = FALSE])
  rownames(P_all) = wide$geo
  binds = 0L; nclamp = 0L; gaps = numeric()
  for (cty in unique(tg$county)) {
    i = which(prec$county == cty)
    if (!length(i)) next
    tv = tg$target[tg$county == cty]; names(tv) = tg$grp[tg$county == cty]
    s = solve_county(prec$est_tot[i] * exp(xb[i]),
                     P_all[prec$geo[i], , drop = FALSE], tv)
    gaps = c(gaps, s$gap)
    if (abs(s$level) > CTY_MAX_LEVEL) {
      binds = binds + 1L
      s$level = sign(s$level) * CTY_MAX_LEVEL
    }
    lvl[prec$geo[i]] = xb[i] + s$level
    cl = pmin(pmax(unname(s$off), -ST_MAX_COMP), ST_MAX_COMP)
    nclamp = nclamp + sum(abs(cl - unname(s$off)) > 1e-9)
    comp = dplyr::filter(comp, county != cty) |>
      dplyr::bind_rows(tibble::tibble(county = cty, mode = names(s$off), off = cl))
  }
  if (nclamp)
    cli_alert_warning("hybrid/county: {nclamp} constraint-derived composition offset(s) \\
                       hit +/-{ST_MAX_COMP}, so those counties' targets are NO LONGER \\
                       reproduced exactly. A mode whose CBS total is far from its prior \\
                       share is the usual cause -- check the mode history for them.")
  if (binds)
    cli_alert_warning("hybrid/county: the constraint-derived level hit \\
                       +/-{CTY_MAX_LEVEL} in {binds} county(ies). A constraint is data, \\
                       not a fit, so this means a unit or join error -- check that the \\
                       CBS county totals and est_tot are the same contest.")
  cli_inform("hybrid/county: {dplyr::n_distinct(tg$county)} county(ies) constrained to \\
              their CBS totals (IPF residual max {sprintf('%.2e', max(c(gaps, 0)))})")
  off$lvl = lvl; off$comp = comp
  off
}

# ---- re-centre the priors, leave everything else alone -------------------------------
recentre_prior = function(cells, off) {
  # acceptance test (a): with every arm off this is the identity, so fit_stage_hybrid()
  # IS fit_stage() -- not "equal to within tolerance", the same object.
  if (!HY_LEVEL && !HY_COMP && !HY_SHARE && !HY_COUNTY) return(cells)
  # tolerate a partially-built offset list, so a diagnostic can exercise one arm alone
  if (is.null(off$lvl))   off$lvl   = setNames(rep(0, nrow(prec)), prec$geo)
  if (is.null(off$comp))  off$comp  = tibble::tibble(county = character(),
                                                     mode = character(), off = numeric())
  if (is.null(off$share)) off$share = tibble::tibble(geo = character(),
                                                     mode = character(), off = numeric())
  m = unname(off$lvl[match(cells$geo, names(off$lvl))]); m[is.na(m)] = 0
  out = cells |>
    dplyr::left_join(dplyr::rename(off$comp, .coff = off), by = c("county", "mode")) |>
    dplyr::mutate(
      est_tot_adj = est_tot * exp(m),
      # composition: offset, then RENORMALISE per precinct. The renormalisation is what
      # makes the level and the composition compose exactly -- sum_m mode_turn_share is
      # 1 whatever the offsets are, so a composition offset cannot move the total.
      mode_turn_share = mode_turn_share * exp(dplyr::coalesce(.coff, 0))
    ) |>
    dplyr::select(-.coff) |>
    dplyr::mutate(mode_turn_share = mode_turn_share / sum(mode_turn_share), .by = geo) |>
    dplyr::mutate(ref_share = mode_turn_share[match(REF_MODE, mode)], .by = geo) |>
    dplyr::mutate(
      prior_tau = est_tot_adj * mode_turn_share,
      y_tau_prior = if (!USE_COMPOSE) log(prior_tau + 0.1)
                    else dplyr::if_else(
                      mode == REF_MODE,
                      logit(pmin(pmax(est_tot_adj / cap, 1e-4), 1 - 1e-4)),
                      log(pmax(mode_turn_share, 1e-6) / pmax(ref_share, 1e-6)))
    )
  if (nrow(off$share)) {
    out = out |>
      dplyr::left_join(dplyr::rename(off$share, .soff = off), by = c("geo", "mode")) |>
      dplyr::mutate(
        prior_s   = invlogit(logit(prior_s) + dplyr::coalesce(.soff, 0)),
        y_s_prior = logit(prior_s)
      ) |>
      dplyr::select(-.soff)
  }
  out
}

# ---- the one overridden step ---------------------------------------------------------
# `...` goes straight to fit_stage (draws, cross_mode, Sigma_hist, level, ...), so the
# signature never needs touching when that one changes. cbs_county sits AFTER `...` on
# purpose: it must be passed by name, so a positional fit_stage argument can never land
# in it. It is a CBS county x method snapshot (live/R/counties.R's read_cbs_latest(), or
# cbs_county_pull.R's return value) for THIS race; NULL and no CTY_SNAPSHOT leaves the
# county-constraint arm inert.
fit_stage_hybrid = function(cells, returns, ..., cbs_county = NULL) {
  if ((HY_LEVEL || HY_COUNTY) && isTRUE(get0("LEVEL_UPDATE", ifnotfound = FALSE))) {
    cli_abort(c("LEVEL_UPDATE=1 and model_hybrid apply the level correction TWICE.",
                "i" = "sum(prior_tau) already carries exp(level_i) from recentre_prior(); \
                       level_tilt then multiplies it again.",
                "x" = "Set LEVEL_UPDATE=0 (or HY_LEVEL=0 HY_COUNTY=0) when using model_hybrid."))
  }
  if ((HY_COMP || HY_COUNTY) && isTRUE(get0("COMPOSITION_UPDATE", ifnotfound = FALSE))) {
    cli_abort(c("COMPOSITION_UPDATE=1 and model_hybrid apply the composition tilt TWICE.",
                "i" = "mode_turn_share already carries exp(comp_cm) from recentre_prior(); \
                       comp_tilt then re-derives the same shift from the same returns.",
                "x" = "Set COMPOSITION_UPDATE=0 (or HY_COMP=0 HY_COUNTY=0) when using model_hybrid."))
  }
  off = estimate_level(cells, returns)
  if (HY_COUNTY) off = apply_county(cells, off, county_targets(cells, cbs_county))
  off$share = estimate_share(cells, returns)
  fit_stage(recentre_prior(cells, off), returns, ...)
}

# ---- self-check ---------------------------------------------------------------------
# Not a test of whether the design WORKS -- only that it runs, and that it is INERT both
# with no returns and with every arm switched off (acceptance test (a)).
if (sys.nframe() == 0L) {
  empty = tibble::tibble(geo = character(), mode = character(), stage = integer(),
                         complete = logical(), votes_dem = numeric(),
                         votes_rep = numeric(), votes_oth = numeric())
  off0 = estimate_level(cells, empty)
  off0$share = estimate_share(cells, empty)
  stopifnot("level must be 0 with no returns" = all(off0$lvl == 0),
            "comp must be empty with no returns" = nrow(off0$comp) == 0,
            "share must be empty with no returns" = nrow(off0$share) == 0)
  c0 = recentre_prior(cells, off0)
  stopifnot(
    "turnout prior must be unchanged when offsets are 0" =
      isTRUE(all.equal(c0$y_tau_prior, cells$y_tau_prior)),
    "share prior must be unchanged when offsets are 0" =
      isTRUE(all.equal(c0$y_s_prior, cells$y_s_prior)),
    "composition must be unchanged when offsets are 0" =
      isTRUE(all.equal(c0$mode_turn_share, cells$mode_turn_share)))
  cli_alert_success("model_hybrid self-check passed: inert with no returns")
}

# ========================================
## MEASURED, 2026-10-26. Every number below is from a run on this tree; the two
## corrections to the earlier `model_both` story are flagged as such.
##
## (a) ALL ARMS OFF == model_turnout.R, BIT-IDENTICALLY. PASS.
##     HY_LEVEL=0 HY_COMP=0 HY_SHARE=0 HY_COUNTY=0 -> recentre_prior() returns `cells`
##     itself (identical(), not all.equal()), and fit_stage_hybrid() vs fit_stage() on the
##     TX 2024 panel at 22:30 are identical() on S_dem, S_2p, turn, prec and every
##     non-`fits` slot. Re-checked against model_turnout.R after its prec_out/`level`
##     changes landed.
##
## (b) tx_turnout_low, LEVEL ARM (HY_COMP=0 HY_SHARE=0 HY_COUNTY=0).
##     Statewide turnout as % of truth, gov:
##
##       counted      0%   3.5%   39%   63%   71%   74%   77%   89%
##       base        119    119   118   116   114   113   112   107
##       model_both  119    131   110   101  99.8 100.0 100.2 100.9
##       hybrid      119    118   108   102   101   102   102   103
##       ORACLE       --     --    101   102   103   104   104   105
##
##     The 3.5% overshoot is FIXED: 131% -> 118%, below base's 119%, and the per-county x
##     mode gate is solely responsible (level +0.0962 -> -0.0108 at 20:15; centring moves
##     it by nothing on this frame). The late figures do
##     NOT reach model_both's 99.8-100.3%, and the ORACLE row is why: that is
##     sim_tx_turnout_low__stateoracle, the same model TOLD the true statewide turnout,
##     and it reads 102-105% from 21:00. No prior-level estimator can reach 100% through
##     this pipeline, because the pipeline is ~2-5% high GIVEN a correct level.
##
##     CORRECTION TO THE model_both RESULT: its 99.8-100.3% was two errors cancelling,
##     not a level estimate. Against tx_turnout_low's true level of -0.1738 (log), on the
##     fit's own counted-out cells:
##       model_both (uncentred X b + additive county FE)  -0.1815  (0.8% over-corrected)
##       hybrid (centred + county^mode split)   22:30     -0.1716  (0.2% off)
##                                              04:00     -0.1744  (0.06% off)
##     model_both over-corrected the prior by about as much as the pipeline over-shoots,
##     in the opposite direction. The hybrid recovers the level and so inherits the
##     pipeline's own drift -- and still beats the oracle at every late checkpoint, since
##     it lands slightly conservative. The remaining 2-3% is a model_turnout.R question
##     (where a correct statewide level still produces 104% at 04:00), not a prior one.
##
## (c) SHARE ARM, REAL TX 2024 BACKTEST. PASS -- it improves, it does not degrade.
##     Re-baselined on this tree first (the 0.44pp in older notes is stale):
##       base                       two-party coverage 12/15, max 1.58pp, mean 0.45pp
##       HY_SHARE only              two-party coverage 13/15, max 1.58pp, mean 0.43pp
##       all arms (county inert)    two-party coverage 12/15, max 1.58pp, mean 0.43pp
##     max |err| is identical in all three because it is the 18:45 checkpoint, where the
##     share estimator is still below its group gate.
##
##     AND THE "SHARE HAS MORE DATA, EARLIER" CLAIM IS MEASURED, on real returns:
##       19:00  share 2,257 reporting cells / 8 groups   turnout  10 counted-out cells
##       19:15  share 3,499 / 14                         turnout  10
##       20:00  share 4,329 / 22                         turnout  40
##       21:00  share 5,202 / 36                         turnout 335  <- first turnout fit
##     So the share arm is live two hours before the turnout arm can be fitted at all.
##     That is the whole argument for using REPORTING cells rather than counted-out ones.
##
## (d) COVARIATE ABLATION. The gain SURVIVES the interaction, and it GREW.
##     tx_turnout_demo, median |log turnout error| on the 504 precincts with NOTHING
##     counted at 04:00, and the correlation of that error with precinct Hispanic share:
##
##       prior (est_turnout)                     0.1278   cor +0.871
##       uniform swing model                     0.1018   cor +0.437
##       hybrid level, county^mode FE only       0.0969   cor +0.460
##       hybrid level, county^mode + covariates  0.0713   cor +0.115
##
##     Read the third row: `county^mode` ALONE is barely better than the uniform swing
##     model (0.0969 vs 0.1018) and does nothing at all for the demographic correlation.
##     The worry that the interaction would absorb the covariates' job was wrong, and the
##     reason is structural -- this misspecification is WITHIN county, and a county x mode
##     fixed effect is still a county-level object however finely it is cut. The
##     covariates cut the error a further 26% and the demographic correlation by 4x, and
##     0.0713 beats the 0.083 recorded for `| county + mode`.
##
## (e) tx_modeshift, LEVEL + COMPOSITION (HY_SHARE=0 HY_COUNTY=0), ONE SEED.
##     Mean |turnout error| 1.30% -> 0.89%; mean |two-party error| 0.073pp -> 0.155pp;
##     interval coverage 71/72 both ways. The mid-night turnout curve goes
##     101.0-101.2% (base) -> 98.7-101.0% (hybrid), i.e. it crosses truth instead of
##     sitting above it.
##
## THE SHARE COST IS REAL AND CONSISTENT, AND IT IS THE PRICE OF THE TURNOUT GAIN.
##     Same pattern on tx_turnout_low (level arm only): mean |two-party error|
##     0.109pp -> 0.229pp while mean |turnout error| goes 10.14% -> 3.36%. Moving the
##     turnout prior re-weights which precincts carry the statewide share, and in these
##     scenarios the base share is already within ~0.1pp of truth, so re-weighting can
##     only cost. Coverage never moves (71/72 and 71/72), and on the REAL TX 2024
##     backtest the share arm IMPROVES coverage -- so this is a sim artefact of a share
##     that was already essentially exact, not a reason to turn the level arm off.
##     It is ONE SEED either way. Per the plan's own warning, single-seed share
##     comparisons at 0.1-0.5pp are not resolvable in this sim, and four "refutations"
##     on 2026-10-22 shared a seed and were weaker than they looked. The second seed is
##     still OPEN, below.
##
## (f) COUNTY CONSTRAINTS. Exact, on the real feed's shapes.
##     Applied the live CBS county x method snapshot (TX gov test feed, 2026-10-26,
##     schema_version 2) to the live TX cells: 31 county x method-group targets across 11
##     counties, reproduced by the re-centred prior to a maximum relative error of 2e-12,
##     with precinct totals preserved to 2e-16. The `adv` counties resolved to an
##     `early+mail` GROUP target, as designed, and 2 counties were skipped by the
##     coverage band.
##     TWO LIMITS, both real: that run had to mark counties at >= 99% as counted out,
##     because NO TX county in the test feed has precinctsIn >= precinctsOf -- the strict
##     completeness gate constrains NOTHING on a mid-count snapshot, by design. And the
##     coverage band is doing work: one county's method rows carry 1.37x its own total.
# ========================================

# ========================================
## ACCEPTANCE TESTS -- how to reproduce MEASURED above, and what is still OPEN
##
## (a) Rscript scripts/model/model_hybrid.R                      (the inert self-check)
##     plus: source model_turnout.R, set all four HY_* to 0, and assert
##     identical(recentre_prior(cells, off), cells) and identical() on fit_stage's
##     output. Re-run this after ANY model_turnout.R change -- it is the one test that
##     catches drift, and it is cheap.
## (b) [retired harness] run_model_tests.R tx_turnout_low --variants=hyblvl    against the base run, and
##     against sim_tx_turnout_low__stateoracle_results.csv, which is the ceiling.
## (c) PLOT=0 Rscript scripts/model/tx24_backtest.R           for the baseline, FIRST,
##     then the same with fit_stage -> fit_stage_hybrid. Always re-baseline: that file
##     reads whatever model_turnout.R is on disk, and it moved twice on 2026-10-26.
## (d) [retired harness] run_model_tests.R tx_turnout_demo --variants=hyblvl,hybnocov --dump-prec=04:00,
##     then median |log(turn_est / t_tot)| on rows with c_tot == 0.
## (f) Apply a cbs_county_pull.R snapshot via CTY_SNAPSHOT and assert the AGGREGATE, not
##     the aggregate's effect: sum_{i in c} prior_tau_im over a constrained county x mode
##     GROUP must equal its coverage-scaled CBS target to within rounding.
##
## OPEN, in priority order:
## (e) tx_modeshift SECOND SEED, to resolve the 0.073 -> 0.155pp share cost in
##     MEASURED (e). One seed cannot settle a 0.08pp difference in this sim, and the
##     direction matters: it is the only measured cost of the level/composition arms.
## (g) The NC case the composition arm was designed for: a county whose early votes
##     arrive only as a site bucket should show a composition offset in the right
##     direction and move its precincts' early shares. Needs the bucket geos PARSED
##     rather than dropped (cell_counts()'s drop diagnostic is the prerequisite), so it
##     cannot be run from the TX harnesses at all.
## (h) The pipeline drift (b) exposes: model_turnout.R reads 104% of truth at 04:00 on
##     tx_turnout_low even when handed the TRUE statewide level. That is upstream of
##     every prior this file sets, and it is now the binding constraint on turnout
##     accuracy in that scenario.
## (i) Production wiring: live/R/model_medsl.R calls fit_stage() and would need
##     fit_stage_hybrid(cells, returns, cbs_county = read_cbs_latest(...)). Not done
##     here -- that file is not this file's to edit.
##
## Background: quality_reports/turnout_findings_2026-10-22.md
# ========================================
