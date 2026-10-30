# ========================================
## model_hybrid -- swing-style re-centring of the precinct x mode model's PRIORS
##
## Used by medsl_sur (live/R/model_medsl.R): sourced into the same environment as
## model_turnout.R, and fit_stage_hybrid() is called in place of fit_stage(). Four arms,
## each switched off with HY_LEVEL / HY_COMP / HY_SHARE / HY_COUNTY = 0; all four off
## reproduces fit_stage() exactly. The share arm acts from the first returns; the level
## arm once counted-out cells exist; the county-constraint arm only on counted-out
## counties in the CBS snapshot, so late in the night.
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
## become 0.1278 / 0.1018 / 0.0713 -- the covariate half gets BETTER, not worse.
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

# ========================================

stopifnot("needs fixest" = requireNamespace("fixest", quietly = TRUE))

# ---- switches -----------------------------------------------------------------------
# Four independent arms. With all four at 0 recentre_prior() returns `cells` unmodified,
# so fit_stage_hybrid() and fit_stage() agree bit for bit.
HY_LEVEL = Sys.getenv("HY_LEVEL", unset = "1") == "1" # theta's prior
HY_COMP = Sys.getenv("HY_COMP", unset = "1") == "1" # eta's prior
HY_SHARE = Sys.getenv("HY_SHARE", unset = "1") == "1" # s's prior
HY_COUNTY = Sys.getenv("HY_COUNTY", unset = "1") == "1" # county aggregate constraints

# How many counted-out cells before the TURNOUT level/composition model is fitted at all.
# This is now only a "can the covariate slopes be estimated" gate, not the level gate:
# below it nothing is applied and the result is exactly model_turnout.R.
ST_MIN_CELLS = as.integer(Sys.getenv("ST_MIN_CELLS", unset = "200"))
# ...and how many distinct gated-in county x mode groups, for the same reason. One
# county's worth of cells identifies a county, not a covariate slope.
ST_MIN_GRP = as.integer(Sys.getenv("ST_MIN_GRP", unset = "5"))
# PER COUNTY x MODE gate: below this many counted-out cells that county x mode's fitted
# effect is DISCARDED and it keeps its prior. This is the overshoot fix; see the header.
ST_MIN_CM = as.integer(Sys.getenv("ST_MIN_CM", unset = "10"))
# Clamp on the fitted level, in log points. exp(+/-0.5) is roughly a 1.65x / 0.61x swing
# in statewide turnout -- far outside anything real, so this is a guard against a
# degenerate fit on thin early data, not a modelling choice.
ST_MAX_LEVEL = as.numeric(Sys.getenv("ST_MAX_LEVEL", unset = "0.5"))
# Clamp on a composition offset, in log points. Looser than the level on purpose: mode
# MIX genuinely moves by a factor of two between counties (80% early vs 40% early is
# 0.69 in log points) where the turnout LEVEL does not.
ST_MAX_COMP = as.numeric(Sys.getenv("ST_MAX_COMP", unset = "1.0"))

# Share arm, mirroring the turnout gates. Separate constants because the share estimator
# runs on REPORTING cells rather than counted-out ones, so it has far more rows, earlier.
SH_MIN_CELLS = as.integer(Sys.getenv("SH_MIN_CELLS", unset = "200"))
SH_MIN_GRP = as.integer(Sys.getenv("SH_MIN_GRP", unset = "5"))
SH_MIN_CM = as.integer(Sys.getenv("SH_MIN_CM", unset = "10"))
# Clamp in LOGIT points on the two-party share prior. 0.25 logit is ~6pp at 50-50, which
# is already a very large statewide share surprise (calibrate_swing.R: 9 states x 37
# contest pairs move 0.22-0.26 logit between a prior contest and its target).
SH_MAX_SHIFT = as.numeric(Sys.getenv("SH_MAX_SHIFT", unset = "0.25"))

# County-constraint arm. The snapshot normally arrives as fit_stage_hybrid()'s
# `cbs_county` argument; CTY_SNAPSHOT is a file-path fallback for testing, pointing at a
# parquet written by live/R/cbs_county_pull.R (schema_version 2). With neither, inert.
CTY_SNAPSHOT = Sys.getenv("CTY_SNAPSHOT", unset = "")
# minimum share of a county's total vote that its per-method rows account for before the
# method split is believed at all (see the 10.6% shortfall in the header)
CTY_MIN_COVER = as.numeric(Sys.getenv("CTY_MIN_COVER", unset = "0.80"))
# a county's total is a CONSTRAINT only once it is counted out; below this it is a lower
# bound and using it as a target would understate turnout by the uncounted share
CTY_MIN_PCT = as.numeric(Sys.getenv("CTY_MIN_PCT", unset = "99"))
# Clamp on a CONSTRAINT-derived level. Wider than ST_MAX_LEVEL because a constraint is
# data, not a fit -- so if it binds, that is a unit or join error worth seeing, and the
# warning says so.
CTY_MAX_LEVEL = as.numeric(Sys.getenv("CTY_MAX_LEVEL", unset = "1.0"))

#' Covariate set for a re-centring estimator
#'
#' Overridable independently of the model's own COVS_TURN / COVS_SHARE. "none" gives the
#' swing-only estimator (`lr ~ 1 | county^mode`); "" cannot mean that, because `cov_env()`
#' reads an empty variable as unset.
#'
#' @param var Environment variable (HY_COVS_TURN or HY_COVS_SHARE).
#' @param default Covariates when unset.
#' @return Character vector; empty for "none".
hy_covs = function(var, default) {
  if (identical(Sys.getenv(var), "none")) character(0) else cov_env(var, default)
}
HY_COVS_TURN = hy_covs("HY_COVS_TURN", COVS_TURN)
HY_COVS_SHARE = hy_covs("HY_COVS_SHARE", COVS_SHARE)

#' CBS county-feed method -> the model modes it constrains
#'
#' `adv` is early + mail combined and `all` is every mode, so each constrains a group and
#' leaves the split inside it at the prior. `prov` must already be gone (`reassign_prov =
#' TRUE` in the pull).
#'
#' @param method One CBS method label.
#' @return Character vector of the model modes in its group (within `MODES`). Stops on an
#'   unknown method or one that maps to none of this state's modes.
cbs_method_group = function(method) {
  # A state modelled without vote method has one pseudo-mode: every CBS method, whatever
  # it is called, constrains that single group. Without this, `adv` intersects
  # c("early","mail") with "all", gets the empty set, and aborts.
  if (identical(MODES, "all")) {
    return("all")
  }
  g = switch(
    method,
    eday = "eday",
    early = "early",
    mail = "mail",
    adv = intersect(c("early", "mail"), MODES),
    all = MODES,
    cli_abort(c(
      "CBS method {.val {method}} has no mapping to the model's modes.",
      "i" = "`prov` should be absorbed upstream by reassign_prov = TRUE.",
      "x" = "Add a mapping in cbs_method_group() rather than guessing here."
    ))
  )
  g = intersect(g, MODES)
  # An empty group means CBS is reporting a method this state's MODES does not model --
  # e.g. `adv` against the degenerate MODES = "all" path. Silently dropping it would
  # throw away the county's votes; silently keeping it would break the IPF.
  if (!length(g)) {
    cli_abort(c(
      "CBS method {.val {method}} maps to none of this state's MODES \
                 ({paste(MODES, collapse = '/')}).",
      "x" = "Reconcile lookups/mode_scope.csv with the CBS vocabulary before \
                       constraining on this feed."
    ))
  }
  g
}

# ---- shared machinery ---------------------------------------------------------------
#' Expected-vote-weighted statewide mean of the design
#'
#' A covariate term built from it has zero statewide level effect by construction.
#'
#' @param covs Covariate names.
#' @param p Precinct frame.
#' @return Numeric vector of means, one per covariate.
covs_centre = function(covs, p = prec) {
  if (!length(covs)) {
    return(numeric(0))
  }
  X = as.matrix(p[, covs, drop = FALSE])
  as.vector(apply(X, 2, stats::weighted.mean, w = p$est_tot))
}

#' Centred design matrix
#'
#' @param covs Covariate names; empty for the swing-only ablation.
#' @param p Precinct frame.
#' @return Matrix (precincts x covariates), zero columns when `covs` is empty.
covs_dev = function(covs, p = prec) {
  if (!length(covs)) {
    return(matrix(0, nrow(p), 0))
  }
  sweep(as.matrix(p[, covs, drop = FALSE]), 2, covs_centre(covs, p), "-")
}

#' One fixest fit of `lr ~ covs | county^mode` with the county x mode gate
#'
#' @param fr Rows to fit.
#' @param covs Covariate names.
#' @param wcol Weight column.
#' @param min_cells Minimum rows for the fit at all.
#' @param min_grp Minimum distinct groups.
#' @param min_cm Minimum rows for a county x mode effect to be kept.
#' @param tag Label for the log line.
#' @return List: b (slopes), g (kept county x mode effects), n_rows; NULL when the fit can't
#'   be trusted.
fit_cm = function(fr, covs, wcol, min_cells, min_grp, min_cm, tag) {
  if (nrow(fr) < min_cells) {
    cli_alert_info(
      "hybrid/{tag}: only {nrow(fr)} usable cells (< {min_cells}) \\
                    -- offsets held at 0"
    )
    return(NULL)
  }
  fr = dplyr::mutate(fr, .cm = paste(county, mode, sep = "\r"))
  n_cm = table(fr$.cm)
  keep = names(n_cm)[n_cm >= min_cm]
  if (length(keep) < min_grp) {
    cli_alert_info(
      "hybrid/{tag}: only {length(keep)} county x mode group(s) with \\
                    >= {min_cm} cells (< {min_grp}) -- offsets held at 0"
    )
    return(NULL)
  }
  form = stats::as.formula(paste(
    "lr ~",
    if (length(covs)) paste(sprintf("`%s`", covs), collapse = " + ") else "1",
    "| .cm"
  ))
  fit = tryCatch(
    fixest::feols(form, data = fr, weights = fr[[wcol]], notes = FALSE, warn = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    cli_alert_warning("hybrid/{tag}: fit failed -- offsets held at 0")
    return(NULL)
  }
  b = if (length(covs)) stats::coef(fit)[covs] else numeric(0)
  b[!is.finite(b)] = 0
  fe = tryCatch(fixest::fixef(fit, notes = FALSE)$.cm, error = function(e) NULL)
  if (is.null(fe)) {
    cli_alert_warning(
      "hybrid/{tag}: no county x mode fixed effects recovered \\
                       -- offsets held at 0"
    )
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
    dplyr::left_join(dplyr::count(fr, county, mode, name = "n_cells"), by = c("county", "mode"))
  cli_inform(
    "hybrid/{tag}: {nrow(fr)} cells, {nrow(g)} of {length(n_cm)} \\
              county x mode groups kept (>= {min_cm} cells)"
  )
  list(b = b, g = g, n_rows = nrow(fr))
}

#' Split county^mode effects into level and composition
#'
#' g_cm ~ L_c + D_m: D_m is pooled across counties, which lets a county with only early
#' counted out separate "turns out high" from "early is ahead of its prior share". With
#' every mode of a county gated in, L_c is the composition-weighted mean.
#'
#' @param g Kept county x mode effects (`fit_cm()`).
#' @param a_cm Prior county x mode expected votes (`prior_cm()`).
#' @param sweeps Alternating-fit iterations.
#' @return List: level (named by county) and comp (county, mode, off). Modes not gated in
#'   get no comp row, i.e. offset 0.
split_fe = function(g, a_cm, sweeps = 25L) {
  d = dplyr::left_join(g, a_cm, by = c("county", "mode")) |>
    dplyr::mutate(w = dplyr::coalesce(a, 0) + 1e-9)
  D = setNames(rep(0, length(MODES)), MODES)
  L = setNames(rep(0, length(unique(d$county))), unique(d$county))
  for (it in seq_len(sweeps)) {
    d$resid = d$fe - unname(L[d$county])
    Dn = d |> dplyr::summarize(v = stats::weighted.mean(resid, w), .by = mode)
    D[] = 0
    D[Dn$mode] = Dn$v
    # centre D so the grand mean lives in the LEVEL, where it belongs: a statewide
    # turnout shift is a level, not a composition
    wm = d |> dplyr::summarize(w = sum(w), .by = mode)
    D = D - stats::weighted.mean(D[wm$mode], wm$w)
    d$resid = d$fe - unname(D[d$mode])
    Ln = d |> dplyr::summarize(v = stats::weighted.mean(resid, w), .by = county)
    L[] = 0
    L[Ln$county] = Ln$v
  }
  comp = d |> dplyr::transmute(county, mode, off = fe - unname(L[county]))
  list(level = L, comp = comp)
}

#' Prior county x mode expected votes
#'
#' A_cm = sum over precincts in c of est_tot_i x mode_turn_share_im: the weight for the
#' split and for the county constraints.
#'
#' @param cells Cells.
#' @return Tibble: county, mode, a.
prior_cm = function(cells) {
  cells |> dplyr::summarize(a = sum(est_tot * mode_turn_share), .by = c(county, mode))
}

# ---- the per-cell TURNOUT level + composition model ----------------------------------
#' Turnout level and composition offsets from counted-out cells
#'
#' `log(c_im / prior_tau_im) ~ covs | county^mode` on counted-out cells (file header), split
#' into level and composition (`split_fe()`), gated per county x mode and capped by
#' `ST_MAX_LEVEL` / `ST_MAX_COMP`.
#'
#' @param cells Cells.
#' @param returns Precinct x mode returns (as `fit_stage()` takes).
#' @param covs Covariates (`HY_COVS_TURN`).
#' @return List: lvl (per-precinct log level offset, named by geo) and comp (county, mode,
#'   off: log composition offsets; missing = 0).
estimate_level = function(cells, returns, covs = HY_COVS_TURN) {
  zero = list(
    lvl = setNames(rep(0, nrow(prec)), prec$geo),
    comp = tibble::tibble(county = character(), mode = character(), off = numeric()),
    b = setNames(rep(0, length(covs)), covs),
    covs = covs
  )
  if (!HY_LEVEL && !HY_COMP) {
    return(zero)
  }
  # Same completeness preference order as fit_stage, including complete_usable(): a
  # `complete` column that is present but never TRUE is treated as absent, because taking
  # it at face value holds this estimator at zero rows for the whole night -- which is the
  # production state (see complete_usable() in model_turnout.R). The COMPLETE_FRAC
  # fallback does inherit prior_tau's error, the very thing this estimator exists to
  # correct, but that error is a BOUNDED per-mode counted-fraction gate
  # (g >= COMPLETE_FRAC * prior/true) rather than an unbounded one, and it beats refusing
  # to run.
  if (!complete_usable(returns)) {
    returns = dplyr::mutate(returns, complete = NA)
  }
  d = cells |>
    dplyr::left_join(returns, by = c("geo", "mode")) |>
    dplyr::mutate(
      tau = dplyr::coalesce(votes_dem, 0) +
        dplyr::coalesce(votes_rep, 0) +
        dplyr::coalesce(votes_oth, 0),
      done = !is.na(votes_dem) &
        tau > 0 &
        prior_tau > 0 &
        dplyr::if_else(
          is.na(complete),
          tau >= COMPLETE_FRAC * prior_tau,
          dplyr::coalesce(complete, FALSE)
        ),
      lr = log(tau / prior_tau)
    )
  fit = fit_cm(dplyr::filter(d, done), covs, "tau", ST_MIN_CELLS, ST_MIN_GRP, ST_MIN_CM, "turnout")
  if (is.null(fit)) {
    return(zero)
  }
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
  if (!HY_LEVEL) {
    lvl[] = 0
  }
  if (!HY_COMP) {
    comp = zero$comp
  }
  if (HY_LEVEL) {
    cli_inform(
      "hybrid/turnout: level median {sprintf('%+.3f', stats::median(lvl))} \\
                ({sprintf('%+.1f%%', 100 * (exp(stats::median(lvl)) - 1))} on est_tot), \\
                range {sprintf('%+.3f', min(lvl))} to {sprintf('%+.3f', max(lvl))}"
    )
  }
  if (HY_COMP && nrow(comp)) {
    cli_inform(
      "hybrid/comp: {nrow(comp)} county x mode offsets, \\
                median |off| {sprintf('%.3f', stats::median(abs(comp$off)))}, \\
                range {sprintf('%+.3f', min(comp$off))} to {sprintf('%+.3f', max(comp$off))}"
    )
  }
  list(lvl = setNames(lvl, prec$geo), comp = comp, b = fit$b, covs = covs)
}

# ---- the per-cell SHARE model --------------------------------------------------------
#' Share offsets from reporting cells
#'
#' `logit(s_obs) - logit(prior_s) ~ covs | county^mode` on reporting cells, weighted by
#' counted two-party ballots; capped by `SH_MAX_SHIFT`.
#'
#' @param cells Cells.
#' @param returns Precinct x mode returns.
#' @param covs Covariates (`HY_COVS_SHARE`).
#' @return Tibble: geo, mode, off (logit points).
estimate_share = function(cells, returns, covs = HY_COVS_SHARE) {
  zero = tibble::tibble(geo = character(), mode = character(), off = numeric())
  if (!HY_SHARE) {
    return(zero)
  }
  d = cells |>
    dplyr::left_join(returns, by = c("geo", "mode")) |>
    dplyr::mutate(
      tot2 = dplyr::coalesce(votes_dem, 0) + dplyr::coalesce(votes_rep, 0),
      tau = tot2 + dplyr::coalesce(votes_oth, 0),
      # the same `obs` test fit_stage uses -- a cell's SHARES are usable as soon as it
      # reports, because a partial count is a sample of its electorate, not a bound
      done = !is.na(votes_dem) & stage >= STAGE_DONE & tau > 0 & tot2 > 0,
      # continuity-corrected, matching fit_stage's y_s, so a 0-Dem cell is finite
      lr = logit((dplyr::coalesce(votes_dem, 0) + 0.5) / (tot2 + 1)) - logit(prior_s),
      w2 = pmax(tot2, 1)
    )
  fit = fit_cm(
    dplyr::filter(d, done, is.finite(lr)),
    covs,
    "w2",
    SH_MIN_CELLS,
    SH_MIN_GRP,
    SH_MIN_CM,
    "share"
  )
  if (is.null(fit)) {
    return(zero)
  }
  xb = setNames(as.vector(covs_dev(covs) %*% fit$b), prec$geo)
  off = cells |>
    dplyr::select(geo, county, mode) |>
    dplyr::left_join(dplyr::select(fit$g, county, mode, fe), by = c("county", "mode")) |>
    dplyr::transmute(
      geo,
      mode,
      off = pmin(pmax(unname(xb[geo]) + dplyr::coalesce(fe, 0), -SH_MAX_SHIFT), SH_MAX_SHIFT)
    )
  cli_inform(
    "hybrid/share: offset median {sprintf('%+.3f', stats::median(off$off))} logit, \\
              range {sprintf('%+.3f', min(off$off))} to {sprintf('%+.3f', max(off$off))}"
  )
  off
}

# ---- county-level returns as exact aggregate constraints ------------------------------
#' County x mode-group targets from a CBS snapshot
#'
#' Per county, the counted-out vote totals by mode group to reproduce (file header: the
#' level comes from county_total_vote, the composition from the method rows scaled by
#' coverage; counties below `CTY_MIN_COVER` coverage or not counted out, `CTY_MIN_PCT`, are
#' skipped; `adv` constrains early + mail jointly).
#'
#' @param cells Cells.
#' @param snap A cbs_county_pull.R snapshot (schema_version 2); NULL reads `CTY_SNAPSHOT`
#'   if set, else the arm is inert.
#' @return Tibble: county, grp, target; 0 rows when inert. Stops on a county that files
#'   `adv` or `all` beside per-method rows.
county_targets = function(cells, snap = NULL) {
  none = tibble::tibble(county = character(), grp = character(), target = numeric())
  if (!HY_COUNTY) {
    return(none)
  }
  if (is.null(snap)) {
    # No snapshot passed and no path set: the arm is inert (the sims). On the night
    # predict_one() passes the frame from live/R/counties.R's read_cbs_latest().
    if (!nzchar(CTY_SNAPSHOT)) {
      return(none)
    }
    if (!file.exists(CTY_SNAPSHOT)) {
      cli_abort("CTY_SNAPSHOT={.path {CTY_SNAPSHOT}} does not exist.")
    }
    snap = tibble::as_tibble(nanoparquet::read_parquet(CTY_SNAPSHOT))
  }
  snap = tibble::as_tibble(snap)
  need = c(
    "county_cbs",
    "method",
    "votes",
    "county_total_vote",
    "county_pct_exp_vote",
    "county_precincts_in",
    "county_precincts_of"
  )
  miss = setdiff(need, names(snap))
  if (length(miss)) {
    cli_abort(c(
      "CBS county snapshot is missing {miss}.",
      "i" = "cbs_county_pull.R writes schema_version 2; a v1 snapshot is \\
                       mode-blind and cannot be used here."
    ))
  }
  if (!is.null(snap$schema_version) && any(snap$schema_version < 2L, na.rm = TRUE)) {
    cli_abort(
      "CBS county snapshot is schema_version 1 (mode-blind); re-pull with by_method = TRUE."
    )
  }

  # KEY ON THE PRECINCT ID's CBS COUNTY NUMBER, not the county name. The precinct ids are
  # tx_{county_cbs}_*, and cbs_county_pull.R already asserts that its county_cbs agrees
  # with that prefix -- so this is the one join that is checked at both ends.
  key = cells |>
    dplyr::distinct(county, geo) |>
    dplyr::mutate(county_cbs = stringr::str_extract(geo, "(?<=_)[0-9]{3}(?=_)")) |>
    dplyr::distinct(county, county_cbs) |>
    dplyr::filter(!is.na(county_cbs))
  if (!nrow(key) || !any(key$county_cbs %in% snap$county_cbs)) {
    cli_alert_warning(
      "hybrid/county: no precinct id matches a CBS county_cbs \\
                       ({nrow(key)} county keys derived) -- county constraints skipped"
    )
    return(none)
  }

  # COUNTY COMPLETENESS GATE. A partial county total is a lower bound, not a target.
  # county_pct_exp_vote is 0-100 in schema_version 2 (CBS files an integer percent). If
  # that ever becomes a 0-1 fraction, `pct >= 99` stops firing and this arm goes SILENTLY
  # inert, which is the worst failure mode available -- so check the scale, don't assume.
  if (
    any(is.finite(snap$county_pct_exp_vote)) &&
      max(snap$county_pct_exp_vote, na.rm = TRUE) <= 1.5
  ) {
    cli_abort(c(
      "county_pct_exp_vote tops out at \
                 {max(snap$county_pct_exp_vote, na.rm = TRUE)}, which looks like a 0-1 \
                 fraction, not the 0-100 percent CTY_MIN_PCT={CTY_MIN_PCT} expects.",
      "x" = "Fix the scale here (and in cbs_county_pull.R) rather than letting \
                       the completeness gate never fire."
    ))
  }
  cty = snap |>
    dplyr::summarize(
      total = dplyr::first(county_total_vote),
      pct = dplyr::first(county_pct_exp_vote),
      pin = dplyr::first(county_precincts_in),
      pof = dplyr::first(county_precincts_of),
      method_votes = sum(votes, na.rm = TRUE),
      .by = county_cbs
    ) |>
    dplyr::mutate(
      cover = method_votes / pmax(total, 1),
      done = is.finite(pct) &
        pct >= CTY_MIN_PCT &
        is.finite(pin) &
        is.finite(pof) &
        pof > 0 &
        pin >= pof,
      # a BAND, not a floor: on the TX gov test feed one county's method rows
      # carry 1.37x its total, which is an over-count, not a shortfall, and
      # scaling it down would silently launder a feed inconsistency
      ok = done & is.finite(cover) & cover >= CTY_MIN_COVER & cover <= 1 / CTY_MIN_COVER
    )
  n_in = sum(cty$county_cbs %in% key$county_cbs)
  n_ok = sum(cty$ok & cty$county_cbs %in% key$county_cbs)
  n_cov = sum(cty$done & !cty$ok & cty$county_cbs %in% key$county_cbs)
  # The completeness gate is STRICT on purpose, and it makes this a LATE-night
  # correction: a partially-counted county total is a lower bound, and using it as a
  # target understates turnout by exactly the uncounted share. The version that fires
  # earlier needs a trustworthy per-mode counted fraction, and cbs_county_pull.R records
  # why we do not have one -- exp_method is floored to rep_method when CBS files no
  # expectation, so a fraction of 1.0 cannot be told apart from "counted out".
  cli_inform(
    "hybrid/county: {n_in} CBS counties in frame, {n_ok} counted out and \\
              >= {round(100*CTY_MIN_COVER)}% method coverage \\
              (median coverage {sprintf('%.3f', stats::median(cty$cover))}; \\
              {n_cov} counted out but outside the \
              [{CTY_MIN_COVER}, {round(1/CTY_MIN_COVER, 2)}] coverage band, skipped)"
  )
  if (!n_ok) {
    return(none)
  }

  # Per method-GROUP totals, SCALED to the county total: the level comes from
  # county_total_vote (authoritative) and only the split from the method rows.
  tg = snap |>
    dplyr::inner_join(
      dplyr::filter(cty, ok) |> dplyr::select(county_cbs, cover),
      by = "county_cbs"
    ) |>
    dplyr::summarize(
      v = sum(votes, na.rm = TRUE),
      cover = dplyr::first(cover),
      .by = c(county_cbs, method)
    ) |>
    dplyr::mutate(target = v / cover)
  grp = lapply(unique(tg$method), function(m) {
    tibble::tibble(method = m, grp = paste(sort(cbs_method_group(m)), collapse = "+"))
  })
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
  if (nrow(ov)) {
    cli_abort(c(
      "{nrow(ov)} county x mode(s) are covered by more than one CBS method group.",
      "i" = "`adv` is early+mail combined and `all` is every mode; summing \\
                       either beside a per-method row double-counts.",
      "x" = "Offending: {paste(ov$county, ov$m, collapse = ', ')}"
    ))
  }
  tg
}

#' Level and composition offsets that reproduce one county's targets
#'
#' Level is closed form (one unknown, one equation); composition is iterative proportional
#' fitting over the method groups.
#'
#' @param E The county's precincts' prior expected total votes.
#' @param P Their prior mode composition (precincts x modes, rows summing to 1).
#' @param tg Named targets by group ("early+mail" names a group of modes); targets of 0
#'   are raised to 0.5 to keep the solve finite.
#' @param maxit,tol IPF iterations and tolerance.
#' @return List: level, off (per-mode composition offsets), iters, gap.
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
    for (g in names(grp)) {
      W[, grp[[g]]] = W[, grp[[g]], drop = FALSE] * exp(o[[g]])
    }
    W = W / rowSums(W)
    ach = vapply(grp, function(ms) sum(colSums(Ea * W)[ms]), 0)
    step = log(tg / pmax(ach, 1e-9))
    o = o + step
    if (max(abs(step)) < tol) break
  }
  off = setNames(rep(0, ncol(P)), colnames(P))
  for (g in names(grp)) {
    off[grp[[g]]] = o[[g]]
  }
  list(level = lam, off = off, iters = it, gap = max(abs(step)))
}

#' Fold the county constraints into the regression offsets
#'
#' A constrained county's level and composition come entirely from its constraint (a
#' complete county total and a fitted effect are two estimates of the same thing). The
#' covariate term is kept, so the aggregate stays exact. This arm supplies a level even with
#' HY_LEVEL = 0: a complete county total is not a regression.
#'
#' @param cells Cells.
#' @param off `estimate_level()` offsets.
#' @param tg `county_targets()`.
#' @return `off` with lvl and comp updated.
apply_county = function(cells, off, tg) {
  if (!nrow(tg)) {
    return(off)
  }
  covs = off$covs
  Xc = covs_dev(covs)
  # the covariate-only part of the level, which a constrained county KEEPS (it
  # redistributes within the county; the constraint's level is solved around it)
  xb = pmin(pmax(as.vector(Xc %*% off$b), -ST_MAX_LEVEL), ST_MAX_LEVEL)
  lvl = off$lvl
  comp = off$comp
  wide = cells |>
    dplyr::select(geo, mode, mode_turn_share) |>
    tidyr::pivot_wider(names_from = mode, values_from = mode_turn_share) |>
    dplyr::arrange(match(geo, prec$geo))
  P_all = as.matrix(wide[, MODES, drop = FALSE])
  rownames(P_all) = wide$geo
  binds = 0L
  nclamp = 0L
  gaps = numeric()
  for (cty in unique(tg$county)) {
    i = which(prec$county == cty)
    if (!length(i)) {
      next
    }
    tv = tg$target[tg$county == cty]
    names(tv) = tg$grp[tg$county == cty]
    s = solve_county(prec$est_tot[i] * exp(xb[i]), P_all[prec$geo[i], , drop = FALSE], tv)
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
  if (nclamp) {
    cli_alert_warning(
      "hybrid/county: {nclamp} constraint-derived composition offset(s) \\
                       hit +/-{ST_MAX_COMP}, so those counties' targets are NO LONGER \\
                       reproduced exactly. A mode whose CBS total is far from its prior \\
                       share is the usual cause -- check the mode history for them."
    )
  }
  if (binds) {
    cli_alert_warning(
      "hybrid/county: the constraint-derived level hit \\
                       +/-{CTY_MAX_LEVEL} in {binds} county(ies). A constraint is data, \\
                       not a fit, so this means a unit or join error -- check that the \\
                       CBS county totals and est_tot are the same contest."
    )
  }
  cli_inform(
    "hybrid/county: {dplyr::n_distinct(tg$county)} county(ies) constrained to \\
              their CBS totals (IPF residual max {sprintf('%.2e', max(c(gaps, 0)))})"
  )
  off$lvl = lvl
  off$comp = comp
  off
}

# ---- re-centre the priors, leave everything else alone -------------------------------
#' Re-centre the cells' priors on the offsets
#'
#' Moves the turnout level (theta's prior), composition (eta's prior) and two-party share
#' (s's prior); everything else is left alone. With every arm off it returns `cells`
#' itself.
#'
#' @param cells Cells.
#' @param off Offsets: lvl, comp, share (missing parts are treated as zero).
#' @return `cells` with re-centred priors.
recentre_prior = function(cells, off) {
  # acceptance test (a): with every arm off this is the identity, so fit_stage_hybrid()
  # IS fit_stage() -- not "equal to within tolerance", the same object.
  if (!HY_LEVEL && !HY_COMP && !HY_SHARE && !HY_COUNTY) {
    return(cells)
  }
  # tolerate a partially-built offset list, so a diagnostic can exercise one arm alone
  if (is.null(off$lvl)) {
    off$lvl = setNames(rep(0, nrow(prec)), prec$geo)
  }
  if (is.null(off$comp)) {
    off$comp = tibble::tibble(county = character(), mode = character(), off = numeric())
  }
  if (is.null(off$share)) {
    off$share = tibble::tibble(geo = character(), mode = character(), off = numeric())
  }
  m = unname(off$lvl[match(cells$geo, names(off$lvl))])
  m[is.na(m)] = 0
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
      y_tau_prior = if (!USE_COMPOSE) {
        log(prior_tau + 0.1)
      } else {
        dplyr::if_else(
          mode == REF_MODE,
          logit(pmin(pmax(est_tot_adj / cap, 1e-4), 1 - 1e-4)),
          log(pmax(mode_turn_share, 1e-6) / pmax(ref_share, 1e-6))
        )
      }
    )
  if (nrow(off$share)) {
    out = out |>
      dplyr::left_join(dplyr::rename(off$share, .soff = off), by = c("geo", "mode")) |>
      dplyr::mutate(
        prior_s = invlogit(logit(prior_s) + dplyr::coalesce(.soff, 0)),
        y_s_prior = logit(prior_s)
      ) |>
      dplyr::select(-.soff)
  }
  out
}

# ---- the one overridden step ---------------------------------------------------------
#' fit_stage() on re-centred priors
#'
#' @param cells Cells.
#' @param returns Precinct x mode returns.
#' @param ... Passed to `fit_stage()` (draws, cross_mode, Sigma_hist, level, county_obs).
#' @param cbs_county CBS county x method snapshot for this race (`read_cbs_latest()`); must
#'   be named. NULL and no `CTY_SNAPSHOT` leaves the county-constraint arm inert.
#' @return `fit_stage()` result.
fit_stage_hybrid = function(cells, returns, ..., cbs_county = NULL) {
  if ((HY_LEVEL || HY_COUNTY) && isTRUE(get0("LEVEL_UPDATE", ifnotfound = FALSE))) {
    cli_abort(c(
      "LEVEL_UPDATE=1 and model_hybrid apply the level correction TWICE.",
      "i" = "sum(prior_tau) already carries exp(level_i) from recentre_prior(); \
                       level_tilt then multiplies it again.",
      "x" = "Set LEVEL_UPDATE=0 (or HY_LEVEL=0 HY_COUNTY=0) when using model_hybrid."
    ))
  }
  if ((HY_COMP || HY_COUNTY) && isTRUE(get0("COMPOSITION_UPDATE", ifnotfound = FALSE))) {
    cli_abort(c(
      "COMPOSITION_UPDATE=1 and model_hybrid apply the composition tilt TWICE.",
      "i" = "mode_turn_share already carries exp(comp_cm) from recentre_prior(); \
                       comp_tilt then re-derives the same shift from the same returns.",
      "x" = "Set COMPOSITION_UPDATE=0 (or HY_COMP=0 HY_COUNTY=0) when using model_hybrid."
    ))
  }
  off = estimate_level(cells, returns)
  if (HY_COUNTY) {
    off = apply_county(cells, off, county_targets(cells, cbs_county))
  }
  off$share = estimate_share(cells, returns)
  fit_stage(recentre_prior(cells, off), returns, ...)
}
