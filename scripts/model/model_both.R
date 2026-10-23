# ========================================
## PROPOSED: swing-style theta that keeps the covariates
##
## STATUS: untested scaffold. Nothing here has been run against a scenario. It exists so
## the design is executable rather than prose; see the acceptance tests at the bottom
## before trusting any number it produces.
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
## So each half fixes what the other cannot. This file combines them.
##
## THE SPECIFICATION. On counted-out cells,
##
##     log(c_im / prior_tau_im) ~ covariates | county + mode
##
## The decomposition log(c_im/tau0_im) = log(T_i/E_i) + log(p_im/p0_im) says what each
## term absorbs:
##   mode FE     the average composition shift per mode (eta's job, excluded from the level)
##   county FE   county turnout level, UNSHRUNK, so no county borrows another's
##   covariates  the within-county precinct structure a county effect cannot see
##
## Per-cell, so never starved. County effects FIXED rather than shrunk is precisely why
## the swing model avoids the selection bias that killed the gross-up (measured there:
## cor(precinct turnout delta, dem2) = -0.36 BETWEEN counties, +0.06 within).
##
## HOW THE BOUNDS SURVIVE. This does not become theta. It sets theta's PRIOR:
##     est_tot_i   <- est_tot_i * exp(level_i)
##     y_tau_prior <- logit(est_tot_i / cap_i)
## Everything downstream is model_turnout.R's, untouched: T = kappa*n*invlogit(theta),
## softmax(eta), the partial-count mixture, adding-up, the counted-votes floor and pin.
## When theta's own equation is empty -- which is all night -- the solve now returns a
## prior that has LEARNED FROM THE RETURNS rather than the vendor score. The share
## equations are not touched at all.
##
## DO NOT COMBINE WITH LEVEL_UPDATE=1. Both apply a statewide turnout level correction,
## arrived at from different directions, and together they apply it TWICE:
##   LEVEL_UPDATE   r_prior = cnt_prec / (sum(prior_tau) * exp(level_tilt))
##   this file      prior_tau = est_tot * exp(level_i) * mode_turn_share
## so sum(prior_tau) already carries exp(level_i) and level_tilt multiplies it again.
## LEVEL_UPDATE is the narrower fix -- it only corrects r_prior, which is the atom's
## evidence for P(precinct has finished) -- and this file's correction is the general
## one. If this design works, LEVEL_UPDATE should be REMOVED rather than left as a trap;
## a silent double-correction is exactly the kind of thing that surfaces months later as
## an unexplained bias. fit_stage_swingtheta() warns if it sees both.
##
## NOT affected: q_share and therefore PIN_DONE's gate. mode_turn_share is a normalised
## share, so the level cancels out of it; the pin's threshold is independent of this
## correction and the two compose cleanly.
##
## NOT A FORK. This sources model_turnout.R with SOURCE_ONLY=1 and overrides one step.
## scripts/model/live/R/model_share.R is already a COPY of model_turnout.R and its own header
## warns about drift; a second copy would be worse. A change to the model is picked up
## here with no edits.
##
## Run:  Rscript scripts/model/tx_gov_swingtheta.R        (prints a self-check)
## Use:  source this file, then call fit_stage_swingtheta(cells, returns) wherever
##       fit_stage(cells, returns) is called today.
# ========================================

# Source the model only if it is not already loaded. test_sim_night.R loads
# model_turnout.R by its own parse-and-eval (replacing the two file reads), so
# re-sourcing here would clobber the harness's `cells` and `prec`.
if (!exists("fit_stage", mode = "function")) {
  Sys.setenv(SOURCE_ONLY = "1")
  source("scripts/model/model_turnout.R")
}
stopifnot("needs fixest" = requireNamespace("fixest", quietly = TRUE))

# How many counted-out cells before the level model is trusted at all. Below this,
# level_i is 0 everywhere and the result is exactly model_turnout.R.
ST_MIN_CELLS  = as.integer(Sys.getenv("ST_MIN_CELLS", unset = "200"))
# Clamp on the fitted level, in log points. exp(+/-0.5) is roughly a 1.65x / 0.61x swing
# in statewide turnout -- far outside anything real, so this is a guard against a
# degenerate fit on thin early data, not a modelling choice.
ST_MAX_LEVEL  = as.numeric(Sys.getenv("ST_MAX_LEVEL", unset = "0.5"))
# Include the composition-weighted average mode effect in the level? The mode FEs are
# composition deviations and their composition-weighted mean is ~0 by construction, so
# this should barely matter -- but it is the one place the level/composition split is
# approximate rather than exact, so it is switchable for the ablation.
ST_WITH_MODE  = Sys.getenv("ST_WITH_MODE", unset = "0") == "1"

# ---- the per-cell level model -------------------------------------------------------
# Returns one log-scale level correction per precinct, in `prec$geo` order.
estimate_level = function(cells, returns, covs = COVS_TURN) {
  d = cells |>
    dplyr::left_join(returns, by = c("geo", "mode")) |>
    dplyr::mutate(
      tau  = dplyr::coalesce(votes_dem, 0) + dplyr::coalesce(votes_rep, 0) +
             dplyr::coalesce(votes_oth, 0),
      done = !is.na(votes_dem) & dplyr::coalesce(complete, FALSE) &
             tau > 0 & prior_tau > 0,
      lr   = log(tau / prior_tau)
    )
  fr = dplyr::filter(d, done)
  if (nrow(fr) < ST_MIN_CELLS) {
    cli_alert_info("swing-theta: only {nrow(fr)} counted-out cells \\
                    (< {ST_MIN_CELLS}) -- level correction held at 0")
    return(setNames(rep(0, nrow(prec)), prec$geo))
  }
  form = stats::as.formula(paste("lr ~", paste(sprintf("`%s`", covs), collapse = " + "),
                                 "| county + mode"))
  fit = tryCatch(fixest::feols(form, data = fr, weights = fr$tau, notes = FALSE, warn = FALSE),
                 error = function(e) NULL)
  if (is.null(fit)) {
    cli_alert_warning("swing-theta: level fit failed -- correction held at 0")
    return(setNames(rep(0, nrow(prec)), prec$geo))
  }
  # Covariate part, for EVERY precinct including ones that have reported nothing. This
  # is the half the swing model cannot do, and it extrapolates on OBSERVABLES rather
  # than on an unobserved county level inferred from whoever reported first.
  b  = stats::coef(fit)
  Xp = as.matrix(prec[, covs, drop = FALSE])
  lvl = as.vector(Xp %*% b[covs])
  # County fixed effects, unshrunk. A county with no counted-out cells gets 0 -- it keeps
  # its prior level rather than borrowing one, which is the point.
  fe = tryCatch(fixest::fixef(fit, notes = FALSE), error = function(e) NULL)
  if (!is.null(fe$county)) {
    cf = fe$county[as.character(prec$county)]
    lvl = lvl + dplyr::coalesce(unname(cf), 0)
  }
  if (ST_WITH_MODE && !is.null(fe$mode)) {
    # composition-weighted average mode effect; ~0 by construction, see ST_WITH_MODE
    mw = cells |> dplyr::summarize(m = sum(mode_turn_share * dplyr::coalesce(
           unname(fe$mode[as.character(mode)]), 0)), .by = geo) |>
      dplyr::arrange(match(geo, prec$geo))
    lvl = lvl + mw$m
  }
  lvl = pmin(pmax(lvl, -ST_MAX_LEVEL), ST_MAX_LEVEL)
  cli_inform("swing-theta: level from {nrow(fr)} counted-out cells; \\
              median {sprintf('%+.3f', median(lvl))} \\
              ({sprintf('%+.1f%%', 100 * (exp(median(lvl)) - 1))} on est_tot), \\
              range {sprintf('%+.3f', min(lvl))} to {sprintf('%+.3f', max(lvl))}")
  setNames(lvl, prec$geo)
}

# ---- re-centre theta's prior, leave everything else alone ---------------------------
recentre_prior = function(cells, lvl) {
  m = unname(lvl[match(cells$geo, names(lvl))])
  m[is.na(m)] = 0
  cells |>
    dplyr::mutate(
      est_tot_adj = est_tot * exp(m),
      prior_tau   = est_tot_adj * mode_turn_share,
      y_tau_prior = if (!USE_COMPOSE) log(prior_tau + 0.1)
                    else dplyr::if_else(
                      mode == REF_MODE,
                      logit(pmin(pmax(est_tot_adj / cap, 1e-4), 1 - 1e-4)),
                      y_tau_prior)     # eta's prior is a composition ratio: unchanged
    )
}

fit_stage_swingtheta = function(cells, returns, ...) {
  if (isTRUE(get0("LEVEL_UPDATE", ifnotfound = FALSE))) {
    cli_abort(c("LEVEL_UPDATE=1 and model_both apply the level correction TWICE.",
                "i" = "sum(prior_tau) already carries exp(level_i) from recentre_prior(); \
                       level_tilt then multiplies it again.",
                "x" = "Set LEVEL_UPDATE=0 when using model_both."))
  }
  lvl = estimate_level(cells, returns)
  fit_stage(recentre_prior(cells, lvl), returns, ...)
}

# ---- self-check ---------------------------------------------------------------------
# Not a test of whether the design WORKS -- only that it runs and is inert with no data.
if (sys.nframe() == 0L) {
  empty = tibble::tibble(geo = character(), mode = character(), stage = integer(),
                         complete = logical(), votes_dem = numeric(),
                         votes_rep = numeric(), votes_oth = numeric())
  lvl0 = estimate_level(cells, empty)
  stopifnot("level must be 0 with no returns" = all(lvl0 == 0))
  c0 = recentre_prior(cells, lvl0)
  stopifnot("prior must be unchanged when level is 0" =
              isTRUE(all.equal(c0$y_tau_prior, cells$y_tau_prior)))
  cli_alert_success("swing-theta self-check passed: inert with no returns")
}

# ========================================
## ACCEPTANCE TESTS -- run in this order, each can kill the design cheaply
##
## 1. DEFAULT PRESERVATION on tx_base. Two of five model edits on 2026-10-22 had bugs
##    that only surfaced on execution; "parses clean" is worth nothing here.
## 2. tx_turnout_low -- does it recover the level the gross-up did (119% -> 101%)?
## 3. tx_turnout_demo precinct dump -- does it keep the covariate advantage on
##    UNREPORTED precincts? Target: beat swing's 0.102 median |log err| / cor 0.45.
## 4. tx_modeshift on BOTH seeds -- the share increment over default. The gross-up
##    scored -0.320 and -0.510 with coverage 16/37 and 4/37. Anything near that fails.
##    Use two seeds: single-seed share comparisons at 0.1-0.5pp are not resolvable in
##    this sim, and four "refutations" on 2026-10-22 shared a seed and were weaker than
##    they looked.
## 5. the REAL TX 2024 backtest, last. The sim has an exact completeness flag and the
##    real harness has a plateau proxy; the gross-up looked fine in the sim and
##    fabricated votes on real returns (103.7% -> 109.0% at full count).
##
## Background: quality_reports/turnout_findings_2026-10-22.md
# ========================================
