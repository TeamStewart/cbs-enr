# ========================================
## model_core.R -- the fitting primitives, shared by the live model and the backtests.
## Extracted from tx_gov_trial.R so the two cannot drift apart. Pure functions: no data
## loading, no globals, no side effects.
# ========================================

logit    = function(p) qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
invlogit = function(x) plogis(x)
sig_of   = function(f) if (is.null(f$post)) f$sigma_scale else f$post$sigma_scale

# `cens` / `y_lb`: RIGHT-CENSORED observations. A cell that is reporting but not known to
# be counted out tells us its final value lies ABOVE what has been counted -- y_i >= y_lb_i
# -- which is a true, exact constraint even when only 5% of the cell is in. Without this
# the only options are to treat the partial count as final (catastrophic early) or to drop
# the cell from the fit (throws the constraint away), and the model has to decide which via
# a completeness flag that in production arrives days late, varies by state, is downgraded
# by our own loader, and moves BACKWARDS (pct_rep 0.997 -> 0.269 -> 0.364). Censoring makes
# that flag non-load-bearing: a wrongly-unflagged complete cell still contributes its bound.
#
# Fitted by EM, which keeps every M-step in closed form (one weighted QR, as before):
#   E-step  for censored i, under N(mu_i, sigma^2) truncated below at a_i:
#             alpha = (a_i - mu_i)/sigma,  lambda = phi(alpha)/(1 - Phi(alpha))
#             m_i = mu_i + sigma*lambda                     E[y_i | y_i >= a_i]
#             v_i = sigma^2 * (1 + alpha*lambda - lambda^2) Var[y_i | y_i >= a_i]
#   M-step  refit with censored y replaced by m_i; sigma^2 adds the v_i back, because
#           using only the imputed MEAN would understate the residual variance.
#
# `p_atom`: ATOM AT THE BOUND. Pure Tobit has E[y | y >= a] > a STRICTLY, so every
# still-censored unit is pushed above its bound -- which is why censoring measured as
# fixing early turnout (184% -> 99% at 20:00) and breaking late (100% -> 124%). The
# misspecification is that a FINISHED unit sits exactly AT its bound, and the continuous
# model gives that outcome probability zero, when y = a exactly is the common case late
# on. So mix a point mass in: with probability p_atom_i the unit is done and y_i = a_i
# exactly, with probability 1 - p_atom_i it is still counting and y_i > a_i.
#   m_i = p*a_i + (1-p)*(mu_i + sigma*lambda)
#   v_i = p*a_i^2 + (1-p)*(v_trunc + m_trunc^2) - m_i^2
# The variance picks up the BETWEEN-component spread, so a unit we are unsure about
# widens sigma rather than silently sharpening it. p_atom = 0 is exactly pure Tobit, so
# the default is unchanged.
fit_conj_re = function(y, Xcov, Cty, obs, prior_loc, prior_eff, lambda, wts = NULL,
                       sigma_df0 = 4, sigma_s0 = 0.05, sd_level = NULL,
                       cens = NULL, y_lb = NULL, p_atom = NULL, n_em = 4L) {
  N = nrow(Xcov); p = ncol(Xcov); J = ncol(Cty)
  if (is.null(wts)) wts = rep(1, N)
  if (is.null(cens)) cens = rep(FALSE, N)
  cens = cens & !obs                                    # exact beats censored
  use  = obs | cens                                     # every row contributing data
  n_obs = sum(use)
  n_cen = sum(cens)
  cen_in_use = cens[use]                                # position of censored rows in A_dat
  a_lb = if (n_cen > 0) y_lb[cens] else numeric(0)
  # probability each censored unit is already FINISHED (sits exactly at its bound)
  pa = if (n_cen == 0) numeric(0)
       else if (is.null(p_atom)) rep(0, n_cen)
       else pmin(pmax(p_atom[cens], 0), 1)
  Zp = matrix(0, N, J)                                  # g-prior rows ignore county block
  A_dat = cbind(Xcov[use, , drop = FALSE], Cty[use, , drop = FALSE])
  A_rid = cbind(matrix(0, J, p), diag(J))               # shrink u_c toward 0
  y_use = y[use]
  if (n_cen > 0) y_use[cen_in_use] = a_lb               # EM start: impute at the bound

  # ---- LEVEL / SHAPE SPLIT (sd_level non-NULL) ------------------------------------
  # The g-prior pulls FITTED VALUES toward prior_loc with total weight prior_n, which
  # bundles two different things: the statewide LEVEL and the precinct-to-precinct
  # SHAPE. That bundling has two bad consequences -- the implied level prior scales with
  # sigma (precinct-level noise, which a statewide swing has nothing to do with), and it
  # cannot be set without also changing how fast the covariate slopes learn.
  # Measured implied level prior sd = sigma/sqrt(prior_n) ~= 0.17/sqrt(3) ~= 0.098.
  # Measured ACTUAL statewide swing sd (9 states, 37 contest pairs, calibrate_swing.R)
  # = 0.22-0.26 logit. So the bundled prior is ~2.3x too tight.
  # Split: ONE row at the mean covariate vector carries the level at its own precision,
  # and the N g-prior rows become CENTERED contrasts which are orthogonal to it, so
  # prior_n governs shape alone.
  # xbar rather than the intercept column, because log_n / dem2 / tp_prior_raw are not
  # standardized, so the intercept is not the mean level.
  use_lvl = !is.null(sd_level) && is.finite(sd_level) && sd_level > 0
  if (use_lvl) {
    xbar  = colMeans(Xcov)
    plbar = mean(prior_loc)
    Xc    = sweep(Xcov, 2, xbar)                        # intercept column becomes 0
    A_lvl = cbind(matrix(xbar, 1, p), matrix(0, 1, J))
    build = function(kappa) {
      list(X = rbind(cbind(Xc, Zp), A_lvl, A_dat, A_rid),
           y = c(prior_loc - plbar, plbar, y_use, rep(0, J)),
           w = c(rep(prior_eff, N), kappa, wts[use], rep(lambda, J)),
           i_dat = if (n_obs > 0) (N + 2L):(N + 1L + n_obs) else integer(0))
      }
  } else {
    build = function(kappa) {
      list(X = rbind(cbind(Xcov, Zp), A_dat, A_rid),
           y = c(prior_loc, y_use, rep(0, J)),
           w = c(rep(prior_eff, N), wts[use], rep(lambda, J)),
           i_dat = if (n_obs > 0) (N + 1L):(N + n_obs) else integer(0))
    }
  }
  # kappa is a precision RELATIVE to sigma^2, so turning sd_level into one needs sigma.
  # Two-pass plug-in: fit once at the old total prior weight to get post_s, then refit
  # with kappa = post_s^2 / sd_level^2. Each pass is exactly conjugate; the plug-in is
  # what makes the level prior's scale independent of an arbitrary reference sigma.
  kappa = prior_eff * N
  v_cen = numeric(n_cen)                 # E-step conditional variances, 0 until computed
  for (em in seq_len(if (n_cen > 0) n_em else 1L)) {
    for (pass in seq_len(if (use_lvl) 2L else 1L)) {
      B = build(kappa)
      X_fit = B$X; y_fit = B$y; w_fit = B$w
      sw = sqrt(w_fit); qrX = qr(X_fit * sw)
      if (qrX$rank < ncol(X_fit)) return(NULL)
      if (pass == 1L && use_lvl) {
        r1 = qr.resid(qrX, y_fit * sw)
        s2 = if (n_obs > 1) sum(r1[B$i_dat]^2) / (n_obs - 1) else sigma_s0^2
        kappa = max(s2, sigma_s0^2) / sd_level^2
      }
    }
    if (n_cen == 0 || em == n_em) break
    # ---- E-step ----
    cf  = qr.solve(qrX, y_fit * sw)
    mu  = c(A_dat %*% cf)
    rr  = qr.resid(qrX, y_fit * sw)
    sg  = sqrt(max(sum(rr[B$i_dat]^2) / max(n_obs - p, 1), sigma_s0^2))
    alpha = (a_lb - mu[cen_in_use]) / sg
    lam = exp(dnorm(alpha, log = TRUE) - pnorm(alpha, lower.tail = FALSE, log.p = TRUE))
    lam[!is.finite(lam)] = pmax(alpha[!is.finite(lam)], 0)   # far tail: E[y|y>=a] -> a
    m_trunc = mu[cen_in_use] + sg * lam              # E[y | y > a], the "still counting" arm
    v_trunc = sg^2 * pmax(1 + alpha * lam - lam^2, 0)
    y_use[cen_in_use] = pa * a_lb + (1 - pa) * m_trunc
    # second moment of the mixture, then the usual Var = E[y^2] - E[y]^2. With pa = 0
    # this collapses to v_trunc exactly.
    e_y2  = pa * a_lb^2 + (1 - pa) * (v_trunc + m_trunc^2)
    v_cen = pmax(e_y2 - y_use[cen_in_use]^2, 0)
  }
  coefs = qr.solve(qrX, y_fit * sw)
  res   = qr.resid(qrX, y_fit * sw)
  # The augmented system holds three kinds of row -- g-prior pseudo-rows, DATA rows, and
  # county ridge rows -- and only the data rows carry residual variance. Taking the SSR
  # over all of them while dividing by a data-only df mixes the two: when n_obs is small
  # relative to N and J the prior and ridge blocks dominate the numerator and sigma
  # explodes (observed: the eday equation on one county's 15 cells gave sigma 2.19 on the
  # log-turnout scale, so exp(mu + eps) produced turnout in the 10^12). Residual variance
  # is therefore computed on the data rows alone.
  i_dat = if (n_obs > 0) B$i_dat else integer(0)
  # Effective df consumed, exactly: the trace of the hat matrix over the data rows. The
  # county block is penalised, so it consumes well under J -- this measures how much
  # rather than assuming it away, and it is correct when only a few county levels are
  # identified by data at all.
  Q = qr.Q(qrX)
  edf = if (n_obs > 0) sum(Q[i_dat, , drop = FALSE]^2) else 0
  df = max(n_obs - edf, 1)
  # Imputed values are conditional MEANS, so their squared residual understates the true
  # dispersion; add the conditional variances back or sigma is biased down by censoring.
  ss_cen = if (n_cen > 0) sum(wts[cens] * v_cen) else 0
  sig2 = if (n_obs > 0) (sum(res[i_dat]^2) + ss_cen) / df else sigma_s0^2
  # Conjugate scaled-inverse-chi2 update on the DATA df. Using nrow(X_fit) here instead
  # inflates post_df by the pseudo-rows, which makes sigma effectively KNOWN and strips
  # the sigma uncertainty out of every predictive interval.
  post_df = sigma_df0 + df
  post_s  = sqrt((sigma_df0 * sigma_s0^2 + df * sig2) / post_df)
  fitted = c(A_dat %*% coefs)
  list(coefs = coefs, R = qr.R(qrX), sigma_scale = post_s, sigma_df = post_df,
       residuals = y_use - fitted, p_cov = p, J = J,
       n_cens = n_cen, use = use)
}

draw_beta_re = function(f, draws) {
  p = length(f$coefs)
  sigma = f$sigma_scale * sqrt(f$sigma_df / rchisq(draws, f$sigma_df))
  z = sweep(matrix(rnorm(draws * p), p, draws), 2, sigma, "*")
  t(f$coefs + backsolve(f$R, z))
}

psd_repair = function(S, floor = 1e-10) {
  e = eigen(S, symmetric = TRUE)
  e$vectors %*% diag(pmax(e$values, floor), nrow(S)) %*% t(e$vectors)
}

cov_pairwise = function(R, min_overlap = 30) {
  k = ncol(R); S = matrix(0, k, k, dimnames = dimnames(R)[c(2, 2)])
  for (a in seq_len(k)) for (b in a:k) {
    ok = !is.na(R[, a]) & !is.na(R[, b])
    if (sum(ok) >= min_overlap) {
      S[a, b] = S[b, a] = stats::cov(R[ok, a], R[ok, b])
    } else if (a == b) {
      S[a, a] = stats::var(R[!is.na(R[, a]), a])
    }
  }
  diag(S)[!is.finite(diag(S)) | diag(S) <= 0] = 1e-4
  psd_repair(S)
}

draw_beta = function(m, draws) {
  p     = length(m$post$beta_loc)
  sigma = m$post$sigma_scale * sqrt(m$post$sigma_df / rchisq(draws, m$post$sigma_df))
  z     = sweep(matrix(rnorm(draws * p), p, draws), 2, sigma, "*")
  t(m$post$beta_loc + backsolve(m$post$beta_invchol, z))        # draws x p
}
