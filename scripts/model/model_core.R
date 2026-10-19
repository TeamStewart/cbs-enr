# ========================================
## model_core.R -- the fitting primitives, shared by the live model and the backtests.
## Extracted from tx_gov_trial.R so the two cannot drift apart. Pure functions: no data
## loading, no globals, no side effects.
# ========================================

logit    = function(p) qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
invlogit = function(x) plogis(x)
sig_of   = function(f) if (is.null(f$post)) f$sigma_scale else f$post$sigma_scale

fit_conj_re = function(y, Xcov, Cty, obs, prior_loc, prior_eff, lambda, wts = NULL,
                       sigma_df0 = 4, sigma_s0 = 0.05) {
  N = nrow(Xcov); p = ncol(Xcov); J = ncol(Cty)
  n_obs = sum(obs)
  if (is.null(wts)) wts = rep(1, N)
  Zp = matrix(0, N, J)                                  # g-prior rows ignore county block
  A_dat = cbind(Xcov[obs, , drop = FALSE], Cty[obs, , drop = FALSE])
  A_rid = cbind(matrix(0, J, p), diag(J))               # shrink u_c toward 0
  X_fit = rbind(cbind(Xcov, Zp), A_dat, A_rid)
  y_fit = c(prior_loc, y[obs], rep(0, J))
  w_fit = c(rep(prior_eff, N), wts[obs], rep(lambda, J))
  sw = sqrt(w_fit); qrX = qr(X_fit * sw)
  if (qrX$rank < ncol(X_fit)) return(NULL)
  coefs = qr.solve(qrX, y_fit * sw)
  res   = qr.resid(qrX, y_fit * sw)
  # count only the covariate columns as consumed df: the county block is penalised, so
  # its effective df is well below J
  df = max(n_obs - p, 1)
  sig2 = sum(res^2) / df
  nr = nrow(X_fit)
  post_df = sigma_df0 + nr
  post_s  = sqrt((sigma_df0 * sigma_s0^2 + nr * sig2) / post_df)
  fitted = c(cbind(Xcov[obs, , drop=FALSE], Cty[obs, , drop=FALSE]) %*% coefs)
  list(coefs = coefs, R = qr.R(qrX), sigma_scale = post_s, sigma_df = post_df,
       residuals = y[obs] - fitted, p_cov = p, J = J)
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
