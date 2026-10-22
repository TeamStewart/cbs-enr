# ========================================
## TX 2022 midterm turnout: where does precinct variation come from?
##
## The election-night model's turnout estimates run high while its two-party share is in
## good shape, so this sizes the turnout variance components on real TX midterm data
## instead of the three hardcoded ICC constants in the repo (ICC_TURN = 0.200 in
## model_turnout.R:326, 0.269 share in the comment table at model_turnout.R:286-311, 0.302
## in tests/model_test_common.R:44). None of them is reproducible from a script today.
##
## WHAT THE CONSTANT ACTUALLY GOVERNS. model_core.R appends J ridge rows of weight
## lambda targeting zero, which is exactly u_c ~ N(0, sigma^2/lambda) with
## lambda = (1-ICC)/ICC. So ICC = sigma_u^2 / (sigma_u^2 + sigma^2) where sigma^2 is the
## residual variance OF THE MODEL'S OWN FIT -- i.e. a RESIDUAL county ICC, after
## covariates, not a raw one. Under the default USE_COMPOSE=1 that fit's reference-mode
## outcome is y_tau = logit(T_final / cap) (model_turnout.R:910), i.e. THETA. Measuring on
## log(votes) instead understates it by about 1.85x, because the logit Jacobian amplifies
## county-level turnout differences more than it amplifies precinct noise.
##
## TWO ICC ESTIMATORS, reported side by side because they disagree by ~2x and the
## difference is itself diagnostic:
##   icc_mom = (s_t^2 - s_w^2)/s_t^2        subtracts within variance; noise-corrected
##   icc_raw = cty_sd^2/(cty_sd^2 + s_w^2)  the 0.200 constant's own estimator
## The gap is mean_j(sigma_e^2/n_j): small counties leak precinct noise into their means,
## and that leakage scales with the outcome's residual variance. So an outcome with more
## precinct noise gets a spuriously LARGER cty_sd. Watch for that before reading a rise
## in cty_sd as a rise in county signal.
##
## TWO SAMPLES, deliberately:
##   TX_results.parquet  9,257 precincts / 254 counties / ~100% of the statewide vote.
##                       votehub only, NO L2 join. Used for the county component free of
##                       both join selection and the registrant denominator. But it has
##                       no covariates, so it can only give a RAW ICC -- not the residual
##                       quantity the constant governs. Labelled as such; do not retune
##                       off it directly.
##   TX_panel.parquet    5,633 precincts / 32 counties / 65.9% of the vote. Carries L2
##                       registrants and demographics, so it is the only sample that can
##                       give the residual ICC. Urban-skewed; J=32 means the sampling SE
##                       is large, which is why the jackknife SE is reported.
##
## DENOMINATOR. L2 `n` is a stock dated 2026-10-11, four years after the outcome. It adds
## a large PRECINCT-level error, which sits in the ICC denominator and therefore DEFLATES
## the measured ICC. (An earlier version of this header had that backwards.) The
## no-L2-denominator variant in A2b bounds it.
##
## Run:  Rscript scripts/model/calibrate_turnout.R
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr); library(cli)
  library(nanoparquet)
})

PANELS   <- "scripts/model/panels"
OUT_CSV  <- "quality_reports/turnout_variance_tx2022.csv"
OUT_RDS  <- "quality_reports/turnout_variance_tx2022_objects.rds"
OUT_FIG  <- "quality_reports/figures"
KAPPA    <- 1.25        # model_turnout.R:154
ICC_USED <- 0.200       # model_turnout.R:326, the constant under test
MIN_VOTES <- 20         # precinct outcome floor
MIN_REG   <- 50         # L2 denominator floor -- a DATA-QUALITY cut, reported separately
NEAR_CAP  <- 0.8        # "near the ceiling" threshold, as a fraction of KAPPA*n
GH_NODES  <- 401      # quadrature nodes for the Jensen integral (deterministic, seed-free)
set.seed(2026)
dir.create(OUT_FIG, showWarnings = FALSE, recursive = TRUE)

lg <- function(p) qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4))
rows <- list()
# `statistic` is mandatory: the table mixes sds, variance ratios and precision weights,
# and without it `value` is uninterpretable six months from now.
add <- function(part, quantity, scale, statistic, value, n_prec, n_cty)
  rows[[length(rows) + 1]] <<- tibble(part, quantity, scale, statistic, value, n_prec, n_cty)

# Method-of-moments county variance components, both estimators (see header).
vc <- function(r, county) {
  s_w <- sigma(lm(r ~ factor(county))); s_t <- stats::sd(r)
  cty_sd <- stats::sd(tapply(r, county, mean))
  list(cty_sd = cty_sd, within = s_w, total = s_t,
       icc_mom = max(0, 1 - s_w^2 / s_t^2),
       icc_raw = cty_sd^2 / (cty_sd^2 + s_w^2))
}
# Record every component, not just the ratio. Losing the numerator and denominator is
# what made the first version of this script's headline unfalsifiable from its own CSV.
add_vc <- function(v, part, quantity, scale, np, nc) {
  for (k in c("cty_sd", "within", "total"))
    add(part, paste0(quantity, "_", k), scale, "sd", v[[k]], np, nc)
  add(part, paste0(quantity, "_icc_mom"), scale, "icc", v$icc_mom, np, nc)
  add(part, paste0(quantity, "_icc_raw"), scale, "icc", v$icc_raw, np, nc)
}
lam <- function(i) (1 - i) / i

# ========================================
## A0. Data-quality gate
# ========================================
# CONTRACT.md 14.2 asks for the per-county exceedance rate as a standing signal. Two
# distinct failure modes, needing different treatment:
#   LEVEL    a county whose `n` is uniformly deflated (LAMAR: median ratio 0.874 vs 0.440
#            statewide, but within-county cor(gov22, n) = 0.815 and cor(vote22, n) =
#            0.936, both normal, and a sane county aggregate of 0.653). A county effect
#            absorbs this, so it corrupts the BETWEEN-county component only. Reported
#            both ways rather than dropped.
#   PRECINCT individually implausible rows. EXCLUDED, not clamped.
# The join key is NOT a contributor: pad-4 (backtest_panel.R:67) vs strip-leading-zeros
# (prep_tx_model_inputs.R:94) differ on 4 L2 precincts in RED RIVER, not a panel county.
cli_h1("A0. Data-quality gate")

panel <- read_parquet(file.path(PANELS, "TX_panel.parquet")) |>
  as_tibble() |>
  mutate(ratio = gov22_total / pmax(n, 1))

# The join fans out if L2 keys are not unique; votehub results would be replicated across
# rows with different n, inflating within-precinct variance for the ballots outcome only.
stopifnot("TX_panel geo key is not unique -- the L2 join fanned out" =
            !anyDuplicated(panel$geo))
cli_inform("geo keys unique: {nrow(panel)} rows / {n_distinct(panel$geo)} geos")

panel <- panel |> mutate(
  bad_high  = ratio > KAPPA,
  bad_votes = gov22_total <= MIN_VOTES,          # outcome property: fine to drop
  bad_reg   = n <= MIN_REG,                      # DENOMINATOR failure: a data-quality signal
  bad_l2    = vote22 <= 0,                       # log(vote22) would be -Inf in A2b
  drop      = coalesce(bad_high | bad_votes | bad_reg | bad_l2, TRUE)
)
cli_inform(c(
  "panel: {nrow(panel)} precincts / {n_distinct(panel$county)} counties",
  "ratio > 1.00: {sum(panel$ratio > 1)} ({round(100 * mean(panel$ratio > 1), 2)}%)",
  "ratio > {KAPPA}: {sum(panel$bad_high)} ({round(100 * mean(panel$bad_high), 2)}%)",
  "gov22_total <= {MIN_VOTES}: {sum(panel$bad_votes)}",
  "n <= {MIN_REG} (L2 denominator failure): {sum(panel$bad_reg)} \\
   [of which {sum(panel$bad_reg & !panel$bad_votes)} have usable votes -- \\
   these are DROPPED ON THE DENOMINATOR, which is selection on the thing under test]",
  "vote22 == 0: {sum(panel$bad_l2)}",
  "EXCLUDED total: {sum(panel$drop)} ({round(100 * mean(panel$drop), 2)}%)"
))
for (nm in c("bad_high", "bad_votes", "bad_reg", "bad_l2", "drop"))
  add("A0", paste0("n_", nm), "count", "count", sum(panel[[nm]]), nrow(panel), n_distinct(panel$county))

cty_exc <- panel |>
  summarize(np = n(), over1 = sum(ratio > 1), pct_over1 = 100 * mean(ratio > 1),
            med_ratio = median(ratio), max_ratio = max(ratio),
            cor_gov_n = if (sum(!drop) > 5) cor(gov22_total[!drop], n[!drop]) else NA_real_,
            .by = c(county, fips5)) |>
  arrange(desc(pct_over1))
cli_h3("per-county exceedance (counties with any ratio > 1)")
print(as.data.frame(filter(cty_exc, over1 > 0)), row.names = FALSE, digits = 3)

# NA correlation (too few usable rows) must NOT silently pass: a small county with a
# denominator error is exactly where this fires, and small counties are where it is most
# likely. Flag them rather than dropping them to FALSE.
LEVEL_SUSPECT <- cty_exc |> filter(pct_over1 > 5, is.na(cor_gov_n) | cor_gov_n > 0.75) |> pull(county)
if (any(is.na(cty_exc$cor_gov_n)))
  cli_alert_info("counties with too few usable precincts to correlate: \\
                  {paste(cty_exc$county[is.na(cty_exc$cor_gov_n)], collapse = ', ')}")
if (length(LEVEL_SUSPECT))
  cli_alert_warning("level-error counties (deflated `n`, normal within-county structure): \\
                     {paste(LEVEL_SUSPECT, collapse = ', ')}")

d <- panel |> filter(!drop) |> mutate(
  cap       = KAPPA * n,
  theta     = lg(gov22_total / cap),     # the scale the model actually fits
  log_v     = log(gov22_total),
  log_n     = log(n),
  turnout18 = vote18 / n,
  turnout20 = vote20 / n,
  dem2      = sen20_dem / pmax(sen20_dem + sen20_rep, 1)
)
stopifnot(all(is.finite(d$theta)), all(is.finite(d$log_v)))
cli_inform("clean sample: {nrow(d)} precincts / {n_distinct(d$county)} counties")

# ========================================
## A1. Baseline prior vs actual
# ========================================
# NOTE: est_turn = turnout18 * n = vote18 EXACTLY (backtest_panel.R:139 defines
# turnout18 = vote18/n, and n > MIN_REG here). It is a raw L2 count, NOT the production
# prior, which is yg_turnoutPred * n. Named accordingly in the CSV.
cli_h1("A1. Backtest prior (L2 vote18) vs actual ballots")

agg   <- sum(d$vote18) / sum(d$gov22_total)
r     <- log(pmax(d$vote18, 1) / d$gov22_total)
l2cov <- sum(d$vote22) / sum(d$gov22_total)
cli_inform(c(
  "L2 vote18 {format(round(sum(d$vote18)), big.mark = ',')} vs actual \\
   {format(round(sum(d$gov22_total)), big.mark = ',')}  ratio {round(agg, 4)}",
  "log(vote18/actual): mean {round(mean(r), 4)}  sd {round(sd(r), 4)}",
  "corr(log vote18, log actual) = {round(cor(log(pmax(d$vote18, 1)), d$log_v), 4)}",
  "L2 vote22 / actual gov22 = {round(l2cov, 4)} -- SAME election, so pure \\
   survivorship/coverage loss with no model involved",
  "=> of the {round(100 * (1 - agg), 1)}pp shortfall, {round(100 * (1 - l2cov), 1)}pp is \\
   the L2 basis and only {round(100 * (l2cov - agg), 1)}pp is the 2018->2022 change",
  "(TX statewide actual: 2018 senate ~8.37M, 2022 gov ~8.10M, true ratio ~1.03)"
))
cli_alert_warning("this is a BACKTEST artifact. The production model primes from \\
                   yg_turnoutPred * n, not L2 vote18, so it says nothing about the \\
                   production prior.")
add("A1", "l2_vote18_over_gov22", "ratio", "ratio", agg, nrow(d), n_distinct(d$county))
add("A1", "l2_vote22_over_gov22", "ratio", "ratio", l2cov, nrow(d), n_distinct(d$county))

# ========================================
## A2. Variance ladder
# ========================================
cli_h1("A2. Variance ladder (32 counties)")

# The model's own COVS_TURN (model_turnout.R:343-345) is
# {log_n, turnout22, turnout24, age, male, pred.bla, pred.his, dem2}. For a 2022 backtest
# turnout22/turnout24 are post-outcome, so the honest analogue lags them one cycle.
COVS      <- c("log_n", "turnout18", "turnout20", "p_male", "p_age65", "p_black", "p_hisp", "p_col", "dem2")
# The 0.200 constant's OWN documented spec: registration + demographics + 2018 turnout.
COVS_DOC  <- c("log_n", "turnout18", "p_male", "p_age65", "p_black", "p_hisp", "p_col")

ladder <- function(dat, yname, label) {
  f <- function(rhs, rung) {
    m <- lm(as.formula(paste(yname, "~", rhs)), data = dat)
    s <- sigma(m); r2 <- summary(m)$r.squared
    cli_inform("  {sprintf('%-26s', rung)} sd_resid {sprintf('%.4f', s)}   R2 {sprintf('%.4f', r2)}")
    add("A2", rung, label, "sd", s, nrow(dat), n_distinct(dat$county))
    s
  }
  cli_h3("{label}")
  s0 <- f("1", "intercept")
  s1 <- f("factor(county)", "county")
  s2 <- f(paste(COVS, collapse = " + "), "covariates")
  s3 <- f(paste(c(COVS, "factor(county)"), collapse = " + "), "covariates + county")
  icc_raw_ladder <- max(0, 1 - s1^2 / s0^2)
  # THE quantity the constant governs: county share of the POST-covariate variance.
  icc_resid <- max(0, 1 - s3^2 / s2^2)
  cli_inform(c("  raw county ICC (no covariates)      {round(icc_raw_ladder, 4)}",
               "  --> RESIDUAL county ICC (governs lambda) {round(icc_resid, 4)}   \\
                    [constant in use: {ICC_USED}]"))
  add("A2", "raw_county_ICC", label, "icc", icc_raw_ladder, nrow(dat), n_distinct(dat$county))
  add("A2", "residual_county_ICC", label, "icc", icc_resid, nrow(dat), n_distinct(dat$county))
  list(s0 = s0, s1 = s1, s2 = s2, s3 = s3, icc_raw = icc_raw_ladder, icc_resid = icc_resid)
}
th_full <- ladder(d, "theta", "theta")
lv_full <- ladder(d, "log_v", "log_votes")

cli_alert_info("scale matters: residual ICC is {round(th_full$icc_resid, 4)} on theta vs \\
                {round(lv_full$icc_resid, 4)} on log(votes) -- \\
                {round(th_full$icc_resid / lv_full$icc_resid, 2)}x. The model fits theta.")

# ---- A2b. What ICC_TURN = 0.200 was measured on, and the denominator's role -----------
cli_h3("A2b. provenance of the constant, and the L2 denominator")

resid_vc <- function(yv, covs, lab, part = "A2b") {
  dd <- mutate(d, .y = yv)
  v  <- vc(resid(lm(as.formula(paste(".y ~", paste(covs, collapse = " + "))), data = dd)), dd$county)
  cli_inform("  {sprintf('%-40s', lab)} cty_sd {sprintf('%.4f', v$cty_sd)}  \\
              within {sprintf('%.4f', v$within)}  icc_mom {sprintf('%.4f', v$icc_mom)}  \\
              icc_raw {sprintf('%.4f', v$icc_raw)}")
  add_vc(v, part, lab, if (identical(yv, d$theta)) "theta" else "log", nrow(dd), n_distinct(dd$county))
  v
}
v_l2 <- resid_vc(log(d$vote22),      COVS_DOC, "L2 vote22, documented spec")
v_vh <- resid_vc(d$log_v,            COVS_DOC, "votehub gov22, documented spec")
v_th <- resid_vc(d$theta,            COVS,     "theta, model's own covariates")

# Does the L2-vs-ballots gap live in the county NUMERATOR or the ICC DENOMINATOR?
bet_l2 <- v_l2$total^2 - v_l2$within^2; bet_vh <- v_vh$total^2 - v_vh$within^2
cli_inform(c(
  "between-county variance: L2 {signif(bet_l2, 4)} vs ballots {signif(bet_vh, 4)} \\
   ({round(bet_l2 / bet_vh, 2)}x)",
  "within variance:         L2 {signif(v_l2$within^2, 4)} vs ballots \\
   {signif(v_vh$within^2, 4)} ({round(v_vh$within^2 / v_l2$within^2, 2)}x)"
))
cli_alert_warning("the ICC gap is in the DENOMINATOR. The county component is similar on \\
                   both; what explodes is precinct-level residual. So L2 does not \\
                   'manufacture county correlation' -- predicting ballots from a 2026 L2 \\
                   stock adds precinct noise, which DEFLATES the ICC.")

# Decisive test: keep actual ballots as the outcome, remove the L2 denominator entirely
# by scaling against another contest in the same precinct.
d_nod <- filter(d, sen20_total > MIN_VOTES)
v_nod <- vc(resid(lm(log_v ~ log(sen20_total) + p_male + p_age65 + p_black + p_hisp + p_col,
                     data = d_nod)), d_nod$county)
cli_inform("  no-L2-denominator (log gov22 on log sen20): icc_mom \\
            {round(v_nod$icc_mom, 4)} vs {round(v_vh$icc_mom, 4)} with it \\
            [{nrow(d_nod)} precincts; {nrow(d) - nrow(d_nod)} dropped for sen20 <= {MIN_VOTES}]")
add_vc(v_nod, "A2b", "no_l2_denominator", "log", nrow(d_nod), n_distinct(d_nod$county))

# ---- Jackknife SE: with J=32 the county component is poorly determined ---------------
jk <- map_dbl(unique(d$county), function(cc) {
  dd <- filter(d, county != cc)
  m2 <- lm(as.formula(paste("theta ~", paste(COVS, collapse = " + "))), data = dd)
  m3 <- lm(as.formula(paste("theta ~", paste(c(COVS, "factor(county)"), collapse = " + "))), data = dd)
  max(0, 1 - sigma(m3)^2 / sigma(m2)^2)
})
J <- length(jk); se_jk <- sqrt((J - 1) / J * sum((jk - mean(jk))^2))
cli_inform("residual county ICC (theta) {round(th_full$icc_resid, 4)} \\
            +/- {round(se_jk, 4)} (jackknife SE, J={J}); \\
            vs {ICC_USED} that is {round(abs(ICC_USED - th_full$icc_resid) / se_jk, 1)} SE")
cli_alert_info("a LOO RANGE is not a confidence interval -- jackknife deviations scale by \\
                sqrt(J-1). The 32-county panel cannot cleanly separate these candidates.")
add("A2b", "residual_county_ICC_jackknife_se", "theta", "se", se_jk, nrow(d), J)

if (length(LEVEL_SUSPECT)) {
  d_nl <- filter(d, !county %in% LEVEL_SUSPECT)
  m2 <- lm(as.formula(paste("theta ~", paste(COVS, collapse = " + "))), data = d_nl)
  m3 <- lm(as.formula(paste("theta ~", paste(c(COVS, "factor(county)"), collapse = " + "))), data = d_nl)
  i_nl <- max(0, 1 - sigma(m3)^2 / sigma(m2)^2)
  cli_inform("dropping {paste(LEVEL_SUSPECT, collapse = ', ')}: residual ICC {round(i_nl, 4)}")
  add("A2b", "residual_county_ICC_no_level_err", "theta", "icc", i_nl, nrow(d_nl), n_distinct(d_nl$county))
}

# ========================================
## A3. County component at full state coverage
# ========================================
# No L2 join, no registrant denominator, 254 counties, J large enough for a tight SE.
# CAVEAT: no covariates exist here, so these are RAW ICCs -- NOT the residual quantity
# the constant governs. They bound the county component from above; do not retune off
# them directly. They are also a LOWER bound in another sense: differencing two contests
# removes the county effect they share, and that shared part is the persistent one.
cli_h1("A3. County component, 254 counties, returns-based scale (RAW ICC)")

res <- read_parquet(file.path(PANELS, "TX_results.parquet")) |>
  as_tibble() |> filter(!is.na(gov22_total), gov22_total > MIN_VOTES)
cli_inform("TX_results: {nrow(res)} precincts / {n_distinct(res$fips5)} counties, \\
            gov22 total {format(sum(res$gov22_total), big.mark = ',')} [statewide ~8.10M]")

for (base in c("sen20_total", "pres24_total")) {
  rr <- res |> filter(!is.na(.data[[base]]), .data[[base]] > MIN_VOTES)
  # Free the coefficient rather than constraining it to 1, which a raw ratio does:
  # a constrained coefficient inflates residual variance and deflates the ICC.
  fm <- as.formula(sprintf("log(gov22_total) ~ log(%s)", base))
  v <- vc(resid(lm(fm, data = rr)), rr$fips5)
  cli_inform("  log gov22 ~ log {base}: cty_sd {round(v$cty_sd, 4)}  \\
              within {round(v$within, 4)}  icc_mom {round(v$icc_mom, 4)}")
  add_vc(v, "A3", paste0("vs_", base), "log", nrow(rr), n_distinct(rr$fips5))
}
r32 <- res |> filter(fips5 %in% unique(panel$fips5), !is.na(sen20_total), sen20_total > MIN_VOTES)
v32 <- vc(resid(lm(log(gov22_total) ~ log(sen20_total), data = r32)), r32$fips5)
cli_inform("  restricted to the 32 panel counties: icc_mom {round(v32$icc_mom, 4)} \\
            -- difference vs 254 is SAMPLE, not denominator")
add_vc(v32, "A3", "vs_sen20_32cty", "log", nrow(r32), n_distinct(r32$fips5))

# ========================================
## A4. Mechanism attribution
# ========================================
cli_h1("A4. Mechanism attribution")

# (a) Jensen, by Gauss-Hermite so the answer is exact and seed-free. Each sigma is paired
#     with ITS OWN fitted mean -- pairing an unconditional sigma with a fully-conditioned
#     mu describes a model that knows everything about its mean and nothing about its
#     precision, which is not a model anyone runs.
# Deterministic stratified grid on the standard normal: E[g(mu + sigma Z)] averaged over
# z_i = qnorm((i - 0.5)/N). Exact to far more digits than needed for a smooth bounded g,
# with no RNG and no extra package.
GH_Z <- qnorm((seq_len(GH_NODES) - 0.5) / GH_NODES)
e_invlogit <- function(mu, sg) rowMeans(plogis(outer(mu, sg * GH_Z, "+")))
jensen_for <- function(rhs, sg, lab) {
  mu   <- fitted(lm(as.formula(paste("theta ~", rhs)), data = d))
  infl <- sum(d$cap * e_invlogit(mu, sg)) / sum(d$cap * plogis(mu))
  cli_inform("  {sprintf('%-24s', lab)} sigma {sprintf('%.3f', sg)}: \\
              E[T] inflated {sprintf('%+.2f%%', 100 * (infl - 1))}")
  add("A4", paste0("jensen_", lab), "ratio", "ratio", infl, nrow(d), n_distinct(d$county))
}
jensen_for(paste(c(COVS, "factor(county)"), collapse = " + "), th_full$s3, "cov+county")
jensen_for(paste(COVS, collapse = " + "),                      th_full$s2, "covariates")
jensen_for("1",                                                th_full$s0, "unconditional")
cli_alert_info("these are OLS residual sigmas, a FLOOR on the model's predictive sd \\
                (which adds county-RE and parameter uncertainty). Jensen is priced at \\
                the most favourable sigma in the set.")

# (b) Cap. Must be computed on the UNFILTERED panel: A0 dropped ratio > KAPPA, and
#     gov22/cap > 0.8 is identical to ratio > 1.0, so measuring on `d` would exclude
#     exactly the observations that could falsify the verdict.
near <- mean(panel$ratio > NEAR_CAP * KAPPA)
cli_inform("  above {NEAR_CAP} of cap (= ratio > {NEAR_CAP * KAPPA}), UNFILTERED panel: \\
            {sum(panel$ratio > NEAR_CAP * KAPPA)} ({round(100 * near, 2)}%)  \\
            [model_turnout.R:151 independently records kappa binding on 1.16%]")
add("A4", "frac_near_cap_unfiltered", "ratio", "ratio", near, nrow(panel), n_distinct(panel$county))

# (b2) Shrinkage implied by the constant vs the measurement.
icc_best <- th_full$icc_resid
cli_inform(c(
  "  ICC {ICC_USED} (in use)            -> lambda {round(lam(ICC_USED), 1)}",
  "  ICC {round(icc_best, 3)} (theta, residual) -> lambda {round(lam(icc_best), 1)}",
  "  county effects shrunk {round(lam(icc_best) / lam(ICC_USED), 1)}x less than the data supports"
))
add("A4", "lambda_in_use",   "weight", "weight", lam(ICC_USED), nrow(d), n_distinct(d$county))
add("A4", "lambda_measured", "weight", "weight", lam(icc_best), nrow(d), n_distinct(d$county))
cli_alert_warning("ICC_TURN ALSO parameterizes the synthetic generator \\
                   (model_turnout.R:773, c_tau sd = sd_tau * sqrt(ICC/(1-ICC))). Changing \\
                   it moves DGP and fit together, so an ablation on simulated nights is \\
                   uninformative. Split the constant into _FIT and _DGP, or ablate on \\
                   real returns only.")

# (c) Mode composition. L2 mode SHARES only -- there is no precinct x mode ballots-cast
#     file for TX 2022. This inherits the SAME L2 coverage caveat as A2b, so treat it as
#     an upper bound rather than a clean measurement.
mode_raw <- d |> transmute(county, mail = vote22_mail, early = vote22_early, eday = vote22_eday)
for (nm in c("eday", "mail")) {
  # Exclude zero-mode cells rather than clamping them: a clamp at 1e-4 fabricates an
  # |eta| ~ 8-9 outlier that would drive both the sd and the ICC.
  ok <- mode_raw$early > 0 & mode_raw[[nm]] > 0
  eta <- log(mode_raw[[nm]][ok] / mode_raw$early[ok])
  v <- vc(eta, mode_raw$county[ok])
  cli_inform("  eta = log({nm}/early): sd {round(v$total, 4)}  within {round(v$within, 4)}  \\
              icc_mom {round(v$icc_mom, 4)}  [{sum(ok)} cells, {sum(!ok)} zero-mode excluded]")
  add_vc(v, "A4", paste0("eta_", nm, "_early"), "log_ratio", sum(ok), n_distinct(mode_raw$county[ok]))
}
cli_alert_info("mode LEVELS are not validatable for 2022; these are L2 shares and are an \\
                UPPER bound, by the same argument that deflates the A2b ICC.")

# ========================================
## Write
# ========================================
out <- bind_rows(rows) |> arrange(part, quantity, scale) |> mutate(value = round(value, 6))
stopifnot(all(is.finite(out$value)), !anyNA(out$quantity),
          !anyDuplicated(out[c("part", "quantity", "scale", "statistic")]))
write_csv(out, OUT_CSV)
saveRDS(list(ladder_theta = th_full, ladder_logv = lv_full, jackknife = jk, se_jk = se_jk,
             vc = list(l2 = v_l2, votehub = v_vh, theta = v_th, no_denom = v_nod),
             cty_exc = cty_exc, level_suspect = LEVEL_SUSPECT, covs = COVS, covs_doc = COVS_DOC,
             n_panel = nrow(panel), n_clean = nrow(d), run_at = Sys.time(),
             session = sessionInfo()), OUT_RDS)
cli_alert_success("wrote {OUT_CSV} ({nrow(out)} rows) and {OUT_RDS}")

# Figure: the ICC estimators with the jackknife SE, which is the comparison that carries
# the argument -- a residual-sd ladder cannot show that the candidates overlap.
pdf(file.path(OUT_FIG, "turnout_icc_tx2022.pdf"), width = 7.5, height = 4.4)
op <- par(mar = c(9, 4.2, 3, 1), cex = 0.8)
est <- c("theta\nresidual" = th_full$icc_resid, "log votes\nresidual" = lv_full$icc_resid,
         "theta\nraw" = th_full$icc_raw, "no L2 denom\n(log)" = v_nod$icc_mom,
         "254 cty\nvs sen20" = NA, "in use" = ICC_USED)
est[["254 cty\nvs sen20"]] <- out$value[out$part == "A3" & out$quantity == "vs_sen20_total_icc_mom"]
bp <- barplot(est, las = 2, ylab = "County ICC", col = c(rep("grey80", 5), "grey45"),
              border = NA, ylim = c(0, max(est, na.rm = TRUE) * 1.25),
              main = "TX 2022 turnout: county ICC by estimator")
arrows(bp[1], th_full$icc_resid - se_jk, bp[1], th_full$icc_resid + se_jk,
       angle = 90, code = 3, length = 0.05, lwd = 1.5)
# bar 1's label clears the error-bar cap, not the bar top, or the two collide
lab_y <- est; lab_y[1] <- th_full$icc_resid + se_jk
text(bp, lab_y, sprintf("%.3f", est), pos = 3, cex = 0.8, xpd = NA)
mtext("bar 1 carries the jackknife SE (J=32); bars 2-5 are not all the same estimand",
      side = 1, line = 7.4, cex = 0.7)
par(op); invisible(dev.off())
cli_alert_success("wrote {file.path(OUT_FIG, 'turnout_icc_tx2022.pdf')}")
