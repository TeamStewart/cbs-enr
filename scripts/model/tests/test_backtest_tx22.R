# ========================================
## TX 2022 governor backtest: estimate and interval at each stage of reporting
##
## Rebuilds Young-Kyung's panel (tx_backtest_panel.R) from the shared Dropbox files and
## runs the model core (model_core.R) the way tx_backtest.R does, at 50 checkpoints,
## under three reporting orders:
##   county_re    whole counties report smallest first; county random effect ON
##   county_nore  same order; county random effect OFF
##   random_re    precincts report in random order; county random effect ON
## Out-of-sample: every predictor is pre-2022 (2020 Senate share, 2018 turnout, L2).
##
## Output: 26_general/model_tests/backtest_tx22_results.csv, one row per order x checkpoint
## Run from the cbs-enr folder:  Rscript scripts/model/tests/test_backtest_tx22.R
## Takes a few minutes.
# ========================================
source("scripts/model/tests/model_test_common.R")
set.seed(2022)
DRAWS = 500

# ---- 1. panel: L2 frame + votehub precinct results --------------------------------
l2 = read_pq(file.path(DIR_26, "input_data", "l2_precinct_frame_method.parquet")) |>
  filter(state == "TX")
key_of = function(fips5, prec) paste(fips5, str_pad(str_remove_all(toupper(trimws(prec)), "[^0-9A-Z]"), 4, pad = "0"))
l2 = l2 |> mutate(fips5 = paste0("48", str_pad(FIPS, 3, pad = "0")), key = key_of(fips5, precinct))

vh = read_csv(file.path(DIR_26, "input_data", "votehub-shapefiles", "estimated_past_results.csv"),
              col_types = cols(.default = "c"), progress = FALSE) |>
  filter(state_abbr == "TX") |>
  mutate(fips5 = str_extract(unit_id, "^[0-9]+"), prec = str_remove(unit_id, "^[0-9]+-:-"), key = key_of(fips5, prec)) |>
  mutate(across(matches("^(gov22|sen20)_"), as.numeric)) |>
  select(key, starts_with("gov22_"), starts_with("sen20_")) |>
  distinct(key, .keep_all = TRUE)

d = inner_join(l2, vh, by = "key")
cat(sprintf("Joined %d of %d L2 precincts (%.1f%%)\n", nrow(d), nrow(l2), 100 * nrow(d) / nrow(l2)))
d = d |>
  transmute(
    geo = key, county = county_name,
    y_votes = gov22_total, y_dem = gov22_dem, y_rep = gov22_rep, y_oth = gov22_oth,
    dem2 = (sen20_dem + 1e-6) / (sen20_dem + sen20_rep + 2e-6),
    oth_base = pmin(pmax(sen20_oth / pmax(sen20_total, 1), 1e-4), 0.15),
    est_turn = pmax(vote18, 1),
    log_n = log(pmax(n, 1)), turnout18 = vote18 / pmax(n, 1), turnout20 = vote20 / pmax(n, 1),
    p_male = male / pmax(male + fem, 1), p_age_65 = age4 / pmax(n, 1),
    p_black = race2 / pmax(n, 1), p_hisp = race3 / pmax(n, 1),
    p_col = col_known / pmax(col_known + noncol_known, 1),
    sen20_total, n
  ) |>
  filter(y_votes > 20, sen20_total > 20, n > 50, is.finite(dem2), dem2 > 0, dem2 < 1)
COVS = c("log_n", "turnout18", "turnout20", "p_male", "p_age_65", "p_black", "p_hisp", "p_col")
d = d |> mutate(across(all_of(setdiff(COVS, "log_n")), ~ as.numeric(scale(.x)))) |> filter(if_all(all_of(COVS), is.finite))
N = nrow(d)
TRUTH = list(turn = sum(d$y_votes), twop = sum(d$y_dem) / (sum(d$y_dem) + sum(d$y_rep)))
cat(sprintf("Panel: %d precincts, %d counties; true 2022 two-party Dem %.2f%%, %s votes\n",
            N, n_distinct(d$county), 100 * TRUTH$twop, format(TRUTH$turn, big.mark = ",")))

# ---- 2. run one reporting order --------------------------------------------------
run_order = function(order, use_re, fracs) {
  dd = d
  cty = dd |> summarize(votes = sum(y_votes), .by = county) |> arrange(votes)
  dd = dd |> left_join(mutate(cty, cty_rank = row_number()), by = "county")
  dd$pos = if (order == "random") runif(N) else (dd$cty_rank + runif(N, 0, 0.9)) / (nrow(cty) + 1)
  X = cbind(1, as.matrix(dd[, COVS]))
  CTY = if (use_re) model.matrix(~ factor(dd$county) - 1) else matrix(0, N, 0)
  lam = (1 - ICC) / ICC
  ypt = log(dd$est_turn + 0.1); yps = logit(dd$dem2); ypo = logit(dd$oth_base)
  tot2 = dd$y_dem + dd$y_rep
  y_t = log(dd$y_votes + 0.1); y_s = logit((dd$y_dem + .5) / (tot2 + 1)); y_o = logit((dd$y_oth + .5) / (dd$y_votes + 1))
  map_dfr(fracs, function(frac) {
    obs = dd$pos <= frac
    if (sum(obs) < 40) return(NULL)
    wt = { w = pmax(dd$y_votes, 1); w[!obs] = 1; w / mean(w[obs]) }
    fits = list(tau = fit_conj_re(y_t, X, CTY, obs, ypt, PRIOR_N / N, lam),
                tp  = fit_conj_re(y_o, X, CTY, obs, ypo, PRIOR_N / N, lam, wts = wt),
                s   = fit_conj_re(y_s, X, CTY, obs, yps, PRIOR_N / N, lam, wts = wt))
    if (any(vapply(fits, is.null, TRUE))) return(NULL)
    A = cbind(X, CTY)
    mu = lapply(fits, \(f) draw_beta_re(f, DRAWS) %*% t(A))
    eps = lapply(vapply(fits, sig_of, 0), \(s) matrix(rnorm(DRAWS * N, 0, s), DRAWS, N))
    tau = exp(mu$tau + eps$tau) - 0.1; tau[tau < 0] = 0
    tp = invlogit(mu$tp + eps$tp); s = invlogit(mu$s + eps$s)
    nb = sum(obs)                                   # counted precincts: use their actual votes
    tau[, obs] = matrix(dd$y_votes[obs], DRAWS, nb, byrow = TRUE)
    tp[, obs]  = matrix(dd$y_oth[obs] / pmax(dd$y_votes[obs], 1), DRAWS, nb, byrow = TRUE)
    s[, obs]   = matrix(dd$y_dem[obs] / pmax(tot2[obs], 1), DRAWS, nb, byrow = TRUE)
    two = tau * (1 - tp); dem = rowSums(two * s); rep = rowSums(two * (1 - s)); tot = rowSums(tau)
    S2 = dem / (dem + rep); ci = cred(S2); ct = cred(tot)
    tibble(order = order, county_effect = use_re, checkpoint = frac, n_precincts = nb,
           n_counties = n_distinct(dd$county[obs]), pct_votes = sum(dd$y_votes[obs]) / TRUTH$turn,
           raw_twop = sum(dd$y_dem[obs]) / sum(tot2[obs]),
           est_twop = mean(S2), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]], p_dem_win = mean(S2 > 0.5),
           est_turnout = mean(tot), lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]],
           truth_twop = TRUTH$twop, truth_turnout = TRUTH$turn)
  })
}

fracs = c(seq(0.02, 0.98, by = 0.02), 1)
res = bind_rows(
  run_order("county_size", TRUE,  fracs),
  run_order("county_size", FALSE, fracs),
  run_order("random",      TRUE,  fracs)
) |>
  mutate(covers = truth_twop >= lo_twop & truth_twop <= hi_twop,
         confident_wrong = (truth_twop > 0.5 & hi_twop < 0.5) | (truth_twop < 0.5 & lo_twop > 0.5))

out = file.path(DIR_OUT, "backtest_tx22_results.csv")
write_csv(res, out)
cat("\nSummary (two-party Dem share):\n")
res |>
  summarize(checkpoints = n(), interval_holds_truth = sum(covers), confident_wrong = sum(confident_wrong),
            last_wrong_at_pct_counted = if (any(confident_wrong)) round(100 * max(pct_votes[confident_wrong]), 1) else NA,
            .by = c(order, county_effect)) |>
  print()
cat("\nWrote", out, "\n")
