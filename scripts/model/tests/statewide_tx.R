# ========================================
## Rough statewide estimate for TX from the 32-county precinct model
##
## Our precinct model covers only the 32 counties (about two-thirds of the statewide vote).
## For the other 222 counties this predicts each precinct's swing from its demographics,
## using how swing varies with demographics across the 32 counties tonight, and applies
## it to the precinct's Harris 2024 share. No rest-of-state returns are used.
##
## At each checkpoint:
##   1. y_i = logit(model's two-party Dem share in precinct i) - logit(i's Harris 2024 share)
##      for every precinct in the 32 counties, weighted by its predicted votes
##   2. regress y on demographics (L2: Hispanic, Black, 65+, male and college shares,
##      2018 and 2020 turnout, log registrants) and the precinct's baseline lean
##   3. predict y for every precinct in the other 222 counties (statewide L2 file +
##      votehub's precinct Harris 2024 results); precincts that don't join to L2 get the
##      32 counties' average swing
##   4. rest-of-state share = vote-weighted average (2022 governor votes as weights)
##   5. uncertainty, per posterior draw: the 32-county share and turnout draws, the rest's
##      swing moving with the 32 counties', an extra error for the rest swinging
##      differently (GAP_SD), and the rest/32 vote ratio (2022 ratio, +/- its spread over
##      2020-2024)
## GAP_SD is measured on past TX elections: the same procedure, with the 32 counties'
## final precinct results known, missed the rest of the state by GAP_SD at most (logit
## scale) across sen20 -> gov22, gov22 -> pres24 and sen20 -> pres24. The uniform-swing
## version (no demographics) is computed alongside for comparison.
##
## Used by test_sim_night.R; needs input_data/l2/tx_*_precincts.parquet,
## input_data/votehub-shapefiles/estimated_past_results.csv and TX_precinct.parquet.
# ========================================
SW_FEATURES = c("p_hisp", "p_black", "p_age_65_up", "p_male", "p_col", "turnout18", "turnout20", "log_n")

sw_key = function(fips5, prec) paste(fips5, str_pad(str_remove_all(toupper(trimws(prec)), "[^0-9A-Z]"), 4, pad = "0"))

# weighted least squares, returns coefficients (features + lb, with intercept)
sw_fit = function(X, y, w) {
  ok = stats::complete.cases(X) & is.finite(y) & is.finite(w) & w > 0
  A = cbind(1, as.matrix(X[ok, , drop = FALSE]))
  stats::lm.wfit(A, y[ok], w[ok])$coefficients |> (\(b) { b[is.na(b)] = 0; b })()
}
sw_pred = function(beta, X) drop(cbind(1, as.matrix(X)) %*% beta)

statewide_setup = function(inp) {
  p_l2 = list.files(file.path(DIR_26, "input_data", "l2"), pattern = "^tx_.*_precincts\\.parquet$", full.names = TRUE)
  p_vh = file.path(DIR_26, "input_data", "votehub-shapefiles", "estimated_past_results.csv")
  if (!length(p_l2) || !file.exists(p_vh)) {
    cat("Statewide estimate skipped: needs input_data/l2/tx_*_precincts.parquet and votehub estimated_past_results.csv\n")
    return(NULL)
  }
  l2 = read_pq(sort(p_l2, decreasing = TRUE)[1]) |>
    mutate(fips5 = paste0("48", str_pad(FIPS, 3, pad = "0")), key = sw_key(fips5, precinct), nn = pmax(n, 1),
           # same definitions as prep_tx_model_inputs.R
           p_hisp = race3 / nn, p_black = race2 / nn, p_age_65_up = age4 / nn, p_male = male / nn,
           p_col = if_else(col_known + noncol_known > 0, col_known / (col_known + noncol_known), NA_real_),
           turnout18 = vote18 / nn, turnout20 = vote20 / nn, log_n = log(nn)) |>
    distinct(key, .keep_all = TRUE) |>
    select(key, all_of(SW_FEATURES))
  f32 = unique(inp$county_fips)
  vh = read_csv(p_vh, col_types = cols(.default = "c"), progress = FALSE) |>
    filter(state_abbr == "TX") |>
    mutate(fips5 = str_extract(unit_id, "^[0-9]+"), key = sw_key(fips5, str_remove(unit_id, "^[0-9]+-:-")),
           in32 = fips5 %in% f32, across(matches("^(pres24|gov22|sen20)_"), as.numeric)) |>
    left_join(l2, by = "key")
  two = function(d, e) d[[paste0(e, "_dem")]] / (d[[paste0(e, "_dem")]] + d[[paste0(e, "_rep")]])
  tw = function(d, e) sum(d[[paste0(e, "_dem")]], na.rm = TRUE) / sum(d[[paste0(e, "_dem")]] + d[[paste0(e, "_rep")]], na.rm = TRUE)

  # ---- historical check: how far each method missed the rest of the state ----------
  hist = lapply(list(c("sen20", "gov22"), c("gov22", "pres24"), c("sen20", "pres24")), function(pr) {
    x = vh |>
      mutate(b2 = two(vh, pr[1]), t2 = two(vh, pr[2]),
             w = .data[[paste0(pr[2], "_dem")]] + .data[[paste0(pr[2], "_rep")]],
             wb = .data[[paste0(pr[1], "_dem")]] + .data[[paste0(pr[1], "_rep")]]) |>
      filter(wb > 20, w > 20, b2 > 0.005, b2 < 0.995, t2 > 0.005, t2 < 0.995) |>
      mutate(lb = qlogis(b2), y = qlogis(t2) - lb)
    A = filter(x, in32); R = filter(x, !in32)
    sw32 = qlogis(weighted.mean(A$t2, A$w)) - qlogis(weighted.mean(A$b2, A$w))
    beta = sw_fit(A[, c(SW_FEATURES, "lb")], A$y, A$w)
    yr = sw_pred(beta, R[, c(SW_FEATURES, "lb")]); yr[!is.finite(yr)] = sw32
    act = weighted.mean(R$t2, R$w)
    pu = weighted.mean(plogis(R$lb + sw32), R$w); pd = weighted.mean(plogis(R$lb + yr), R$w)
    fr = sum(R$w) / (sum(R$w) + sum(A$w))
    list(pair = paste(pr, collapse = " to "),
         gap_u = qlogis(act) - qlogis(pu), gap_d = qlogis(act) - qlogis(pd),
         err_u = round(100 * fr * (pu - act), 2), err_d = round(100 * fr * (pd - act), 2))
  })
  ratios = vapply(c("sen20", "gov22", "pres24"), \(e) sum(vh[[paste0(e, "_total")]][!vh$in32], na.rm = TRUE) /
                                                       sum(vh[[paste0(e, "_total")]][vh$in32], na.rm = TRUE), 0)
  rest = vh |> filter(!in32) |>
    mutate(b2 = pres24_dem / (pres24_dem + pres24_rep), w = gov22_dem + gov22_rep,
           w = if_else(is.finite(w) & w > 0, w, coalesce(pres24_dem + pres24_rep, 0))) |>
    filter(is.finite(b2), b2 > 0.005, b2 < 0.995, w > 0) |>
    mutate(lb = qlogis(b2))
  # the 32 counties' own precincts, in TX_precinct order, with the same features
  p32 = inp |> transmute(geo, lb = qlogis(pmin(pmax(dem2, 0.005), 0.995)), !!!rlang::syms(setdiff(SW_FEATURES, "log_n")),
                         log_n = log(pmax(n, 1)))
  st = list(rest = rest, p32 = p32,
            base32 = tw(vh[vh$in32, ], "pres24"),
            gap_sd_d = max(abs(vapply(hist, \(h) h$gap_d, 0))), gap_sd_u = max(abs(vapply(hist, \(h) h$gap_u, 0))),
            ratio = ratios[["gov22"]], ratio_sd = sd(ratios),
            hist = lapply(hist, \(h) h[c("pair", "err_u", "err_d")]),
            rest_join = sum(rest$w[stats::complete.cases(rest[, SW_FEATURES])]) / sum(rest$w))
  cat(sprintf("Statewide setup: %d rest-of-state precincts (%.0f%% of their votes have L2 demographics); extra error sd %.3f logit (demographic) / %.3f (uniform)\n",
              nrow(rest), 100 * st$rest_join, st$gap_sd_d, st$gap_sd_u))
  st
}

# s2_prec: model's two-party Dem share per 32-county precinct (in st$p32 order);
# w_prec: its predicted votes; S2, T32: posterior draws for the 32 counties
statewide_draws = function(st, s2_prec, w_prec, S2, T32, noise = TRUE) {
  p = st$p32 |> mutate(y = qlogis(pmin(pmax(s2_prec, 0.005), 0.995)) - lb, w = w_prec)
  beta = sw_fit(p[, c(SW_FEATURES, "lb")], p$y, p$w)
  sw32 = qlogis(weighted.mean(s2_prec, w_prec, na.rm = TRUE)) - qlogis(weighted.mean(plogis(p$lb), w_prec, na.rm = TRUE))
  yr = sw_pred(beta, st$rest[, c(SW_FEATURES, "lb")]); yr[!is.finite(yr)] = sw32
  R_d = weighted.mean(plogis(st$rest$lb + yr), st$rest$w)                 # demographic swing
  R_u = weighted.mean(plogis(st$rest$lb + sw32), st$rest$w)               # uniform swing
  n = length(S2)
  dev = qlogis(pmin(pmax(S2, 1e-4), 1 - 1e-4)) - qlogis(mean(S2))         # 32-county draw deviation
  ratio = if (noise) rnorm(n, st$ratio, st$ratio_sd) else st$ratio
  out = function(R, sd) {
    Rj = plogis(qlogis(R) + dev + if (noise) rnorm(n, 0, sd) else 0)
    (S2 * T32 + Rj * T32 * ratio) / (T32 * (1 + ratio))
  }
  list(dem = out(R_d, st$gap_sd_d), uni = out(R_u, st$gap_sd_u))
}
