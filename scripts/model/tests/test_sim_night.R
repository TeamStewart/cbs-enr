# ========================================
## Run the model core on one of Ethan's simulated nights
##
## Reads 26_sim/<scenario>/TX_truth.parquet and TX_schedule.parquet (from
## scripts/sim/make_sim_tx.R on branch ethan-sim) plus our model inputs, and refits the
## model every 15 minutes from 8pm to 4am ET, then at four later times.
##
## Model: model_core.R on precinct x vote-mode cells (early, eday, mail), each mode fit
## separately, same covariates / g-prior / county effect as the backtest. Partial counts:
## shares are fit on every cell with votes counted (weighted by votes); turnout only on
## fully counted cells; a partly counted cell is never predicted below its count. No
## cross-mode correlation and mode offset delta = 0 (the data contract's current default).
## Snapshot logic follows scripts/sim/sim_snapshot.R; censored precincts never report.
##
## Output: 26_general/model_tests/sim_<scenario>_results.csv, one row per race x time
## Run from the cbs-enr folder:
##   Rscript scripts/model/tests/test_sim_night.R            # scenario tx_base
##   Rscript scripts/model/tests/test_sim_night.R my_scenario
## Takes a few minutes.
# ========================================
source("scripts/model/tests/model_test_common.R")
args = commandArgs(trailingOnly = TRUE)
SCENARIO = if (length(args) >= 1) args[1] else "tx_base"
set.seed(2026)
DRAWS = 300
TZ = "America/New_York"
METHODS = c("early", "eday", "mail")

dir_sc = file.path(DIR_SIM, SCENARIO)
truth = read_pq(file.path(dir_sc, "TX_truth.parquet")) |> mutate(across(c(geo, county_cbs), as.character))
sched = read_pq(file.path(dir_sc, "TX_schedule.parquet")) |> mutate(geo = as.character(geo), t = lubridate::with_tz(t, TZ))
prec = read_pq(file.path(DIR_26, "input_data", "model_inputs", "TX_precinct.parquet"))
mh = read_pq(file.path(DIR_26, "input_data", "model_inputs", "TX_precinct_mode_history.parquet")) |> filter(election == "2022_general")
cat(sprintf("Scenario %s: %d precincts, %d censored\n", SCENARIO, n_distinct(truth$geo), n_distinct(truth$geo[truth$censored])))

# ---- cells and priors -----------------------------------------------------------
COVS = c("log_n", "turnout18", "turnout20", "p_male", "p_age_65_up", "p_black", "p_hisp", "p_col")
share = mh |> mutate(sh = voters / sum(voters), .by = geo) |> select(geo, method, sh) |> filter(is.finite(sh))
cells = prec |>
  select(geo, county_name, n, est_turnout, dem2, oth_base, all_of(setdiff(COVS, "log_n"))) |>
  cross_join(tibble(method = METHODS)) |>
  left_join(share, by = c("geo", "method")) |>
  mutate(sh = coalesce(sh, sum(sh * n, na.rm = TRUE) / sum(n[!is.na(sh)])), .by = c(county_name, method)) |>
  mutate(sh = sh / sum(sh), .by = geo) |>
  mutate(log_n = log(pmax(n, 1)), across(all_of(setdiff(COVS, "log_n")), ~ as.numeric(scale(.x))))

final = truth |>
  mutate(party = if_else(party %in% c("dem", "rep"), party, "oth")) |>
  summarize(votes = sum(votes), .by = c(contest, geo, method, party)) |>
  pivot_wider(names_from = party, values_from = votes, values_fill = 0)
censored = distinct(truth, geo, censored)

frac_at = function(time) {
  landed = filter(sched, !is.na(t), !is.na(cum_frac), t <= time)
  if (nrow(landed) == 0) return(tibble(geo = character(), method = character(), frac = numeric()))
  summarize(landed, frac = max(cum_frac), .by = c(geo, method))
}

run_contest = function(contest_id, times) {
  fin = filter(final, contest == contest_id)
  truth_twop = sum(fin$dem) / (sum(fin$dem) + sum(fin$rep))
  base = cells |>
    left_join(fin, by = c("geo", "method")) |>
    mutate(across(c(dem, rep, oth), ~ coalesce(.x, 0))) |>
    left_join(censored, by = "geo")
  map_dfr(times, function(tm) {
    d = base |>
      left_join(frac_at(tm), by = c("geo", "method")) |>
      mutate(frac = if_else(coalesce(censored, FALSE), 0, coalesce(frac, 0)),
             c_dem = round(dem * frac), c_rep = round(rep * frac), c_oth = round(oth * frac),
             c_tot = c_dem + c_rep + c_oth)
    sims = matrix(0, DRAWS, 3, dimnames = list(NULL, c("dem", "rep", "tot")))
    for (m in METHODS) {
      dm = filter(d, method == m); N = nrow(dm)
      X = cbind(1, as.matrix(dm[, COVS])); CTY = model.matrix(~ factor(dm$county_name) - 1); A = cbind(X, CTY)
      done = dm$frac >= 1 & dm$c_tot > 0
      some = dm$c_tot > 0
      ypt = log(dm$est_turnout * dm$sh + 0.1); yps = logit(dm$dem2); ypo = logit(dm$oth_base)
      y_t = log(dm$c_tot + 0.1)
      y_s = logit((dm$c_dem + .5) / (dm$c_dem + dm$c_rep + 1)); y_o = logit((dm$c_oth + .5) / (dm$c_tot + 1))
      wt = { w = pmax(dm$c_tot, 1); w[!some] = 1; if (any(some)) w / mean(w[some]) else w }
      fit = function(y, obs, prior, w = NULL) if (sum(obs) >= 20) fit_conj_re(y, X, CTY, obs, prior, PRIOR_N / N, (1 - ICC) / ICC, wts = w) else NULL
      ft = fit(y_t, done, ypt); fs = fit(y_s, some, yps, wt); fo = fit(y_o, some, ypo, wt)
      # before a mode has 20 usable cells: draw around the prior (shared shift + cell noise, sd 0.3)
      draw = function(f, prior) if (is.null(f)) {
        matrix(rnorm(DRAWS, 0, 0.3), DRAWS, N) + matrix(prior, DRAWS, N, byrow = TRUE) + matrix(rnorm(DRAWS * N, 0, 0.3), DRAWS, N)
      } else {
        draw_beta_re(f, DRAWS) %*% t(A) + matrix(rnorm(DRAWS * N, 0, sig_of(f)), DRAWS, N)
      }
      tau = exp(draw(ft, ypt)) - 0.1; tau[tau < 0] = 0
      s = invlogit(draw(fs, yps)); tp = invlogit(draw(fo, ypo))
      cnt = matrix(dm$c_tot, DRAWS, N, byrow = TRUE)
      tau = pmin(pmax(tau, cnt), matrix(1.25 * dm$n, DRAWS, N, byrow = TRUE) + cnt)   # >= counted, <= 1.25 x registrants
      donem = matrix(dm$frac >= 1, DRAWS, N, byrow = TRUE); tau[donem] = cnt[donem]
      rem = tau - cnt
      sims[, "dem"] = sims[, "dem"] + rowSums(matrix(dm$c_dem, DRAWS, N, byrow = TRUE) + rem * (1 - tp) * s)
      sims[, "rep"] = sims[, "rep"] + rowSums(matrix(dm$c_rep, DRAWS, N, byrow = TRUE) + rem * (1 - tp) * (1 - s))
      sims[, "tot"] = sims[, "tot"] + rowSums(tau)
    }
    S2 = sims[, "dem"] / (sims[, "dem"] + sims[, "rep"]); ci = cred(S2); ct = cred(sims[, "tot"])
    tot_m = function(m) sum((d$dem + d$rep + d$oth)[d$method == m])
    tibble(race = contest_id, time = format(tm, "%Y-%m-%d %H:%M"),
           pct_votes = sum(d$c_tot) / sum(d$dem + d$rep + d$oth),
           pct_early = sum(d$c_tot[d$method == "early"]) / tot_m("early"),
           pct_eday = sum(d$c_tot[d$method == "eday"]) / tot_m("eday"),
           pct_mail = sum(d$c_tot[d$method == "mail"]) / tot_m("mail"),
           raw_twop = sum(d$c_dem) / max(sum(d$c_dem + d$c_rep), 1),
           est_twop = mean(S2), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]], p_dem_win = mean(S2 > 0.5),
           est_turnout = mean(sims[, "tot"]), lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]],
           truth_twop = truth_twop, truth_turnout = sum(fin$dem + fin$rep + fin$oth))
  })
}

times = c(seq(as.POSIXct("2026-11-03 20:00", tz = TZ), as.POSIXct("2026-11-04 04:00", tz = TZ), by = "15 min"),
          as.POSIXct(c("2026-11-04 12:00", "2026-11-05 12:00", "2026-11-07 12:00", "2026-11-10 12:00"), tz = TZ))
res = map_dfr(sort(unique(final$contest)), \(r) run_contest(r, times)) |>
  mutate(covers = truth_twop >= lo_twop & truth_twop <= hi_twop,
         confident_wrong = (truth_twop > 0.5 & hi_twop < 0.5) | (truth_twop < 0.5 & lo_twop > 0.5))

out = file.path(DIR_OUT, sprintf("sim_%s_results.csv", SCENARIO))
write_csv(res, out)
cat("\nSummary (two-party Dem share):\n")
res |>
  summarize(checkpoints = n(), interval_holds_truth = sum(covers), confident_wrong = sum(confident_wrong),
            truth = round(100 * first(truth_twop), 2), .by = race) |>
  print()
cat("\nWrote", out, "\n")
