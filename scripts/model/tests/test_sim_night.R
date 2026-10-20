# ========================================
## Run the model on one of Ethan's simulated nights
##
## Reads 26_sim/<scenario>/TX_truth.parquet and TX_schedule.parquet (from
## scripts/sim/make_sim_tx.R) plus our model inputs, and refits the model every 15
## minutes from 8pm to 4am ET, then at four later times, for every race in the sim.
##
## Two models (choose with --model=):
##   trial (default)  the full model in scripts/model/tx_gov_trial.R: precinct x mode cells,
##                    turnout / third-party / two-party equations, county random effect,
##                    cross-mode correlation (Sigma), mode partisan offsets, and its
##                    anytime-valid confidence sequence. That file's own code runs (config,
##                    functions, precinct frame, mode priors, cells); only its two input
##                    files are swapped for TX_precinct / TX_precinct_mode_history.
##   core             the simpler model_core.R version this test used before (by mode,
##                    no cross-mode correlation, no mode offsets), kept for comparison.
##
## The trial model's own switches still work as environment variables, e.g.
##   MODE_OFFSET_SOURCE=nc  borrow NC's mode partisan offsets (default here: zero, since
##                          the TX mode history file has turnout by mode but not party)
##     + MODE_OFFSETS_TX24=1  use the TX 2024 offsets measured from the 2024 scrape
##                          (CONTRACT.md section 7) instead of NC's
##   SIGMA_STRUCT=diag   turn off cross-mode correlation (default: full)
##   USE_COUNTY_RE=0     turn off the county random effect
## Settings used are printed at the start and saved with the results.
##
## How a simulated snapshot becomes the trial model's input (--cells=):
##   any (default)  a precinct x mode cell counts as reported once it has ANY votes, with
##                  whatever has been counted so far, as on the live feed. Its vote
##                  SHARES are used at once; its TURNOUT only once it is complete.
##   complete       a cell counts only once that mode is fully counted (cum_frac = 1).
##                  In Ethan's sim mail finishes days later, so mail has no reported cells
##                  on election night -- a stress test of a mode with no data.
##
## When a cell counts as complete (--complete=), which the trial needs for turnout:
##   feed (default)  that mode is fully counted in that precinct -- what the feed's
##                   pct_reported per precinct x mode would say
##   stopped         guessed from the snapshots alone: the cell has votes and has not
##                   grown in 30 minutes (how the TX backtest does it). Fooled by mail,
##                   whose last 5-25% arrives days later.
##
## Output: 26_general/model_tests/sim_<scenario>_results.csv, one row per race x time.
## With --label=<name> (for a run with non-default model settings), the output is
## sim_<scenario>__<name>_results.csv, so it sits beside the default run instead of
## replacing it; the page shows each as its own row.
## Run from the cbs-enr folder:
##   Rscript scripts/model/tests/test_sim_night.R [scenario] [--model=trial|core]
##       [--cells=any|complete] [--complete=feed|stopped] [--label=name]
# ========================================
source("scripts/model/tests/model_test_common.R")
args = commandArgs(trailingOnly = TRUE)
MODEL = sub("^--model=", "", c(grep("^--model=", args, value = TRUE), "--model=trial")[1])
CELLS = sub("^--cells=", "", c(grep("^--cells=", args, value = TRUE), "--cells=any")[1])
stopifnot("--cells must be any or complete" = CELLS %in% c("any", "complete"))
COMPLETE = sub("^--complete=", "", c(grep("^--complete=", args, value = TRUE), "--complete=feed")[1])
stopifnot("--complete must be feed or stopped" = COMPLETE %in% c("feed", "stopped"))
SCENARIO = c(grep("^--", args, value = TRUE, invert = TRUE), "tx_base")[1]
stopifnot("--model must be trial or core" = MODEL %in% c("trial", "core"))
LABEL = sub("^--label=", "", c(grep("^--label=", args, value = TRUE), "--label=")[1])
stopifnot("--label may use only letters, digits, - and _" = grepl("^[A-Za-z0-9_-]*$", LABEL))
TZ = "America/New_York"

dir_sc = file.path(DIR_SIM, SCENARIO)
truth = read_pq(file.path(dir_sc, "TX_truth.parquet")) |> mutate(across(c(geo, county_cbs), as.character))
sched = read_pq(file.path(dir_sc, "TX_schedule.parquet")) |> mutate(geo = as.character(geo), t = lubridate::with_tz(t, TZ))
inp = read_pq(file.path(DIR_26, "input_data", "model_inputs", "TX_precinct.parquet"))
mh = read_pq(file.path(DIR_26, "input_data", "model_inputs", "TX_precinct_mode_history.parquet")) |>
  filter(election == "2022_general")
cat(sprintf("Scenario %s: %d precincts, %d censored. Model: %s%s\n",
            SCENARIO, n_distinct(truth$geo), n_distinct(truth$geo[truth$censored]), MODEL,
            if (MODEL == "trial") paste0(", cells reported when ", CELLS, ", complete from ", COMPLETE) else ""))

final = truth |>
  mutate(party = if_else(party %in% c("dem", "rep"), party, "oth")) |>
  summarize(votes = sum(votes), .by = c(contest, geo, method, party)) |>
  pivot_wider(names_from = party, values_from = votes, values_fill = 0)
censored = distinct(truth, geo, censored)

# share of each cell counted at `time` (0 if nothing has landed; censored never report)
frac_at = function(time) {
  landed = filter(sched, !is.na(t), !is.na(cum_frac), t <= time)
  if (nrow(landed) == 0) return(tibble(geo = character(), method = character(), frac = numeric()))
  summarize(landed, frac = max(cum_frac), .by = c(geo, method))
}
counted_at = function(fin, time) {
  fin |>
    left_join(frac_at(time), by = c("geo", "method")) |>
    left_join(censored, by = "geo") |>
    mutate(frac = if_else(coalesce(censored, FALSE), 0, coalesce(frac, 0)),
           c_dem = round(dem * frac), c_rep = round(rep * frac), c_oth = round(oth * frac),
           c_tot = c_dem + c_rep + c_oth)
}
times = c(seq(as.POSIXct("2026-11-03 20:00", tz = TZ), as.POSIXct("2026-11-04 04:00", tz = TZ), by = "15 min"),
          as.POSIXct(c("2026-11-04 12:00", "2026-11-05 12:00", "2026-11-07 12:00", "2026-11-10 12:00"), tz = TZ))

# one results row from draws of the two-party share, plus what has been counted
summarize_draws = function(race, tm, d, S2, turn, extra = list()) {
  ci = cred(S2); ct = cred(turn)
  tot_m = function(m) sum((d$dem + d$rep + d$oth)[d$method == m])
  tibble(race = race, time = format(tm, "%Y-%m-%d %H:%M"),
         pct_votes = sum(d$c_tot) / sum(d$dem + d$rep + d$oth),
         pct_early = sum(d$c_tot[d$method == "early"]) / tot_m("early"),
         pct_eday = sum(d$c_tot[d$method == "eday"]) / tot_m("eday"),
         pct_mail = sum(d$c_tot[d$method == "mail"]) / tot_m("mail"),
         raw_twop = sum(d$c_dem) / max(sum(d$c_dem + d$c_rep), 1),
         est_twop = mean(S2), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]], p_dem_win = mean(S2 > 0.5),
         est_turnout = mean(turn), lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]],
         truth_twop = sum(d$dem) / (sum(d$dem) + sum(d$rep)), truth_turnout = sum(d$dem + d$rep + d$oth),
         !!!extra)
}

# ======================================================================
## A. the full trial model (tx_gov_trial.R)
# ======================================================================
if (MODEL == "trial") {
  # The trial's mode offsets come from mode x PARTY history, which TX does not have
  # (TX_precinct_mode_history has voters by mode only). Unless told otherwise, keep the
  # derived turnout split and set the offsets to zero; MODE_OFFSET_SOURCE=nc borrows NC's.
  if (!nzchar(Sys.getenv("MODE_OFFSET_SOURCE"))) Sys.setenv(MODE_OFFSET_SOURCE = "zero")

  # ---- the trial's input frame (`covs_long`), built from TX_precinct.parquet ----
  # tx_gov_trial.R reads a frame long by party (est = expected votes for that party,
  # method "all"). Column mapping from our model inputs:
  #   county       <- county_name              est (dem/rep) <- est_turnout x dem2 / (1 - dem2)
  #   turnout22/24 <- L2 turnout22/24 (rates)  age           <- p_age_65_up (share 65+)
  #   male         <- p_male                   pred.bla/his  <- yg_pred_bla / yg_pred_his
  #   tp_raw       <- oth_base (third-party share; the trial clamps it to [0, 0.25])
  covs_wide = inp |>
    transmute(geo, county = county_name, geostratum, stratum_demo, n, dem2,
              turnout22, turnout24, age = p_age_65_up, male = p_male,
              `pred.bla` = yg_pred_bla, `pred.his` = yg_pred_his, tp_raw = oth_base,
              est_turnout)
  bad = names(which(colSums(is.na(select(covs_wide, -geo))) > 0))
  if (length(bad)) stop("TX_precinct.parquet has blank values in: ", paste(bad, collapse = ", "),
                        ". Rerun prep_tx_model_inputs.R.")
  covs_long = bind_rows(
    mutate(covs_wide, party = "dem", est = est_turnout * dem2),
    mutate(covs_wide, party = "rep", est = est_turnout * (1 - dem2))) |>
    select(-est_turnout) |>
    mutate(method = "all")
  # mode history in the trial's schema (geo, method, party, votes). Only the turnout
  # split is real; the party split just follows dem2 and carries no mode lean.
  mode_hist = mh |>
    inner_join(select(covs_wide, geo, dem2), by = "geo") |>
    reframe(party = c("dem", "rep"), votes = voters * c(dem2, 1 - dem2), .by = c(geo, method))

  # ---- run tx_gov_trial.R itself, in order, up to its synthetic-returns section ----
  # Every top-level statement runs (config, functions, `prec`, mode priors, `cells`,
  # CTY_MAT, ...) EXCEPT its two file reads, which are replaced by the frames above.
  # So a change Young-Kyung makes to the model is picked up here with no edits.
  path_trial = "scripts/model/tx_gov_trial.R"
  src = readLines(path_trial, warn = FALSE)
  cut_ret = grep("^# ---- 3\\. synthetic returns", src)[1]
  cut_run = grep("^# ---- 6\\. run", src)[1]
  if (is.na(cut_ret) || is.na(cut_run)) stop("tx_gov_trial.R section markers changed; update test_sim_night.R")
  ex = parse(path_trial, keep.source = TRUE); sr = attr(ex, "srcref")
  lhs = function(e) if (is.call(e) && as.character(e[[1]]) %in% c("=", "<-") && is.name(e[[2]])) as.character(e[[2]]) else ""
  is_fun = function(e) lhs(e) != "" && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
  skipped = character()
  for (i in seq_along(ex)) {
    line = sr[[i]][1]
    if (lhs(ex[[i]]) %in% c("covs_long", "mode_hist")) { skipped = c(skipped, lhs(ex[[i]])); next }
    # sections 1-2 run in full; sections 3-5 contribute only their functions
    if (line < cut_ret || (line < cut_run && is_fun(ex[[i]]))) eval(ex[[i]], envir = globalenv())
    # MODE_OFFSETS_TX24=1 (with MODE_OFFSET_SOURCE=nc): use the TX 2024 offsets measured
    # from the real 2024 scrape (CONTRACT.md section 7) in place of NC's. The trial has no
    # source option for them yet, so they are swapped into its NC_DELTA right after it is
    # defined, before the mode priors are built from it.
    if (lhs(ex[[i]]) == "NC_DELTA" && Sys.getenv("MODE_OFFSETS_TX24") == "1") {
      assign("NC_DELTA", rbind(mail  = c(a = 0.769, b = -0.088),
                               early = c(a = 0.087, b = -0.039),
                               eday  = c(a = 0.082, b = -0.288)), envir = globalenv())
      cat("Mode offsets: TX 2024 measured values (CONTRACT.md section 7) in place of NC's\n")
    }
  }
  if (!setequal(skipped, c("covs_long", "mode_hist")))
    stop("tx_gov_trial.R no longer assigns covs_long / mode_hist at top level; update test_sim_night.R")
  cat(sprintf(paste0("Trial settings: DRAWS %d | bounded turnout %s (KAPPA %.2f, ref %s) | Sigma %s/%s | ",
                     "county RE %s | mode offsets %s | gates %d cells / %d counties\n"),
              DRAWS, USE_COMPOSE, KAPPA, REF_MODE, SIGMA_SOURCE, SIGMA_STRUCT, USE_COUNTY_RE,
              MODE_OFFSET_SOURCE, MIN_MODE_CELLS, MIN_MODE_CTY))
  d0 = prior_draws(cells)          # reference prior for the confidence sequence
  # rough statewide estimate (see statewide_tx.R); NULL if its inputs are missing
  source("scripts/model/tests/statewide_tx.R")
  ST = statewide_setup(inp)
  # this run's two-party Dem share and votes per precinct, in ST$p32 order
  prec_two_party = function(f) {
    i = match(ST$p32$geo, f$prec$geo)
    tp = prec$tp_prior[i]
    s2 = f$prec$dem_est[i] / pmax(1 - tp, 0.5)          # Dem share of all votes -> two-party
    w = f$prec$turn_est[i]
    list(s2 = if_else(is.na(s2), plogis(ST$p32$lb), s2), w = coalesce(w, 0))
  }
  st_cols = function(f) {
    if (is.null(ST)) return(list())
    pt = prec_two_party(f)
    z = statewide_draws(ST, pt$s2, pt$w, f$S_2p, f$turn)
    q = function(x) unname(quantile(x, c(0.025, 0.975)))
    list(st_est = mean(z$dem), st_lo = q(z$dem)[1], st_hi = q(z$dem)[2],
         stu_est = mean(z$uni), stu_lo = q(z$uni)[1], stu_hi = q(z$uni)[2])
  }

  run_contest = function(contest_id) {
    fin = filter(final, contest == contest_id, geo %in% prec$geo)
    # statewide if the 32 counties' final precinct results were known exactly
    st_exact = list()
    if (!is.null(ST)) {
      tr = fin |> summarize(dem = sum(dem), rep = sum(rep), tot = sum(dem + rep + oth), .by = geo)
      i = match(ST$p32$geo, tr$geo)
      s2 = tr$dem[i] / pmax(tr$dem[i] + tr$rep[i], 1)
      ok = !is.na(i) & tr$tot[i] > 0
      S2t = sum(tr$dem) / sum(tr$dem + tr$rep)
      z = statewide_draws(ST, if_else(ok, s2, plogis(ST$p32$lb)), if_else(ok, tr$tot[i], 0), rep(S2t, 2), rep(sum(tr$tot), 2), noise = FALSE)
      st_exact = list(st_exact = z$dem[1], stu_exact = z$uni[1])
    }
    map_dfr(times, function(tm) {
      d = counted_at(fin, tm)
      # A cell is COMPLETE when that mode is fully counted (what a feed's pct_reported
      # per precinct x mode would say). --complete=stopped instead guesses it the way
      # the TX backtest does: the cell has votes and has not grown in 30 minutes.
      done = if (COMPLETE == "feed") d$frac >= 1 else {
        prev = counted_at(fin, tm - 30 * 60)
        d$c_tot > 0 & d$c_tot == prev$c_tot[match(paste(d$geo, d$method), paste(prev$geo, prev$method))]
      }
      rep_cell = if (CELLS == "complete") done else d$c_tot > 0
      returns = d |>
        mutate(rep_cell = rep_cell, done = done) |>
        transmute(geo, mode = method,
                  stage = case_when(rep_cell & done ~ 6L, rep_cell ~ 4L, TRUE ~ 0L),
                  complete = rep_cell & done,
                  votes_dem = if_else(rep_cell, c_dem, NA_real_),
                  votes_rep = if_else(rep_cell, c_rep, NA_real_),
                  votes_oth = if_else(rep_cell, c_oth, NA_real_))
      f = suppressMessages(fit_stage(cells, returns, cross_mode = USE_CROSS_MODE))
      cs = conf_seq(d0, f$S_dem, level = LEVEL)
      ci_all = cred(f$S_dem)
      summarize_draws(contest_id, tm, d, f$S_2p, f$turn, c(list(
        obs_cells = f$n_obs_cells, complete_cells = sum(returns$complete),
        est_dem_all = mean(f$S_dem), lo_dem_all = ci_all[["lower"]], hi_dem_all = ci_all[["upper"]],
        cs_lo_dem_all = cs[["lower"]], cs_hi_dem_all = cs[["upper"]],
        truth_dem_all = sum(d$dem) / sum(d$dem + d$rep + d$oth)), st_cols(f), st_exact))
    })
  }
}

# ======================================================================
## B. the simpler model_core version (for comparison)
# ======================================================================
if (MODEL == "core") {
  set.seed(2026)
  DRAWS = 300
  METHODS = c("early", "eday", "mail")
  COVS = c("log_n", "turnout18", "turnout20", "p_male", "p_age_65_up", "p_black", "p_hisp", "p_col")
  share = mh |> mutate(sh = voters / sum(voters), .by = geo) |> select(geo, method, sh) |> filter(is.finite(sh))
  cells_core = inp |>
    select(geo, county_name, n, est_turnout, dem2, oth_base, all_of(setdiff(COVS, "log_n"))) |>
    cross_join(tibble(method = METHODS)) |>
    left_join(share, by = c("geo", "method")) |>
    mutate(sh = coalesce(sh, sum(sh * n, na.rm = TRUE) / sum(n[!is.na(sh)])), .by = c(county_name, method)) |>
    mutate(sh = sh / sum(sh), .by = geo) |>
    mutate(log_n = log(pmax(n, 1)), across(all_of(setdiff(COVS, "log_n")), ~ as.numeric(scale(.x))))

  run_contest = function(contest_id) {
    fin = filter(final, contest == contest_id)
    base = cells_core |> left_join(fin |> select(-contest), by = c("geo", "method")) |> mutate(across(c(dem, rep, oth), ~ coalesce(.x, 0)))
    map_dfr(times, function(tm) {
      d = counted_at(select(base, geo, method, dem, rep, oth), tm) |>
        left_join(select(base, -dem, -rep, -oth), by = c("geo", "method"))
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
        draw = function(f, prior) if (is.null(f)) {
          matrix(rnorm(DRAWS, 0, 0.3), DRAWS, N) + matrix(prior, DRAWS, N, byrow = TRUE) + matrix(rnorm(DRAWS * N, 0, 0.3), DRAWS, N)
        } else {
          draw_beta_re(f, DRAWS) %*% t(A) + matrix(rnorm(DRAWS * N, 0, sig_of(f)), DRAWS, N)
        }
        tau = exp(draw(ft, ypt)) - 0.1; tau[tau < 0] = 0
        s = invlogit(draw(fs, yps)); tp = invlogit(draw(fo, ypo))
        cnt = matrix(dm$c_tot, DRAWS, N, byrow = TRUE)
        tau = pmin(pmax(tau, cnt), matrix(1.25 * dm$n, DRAWS, N, byrow = TRUE) + cnt)
        donem = matrix(dm$frac >= 1, DRAWS, N, byrow = TRUE); tau[donem] = cnt[donem]
        rem = tau - cnt
        sims[, "dem"] = sims[, "dem"] + rowSums(matrix(dm$c_dem, DRAWS, N, byrow = TRUE) + rem * (1 - tp) * s)
        sims[, "rep"] = sims[, "rep"] + rowSums(matrix(dm$c_rep, DRAWS, N, byrow = TRUE) + rem * (1 - tp) * (1 - s))
        sims[, "tot"] = sims[, "tot"] + rowSums(tau)
      }
      summarize_draws(contest_id, tm, d, sims[, "dem"] / (sims[, "dem"] + sims[, "rep"]), sims[, "tot"])
    })
  }
}

# ======================================================================
## run every race in the sim
# ======================================================================
t0 = Sys.time()
res = map_dfr(sort(unique(final$contest)), function(r) { cat("  fitting", r, "...\n"); run_contest(r) }) |>
  mutate(model = MODEL,
         covers = truth_twop >= lo_twop & truth_twop <= hi_twop,
         confident_wrong = (truth_twop > 0.5 & hi_twop < 0.5) | (truth_twop < 0.5 & lo_twop > 0.5))
# what this run changes from the default model settings, in words, for the page
changes = c(if (MODEL == "core") "simpler model_core version",
            if (CELLS == "complete") "cells used only once fully counted",
            if (COMPLETE == "stopped") "fully-counted guessed from counts (no growth in 30 min)")
if (MODEL == "trial") changes = c(changes,
  if (MODE_OFFSET_SOURCE != "zero") paste0("mode offsets: ",
    if (MODE_OFFSET_SOURCE == "nc" && Sys.getenv("MODE_OFFSETS_TX24") == "1") "TX 2024 (measured, CONTRACT.md section 7)"
    else if (MODE_OFFSET_SOURCE == "nc") "NC 2024" else MODE_OFFSET_SOURCE),
  if (SIGMA_STRUCT != "full") paste0("cross-mode correlation: ", SIGMA_STRUCT),
  if (!USE_COUNTY_RE) "no county random effect",
  if (!USE_COMPOSE) "unbounded turnout")
res = mutate(res, scenario = SCENARIO, label = LABEL,
             model_changes = if (length(changes)) paste(changes, collapse = "; ") else "")
if (MODEL == "trial") {
  res = mutate(res, settings = sprintf("cells %s; complete %s; bounded turnout %s (KAPPA %.2f); Sigma %s/%s; county RE %s; mode offsets %s; draws %d",
                                       CELLS, COMPLETE, USE_COMPOSE, KAPPA, SIGMA_SOURCE, SIGMA_STRUCT,
                                       USE_COUNTY_RE, MODE_OFFSET_SOURCE, DRAWS))
}

out = file.path(DIR_OUT, sprintf("sim_%s%s_results.csv", SCENARIO, if (nzchar(LABEL)) paste0("__", LABEL) else ""))
write_csv(res, out)
cat(sprintf("\nSummary (two-party Dem share), %s model%s, %.1f minutes:\n", MODEL,
            if (length(changes)) paste0(" [", paste(changes, collapse = "; "), "]") else "", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
res |>
  summarize(checkpoints = n(), interval_holds_truth = sum(covers), confident_wrong = sum(confident_wrong),
            truth = round(100 * first(truth_twop), 2), .by = race) |>
  print()
cat("\nWrote", out, "\n")
