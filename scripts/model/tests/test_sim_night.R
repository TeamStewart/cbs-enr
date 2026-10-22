# ========================================
## Run the model on one of Ethan's simulated nights
##
## Reads 26_sim/<scenario>/TX_truth.parquet and TX_schedule.parquet (from
## scripts/sim/make_sim_tx.R) plus our model inputs, and refits the model every 15
## minutes from 8pm to 4am ET, then at four later times, for every race in the sim.
##
## Three models (choose with --model=):
##   trial (default)  the full model in scripts/model/model_turnout.R: precinct x mode cells,
##                    turnout / third-party / two-party equations, county random effect,
##                    cross-mode correlation (Sigma), mode partisan offsets, and its
##                    anytime-valid confidence sequence. That file's own code runs (config,
##                    functions, precinct frame, mode priors, cells); only its two input
##                    files are swapped for TX_precinct / TX_precinct_mode_history.
##   core             the simpler model_core.R version this test used before (by mode,
##                    no cross-mode correlation, no mode offsets), kept for comparison.
##   swing            the standard uniform swing model, fit with fixest::feols with county
##                    fixed effects: each reported precinct x method cell's swing in Dem
##                    two-party share from its Harris 2024 share, regressed on county and
##                    method fixed effects, weighted by expected votes. Unreported cells get
##                    their county's swing; counties with nothing in get the average swing.
##                    Turnout the same way (log ratio of counted to expected votes, county and
##                    method fixed effects, fully counted cells only). Interval: bootstrap
##                    over counties. Needs the fixest package.
##   cory             model_cory: Cory McCartan's model_legacy() from the enightmodels package,
##                    called as is with its default configuration (model_legacy_config():
##                    by method, strata = geostratum, stage_complete = 6). Its input is the
##                    standard election-data format (one row per precinct x method x
##                    candidate, with stage 0 / 4 / 6 as in Ethan's snapshots) plus `past`, the
##                    expected votes for that row. It gives point estimates only: no interval.
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
# "both" is the trial model with model_both.R's swing-style theta prior: same cells, same
# fit_stage, one extra step that re-centres theta's prior from a per-cell level fit.
IS_BOTH = MODEL == "both"
if (IS_BOTH) MODEL = "trial"
# --dump-prec=HH:MM[,HH:MM] writes PRECINCT-level predictions at those checkpoints to
# 26_general/model_tests/prec_<scenario>[__<label>]_<HHMM>.csv. Diagnostic only: an
# aggregate run yields ONE number (the statewide bias) while a dump yields ~6,100
# observations from the same fit, which is what lets a bias be attributed to WHICH
# precincts move rather than inferred from the spread of aggregates across seeds.
DUMP_PREC = sub("^--dump-prec=", "", c(grep("^--dump-prec=", args, value = TRUE), "--dump-prec=")[1])
DUMP_PREC = if (nzchar(DUMP_PREC)) strsplit(DUMP_PREC, ",")[[1]] else character(0)
CELLS = sub("^--cells=", "", c(grep("^--cells=", args, value = TRUE), "--cells=any")[1])
stopifnot("--cells must be any or complete" = CELLS %in% c("any", "complete"))
COMPLETE = sub("^--complete=", "", c(grep("^--complete=", args, value = TRUE), "--complete=feed")[1])
stopifnot("--complete must be feed or stopped" = COMPLETE %in% c("feed", "stopped"))
SCENARIO = c(grep("^--", args, value = TRUE, invert = TRUE), "tx_base")[1]
stopifnot("--model must be trial, core, swing or cory" = MODEL %in% c("trial", "core", "swing", "cory"))
LABEL = sub("^--label=", "", c(grep("^--label=", args, value = TRUE), "--label=")[1])
# calibration runs (run_calibration.R): a night outside 26_sim, fewer checkpoints, no
# statewide step, and an explicit output file
SIM_DIR = sub("^--sim_dir=", "", c(grep("^--sim_dir=", args, value = TRUE), "--sim_dir=")[1])
OUT_FILE = sub("^--out=", "", c(grep("^--out=", args, value = TRUE), "--out=")[1])
CALIB = "--checkpoints=calib" %in% args
NO_ST = "--no_statewide" %in% args
if (nzchar(SIM_DIR)) SCENARIO = basename(SIM_DIR)
stopifnot("--label may use only letters, digits, - and _" = grepl("^[A-Za-z0-9_-]*$", LABEL))
TZ = "America/New_York"

dir_sc = if (nzchar(SIM_DIR)) SIM_DIR else file.path(DIR_SIM, SCENARIO)
truth = read_pq(file.path(dir_sc, "TX_truth.parquet")) |> mutate(across(c(geo, county_cbs), as.character))
sched = read_pq(file.path(dir_sc, "TX_schedule.parquet")) |> mutate(geo = as.character(geo), t = lubridate::with_tz(t, TZ))
inp = read_pq(file.path(DIR_26, "input_data", "model_inputs", "TX_precinct.parquet"))
mh = read_pq(file.path(DIR_26, "input_data", "model_inputs", "TX_precinct_mode_history.parquet")) |>
  filter(election == "2022_general")
# expected votes per precinct x method (used by model_simpleswing and model_cory as the
# baseline): est_turnout x the precinct's 2022 method split, shrunk toward its county's
# split (k = 40, as in the trial model's mode priors)
METHODS = c("early", "eday", "mail")
expected_cells = function() {
  sh = mh |> inner_join(select(inp, geo, county = county_name), by = "geo") |> rename(v = voters)
  cty_sh = sh |> summarize(v = sum(v), .by = c(county, method)) |> mutate(cs = v / sum(v), .by = county) |>
    select(county, method, cs)
  inp |>
    transmute(geo, county = county_name, geostratum, dem2, oth = pmin(pmax(oth_base, 0), 0.25), est_turnout) |>
    cross_join(tibble(method = METHODS)) |>
    left_join(sh |> mutate(n_p = sum(v), .by = geo) |> select(geo, method, v, n_p), by = c("geo", "method")) |>
    left_join(cty_sh, by = c("county", "method")) |>
    mutate(v = coalesce(v, 0), n_p = coalesce(n_p, 0), cs = coalesce(cs, 1 / 3),
           share = (v + 40 * cs) / (n_p + 40)) |>
    mutate(share = share / sum(share), .by = geo) |>
    mutate(exp_votes = pmax(est_turnout * share, 0.5)) |>
    select(geo, county, geostratum, method, dem2, oth, exp_votes)
}
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
if (CALIB) times = as.POSIXct(c("2026-11-03 20:30", "2026-11-03 21:00", "2026-11-03 21:30", "2026-11-03 22:30",
                                 "2026-11-04 00:00", "2026-11-04 04:00", "2026-11-10 12:00"), tz = TZ)

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
## A. the full trial model (model_turnout.R)
# ======================================================================
if (MODEL == "trial") {
  # The trial's mode offsets come from mode x PARTY history, which TX does not have
  # (TX_precinct_mode_history has voters by mode only). Unless told otherwise, keep the
  # derived turnout split and set the offsets to zero; MODE_OFFSET_SOURCE=nc borrows NC's.
  if (!nzchar(Sys.getenv("MODE_OFFSET_SOURCE"))) Sys.setenv(MODE_OFFSET_SOURCE = "zero")

  # ---- the trial's input frame (`covs_long`), built from TX_precinct.parquet ----
  # model_turnout.R reads a frame long by party (est = expected votes for that party,
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

  # ---- run model_turnout.R itself, in order, up to its synthetic-returns section ----
  # Every top-level statement runs (config, functions, `prec`, mode priors, `cells`,
  # CTY_MAT, ...) EXCEPT its two file reads, which are replaced by the frames above.
  # So a change Young-Kyung makes to the model is picked up here with no edits.
  path_trial = "scripts/model/model_turnout.R"
  src = readLines(path_trial, warn = FALSE)
  cut_ret = grep("^# ---- 3\\. synthetic returns", src)[1]
  cut_run = grep("^# ---- 6\\. run", src)[1]
  if (is.na(cut_ret) || is.na(cut_run)) stop("model_turnout.R section markers changed; update test_sim_night.R")
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
    stop("model_turnout.R no longer assigns covs_long / mode_hist at top level; update test_sim_night.R")
  cat(sprintf(paste0("Trial settings: DRAWS %d | bounded turnout %s (KAPPA %.2f, ref %s) | Sigma %s/%s | ",
                     "county RE %s | mode offsets %s | gates %d cells / %d counties\n"),
              DRAWS, USE_COMPOSE, KAPPA, REF_MODE, SIGMA_SOURCE, SIGMA_STRUCT, USE_COUNTY_RE,
              MODE_OFFSET_SOURCE, MIN_MODE_CELLS, MIN_MODE_CTY))
  if (IS_BOTH) {
    source("scripts/model/model_both.R")   # defines fit_stage_swingtheta; does not re-source
    cli_alert_info("model=both: swing-style theta prior from model_both.R")
  }
  d0 = prior_draws(cells)          # reference prior for the confidence sequence
  # rough statewide estimate (see statewide_tx.R); NULL if its inputs are missing
  source("scripts/model/tests/statewide_tx.R")
  ST = if (NO_ST) NULL else statewide_setup(inp)
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
      f = suppressMessages(
        if (IS_BOTH) fit_stage_swingtheta(cells, returns, cross_mode = USE_CROSS_MODE)
        else         fit_stage(cells, returns, cross_mode = USE_CROSS_MODE))
      if (length(DUMP_PREC) && format(tm, "%H:%M") %in% DUMP_PREC) {
        # truth per precinct at this checkpoint, so the dump is self-contained
        tr_p = fin |> summarize(t_dem = sum(dem), t_rep = sum(rep),
                                t_tot = sum(dem + rep + oth), .by = geo)
        cn_p = d |> summarize(c_tot = sum(c_tot), n_rep_modes = sum(c_tot > 0),
                              n_done_modes = sum(done), .by = geo)
        dmp = f$prec |>
          left_join(tr_p, by = "geo") |>
          left_join(cn_p, by = "geo") |>
          left_join(select(inp, geo, n, dem2, est_turnout, geostratum, yg_pred_his), by = "geo") |>
          mutate(scenario = SCENARIO, label = LABEL, time = format(tm, "%H:%M"),
                 truth_dem2 = t_dem / pmax(t_dem + t_rep, 1))
        fp = file.path(DIR_OUT, sprintf("prec_%s%s_%s.csv", SCENARIO,
                       if (nzchar(LABEL)) paste0("__", LABEL) else "", format(tm, "%H%M")))
        readr::write_csv(dmp, fp)
        cat("  dumped", nrow(dmp), "precincts ->", basename(fp), "\n")
      }
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
## C. the standard uniform swing model (feols with county fixed effects)
# ======================================================================
if (MODEL == "swing") {
  if (!requireNamespace("fixest", quietly = TRUE))
    stop('The uniform swing model needs the fixest package. In R: install.packages("fixest")')
  set.seed(2026)
  BOOT = 100                                   # county bootstrap replicates for the interval
  base = expected_cells()
  source("scripts/model/tests/statewide_tx.R")
  ST = if (NO_ST) NULL else statewide_setup(inp)
  cat("Model: standard uniform swing model (feols, county fixed effects), interval from", BOOT, "county bootstrap replicates\n")

  # predictions from county + method fixed effects; cells whose county (or method) has no
  # data fall back to the weighted average for that method, then overall
  fe_predict = function(dd, yname, fit_rows, w) {
    fr = dd[fit_rows & w > 0, ]
    if (nrow(fr) < 2) return(rep(if (nrow(fr)) fr[[yname]][1] else 0, nrow(dd)))
    fit = tryCatch(fixest::feols(stats::as.formula(paste(yname, "~ 1 | county + method")), data = fr,
                                 weights = w[fit_rows & w > 0], notes = FALSE, warn = FALSE),
                   error = function(e) NULL)
    p = if (is.null(fit)) rep(NA_real_, nrow(dd)) else suppressWarnings(predict(fit, newdata = dd))
    by_m = tapply(fr[[yname]] * w[fit_rows & w > 0], fr$method, sum) / tapply(w[fit_rows & w > 0], fr$method, sum)
    all_m = sum(fr[[yname]] * w[fit_rows & w > 0]) / sum(w[fit_rows & w > 0])
    fb = unname(ifelse(dd$method %in% names(by_m), by_m[dd$method], all_m))
    if_else(is.na(p), fb, p)
  }

  swing_once = function(d, cw) {
    w = d$exp_votes * cw[d$county]                # county bootstrap weights (1 for the point fit)
    w[is.na(w)] = 0
    rep_cell = d$c_dem + d$c_rep > 0
    done = rep_cell & d$frac >= 1
    sw = fe_predict(d, "y", rep_cell, w)
    lr = if (any(done)) fe_predict(d, "lr", done, w) else rep(0, nrow(d))
    tot = if_else(done, d$c_tot, pmax(d$c_tot, d$exp_votes * exp(lr)))
    s = pmin(pmax(d$dem2 + sw, 0.001), 0.999)
    rem2 = (tot - d$c_tot) * (1 - d$oth)
    tibble(geo = d$geo, dem = if_else(done, d$c_dem, d$c_dem + rem2 * s),
           rep = if_else(done, d$c_rep, d$c_rep + rem2 * (1 - s)), tot = tot)
  }

  run_contest = function(contest_id) {
    fin = filter(final, contest == contest_id, geo %in% base$geo)
    cells0 = base |> left_join(select(fin, -contest), by = c("geo", "method")) |>
      mutate(across(c(dem, rep, oth.y), \(x) coalesce(x, 0))) |>
      rename(oth = oth.x, oth_v = oth.y)
    # statewide if the 32 counties' final precinct results were known exactly
    st_exact = list()
    if (!is.null(ST)) {
      tr = fin |> summarize(dem = sum(dem), rep = sum(rep), tot = sum(dem + rep + oth), .by = geo)
      i = match(ST$p32$geo, tr$geo)
      ok = !is.na(i) & tr$tot[i] > 0
      s2 = tr$dem[i] / pmax(tr$dem[i] + tr$rep[i], 1)
      z = statewide_draws(ST, if_else(ok, s2, plogis(ST$p32$lb)), if_else(ok, tr$tot[i], 0),
                          rep(sum(tr$dem) / sum(tr$dem + tr$rep), 2), rep(sum(tr$tot), 2), noise = FALSE)
      st_exact = list(st_exact = z$dem[1], stu_exact = z$uni[1])
    }
    # the uniform swing model has nothing to estimate before any votes are counted, so
    # checkpoints with no votes in are left out for this model
    map_dfr(times, function(tm) {
      d = counted_at(transmute(cells0, geo, method, dem, rep, oth = oth_v), tm) |>
        left_join(select(cells0, geo, method, county, dem2, exp_votes, oth_share = oth), by = c("geo", "method")) |>
        rename(oth_v = oth, oth = oth_share)
      if (!any(d$c_tot > 0)) return(NULL)
      d = d |>
        mutate(y = c_dem / pmax(c_dem + c_rep, 1) - dem2,
               lr = log(pmax(c_tot, 0.5) / exp_votes))
      ctys = unique(d$county)
      pt = swing_once(d, setNames(rep(1, length(ctys)), ctys))
      rep_ctys = unique(d$county[d$c_tot > 0])
      draws = lapply(seq_len(BOOT), function(b) {
        cw = setNames(rep(1, length(ctys)), ctys)
        if (length(rep_ctys) > 1) {
          k = table(factor(sample(rep_ctys, length(rep_ctys), replace = TRUE), levels = rep_ctys))
          cw[rep_ctys] = as.numeric(k)
        }
        o = swing_once(d, cw)
        c(s2 = sum(o$dem) / sum(o$dem + o$rep), turn = sum(o$tot))
      })
      if (length(DUMP_PREC) && format(tm, "%H:%M") %in% DUMP_PREC) {
        # swing's per-geo prediction, in the same shape the trial branch dumps, so the
        # two models can be compared at PRECINCT level (the aggregate cannot
        # discriminate on a scenario that preserves the statewide total by construction)
        pg = pt |> summarize(turn_est = sum(tot), dem_v = sum(dem), rep_v = sum(rep), .by = geo) |>
          mutate(dem_est = dem_v / pmax(turn_est, 1))
        tr_p = fin |> summarize(t_dem = sum(dem), t_rep = sum(rep),
                                t_tot = sum(dem + rep + oth), .by = geo)
        cn_p = d |> summarize(c_tot = sum(c_tot), .by = geo)
        dmp = pg |> left_join(tr_p, by = "geo") |> left_join(cn_p, by = "geo") |>
          left_join(select(inp, geo, n, dem2, est_turnout, yg_pred_his), by = "geo") |>
          mutate(scenario = SCENARIO, label = LABEL, time = format(tm, "%H:%M"))
        fp = file.path(DIR_OUT, sprintf("prec_%s%s_%s.csv", SCENARIO,
                       if (nzchar(LABEL)) paste0("__", LABEL) else "", format(tm, "%H%M")))
        readr::write_csv(dmp, fp)
        cat("  dumped", nrow(dmp), "precincts ->", basename(fp), "\n")
      }
      S2 = vapply(draws, `[[`, 0, "s2"); TT = vapply(draws, `[[`, 0, "turn")
      # centre the bootstrap draws on the point fit
      S2 = S2 - mean(S2) + sum(pt$dem) / sum(pt$dem + pt$rep); TT = TT - mean(TT) + sum(pt$tot)
      st = list()
      if (!is.null(ST)) {
        pg = pt |> summarize(dem = sum(dem), rep = sum(rep), tot = sum(tot), .by = geo)
        i = match(ST$p32$geo, pg$geo)
        s2 = pg$dem[i] / pmax(pg$dem[i] + pg$rep[i], 1)
        z = statewide_draws(ST, if_else(is.na(s2), plogis(ST$p32$lb), s2), coalesce(pg$tot[i], 0), S2, TT)
        q = function(x) unname(quantile(x, c(0.025, 0.975)))
        st = list(st_est = mean(z$dem), st_lo = q(z$dem)[1], st_hi = q(z$dem)[2],
                  stu_est = mean(z$uni), stu_lo = q(z$uni)[1], stu_hi = q(z$uni)[2])
      }
      dd = transmute(d, geo, method, dem, rep, oth = oth_v, c_dem, c_rep, c_oth, c_tot)
      summarize_draws(contest_id, tm, dd, S2, TT,
                      c(list(obs_cells = sum(d$c_tot > 0), complete_cells = sum(d$c_tot > 0 & d$frac >= 1)), st, st_exact))
    })
  }
}

# ======================================================================
## D. model_cory: enightmodels::model_legacy(), as in the package
# ======================================================================
if (MODEL == "cory") {
  suppressPackageStartupMessages(library(enightmodels))
  base = expected_cells()
  source("scripts/model/tests/statewide_tx.R")
  ST = if (NO_ST) NULL else statewide_setup(inp)
  cfg = model_legacy_config()
  cat("Model: enightmodels::model_legacy() with model_legacy_config() defaults:",
      paste(names(cfg), vapply(cfg, \(x) paste(x, collapse = ","), ""), sep = " = ", collapse = "; "), "\n")
  # third-party past votes: the minor-party candidate 85%, write-ins 15%, as in Ethan's sim
  cand_share = function(party, candidate, dem2, oth)
    case_when(party == "dem" ~ (1 - oth) * dem2, party == "rep" ~ (1 - oth) * (1 - dem2),
              candidate == "write-in" ~ oth * 0.15, TRUE ~ oth * 0.85)

  run_contest = function(contest_id) {
    # every precinct x method x candidate in the 32-county frame; precincts the feed drops
    # (censored) stay in as unreported (stage 0), as they would in a joined frame
    tr = truth |> filter(contest == contest_id, geo %in% base$geo)
    rows = tr |>
      select(state, election, contest, candidate, party, method, geo, votes_final = votes) |>
      left_join(base, by = c("geo", "method")) |>
      mutate(past = exp_votes * cand_share(party, candidate, dem2, oth))
    st_exact = list()
    if (!is.null(ST)) {
      fin = tr |> summarize(dem = sum(votes[party == "dem"]), rep = sum(votes[party == "rep"]), tot = sum(votes), .by = geo)
      i = match(ST$p32$geo, fin$geo); ok = !is.na(i) & fin$tot[i] > 0
      z = statewide_draws(ST, if_else(ok, fin$dem[i] / pmax(fin$dem[i] + fin$rep[i], 1), plogis(ST$p32$lb)),
                          if_else(ok, fin$tot[i], 0), rep(sum(fin$dem) / sum(fin$dem + fin$rep), 2), rep(sum(fin$tot), 2), noise = FALSE)
      st_exact = list(st_exact = z$dem[1], stu_exact = z$uni[1])
    }
    map_dfr(times, function(tm) {
      fr = frac_at(tm) |> right_join(distinct(rows, geo, method), by = c("geo", "method")) |>
        left_join(censored, by = "geo") |>
        mutate(frac = if_else(coalesce(censored, FALSE), 0, coalesce(frac, 0))) |>
        mutate(stage = case_when(all(frac >= 1) ~ 6L, any(frac > 0) ~ 4L, TRUE ~ 0L), .by = geo) |>
        select(geo, method, frac, stage)
      d = rows |> left_join(fr, by = c("geo", "method")) |>
        mutate(votes = round(votes_final * frac), time = tm, jurisdiction = "TX", district = NA_integer_,
               election = as.Date(election)) |>
        select(state, election, jurisdiction, contest, district, candidate, party, time, stage, method, geo,
               votes, geostratum, past, votes_final, frac)
      dd = select(d, -votes_final, -frac) |> enightmodels::coerce_elec_data()
      out = enightmodels::model_legacy(dd, past, cfg)
      # model_legacy() RETURNS the prediction tibble (COLS_PRED: ..., party, pred, ...).
      # `out$pred` is the pred COLUMN, so the old `pr = out$pred` then `pr$pred` errored
      # with "$ operator is invalid for atomic vectors".
      pr = out
      dem = sum(pr$pred[pr$party == "dem"]); repv = sum(pr$pred[pr$party == "rep"])
      S2 = dem / (dem + repv); TT = sum(pr$pred)
      # precinct-level predictions for the statewide step: the same two functions
      # model_legacy() calls internally
      st = list()
      if (!is.null(ST)) {
        d_tot = enightmodels::model_legacy_total(dd, past, cfg)
        d_cand = enightmodels::model_legacy_cand(dd, d_tot, past, cfg) |>
          mutate(geo = as.character(geo), party = as.character(party)) |>
          summarize(dem = sum(pred[party == "dem"]), rep = sum(pred[party == "rep"]), tot = sum(pred), .by = geo)
        i = match(ST$p32$geo, d_cand$geo)
        s2 = d_cand$dem[i] / pmax(d_cand$dem[i] + d_cand$rep[i], 1)
        z = statewide_draws(ST, if_else(is.na(s2), plogis(ST$p32$lb), s2), coalesce(d_cand$tot[i], 0),
                            rep(S2, 2), rep(TT, 2), noise = FALSE)
        st = list(st_est = z$dem[1], st_lo = NA_real_, st_hi = NA_real_,
                  stu_est = z$uni[1], stu_lo = NA_real_, stu_hi = NA_real_)
      }
      cells = d |> mutate(party3 = if_else(party %in% c("dem", "rep"), party, "oth")) |>
        summarize(v = sum(votes_final), c = sum(votes), .by = c(geo, method, party3)) |>
        tidyr::pivot_wider(names_from = party3, values_from = c(v, c), values_fill = 0) |>
        transmute(geo, method, dem = v_dem, rep = v_rep, oth = v_oth,
                  c_dem, c_rep, c_oth, c_tot = c_dem + c_rep + c_oth)
      summarize_draws(contest_id, tm, cells, rep(S2, 2), rep(TT, 2),
                      c(list(obs_cells = sum(cells$c_tot > 0)), st, st_exact)) |>
        mutate(lo_twop = NA_real_, hi_twop = NA_real_, p_dem_win = NA_real_,
               lo_turnout = NA_real_, hi_turnout = NA_real_)
    })
  }
}

# ======================================================================
## run every race in the sim
# ======================================================================
t0 = Sys.time()
res = map_dfr(sort(unique(final$contest)), function(r) { cat("  fitting", r, "...\n"); run_contest(r) }) |>
  mutate(model = if (IS_BOTH) "model_both" else
                 c(trial = "model_y", swing = "model_simpleswing", core = "model_core", cory = "model_cory")[[MODEL]],
         covers = truth_twop >= lo_twop & truth_twop <= hi_twop,
         confident_wrong = (truth_twop > 0.5 & hi_twop < 0.5) | (truth_twop < 0.5 & lo_twop > 0.5))
# what this run changes from the default model settings, in words, for the page
changes = c(if (MODEL == "core") "simpler model_core version",
            if (MODEL == "swing") "standard uniform swing model (feols with county fixed effects)",
            if (MODEL == "cory") "enightmodels::model_legacy() (Cory), default configuration; point estimates only",
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
if (MODEL == "cory") {
  res = mutate(res, settings = paste0("enightmodels::model_legacy() with model_legacy_config() defaults (",
                                      paste(names(cfg), vapply(cfg, \(x) paste(x, collapse = ","), ""), sep = " = ", collapse = "; "),
                                      "); past = expected votes per precinct x method x candidate; no interval"))
}
if (MODEL == "swing") {
  res = mutate(res, settings = sprintf("standard uniform swing model: feols(swing ~ 1 | county + method), weighted by expected votes; turnout feols(log ratio ~ 1 | county + method) on fully counted cells; cells %s, complete %s; interval from %d county bootstrap replicates",
                                       CELLS, COMPLETE, BOOT))
}
if (MODEL == "trial") {
  res = mutate(res, settings = sprintf("cells %s; complete %s; bounded turnout %s (KAPPA %.2f); Sigma %s/%s; county RE %s; mode offsets %s; draws %d",
                                       CELLS, COMPLETE, USE_COMPOSE, KAPPA, SIGMA_SOURCE, SIGMA_STRUCT,
                                       USE_COUNTY_RE, MODE_OFFSET_SOURCE, DRAWS))
}

out = if (nzchar(OUT_FILE)) OUT_FILE else
  file.path(DIR_OUT, sprintf("sim_%s%s_results.csv", SCENARIO, if (nzchar(LABEL)) paste0("__", LABEL) else ""))
write_csv(res, out)
cat(sprintf("\nSummary (two-party Dem share), %s model%s, %.1f minutes:\n", MODEL,
            if (length(changes)) paste0(" [", paste(changes, collapse = "; "), "]") else "", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
res |>
  summarize(checkpoints = n(), interval_holds_truth = if (all(is.na(covers))) NA_integer_ else sum(covers, na.rm = TRUE),
            confident_wrong = if (all(is.na(confident_wrong))) NA_integer_ else sum(confident_wrong, na.rm = TRUE),
            mean_abs_err_pts = round(100 * mean(abs(est_twop - truth_twop)[pct_votes > 0]), 2),
            truth = round(100 * first(truth_twop), 2), .by = race) |>
  print()
cat("\nWrote", out, "\n")
