# ========================================
## counties.R -- statewide predictions: the models' scraped counties + CBS county results
## for every other county.
##
## Our models cover only the counties we scrape precinct by precinct (TX: 32). For the
## rest of the state we use CBS's county results (scripts/model/live/R/cbs_county_pull.R writes
## them to 26_general/cbs_api/{env}/{ST}_{contest}[_{district}]/). Scraped counties keep
## the model's precinct-level prediction; CBS is used ONLY for the others.
##
## For each other county c, at each run:
##   counted     d_c, r_c, o_c from the latest CBS snapshot; t_c = d_c + r_c + o_c;
##               p_c = CBS's share of expected vote counted (pct_exp_vote / 100)
##   expected    E_c = (2022 governor votes in c) x tonight's turnout ratio, where the
##               ratio = the model's predicted turnout in the scraped counties / their 2022
##               governor votes
##   final total T_c = p_c x (t_c / p_c) + (1 - p_c) x E_c = t_c + (1 - p_c) x E_c
##               (CBS's own implied total, trusted more as more is counted);
##               still to count V_c = (1 - p_c) x T_c
##   remaining   third-party share  = p_c x (o_c / t_c) + (1 - p_c) x (2024 third-party share)
##   shares      two-party Dem share, on the logit scale,
##                 = p_c x logit(counted Dem share) + (1 - p_c) x [logit(Harris 2024 share in c) + swing]
##               where swing = logit(model's two-party Dem share in the scraped counties)
##                             - logit(Harris 2024 share in the scraped counties)
##   final       Dem_c = d_c + V_c x (1 - third-party share) x Dem share, Rep_c likewise,
##               Oth_c = o_c + V_c x third-party share
## A county with nothing counted (p_c = 0) is all expectation: E_c votes at the Harris
## 2024 share moved by tonight's swing.
##
## Intervals (models with draws: model_simpleswing, model_medsl): for each of the model's draws,
## the swing and turnout ratio come from that draw (so the rest of the state moves with
## the scraped counties), plus
##   REST_COMMON_SD  the rest of the state swinging differently from the scraped counties
##                   (logit, one value per draw)
##   REST_COUNTY_SD  each county's own swing (logit, independent across counties; real TX
##                   between-county swing sd is about 2 points, ~0.08 logit)
##   REST_TURN_SD    each county's turnout vs expectation (log scale)
## all applied only to the part of a county not yet counted. These are set by judgment,
## not fitted; the calibration nights do not cover the rest of the state.
##
##   county_baseline("TX")                           2024 / 2022 county results, all counties
##   read_cbs_latest("TX", "gov", env = "live")      the latest CBS snapshot for a race
# ========================================

REST_COMMON_SD = 0.05
REST_COUNTY_SD = 0.08
REST_TURN_SD   = 0.10

# one row per county (5-digit FIPS): Harris 2024 two-party share, 2024 third-party share,
# 2022 governor and 2024 presidential votes. From votehub's precinct estimates.
county_baseline = function(state, dir_26 = find_26_general()) {
  f = file.path(dir_26, "input_data", "votehub-shapefiles", "estimated_past_results.csv")
  if (!file.exists(f)) stop("Missing ", f)
  x = readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE) |>
    filter(state_abbr == state)
  # votehub's file covers AK, FL, GA, IA, MI, NC, NE, OH, SC, TX (not CA or NY, as of 2026-10-22)
  if (!nrow(x)) stop("No county history for ", state, " in ", basename(f),
                     ": statewide rows for ", state, " need 2024 / 2022 county results from another source")
  x |>
    mutate(county_fips = stringr::str_extract(unit_id, "^[0-9]+"),
           across(c(pres24_dem, pres24_rep, pres24_oth, pres24_total, gov22_total), as.numeric)) |>
    summarize(across(c(pres24_dem, pres24_rep, pres24_oth, pres24_total, gov22_total), \(x) sum(x, na.rm = TRUE)),
              .by = county_fips) |>
    mutate(h24 = pres24_dem / pmax(pres24_dem + pres24_rep, 1),
           oth24 = pres24_oth / pmax(pres24_total, 1),
           exp_base = if_else(gov22_total > 0, gov22_total, pres24_total))
}

# the latest snapshot for a race written by cbs_county_pull.R, as of `at` (default: the
# newest). NULL if there is none yet.
read_cbs_latest = function(state, contest, district = NA, env = "live", at = NULL, dir_26 = find_26_general()) {
  tag = if (is.na(district)) paste0(state, "_", contest) else paste0(state, "_", contest, "_", district)
  d = file.path(dir_26, "cbs_api", env, tag)
  fs = list.files(d, pattern = paste0("^", tag, "_[0-9]{8}_[0-9]{6}\\.parquet$"), full.names = TRUE)
  if (!length(fs)) return(NULL)
  ts = as.POSIXct(stringr::str_extract(basename(fs), "[0-9]{8}_[0-9]{6}"), format = "%Y%m%d_%H%M%S", tz = "UTC")
  if (!is.null(at)) { keep = ts <= at; fs = fs[keep]; ts = ts[keep] }
  if (!length(fs)) return(NULL)
  x = read_pq(fs[which.max(ts)])
  # snapshots written before cbs_county_pull.R mapped CBS's party labels
  if (any(!x$party %in% c("dem", "rep", "oth")))
    x = mutate(x, party = case_when(tolower(party) %in% c("d", "dem") ~ "dem",
                                    tolower(party) %in% c("r", "rep") ~ "rep", TRUE ~ "oth"))
  x
}

# counties in `cells` (the scraped ones), as CBS 3-digit county numbers
scraped_counties = function(cells) unique(stringr::str_extract(cells$geo, "(?<=_)[0-9]{3}(?=_)"))

# the other counties' counted votes and baseline, one row per county
rest_of_state = function(cbs, base, cells) {
  cbs |>
    filter(!county_cbs %in% scraped_counties(cells)) |>
    summarize(d = sum(votes[party == "dem"]), r = sum(votes[party == "rep"]), o = sum(votes[party == "oth"]),
              p = pmin(pmax(first(county_pct_exp_vote) / 100, 0), 1),
              prec_in = first(county_precincts_in), prec_of = first(county_precincts_of),
              .by = c(county_fips, county_cbs)) |>
    left_join(select(base, county_fips, h24, oth24, exp_base), by = "county_fips") |>
    # a county missing from the history file: the state's average lean, and no expected
    # vote beyond what CBS has counted
    mutate(h24 = coalesce(h24, sum(base$pres24_dem) / sum(base$pres24_dem + base$pres24_rep)),
           oth24 = coalesce(oth24, sum(base$pres24_oth) / sum(base$pres24_total)), exp_base = coalesce(exp_base, 0))
}

# project the other counties for each of B draws. swing, turn_ratio: length-B vectors
# (one per draw; length 1 for a point estimate). noise: add the REST_* terms.
# Returns B x 3 matrix of statewide-rest Dem / Rep / Oth final votes.
project_rest = function(rest, swing, turn_ratio, noise = TRUE) {
  B = length(swing); n = nrow(rest)
  if (n == 0) return(matrix(0, B, 3, dimnames = list(NULL, c("dem", "rep", "oth"))))
  lg = function(x) qlogis(pmin(pmax(x, 1e-4), 1 - 1e-4))
  M = function(v) matrix(v, B, n, byrow = TRUE)
  t = rest$d + rest$r + rest$o
  p = M(if_else(t > 0, rest$p, 0))
  common = if (noise) rnorm(B, 0, REST_COMMON_SD) else rep(0, B)
  e_cty = if (noise) matrix(rnorm(B * n, 0, REST_COUNTY_SD), B, n) else 0
  e_trn = if (noise) matrix(rnorm(B * n, 0, REST_TURN_SD), B, n) else 0
  E = M(rest$exp_base) * turn_ratio * exp(e_trn)
  Tt = M(t)
  # blended final total: CBS's implied total t / p with weight p, expectation E with
  # weight 1 - p  (p x t / p = t); the uncounted part is (1 - p) of it
  V = (1 - p) * (Tt + (1 - p) * E)
  o_rem = p * M(if_else(t > 0, rest$o / pmax(t, 1), rest$oth24)) + (1 - p) * M(rest$oth24)
  s_obs = M(if_else(rest$d + rest$r > 0, rest$d / pmax(rest$d + rest$r, 1), rest$h24))
  s_pri = M(lg(rest$h24)) + swing + common + e_cty
  w = p * M(as.numeric(rest$d + rest$r > 0))
  s_rem = plogis(w * lg(s_obs) + (1 - w) * s_pri)
  cbind(dem = rowSums(M(rest$d) + V * (1 - o_rem) * s_rem),
        rep = rowSums(M(rest$r) + V * (1 - o_rem) * (1 - s_rem)),
        oth = rowSums(M(rest$o) + V * o_rem))
}

# Statewide rows (geo = "all") for one race x model, in the same format as to_pred_data().
# fit: run_race() result; cbs: latest CBS snapshot for the race; base: county_baseline().
state_pred_data = function(fit, model, cells, returns, race, time, cbs, base, level = 0.95) {
  if (!identical(fit$status, "ok") || is.null(cbs) || !nrow(cbs)) return(NULL)
  rest = rest_of_state(cbs, base, cells)
  sc = scraped_counties(cells)
  b32 = cbs |> distinct(county_fips, county_cbs) |> filter(county_cbs %in% sc) |>
    left_join(base, by = "county_fips")
  h32 = sum(b32$pres24_dem) / max(sum(b32$pres24_dem + b32$pres24_rep), 1)
  e32 = sum(b32$exp_base)
  # tonight's turnout ratio and swing are measured on the scraped counties' history, found
  # through the CBS rows for those counties (CBS's feed lists every county)
  if (!nrow(b32) || !(e32 > 0))
    stop("The CBS snapshot has no rows for the scraped counties, so tonight's swing and turnout can't be carried to the rest of the state")
  lg = function(x) qlogis(pmin(pmax(x, 1e-4), 1 - 1e-4))

  cand = reported_by_candidate(returns)
  if (model %in% c("swing", "medsl")) {
    dr = fit$draws
    D32 = dr$dem; R32 = dr$rep; T32 = dr$turnout
    pt = fit_point(fit)
    conf = sprintf(if (model == "swing") "county_bootstrap_%g_plus_rest" else "posterior_%g_plus_rest", 100 * level)
  } else if (model == "cory") {
    pr = if (is.data.frame(fit$raw)) fit$raw else fit$raw$pred
    pr = mutate(pr, party = as.character(party))
    pt = c(dem = sum(pr$pred[pr$party == "dem"]), rep = sum(pr$pred[pr$party == "rep"]), tot = sum(pr$pred))
    D32 = pt[["dem"]]; R32 = pt[["rep"]]; T32 = pt[["tot"]]
    conf = NA_character_
  } else stop("state_pred_data() has no rule for model '", model, "' yet")

  # rest of the state: point (no noise, from the point fit) and per-draw
  rest_pt = project_rest(rest, lg(pt[["dem"]] / (pt[["dem"]] + pt[["rep"]])) - lg(h32),
                         pt[["tot"]] / max(e32, 1), noise = FALSE)
  has_draws = length(D32) > 1
  rest_dr = if (has_draws) project_rest(rest, lg(D32 / (D32 + R32)) - lg(h32), T32 / max(e32, 1)) else NULL

  # by party: point, and draws if any
  P = c(dem = pt[["dem"]] + rest_pt[1, "dem"], rep = pt[["rep"]] + rest_pt[1, "rep"],
        oth = pt[["tot"]] - pt[["dem"]] - pt[["rep"]] + rest_pt[1, "oth"])
  names(P) = c("dem", "rep", "oth")
  Dd = if (has_draws) list(dem = D32 + rest_dr[, "dem"], rep = R32 + rest_dr[, "rep"],
                           oth = (T32 - D32 - R32) + rest_dr[, "oth"]) else NULL
  tot_pt = sum(P); tot_d = if (has_draws) Dd$dem + Dd$rep + Dd$oth else NULL

  # reported: scraped counties from our returns, the rest from CBS
  rep_rest = c(dem = sum(rest$d), rep = sum(rest$r), oth = sum(rest$o))
  oth_c = cand$party %in% c("oth", "ind")
  w = if (any(oth_c)) {
    ww = if (sum(cand$rep[oth_c]) > 0) cand$rep / sum(cand$rep[oth_c]) else rep(1 / sum(oth_c), nrow(cand))
    if_else(oth_c, ww, NA_real_)
  } else rep(NA_real_, nrow(cand))
  q = function(x) unname(cred(x, level))
  rows = lapply(seq_len(nrow(cand)), function(i) {
    pty = if (oth_c[i]) "oth" else cand$party[i]
    k = if (oth_c[i]) w[i] else 1
    rp = cand$rep[i] + k * rep_rest[[pty]]
    point = k * P[[pty]]
    if (has_draws) {
      v = k * Dd[[pty]]; ci_v = q(v); ci_s = q(v / tot_d)
      pw = if (pty == "dem") mean(Dd$dem > Dd$rep) else if (pty == "rep") mean(Dd$rep > Dd$dem) else NA_real_
    } else { ci_v = ci_s = c(NA_real_, NA_real_); pw = NA_real_ }
    tibble(candidate = cand$candidate[i], party = cand$party[i], pred_type = c("vote", "share"),
           rep = c(rp, NA_real_), pred = c(point, point / tot_pt),
           pred_min = pmax(c(ci_v[1], ci_s[1]), 0), pred_max = c(ci_v[2], ci_s[2]), p_win = pw)
  })
  out = bind_rows(rows)
  # reported share = candidate's share of everything counted so far, statewide
  rep_tot = sum(out$rep[out$pred_type == "vote"])
  out$rep[out$pred_type == "share"] = out$rep[out$pred_type == "vote"] / max(rep_tot, 1)

  n_prec = n_distinct(cells$geo)
  pct = (pct_reporting(cells, returns) * n_prec + sum(rest$prec_in, na.rm = TRUE)) /
    (n_prec + sum(rest$prec_of, na.rm = TRUE))
  bind_cols(race[rep(1, nrow(out)), c("state", "election", "jurisdiction", "contest", "district")], out) |>
    mutate(time = time, model = unname(MODEL_NAMES[model]), model_conf = conf, pct_rep = pct, geo = "all",
           cbs_as_of = max(cbs$observed_at), election = as.Date(election), district = as.integer(district)) |>
    relocate(state, election, jurisdiction, contest, district, candidate, party, pred_type, time,
             model, model_conf, pct_rep, rep, pred, pred_min, pred_max, p_win, geo) |>
    arrange(pred_type, desc(pred))
}
