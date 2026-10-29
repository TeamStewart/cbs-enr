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
## Intervals (fits with draws: swing, pois_bayes, lm_bayes, medsl_sur): for each of the model's draws,
## the swing and turnout ratio come from that draw (so the rest of the state moves with
## the scraped counties), plus
##   REST_COMMON_SD  the rest of the state swinging differently from the scraped counties
##                   (logit, one value per draw)
##   REST_COUNTY_SD  each county's own swing (logit, independent across counties; real TX
##                   between-county swing sd is about 2 points, ~0.08 logit)
##   REST_TURN_SD    each county's turnout vs expectation (log scale)
## all applied only to the part of a county not yet counted. These are set by judgment,
## not fitted; the calibration nights do not cover the rest of the state.
## Fits with bounds instead of draws (conf_seq, conformal; project_rest_bounds()): each end
## of a party's interval takes the swing and turnout ratio from the scraped counties'
## interval ends that are worst for that party, plus the quantiles of the REST_* terms.
##
##   county_baseline("TX")                           2024 / 2022 county results, all counties
##   read_cbs_latest("TX", "gov", env = "live")      the latest CBS snapshot for a race
# ========================================

REST_COMMON_SD = 0.05
REST_COUNTY_SD = 0.08
REST_TURN_SD = 0.10

#' County history for the statewide rows
#'
#' Source, first that exists:
#' 1. 26_general/input_data/county_history/{ST}_county_history.csv, for states votehub
#'    doesn't cover (CA, NY): county_fips, pres24_dem, pres24_rep, pres24_total,
#'    gov22_total (may be blank: 2024 presidential turnout is used instead). AK uses its 40
#'    election districts, county_fips 02001-02040.
#' 2. votehub's precinct estimates (input_data/votehub-shapefiles/estimated_past_results.csv).
#'
#' @param state Two-letter state code.
#' @param dir_26 Path to 26_general.
#' @return Tibble, one row per county (5-digit FIPS): Harris 2024 two-party share, 2024
#'   third-party share, 2022 governor and 2024 presidential votes.
county_baseline = function(state, dir_26 = find_26_general()) {
  f1 = file.path(dir_26, "input_data", "county_history", paste0(state, "_county_history.csv"))
  if (file.exists(f1)) {
    x = readr::read_csv(f1, col_types = readr::cols(.default = "c"), progress = FALSE)
    miss = setdiff(c("county_fips", "pres24_dem", "pres24_rep", "pres24_total"), names(x))
    if (length(miss)) {
      stop(basename(f1), " is missing columns: ", paste(miss, collapse = ", "))
    }
    if (!"gov22_total" %in% names(x)) {
      x$gov22_total = NA_character_
    }
    return(
      x |>
        transmute(
          county_fips = stringr::str_pad(county_fips, 5, pad = "0"),
          across(c(pres24_dem, pres24_rep, pres24_total, gov22_total), as.numeric),
          pres24_oth = pmax(pres24_total - pres24_dem - pres24_rep, 0)
        ) |>
        mutate(
          h24 = pres24_dem / pmax(pres24_dem + pres24_rep, 1),
          oth24 = pres24_oth / pmax(pres24_total, 1),
          exp_base = if_else(coalesce(gov22_total, 0) > 0, gov22_total, pres24_total)
        )
    )
  }
  f = file.path(dir_26, "input_data", "votehub-shapefiles", "estimated_past_results.csv")
  if (!file.exists(f)) {
    stop("Missing ", f)
  }
  x = readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE) |>
    filter(state_abbr == state)
  if (!nrow(x)) {
    stop(
      "No county history for ",
      state,
      ": add input_data/county_history/",
      state,
      "_county_history.csv (county_fips, pres24_dem, pres24_rep, pres24_total, gov22_total)"
    )
  }
  x |>
    mutate(
      county_fips = stringr::str_extract(unit_id, "^[0-9]+"),
      across(c(pres24_dem, pres24_rep, pres24_oth, pres24_total, gov22_total), as.numeric)
    ) |>
    summarize(
      across(c(pres24_dem, pres24_rep, pres24_oth, pres24_total, gov22_total), \(x) {
        sum(x, na.rm = TRUE)
      }),
      .by = county_fips
    ) |>
    mutate(
      h24 = pres24_dem / pmax(pres24_dem + pres24_rep, 1),
      oth24 = pres24_oth / pmax(pres24_total, 1),
      exp_base = if_else(gov22_total > 0, gov22_total, pres24_total)
    )
}

#' Latest saved CBS snapshot for a race
#'
#' @param state,contest,district The race.
#' @param env CBS feed folder ("live", "test", "sim").
#' @param at Newest snapshot at or before this time; NULL = the newest.
#' @param dir_26 Path to 26_general.
#' @return The snapshot written by cbs_county_pull.R, with party as dem / rep / oth; NULL if
#'   there is none yet.
read_cbs_latest = function(
  state,
  contest,
  district = NA,
  env = "live",
  at = NULL,
  dir_26 = find_26_general()
) {
  tag = if (is.na(district)) {
    paste0(state, "_", contest)
  } else {
    paste0(state, "_", contest, "_", district)
  }
  d = file.path(dir_26, "cbs_api", env, tag)
  fs = list.files(d, pattern = paste0("^", tag, "_[0-9]{8}_[0-9]{6}\\.parquet$"), full.names = TRUE)
  if (!length(fs)) {
    return(NULL)
  }
  ts = as.POSIXct(
    stringr::str_extract(basename(fs), "[0-9]{8}_[0-9]{6}"),
    format = "%Y%m%d_%H%M%S",
    tz = "UTC"
  )
  if (!is.null(at)) {
    keep = ts <= at
    fs = fs[keep]
    ts = ts[keep]
  }
  if (!length(fs)) {
    return(NULL)
  }
  x = read_pq(fs[which.max(ts)])
  # snapshots written before cbs_county_pull.R mapped CBS's party labels
  if (any(!x$party %in% c("dem", "rep", "oth"))) {
    x = mutate(
      x,
      party = case_when(
        tolower(party) %in% c("d", "dem") ~ "dem",
        tolower(party) %in% c("r", "rep") ~ "rep",
        TRUE ~ "oth"
      )
    )
  }
  x
}

#' Counties the models cover
#'
#' @param cells Cells.
#' @return CBS 3-digit county numbers of the counties in `cells`.
scraped_counties = function(cells) unique(stringr::str_extract(cells$geo, "(?<=_)[0-9]{3}(?=_)"))

#' The other counties' counted votes and baseline
#'
#' @param cbs CBS snapshot (`read_cbs_latest()`).
#' @param base `county_baseline()`.
#' @param cells Cells (the scraped counties, which are left out).
#' @return Tibble, one row per other county: counted d / r / o, share of expected vote
#'   counted p, precincts in / of, h24 (2024 two-party Dem share), oth24 (2024 third-party
#'   share), exp_base (baseline votes). A county missing from `base` gets the state's
#'   average lean and no expected vote beyond what CBS has counted.
rest_of_state = function(cbs, base, cells) {
  cbs |>
    filter(!county_cbs %in% scraped_counties(cells)) |>
    summarize(
      d = sum(votes[party == "dem"]),
      r = sum(votes[party == "rep"]),
      o = sum(votes[party == "oth"]),
      p = pmin(pmax(first(county_pct_exp_vote) / 100, 0), 1),
      prec_in = first(county_precincts_in),
      prec_of = first(county_precincts_of),
      .by = c(county_fips, county_cbs)
    ) |>
    left_join(select(base, county_fips, h24, oth24, exp_base), by = "county_fips") |>
    # a county missing from the history file: the state's average lean, and no expected
    # vote beyond what CBS has counted
    mutate(
      h24 = coalesce(h24, sum(base$pres24_dem) / sum(base$pres24_dem + base$pres24_rep)),
      oth24 = coalesce(oth24, sum(base$pres24_oth) / sum(base$pres24_total)),
      exp_base = coalesce(exp_base, 0)
    )
}

#' Project the other counties' final votes
#'
#' Method in the file header. With `noise`, adds the `REST_COMMON_SD`, `REST_COUNTY_SD`
#' and `REST_TURN_SD` terms to the part of each county not yet counted.
#'
#' @param rest `rest_of_state()`.
#' @param swing Length-B vector of logit swings (one per draw; length 1 for a point).
#' @param turn_ratio Length-B vector of turnout ratios.
#' @param noise Add the REST_* terms.
#' @return B x 3 matrix of rest-of-state Dem / Rep / Oth final votes.
project_rest = function(rest, swing, turn_ratio, noise = TRUE) {
  B = length(swing)
  n = nrow(rest)
  if (n == 0) {
    return(matrix(0, B, 3, dimnames = list(NULL, c("dem", "rep", "oth"))))
  }
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
  cbind(
    dem = rowSums(M(rest$d) + V * (1 - o_rem) * s_rem),
    rep = rowSums(M(rest$r) + V * (1 - o_rem) * (1 - s_rem)),
    oth = rowSums(M(rest$o) + V * o_rem)
  )
}

#' Rest-of-state party totals for a fit with bounds
#'
#' A party's low end takes the ends worst for it (Dem: low share, low turnout; Rep: high
#' share, low turnout; third parties: low turnout), its high end the opposite, and both add
#' the quantiles of the REST_* terms (`DRAWS` draws around the point, seeded). NA ends give
#' NA bounds.
#'
#' @param rest `rest_of_state()`.
#' @param swing,turn_ratio `c(point, lower, upper)`, from the scraped counties' two-party
#'   share and turnout intervals (`fit_swing_bounds()`).
#' @param level Interval level.
#' @return Tibble: party, pred, pred_min, pred_max.
project_rest_bounds = function(rest, swing, turn_ratio, level = 0.95) {
  a = (1 - level) / 2
  # rows: point; (low share, low turnout); (high, high); (low, high); (high, low)
  sc = project_rest(rest, swing[c(1, 2, 3, 2, 3)], turn_ratio[c(1, 2, 3, 3, 2)], noise = FALSE)
  set.seed(SEED)
  nz = project_rest(rest, rep(swing[1], DRAWS), rep(turn_ratio[1], DRAWS)) -
    matrix(sc[1, ], DRAWS, 3, byrow = TRUE)
  q = function(p) unname(apply(nz, 2, quantile, p, names = FALSE))
  tibble(
    party = c("dem", "rep", "oth"),
    pred = unname(sc[1, ]),
    pred_min = unname(c(sc[2, "dem"], sc[5, "rep"], sc[2, "oth"])) + q(a),
    pred_max = unname(c(sc[3, "dem"], sc[4, "rep"], sc[3, "oth"])) + q(1 - a)
  )
}

#' Swing and turnout ratio of a fit with bounds
#'
#' @param fit Fit with bounds (conf_seq or conformal).
#' @param pt Point predictions `c(dem, rep, tot)`.
#' @param base_s Baseline Dem two-party share of the scraped counties.
#' @param E Their expected votes.
#' @return List of `c(point, lower, upper)` swing and turn_ratio; the point from `pt`, the
#'   ends from the fit's race row (a missing turnout end is the point, as for conformal).
fit_swing_bounds = function(fit, pt, base_s, E) {
  lg = function(x) qlogis(pmin(pmax(x, 1e-4), 1 - 1e-4))
  r = fit$race
  tw = c(pt[["dem"]] / (pt[["dem"]] + pt[["rep"]]), r$lo_twop, r$hi_twop)
  tt = c(pt[["tot"]], coalesce(r$lo_turnout, pt[["tot"]]), coalesce(r$hi_turnout, pt[["tot"]]))
  list(swing = lg(tw) - lg(base_s), turn_ratio = tt / max(E, 1))
}

#' Statewide prediction rows for one race x model
#'
#' Scraped counties from the model plus every other county from CBS (file header), in the
#' same format as `to_pred_data()`, with geo = "all".
#'
#' @param fit `run_race()` result.
#' @param model Model name.
#' @param cells Cells the model fit.
#' @param returns The race's returns.
#' @param race One row of `races_from_priors()`.
#' @param time Snapshot time.
#' @param cbs Latest CBS snapshot for the race.
#' @param base `county_baseline()`.
#' @param level Interval level.
#' @return Tibble in enightmodels' prediction format (model_conf ending "_plus_rest"), or
#'   NULL when the fit is not ok or there is no CBS snapshot.
state_pred_data = function(fit, model, cells, returns, race, time, cbs, base, level = 0.95) {
  if (!fit_ok(fit) || is.null(cbs) || !nrow(cbs)) {
    return(NULL)
  }
  name = model_label(model)
  model = model_base(model)
  rest = rest_of_state(cbs, base, cells)
  sc = scraped_counties(cells)
  b32 = cbs |>
    distinct(county_fips, county_cbs) |>
    filter(county_cbs %in% sc) |>
    left_join(base, by = "county_fips")
  h32 = sum(b32$pres24_dem) / max(sum(b32$pres24_dem + b32$pres24_rep), 1)
  e32 = sum(b32$exp_base)
  # tonight's turnout ratio and swing are measured on the scraped counties' history, found
  # through the CBS rows for those counties (CBS's feed lists every county)
  if (!nrow(b32) || !(e32 > 0)) {
    stop(
      "The CBS snapshot has no rows for the scraped counties, so tonight's swing and turnout can't be carried to the rest of the state"
    )
  }
  lg = function(x) qlogis(pmin(pmax(x, 1e-4), 1 - 1e-4))

  cand = reported_by_candidate(returns)
  has_draws = is.null(fit$pred)
  conf = if (is.na(fit$conf %||% NA)) NA_character_ else paste0(fit$conf, "_plus_rest")
  if (has_draws) {
    dr = fit$draws
    D32 = dr$dem
    R32 = dr$rep
    T32 = dr$turnout
    pt = fit_point(fit)
  } else {
    # bounds instead of draws: the scraped counties' party bounds plus the rest's
    # (project_rest_bounds()); a fit without an interval keeps NA bounds
    pr = mutate(fit$pred, grp = if_else(party %in% c("dem", "rep"), party, "oth"))
    pt = c(
      dem = sum(pr$pred[pr$grp == "dem"]),
      rep = sum(pr$pred[pr$grp == "rep"]),
      tot = sum(pr$pred)
    )
    sb = fit_swing_bounds(fit, pt, h32, e32)
    rb = project_rest_bounds(rest, sb$swing, sb$turn_ratio, level)
    b32 = summarize(pr, across(c(pred_min, pred_max), sum), .by = grp)
    i = match(rb$party, b32$grp) # NA: no candidate of that party in the race
    lo = setNames(ifelse(is.na(i), 0, b32$pred_min[i]) + rb$pred_min, rb$party)
    hi = setNames(ifelse(is.na(i), 0, b32$pred_max[i]) + rb$pred_max, rb$party)
  }

  # rest of the state: point (no noise, from the point fit) and per-draw
  rest_pt = project_rest(
    rest,
    lg(pt[["dem"]] / (pt[["dem"]] + pt[["rep"]])) - lg(h32),
    pt[["tot"]] / max(e32, 1),
    noise = FALSE
  )
  rest_dr = if (has_draws) {
    project_rest(rest, lg(D32 / (D32 + R32)) - lg(h32), T32 / max(e32, 1))
  } else {
    NULL
  }

  # by party: point, and draws if any
  P = c(
    dem = pt[["dem"]] + rest_pt[1, "dem"],
    rep = pt[["rep"]] + rest_pt[1, "rep"],
    oth = pt[["tot"]] - pt[["dem"]] - pt[["rep"]] + rest_pt[1, "oth"]
  )
  names(P) = c("dem", "rep", "oth")
  Dd = if (has_draws) {
    list(
      dem = D32 + rest_dr[, "dem"],
      rep = R32 + rest_dr[, "rep"],
      oth = (T32 - D32 - R32) + rest_dr[, "oth"]
    )
  } else {
    NULL
  }
  tot_pt = sum(P)
  tot_d = if (has_draws) Dd$dem + Dd$rep + Dd$oth else NULL

  # reported: scraped counties from our returns, the rest from CBS
  rep_rest = c(dem = sum(rest$d), rep = sum(rest$r), oth = sum(rest$o))
  cand = cand_split(cand)
  q = function(x) unname(cred(x, level))
  rows = lapply(seq_len(nrow(cand)), function(i) {
    pty = cand$grp[i]
    k = cand$w[i]
    rp = cand$rep[i] + k * rep_rest[[pty]]
    point = k * P[[pty]]
    if (has_draws) {
      v = k * Dd[[pty]]
      ci_v = q(v)
      ci_s = q(v / tot_d)
      pw = if (pty == "dem") {
        mean(Dd$dem > Dd$rep)
      } else if (pty == "rep") {
        mean(Dd$rep > Dd$dem)
      } else {
        NA_real_
      }
    } else {
      # the share version of a bound (bound / point total) is approximate, as in to_pred_data()
      ci_v = k * c(lo[[pty]], hi[[pty]])
      ci_s = ci_v / tot_pt
      pw = NA_real_
    }
    tibble(
      candidate = cand$candidate[i],
      party = cand$party[i],
      pred_type = c("vote", "share"),
      rep = c(rp, NA_real_),
      pred = c(point, point / tot_pt),
      pred_min = pmax(c(ci_v[1], ci_s[1]), 0),
      pred_max = c(ci_v[2], ci_s[2]),
      p_win = pw
    )
  })
  out = bind_rows(rows)
  # reported share = candidate's share of everything counted so far, statewide
  rep_tot = sum(out$rep[out$pred_type == "vote"])
  out$rep[out$pred_type == "share"] = out$rep[out$pred_type == "vote"] / max(rep_tot, 1)

  n_prec = n_distinct(cells$geo)
  pct = (pct_reporting(cells, returns) * n_prec + sum(rest$prec_in, na.rm = TRUE)) /
    (n_prec + sum(rest$prec_of, na.rm = TRUE))
  # race_id carried, not reconstructed -- see the note in output.R::to_pred_data()
  bind_cols(
    race[
      rep(1, nrow(out)),
      c("race_id", "state", "election", "jurisdiction", "contest", "district")
    ],
    out
  ) |>
    mutate(
      time = time,
      model = name,
      model_conf = conf,
      pct_rep = pct,
      geo = "all",
      cbs_as_of = max(cbs$observed_at),
      election = as.Date(election),
      district = as.integer(district)
    ) |>
    relocate(
      state,
      election,
      jurisdiction,
      contest,
      district,
      candidate,
      party,
      pred_type,
      time,
      model,
      model_conf,
      pct_rep,
      rep,
      pred,
      pred_min,
      pred_max,
      p_win,
      geo,
      race_id
    ) |>
    arrange(pred_type, desc(pred))
}
