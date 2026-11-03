# ========================================
## model_medsl.R -- medsl_sur: the precinct x vote-method model in live/medsl/
## (model_turnout.R, with model_hybrid.R re-centring its priors on tonight's returns),
## run on any race in the live format.
##
## medsl_setup() builds one model environment per state (and county set and spec), cached
## for the session: it puts STATE, covs_long and mode_hist (built here from
## {ST}_precinct.parquet and the mode history, medsl_inputs()) into a fresh environment and
## sources model_turnout.R and model_hybrid.R into it. The model's switches are environment
## variables read at that point (e.g. MODE_OFFSET_SOURCE, USE_COUNTY_RE, SIGMA_STRUCT,
## HY_LEVEL / HY_COMP / HY_SHARE / HY_COUNTY). MODE_OFFSET_SOURCE defaults to "zero" here:
## our mode history has turnout by method but not by party.
##
## fit_medsl_sur(cells, returns) takes the same inputs as the other models (run.R). A cell
## (precinct x method) counts as reported once it has any votes (its shares are used at
## once) and as complete once that method is fully counted (its turnout is used only then).
## Intervals (results.R): credible (default), the posterior draws; conf_seq, against
## prior_totals() on the model's own (not re-centred) priors; conformal, from fit_stage()'s
## per-cell final votes and fitted shares (`cell_votes`).
##
## Draws: model_turnout.R's DRAWS (500) unless MEDSL_DRAWS is set, cut further for very
## large states by MEDSL_MAX_PRECINCT_DRAWS (draws x precincts, default 3.5e6). TX, one
## race: about 6 s of fixed work plus 1 s per 100 draws.
##
## YouGov race predictions (yg_pred_bla, yg_pred_his) are blank for some MI and CA
## precincts; those get the county mean (state mean if the whole county is blank), noted in
## `settings`. Any other blank required column stops the fit.
# ========================================

MEDSL_SRC = "scripts/model/live/medsl/model_turnout.R"
HYBRID_SRC = "scripts/model/live/medsl/model_hybrid.R"
.medsl_env = new.env() # cache: one model environment per state x county set x spec

#' model_turnout.R's inputs from our model inputs
#'
#' Builds `covs_long` (long by party) and `mode_hist`. Column mapping: county <-
#' county_name; turnout22/24 <- L2 turnout rates; age <- p_age_65_up; male <- p_male;
#' pred.bla / pred.his <- yg_pred_bla / yg_pred_his; tp_raw <- oth_base (the model clamps
#' it to [0, 0.25]); est (dem / rep) <- est_turnout x dem2 / (1 - dem2). Blank YouGov race
#' predictions get the county mean, then the state mean; any other blank column stops.
#'
#' @param state Two-letter state code.
#' @param dir_26 Path to 26_general.
#' @param mode_election Passed to `mode_history()`.
#' @param cols Extra precinct columns a demographic spec needs (`demo_covs()`; "dem2_sq" is
#'   dem2 squared), joined on under their own names.
#' @return List: covs_long, mode_hist (geo, method, party, votes: the turnout split is real,
#'   the party split just follows dem2), mode_forbid (geo x method cells the county's feed
#'   cannot report), note (what was filled, or "").
medsl_inputs = function(
  state,
  dir_26 = find_26_general(),
  mode_election = NULL,
  cols = character()
) {
  dir_in = file.path(dir_26, "input_data", "model_inputs")
  inp = read_pq(file.path(dir_in, paste0(state, "_precinct.parquet")))
  mh = mode_history(state, dir_26, mode_election)
  covs_wide = inp |>
    transmute(
      geo = as.character(geo),
      county = county_name,
      geostratum,
      stratum_demo,
      n,
      dem2,
      turnout22,
      turnout24,
      age = p_age_65_up,
      male = p_male,
      `pred.bla` = yg_pred_bla,
      `pred.his` = yg_pred_his,
      tp_raw = oth_base,
      est_turnout,
      # 2026 congressional district, for the turnout equations' district effects
      cd = if ("cd_main" %in% names(inp)) coalesce(as.character(cd_main), "0") else "0"
    )
  if (length(setdiff(cols, "dem2_sq"))) {
    covs_wide = left_join(
      covs_wide,
      select(demo_covs(state, dir_26), geo, all_of(setdiff(cols, "dem2_sq"))),
      by = "geo"
    )
  }
  if ("dem2_sq" %in% cols) {
    covs_wide = mutate(covs_wide, dem2_sq = dem2^2)
  }
  # fill blank YouGov race predictions: county mean, then state mean
  filled = character()
  for (v in c("pred.bla", "pred.his")) {
    nas = sum(is.na(covs_wide[[v]]))
    if (nas) {
      filled = c(filled, sprintf("%s blank for %d precincts", v, nas))
      covs_wide = covs_wide |>
        mutate(!!v := coalesce(.data[[v]], mean(.data[[v]], na.rm = TRUE)), .by = county) |>
        mutate(!!v := coalesce(.data[[v]], mean(.data[[v]], na.rm = TRUE)))
    }
  }
  bad = names(which(colSums(is.na(select(covs_wide, -geo))) > 0))
  if (length(bad)) {
    stop(
      state,
      "_precinct.parquet has blank values in: ",
      paste(bad, collapse = ", "),
      ". Rerun prep_model_inputs.R ",
      state,
      "."
    )
  }
  covs_long = bind_rows(
    mutate(covs_wide, party = "dem", est = est_turnout * dem2),
    mutate(covs_wide, party = "rep", est = est_turnout * (1 - dem2))
  ) |>
    select(-est_turnout) |>
    mutate(method = "all")
  # mode history in the model's schema (geo, method, party, votes). Only the turnout split
  # is real; the party split just follows dem2 and carries no mode lean.
  mode_hist = mh |>
    mutate(geo = as.character(geo)) |>
    inner_join(select(covs_wide, geo, dem2), by = "geo") |>
    reframe(party = c("dem", "rep"), votes = voters * c(dem2, 1 - dem2), .by = c(geo, method))
  # Cells a county's feed cannot report (lookups/mode_fold.csv), resolved to this state's
  # precincts. model_turnout.R gives these a prior share of EXACTLY 0 rather than letting
  # them fall through to the MODE_TURN_SHARE fallback; the reason it matters is q_share, and
  # it is spelled out at the use site.
  fold = read_mode_fold() |> filter(state == !!state)
  mode_forbid = if (!nrow(fold)) {
    tibble(geo = character(), method = character())
  } else {
    tibble(geo = covs_wide$geo, county_cbs = geo_county(covs_wide$geo)) |>
      inner_join(select(fold, county_cbs, method = from_method), by = "county_cbs") |>
      select(geo, method)
  }
  list(
    covs_long = covs_long,
    mode_hist = mode_hist,
    mode_forbid = mode_forbid,
    note = if (length(filled)) {
      paste0("filled with county means: ", paste(filled, collapse = "; "))
    } else {
      ""
    }
  )
}

#' A demographic spec as model_turnout.R settings
#'
#' Covariates follow log_n (the model's scale term), named as `medsl_inputs()` names them;
#' `(1 | x)` terms, and tpsob_g's geostratum, become the random-effect grouping.
#'
#' @param demo Spec name (demos.R).
#' @return NULL for "medsl_cov" (the model's own design, unchanged). Otherwise a list: covs
#'   (COVS_TURN and COVS_SHARE), re (RE_GROUPING, or NULL to keep the county effect;
#'   "cory" for both strata), cols (frame columns to add).
medsl_spec = function(demo) {
  if (demo == "medsl_cov") {
    return(NULL)
  }
  f = DEMOS[[check_demo(demo)]]
  fixed = if (is.null(reformulas::nobars(f))) {
    character()
  } else {
    attr(terms(reformulas::nobars(f)), "term.labels")
  }
  grp = c(
    vapply(reformulas::findbars(f), \(b) deparse(b[[3]]), ""),
    intersect(fixed, c("geostratum", "stratum_demo"))
  )
  fixed = setdiff(fixed, c("geostratum", "stratum_demo"))
  own = c(
    `I(dem2^2)` = "dem2_sq",
    p_age_65_up = "age",
    p_male = "male",
    yg_pred_bla = "pred.bla",
    yg_pred_his = "pred.his",
    dem2 = "dem2",
    turnout22 = "turnout22",
    turnout24 = "turnout24"
  )
  covs = ifelse(fixed %in% names(own), own[fixed], fixed)
  re = if (setequal(grp, c("geostratum", "stratum_demo"))) {
    "cory"
  } else if (length(grp)) {
    grp[1]
  }
  list(
    covs = c("log_n", unname(covs)),
    re = re,
    cols = c(setdiff(fixed, names(own)), if ("I(dem2^2)" %in% fixed) "dem2_sq")
  )
}

#' Cache key for a model environment
#'
#' @param state Two-letter state code.
#' @param counties County names, or NULL for the whole state.
#' @param demo Spec name.
#' @return Character key.
medsl_key = function(state, counties, demo = "medsl_cov", by_method = TRUE) {
  paste0(
    if (is.null(counties)) state else paste(state, paste(sort(unique(counties)), collapse = "|")),
    if (demo != "medsl_cov") paste0("|", demo),
    if (!by_method) "|nomode",
    # the composition prior depends on lookups/mode_fold.csv and on CBS's county expected
    # vote, so neither an edit to the table nor a different frozen target may be served a
    # fit cached under the old one
    mode_fold_tag(state),
    mode_target_tag(state),
    # the turnout constraint is baked into the env at source time, so a cached fit must not
    # outlive the switch. `counties` is already in the key and the CBS totals are frozen for
    # the session, so the target itself cannot change underneath a key -- only this can.
    if (Sys.getenv("CBS_SCRAPED_TURNOUT", unset = "1") == "0") "|notot"
  )
}

# THE SHARE PRIOR'S STATEWIDE LEVEL, FROM priors.csv (CBS_SHARE_PRIOR=0 = off, the default).
#
# `dem2` is a 2024 PRESIDENTIAL two-party baseline (prep_model_inputs.R section 4: 2024
# precinct returns where the voter file has them, else the YouGov pr_harris/pr_trump model),
# so prior_s's LEVEL is 2024 presidential. priors.csv's prior_dem / prior_rep are a 2026
# forecast for THIS race, and they are the only 2026 partisan information in the inputs --
# CBS publishes no partisan expectation at any geography (all seven `expected*` fields are
# turnout, constant across candidates within a county; per-candidate `pct` is the share
# COUNTED SO FAR). So this is the share-side twin of est_turnout being 2022 turnout, except
# that the turnout side could reach for CBS and this side cannot.
#
# STATEWIDE TO STATEWIDE, AND THAT IS THE WHOLE SUBTLETY. The shift is
#
#     shift = logit(T_state) - logit(P_state)
#
# with T_state the 2026 target and P_state the STATEWIDE 2024 presidential two-party Dem
# share, applied as a COMMON logit shift to every precinct's prior_s.
#
# It is tempting to compare T_state against the FRAME's own dem2 instead, and that is wrong
# in exactly the way STATE_TURNOUT was wrong (see SCRAPED_TURNOUT in model_turnout.R). The
# frame is a county subset -- OH 15 of 88 counties, TX 72 of 254 -- and the subsets are
# partisan-skewed: OH's 15 are the metros, at pres24 0.5392 against a statewide 0.4432, a
# +0.385 logit skew. Measured 2026-11-03, OH_sen would take -0.078 logit (R-ward) on the
# naive comparison where +0.307 (D-ward) is right: a ~9.5pp error, silent, because every
# per-precinct bound still holds.
#
# But UNLIKE the turnout case this needs no rescoping, because dem2 already IS each
# precinct's 2024 presidential share -- the frame's geography is already in the prior and a
# common shift preserves it. The frame term is a residual, logit(P_frame) - logit(dem2_frame),
# measured at -0.020 to +0.021 logit across all 11 races against swings of 0.048 to 0.352. It
# cancels. So: one statewide number per state, no county join, no coverage gate. Keeping the
# two scopes separate is the fix; correcting for their mixture was the wrong instinct.
#
# WHAT IT ASSUMES: that the 2024->2026 swing is roughly uniform across counties. Evidence,
# from the MEDSL 2022 official archive -- the frame-vs-state partisan skew is stable between
# presidential and midterm electorates (OH frame: 2022 Sen +0.3549, 2022 Gov +0.3456, pres24
# +0.3853; TX frame: 2022 Gov +0.1438, pres24 +0.1260), and the sign of the pres24 discrepancy
# is not even consistent across the two states, so it is not a correctable bias. ~0.02-0.04
# logit of county-composition drift is unaccounted for, well inside the swing sd below.
#
# HOW BIG A CLAIM THIS IS. calibrate_swing.R measured the statewide two-party Dem SD of
# movement between a prior contest and its target at 0.22-0.26 logit over 9 states x 37
# contest pairs (model_core.R: "ACTUAL statewide swing sd" is the authoritative wording -- it
# is a DISPERSION, and anywhere it reads as a typical movement that is a magnitude and wrong).
# The shifts priors.csv implies run 0.028 to 0.354, median ~0.25. So the quantity imposed is
# the same magnitude as its own uncertainty: a real improvement in expectation, not a precise
# correction. Clamped at SHARE_PRIOR_MAX_SHIFT (0.6, ~2.4x that sd) and deliberately NOT at
# SH_MAX_SHIFT -- see "the bound, and why it is NOT SH_MAX_SHIFT" at the use site: 0.25 bounds
# model_hybrid's data-driven RESIDUAL, whereas this is the EXPECTED move, and bounding an
# expected move at one sd of itself withholds the forecast in the ordinary case. A binding
# clamp is SIGNAL that priors.csv is asking for a swing past 2.4 sd, and it is logged as such
# rather than silently applied.
#
# WHY A LOCATION SHIFT AND NOT A PRIOR SD. SD_SYS_LEVEL (model_turnout.R) already used the
# same 0.22 to LOOSEN the statewide level prior, and is off by default: coverage unchanged
# (15/15 either way), point error WORSE (max |err| 1.40 -> 2.23pp on TX 2024), because the
# early-night data a looser prior lets through is BIASED (+8.9pp Dem in the first batches),
# not merely noisy. Its own conclusion is that location and width are separate jobs and width
# belongs on the PREDICTIVE draw, SD_UNREP's shape. This moves the location only.
#
# OFF BY DEFAULT. Built 2026-11-03, election day, with no outcome test behind it: no sim can
# validate it (make_sim.R builds its share truth FROM priors.csv, so the sim would agree by
# construction) and 2026 truth does not exist yet. CBS_SHARE_PRIOR=1 turns it on.

#' The share prior's statewide level shift, from priors.csv
#'
#' `logit(T_state) - logit(P_state)`: the 2026 two-party target for this race against the
#' statewide 2024 presidential baseline that `dem2` is built from. A common logit shift on
#' `prior_s`, applied by `fit_medsl_sur()` BEFORE `fit_stage_hybrid()` so model_hybrid's
#' share arm measures its residual against the corrected prior.
#'
#' Takes no `geos`, and that is deliberate: unlike `scraped_turnout_var()` this is NOT
#' frame-scoped, because `dem2` already carries the frame's geography (see the comment above).
#' Reading the frame here would reintroduce the scope mixture the design exists to avoid.
#'
#' @param state Two-letter state code.
#' @param race_id The race (`returns$race_id`), since priors.csv differs BETWEEN races in a
#'   state -- TX_gov 0.4912 vs TX_sen 0.5182 -- while the model environment is cached per
#'   state.
#' @param dir_26 Path to 26_general.
#' `P_state` comes from `county_baseline()` by default; a `pres24_dem2` column in priors.csv
#' overrides it where one is filled (provenance, not arithmetic -- see the use site).
#'
#' @return List (`shift`, `t_state`, `p_state`, `src`), or NULL when the arm is off, this
#'   race is not in priors.csv, or no statewide 2024 baseline can be had. Every NULL under
#'   `CBS_SHARE_PRIOR=1` warns -- a silently inert arm that was explicitly asked for is the
#'   failure mode this guards.
share_prior_shift = function(state, race_id, dir_26 = find_26_general()) {
  if (Sys.getenv("CBS_SHARE_PRIOR", unset = "0") != "1") {
    return(NULL)
  }
  # an explicit shift wins, so a sweep or a backtest can set it directly
  ex = Sys.getenv("SHARE_PRIOR_SHIFT", unset = "")
  if (nzchar(ex)) {
    v = suppressWarnings(as.numeric(ex))
    if (!is.finite(v)) {
      cli::cli_abort("SHARE_PRIOR_SHIFT is set to {.val {ex}}, which is not a number.")
    }
    return(list(shift = v, t_state = NA_real_, p_state = NA_real_, src = "SHARE_PRIOR_SHIFT"))
  }
  f = file.path(dir_26, "input_data", "priors.csv")
  if (!file.exists(f)) {
    return(NULL)
  }
  pr = readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE)
  if (!"pres24_dem2" %in% names(pr)) {
    pr$pres24_dem2 = NA_character_ # optional override; county_baseline() supplies it below
  }
  st = dplyr::filter(pr, state == !!state)
  # The live path's race_id is "MN_sen" (races_from_priors()), but the SIM path's is the bare
  # contest "sen" (returns_from_sim() filters sim$truth$contest). Resolve both, or every
  # sim-based test of this arm no-ops while looking like it ran -- which is exactly what
  # happened the first time it was wired, and the SHARE_PRIOR_SHIFT override masked it
  # because that path never reaches this lookup.
  row = dplyr::filter(st, race_id == !!race_id)
  if (!nrow(row)) {
    row = dplyr::filter(st, race_id == paste0(!!state, "_", !!race_id))
  }
  if (nrow(row) != 1) {
    # CBS_SHARE_PRIOR=1 is an explicit request for this arm, so failing to find the race is
    # worth saying out loud. Returning NULL quietly would leave the fit on its 2024 level
    # with nothing in the log or the settings to show the arm was asked for and did nothing.
    cli::cli_alert_warning(c(
      "CBS_SHARE_PRIOR=1 but {.path priors.csv} has no row for {state} {.val {race_id}} \\
       ({nrow(row)} match{?es}), so the share prior keeps its 2024 level."
    ))
    return(NULL)
  }
  num = function(x) suppressWarnings(as.numeric(x))
  d = num(row$prior_dem)
  r = num(row$prior_rep)
  if (!all(is.finite(c(d, r))) || d <= 0 || r <= 0) {
    cli::cli_alert_warning(
      "CBS_SHARE_PRIOR=1 but {state} {.val {race_id}} has no prior_dem/prior_rep in \\
       {.path priors.csv}, so the share prior keeps its 2024 level."
    )
    return(NULL)
  }
  # ---- P_state: the statewide 2024 presidential two-party Dem share -------------------
  # DERIVED BY DEFAULT, overridable by a column. county_baseline() (counties.R) carries
  # pres24_dem / pres24_rep for EVERY county in the state, so summing them gives this
  # directly and the arm needs no new data entry to work.
  #
  # The optional `pres24_dem2` column exists for PROVENANCE, not for arithmetic. For OH and
  # TX -- the two states whose frames are county subsets, so the two that matter most --
  # county_baseline() falls through to votehub's `estimated_past_results.csv`, whose partisan
  # columns are not validated against official returns. Only the statewide AGGREGATE has an
  # official check, and only for TX: the county sum is 0.43061 against sim_state_params.csv's
  # 0.430629 (42.46 / (42.46 + 56.14)), agreeing to 5 decimals, with the other eight states
  # consistent with published 2024 results to ~1e-3. That is one official check plus eight
  # plausibility checks, which is why a hand-entered audited number is allowed to WIN --
  # but it is an improvement on this path, not a precondition for it.
  p_state = NA_real_
  src = NA_character_
  pp = unique(num(st$pres24_dem2[nzchar(trimws(dplyr::coalesce(st$pres24_dem2, "")))]))
  if (length(pp) > 1) {
    # a per-STATE value living in a per-RACE file (as `state`, `election` and `jurisdiction`
    # already do), so one state's rows must agree. Not something to average: two 2024
    # baselines for one state would give two different swings for the same election.
    cli::cli_abort(c(
      "{.path {f}} gives {state} more than one {.field pres24_dem2}: {.val {pp}}.",
      "i" = "It is the statewide 2024 presidential two-party Dem share -- one value per \\
             state, repeated on that state's race rows."
    ))
  }
  if (length(pp) == 1 && is.finite(pp) && pp > 0 && pp < 1) {
    p_state = pp
    src = "priors.csv pres24_dem2"
  } else {
    b = tryCatch(county_baseline(state, dir_26), error = function(e) NULL)
    if (!is.null(b) && nrow(b)) {
      tot = sum(b$pres24_dem, na.rm = TRUE) + sum(b$pres24_rep, na.rm = TRUE)
      if (is.finite(tot) && tot > 0) {
        p_state = sum(b$pres24_dem, na.rm = TRUE) / tot
        src = "county_baseline()"
      }
    }
  }
  if (!is.finite(p_state) || p_state <= 0 || p_state >= 1) {
    cli::cli_alert_warning(
      "CBS_SHARE_PRIOR=1 but no statewide 2024 baseline for {state} -- neither a \\
       {.field pres24_dem2} in {.path priors.csv} nor a usable {.fn county_baseline}. \\
       The share prior keeps its 2024 level."
    )
    return(NULL)
  }
  t_state = d / (d + r)
  lg = function(p) qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  list(
    shift = lg(t_state) - lg(p_state),
    t_state = t_state,
    p_state = p_state,
    src = paste0("priors.csv / ", src)
  )
}

#' CBS's expected vote over a fit's own precincts, as a SCRAPED_TURNOUT setting
#'
#' Returns a named character vector for `Sys.setenv()`, or `character()` when there is
#' nothing to set -- CBS has published no county totals, or `CBS_SCRAPED_TURNOUT=0`.
#'
#' Frozen for the night like the composition target, and for the same reason: a target that
#' moved between refreshes would move turnout with no new votes behind it. Only a non-empty
#' read is cached.
#'
#' @param state Two-letter state code.
#' @param geos The fit's precinct ids (`covs_long$geo`), already filtered to its counties.
#' @param dir_26 Path to 26_general.
#' @return Named character vector, possibly empty.
scraped_turnout_var = function(state, geos, dir_26 = find_26_general()) {
  if (Sys.getenv("CBS_SCRAPED_TURNOUT", unset = "1") == "0") {
    return(character())
  }
  if (nzchar(Sys.getenv("SCRAPED_TURNOUT"))) {
    return(character()) # an explicit setting wins, so backtests and sweeps are unaffected
  }
  key = paste("tot", state, if (exists("MODEL_CBS_ENV")) MODEL_CBS_ENV else "live", sep = "|")
  tot = .mode_target_env[[key]]
  if (is.null(tot)) {
    tot = tryCatch(
      cbs_county_total(
        state,
        env = if (exists("MODEL_CBS_ENV")) MODEL_CBS_ENV else "live",
        dir_26 = dir_26
      ),
      error = function(e) NULL
    )
    if (is.null(tot) || !nrow(tot)) {
      return(character())
    }
    .mode_target_env[[key]] = tot
  }
  cc = unique(geo_county(geos))
  hit = filter(tot, county_cbs %in% cc)
  if (!nrow(hit)) {
    return(character())
  }
  target = sum(hit$expected)
  if (!is.finite(target) || target <= 0) {
    return(character())
  }
  cli::cli_inform(c(
    "i" = "{state}: turnout constraint target {format(round(target), big.mark = ',')} from CBS
           expected vote over {nrow(hit)} of {length(cc)} fitted counties",
    " " = "(sd {Sys.getenv('SCRAPED_TURNOUT_SD', unset = '0.05')} log;
           CBS_SCRAPED_TURNOUT=0 to disable)"
  ))
  c(SCRAPED_TURNOUT = format(target, scientific = FALSE))
}

#' CBS's expected vote PER COUNTY, for the level prior (`CBS_LEVEL_PRIOR=1`)
#'
#' Sibling of `scraped_turnout_var()`: that one collapses CBS to ONE statewide number and
#' applies it as a post-hoc constraint on the fitted total; this passes the counties through
#' individually so the level can enter each precinct's PRIOR instead -- including the
#' completeness gate, which grades counted votes against `prior_tau`.
#' Per-county because the growth is not uniform: TX asks 1.3644 over its 25 zero-file
#' counties but 1.079-1.662 BY COUNTY (measured 2026-11-03).
#' Returns county TOTALS, not ratios: the denominator is est_tot over the fit's own
#' precincts, which only the medsl env knows after its `counties` filter.
cbs_level_prior_var = function(state, geos, dir_26 = find_26_general()) {
  if (Sys.getenv("CBS_LEVEL_PRIOR", unset = "0") != "1") return(character())
  if (nzchar(Sys.getenv("CBS_COUNTY_EXPECTED"))) return(character())
  env = if (exists("MODEL_CBS_ENV")) MODEL_CBS_ENV else "live"
  key = paste("tot", state, env, sep = "|")
  tot = .mode_target_env[[key]]
  if (is.null(tot)) {
    tot = tryCatch(cbs_county_total(state, env = env, dir_26 = dir_26), error = function(e) NULL)
    if (is.null(tot) || !nrow(tot)) {
      cli::cli_warn("{state}: CBS_LEVEL_PRIOR=1 but no CBS county totals in the {.val {env}} feed -- level prior stays on 2022.")
      return(character())
    }
    .mode_target_env[[key]] = tot
  }
  cc = unique(geo_county(geos))
  hit = filter(tot, county_cbs %in% cc)
  if (!nrow(hit) || length(miss <- setdiff(cc, hit$county_cbs))) {
    cli::cli_warn("{state}: CBS_LEVEL_PRIOR=1 but {length(miss)} of {length(cc)} fitted counties have no CBS total -- level prior stays on 2022.")
    return(character())
  }
  cli::cli_inform("{state}: level prior from CBS expected vote over {nrow(hit)} fitted counties, {format(round(sum(hit$expected)), big.mark = ',')} total")
  c(CBS_COUNTY_EXPECTED = paste0(hit$county_cbs, "=", format(hit$expected, scientific = FALSE), collapse = ","))
}

#' Build (or fetch) the model environment for a state
#'
#' Cached for the session. Puts STATE, covs_long and mode_hist into a fresh environment and
#' sources model_turnout.R and model_hybrid.R into it, with the spec's settings and
#' MODE_OFFSET_SOURCE = "zero" (unless already set) passed as environment variables and
#' restored on exit.
#'
#' @param state Two-letter state code.
#' @param dir_26 Path to 26_general.
#' @param refresh Rebuild even if cached.
#' @param counties Fit only these counties' precincts (county names as in `cells$county`):
#'   the counties the live feed covers, so the model's totals never include counties whose
#'   votes come from CBS. NULL = every county in the state's inputs.
#' @param demo Demographic spec (`medsl_spec()`).
#' @return The environment, with `.draws` and `.settings` added.
medsl_setup = function(
  state,
  dir_26 = find_26_general(),
  refresh = FALSE,
  counties = NULL,
  demo = "medsl_cov",
  by_method = TRUE
) {
  key = medsl_key(state, counties, demo, by_method)
  if (!refresh && !is.null(.medsl_env[[key]])) {
    return(.medsl_env[[key]])
  }
  if (!file.exists(MEDSL_SRC)) {
    stop("Can't find ", MEDSL_SRC, " (run from the cbs-enr folder)")
  }
  spec = medsl_spec(demo)
  inp = medsl_inputs(state, dir_26, cols = spec$cols)
  if (!is.null(counties)) {
    inp$covs_long = filter(inp$covs_long, county %in% counties)
    inp$mode_hist = filter(inp$mode_hist, geo %in% inp$covs_long$geo)
    inp$mode_forbid = filter(inp$mode_forbid, geo %in% inp$covs_long$geo)
    if (!nrow(inp$covs_long)) stop("none of the counties given are in ", state, "_precinct.parquet")
  }

  # settings model_turnout.R reads from the environment, restored on exit: the spec's
  # covariates and grouping, and zero mode offsets unless the caller chose otherwise
  vars = c(
    # modelled without vote method: one pseudo-mode, so 3 equations rather than 3M.
    # model_turnout.R reads both of these at SOURCE time, which is why they go here
    # rather than being passed as arguments.
    if (!by_method) c(MODES = "all", REF_MODE = "all"),
    if (!nzchar(Sys.getenv("MODE_OFFSET_SOURCE"))) c(MODE_OFFSET_SOURCE = "zero"),
    if (!is.null(spec)) {
      c(COVS_TURN = paste(spec$covs, collapse = ","), COVS_SHARE = paste(spec$covs, collapse = ","))
    },
    if (!is.null(spec$re)) c(RE_GROUPING = spec$re),
    # CBS's expected vote over THIS FIT'S counties, as the turnout constraint's target.
    # est_turnout is each county's 2022 turnout by construction, so without this the model
    # starts from 2022 and must discover 2026's level from the returns; CBS's expectation
    # over the same counties runs 5-35% higher. The constraint is scoped to exactly this
    # frame (see SCRAPED_TURNOUT in model_turnout.R), which is why it is computed here,
    # after the `counties` filter, rather than once per state.
    scraped_turnout_var(state, inp$covs_long$geo, dir_26),
    cbs_level_prior_var(state, inp$covs_long$geo, dir_26)
  )
  old = Sys.getenv(names(vars), unset = NA, names = TRUE)
  on.exit(
    for (v in names(old)) {
      if (is.na(old[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(old[v]))
    },
    add = TRUE
  )
  if (length(vars)) {
    do.call(Sys.setenv, as.list(vars))
  }

  env = new.env(parent = globalenv())
  env$STATE = state
  env$covs_long = inp$covs_long
  env$mode_hist = inp$mode_hist
  env$mode_forbid = inp$mode_forbid
  suppressMessages(sys.source(MEDSL_SRC, envir = env))
  suppressMessages(sys.source(HYBRID_SRC, envir = env))

  env$.draws = as.integer(Sys.getenv("MEDSL_DRAWS", unset = env$DRAWS))
  env$.settings = with(
    env,
    sprintf(
      paste0(
        "model_hybrid.R (arms: level %s, composition %s, share %s, county %s) on ",
        "model_turnout.R (precinct x method: turnout, third-party and two-party equations); %d posterior draws; ",
        "bounded turnout %s (KAPPA %.2f, ref %s); Sigma %s/%s; county RE %s; mode offsets %s; gates %d cells / %d counties"
      ),
      HY_LEVEL,
      HY_COMP,
      HY_SHARE,
      HY_COUNTY,
      .draws,
      USE_COMPOSE,
      KAPPA,
      REF_MODE,
      SIGMA_SOURCE,
      SIGMA_STRUCT,
      USE_COUNTY_RE,
      MODE_OFFSET_SOURCE,
      MIN_MODE_CELLS,
      MIN_MODE_CTY
    )
  )
  if (!is.null(spec)) {
    env$.settings = sprintf(
      "%s; spec %s (covariates %s; random effect %s)",
      env$.settings,
      demo,
      paste(spec$covs, collapse = ", "),
      env$RE_GROUPING
    )
  }
  if (nzchar(inp$note)) {
    env$.settings = paste0(env$.settings, "; ", inp$note)
  }
  # which composition the estimate used, recorded on every fit: the CBS target is on by
  # default, so section 15.2's `settings` has to say whether a number came from CBS's county
  # expected vote or from the L2 mode history, without anyone having to reconstruct it
  env$.settings = paste0(
    env$.settings,
    "; composition ",
    local({
      tg = tryCatch(mode_target_for(state, dir_26), error = function(e) NULL)
      if (is.null(tg) || !nrow(tg)) {
        "mode history (no CBS county expected vote)"
      } else {
        sprintf("CBS county expected vote (%s)", tg$src[1])
      }
    })
  )
  .medsl_env[[key]] = env
  env
}

#' The model environment for the counties in `cells`
#'
#' All of the state's counties map to the statewide cached set-up. Also run before forking,
#' through `MODEL_SETUP` (run.R).
#'
#' @param cells Cells the fit will use.
#' @param state Two-letter state code.
#' @param dir_26 Path to 26_general.
#' @param demo Demographic spec.
#' @return The environment (`medsl_setup()`).
medsl_setup_for = function(cells, state, dir_26 = find_26_general(), demo = "medsl_cov") {
  # A cells frame carrying only `all` means the state is modelled WITHOUT vote method --
  # MN and MI by their mode history, or any race whose feed gives precinct totals only
  # (shape_for_method() collapses the frame in that case). MODES must follow the cells,
  # or the model builds 3M equations against 1 method and nothing joins.
  by_method = !all(as.character(cells$method) == "all")
  key = paste0("counties_", state)
  if (is.null(.medsl_env[[key]])) {
    .medsl_env[[key]] =
      unique(
        read_pq(file.path(
          dir_26,
          "input_data",
          "model_inputs",
          paste0(state, "_precinct.parquet")
        ))$county_name
      )
  }
  ctys = unique(cells$county)
  medsl_setup(
    state,
    dir_26,
    counties = if (setequal(ctys, .medsl_env[[key]])) NULL else ctys,
    demo = demo,
    by_method = by_method
  )
}

#' Fit medsl_sur to one race
#'
#' Precinct x mode returns go to `fit_stage_hybrid()`; votes on non-precinct geos become
#' county x mode observations (`bucket_counts()`) that bound each county's turnout from
#' below. A fit that is entirely its prior returns the same columns as a real one, so the
#' status says so: "prior_only: ..." (no predictions are published, `fit_ok()`) or
#' "ok (partial prior: ...)".
#'
#' @param cells Cells from `build_cells()` (used for the counts and counted totals, so they
#'   match the other models exactly).
#' @param returns One race's standard returns.
#' @param level Interval level.
#' @param seed Seed set before the draws.
#' @param dir_26 Path to 26_general.
#' @param cbs_county CBS's county snapshot (`read_cbs_latest()`) for model_hybrid.R's
#'   county-constraint arm; NULL (the sims) leaves that arm inert.
#' @param demo Demographic spec (`medsl_spec()`).
#' @param interval "credible", "conf_seq" or "conformal" (results.R).
#' @return `model_result()` list, plus `prior_only` (fit_stage()'s ledger of equations held
#'   at their prior).
fit_medsl_sur <- function(
  cells,
  returns,
  level = 0.95,
  seed = 2026,
  dir_26 = find_26_general(),
  cbs_county = NULL,
  demo = "medsl_cov",
  interval = "credible"
) {
  state <- unique(returns$state)
  if (length(state) != 1) {
    stop("fit_medsl_sur() takes one race (one state) at a time")
  }
  name <- "medsl_sur"
  # fit the counties in `cells` (the ones the feed covers; see feed_cells())
  env <- medsl_setup_for(cells, state, dir_26, demo)
  counts <- cell_counts(cells, returns)

  counted <- race_counted(cells, counts)
  settings <- sprintf("%s; %g%% interval", env$.settings, 100 * level)
  if (!any(counts$c_tot > 0)) {
    return(model_result(name, "no_votes", settings, counted, interval = interval, level = level))
  }

  # the model's returns: one row per precinct x mode, reported once it has votes, complete
  # once that method is counted out
  rep_cell <- counts$c_tot > 0
  ret_m <- counts |>
    transmute(
      geo,
      mode = method,
      stage = case_when(rep_cell & complete ~ 6L, rep_cell ~ 4L, TRUE ~ 0L),
      complete = rep_cell & complete,
      votes_dem = if_else(rep_cell, c_dem, NA_real_),
      votes_rep = if_else(rep_cell, c_rep, NA_real_),
      votes_oth = if_else(rep_cell, c_oth, NA_real_)
    )
  # memory grows with precincts x draws: cap the draws for very large states (CA) so the
  # fit stays near TX's size (6,134 precincts x 500 draws)
  max_pd <- as.numeric(Sys.getenv("MEDSL_MAX_PRECINCT_DRAWS", unset = "3.5e6"))
  draws <- min(env$.draws, max(50L, floor(max_pd / nrow(env$prec))))
  if (draws < env$.draws) {
    settings <- paste0(
      settings,
      sprintf(
        "; draws cut to %d for %d precincts (MEDSL_MAX_PRECINCT_DRAWS)",
        draws,
        nrow(env$prec)
      )
    )
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # ---- the share prior's 2026 level, from priors.csv (see share_prior_shift) ----------
  # Applied to a COPY of env$cells, never the cached environment: one env serves every race
  # in the state and this shift is per-race.
  # BEFORE fit_stage_hybrid, not as an off$share contributor. estimate_share() computes
  # lr = logit(s_obs) - logit(prior_s), so shifting the prior first makes the data-driven arm
  # measure its residual against the CORRECTED prior -- it absorbs what is left after the
  # forecast, which is what a hierarchy should do. Adding to off$share instead would have both
  # arms measuring from the uncorrected 2024 prior and then summing, double-counting the one
  # level move between them.
  cells_fit <- env$cells
  sp <- share_prior_shift(state, unique(returns$race_id)[1], dir_26)
  if (!is.null(sp)) {
    # prior_s is in (0, 1) for every cell -- mode_forbid zeroes mode_turn_share, not the
    # share -- but the guard keeps the shift from inventing a share for a degenerate cell.
    ok <- cells_fit$prior_s > 0 & cells_fit$prior_s < 1
    # ---- the swing is an AGGREGATE movement, so solve for the per-cell shift ----------
    # share_prior_shift() returns logit(T_state) - logit(P_state): a movement of the
    # STATEWIDE SHARE on the logit scale, which is also what calibrate_swing.R measured
    # ("the statewide two-party Dem share MOVES 0.22-0.26 logit between a prior contest and
    # its target"). Adding it to every cell's logit does NOT move the aggregate by that
    # amount -- logit(weighted.mean(p)) != weighted.mean(logit(p)), and the dispersion of
    # prior_s across cells attenuates it. Measured on MN: asking +0.0559 per cell moves the
    # tau-weighted aggregate +0.0480, 86% of it. So a naive common shift quietly
    # under-delivers the published swing by the Jensen gap.
    # Solve instead for the delta whose AGGREGATE effect is the swing. Same shape as
    # SCRAPED_TURNOUT's uniroot over a common logit shift, and as enightmodels::logit_shift.
    #
    # NOTE what is deliberately NOT done: forcing the frame's prior share TO T_state. The
    # frame is a county subset and its share SHOULD differ from the state's -- OH's 15
    # counties sit 9.5pp more Dem than Ohio. Moving the frame's own aggregate BY the
    # statewide swing is scope-correct; pinning it to the statewide LEVEL is the error this
    # whole design exists to avoid.
    w <- cells_fit$prior_tau[ok]
    s0 <- cells_fit$prior_s[ok]
    agg <- function(d) stats::weighted.mean(plogis(qlogis(s0) + d), w)
    lg <- function(p) qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
    want <- lg(agg(0)) + sp$shift
    delta <- tryCatch(
      stats::uniroot(function(d) lg(agg(d)) - want, c(-10, 10), tol = 1e-10)$root,
      error = function(e) NA_real_
    )
    if (!is.finite(delta)) {
      # unreachable aggregate: fall back to the common shift rather than dropping the arm,
      # and say so -- this should not happen for any swing inside +/-10 logit
      cli::cli_alert_warning(
        "share prior: could not solve for the aggregate swing; using the common shift."
      )
      delta <- sp$shift
    }
    raw <- delta
    # ---- the bound, and why it is NOT SH_MAX_SHIFT ------------------------------------
    # SH_MAX_SHIFT (0.25) bounds model_hybrid's DATA-DRIVEN share offset, and its stated
    # justification is that "0.25 logit is ~6pp at 50-50, which is already a very large
    # statewide share SURPRISE". That is a bound on a RESIDUAL -- how far tonight's returns
    # may drag the prior. priors.csv's swing is not a surprise, it is the EXPECTED movement,
    # and calibrate_swing.R puts the sd of exactly that movement at 0.22-0.26 logit. Bounding
    # an expected move at one sd of itself withholds the forecast in the ordinary case:
    # measured 2026-11-03, the solved delta binds at 0.25 in NC, GA, AK, OH and TX -- 5 of 6
    # states -- delivering 0.19-0.23 of an asked 0.27-0.35.
    # So this arm gets its own bound, as the bucket arm does (BK_MAX_SHIFT, wider because a
    # bucket is counted votes). 0.6 is ~2.4x the measured swing sd and ~14pp at 50-50: it
    # cannot bind on an ordinary swing, and a forecast past it is a data error worth seeing.
    lim <- as.numeric(Sys.getenv("SHARE_PRIOR_MAX_SHIFT", unset = "0.6"))
    delta <- max(min(raw, lim), -lim)
    cells_fit$prior_s[ok] <- plogis(qlogis(s0) + delta)
    cells_fit$y_s_prior[ok] <- qlogis(cells_fit$prior_s[ok])
    got <- lg(stats::weighted.mean(cells_fit$prior_s[ok], w)) - lg(agg(0))
    sp$shift_applied <- delta
    sp$agg_moved <- got
    # the override path has no target or baseline to name, so it says only the swing
    from_txt <- if (is.finite(sp$t_state)) {
      sprintf("target %.4f vs 2024 baseline %.4f = ", sp$t_state, sp$p_state)
    } else {
      ""
    }
    cli::cli_alert_info(paste0(
      "share prior from {sp$src}: {from_txt}{sprintf('%+.4f', sp$shift)} logit swing; ",
      "applied {sprintf('%+.4f', delta)} per cell, aggregate moved ",
      "{sprintf('%+.4f', got)} ({sum(ok)} of {nrow(cells_fit)} cells, ",
      "prior share {sprintf('%.4f', agg(0))} -> ",
      "{sprintf('%.4f', stats::weighted.mean(cells_fit$prior_s[ok], w))})"
    ))
    # ---- the turnout level this swing was solved against ------------------------------
    # `w` above is prior_tau, so the delta is the root of a TAU-WEIGHTED aggregate: the
    # weights are each cell's expected turnout. CBS_LEVEL_PRIOR (model_turnout.R) is what
    # puts 2026 into those weights, and it does so ONLY under CBS_LEVEL_SCOPE="both" --
    # model_hybrid's recentre_prior() reads
    #   prior_tau = (if (CBS_LEVEL_SCOPE == "both") tgt_tot_adj else est_tot_adj) * mode_turn_share
    # so scope="theta" leaves prior_tau on the 2022 level even with the level arm on.
    #
    # Without it the swing is still DELIVERED (the solve hits its target), but it is placed
    # using 2022 county weights, and CBS's growth is not uniform: TX asks 1.3644 over its 25
    # fitted counties and 1.079-1.662 BY COUNTY. County turnout growth is normally
    # partisan-correlated, so the misweighting is a bias, not noise.
    #
    # This is a WARNING, not a dependency: the two arms stay independently runnable, so each
    # can still be ablated, and the share arm remains the right fallback when the CBS feed is
    # short a county (cbs_level_prior_var() degrades to "level prior stays on 2022" exactly
    # then). What it buys is that the confound cannot be entered silently -- read a
    # CBS_SHARE_PRIOR verdict off `both` vs `level-only`, never off `share-only` vs nothing.
    # See quality_reports/switch_taxonomy.md, "I. Wrong-year prior levels".
    lvl_on <- Sys.getenv("CBS_LEVEL_PRIOR", unset = "0") == "1" ||
      nzchar(Sys.getenv("CBS_COUNTY_EXPECTED"))
    lvl_scope <- Sys.getenv("CBS_LEVEL_SCOPE", unset = "both")
    if (!lvl_on || lvl_scope != "both") {
      why <- if (!lvl_on) {
        "CBS_LEVEL_PRIOR is off"
      } else {
        sprintf('CBS_LEVEL_SCOPE is "%s", not "both", so prior_tau keeps the 2022 level', lvl_scope)
      }
      cli::cli_alert_warning(paste0(
        "share prior solved against 2022 turnout weights ({why}). The swing is delivered ",
        "but distributed on stale county weights; CBS growth is 1.079-1.662 by county in TX. ",
        "Judge this arm against a CBS_LEVEL_PRIOR=1 baseline, not against an all-off one."
      ))
    }
    if (abs(raw - delta) > 1e-9) {
      # a clamp here is not housekeeping: SHARE_PRIOR_MAX_SHIFT is ~2.4x the measured
      # statewide swing sd, so binding means priors.csv is asking for a swing far past
      # anything calibrate_swing.R saw and the model is NOT delivering the stated one
      cli::cli_alert_warning(paste0(
        "share prior clamped: {sprintf('%+.4f', raw)} -> {sprintf('%+.4f', delta)} logit ",
        "(SHARE_PRIOR_MAX_SHIFT {lim}, ~2.4x the measured swing sd). The swing is NOT ",
        "delivered -- at this size suspect priors.csv or the 2024 baseline, not the clamp."
      ))
    }
    settings <- paste0(
      settings,
      sprintf(
        "; share prior %+.4f logit swing from %s (%s), applied %+.4f per cell, aggregate %+.4f%s",
        sp$shift,
        sp$src,
        if (is.finite(sp$t_state)) {
          sprintf("target %.4f, 2024 baseline %.4f", sp$t_state, sp$p_state)
        } else {
          "explicit override"
        },
        delta,
        sp$agg_moved,
        if (abs(raw - delta) > 1e-9) sprintf(", clamped from %+.4f", raw) else ""
      )
    )
  }
  # ---- county x mode observations for the votes that land on NO precinct ------------
  # cell_counts() joins FROM `cells`, so AK's house-district mail/early/provisional piles
  # and NC's county mail and one-stop-site piles -- 31% of each state's vote -- are dropped
  # before the model sees them. bucket_counts() turns exactly those rows into one (county,
  # mode) observation each, and fit_stage()'s CTY_BOUND uses the county totals as a LOWER
  # BOUND on the county's drawn turnout. CA, FL, MI and TX have no bucket votes, so there
  # it returns 0 rows and the bound is inert.
  # The `known` precinct set is the UNION of the feed's cells and the model's: a geo that is
  # a real precinct in either must never be read as a county pile (NC has precincts actually
  # named `PROVI` and `EV`), and the two frames can differ when the feed covers a subset of
  # counties.
  known_cells <- tibble(geo = union(as.character(cells$geo), as.character(env$cells$geo)))
  cty_obs <- bucket_counts(known_cells, returns)
  report_buckets(cty_obs, cells = known_cells, label = unique(returns$race_id)[1])
  f <- suppressMessages(
    env$fit_stage_hybrid(
      cells_fit,
      ret_m,
      draws = draws,
      cross_mode = env$USE_CROSS_MODE,
      county_obs = cty_obs,
      cbs_county = cbs_county
    )
  )
  # fit_stage() is run under suppressMessages (the model is chatty), which would also eat
  # the county bound's per-county log -- and that log is the only way to tell a bound that
  # binds on real votes from one that binds on an id join error. So it is re-emitted here
  # from the returned frame.
  if (!is.null(f$cty_bound) && nrow(f$cty_bound)) {
    cb <- f$cty_bound
    cli::cli_alert_info(paste0(
      "county turnout bound: binds in {nrow(cb)} county(ies), raising the county total by ",
      "{prettyNum(round(sum(cb$mean_short)), big.mark = ',')} votes in total ",
      "(max {prettyNum(round(max(cb$max_short)), big.mark = ',')} in one county)"
    ))
    for (i in seq_len(nrow(cb))) {
      cli::cli_alert_info(paste0(
        "  county {cb$county[i]}: +{round(cb$mean_short[i])} mean on ",
        "{cb$draws_bound[i]} draw{?s}, bucket {prettyNum(round(cb$bucket[i]), big.mark = ',')}, ",
        "counted {prettyNum(round(cb$cnt_prec[i]), big.mark = ',')}"
      ))
    }
  }

  dr <- tibble(twop = f$S_2p, turnout = f$turn, dem = f$S_dem * f$turn, rep = f$S_rep * f$turn)
  # ---- THE SILENT-PRIOR ALARM ------------------------------------------------------
  # A fit that is entirely its own prior returns the same columns and the same intervals as
  # a real one. Nothing in the output distinguishes them, so on the night a state whose feed
  # does not join would publish a row labelled `medsl_sur` that is just the 2024 baseline
  # echoed back. MI is the live case: its geos join `cells` at 0.6% and its reporting
  # counties send `method = "all"` for which there is no cell, so it reaches ~0 observed
  # cells and every equation falls back to prior_loc.
  #
  # fit_stage() counts the three degradation paths (whole-fit below 30 observed cells, a
  # single equation below MIN_FIT_OBS, a mode held at prior by the support gate) and returns
  # them in `prior_only`, because its own cli messages are eaten by the suppressMessages()
  # above. Re-emitted here, and -- the part that matters -- carried in `status`, so it
  # reaches the status table and the dashboard rather than only the log.
  po <- f$prior_only
  st <- "ok"
  if (!is.null(po)) {
    if (isTRUE(po$all_prior)) {
      st <- sprintf(
        "prior_only: %d of %d equations at prior, %d observed cells of %d",
        po$eq_prior,
        po$eq_total,
        po$n_obs_cells,
        po$n_cells
      )
      cli::cli_alert_danger(paste0(
        "{state}: THIS FIT IS ITS PRIOR. {po$n_obs_cells} observed cell(s) of ",
        "{po$n_cells}; {po$eq_prior} of {po$eq_total} equations at prior. The numbers ",
        "below carry no information from tonight -- do not read them as a forecast."
      ))
    } else if (po$eq_prior > 0 || length(po$gated_modes)) {
      st <- sprintf(
        "ok (partial prior: %d of %d equations%s)",
        po$eq_prior,
        po$eq_total,
        if (length(po$gated_modes)) {
          paste0("; modes gated: ", paste(po$gated_modes, collapse = "/"))
        } else {
          ""
        }
      )
      cli::cli_alert_warning(paste0(
        "{state}: {po$eq_prior} of {po$eq_total} equations are at their prior",
        if (length(po$gated_modes)) {
          " and mode(s) {.val {po$gated_modes}} are held at prior by the support gate"
        } else {
          ""
        },
        " -- partial fit."
      ))
    }
  }
  # One row per precinct x mode of the model's frame, with what is counted. Built for every
  # arm, not just conformal: fit_stage() already computes it (`cell_votes`, filled in the
  # prediction loop), and it is the ONLY per-precinct x mode prediction the model exposes --
  # `cells` is collapsed to precinct grain (4032bc19) so its `turn_est` is the precinct
  # total repeated and its `method` is NA. Needed to judge whether a precinct's mode
  # composition is reasonable, which is the standard the composition prior has to meet.
  cell_pred <- local({
    cv <- f$cell_votes
    if (is.null(cv) || is.null(cv$fin_dem)) {
      return(NULL)
    }
    tibble(
      geo = rep(as.character(env$prec$geo), ncol(cv$fin_dem)),
      method = rep(colnames(cv$fin_dem), each = nrow(cv$fin_dem)),
      !!!lapply(cv, as.vector)
    ) |>
      left_join(
        transmute(
          counts,
          geo = as.character(geo),
          method = as.character(method),
          c_dem,
          c_rep,
          c_oth,
          c_tot
        ),
        by = c("geo", "method")
      ) |>
      mutate(
        across(c(c_dem, c_rep, c_oth, c_tot), \(x) coalesce(x, 0)),
        across(starts_with("fin_") | starts_with("fit_"), \(x) coalesce(x, 0))
      ) |>
      mutate(pred = fin_dem + fin_rep + fin_oth)
  })
  fc <- if (interval == "conformal") cell_pred
  # conf_seq: prior predictive draws of the race totals, nothing counted (cells_fit holds
  # the model's own priors; fit_stage_hybrid() re-centres a copy).
  # cells_fit, not env$cells: priors.csv's level is EXOGENOUS -- known before a vote is
  # counted -- so it is part of the model's own prior, not a re-centring on tonight's
  # returns. A reference measure that has seen the data is not a reference measure; one that
  # has seen a published forecast still is. (model_hybrid's arms, which DO read the returns,
  # are applied inside fit_stage_hybrid and correctly never reach here.)
  prior <- if (interval == "conf_seq") env$prior_totals(cells_fit, draws = draws)
  res <- draws_result(
    name,
    interval,
    settings,
    counted,
    dr,
    level,
    returns,
    prior = prior,
    fc = fc,
    cells = prec_rows(f$prec),
    # by mode; conf_seq's prior draws are race totals only (prior_totals()), so its modes
    # are points
    mode_dr = f$turn_mode
  )
  res$status <- res$race$status <- st
  # the structured ledger as well as the status string, so a consumer can test
  # `fit$prior_only$all_prior` rather than parse prose
  res$prior_only <- po
  # per-precinct x mode predictions, for the precinct-level sanity check
  # (tests/check_precinct_sanity.R). Attached here rather than threaded through
  # draws_result() so no other model's result shape changes.
  res$cell_pred <- cell_pred
  res
}
