# ========================================
## model_medsl.R -- medsl_sur: Young-Kyung's precinct x vote-method model
## (scripts/model/model_turnout.R, with model_hybrid.R's re-centred priors; fit_medsl_sur()),
## run on any race in the live format. fit_medsl() alone, the plain model_turnout.R model
## (formerly model_medsl), is no longer run on the night but is what fit_medsl_sur() calls.
##
## model_turnout.R is NOT copied or edited here. Its own code runs, from its own file, each
## time the models are loaded for a state, so a change Young-Kyung makes there is picked
## up with no edit to live/:
##   - every top-level statement of its sections 1-2 runs (config, functions, `prec`, mode
##     priors, `cells`, CTY_MAT), and sections 3-5 contribute only their functions
##     (fit_stage(), prior_draws(), ...). Its synthetic run (section 6) never runs.
##   - its two file reads (`covs_long`, `mode_hist`) are replaced by frames built here from
##     the state's model inputs, {ST}_precinct.parquet and {ST}_precinct_mode_history.parquet
##     (prep_model_inputs.R); the column mapping is below.
##   - it runs in its own environment, one per state, so its `cells`, `prec` etc. never
##     collide with live/'s objects or with another state's.
##   - MODE_OFFSET_SOURCE defaults to "zero" (our mode history has turnout by method but
##     not by party). Any of the model's own switches can still be
##     set as environment variables before loading (e.g. MODE_OFFSET_SOURCE=nc,
##     USE_COUNTY_RE=0, SIGMA_STRUCT=diag).
##
## Inputs and output are the same as the other models (see run.R):
##   fit_medsl(cells, returns)   cells from build_cells(), one race's standard returns
## A cell (precinct x method) counts as reported once it has any votes (its shares are used
## at once) and as complete once that method is fully counted (`complete` in the returns;
## its turnout is used only then).
## Interval: the model's posterior draws, mean +/- z * sd as for the other models. The
## number of draws is model_turnout.R's DRAWS (500) unless MEDSL_DRAWS is set, e.g.
## MEDSL_DRAWS=250 in ~/.Renviron. Speed (TX, one race): about 6 s of fixed work plus
## about 1 s per 100 draws, and the 95% interval moves by under 0.05 points between 250
## and 500 draws. Memory grows with draws x precincts: CA (23,685 precincts) ran out of
## 6.5 GB at 500 draws and took 29 s at 200.
##
## Inputs the model needs that a state's file lacks: YouGov race predictions
## (yg_pred_bla, yg_pred_his) are blank for some precincts in MI and CA; those are filled
## with the county mean (state mean if the whole county is blank), and the fill is noted
## in `settings`. Any other blank required column stops the fit with the column names.
# ========================================

MEDSL_SRC = "scripts/model/model_turnout.R"
HYBRID_SRC = "scripts/model/model_hybrid.R"
.medsl_env = new.env()   # cache: one model environment per state

# model_turnout.R's input frame (`covs_long`, long by party) and mode history, from our
# model inputs. Column mapping:
#   county       <- county_name              est (dem/rep) <- est_turnout x dem2 / (1 - dem2)
#   turnout22/24 <- L2 turnout22/24 (rates)  age           <- p_age_65_up (share 65+)
#   male         <- p_male                   pred.bla/his  <- yg_pred_bla / yg_pred_his
#   tp_raw       <- oth_base (third-party share; the model clamps it to [0, 0.25])
medsl_inputs = function(state, dir_26 = find_26_general(), mode_election = NULL) {
  dir_in = file.path(dir_26, "input_data", "model_inputs")
  inp = read_pq(file.path(dir_in, paste0(state, "_precinct.parquet")))
  mh = mode_history(state, dir_26, mode_election)
  covs_wide = inp |>
    transmute(geo = as.character(geo), county = county_name, geostratum, stratum_demo, n, dem2,
              turnout22, turnout24, age = p_age_65_up, male = p_male,
              `pred.bla` = yg_pred_bla, `pred.his` = yg_pred_his, tp_raw = oth_base,
              est_turnout)
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
  if (length(bad)) stop(state, "_precinct.parquet has blank values in: ", paste(bad, collapse = ", "),
                        ". Rerun prep_model_inputs.R ", state, ".")
  covs_long = bind_rows(
    mutate(covs_wide, party = "dem", est = est_turnout * dem2),
    mutate(covs_wide, party = "rep", est = est_turnout * (1 - dem2))) |>
    select(-est_turnout) |>
    mutate(method = "all")
  # mode history in the model's schema (geo, method, party, votes). Only the turnout split
  # is real; the party split just follows dem2 and carries no mode lean.
  mode_hist = mh |>
    mutate(geo = as.character(geo)) |>
    inner_join(select(covs_wide, geo, dem2), by = "geo") |>
    reframe(party = c("dem", "rep"), votes = voters * c(dem2, 1 - dem2), .by = c(geo, method))
  list(covs_long = covs_long, mode_hist = mode_hist,
       note = if (length(filled)) paste0("filled with county means: ", paste(filled, collapse = "; ")) else "")
}

# Run model_turnout.R's sections 1-2 and its function definitions for one state, in a fresh
# environment, and return that environment. Cached per state (and county set) for the session.
# counties: fit only these counties' precincts (county names as in cells$county) -- the
#   counties the live feed actually covers, so the model's totals never include counties
#   whose votes come from CBS instead. NULL = every county in the state's inputs.
# hybrid: also load model_hybrid.R into the environment (fit_stage_hybrid(), see fit_medsl_sur())
medsl_key = function(state, counties, hybrid = FALSE)
  paste0(if (is.null(counties)) state else paste(state, paste(sort(unique(counties)), collapse = "|")),
         if (hybrid) "|hybrid")
medsl_setup = function(state, dir_26 = find_26_general(), src = MEDSL_SRC, refresh = FALSE, counties = NULL,
                       hybrid = FALSE) {
  key = medsl_key(state, counties, hybrid)
  if (!refresh && !is.null(.medsl_env[[key]])) return(.medsl_env[[key]])
  if (!file.exists(src)) stop("Can't find ", src, " (run from the cbs-enr folder)")
  inp = medsl_inputs(state, dir_26)
  if (!is.null(counties)) {
    inp$covs_long = filter(inp$covs_long, county %in% counties)
    inp$mode_hist = filter(inp$mode_hist, geo %in% inp$covs_long$geo)
    if (!nrow(inp$covs_long)) stop("none of the counties given are in ", state, "_precinct.parquet")
  }

  # the model's own default for mode offsets is its party-by-mode history, which we don't
  # have; use zero unless the person running it has chosen otherwise
  if (!nzchar(Sys.getenv("MODE_OFFSET_SOURCE"))) {
    Sys.setenv(MODE_OFFSET_SOURCE = "zero"); on.exit(Sys.unsetenv("MODE_OFFSET_SOURCE"), add = TRUE)
  }

  txt = readLines(src, warn = FALSE)
  cut_ret = grep("^# ---- 3\\. synthetic returns", txt)[1]
  cut_run = grep("^# ---- 6\\. run", txt)[1]
  if (is.na(cut_ret) || is.na(cut_run))
    stop("model_turnout.R's section markers ('# ---- 3. synthetic returns', '# ---- 6. run') changed; update live/R/model_medsl.R")
  ex = parse(src, keep.source = TRUE); sr = attr(ex, "srcref")
  lhs = function(e) if (is.call(e) && as.character(e[[1]]) %in% c("=", "<-") && is.name(e[[2]])) as.character(e[[2]]) else ""
  is_fun = function(e) lhs(e) != "" && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
  is_source = function(e) is.call(e) && identical(e[[1]], as.name("source"))

  env = new.env(parent = globalenv())
  assign("covs_long", inp$covs_long, envir = env)
  assign("mode_hist", inp$mode_hist, envir = env)
  # model_turnout.R hardcodes STATE = "TX" (its own default driver is the TX trial), and
  # nothing here used to override it -- so every state ran with STATE == "TX". Harmless
  # while STATE only fed unread file paths and a cli header, but anything keyed on it (the
  # per-state REF_MODE / MODES lookup) would silently read TX for all six states. Inject it
  # and skip the model's own assignment, exactly as covs_long / mode_hist are handled.
  assign("STATE", state, envir = env)
  skipped = character()
  suppressMessages(for (i in seq_along(ex)) {
    e = ex[[i]]; line = sr[[i]][1]
    if (lhs(e) %in% c("covs_long", "mode_hist", "STATE")) { skipped = c(skipped, lhs(e)); next }
    if (!(line < cut_ret || (line < cut_run && is_fun(e)))) next
    # source(model_core.R) goes into the model's environment, not the global one
    if (is_source(e)) e[["local"]] = env
    eval(e, envir = env)
  })
  if (!setequal(skipped, c("covs_long", "mode_hist", "STATE")))
    stop("model_turnout.R no longer assigns covs_long / mode_hist / STATE at top level; update live/R/model_medsl.R")
  if (!exists("fit_stage", envir = env, inherits = FALSE)) stop("model_turnout.R no longer defines fit_stage()")

  env$.draws = as.integer(Sys.getenv("MEDSL_DRAWS", unset = env$DRAWS))
  env$.settings = with(env, sprintf(paste0(
    "model_turnout.R (precinct x method: turnout, third-party and two-party equations); %d posterior draws; ",
    "bounded turnout %s (KAPPA %.2f, ref %s); Sigma %s/%s; county RE %s; mode offsets %s; gates %d cells / %d counties"),
    .draws, USE_COMPOSE, KAPPA, REF_MODE, SIGMA_SOURCE, SIGMA_STRUCT, USE_COUNTY_RE,
    MODE_OFFSET_SOURCE, MIN_MODE_CELLS, MIN_MODE_CTY))
  if (nzchar(inp$note)) env$.settings = paste0(env$.settings, "; ", inp$note)
  if (hybrid) {
    suppressMessages(sys.source(HYBRID_SRC, envir = env))
    env$.settings = with(env, sprintf("model_hybrid.R (arms: level %s, composition %s, share %s, county %s) on %s",
                                      HY_LEVEL, HY_COMP, HY_SHARE, HY_COUNTY, .settings))
  }
  .medsl_env[[key]] = env
  env
}

# the model environment for the counties in `cells` (all of the state's counties -> the
# state-wide cached set-up)
fit_medsl_setup_for = function(cells, returns, dir_26 = find_26_general(), hybrid = FALSE) {
  state = unique(returns$state)
  if (length(state) != 1) stop("fit_medsl() takes one race (one state) at a time")
  key = paste0("counties_", state)
  if (is.null(.medsl_env[[key]])) .medsl_env[[key]] =
    unique(read_pq(file.path(dir_26, "input_data", "model_inputs", paste0(state, "_precinct.parquet")))$county_name)
  ctys = unique(cells$county)
  medsl_setup(state, dir_26, counties = if (setequal(ctys, .medsl_env[[key]])) NULL else ctys, hybrid = hybrid)
}

# cells:   build_cells() output (used for the precinct x method counts and the counted
#          totals, so they match the other models exactly)
# returns: one race's standard returns (returns.R)
# hybrid / cbs_county: see fit_medsl_sur()
fit_medsl = function(cells, returns, level = 0.95, seed = 2026, dir_26 = find_26_general(),
                     hybrid = FALSE, cbs_county = NULL) {
  state = unique(returns$state)
  if (length(state) != 1) stop("fit_medsl() takes one race (one state) at a time")
  name = if (hybrid) "medsl_sur" else "medsl"
  # fit the counties in `cells` (the ones the feed covers; see feed_cells())
  env = fit_medsl_setup_for(cells, returns, dir_26, hybrid)
  counts = cell_counts(cells, returns)

  counted = tibble(raw_twop = sum(counts$c_dem) / max(sum(counts$c_dem + counts$c_rep), 1),
                   counted_votes = sum(counts$c_tot), expected_votes = sum(cells$exp_votes),
                   obs_cells = sum(counts$c_tot > 0), complete_cells = sum(counts$c_tot > 0 & counts$complete))
  settings = sprintf("%s; %g%% interval", env$.settings, 100 * level)
  if (!any(counts$c_tot > 0)) {
    return(list(model = name, status = "no_votes", settings = settings, conf = sprintf("posterior_%g", 100 * level),
                race = bind_cols(tibble(status = "no_votes", est_twop = NA_real_, lo_twop = NA_real_, hi_twop = NA_real_,
                                        p_dem_win = NA_real_, est_turnout = NA_real_, lo_turnout = NA_real_,
                                        hi_turnout = NA_real_), counted),
                cells = NULL, draws = NULL, point = NULL))
  }

  # the model's returns: one row per precinct x mode, reported once it has votes, complete
  # once that method is counted out
  rep_cell = counts$c_tot > 0
  ret_m = counts |>
    transmute(geo, mode = method,
              stage = case_when(rep_cell & complete ~ 6L, rep_cell ~ 4L, TRUE ~ 0L),
              complete = rep_cell & complete,
              votes_dem = if_else(rep_cell, c_dem, NA_real_),
              votes_rep = if_else(rep_cell, c_rep, NA_real_),
              votes_oth = if_else(rep_cell, c_oth, NA_real_))
  # memory grows with precincts x draws: cap the draws for very large states (CA) so the
  # fit stays near TX's size (6,134 precincts x 500 draws)
  max_pd = as.numeric(Sys.getenv("MEDSL_MAX_PRECINCT_DRAWS", unset = "3.5e6"))
  draws = min(env$.draws, max(50L, floor(max_pd / nrow(env$prec))))
  if (draws < env$.draws) settings = paste0(settings, sprintf("; draws cut to %d for %d precincts (MEDSL_MAX_PRECINCT_DRAWS)", draws, nrow(env$prec)))
  if (!is.null(seed)) set.seed(seed)
  # ---- county x mode observations for the votes that land on NO precinct ------------
  # cell_counts() joins FROM `cells`, so AK's house-district mail/early/provisional piles
  # and NC's county mail and one-stop-site piles -- 31% of each state's vote -- are dropped
  # before the model sees them. bucket_counts() turns exactly those rows into one (county,
  # mode) observation each, and fit_stage()'s CTY_BOUND uses the county totals as a LOWER
  # BOUND on the county's drawn turnout. This is the only wiring that makes it reach air.
  # CA, FL, MI and TX have zero bucket vote mass, so there it returns 0 rows and the bound
  # is inert by construction, not by a state test.
  # The `known` precinct set is the UNION of the feed's cells and the model's: a geo that is
  # a real precinct in either must never be read as a county pile (NC has precincts actually
  # named `PROVI` and `EV`), and the two frames can differ when the feed covers a subset of
  # counties.
  known_cells = tibble(geo = union(as.character(cells$geo), as.character(env$cells$geo)))
  cty_obs = bucket_counts(known_cells, returns)
  report_buckets(cty_obs, cells = known_cells, label = unique(returns$race_id)[1])
  f = suppressMessages(if (hybrid)
    env$fit_stage_hybrid(env$cells, ret_m, draws = draws, cross_mode = env$USE_CROSS_MODE,
                         county_obs = cty_obs, cbs_county = cbs_county)
  else env$fit_stage(env$cells, ret_m, draws = draws, cross_mode = env$USE_CROSS_MODE, county_obs = cty_obs))
  # fit_stage() is run under suppressMessages (the model is chatty), which would also eat
  # the county bound's per-county log -- and that log is the only way to tell a bound that
  # binds on real votes from one that binds on an id join error. So it is re-emitted here
  # from the returned frame.
  if (!is.null(f$cty_bound) && nrow(f$cty_bound)) {
    cb = f$cty_bound
    cli::cli_alert_info(paste0(
      "county turnout bound: binds in {nrow(cb)} county(ies), raising the county total by ",
      "{prettyNum(round(sum(cb$mean_short)), big.mark = ',')} votes in total ",
      "(max {prettyNum(round(max(cb$max_short)), big.mark = ',')} in one county)"))
    for (i in seq_len(nrow(cb))) cli::cli_alert_info(paste0(
      "  county {cb$county[i]}: +{round(cb$mean_short[i])} mean on ",
      "{cb$draws_bound[i]} draw{?s}, bucket {prettyNum(round(cb$bucket[i]), big.mark = ',')}, ",
      "counted {prettyNum(round(cb$cnt_prec[i]), big.mark = ',')}"))
  }

  dr = tibble(twop = f$S_2p, turnout = f$turn, dem = f$S_dem * f$turn, rep = f$S_rep * f$turn)
  ci = cred(dr$twop, level); ct = cred(dr$turnout, level)
  # ---- THE SILENT-PRIOR ALARM ------------------------------------------------------
  # A fit that is entirely its own prior returns the same columns and the same intervals as
  # a real one. Nothing in the output distinguishes them, so on the night a state whose feed
  # does not join would publish a row labelled `model_medsl` that is just the 2024 baseline
  # echoed back. MI is the live case: its geos join `cells` at 0.6% and its reporting
  # counties send `method = "all"` for which there is no cell, so it reaches ~0 observed
  # cells and every equation falls back to prior_loc.
  #
  # fit_stage() counts the three degradation paths (whole-fit below 30 observed cells, a
  # single equation below MIN_FIT_OBS, a mode held at prior by the support gate) and returns
  # them in `prior_only`, because its own cli messages are eaten by the suppressMessages()
  # above. Re-emitted here, and -- the part that matters -- carried in `status`, so it
  # reaches the status table and the dashboard rather than only the log.
  po = f$prior_only
  st = "ok"
  if (!is.null(po)) {
    if (isTRUE(po$all_prior)) {
      st = sprintf("prior_only: %d of %d equations at prior, %d observed cells of %d",
                   po$eq_prior, po$eq_total, po$n_obs_cells, po$n_cells)
      cli::cli_alert_danger(paste0(
        "{state}: THIS FIT IS ITS PRIOR. {po$n_obs_cells} observed cell(s) of ",
        "{po$n_cells}; {po$eq_prior} of {po$eq_total} equations at prior. The numbers ",
        "below carry no information from tonight -- do not read them as a forecast."))
    } else if (po$eq_prior > 0 || length(po$gated_modes)) {
      st = sprintf("ok (partial prior: %d of %d equations%s)", po$eq_prior, po$eq_total,
                   if (length(po$gated_modes))
                     paste0("; modes gated: ", paste(po$gated_modes, collapse = "/")) else "")
      cli::cli_alert_warning(paste0(
        "{state}: {po$eq_prior} of {po$eq_total} equations are at their prior",
        if (length(po$gated_modes))
          " and mode(s) {.val {po$gated_modes}} are held at prior by the support gate"
        else "", " -- partial fit."))
    }
  }
  list(model = name, status = st, settings = settings, conf = sprintf("posterior_%g", 100 * level),
       # the structured ledger as well as the status string, so a consumer can test
       # `fit$prior_only$all_prior` rather than parse prose
       prior_only = po,
       race = bind_cols(tibble(status = st, est_twop = mean(dr$twop), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]],
                               p_dem_win = mean(dr$twop > 0.5), est_turnout = mean(dr$turnout),
                               lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]]), counted),
       cells = f$prec,                       # precinct: turn_est, dem_est (share of all votes), interval
       draws = dr,
       point = c(dem = mean(dr$dem), rep = mean(dr$rep), tot = mean(dr$turnout)))
}

# medsl_sur: Young-Kyung's scripts/model/model_hybrid.R. The same model as model_medsl, with
# its turnout level, method composition and vote-share PRIORS re-centred from tonight's
# returns (log counted / prior and logit share offsets on county x method fixed effects,
# gated per county x method), and CBS's counted-out county x method totals imposed as exact
# constraints when a snapshot is passed. Arms switch off with HY_LEVEL / HY_COMP / HY_SHARE /
# HY_COUNTY = 0 (all four off reproduces model_medsl). No snapshot (the sims) leaves the
# county arm inert.
fit_medsl_sur = function(cells, returns, ..., cbs_county = NULL)
  fit_medsl(cells, returns, ..., hybrid = TRUE, cbs_county = cbs_county)
