# ========================================
## model_medsl.R -- model_medsl: Young-Kyung's precinct x vote-method model
## (scripts/model/model_turnout.R), run on any race in the live format.
##
## model_turnout.R is NOT copied or edited here. Its own code runs, from its own file, each
## time the models are loaded for a state, so a change Young-Kyung makes there is picked
## up with no edit to live/. This is the same way scripts/model/tests/test_sim_night.R
## (section A, "trial") runs it:
##   - every top-level statement of its sections 1-2 runs (config, functions, `prec`, mode
##     priors, `cells`, CTY_MAT), and sections 3-5 contribute only their functions
##     (fit_stage(), prior_draws(), ...). Its synthetic run (section 6) never runs.
##   - its two file reads (`covs_long`, `mode_hist`) are replaced by frames built here from
##     the state's model inputs, {ST}_precinct.parquet and {ST}_precinct_mode_history.parquet
##     (prep_model_inputs.R), with the same column mapping test_sim_night.R uses.
##   - it runs in its own environment, one per state, so its `cells`, `prec` etc. never
##     collide with live/'s objects or with another state's.
##   - MODE_OFFSET_SOURCE defaults to "zero" (our mode history has turnout by method but
##     not by party), as in test_sim_night.R. Any of the model's own switches can still be
##     set as environment variables before loading (e.g. MODE_OFFSET_SOURCE=nc,
##     USE_COUNTY_RE=0, SIGMA_STRUCT=diag).
##
## Inputs and output are the same as the other models (see run.R):
##   fit_medsl(cells, returns)   cells from build_cells(), one race's standard returns
## A cell (precinct x method) counts as reported once it has any votes (its shares are used
## at once) and as complete once that method is fully counted (`complete` in the returns;
## its turnout is used only then) -- test_sim_night.R's defaults (--cells=any
## --complete=feed).
## Interval: the model's posterior draws (DRAWS, 500 by default), mean +/- z * sd as for
## the other models.
##
## Inputs the model needs that a state's file lacks: YouGov race predictions
## (yg_pred_bla, yg_pred_his) are blank for some precincts in MI and CA; those are filled
## with the county mean (state mean if the whole county is blank), and the fill is noted
## in `settings`. Any other blank required column stops the fit with the column names.
# ========================================

MEDSL_SRC = "scripts/model/model_turnout.R"
.medsl_env = new.env()   # cache: one model environment per state

# model_turnout.R's input frame (`covs_long`, long by party) and mode history, from our
# model inputs. Column mapping (as in test_sim_night.R section A):
#   county       <- county_name              est (dem/rep) <- est_turnout x dem2 / (1 - dem2)
#   turnout22/24 <- L2 turnout22/24 (rates)  age           <- p_age_65_up (share 65+)
#   male         <- p_male                   pred.bla/his  <- yg_pred_bla / yg_pred_his
#   tp_raw       <- oth_base (third-party share; the model clamps it to [0, 0.25])
medsl_inputs = function(state, dir_26 = find_26_general(), mode_election = "2022_general") {
  dir_in = file.path(dir_26, "input_data", "model_inputs")
  inp = read_pq(file.path(dir_in, paste0(state, "_precinct.parquet")))
  mh = read_pq(file.path(dir_in, paste0(state, "_precinct_mode_history.parquet"))) |>
    filter(election == mode_election)
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
# environment, and return that environment. Cached per state for the session.
medsl_setup = function(state, dir_26 = find_26_general(), src = MEDSL_SRC, refresh = FALSE) {
  if (!refresh && !is.null(.medsl_env[[state]])) return(.medsl_env[[state]])
  if (!file.exists(src)) stop("Can't find ", src, " (run from the cbs-enr folder)")
  inp = medsl_inputs(state, dir_26)

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
  skipped = character()
  suppressMessages(for (i in seq_along(ex)) {
    e = ex[[i]]; line = sr[[i]][1]
    if (lhs(e) %in% c("covs_long", "mode_hist")) { skipped = c(skipped, lhs(e)); next }
    if (!(line < cut_ret || (line < cut_run && is_fun(e)))) next
    # source(model_core.R) goes into the model's environment, not the global one
    if (is_source(e)) e[["local"]] = env
    eval(e, envir = env)
  })
  if (!setequal(skipped, c("covs_long", "mode_hist")))
    stop("model_turnout.R no longer assigns covs_long / mode_hist at top level; update live/R/model_medsl.R")
  if (!exists("fit_stage", envir = env, inherits = FALSE)) stop("model_turnout.R no longer defines fit_stage()")

  env$.settings = with(env, sprintf(paste0(
    "model_turnout.R (precinct x method: turnout, third-party and two-party equations); %d posterior draws; ",
    "bounded turnout %s (KAPPA %.2f, ref %s); Sigma %s/%s; county RE %s; mode offsets %s; gates %d cells / %d counties"),
    DRAWS, USE_COMPOSE, KAPPA, REF_MODE, SIGMA_SOURCE, SIGMA_STRUCT, USE_COUNTY_RE,
    MODE_OFFSET_SOURCE, MIN_MODE_CELLS, MIN_MODE_CTY))
  if (nzchar(inp$note)) env$.settings = paste0(env$.settings, "; ", inp$note)
  .medsl_env[[state]] = env
  env
}

# cells:   build_cells() output (used for the precinct x method counts and the counted
#          totals, so they match the other models exactly)
# returns: one race's standard returns (returns.R)
fit_medsl = function(cells, returns, level = 0.95, seed = 2026, dir_26 = find_26_general()) {
  state = unique(returns$state)
  if (length(state) != 1) stop("fit_medsl() takes one race (one state) at a time")
  env = medsl_setup(state, dir_26)
  counts = cell_counts(cells, returns)

  counted = tibble(raw_twop = sum(counts$c_dem) / max(sum(counts$c_dem + counts$c_rep), 1),
                   counted_votes = sum(counts$c_tot), expected_votes = sum(cells$exp_votes),
                   obs_cells = sum(counts$c_tot > 0), complete_cells = sum(counts$c_tot > 0 & counts$complete))
  settings = sprintf("%s; %g%% interval", env$.settings, 100 * level)
  if (!any(counts$c_tot > 0)) {
    return(list(model = "medsl", status = "no_votes", settings = settings,
                race = bind_cols(tibble(status = "no_votes", est_twop = NA_real_, lo_twop = NA_real_, hi_twop = NA_real_,
                                        p_dem_win = NA_real_, est_turnout = NA_real_, lo_turnout = NA_real_,
                                        hi_turnout = NA_real_), counted),
                cells = NULL, draws = NULL, point = NULL))
  }

  # the model's returns: one row per precinct x mode (test_sim_night.R --cells=any
  # --complete=feed): reported once it has votes, complete once that method is counted out
  rep_cell = counts$c_tot > 0
  ret_m = counts |>
    transmute(geo, mode = method,
              stage = case_when(rep_cell & complete ~ 6L, rep_cell ~ 4L, TRUE ~ 0L),
              complete = rep_cell & complete,
              votes_dem = if_else(rep_cell, c_dem, NA_real_),
              votes_rep = if_else(rep_cell, c_rep, NA_real_),
              votes_oth = if_else(rep_cell, c_oth, NA_real_))
  if (!is.null(seed)) set.seed(seed)
  f = suppressMessages(env$fit_stage(env$cells, ret_m, cross_mode = env$USE_CROSS_MODE))

  dr = tibble(twop = f$S_2p, turnout = f$turn, dem = f$S_dem * f$turn, rep = f$S_rep * f$turn)
  ci = cred(dr$twop, level); ct = cred(dr$turnout, level)
  list(model = "medsl", status = "ok", settings = settings,
       race = bind_cols(tibble(status = "ok", est_twop = mean(dr$twop), lo_twop = ci[["lower"]], hi_twop = ci[["upper"]],
                               p_dem_win = mean(dr$twop > 0.5), est_turnout = mean(dr$turnout),
                               lo_turnout = ct[["lower"]], hi_turnout = ct[["upper"]]), counted),
       cells = f$prec,                       # precinct: turn_est, dem_est (share of all votes), interval
       draws = dr,
       point = c(dem = mean(dr$dem), rep = mean(dr$rep), tot = mean(dr$turnout)))
}
