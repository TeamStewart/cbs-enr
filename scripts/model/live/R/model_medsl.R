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
#'   the party split just follows dem2), note (what was filled, or "").
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
  list(
    covs_long = covs_long,
    mode_hist = mode_hist,
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
    if (!by_method) "|nomode"
  )
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
    if (!is.null(spec$re)) c(RE_GROUPING = spec$re)
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
      env$cells,
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
  # conformal: one row per precinct x mode of the model's frame, with what is counted
  fc <- if (interval == "conformal") {
    cv <- f$cell_votes
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
      )
  }
  # conf_seq: prior predictive draws of the race totals, nothing counted (env$cells holds
  # the model's own priors; fit_stage_hybrid() re-centres a copy)
  prior <- if (interval == "conf_seq") env$prior_totals(env$cells, draws = draws)
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
    cells = prec_rows(f$prec)
  )
  res$status <- res$race$status <- st
  # the structured ledger as well as the status string, so a consumer can test
  # `fit$prior_only$all_prior` rather than parse prose
  res$prior_only <- po
  res
}
