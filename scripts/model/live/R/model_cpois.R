# ========================================
## model_cpois.R -- model_cory_pois: Cory McCartan's precinct Poisson GLMM
## (model_prec_pois(), scripts/model/modeling-cory/tx-prim-26/R/prec_model.R), run on any
## race in the live format.
##
## prec_model.R is NOT copied or edited here. Its own code runs, from its own file, the
## first time the model is used in a session -- the same anti-drift arrangement
## model_medsl.R has with scripts/model/model_turnout.R. Note the difference in how safe
## that is: model_turnout.R is in git, while scripts/model/modeling-cory/ is an UNTRACKED
## vendored snapshot of Cory's repo (there are .zip files beside it). If the folder is not
## there, cpois_setup() stops with the path it looked for; it never falls back to a copy.
##
## THE MODEL. votes ~ log_s(est) + <random effects>, fitted with lme4::glmer(poisson())
## separately for every (candidate, party, method) cell group, where log_s(x) = log(x + 0.1)
## and `est` is the cell's expected votes. `formulas` is a cascade tried in order by
## map_until(): the first one that fits without erroring wins, and its name is reported as
## the `model` column. A formula with no random effects goes to glm_minnorm() (glmnet with
## lambda = 10) instead -- glmnet is NOT installed here, so CPOIS_FORMULAS deliberately
## contains only random-effect formulas; see CPOIS_FORMULAS below.
##
## Before fitting, the model estimates a turnout ratio per (geostratum, method) from the
## cells at stage >= stage_done and rescales each cell's `est` by it; partial cells have
## their counts grossed up by est_tot / votes_tot. Prediction is
## predict(type = "response", allow.new.levels = TRUE), so a geostratum that never reports
## still gets its fixed-effect prediction.
##
## Intervals are SPLIT-CONFORMAL ON THE SHARE SCALE, not posterior draws: residual
## quantiles q_min / q_max of (reported share - predicted share) within a precinct x method
## cell, taken at conf_level over the reporting cells, then applied as
## pred_min = tot_pred * pmax(share + q_min, 0). So they describe a candidate's share of
## the cell, and the cell total carries no uncertainty at all -- which is why
## lo_turnout / hi_turnout are NA here (see fit_cpois).
##
## Inputs and output are the same as the other models (see run.R):
##   fit_cpois(cells, returns)   cells from build_cells(), one race's standard returns
## There are no draws, so p_dem_win is NA (as for model_cory).
##
## STAGE. The model tests stage > 0L (reporting), stage >= 2L (is_rep) and stage <= 4L
## (use the prediction rather than the raw count), and stage >= stage_done for the turnout
## ratio. Our vocabulary is 0 / 4 / 6 (CONTRACT.md section 8), which lines up: 0 = nothing,
## 4 = reporting but not complete (prediction used), 6 = complete (raw count used, and the
## cell feeds the turnout ratio). stage_done is therefore 6L, not the model's 6L default by
## accident -- it is passed explicitly. Two caveats, both inherited rather than introduced:
##   - returns.R sets `stage` per PRECINCT (6 only when every method of that precinct is
##     counted out), so a method that is finished while a sibling method is not reads 4.
##     The turnout ratio is correspondingly conservative early in the night.
##   - CONTRACT.md section 8 argues no model component should gate on completeness at all.
##     This model's turnout ratio does. That is Cory's design and is left alone here.
##
## UPSTREAM INCONSISTENCY, handled. prec_model.R hardcodes model_conf = "approx_conform_99"
## while its conf_level argument defaults to 0.95 -- the label does not track the
## parameter. fit_cpois() overwrites it from `level` (so "approx_conform_95" at the
## default) in both the race row's settings and res$pred$model_conf.
# ========================================

CPOIS_DIR = "scripts/model/modeling-cory/tx-prim-26/R"
.cpois_env = new.env()   # cache: one sourced copy of Cory's model per session

# The fallback cascade, tried in order; the first that fits wins and names the fit.
# Only random-effect formulas: map_until() sends a formula with no random effects to
# glm_minnorm(), which needs glmnet, and glmnet is not installed (it is in renv.lock as a
# dependency of broom, not of this). Adding `~ 1` as a last resort would make a thin-data
# race fail loudly instead of degrading -- if that is wanted, install glmnet first.
# dem2 earns its place because `est` is NOT party-split here (see cpois_inputs): the
# precinct's partisan lean has to enter as a covariate instead.
CPOIS_FORMULAS = list(
  strat_g_cov = ~ (1 | geostratum) + dem2 + I(dem2^2),
  strat_g     = ~ (1 | geostratum)
)

# Source prec_model.R and utils.R (glm_minnorm(), predict.glm_minnorm(), map_until()) into
# a fresh environment whose parent is the global one, so they see the packages load.R
# attaches (dplyr, tidyr, purrr, readr, stringr) exactly as they do in Cory's own session.
# `log_s` is enightmodels' and prec_model.R calls it unqualified inside the model formula,
# so it is bound here and the formulas are re-environment()ed onto this env in fit_cpois().
cpois_setup = function(src = CPOIS_DIR, refresh = FALSE) {
  if (!refresh && !is.null(.cpois_env$env)) return(.cpois_env$env)
  need = c("lme4", "broom.mixed", "collapse", "enightmodels")
  miss = need[!vapply(need, \(p) requireNamespace(p, quietly = TRUE), NA)]
  if (length(miss))
    stop("model_cory_pois needs ", paste(miss, collapse = ", "),
         " (not installed). lme4 / broom.mixed / collapse are plain CRAN installs; ",
         "enightmodels is Cory's package and is in the project's renv.lock.")
  files = file.path(src, c("utils.R", "prec_model.R"))
  if (!all(file.exists(files)))
    stop("Can't find Cory's precinct model. Expected utils.R and prec_model.R in\n  ",
         normalizePath(src, mustWork = FALSE),
         "\nscripts/model/modeling-cory/ is an UNTRACKED vendored snapshot of Cory's repo: ",
         "unzip tx-prim-26.zip beside it, or run from the cbs-enr folder. ",
         "The model body is deliberately NOT copied into live/R/, so there is no fallback.")
  env = new.env(parent = globalenv())
  env$log_s = enightmodels::log_s
  for (f in files) sys.source(f, envir = env)
  if (!is.function(env$model_prec_pois) || !is.function(env$map_until))
    stop("prec_model.R / utils.R no longer define model_prec_pois() and map_until(); ",
         "update scripts/model/live/R/model_cpois.R")
  .cpois_env$env = env
  env
}

# ---- the input adapter -------------------------------------------------------------
# The model wants two frames. The hard part is `party`, and the reason is that
# model_prec_pois() was written for a PRIMARY -- one party per contest. Two places assume
# it (prec_model.R, the d_frame block):
#     d_frame = filter(d_cov, party == d_race$party[1], ...) |>
#               cross_join(tibble(candidate = unique(d_race$candidate)))
# so the prediction frame takes ONE party's covariates and hands them to EVERY candidate,
# and then joins the reported votes back on `party`. In a primary that is exactly right.
# In a general election with a Dem and a Rep it is not: with a party-long d_cov the Rep
# candidate would be predicted from the Dem's `est` and would never see its own counted
# votes (the join on party fails, so use_pred stays TRUE for every reported cell).
#
# So `party_est` picks how the party dimension is carried:
#
#   "pooled" (default, and the mode this is wired in) -- d_cov carries ONE party label,
#     "all", and est = exp_votes, the cell's whole expected vote. Every candidate is fitted
#     against the same cell-size covariate, with its own intercept and its own coefficient
#     on dem2, so the partisan lean enters through the formula instead of through `est`.
#     Everything in prec_model.R then works as written: d_join joins, d_frame gets the
#     right `est` and the right `votes` for every candidate, the conformal residuals are
#     shares of the whole field within a cell (non-degenerate), and tot_pred is the cell's
#     real predicted total.
#
#   "long" -- d_cov party-long with est = exp_votes * dem2 (dem) and exp_votes * (1 - dem2)
#     (rep), which is the obvious reading of the model's `est`. The FIT is correct this way
#     (d_join joins on party, so each candidate is fitted against its own party's est), but
#     d_frame is not, for the reason above. Measured on tx_base, the Rep candidate's rows
#     are dropped outright by d_pred's inner_join(models) -- their d_frame party is the Dem
#     label while `models` is keyed on the real party -- so the result has Dem rows only
#     and no two-party share at all. Kept, behind this argument, because it is the natural
#     reading and someone will try it; it is not usable as a race model.
#
# A third option exists and was rejected: run the model once per party. The fit and the
# prediction frame are then both correct, but the conformal residual is a share WITHIN the
# call, and with one candidate per party that share is identically 1 -- so q_min = q_max = 0
# and every interval collapses to a point. Zero-width is worse than approximate.
#
# Also handled here:
#   county      joined from `cells`; the model joins d_race to d_cov on it.
#   geostratum  a factor, as in Cory's own call site (prec_workflow.R:100).
#   house races the model only filters d_cov by `cd` when the contest string starts with
#               "house_", and parse_race_id() gives us contest "house" with the district in
#               its own column -- so that filter never fires for us. The district
#               restriction is therefore done HERE, from {ST}_precinct_cd.parquet (dominant
#               cd per geo), with the returns' own geos as the fallback. Without it a house
#               race would be predicted over the whole state.
cpois_inputs = function(cells, returns, contest = NULL, district = NULL,
                        party_est = c("pooled", "long"), by_method = TRUE,
                        dir_26 = find_26_general()) {
  party_est = match.arg(party_est)
  state = unique(as.character(returns$state))

  cl = cells |>
    transmute(geo = as.character(geo), county = as.character(county),
              geostratum = factor(as.character(geostratum)),
              method = as.character(method), dem2 = as.numeric(dem2),
              oth = as.numeric(oth), exp_votes = as.numeric(exp_votes))

  # house race: keep only the district's precincts
  cd_note = ""
  if (!is.null(contest) && identical(as.character(contest), "house") &&
      length(district) == 1 && !is.na(district)) {
    f = file.path(dir_26, "input_data", "model_inputs", paste0(state[1], "_precinct_cd.parquet"))
    if (file.exists(f) && file.size(f) > 0) {
      cd = read_pq(f) |>
        mutate(geo = as.character(geo)) |>
        slice_max(share, n = 1, with_ties = FALSE, by = geo) |>
        select(geo, cd)
      cl = cl |> inner_join(cd, by = "geo") |> filter(cd == as.integer(district))
      cd_note = sprintf("house district %s: %d precincts from %s_precinct_cd.parquet",
                        district, n_distinct(cl$geo), state[1])
    } else {
      cl = semi_join(cl, distinct(mutate(returns, geo = as.character(geo)), geo), by = "geo")
      cl$cd = as.integer(district)
      cd_note = sprintf("house district %s: no %s_precinct_cd.parquet, fell back to the %d precincts in the returns",
                        district, state[1], n_distinct(cl$geo))
    }
    if (!nrow(cl)) stop("no precincts left for house district ", district, " in ", state[1])
  }
  if (!nrow(cl)) stop("fit_cpois(): `cells` is empty")

  # d_race: standard returns plus the county from cells. inner_join, so a returns row whose
  # (geo, method) is not a cell is dropped -- cell_counts() has already reported those
  # (returns_dropped(), returns.R), so this adds no new silent loss.
  d_race = returns |>
    transmute(geo = as.character(geo), method = as.character(method),
              candidate = as.character(candidate), party_true = as.character(party),
              votes = as.numeric(votes), stage = as.integer(stage), time) |>
    inner_join(distinct(cl, geo, county, method), by = c("geo", "method"))
  if (!nrow(d_race)) stop("fit_cpois(): no returns row is on a known precinct x method cell")

  keep = c("geo", "county", "geostratum", "method", "dem2", "oth",
           if ("cd" %in% names(cl)) "cd")
  if (party_est == "pooled") {
    d_race$party = "all"
    d_cov = cl |> mutate(party = "all", est = exp_votes) |> select(all_of(keep), party, est)
  } else {
    d_race$party = d_race$party_true
    d_cov = bind_rows(
      cl |> mutate(party = "dem", est = exp_votes * dem2),
      cl |> mutate(party = "rep", est = exp_votes * (1 - dem2))) |>
      select(all_of(keep), party, est)
  }

  # by_method = FALSE: the model aggregates d_race over methods itself and then does
  # filter(d_cov, method == "all"), and d_frame does NOT filter the method -- so d_cov must
  # be "all"-only or every precinct is predicted once per method and summed.
  # by_method = TRUE: the mirror image, d_frame drops method == "all", so d_cov must carry
  # the real methods and no "all" row. Our cells are precinct x method, hence the default.
  if (!by_method) {
    d_cov = d_cov |>
      summarize(across(any_of(c("geostratum", "dem2", "oth", "cd")), \(x) x[1]),
                est = sum(est), .by = c(geo, county, party)) |>
      mutate(method = "all")
  }
  list(d_race = d_race, d_cov = d_cov,
       cand_party = distinct(d_race, candidate, party_true), note = cd_note)
}

# ---- the model ---------------------------------------------------------------------
# cells:   build_cells() output (also the counted totals, via cell_counts(), so they match
#          the other models exactly)
# returns: one race's standard returns (returns.R)
# level:   the conformal level; it sets conf_level AND model_conf, which prec_model.R
#          hardcodes to "approx_conform_99" regardless (see the header).
fit_cpois = function(cells, returns, formulas = CPOIS_FORMULAS, stage_done = 6L,
                     turn_ratio = 1.0, level = 0.95, by_method = TRUE,
                     party_est = c("pooled", "long"), src = CPOIS_DIR,
                     dir_26 = find_26_general()) {
  party_est = match.arg(party_est)
  if (n_distinct(returns$race_id) != 1) stop("fit_cpois() takes one race at a time")
  env = cpois_setup(src)
  # update(f, votes ~ log_s(est) + .) keeps environment(f), which is how log_s() gets found
  forms = lapply(formulas, function(f) { environment(f) = env; f })

  rid = returns$race_id[1]
  ri = if (grepl("^[A-Za-z]{2}_", rid)) parse_race_id(rid) else tibble(contest = rid, district = NA_integer_)

  counts = cell_counts(cells, returns)
  counted = tibble(raw_twop = sum(counts$c_dem) / max(sum(counts$c_dem + counts$c_rep), 1),
                   counted_votes = sum(counts$c_tot), expected_votes = sum(cells$exp_votes),
                   obs_cells = sum(counts$c_tot > 0),
                   complete_cells = sum(counts$c_tot > 0 & counts$complete))
  mconf = sprintf("approx_conform_%g", 100 * level)
  settings = sprintf(paste0(
    "model_prec_pois() (Poisson GLMM, lme4::glmer, one fit per candidate x method: ",
    "votes ~ log_s(est) + .); formula cascade %s; by_method = %s; est = %s; ",
    "stage_done = %d; turn_ratio = %g; split-conformal share interval %s"),
    paste(names(forms), collapse = " -> "), by_method, party_est,
    as.integer(stage_done), turn_ratio, mconf)

  nores = function(why, extra = NULL)
    list(model = "cpois", status = why,
         settings = if (is.null(extra)) settings else paste0(settings, "; ", extra),
         race = bind_cols(tibble(status = why, est_twop = NA_real_, lo_twop = NA_real_,
                                 hi_twop = NA_real_, p_dem_win = NA_real_,
                                 est_turnout = NA_real_, lo_turnout = NA_real_,
                                 hi_turnout = NA_real_), counted),
         cells = NULL, draws = NULL, point = NULL, raw = NULL)
  if (!any(counts$c_tot > 0)) return(nores("no_votes"))

  inp = cpois_inputs(cells, returns, ri$contest, ri$district, party_est, by_method, dir_26)
  if (nzchar(inp$note)) settings = paste0(settings, "; ", inp$note)

  # glmer emits a singular-fit / convergence warning per group; Cory's own call site wraps
  # the model in purrr::quietly() for the same reason (prec_workflow.R:100). Count them and
  # report the count rather than dropping them on the floor.
  nwarn = 0L
  res = withCallingHandlers(
    suppressMessages(env$model_prec_pois(
      inp$d_race, inp$d_cov, ri$contest, ri$district, formulas = forms,
      stage_done = as.integer(stage_done), turn_ratio = turn_ratio,
      conf_level = level, by_method = by_method)),
    warning = function(w) { nwarn <<- nwarn + 1L; invokeRestart("muffleWarning") })
  if (nwarn) settings = paste0(settings, sprintf("; %d fit warning(s) muffled", nwarn))
  if (is.null(res) || is.null(res$pred) || !nrow(res$pred)) return(nores("no_fit"))

  # the upstream label is hardcoded 99 whatever conf_level was; make it tell the truth
  pr = res$pred |>
    mutate(candidate = as.character(candidate), model_conf = mconf) |>
    left_join(inp$cand_party, by = "candidate") |>
    mutate(party_agg = case_when(party_true == "dem" ~ "dem", party_true == "rep" ~ "rep",
                                 TRUE ~ "oth"))
  res$pred = pr
  if (anyNA(pr$party_true))
    return(nores("no_fit", "a predicted candidate is not in the returns"))

  bp = pr |> summarize(pred = sum(pred), lo = sum(pred_min), hi = sum(pred_max), .by = party_agg)
  g = function(p, col) { v = bp[[col]][bp$party_agg == p]; if (length(v) == 1) v else NA_real_ }
  dem = g("dem", "pred"); rp = g("rep", "pred")
  if (is.na(dem) || is.na(rp)) return(nores("no_fit", "no fit for one of the two major parties"))

  # TWO-PARTY SHARE. The model predicts per candidate, so dem / (dem + rep) is exact for
  # the point estimate. The INTERVAL is not: pred_min / pred_max are conformal bounds on
  # each party's VOTES, and the ratio of two such bounds is not a conformal interval on the
  # ratio. The combination below (each party's bound against the other party's opposite
  # bound) is the widest one the two intervals admit, so it is conservative in the usual
  # case and exact in none -- read lo_twop / hi_twop as approximate. It is the natural
  # pairing here because the conformal residuals are SHARES within a cell and therefore
  # sum to about zero across candidates: a Dem share residual at its low quantile goes with
  # a Rep share residual at its high one.
  twop = function(d, r) if (is.na(d) || is.na(r) || d + r <= 0) NA_real_ else d / (d + r)
  list(model = "cpois", status = "ok", settings = settings,
       race = bind_cols(tibble(
         status = "ok",
         est_twop = twop(dem, rp),
         lo_twop = twop(g("dem", "lo"), g("rep", "hi")),
         hi_twop = twop(g("dem", "hi"), g("rep", "lo")),
         p_dem_win = NA_real_,              # frequentist, no draws (as model_cory)
         est_turnout = sum(pr$pred),
         # NA by construction, not by omission: the conformal residual is a candidate's
         # share WITHIN a cell, and the cell total (tot_pred) carries no uncertainty, so
         # summing pred_min / pred_max over candidates returns the point total.
         lo_turnout = NA_real_, hi_turnout = NA_real_), counted),
       cells = NULL,   # model_prec_pois() summarises geo away; no precinct frame to give
       draws = NULL,
       point = c(dem = dem, rep = rp, tot = sum(pr$pred)),
       raw = res)      # list(pred =, coef =), the same shape fit_cory() returns in `raw`
}
