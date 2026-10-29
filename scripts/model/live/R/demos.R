# ========================================
## demos.R -- the demographic specs every precinct model can take: Cory McCartan's
## `model_specs` from the 2026 TX primaries (tx-prim-26/R/prec_workflow.R), in our column
## names (dem24_est -> dem2, age_18_29 -> p_age_18_29, ...).
##
##   ratio         nothing: expected votes only
##   strat_g       (1 | geostratum)                    model_prec_pois()'s own default
##   strat_d       (1 | stratum_demo)
##   strat_dg      (1 | stratum_demo) + (1 | geostratum)
##   strat_d_cov   strat_d + dem2 + dem2^2 + ages 18-29 and 65+ + white college / non-college
##   strat_dg_cov  strat_dg + the same covariates
##   tpsob         z: 43-column thin-plate spline basis (bases::b_tpsob) of dem2, ages 18-29
##                 and 65+, white college / non-college, Black, Hispanic
##   tpsob_g       z + geostratum
##   medsl_cov     medsl_sur's own covariates: dem2, 2022 and 2024 turnout (L2 rates), ages
##                 65+, male, YouGov Black and Hispanic predictions (not one of Cory's)
##
## Pick one with `demo =` on any fit_*(), or on the command line as "<model>/<demo>", e.g.
## --models=swing,swing/strat_d_cov (run.R). How each model uses a spec:
##   swing, lm_bayes, pois_bayes   as fixed effects (demo_design()): each (1 | x) becomes dummies
##   ratio_strata   ratio within strata: only ratio and the strat_* specs without covariates
##   medsl_sur      at set-up (medsl_spec(), model_medsl.R): covariates go into model_turnout.R's
##                  COVS_TURN / COVS_SHARE after log_n, (1 | x) terms (and tpsob_g's geostratum)
##                  become its random-effect grouping in place of county; default medsl_cov,
##                  which is the model's own design unchanged
# ========================================

DEMO_COVS = c("dem2", "I(dem2^2)", "p_age_18_29", "p_age_65_up", "p_white_noncol", "p_white_col")
TPSOB_VARS = c(
  "dem2",
  "p_age_18_29",
  "p_age_65_up",
  "p_white_col",
  "p_white_noncol",
  "p_black",
  "p_hisp"
)
TPSOB_P = 43L
TPSOB_Z = sprintf("z_%02d", seq_len(TPSOB_P))
MEDSL_COVS = c(
  "dem2",
  "turnout22",
  "turnout24",
  "p_age_65_up",
  "p_male",
  "yg_pred_bla",
  "yg_pred_his"
)

DEMOS = list(
  ratio = ~1,
  strat_g = ~ (1 | geostratum),
  strat_d = ~ (1 | stratum_demo),
  strat_dg = ~ (1 | stratum_demo) + (1 | geostratum),
  strat_d_cov = reformulate(c("(1 | stratum_demo)", DEMO_COVS)),
  strat_dg_cov = reformulate(c("(1 | stratum_demo)", "(1 | geostratum)", DEMO_COVS)),
  tpsob = reformulate(TPSOB_Z),
  tpsob_g = reformulate(c(TPSOB_Z, "geostratum")),
  medsl_cov = reformulate(MEDSL_COVS)
)

# the spec each model runs when none is given
DEMO_DEFAULT = list(
  swing = "ratio",
  ratio_strata = "strat_g",
  pois_bayes = "strat_dg_cov",
  lm_bayes = "ratio",
  medsl_sur = "medsl_cov"
)

#' Check demographic spec names
#'
#' @param demo Character vector of spec names.
#' @return `demo`, unchanged. Stops on any name not in `DEMOS`.
check_demo = function(demo) {
  bad = setdiff(demo, names(DEMOS))
  if (length(bad)) {
    stop(
      "unknown demographic spec(s): ",
      paste(bad, collapse = ", "),
      ". Choose from: ",
      paste(names(DEMOS), collapse = ", ")
    )
  }
  demo
}

.demos = new.env()

#' Precinct covariates for every demographic spec
#'
#' One row per precinct of `{ST}_precinct.parquet`: stratum_demo, every covariate any spec
#' uses (a missing value gets the state mean), and the thin-plate basis `z_01` ... `z_43`
#' (`bases::b_tpsob()` of `TPSOB_VARS`), built over the whole state. Cached per state for
#' the session.
#'
#' @param state Two-letter state code.
#' @param dir_26 Path to 26_general.
#' @return Tibble keyed by geo (dem2 is left out; cells carry it).
demo_covs = function(state, dir_26 = find_26_general()) {
  if (!is.null(.demos[[state]])) {
    return(.demos[[state]])
  }
  f = file.path(dir_26, "input_data", "model_inputs", paste0(state, "_precinct.parquet"))
  d = read_pq(f) |>
    transmute(
      geo = as.character(geo),
      stratum_demo = coalesce(as.character(stratum_demo), "none"),
      across(all_of(union(TPSOB_VARS, MEDSL_COVS)), \(x) coalesce(x, mean(x, na.rm = TRUE)))
    )
  z = do.call(bases::b_tpsob, c(unname(as.list(d[TPSOB_VARS])), p = TPSOB_P))
  .demos[[state]] = bind_cols(
    select(d, -dem2),
    as_tibble(setNames(as.data.frame(z[, seq_len(TPSOB_P)]), TPSOB_Z))
  )
  .demos[[state]]
}

#' Cells with the demographic columns joined on
#'
#' @param cells Cells from `build_cells()`.
#' @param state Two-letter state code.
#' @return `cells` plus `demo_covs()` columns; geostratum and stratum_demo as factors
#'   (missing stratum_demo -> "none").
demo_frame = function(cells, state) {
  cells |>
    mutate(geo = as.character(geo)) |>
    left_join(demo_covs(state), by = "geo") |>
    mutate(
      stratum_demo = coalesce(stratum_demo, "none"),
      across(c(geostratum, stratum_demo), \(x) factor(as.character(x)))
    )
}

#' Strata of a spec without covariates
#'
#' For ratio_strata, which pools ratios within strata and takes no covariates.
#'
#' @param demo Spec name.
#' @return Character vector of the `(1 | x)` grouping variables; NULL if the spec has
#'   fixed-effect terms.
demo_strata = function(demo) {
  f = DEMOS[[demo]]
  if (
    !is.null(reformulas::nobars(f)) && length(attr(terms(reformulas::nobars(f)), "term.labels"))
  ) {
    return(NULL)
  }
  vapply(reformulas::findbars(f), \(b) deparse(b[[3]]), "")
}

#' A spec as a numeric fixed-effect design matrix
#'
#' Each `(1 | x)` term becomes dummies for x (no intercept). Factors with one level and
#' constant columns are dropped, and columns are renamed `d_1`, `d_2`, ... so they can go
#' straight into a formula.
#'
#' @param d Rows from `demo_frame()`.
#' @param demo Spec name.
#' @param drop Terms the model already has (lm_bayes drops "dem2").
#' @return Numeric matrix with `nrow(d)` rows; 0 columns if nothing is left.
demo_design = function(d, demo, drop = character()) {
  f = DEMOS[[demo]]
  terms_ = c(
    if (!is.null(reformulas::nobars(f))) attr(terms(reformulas::nobars(f)), "term.labels"),
    vapply(reformulas::findbars(f), \(b) deparse(b[[3]]), "")
  )
  terms_ = setdiff(terms_, drop)
  terms_ = terms_[
    !terms_ %in% c("geostratum", "stratum_demo") | vapply(terms_, \(t) nlevels(d[[t]]) > 1, NA)
  ]
  if (!length(terms_)) {
    return(matrix(numeric(), nrow(d), 0))
  }
  X = model.matrix(reformulate(terms_), d)[, -1, drop = FALSE]
  X = X[, apply(X, 2, \(x) diff(range(x)) > 1e-12), drop = FALSE]
  colnames(X) = sprintf("d_%d", seq_len(ncol(X)))
  X
}
