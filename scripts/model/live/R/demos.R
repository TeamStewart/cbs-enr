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
##
## Pick one with `demo =` on any fit_*(), or on the command line as "<model>/<demo>", e.g.
## --models=swing,swing/strat_d_cov (run.R). How each model uses a spec:
##   swing, lm_bayes, pois_bayes   as fixed effects (demo_design()): each (1 | x) becomes dummies
##   ratio_strata   ratio within strata: only ratio and the strat_* specs without covariates
##   medsl_sur      none (Young-Kyung's own covariates)
##   (retired model_cory_pois, R/model_cpois.R: as written, lme4::glmer() with the (1 | x)
##   terms, ridge GLM without; its default is the cpois cascade below)
# ========================================

DEMO_COVS = c("dem2", "I(dem2^2)", "p_age_18_29", "p_age_65_up", "p_white_noncol", "p_white_col")
TPSOB_VARS = c("dem2", "p_age_18_29", "p_age_65_up", "p_white_col", "p_white_noncol", "p_black", "p_hisp")
TPSOB_P = 43L
TPSOB_Z = sprintf("z_%02d", seq_len(TPSOB_P))

DEMOS = list(
    ratio        = ~1,
    strat_g      = ~ (1 | geostratum),
    strat_d      = ~ (1 | stratum_demo),
    strat_dg     = ~ (1 | stratum_demo) + (1 | geostratum),
    strat_d_cov  = reformulate(c("(1 | stratum_demo)", DEMO_COVS)),
    strat_dg_cov = reformulate(c("(1 | stratum_demo)", "(1 | geostratum)", DEMO_COVS)),
    tpsob        = reformulate(TPSOB_Z),
    tpsob_g      = reformulate(c(TPSOB_Z, "geostratum"))
)

# the spec each model runs when none is given (cpois, retired: Cory's strat_lin cascade)
DEMO_DEFAULT = list(swing = "ratio", ratio_strata = "strat_g", pois_bayes = "strat_dg_cov", lm_bayes = "ratio",
                    cpois = c("strat_dg_cov", "strat_d_cov", "ratio"))

check_demo = function(demo) {
    bad = setdiff(demo, names(DEMOS))
    if (length(bad)) stop("unknown demographic spec(s): ", paste(bad, collapse = ", "),
                          ". Choose from: ", paste(names(DEMOS), collapse = ", "))
    demo
}

.demos = new.env()

# One row per precinct: stratum_demo, the covariates (a missing value gets the state mean)
# and the thin-plate basis z_01..z_43, built over the whole state. Cached per state.
demo_covs = function(state, dir_26 = find_26_general()) {
    if (!is.null(.demos[[state]])) return(.demos[[state]])
    f = file.path(dir_26, "input_data", "model_inputs", paste0(state, "_precinct.parquet"))
    d = read_pq(f) |>
        transmute(geo = as.character(geo), stratum_demo = coalesce(as.character(stratum_demo), "none"),
                  across(all_of(TPSOB_VARS), \(x) coalesce(x, mean(x, na.rm = TRUE))))
    z = do.call(bases::b_tpsob, c(unname(as.list(d[TPSOB_VARS])), p = TPSOB_P))
    .demos[[state]] = bind_cols(select(d, -dem2), as_tibble(setNames(as.data.frame(z[, seq_len(TPSOB_P)]), TPSOB_Z)))
    .demos[[state]]
}

# cells with the demographic columns joined on; geostratum and stratum_demo as factors
demo_frame = function(cells, state) {
    cells |>
        mutate(geo = as.character(geo)) |>
        left_join(demo_covs(state), by = "geo") |>
        mutate(stratum_demo = coalesce(stratum_demo, "none"),
               across(c(geostratum, stratum_demo), \(x) factor(as.character(x))))
}

# the strata of a spec with no covariates ((1 | x) terms only), for ratio_strata; NULL otherwise
demo_strata = function(demo) {
    f = DEMOS[[demo]]
    if (!is.null(reformulas::nobars(f)) && length(attr(terms(reformulas::nobars(f)), "term.labels"))) return(NULL)
    vapply(reformulas::findbars(f), \(b) deparse(b[[3]]), "")
}

# A spec as a numeric fixed-effect design (no intercept) on demo_frame() rows: each (1 | x)
# becomes dummies for x. Factors with one level and constant columns are dropped, and column
# names are made syntactic (d_1, d_2, ...) so they can go straight into a formula. `drop`:
# terms the model already has (lm_bayes's dem2).
demo_design = function(d, demo, drop = character()) {
    f = DEMOS[[demo]]
    terms_ = c(if (!is.null(reformulas::nobars(f))) attr(terms(reformulas::nobars(f)), "term.labels"),
               vapply(reformulas::findbars(f), \(b) deparse(b[[3]]), ""))
    terms_ = setdiff(terms_, drop)
    terms_ = terms_[!terms_ %in% c("geostratum", "stratum_demo") | vapply(terms_, \(t) nlevels(d[[t]]) > 1, NA)]
    if (!length(terms_)) return(matrix(numeric(), nrow(d), 0))
    X = model.matrix(reformulate(terms_), d)[, -1, drop = FALSE]
    X = X[, apply(X, 2, \(x) diff(range(x)) > 1e-12), drop = FALSE]
    colnames(X) = sprintf("d_%d", seq_len(ncol(X)))
    X
}
