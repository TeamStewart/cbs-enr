# expects `cands` to be defined
suppressPackageStartupMessages({
    library(seine)
    library(bases)
    library(purrr)
    library(dplyr)
})


make_spec_nyc = function(d, preds, trees = 100) {
    d = d |>
        filter(obs == 1) |>
        mutate(
            log_active = log_s(active),
            across(names(cands), ~ .x * total / n),
            prim25_mamdani = prim25_cvr_top_m * prim25_cvr_total / n,
            prim25_cuomo = prim25_cvr_top_c * prim25_cvr_total / n,
            prim25_abs = 1 - prim25_cvr_total / n,
            abstain = (n - total) / n
        )
    preds = tidyselect::eval_select(rlang::enquo(preds), d)
    z = select(
        d,
        pct_potus_24_dem,
        pct_mayor_21_dem,
        prim25_cvr_bullet,
        prim25_cvr_eday,
        log_active,
        p_male:p_col,
        p_wfp:p_con,
        -all_of(names(preds))
    )
    d$z = cbind(b_bart(z, trees = trees), as.matrix(select(d, ff_01:ff_16)))
    colnames(d$z) = NULL
    ei_spec(
        d,
        predictors = names(preds),
        outcome = c(names(cands), "abstain"),
        total = n,
        covariates = "z"
    )
}

make_run_ei <- function(d, preds, trees = 100, ...) {
    make_spec_nyc(d, {{ preds }}, trees = trees) |>
        ei_run(...) |>
        suppressWarnings()
}


enr_run_ei <- function(d, min_obs = 50, ...) {
    if (sum(d$obs == 1) > min_obs) {
        ei_ests = list(
            gender = make_run_ei(d, p_male:p_fem, ...),
            age = make_run_ei(d, starts_with("p_age"), ...),
            race = make_run_ei(d, p_white:p_asian, ...),
            party = make_run_ei(d, p_ind:p_rep, ...),
            pres24 = make_run_ei(d, pct_potus_24_dem:pct_potus_24_rep, ...),
            educ = make_run_ei(d, p_noncol:p_col, ...),
            prim25 = make_run_ei(d, prim25_mamdani:prim25_abs, ...),
            inc = make_run_ei(d, starts_with("inc_"), ...),
            tenure = make_run_ei(d, c(p_rent, p_own), ...)
        )

        list(
            turn = bind_rows(map(ei_ests, ei_abs_to_turn), .id = "demo"),
            vote = bind_rows(map(ei_ests, ei_norm_abs), .id = "demo"),
            ...
        )
    } else {
        NULL
    }
}

