setwd("gen-25")
here::i_am("R/nj_passaic.R")
library(here)
library(tidyverse)
library(seine)

path =
header = read_csv(path, col_names = FALSE, n_max = 2, show_col_types = FALSE) |>
    t()

d = here("data/nj/passaic.csv") |>
    read_csv(
        col_names = c(
            "precinct",
            "vap", "vap_hisp", NA,
            "reg_24", NA, NA, "dem_24", "rep_24", "turn_24", NA,
            "reg_25", NA, NA, "dem_25", "rep_25", "turn_25", NA,
            NA, NA, NA, NA
        ),
        col_types = cols(.default= "c"),
        skip = 3,
    ) |>
    suppressMessages() |>
    select(!starts_with("...")) |>
    mutate(across(!precinct, parse_number)) |>
    drop_na() |>
    mutate(
        town = word(precinct, 1, sep = " [WD]\\d"),
        vap_hisp = vap_hisp / vap,
        vap_nohisp = 1 - vap_hisp,
        abs_24 = reg_25 - dem_24 - rep_24,
        abs_25 = reg_25 - dem_25 - rep_25,
        swing_24_25 = ((dem_25 - rep_25) - (dem_24 - rep_24)) / turn_24,
    ) |>
    ei_proportions(dem_24, rep_24, abs_24, .total=reg_25) |>
    ei_proportions(dem_25, rep_25, abs_25, .total=reg_25)

spec_hisp = ei_spec(
    d,
    predictors = c(hisp=vap_hisp, nohisp=vap_nohisp),
    outcome = swing_24_25,
    total = reg_25,
    covariates = c(town, dem_24)
)

spec_24 = ei_spec(
    d,
    predictors = c(dem_24, rep_24, abs_24),
    outcome = c(dem_25, rep_25, abs_25),
    total = reg_25,
    covariates = c(town, vap_hisp)
)


do_ei = function(spec, bounds = FALSE, bias = 0.05) {
    m = ei_ridge(spec, bounds = bounds)
    rr = ei_riesz(spec, penalty = m$penalty)
    ei_est(m, rr, data = spec, conf_level = 0.95, use_student=F) |>
        mutate(
            r2 = m$r2[fct_inorder(outcome)],
            nu2 = rr$nu2[predictor]
        ) |>
        ei_sens(c_outcome=1, bias_bound = bias) |>
        ei_sens_rv(bias_bound = bias)
}

est_hisp = do_ei(spec_hisp, bounds=c(-1, 1))
est_24 = do_ei(spec_24, bounds=0:1)

d_pred_n = summarize(d, across(c(dem_24, rep_24, abs_24), ~ sum(.x * reg_25))) |>
    pivot_longer(everything(), names_to="predictor", values_to="n_pred")
d_out_n = summarize(d, across(c(dem_25, rep_25, abs_25), ~ sum(.x * reg_25))) |>
    pivot_longer(everything(), names_to="outcome", values_to="n_out")
d_est_24 = est_24 |>
    select(predictor:estimate) |>
    mutate(estimate = pmax(pmin(estimate, 0.995), 0.005)) |>
    left_join(d_pred_n, by="predictor") |>
    left_join(d_out_n, by="outcome")

for (i in 1:20) {
    d_adj_pred = summarize(d_est_24, estimate = sum(estimate), .by=predictor) |>
        rename(adj = estimate)
    d_est_24 = d_est_24 |>
        left_join(d_adj_pred, by="predictor") |>
        mutate(estimate = estimate / adj) |>
        select(-adj)

    d_adj_out = summarize(d_est_24, estimate = sum(estimate * n_pred), .by=outcome) |>
        left_join(d_out_n, by="outcome") |>
        mutate(adj = estimate / n_out) |>
        select(outcome, adj)
    d_est_24 = d_est_24 |>
        left_join(d_adj_out, by="outcome") |>
        mutate(estimate = estimate / adj) |>
        select(-adj)
}

summarize(d_est_24, estimate = sum(estimate), .by=predictor)
summarize(d_est_24, estimate = sum(estimate * n_pred), .by=outcome)

d_transition = d_est_24 |>
    select(predictor, outcome, estimate) |>
    pivot_wider(names_from=outcome, values_from=estimate)
d_transition_n = d_est_24 |>
    mutate(estimate = estimate * n_pred) |>
    select(predictor, outcome, estimate) |>
    pivot_wider(names_from=outcome, values_from=estimate) |>
    mutate(tot_25 = dem_25 + rep_25 + abs_25)
bind_rows(
    d_transition_n,
    summarize(d_transition_n, across(-predictor, sum)) |>
        mutate(predictor = "tot_24")
)

d_transition |>
    as.data.frame() |>
    column_to_rownames("predictor") |>
    as.matrix() |>
    rowSums()
