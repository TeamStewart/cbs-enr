# expects `cands` to be defined

suppressPackageStartupMessages({
    library(posterior)
    library(dplyr)
    filter <- dplyr::filter
})

fit_forecast_nyc <- function(data, formula, prior_n_turn = 0.5, prior_n_cands = 1.5, ...) {
    forecast_prec(
        data = data,
        cands = names(cands),
        turn = total,
        obs = obs == 1,
        obs_turn = obs_turn == 1,
        ctrl_form = formula,
        prior_turn = pred_turn,
        prior_cands = c(pred_mamdani, pred_sliwa, pred_cuomo),
        prior_n_turn = prior_n_turn,
        prior_n_cands = prior_n_cands,
        ...
    )
}

enr_forecast_nyc <- function(d, draws = 500) {
    m_simple_pres = d |>
        fit_forecast_nyc(
            ~ log_s(pred_turn) + pres24_2way_dem,
            draws = draws,
            prior_n_turn = 1.0, prior_n_cands = 1.5
        )

    m_simple_pred = d |>
        fit_forecast_nyc(
            ~ log_s(pred_turn) + pred_mamdani + pred_cuomo + pred_sliwa,
            draws = draws,
            prior_n_turn = 1.5, prior_n_cands = 2.5
        )

    m_simple_mult = d |>
        fit_forecast_nyc(
            ~ log_s(pred_turn) + log_s(votes_total_24) + log_s(prim25_cvr_total) +
                pct_mayor_21_dem + pres24_2way_dem + pred_mamdani,
            draws = draws,
            prior_n_turn = 2.0, prior_n_cands = 2.5
        )

    m_full = d |>
        fit_forecast_nyc(
            ~ log_s(votes_total_21) + log_s(votes_total_24) + log_s(prim25_cvr_total) +
                log_s(active) + log_s(pred_turn) + prim25_cvr_eday +
                p_age_18_29 + p_age_65_up +
                p_white_col + p_white_noncol + p_black_col + p_black_noncol + p_hisp +
                p_noncol_male + p_noncol_fem +
                # p_wfp + p_ind + p_dem +
                pres24_2way_dem + pct_mayor_21_dem + pres20_2way_dem +
                pred_mamdani + pred_cuomo + pred_sliwa +
                prim25_cvr_top_m + prim25_cvr_top_c + prim25_cvr_slate,# +
                # ff_01 + ff_02 + ff_03 + ff_04 + ff_05 + ff_06 + ff_07 + ff_08 +
                # ff_09 + ff_10 + ff_11 + ff_12 + ff_13 + ff_14 + ff_15 + ff_16, # +
                # ff_17 + ff_18 + ff_19 + ff_20 + ff_21 + ff_22 + ff_23 + ff_24 +
                # ff_25 + ff_26 + ff_27 + ff_28 + ff_29 + ff_30 + ff_31 + ff_32,
            draws = draws,
            prior_n_turn = 15.5, prior_n_cands = 20.0
        )

    list(
        simple_pres = m_simple_pres,
        simple_pred = m_simple_pred,
        simple_mult = m_simple_mult,
        full = m_full
    )
}


enr_to_prec_ct = function(x) {
  x = round(exp(mean(log(x$post_turn))))
  if_else(x == 0, 1, x)
}
