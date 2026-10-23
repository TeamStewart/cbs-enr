# setwd("gen-25")
here::i_am("R/test_nyc.R")
suppressPackageStartupMessages({
    library(here)
    library(readr)
    library(stringr)
    library(posterior)
    devtools::load_all("../enightmodels")
    library(dplyr)
})

# load data ------
d_live_raw = read_csv_bucket(
    "Precincts/20251105-NY-M/NY_M_results.csv",
    col_types = spec_live()
)

d_past = read_csv(
    here("data/nyc/NY_history.csv"),
    col_types = spec_past()
) |>
    rename_with(
        ~ str_replace(.x, "votePct", "pct"),
        starts_with("votePct")
    ) |>
    rename_with(
        ~ str_replace(.x, "precFinal", "total"),
        contains("precFinal")
    ) |>
    rename_with(
        ~ str_replace(.x, "^mayor_", "prim25_cvr_"),
        starts_with("mayor_")
    ) |>
    mutate(across(
        starts_with("prim25_cvr_") & !prim25_cvr_total,
        ~ coalesce(.x / prim25_cvr_total, 0)
    ))

d_l2 = read_csv(
    here("data/nyc/NYC_20250809_demo_prec_20250812.csv"),
    col_types = spec_l2()
) |>
    rename_demo_cbs() |>
    select(county, ad, ed, n, active, starts_with("p_"), general21:primary25_dem)

# join data -------
cands = c(
    mamdani = "Zohran Mamdani",
    sliwa = "Curtis Sliwa",
    cuomo = "Andrew Cuomo"
)

d_live = fmt_wide(
    d_live_raw,
    vote_col = precinct_total,
    cand_col = candidate_name,
    cand_spec = cands,
    id_cols = c(jurisdiction, ad, ed, virtual_precinct, vote_mode)
)

stopifnot(nrow(d_live) == n_distinct(d_live_raw[c("ad", "ed")]))
stopifnot(all(names(cands) %in% names(d_live)))
stopifnot(all(d_live[c("mamdani", "sliwa", "cuomo")] >= 0))
stopifnot(all(d_live[c("mamdani", "sliwa", "cuomo")] <= 1))

d = d_live |>
    add_obs_test_ind(4 * mamdani, prop = 0.25) |>
    join_exact(d_past, by = join_by(ad, ed)) |>
    join_exact(d_l2, by = join_by(ad, ed))

# analyze data -------

data = select(d, ad, ed, obs, mamdani, prim25_cvr_top_m, votes_total_21, p_white, total)
if (FALSE) {
    nyc = d_live |>
        add_obs_test_rank(8 * mamdani) |>
        join_exact(d_past, by = join_by(ad, ed)) |>
        join_exact(d_l2, by = join_by(ad, ed)) |>
        select(ad, ed, obs, mamdani, prim25_cvr_top_m, votes_total_21, p_white, total)
    save(nyc, file = "../enightmodels/data/nyc.rda", compress = "xz")
}

data$other = 1 - data$mamdani
data$other_prim = 1 - data$prim25_cvr_top_m
data$total = coalesce(data$total, 0)
forecast = forecast_prec(
    data = data,
    cands = c(mamdani, other),
    turn = total,
    obs = obs < 0.25,
    ctrl_form = ~ log_s(votes_total_21) + prim25_cvr_top_m + p_white,
    prior_turn = votes_total_21,
    prior_cands = c(prim25_cvr_top_m, other_prim),
    prior_n_turn = 0.5,
    prior_n_cands = 1,
)

f_sum = summarize_forecast(forecast)
f_sum
forecast_intervals(f_sum, 0.99)
