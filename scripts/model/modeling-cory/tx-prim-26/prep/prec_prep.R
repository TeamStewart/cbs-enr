# Compile covariates and build strata

suppressPackageStartupMessages({
    library(tidyverse)
    library(enightmodels)
})

d_gs = cbs_counties |>
    filter(state == "TX") |>
    mutate(
        county = toupper(county),
        county_cbs = str_pad(county_cbs, 3, pad = "0"),
    ) |>
    select(county, county_cbs, geostratum)
d_24 = nanoparquet::read_parquet("data/county_long.parquet") |>
    filter(state == "TX", contest == "pres", year(election) == 2024) |>
    filter_asof() |>
    as_tibble() |>
    summarize(votes = sum(votes), .by = c(party, geo)) |>
    mutate(county_cbs = substring(geo, 7)) |>
    left_join(d_gs, join_by(county_cbs)) |>
    mutate(county = coalesce(county, "LA SALLE")) |>
    pivot_wider(names_from = party, values_from = votes) |>
    transmute(county = county, dem24 = dem / (dem + rep))
d_cd = read_csv("2026/tx-prim-26/data/TX_20251124_precincts_cd.csv", col_types = "_ci_c_")
d_early = read_csv("2026/tx-prim-26/data/tx_20251124_ballots_prec_20260302.csv", col_types = "___cii") |>
    rename(dprimary26_adv = early_dem, rprimary26_adv = early_rep)

# note: other includes asian
raw = read_csv("2026/tx-prim-26/data/tx_20251124_demo_prec_20260220.csv.gz", col_types = "ccc") |>
    rename_demo_cbs() |>
    select(-starts_with("p_"), -matches("_(male|fem)$"), -ind:-rep) |>
    mutate(across(male:other_col, ~ .x / n)) |>
    mutate(
        geo = precinct_cbs,
        .before = everything()
    ) |>
    left_join(d_gs, by = "county") |>
    left_join(d_cd, by = c("county", "precinct_cbs")) |>
    left_join(d_early, by = "precinct_cbs") |>
    select(-precinct_cbs, -precinct, -county_cbs)

SW_TOT_D = 2.5e6
SW_TOT_R = 2.45e6
SW_ADV_D = 1.5e6
SW_ADV_R = 1.3e6
RESC_D = sum(raw$dprimary26_adv, na.rm=TRUE) / SW_ADV_D
RESC_R = sum(raw$rprimary26_adv, na.rm=TRUE) / SW_ADV_R

# bayes est of prec % adv
est_pct_adv_d24 = (3 + raw$dprimary24_adv) / (5 + raw$dprimary24)
est_pct_adv_r24 = (2.5 + raw$rprimary24_adv) / (5 + raw$rprimary24)
# initial log-shifted total turnout
est_tot_d = raw$dprimary20
est_tot_r = raw$rprimary22
est_tot_d = est_tot_d * SW_TOT_D / sum(est_tot_d)
est_tot_r = est_tot_r * SW_TOT_R / sum(est_tot_r)
# fill in missing actual adv (observed adv is ~ 85% of final adv, we think)
est_adv_d = 1 + coalesce(raw$dprimary26_adv / RESC_D, est_tot_d * est_pct_adv_d24)
est_adv_r = 1 + coalesce(raw$rprimary26_adv / RESC_R, est_tot_r * est_pct_adv_r24)
# geometric wtd mean of past total and rescaled adv
est_tot_r = 1 + exp_s(0.7*log_s(raw$rprimary22) + 0.3*log_s(est_adv_r / est_pct_adv_r24))
est_tot_d = 1 + exp_s(0.7*log_s(raw$dprimary20) + 0.3*log_s(est_adv_d / est_pct_adv_d24))

for (i in 1:10) {
    # if tot > n, scale down to n
    bad = which(raw$n < est_tot_d + est_tot_r)
    est_tot_d[bad] = raw$n[bad] * est_tot_d[bad] / (est_tot_d[bad] + est_tot_r[bad])
    est_tot_r[bad] = raw$n[bad] * est_tot_r[bad] / (est_tot_r[bad] + est_tot_r[bad])

    # if adv > tot, don't extrapolate based on RESC
    bad_d = which(est_adv_d >= est_tot_d & est_adv_d > 0)
    bad_r = which(est_adv_r >= est_tot_r & est_adv_r > 0)
    est_adv_d[bad_d] = coalesce(1 + raw$dprimary26_adv, est_tot_d * est_pct_adv_d24)[bad_d]
    est_adv_r[bad_r] = coalesce(1 + raw$rprimary26_adv, est_tot_r * est_pct_adv_r24)[bad_r]
    # replace tot with rescaled adv estimate
    bad_d = which(est_adv_d >= est_tot_d & est_adv_d > 0)
    bad_r = which(est_adv_r >= est_tot_r & est_adv_r > 0)
    est_tot_d[bad_d] = est_adv_d[bad_d] / est_pct_adv_d24[bad_d]
    est_tot_r[bad_r] = est_adv_r[bad_r] / est_pct_adv_r24[bad_r]

    est_tot_d = est_tot_d * SW_TOT_D / sum(est_tot_d)
    est_tot_r = est_tot_r * SW_TOT_R / sum(est_tot_r)
    est_adv_d = est_adv_d * SW_ADV_D / sum(est_adv_d)
    est_adv_r = est_adv_r * SW_ADV_R / sum(est_adv_r)
}
raw$est_tot_d = est_tot_d
raw$est_tot_r = est_tot_r
raw$est_adv_d = est_adv_d
raw$est_adv_r = est_adv_r

hist(est_adv_d / est_tot_d, breaks=100)
hist(est_adv_r / est_tot_r, breaks=100)
plot(est_adv_d / est_tot_d, est_pct_adv_d24, cex=0.5, col="#0002")
plot(est_adv_r / est_tot_r, est_pct_adv_r24, cex=0.5, col="#0002")
plot(est_tot_d / raw$n, est_tot_r / raw$n, cex=0.1, col="#0002")
plot(log(raw$dprimary20),     log(est_tot_d), cex=0.5, col="#0002"); abline(0, 1, col='red')
plot(log(raw$rprimary22),     log(est_tot_r), cex=0.5, col="#0002"); abline(0, 1, col='red')
plot(log(raw$dprimary20_adv), log(est_adv_d), cex=0.5, col="#0002"); abline(0, 1, col='red')
plot(log(raw$rprimary22_adv), log(est_adv_r), cex=0.5, col="#0002"); abline(0, 1, col='red')

d = raw |>
    mutate(
        # build strata by party
        stratum_demo = case_when(
            white > 0.5 & white_noncol > 0.5 * white ~ "white_noncol",
            white > 0.5 & white_noncol > 0.5 * white ~ "white_noncol",
            white > 0.5 ~ "white_col",
            black > 0.5 ~ "black",
            hisp > 0.5 ~ "hisp",
            TRUE ~ "other"
        ),
        dem_proxy = (1 + dprimary24) / (2 + dprimary24 + rprimary24),
        active = as.integer(ceiling(pmax(active, est_tot_d + est_tot_r))),
        n = pmax(n, active)
    ) |>
    left_join(d_24, by = "county") |>
    mutate(
        dem_proxy = logit_shift(dem_proxy, dem24[1], w = vote24),
        .by = county
    ) |>
    select(
        -dprimary20:-rprimary24_adv,
        -vote10:-vote24,
        geostratum,
        dprimary20,
        rprimary22,
        dem24_est = dem_proxy,
        dem24_cty = dem24
    ) |>
    relocate(active, est_tot_d, est_tot_r, est_adv_d, est_adv_r, .after = n) |>
    relocate(cd, contains("stratum"), .before = n)

nanoparquet::write_parquet(d, "2026/tx-prim-26/data/prec_covs.parquet")

# demos
summarize(d, across(male:other_col, ~ weighted.mean(.x, n))) |>
    glimpse()
