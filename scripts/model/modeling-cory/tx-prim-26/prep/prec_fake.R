suppressPackageStartupMessages({
    library(tidyverse)
    library(nanoparquet)
    source("2026/tx-prim-26/R/utils.R")
    source("2026/tx-prim-26/R/scrape_cty.R")
    library(enightmodels)
})

d_prec = read_parquet("2026/tx-prim-26/data/prec_covs.parquet")

# s3_paths = get_s3_paths()
# ex_prec = read_parquet_bucket("Precincts/20260303-TX/TX_tarrant_results.parquet")
url_cty_r = "https://goelect.txelections.civixapps.com/api-ivis-system/api/s3/enr/election/countyInfo/53813"
url_cty_d = "https://goelect.txelections.civixapps.com/api-ivis-system/api/s3/enr/election/countyInfo/53814"
ex_cty = bind_rows(
    scrape_tx_county(url_cty_d, "2026-03-03"),
    scrape_tx_county(url_cty_r, "2026-03-03")
)

d_cand_fake = tribble(
    ~ candidate, ~ pct,
    "Cornyn, John", 0.38,
    "Paxton, Ken", 0.33,
    "Hunt, Wesley", 0.19,
    "Daniels, Sholdon", 0.52,
    "Heise, Gregor", 0.41,
    "Crockett, Jasmine", 0.51,
    "Talarico, James", 0.47,
    "Haynes, Frederick", 0.65
)
d_race_cand = distinct(ex_cty, contest, district, candidate) |>
    filter(str_starts(contest, "sen_") | district %in% c(30L, 33L)) |>
    left_join(d_cand_fake, by = "candidate") |>
    mutate(
        pct = coalesce(pct, (1 - sum(pct, na.rm = TRUE)) * (1:n()) / sum(is.na(pct) * (1:n()))),
        .by = c(contest, district)
    )

rep_int = 10 # min
rep_len = 5 * 60 # min
times = ymd_hms("2026-03-03 20:00:00") + seq(0, rep_len, by=rep_int) * 60
n_times = length(times)

d_cty_time = distinct(d_prec, county) |>
    mutate(
        start_time = sample.int(9, n(), replace = TRUE) +
            (county %in% c("EL PASO", "HUDSPETH")) * (60 / rep_int)
    )

d_prec_votes = d_prec |>
    mutate(
        lp_rep = scale(log10(1 + est_tot_r + est_tot_d)) +
            -0.25 * scale(white) -
            0.25 * scale(col) +
            rnorm(n()),
        lp_turn = c(scale(0.25 * scale(black) + 0.5 * scale(col) + rnorm(n()))),
        lp_adv = c(scale(0.25 * scale(black) + 0.5 * scale(col) + rnorm(n()))),
        tot_d = round(est_tot_d * 1.05 * (1 + 0.1 * lp_turn)),
        tot_r = round(est_tot_r * 0.90 * (1 + 0.1 * lp_turn)),
        adv_d = round(est_adv_d * 1.10 * (1 + 0.1 * lp_turn + 0.1 * lp_adv)),
        adv_r = round(est_adv_r * 1.05 * (1 + 0.1 * lp_turn + 0.1 * lp_adv)),
        adv_d = pmin(adv_d, tot_d),
        adv_r = pmin(adv_r, tot_r),
    ) |>
    left_join(d_cty_time, by = "county") |>
    mutate(
        rep_time = as.integer(round(start_time + cume_dist(lp_rep)^3 * (n_times - start_time))),
        rep_time_mit = if_else(
            county %in% tgt_mit & runif(n()) <= 0.9,
            pmax(rep_time + sample(c(-1L, 0L, 1L), n(), replace = TRUE), 0L),
            n_times + 2L,
        ),
        .by = county
    ) |>
    select(geo, county, cd, contains("stratum"), tot_d:adv_r, rep_time, rep_time_mit) |>
    cross_join(d_race_cand) |>
    cross_join(tibble(method = c("adv", "eday"))) |>
    mutate(ratio_demo_str = rnorm(1, 1, 0.15), .by = c(contest, district, candidate, stratum_demo)) |>
    mutate(ratio_geo_str = rnorm(1, 1, 0.15), .by = c(contest, district, candidate, geostratum)) |>
    mutate(ratio_method = rnorm(1, 1, 0.15), .by = c(contest, district, candidate, method)) |>
    mutate(ratio_district = rnorm(1, 1, 0.1), .by = c(contest, district, candidate, cd)) |>
    mutate(
        state = factor("TX"),
        election = ymd("2026-03-03"),
        jurisdiction = factor("tx"),
        party = if_else(str_ends(contest, "_rep"), "rep", "dem"),
        is_house = str_starts(contest, "house_"),
        ratio = ratio_demo_str * ratio_geo_str * ratio_method,
        ratio = if_else(is_house, ratio_district * ratio, ratio),
        pct = clamp(pct * ratio, 0.0, 0.99),
        votes = case_when(
            is_house & district != cd ~ NA_integer_,
            method == "adv" & party == "rep" ~ rbinom(n(), adv_r, pct),
            method == "adv" & party == "dem" ~ rbinom(n(), adv_d, pct),
            method == "eday" & party == "rep" ~ rbinom(n(), tot_r - adv_r, pct),
            method == "eday" & party == "dem" ~ rbinom(n(), tot_d - adv_d, pct)
        ),
        rep_time = if_else(method == "adv", pmax(rep_time - 1L, 0L), pmin(rep_time + 1L, n_times)),
        rep_time_mit = if_else(method == "adv", pmax(rep_time_mit - 1L, 0L), pmin(rep_time_mit + 1L, n_times + 2L)),
    ) |>
    select(-contains("stratum"), -contains("ratio"), -is_house) |>
    drop_na(votes) |>
    select(any_of(COLS_ELEC), county, rep_time, rep_time_mit) |>
    mutate(across(candidate:geo, as.factor)) |>
    as_tibble()

write_parquet(d_prec_votes, fake_path)
