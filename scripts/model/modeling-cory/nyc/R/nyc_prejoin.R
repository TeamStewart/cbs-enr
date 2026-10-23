# setwd("gen-25")
here::i_am("R/test_nyc.R")
suppressPackageStartupMessages({
    library(here)
    library(readr)
    library(nanoparquet)
    library(stringr)
    library(sf)
    library(geomander)
    devtools::load_all("../enightmodels")
    library(dplyr)
})

# load tabular data ---------
d_l2 = read_csv(
    here("data/nyc/NYC_20251009_demo_prec_20251014.csv"),
    col_types = spec_l2()
) |>
    rename_demo_cbs() |>
    select(ad, ed, county, n, active, starts_with("p_"), general21:primary25_dem, starts_with("county_"))
xw_boro = select(d_l2, ad, ed, county)

d_income = read_csv(here("data/nyc/income.csv"), show_col_types = FALSE)
names(d_income) = c("ElectDist", paste0("inc_", str_pad(0:16, 2, pad="0")))
d_income = d_income |>
    mutate(
        ad = as.integer(floor(ElectDist / 1e3)),
        ed = str_sub(as.character(floor(ElectDist)), 3),
        across(starts_with("inc_"), ~ .x * 2),
        .before = everything()
    ) |>
    select(-inc_00, -ElectDist) |>
    semi_join(d_l2, by = join_by(ad, ed)) |>
    mutate(
        inc_000_029 = inc_01 + inc_02 + inc_03 + inc_04 + inc_05,
        inc_030_039 =  inc_06 + inc_07,
        inc_040_059 = inc_08 + inc_09,
        inc_060_074 = inc_10 + inc_11,
        inc_075_149 = inc_12 + inc_13 + inc_14,
        inc_150_199 = inc_15,
        inc_200_999 = inc_16
    ) |>
    select(-c(inc_01:inc_16))

d_rent = read_csv(here("data/nyc/renter.csv"), show_col_types = FALSE) |>
    mutate(
        ad = as.integer(floor(ElectDist / 1e3)),
        ed = str_sub(as.character(floor(ElectDist)), 3),
        .before = everything()
    ) |>
    select(-ElectDist) |>
    semi_join(d_l2, by = join_by(ad, ed))

turnout_exp = c(
    "NEW YORK" = 570745,
    "BRONX" = 240656,
    "KINGS" = 734374,
    "QUEENS" = 540871,
    "RICHMOND" = 159018
)
turnout_exp = turnout_exp / tibble::deframe(count(d_l2, county, wt = n))[names(turnout_exp)]
vs_exp = list(m = 0.47, c = 0.38, s = 0.12)

d_l2_turn = read_csv(
    here("data/nyc/NYC_20251009_demo_prec_turnout25_20251104.csv"),
    show_col_types = FALSE
) |>
    filter(!str_ends(precinct_cbs, "_NA")) |>
    mutate(
        ad = suppressWarnings(as.integer(str_sub(precinct_cbs, 7, 8))),
        ed = str_sub(precinct_cbs, 9, 11),
        pred_turn = n * logit_shift(pred.turnout / n, turnout_exp[county[1]], n),
        .by = county
    ) |>
    mutate(
        pred_mamdani = logit_shift(pred.mamdani / pred.turnout, vs_exp$m, pred_turn),
        pred_cuomo = logit_shift(pred.cuomo / pred.turnout, vs_exp$c, pred_turn),
        pred_sliwa = logit_shift(pred.sliwa / pred.turnout, vs_exp$s, pred_turn),
    ) |>
    select(ad, ed, turn_early=earlyIP, starts_with("pred_"))

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
    mutate(
        across(
            starts_with("prim25_cvr_") & !prim25_cvr_total,
            ~ coalesce(.x / prim25_cvr_total, 0)
        ),
        pres24_2way_dem = (1e-6 + pct_potus_24_dem) / (2e-6 + pct_potus_24_dem + pct_potus_24_rep),
    ) |>
    semi_join(d_l2, by = join_by(ad, ed))
d_cuomo = read_csv(here("data/nyc/cuomo_diff.csv"), col_types=cols(ad="i", ed="c", cuomo_diff="d")) |>
    semi_join(d_l2, by = join_by(ad, ed))

d_2020 = read_csv(here("data/nyc/pre_20_at_nyed_25c.csv"), show_col_types = FALSE) |>
    transmute(
        ad = as.integer(floor(ElectDist / 1e3)),
        ed = str_sub(as.character(floor(ElectDist)), 3),
        votes_total_20 = pre_20_rep_tru + pre_20_dem_bid,
        pres20_2way_dem = (1e-6 + pre_20_dem_bid) / (2e-6 + votes_total_20),
    ) |>
    semi_join(d_l2, by = join_by(ad, ed))

# geographic features ---------
if (!file.exists(path <- here("data/nyc/nyc_fourier.parquet"))) {
    d_ed = read_sf("data/nyc/geo/nyed_25b") |>
        transmute(
            ad = as.integer(floor(ElectDist / 1e3)),
            ed = str_sub(as.character(ElectDist), 3),
            geometry = geometry
        )
    keep = paste0(d_ed$ad, "-", d_ed$ed) %in% paste0(d_past$ad, "-", d_past$ed)
    ed_adj = adjacency(d_ed)
    ed_adj = ed_adj |>
        add_edge(1537, 2708) |>
        add_edge(1752, 2763) |>
        add_edge(1832, 2874) |>
        add_edge(1835, 2787) |>
        add_edge(213, 249) |>
        add_edge(227, 3871) |>
        add_edge(249, 250) |>
        add_edge(2512, 2626) |>
        add_edge(2513, 2522) |>
        add_edge(2513, 2524) |>
        add_edge(2513, 2525) |>
        add_edge(2692, 2716) |>
        add_edge(290, 3879) |>
        add_edge(2961, 2970) |>
        add_edge(2961, 4251) |>
        add_edge(2970, 4036) |>
        add_edge(2970, 4251) |>
        add_edge(3, 2368) |>
        add_edge(3018, 4045) |>
        add_edge(3018, 4052) |>
        add_edge(3141, 4017) |>
        add_edge(3149, 4045) |>
        add_edge(3178, 3835) |>
        add_edge(3208, 3581) |>
        add_edge(3231, 3584) |>
        add_edge(3245, 3248) |>
        add_edge(3257, 4185) |>
        add_edge(3289, 3590) |>
        add_edge(33, 67) |>
        add_edge(3526, 3575) |>
        add_edge(3561, 3579) |>
        add_edge(3634, 4239) |>
        add_edge(3835, 3836) |>
        add_edge(3934, 4245) |>
        add_edge(3934, 4246) |>
        add_edge(3935, 3939) |>
        add_edge(3935, 4245) |>
        add_edge(3936, 4245) |>
        add_edge(3937, 4245) |>
        add_edge(3938, 3940) |>
        add_edge(3939, 3940) |>
        add_edge(4115, 4117) |>
        add_edge(4244, 4245) |>
        add_edge(58, 70) |>
        add_edge(712, 4111) |>
        add_edge(827, 828) |>
        add_edge(839, 2970) |>
        add_edge(874, 3575) |>
        add_edge(885, 3576) |>
        add_edge(905, 1703) |>
        add_edge(905, 3381) |>
        add_edge(914, 947) |>
        add_edge(915, 1706) |>
        add_edge(915, 1765) |>
        add_edge(943, 1765) |>
        add_edge(969, 1981) |>
        add_edge(972, 3578)
    ed_adj = lapply(ed_adj, unique)
    stopifnot(all(geomander::check_contiguity(ed_adj)$component == 1))

    ed_adj_keep = redist::redist.reduce.adjacency(ed_adj, which(keep))
    ed_adj_keep = ed_adj_keep |>
        add_edge(13, 17) |>
        add_edge(33, 58) |>
        add_edge(491, 507) |>
        add_edge(1427, 2607) |>
        add_edge(1421, 2606) |>
        add_edge(3127, 3968) |>
        add_edge(3166, 3462) |>
        add_edge(821, 3219) |>
        add_edge(747, 780)
    stopifnot(all(geomander::check_contiguity(ed_adj_keep)$component == 1))

    m = diag(lengths(ed_adj_keep))
    for (i in seq_along(ed_adj_keep)) {
        nbors = ed_adj_keep[[i]]
        m[i, nbors + 1L] = -1L
    }
    stopifnot(all(colSums(m) == 0))

    eig = eigen(m, TRUE)
    idx = order(eig$values)
    eig$values = eig$values[idx]
    eig$vectors = eig$vectors[, idx]
    m_fourier = eig$vectors[, 1 + 1:64]
    colnames(m_fourier) = sprintf("ff_%02d", 1:ncol(m_fourier))

    d_fourier = st_drop_geometry(d_ed[keep, ]) |>
        bind_cols(as_tibble(m_fourier))
    write_parquet(d_fourier, path)
} else {
    d_fourier = read_parquet(path)
}

# join -------
join_verb = join_exact
join_verb(d_past, d_2020, join_by(ad, ed)) |>
    join_verb(d_cuomo, join_by(ad, ed)) |>
    join_verb(d_l2, join_by(ad, ed, county)) |>
    join_verb(d_l2_turn, join_by(ad, ed)) |>
    join_verb(d_income, join_by(ad, ed)) |>
    join_verb(d_rent, join_by(ad, ed)) |>
    join_verb(d_fourier, join_by(ad, ed)) |>
    select(-vote_mode) |>
    write_parquet(here("data/nyc/nyc_pre.parquet"))

# save precs -------
if (FALSE) {
    d_ed |>
        inner_join(d_l2_turn, join_by(ad, ed)) |>
        join_exact(d_past, join_by(ad, ed)) |>
        mutate(x = pmin(pred_turn / (1 + votes_total_24), 1)) |>
        mapgl::maplibre_view(column = "x")

    d_ed |>
        filter(keep) |>
        rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE) |>
        transmute(prec = paste0(ad, "-", ed), geometry = geometry) |>
        sf::st_transform(4326) |>
        sf::st_make_valid() |>
        yyjsonr::write_geojson_file(here("reports/data/nyc_precs.geojson"))
}