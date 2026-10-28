suppressWarnings(suppressPackageStartupMessages({
    setwd(here::here())
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(purrr)
    library(lubridate)
    library(nanoparquet)
    library(enightmodels)
    invisible(lapply(Sys.glob("2026/tx-prim-26/R/*.R"), source))

    library(ggplot2)
}))

specs = list(
    nonpara = c("tpsob_g", "tpsob", "ratio"),
    strat_lin = c("strat_dg_cov", "strat_d_cov", "ratio"),
    strat = c("strat_dg", "strat_d", "ratio"),
    ratio = "ratio"
)
src = 0
dry = TRUE
by_method = TRUE

d_cov = read_parquet("2026/tx-prim-26/data/prec_covs.parquet")

d = load_data(Sys.time(), fake = FALSE)


d$prec |>
    summarize(votes = sum(votes), .by = c(method, county, contest)) |>
    pivot_wider(names_from=c(contest, method), values_from=votes) |>
    print(n=30)
d$cty |>
    summarize(votes = sum(votes), .by = c(method, county, contest)) |>
    pivot_wider(names_from=c(contest, method), values_from=votes) |>
    filter(sen_dem_eday > 0) |>
    print(n=30)

precinct_partial(d, d_cov)
precinct_matches(d, d_cov) |>
  print(n=32)


d$prec |>
    filter(contest == "house_dem", district == 33) |>
    # summarize(votes = sum(votes), .by = c(candidate, method)) |>
    # mutate(tot = sum(votes), votes = votes / sum(votes), .by=method) |>
    # arrange(method, desc(votes))

    filter(votes > 2) |>
    filter(n_distinct(method) == 2, .by=geo) |>
    mutate(votes = votes / sum(votes), .by=c(geo, method)) |>
    pivot_wider(names_from=method, values_from = votes) |>
    drop_na() |>
ggplot(aes(adv, eday)) +
    geom_point()

    mutate(tot = sum(votes), votes = votes / sum(votes), .by=method) |>
    summarize(votes = sum(votes), .by = c(candidate, method)) |>
    print()

    mutate(tot = sum(votes), votes = votes / sum(votes), .by=method)


# set up sen_dem
d_cov = d_cov_e
d_race = (d$prec |>
        nest(.by = all_of(RACE_ID)) |>
        cross_join(tibble(model_nm = names(l_form))))$data[[7]]
contest = "sen_dem"
district = NA
stage_done = 6L
turn_ratio = 0.95
conf_level = 0.95
by_method = TRUE
