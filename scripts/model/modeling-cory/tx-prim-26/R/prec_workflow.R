#' Run precinct models for the 2026 TX primaries
#'
#' @param specs List of character vectors. Each element will correspond to a
#'   model being run. Within each element, the first named model will be tried
#'   first, and if it does not succeed, the next model will be tried. Possible
#'   models: `tpsob_g`, `tpsob`, `strat_dg_cov`, `strat_d_cov`, `strat_dg`, `strat_d`, or `ratio`.
#' @param src If `0`, get current data from server. Otherwise, get the numbered
#'   timestamp from the fake data file.
#' @param dry If `TRUE`, do not save output locally.
run_workflow = function(
    specs = list(
        nonpara = c("tpsob_g", "tpsob", "ratio"),
        strat_lin = c("strat_dg_cov", "strat_d_cov", "ratio"),
        strat = c("strat_dg", "strat_d", "ratio"),
        ratio = "ratio"
    ),
    src = 0,
    dry = FALSE
) {
    now = if (src == 0) Sys.time() else fake_times[src]
    specs = lapply(specs, \(x) rlang::arg_match(x, names(model_specs), multiple = TRUE))

    d = load_data(now, fake = src > 0)
    d_cov = read_parquet("2026/tx-prim-26/data/prec_covs.parquet")
    cli::cli_alert_success("{if (src > 0) 'FAKE DATA' else 'Data'} loaded for {now}")

    rep_qc = precinct_qc(d, d_cov)
    rep_match = precinct_matches(d, d_cov)
    rep_partial = precinct_partial(d, d_cov)
    qc_fn = if (sum(rep_qc$counties, na.rm=TRUE) > 1) cli::cli_alert_warning else cli::cli_alert_success
    qc_fn("Quality checks complete:")
    print(as.data.frame(select(rep_qc, issue, counties, precincts)), row.names = FALSE)
    cli::cli_alert_info(
        "{round(sum(rep_match$match_rate * rep_match$n))} of {sum(rep_match$n)} precincts matched."
    )

    res = run_model_prec(d, d_cov, now, specs, by_method = TRUE, turn_ratio = 0.95)
    cli::cli_alert_success("Precinct models fit")
    res_cty = run_model_cty(d, d_cov, now, by_method = TRUE, turn_ratio = 0.95)
    res = bind_rows(res, res_cty)
    cli::cli_alert_success("County models fit")

    ei = run_ei(d, d_cov, ei_demos, min_stage = 2L)
    cli::cli_alert_success("EI run")

    if (isFALSE(dry)) {
        write_output(res, ei, rep_qc, rep_match, rep_partial, d, d_cov)
        cli::cli_alert_success("Output saved locally")
    } else {
        cli::cli_alert_info("Skipping saving output")
    }

    invisible(res)
}

model_specs = list(
    tpsob_g = ~ z + geostratum,
    tpsob = ~ z,
    strat_dg_cov = ~ (1 | stratum_demo) + (1 | geostratum) +
        dem24_est + I(dem24_est^2) + age_18_29 + age_65_up + white_noncol + white_col,
    strat_d_cov = ~ (1 | stratum_demo) + dem24_est + I(dem24_est^2) +
        age_18_29 + age_65_up + white_noncol + white_col,
    strat_dg = ~ (1 | stratum_demo) + (1 | geostratum),
    strat_d = ~ (1 | stratum_demo),
    ratio = ~ 1
)

ei_demos = list(
    age = c("age_18_29", "age_30_44", "age_45_64", "age_65_up" ),
    race_edu = c("white_col", "white_noncol", "black", "hisp", "other")
)

run_model_prec <- function(d, d_cov, time, specs, by_method = TRUE, turn_ratio = 1.0) {
    covs = c("age_18_29", "age_65_up", "white_col", "white_noncol", "black", "hisp", "dem24_est")
    l_form = lapply(specs, \(s) model_specs[s])

    d_cov_e = d_cov |>
        as_tibble() |>
        select(geo, county, cd, contains("stratum"), starts_with("est_"), all_of(covs)) |>
        cross_join(tibble(method = c("adv", "eday", "all"))) |>
        cross_join(tibble(party = c("dem", "rep"))) |>
        mutate(
            est = case_when(
                party == "dem" & method == "all" ~ est_tot_d,
                party == "dem" & method == "adv" ~ est_adv_d,
                party == "dem" & method == "eday" ~ est_tot_d - est_adv_d,
                party == "rep" & method == "all" ~ est_tot_r,
                party == "rep" & method == "adv" ~ est_adv_r,
                party == "rep" & method == "eday" ~ est_tot_r - est_adv_r
            ),
            z = bases::b_tpsob(!!!syms(covs), p = 43),
            across(contains("stratum"), ~ as.factor(.x)),
            .after = stratum_demo,
        ) |>
        select(-starts_with("est_"))
    # help with bases:
    # cumsum(vapply(1:7, \(d) nrow(bases:::mult_partition(d, 7)), 0L))

    quiet_model = function(...) {
        quietly(model_prec_pois)(..., by_method = by_method, turn_ratio = turn_ratio)$result
    }
    d_models = d$prec |>
        nest(.by = all_of(RACE_ID)) |>
        cross_join(tibble(model_nm = names(l_form))) |>
        mutate(
            model = pmap(
                list(data, model_nm, contest, district),
                \(d, n, cc, dd) quiet_model(d, d_cov_e, cc, dd, l_form[[n]]),
                .progress = list(format="Fitting model {cli::pb_current}/{cli::pb_total} {cli::pb_bar} [{cli::pb_elapsed}]")
            ),
        ) |>
        select(-data)
    if (all(map_lgl(d_models$model, is.null))) {
        return(NULL)
    }

    d_coef = d_models |>
        mutate(model = map(model, ~ .x$coef)) |>
        unnest(model) |>
        mutate(
            time = .env$time,
            model = paste0("prec/", if_else(by_method, "method/", "all/"), model_nm, "/", model_spec)
        ) |>
        select(-model_spec, -model_nm) |>
        relocate(where(is.numeric), any_of(c("estimate", "std.error")), .after = where(is.character)) |>
        nest(.by = c(all_of(RACE_ID), time, model), .key = "coef")

    d_pred = d_models |>
        mutate(model = map(model, ~ .x$pred)) |>
        unnest(model) |>
        mutate(
            time = .env$time,
            model = paste0("prec/", if_else(by_method, "method/", "all/"), model_nm, "/", model)
        ) |>
        select(-model_nm) |>
        nest(.by = c(all_of(RACE_ID), time, pred_type, model, model_conf), .key = "pred")

    left_join(d_pred, d_coef, by = c(RACE_ID, "time", "model"))
}

run_model_cty <- function(d, d_cov, time, by_method = TRUE, turn_ratio = 1.0) {
    d_cov_cty = d_cov |>
        as_tibble() |>
        summarize(
            est_adv_d = sum(est_adv_d),
            est_adv_r = sum(est_adv_r),
            est_tot_d = sum(est_tot_d),
            est_tot_r = sum(est_tot_r),
            .by = c(county, geostratum)
        )

    d_cty = d$cty |>
        filter(!grepl("^house_", contest)) |>
        left_join(d_cov_cty, by = "county") |>
        mutate(
            est = case_when(
                party == "dem" & method == "adv" ~ est_adv_d,
                party == "dem" & method == "eday" ~ est_tot_d - est_adv_d,
                party == "dem" & method == "all" ~ est_tot_d,
                party == "rep" & method == "adv" ~ est_adv_r,
                party == "rep" & method == "eday" ~ est_tot_r - est_adv_r,
                party == "rep" & method == "all" ~ est_tot_r
            ) / vctrs::vec_unique_count(pick(candidate, party)),
            .by = c(all_of(RACE_ID))
        ) |>
        select(-starts_with("est_"))

    config = model_legacy_config(by_method = by_method, strata = "geostratum", tr_pre = turn_ratio)
    models = group_split(d_cty, !!!syms(RACE_ID)) |>
        map(~ model_legacy(.x, est, config))

    d_coef = models |>
        map(~ .x$coef) |>
        bind_rows() |>
        nest(.by = c(all_of(RACE_ID), time), .key = "coef")

    models |>
        map(~ .x$pred) |>
        bind_rows() |>
        mutate(outstd = pred - rep) |>
        rename(est = past) |>
        nest(.by = c(all_of(RACE_ID), time, pred_type, model, model_conf), .key = "pred") |>
        mutate(model = if (by_method) "cty/method/legacy" else "cty/all/legacy") |>
        left_join(d_coef, by = c(RACE_ID, "time"))
}

run_ei <- function(d, d_cov, demos, min_stage = 4L) {
    covs = c(
        "age_18_29", "age_65_up", "white_col", "white_noncol",
        "black", "hisp", "male", "dem24_est", "dem24_cty", "geostratum"
    )
    make_run_spec <- function(data, demo) {
        if (length(data) == 0 || nrow(data) == 0) {
            return(NULL)
        }
        outcomes = setdiff(names(data), c("geo", "county"))

        data = inner_join(data, d_cov, by = c("geo", "county"))
        covs_use = setdiff(covs, demos[[demo]])
        if (n_distinct(data$geostratum) == 1L) {
            covs_use = setdiff(covs_use, "geostratum")
        }
        data |>
            mutate(
                n = pmax(n, rowSums(pick(all_of(outcomes))) + 1L),
                abstain = n - rowSums(pick(all_of(outcomes))),
                .after = all_of(outcomes)
            ) |>
            seine::ei_proportions(
                !!!syms(c(outcomes, "abstain")),
                .total = n,
            ) |>
            seine::ei_proportions(
                !!!syms(demos[[demo]]),
                .total = NULL,
            ) |>
            seine::ei_spec(
                predictors = all_of(demos[[demo]]),
                outcome = all_of(c(outcomes, "abstain")),
                total = n,
                covariates = all_of(covs_use)
            ) |>
            ei_run()
    }

    d$prec |>
        filter(stage >= min_stage, !is.na(party)) |>
        mutate(
            cand = paste0(party, "_", tolower(stringr::str_split_i(candidate, ", ", 1))),
        ) |>
        summarize(
            votes = sum(votes),
            .by = c(all_of(RACE_ID), county, geo, cand)
        ) |>
        nest(.by = all_of(RACE_ID)) |>
        mutate(data = map(data, ~ pivot_wider(.x, names_from = cand, values_from = votes, values_fill = 0))) |>
        cross_join(tibble(demo = names(demos))) |>
        mutate(
            res = map2(data, demo, possibly(~ make_run_spec(.x, .y)))
        ) |>
        select(-data)
}

load_data <- function(time, fake = FALSE) {
    if (fake) {
        list(
            prec = fake_data_prec(time),
            cty = fake_data_cty(time)
        )
    } else {
        url_cty_r = "https://goelect.txelections.civixapps.com/api-ivis-system/api/s3/enr/election/countyInfo/53813"
        url_cty_d = "https://goelect.txelections.civixapps.com/api-ivis-system/api/s3/enr/election/countyInfo/53814"

        d_prec = get_s3_paths() |>
            map(read_parquet_bucket, .progress = "Reading from S3") |>
            bind_rows() |>
            as_tibble() |>
            filter(method != "prov") |>
            filter(
                contest %in% c("sen_dem", "sen_rep") |
                contest %in% c("house_dem", "house_rep") & district %in% c(30, 33)
            ) |>
            mutate(
                across(c(jurisdiction, geo), ~ forcats::fct_relabel(.x, toupper)),
                candidate = forcats::fct_relabel(candidate, str_to_title),
                geo = forcats::fct_relabel(geo, function(geo_in) {
                    paste0(
                        cty_to_fips[cbs_to_cty[str_sub(geo_in, 4, 6)]],
                        "_",
                        str_sub(geo_in, 8)
                    )
                }),
                method = forcats::fct_collapse(
                    method,
                    adv = c("adv", "early", "mail"),
                    eday = "eday",
                    all = "all"
                ),
                county = unname(fips_to_cty[str_sub(geo, 3, 5)])
            ) |>
            summarize(
                votes = sum(votes),
                .by = c(all_of(RACE_ID), candidate, party, time, stage, method, geo, county)
            )

        d_cty = bind_rows(
            scrape_tx_county(url_cty_d, "2026-03-03"),
            scrape_tx_county(url_cty_r, "2026-03-03")
        ) |>
        filter(
            contest %in% c("sen_dem", "sen_rep") |
            contest %in% c("house_dem", "house_rep") & district %in% c(30, 33)
        ) |>
          mutate(county = fips_to_cty[str_sub(geo, 3)])

        list(
            prec = d_prec,
            cty = d_cty
        )
                }
}

build_summary = function(d) {
    rep_sum <- function(x) {
        x |>
            summarize(
                votes = sum(votes),
                n_rep = n_distinct(geo[stage > 0L]),
                n_complete = n_distinct(geo[stage > 6L]),
                n_units = n_distinct(geo),
                .by = c(all_of(RACE_ID), method)
            ) |>
            pivot_wider(names_from = method, values_from = votes)
    }

    bind_rows(
        Precincts = rep_sum(d$prec),
        Counties = rep_sum(d$cty),
        .id = "unit_type"
    )
}

write_output <- function(res, ei, rep_qc, rep_match, rep_partial, d, d_cov) {
    obj_full = list(
        models = res,
        summary = build_summary(d),
        ei = ei,
        qc = rep_qc,
        match = rep_match,
        partial = rep_partial
    )
    json_opts = yyjsonr::opts_write_json(digits_signif = 5L, auto_unbox = TRUE)
    yyjsonr::write_json_file(obj_full, "2026/tx-prim-26/data-out/current.json", opts = json_opts)
    log_ndjson(obj_full, "2026/tx-prim-26/data-out/log_full.ndjson", opts = json_opts)
    log_ndjson(select(res, -coef), "2026/tx-prim-26/data-out/log_preds.ndjson", opts = json_opts)

    d_prec_tot = d$prec |>
        summarize(votes = sum(votes), .by = c(-votes, -method, -candidate, -party)) |>
        mutate(district = coalesce(district, 0L)) |>
        select(-state, -election, -jurisdiction, -time, -stage) |>
        pivot_wider(names_from = c(contest, district), values_from = votes)
    d_prec_method = d$prec |>
        summarize(votes = sum(votes), .by = c(all_of(RACE_ID), geo, method)) |>
        summarize(votes = max(votes), .by = c(geo, method)) |>
        pivot_wider(names_from = method, values_from = votes)

    d_prec_out = d$prec |>
        summarize(votes = sum(votes), .by = c(-votes, -method)) |>
        mutate(
            candidate = tolower(stringr::str_split_i(candidate, ", ", 1)),
            district = coalesce(district, 0L)
        ) |>
        select(-state, -election, -jurisdiction, -party, -time) |>
        mutate(stage = max(stage), .by = geo) |>
        mutate(votes = votes / sum(votes), .by = c(contest, district, geo)) |>
        pivot_wider(names_from = c(contest, district, candidate), values_from = votes) |>
        inner_join(d_cov, join_by(geo, county)) |>
        left_join(d_prec_tot, join_by(geo, county)) |>
        left_join(d_prec_method, join_by(geo)) |>
        select(
            -active,
            -male,
            -fem,
            -black_noncol:-other_noncol,
            -black_col:-other_col,
            -dem24_cty
        ) |>
        relocate(
            geo,
            county,
            cd,
            geostratum,
            stratum_demo,
            stage,
            all_of(colnames(d_prec_tot)),
            .before = everything()
        ) |>
        mutate(across(where(is.double), ~ round(.x, digits = 3)))
    readr::write_csv(d_prec_out, "2026/tx-prim-26/data-out/precincts.csv", na="")
}


precinct_qc = function(d, d_cov) {
    d_prec_tot = summarize(d$prec, votes = sum(votes), .by = c(all_of(RACE_ID), county, geo, method))
    d_comp = left_join(
        summarize(d_prec_tot, votes = sum(votes), .by = c(all_of(RACE_ID), county, method)),
        summarize(d$cty, votes = sum(votes), .by = c(all_of(RACE_ID), county, method)),
        by = c(RACE_ID, "county", "method"),
        suffix = c("_prec", "_cty")
    ) |>
        pivot_wider(names_from = method, values_from = c(votes_prec, votes_cty))
    d_prec_comp = d_prec_tot |>
        pivot_wider(names_from = method, values_from = votes) |>
        left_join(
            select(d_cov, geo, county, n, active, starts_with("est_")),
            by = c("geo", "county")
        )

    flag_missing_method = filter(
        d_comp,
        votes_prec_adv == 0,
        within_pct(votes_prec_eday, votes_cty_eday + votes_cty_adv, 0.1)
    )
    flag_adv_low = filter(d_comp, votes_prec_adv < 0.5 * votes_cty_adv)
    flag_eday_low = filter(d_comp, votes_prec_eday < 0.5 * votes_cty_eday)
    flag_eday_low = filter(d_comp, votes_prec_eday < 0.5 * votes_cty_eday)
    flag_tot_exc_a = filter(d_prec_comp, adv + eday > active) |>
        select(all_of(RACE_ID), county, geo, adv, eday, active, n)

    tribble(
        ~issue                                 , ~counties                 , ~precincts           , ~details            ,
        "Precinct data missing vote method"    , nrow(flag_missing_method) , NA                   , flag_missing_method ,
        "Precinct advance vote <50% of county" , nrow(flag_adv_low)        , NA                   , flag_adv_low        ,
        "Precinct e-day vote <50% of county"   , nrow(flag_eday_low)       , NA                   , flag_eday_low       ,
        "More votes than active voters"        , NA                        , nrow(flag_tot_exc_a) , flag_tot_exc_a
    )
}

precinct_matches = function(d, d_cov) {
    d_rep = d$prec |>
        summarize(
            rep = any(stage > 0L),
            .by = c(geo, county)
        ) |>
        complete(county = tgt_mit)

    filter(d_cov, county %in% tgt_mit) |>
        as_tibble() |>
        left_join(d_rep, by = c("geo", "county")) |>
        summarize(
            match_rate = mean(!is.na(rep)),
            n = n(),
            n_rep = sum(rep, na.rm = TRUE),
            .by = county
        ) |>
        arrange(match_rate)
}

precinct_partial = function(d, d_cov) {
    d_tot = d$prec |>
        filter(stage > 0L, str_starts(contest, "sen_")) |>
        summarize(votes = sum(votes), .by = c(all_of(RACE_ID), county, geo, method)) |>
        filter(votes > 0)
    d_both = inner_join(d_tot, d_cov, by = c("geo", "county")) |>
        mutate(
            is_dem = str_ends(contest, "_dem"),
            est = case_when(
                is_dem & method == "all" ~ est_tot_d,
                is_dem & method == "adv" ~ est_adv_d,
                is_dem & method == "eday" ~ est_tot_d - est_adv_d,
                !is_dem & method == "all" ~ est_tot_r,
                !is_dem & method == "adv" ~ est_adv_r,
                !is_dem & method == "eday" ~ est_tot_r - est_adv_r
            )
        )

    out = bind_rows(
        summarize(d_both, value = sum(votes) / sum(est), .by = c(contest, county, method)),
        summarize(d_both, value = sum(votes) / sum(est), method = "all", .by = c(contest, county))
    ) |>
        summarize(value = exp(mean(log(value))), .by = c(county, method)) |>
        pivot_wider(names_from = method, names_prefix = "rep_vs_", names_expand=TRUE)
    if (!hasName(out, "rep_vs_adv")) {
        out$rep_vs_adv = double(nrow(out))
        out$rep_vs_eday = double(nrow(out))
    }
    if (!hasName(out, "rep_vs_all")) {
        out$rep_vs_all = double(nrow(out))
    }
    arrange(out, desc(rep_vs_adv))
}