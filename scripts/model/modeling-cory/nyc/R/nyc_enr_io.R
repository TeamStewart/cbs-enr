suppressPackageStartupMessages({
    library(here)
    library(nanoparquet)
    library(dplyr)
    library(yyjsonr)
    library(readr)
})

path_small = here("reports/data-out/nyc_small_history.json")
path_full = here("reports/data-out/nyc_full_history.json")
path_curr = here("reports/data-out/nyc_current.json")
path_prec = here("reports/data-out/nyc_prec.csv")
path_prec_fore = here("reports/data-out/nyc_forecast_prec.csv")

enr_load_nyc <- function(cands, TEST) {
    d_live_raw = read_csv_bucket(
        "Precincts/20251104-NY-M/NY_M_results.csv",
        col_types = spec_live()
    ) |>
        mutate(ed = str_sub(precinct_id, 4)) |>
        filter(ed != "Total")

    d_live = fmt_wide(
        d_live_raw,
        vote_col = precinct_total,
        cand_col = candidate_name,
        cand_spec = cands,
        id_cols = c(jurisdiction, ad, ed, virtual_precinct, vote_mode, pct_reported)
    )

    # stopifnot(nrow(d_live) == n_distinct(d_live_raw[c("ad", "ed")]))
    stopifnot(all(names(cands) %in% names(d_live)))
    stopifnot(all(d_live[c("mamdani", "sliwa", "cuomo")] >= 0))
    stopifnot(all(d_live[c("mamdani", "sliwa", "cuomo")] <= 1))

    d_pre = read_parquet(here("data/nyc/nyc_pre.parquet"))

    d = d_live |>
        filter(!(ad == 42L & ed == "059"), !(ad == 35L & ed == "050")) |>
        mutate(total = coalesce(total, 0)) |>
        distinct(ad, ed, .keep_all = TRUE) |>
        inner_join(d_pre, join_by(ad, ed), relationship = "one-to-one")

    d = if (isFALSE(TEST) || is.na(TEST)) {
        TEST = FALSE
        mutate(d,
            obs = 1L * (total > turn_early * 1.2),
            obs_turn = 1L * (pct_reported >= 0.98)
        )
    } else {
        add_obs_test_rank(d, mamdani) |>
            mutate(obs = 1L * (obs <= TEST / 8 & !is.na(total)))
    }

    d
}

enr_output_nyc <- function(
    d,
    forecasts,
    ei,
    forecast_prec,
    TEST,
    write_local = TRUE,
    write_s3 = isFALSE(TEST)
) {
    timestamp = if (isFALSE(TEST)) {
        Sys.time()
    } else {
        as.POSIXct("2025-11-04 20:00 EDT") + TEST * 900 # each time step is 15min
    }

    d_rep = d |>
        mutate(n_obs = sum(obs == 1), n_total = n()) |>
        summarize(
            turn = sum(total[obs == 1]),
            n_obs = n_obs[1],
            n_total = n_total[1],
            across(all_of(names(cands)), ~ sum(.x[obs == 1] * total[obs == 1]) / turn)
        )

    output_full = list(
        timestamp = timestamp,
        reported = d_rep,
        forecasts = forecasts,
        ei = ei
    )

    output_small = output_full
    output_small$ei = NULL
    output_small$forecasts$full$model = NULL
    output_small$forecasts$simple_pres$model = NULL
    output_small$forecasts$simple_prim$model = NULL

    # write out
    if (isTRUE(write_local)) {
        log_ndjson(output_small, path_small, digits_signif = 6)
        log_ndjson(output_full, path_full, digits_signif = 6)
        write_json_file(output_full, path_curr, auto_unbox = TRUE, digits_signif = 6)

        d |>
            select(
                borough = county, ad, ed, obs, mamdani:total,
                pred_turn, votes_total_24, votes_total_21, votes_total_20,
                pred_mamdani, pred_cuomo, pred_sliwa,
                prim25_cvr_top_m, prim25_cvr_top_c, cuomo_diff,
                pres24_2way_dem, pct_mayor_21_dem, pres20_2way_dem,
                p_white, p_black, p_hisp, p_asian, p_male,
                p_age_18_29, p_col, #p_wfp,
                starts_with("inc_"), p_rent
            ) |>
            mutate(
                borough = as.integer(factor(borough)),
                prec = paste0(ad, "-", ed),
                pres_24_20_shift = pres24_2way_dem - pres20_2way_dem,
            ) |>
            select(-ad, -ed) |>
            map(~ format(.x, digits = 3)) |>
            as_tibble() |>
            write_csv(path_prec)
    }

    if (isTRUE(write_s3)) {
        write_csv(forecast_prec, path_prec_fore)
        write_file_bucket(path_prec_fore, "Precincts/20251104-NY-M/NY_M_turn_forecast.csv")

        base = here("reports/")
        to_s3 = \(x) paste0("20251104-NY-M/data-out/", basename(x))

        write_file_bucket(path_small, to_s3(path_small))
        write_file_bucket(path_full, to_s3(path_full))
        write_file_bucket(path_curr, to_s3(path_curr))
        write_file_bucket(path_prec, to_s3(path_prec))
    }

    invisible(timestamp)
}

enr_push_nyc <- function() {
    base = here("reports/")
    to_s3 = \(x) paste0("20251104-NY-M/data-out/", basename(x))

    write_file_bucket(path_small, to_s3(path_small))
    write_file_bucket(path_full, to_s3(path_full))
    write_file_bucket(path_curr, to_s3(path_curr))
    write_file_bucket(path_prec, to_s3(path_prec))
}

enr_list_nyc <- function() {
    read_ndjson_file(path_small, type="df")$timestamp
}

enr_delete_nyc <- function(idx) {
    if (idx > 0) {
        read_ndjson_file(path_small, type="list")[-idx] |>
            write_ndjson_file(path_small, auto_unbox = TRUE)
        read_ndjson_file(path_full, type="list")[-idx] |>
            write_ndjson_file(path_full, auto_unbox = TRUE)
    }
}