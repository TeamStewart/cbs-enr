model_prec_pois <- function(
    d_race,
    d_cov,
    contest,
    district,
    formulas = ~ (1 | geostratum),
    stage_done = 6L,
    turn_ratio = 1.0,
    conf_level = 0.95,
    by_method = TRUE
) {
    poss_method = if (isTRUE(by_method)) "method" else NULL
    by_cols = c("candidate", "party", poss_method)

    if (!isTRUE(by_method)) {
        d_race = d_race |>
            collapse::group_by_vars(c("candidate", "party", "time", "stage", "geo", "county")) |>
            collapse::fsummarize(votes = sum(votes))
        d_cov = d_cov |>
            filter(method == "all")
    }

    if (sum(d_race$stage > 0L) == 0) {
        return(NULL)
    }
    if (grepl("^house_", contest)) {
        d_cov = filter(d_cov, cd == district)
    }

    # turnout adjustment
    d_join = d_race |>
        filter(stage > 0L, .by = c(geo, poss_method)) |>
        inner_join(d_cov, by = c("geo", "county", poss_method, "party"))
    if (nrow(d_join) == 0) {
        return(NULL)
    }

    n_cand = n_distinct(d_race$candidate)
    tr_by_cols = c("geostratum", poss_method)
    d_tr = d_join |>
        filter(stage >= stage_done) |>
        summarize(tr = sum(votes) / sum(est / n_cand), .by = all_of(tr_by_cols))

    d_join = d_join |>
            left_join(d_tr, by = tr_by_cols) |>
            mutate(
                est = est * coalesce(tr, turn_ratio) / n_cand,
                est_tot = sum(est),
                votes_tot = sum(votes),
                .by = c("geo", poss_method),
            ) |>
            filter(votes_tot > 0) |>
            mutate(
                votes = if_else(
                    stage >= stage_done | votes_tot > est_tot,
                    as.integer(votes),
                    as.integer(round(votes * est_tot / votes_tot)),
                )
            ) |>
            select(-est_tot, -votes_tot, -tr)

    # modeling
    forms = lapply(formulas, \(f) update(f, votes ~ log_s(est) + .))
    models = d_join |>
        summarize(
            model = map_until(forms, function(form) {
                d_in = pick(everything())
                if (nrow(d_in) == 0 || all(!is.finite(d_in$votes))) {
                    return(NA)
                }

                if (length(lme4::findbars(form)) > 0) {
                    ctrl = lme4::glmerControl(
                        check.conv.singular = lme4::.makeCC(action = "ignore", tol = 1e-4)
                    )
                    lme4::glmer(form, data = d_in, family = poisson(), control = ctrl)
                } else {
                    glm_minnorm(form, data = d_in, family = poisson())
                }
            }),
            model_spec = names(model),
            .by = all_of(by_cols)
        ) |>
        filter(map_lgl(model, ~ !identical(.x, NA))) # remove empty models

    d_coef = models |>
        mutate(
            coefs = map(model, ~ broom.mixed::tidy(.x, effects = c("fixed", "ran_vals"))),
        ) |>
        select(-model) |>
        unnest(coefs) |>
        select(-any_of(c("p.value", "lambda", "dev.ratio", "statistic", "step")))


    d_frame = if (by_method) {
        filter(d_cov, party == d_race$party[1], method != "all")
    } else {
        filter(d_cov, party == d_race$party[1])
    }

    d_frame = d_frame |>
        cross_join(tibble(candidate = unique(d_race$candidate))) |>
        left_join(
            select(d_race, geo, county, party, stage, all_of(by_cols), votes),
            by = c("geo", "county", by_cols)
        ) |>
        left_join(d_tr, by = tr_by_cols) |>
        mutate(est = est * coalesce(tr, 1.0) / n_cand)

    d_conform = nest(d_join, .by = c(by_cols)) |>
        inner_join(models, by = by_cols) |>
        mutate(
            geo = map(data, ~ .x$geo),
            pred = map2(model, data, \(m, d) {
                c(predict(m, d, type="response", allow.new.levels = TRUE))
            }),
            rep = map(data, ~ .x$votes)
        ) |>
        select(-model, -data) |>
        unnest(c(geo, pred, rep)) |>
        mutate(
            resid = rep / sum(rep) - pred / sum(pred),
            .by = c("geo", poss_method)
        ) |>
        filter(is.finite(resid)) |>
        summarize(
            q_min = quantile(resid, 0.5 * (1 - conf_level)),
            q_max = quantile(resid, 0.5 * (1 + conf_level)),
            .by = c(all_of(by_cols), model_spec)
        )

    d_pred = nest(d_frame, .by = by_cols) |>
        inner_join(models, by = by_cols) |>
        mutate(
            pred = purrr::map2(
                model,
                data,
                ~ predict(.x, .y, type = "response", allow.new.levels = TRUE)
            )
        ) |>
        select(-model) |>
        unnest(c(data, pred)) |>
        mutate(
            is_rep = !is.na(votes) & stage >= 2L,
            use_pred = is.na(votes) | stage <= 4L
        ) |>
        mutate(pred = if_else(use_pred, c(pred), votes)) |>
        mutate(
            tot_pred = 0.1 + sum(pred),
            share = (0.1 + pred) / tot_pred,
            .by = c("geo", poss_method)
        ) |>
        left_join(d_conform, by = c(by_cols, "model_spec")) |>
        mutate(
            pred_min = tot_pred * if_else(use_pred, pmax(share + q_min, 0), share),
            pred_max = tot_pred * if_else(use_pred, pmin(share + q_max, 1), share),
        ) |>
        summarize(
            model = model_spec[1],
            rep = sum(votes[!is.na(votes)]),
            pred = sum(pred),
            outstd = pred - rep,
            pred_min = sum(pred_min),
            pred_max = sum(pred_max),
            est = sum(est),
            .by = all_of(by_cols)
        ) |>
        relocate(outstd, .after = rep) |>
        mutate(
            pred_type = "vote",
            model_conf = "approx_conform_99",
            pct_rep = sum(rep) / sum(rep + outstd),
        )

    list(
        pred = d_pred,
        coef = d_coef
    )
}
