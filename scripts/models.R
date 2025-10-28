# ============================
# Function: execute models
# ============================
run_models <- function(
  data,
  state,
  county,
  timestamp,
  history,
  office = "Governor",
  method = "lm",
  uncertainty = "",
  outcome = "votes_governor_25_dem",
  residualize = TRUE,
  subset = NULL,
  covars,
  ...
) {
  if (is.character(data)) {
    data = read_csv(data)
  }

  merged = merge_data(
    data,
    history,
    office,
    impute = TRUE
  )

  if (!is.null(subset) && subset != "") {
    expr <- rlang::parse_expr(subset)
    merged <- filter(merged, !!expr)
  }

  merged = add_obs(merged, outcome, covars, ...)
  if (residualize) {
    merged = add_resid(merged, !!sym(outcome), !!sym(covars[1]))
    outcome = "resid"
  }

  m = run_model(
    merged,
    method,
    uncertainty,
    outcome = outcome,
    covars = covars,
    residualize = residualize,
    ...
  )

  fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/models"))
  qs2::qs_save(m, glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/models/{state}_{county}_{office}_model_{timestamp}.qs2"))
  qs2::qs_save(m, glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/models/{state}_{county}_{office}_model_latest.qs2"))

  return(m)
}

merge_data <- function(data, history, office, impute = FALSE) {
  modes = filter(data, precinct_total > 0) |> distinct(vote_mode) |> pull()
  if (length(office) == 1) {
    office = c(office)
  }

  merged = data |>
    filter(
      race_name %in% office,
      vote_mode %in% modes,
      candidate_name != "Write-ins"
    ) |>
    select(race_name, candidate_party, jurisdiction, precinct_id, timestamp, vote_mode, precinct_total) |>
    mutate(
      turnout = sum(precinct_total, na.rm=TRUE),
      .by = c(jurisdiction, precinct_id, vote_mode, race_name)
    )

  merged = merged |>
    pivot_wider(
      names_from = c(race_name, candidate_party),
      values_from = precinct_total,
      names_glue = "votes_{str_to_lower(str_remove_all(race_name, ' '))}_25_{str_sub(tolower(candidate_party), start=0, end=3)}"
    ) |>
    left_join(
      history,
      join_by(jurisdiction == county, precinct_id == precinct_25, vote_mode)
    )

  if (impute) merged = impute_missing(merged)
  
  return(merged)
}

run_model <- function(
  merged,
  method,
  uncertainty,
  outcome,
  covars,
  obs_cutoff = 0.25,
  level = 0.9,
  weight_var = NULL,
  residualize
) {
  checkmate::assert_choice(method, c("lm", "log-lm", "xgboost", "bayes-lm", "quantreg"))
  checkmate::assert_choice(uncertainty, c("naive", "conformal", ""), null.ok = TRUE)

  ## construct weights?
  if (!is.null(weight_var)) {
    merged = mutate(merged, "{{ weight_var }}" := importance_weights(!!sym(weight_var)))
  }

  # construct train, test, and valid
  if (method == "quantreg") {
    train = filter(merged, obs > obs_cutoff)
    valid = slice_sample(train, prop = 0.2)
    train = anti_join(train, valid, by = colnames(train))
    test = filter(merged, obs <= obs_cutoff)
  } else {
    train = filter(merged, obs > obs_cutoff)
    valid = tibble()
    test = filter(merged, obs <= obs_cutoff)
  }

  if (nrow(train) < 50) {
    cli::cli_abort("Training data has less than 50 observations. Model results may be unreliable.")
  }

  ## construct formula
  if (method %in% c("log-lm", "bayes-lm")) {
    outcome = paste0("log_s(", outcome, ")")
    covars[1] = paste0("log_s(", covars[1], ")")
  }

  form = as.formula(paste0(outcome, "~", paste(covars, collapse = "+")))

  # method selection
  if (method %in% c("lm", "log-lm", "xgboost")) {
    rec_reg <- recipe(
      formula = form,
      data = train
    ) |>
      step_novel(starts_with("jurisdiction")) |>
      step_zv(all_predictors()) |>
      step_log(all_numeric_predictors(), offset = 0.1) |>
      step_dummy(all_nominal_predictors())

    # fit model
    if (method %in% c("lm", "log-lm")) {
      model <- linear_reg() |> set_engine("lm")
    } else if (method == "xgboost") {
      model <- boost_tree(trees = 1000) |>
        set_engine("xgboost") |>
        set_mode("regression")
    }

    fit <- workflow() |>
      # additional blueprint recommended by `tidymodels` package
      add_recipe(rec_reg, blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)) |>
      add_model(model) |>
      fit(data = train)
  } else if (method == "quantreg") {
    rec <- recipe(formula = form, data = train) |>
      step_normalize(all_numeric_predictors()) |>
      step_impute_bag(all_numeric_predictors())

    model <- linear_reg() |>
      set_engine("quantreg") |>
      set_mode("quantile regression", quantile_levels = c((1 - level) / 2, 0.5, 1 - (1 - level) / 2))

    fit <- workflow() |>
      add_recipe(rec) |>
      add_model(model) |>
      fit(data = train)
  }

  # calculate uncertainty
  if (uncertainty == "") {
    preds = predict(fit, test) |>
      bind_rows(
        tibble(.pred = numeric(), .pred_lower = numeric(), .pred_upper = numeric())
      )
  } else if (uncertainty == "naive") {
    if (method %in% c("xgboost", "quantreg")) {
      cli::cli_abort("Naive uncertainty not implemented for xgboost or quantreg models.")
    }
    preds = marginaleffects::predictions(fit, test, conf_level = level) |>
      as_tibble() |>
      rename(
        .pred = estimate,
        .pred_lower = conf.low,
        .pred_upper = conf.high
      )
  } else if (uncertainty == "conformal") {
    if (method == "quantreg") {
      pred_valid = predict(fit, new_data = valid, level = level) |>
        unnest_wider(col = .pred_quantile, names_sep = "_") |>
        setNames(c(".pred_lower", ".pred", ".pred_upper"))

      R_low <- pred_valid$.pred_lower - valid$resid
      R_high <- valid$resid - pred_valid$.pred_upper
      residuals <- pmax(R_low, R_high)

      if (!is.null(weight_var)) {
        ws = valid[[weight_var]]

        # Compute normalized weights
        weights_norm <- ws / sum(ws)

        # Weighted 90(1 + 1/q)-th percentile
        alpha_0 <- 1 - level
        percentile_target <- (1 - alpha_0) * (1 + 1 / length(residuals))

        # Compute q_val
        q_val <- Hmisc::wtd.quantile(
          x = residuals,
          weights = weights_norm,
          probs = percentile_target,
          type = "quantile"
        )
      } else {
        alpha_0 <- 1 - level
        q_ind <- ceiling((1 - alpha_0) * (nrow(valid) + 1))
        q_val <- valid$resid[q_ind]
      }

      # test predict
      preds <- predict(fit, new_data = test, level = level) |>
        unnest_wider(col = .pred_quantile, names_sep = "_") |>
        setNames(c(".pred_lower", ".pred", ".pred_upper")) |>
        mutate(
          .pred_lower = .pred_lower - q_val,
          .pred_upper = .pred_upper + q_val
        )
    } else {
      ctrl <- control_resamples(save_pred = TRUE, extract = I)
      fit_resamples <- fit_resamples(
        fit,
        resamples = vfold_cv(train, v = 5),
        control = ctrl
      )

      con <- probably::int_conformal_cv(fit_resamples)
      preds = predict(con, test, level = level)
    }
  }

  # construct `out`
  out = bind_rows(
    train,
    valid,
    test |>
      mutate(
        estimate = preds$.pred,
        lower = preds$.pred_lower,
        upper = preds$.pred_upper
      )
  ) |>
    mutate(
      estimate = coalesce(estimate, !!sym(outcome)),
      lower = coalesce(lower, !!sym(outcome)),
      upper = coalesce(upper, !!sym(outcome))
    )

  if (residualize) {
    out <- out |>
      mutate(
        # scale estimates back up
        estimate = estimate * !!sym(covars[1]) + !!sym(covars[1]),
        lower = lower * !!sym(covars[1]) + !!sym(covars[1]),
        upper = upper * !!sym(covars[1]) + !!sym(covars[1])
      )
  }

  list(
    method = method,
    uncertainty = uncertainty,
    outcome = outcome,
    covars = covars,
    train = train,
    valid = valid,
    test = test,
    fit = fit,
    out = select(
      out,
      estimate,
      any_of(c("lower", "upper")),
      jurisdiction,
      precinct_id,
      any_of(c("vote_mode")),
      timestamp,
      obs,
      resid
    )
  )
}
