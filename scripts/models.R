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
  outcome,
  residualize = TRUE,
  subset = NULL,
  covars,
  weight_var,
  ...
) {
  if (is.character(data)) {
    data = read_csv(data) |> drop_na(precinct_total)
  }

  merged = merge_data(
    data,
    history,
    office,
    covariates = c(covars, weight_var),
    impute = TRUE,
    impute_var = "polstratum"
  )

  if (!is.null(subset) && subset != "") merged <- filter(merged, !!rlang::parse_expr(subset))
  merged = add_obs(merged, outcome, covars)
  if (residualize) merged = add_resid(merged, !!sym(outcome), !!sym(covars[1]))

  m = run_model(
    merged,
    method,
    uncertainty,
    outcome = outcome,
    covars = covars,
    residualize = residualize,
    weight_var = weight_var,
    ...
  )

  fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/models"))
  qs2::qs_save(m, glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/models/{state}_{county}_{office}_model_{timestamp}.qs2"))
  qs2::qs_save(m, glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/models/{state}_{county}_{office}_model_latest.qs2"))

  return(m)
}

merge_data <- function(data, history, office, covariates, impute, impute_var) {
  modes = filter(data, precinct_total > 0) |> distinct(vote_mode) |> pull()
  if (length(office) == 1) {
    office = c(office)
  }

  merged = data |>
    filter(
      str_detect(race_name, paste0("^(", paste(office, collapse="|"), ")")),
      # vote_mode %in% modes
    ) |>
    select(race_name, candidate_name, candidate_party, jurisdiction, precinct_id, timestamp, vote_mode, precinct_total) |>
    mutate(
      turnout = sum(as.numeric(race_name == "Governor") * precinct_total, na.rm=TRUE),,
      .by = c(jurisdiction, precinct_id, vote_mode)
    ) |> 
    mutate(
      share = ifelse(turnout == 0, 0, precinct_total / turnout),
      .by = c(jurisdiction, precinct_id, vote_mode, race_name)
    ) |>
    filter(
      candidate_name != "Write-ins"
    ) |>
    select(race_name, candidate_party, jurisdiction, precinct_id, timestamp, vote_mode, precinct_total, turnout, share)

  merged = merged |>
    pivot_wider(
      names_from = c(race_name, candidate_party),
      values_from = c(precinct_total, share),
      names_glue = "votes_{str_to_lower(str_remove_all(race_name, ' '))}_25_{str_sub(tolower(candidate_party), start=0, end=3)}_{.value}"
    ) |>
    left_join(
      history,
      join_by(jurisdiction == county, precinct_id == precinct_25, vote_mode)
    )

  if (impute) merged = impute_missing(merged, impute_var, covariates)
  
  merged |> 
    mutate(
      l_votes21 = log_s(votes_precFinal_21),
      l_votes24 = log_s(votes_precFinal_24)
    )
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
  checkmate::assert_choice(method, c("lm", "xgboost", "quantreg"))
  checkmate::assert_choice(uncertainty, c("naive", "conformal", ""), null.ok = TRUE)

  ## construct weights?
  if (!is.null(weight_var)) {
    merged = mutate(merged, {{ weight_var }} := importance_weights(!!sym(weight_var)))
  }

  # construct train, test
  train = filter(merged, obs > obs_cutoff)
  test = filter(merged, obs <= obs_cutoff)
  valid = tibble()

  if (nrow(train) < 50) {
    cli::cli_abort("Training data has less than 50 observations")
  }

  form = as.formula(paste0(if(residualize) "resid" else outcome, "~", paste(covars, collapse = "+")))

  # method selection
  if (method %in% c("lm", "xgboost")) {
    rec_reg <- recipe(
      formula = form,
      data = train
    ) |>
      step_dummy(all_nominal_predictors())

    # fit model
    if (method %in% c("lm")) {
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
      step_novel(starts_with("jurisdiction")) |>
      step_zv(all_predictors()) |>
      step_log(all_numeric_predictors(), offset = 0.1) |>
      step_dummy(all_nominal_predictors())

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
    preds = predict(fit, new_data=test) |>
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
      valid = slice_sample(train, prop=0.2)
      train = anti_join(train, valid, by = colnames(train))

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

  if (residualize){
    test = bind_cols(test, preds) |> 
      mutate(
        .pred = .pred * !!sym(covars[1]) + !!sym(covars[1]),
        .pred_lower = .pred_lower * !!sym(covars[1]) + !!sym(covars[1]),
        .pred_upper = .pred_upper * !!sym(covars[1]) + !!sym(covars[1])
      )
  } else {
    test = bind_cols(test, preds)
  }

  out = bind_rows(train, valid, test) |> 
    mutate(
      estimate = coalesce(.pred, as.numeric(!!sym(outcome)), median(!!sym(covars[1]), na.rm=TRUE)),
      lower = coalesce(.pred_lower, as.numeric(!!sym(outcome)), median(!!sym(covars[1]), na.rm=TRUE)),
      upper = coalesce(.pred_upper, as.numeric(!!sym(outcome)), median(!!sym(covars[1]), na.rm=TRUE)),
      .by = c(polstratum, vote_mode)
    )

  list(
    method = method,
    uncertainty = uncertainty,
    outcome = outcome,
    covars = covars,
    train = train,
    valid = valid,
    test = test,
    fit = fit,
    residualize = residualize,
    out = select(
      out,
      estimate, lower, upper,
      jurisdiction, precinct_id, vote_mode,
      timestamp,
      obs,
      any_of(c("resid")),
      all_of(outcome), !!covars
    )
  )
}

get_model_summary <- function(model) {
  if (str_detect(model$outcome[1], "share$")) {
    f = mean
  } else {
    f = sum
  }

  summarize(
    model$out,
    across(matches("^(estimate|lower|upper)$"), f),
    .by = c(vote_mode, timestamp)
  ) |> 
    mutate(
      outcome = model$outcome,
      method = model$method,
      uncertainty = model$uncertainty,
      covars = list(model$covars)
    )
}

save_modelsummary <- function(summary, state, county, timestamp) {
  dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/summaries"))
  qs_save(summary, glue::glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/summaries/summarymodel_{state}_{county}_{timestamp}.qs2"))
}

cory_modeling <- function(data, history, timestamp) {

  cands = c(
    spanberger = "Abigal Spanberger",
    sears = "Winsome Earle-Sears"
  )
  level = 0.9999

  # load data ------
  d_live = fmt_wide(
    data,
    vote_col = precinct_total,
    cand_col = candidate_name,
    cand_spec = cands,
    id_cols = c(jurisdiction, precinct_id, virtual_precinct, vote_mode)
  )

  stopifnot(nrow(d_live) == distinct(data, jurisdiction, precinct_id, vote_mode) |> tally())
  stopifnot(all(names(cands) %in% names(d_live)))
  stopifnot(all(d_live[c("spanberger", "sears")] >= 0))
  stopifnot(all(d_live[c("spanberger", "sears")] <= 1))

  d = enightmodels:::add_obs_any(d_live, total)
  d = d |>
    mutate(total = coalesce(total, 0)) |>
    left_join(history, by = join_by(jurisdiction == county, precinct_id == precinct_25, vote_mode)) |> 
    drop_na(votes_precFinal_24, votes_precFinal_21)

  d_rep = d |>
    mutate(n_obs = sum(obs == 1), n_total = n()) |>
    filter(obs == 1) |>
    summarize(
      turn = sum(total),
      n_obs = n_obs[1],
      n_total = n_total[1],
      across(all_of(names(cands)), ~ sum(.x * total) / turn)
    )

  # forecasting ---------
  fit_forecast_nyc = function(data, formula, prior_n_turn = 0.5, prior_n_cands = 1.5, ...) {
    forecast_prec(
      data = data,
      cands = names(cands),
      turn = total,
      obs = obs == 1,
      ctrl_form = formula,
      prior_turn = prior_turn,
      prior_cands = c(prior_spanberger, prior_sears),
      prior_n_turn = prior_n_turn,
      prior_n_cands = prior_n_cands,
      ...
    )
  }

  m_simple_pres = d |>
    mutate(
      prior_turn = votes_precFinal_21,
      prior_spanberger = votePct_potus_24_dem * 0.8,
      prior_sears = votePct_potus_24_rep * 0.8
    ) |>
    fit_forecast_nyc(~ log_s(prior_turn) + votePct_potus_24_dem, draws = 500)

  m_full = d |>
    mutate(
      prior_turn = votes_precFinal_21,
      prior_spanberger = votePct_potus_24_dem * 0.8,
      prior_sears = votePct_potus_24_rep * 0.8
    ) |>
    fit_forecast_nyc(
      ~ log_s(votes_precFinal_21) +
        log_s(votes_precFinal_24) +
        log_s(n) +
        p_age_18_29 +
        p_age_65_up +
        p_white_col +
        p_white_noncol +
        p_black_col +
        p_black_noncol +
        p_hisp +
        p_noncol_male +
        p_noncol_fem +
        p_ind +
        p_dem +
        votePct_potus_24_dem,
      draws = 500
    )
  forecasts = list(
    simple_pres = forecast_package(m_simple_pres),
    full = forecast_package(m_full),
    level = level
  )

  to_prec_ct = function(x) {
    x = round(mean(x$post_turn))
    if_else(x == 0, 1, x)
  }
  forecast_prec = tibble(
    jurisdiction = d$jurisdiction,
    precinct_id = d$precinct_id,
    vote_mode = d$vote_mode,
    simple_pres = to_prec_ct(m_simple_pres),
    full = to_prec_ct(m_full)
  )

  qs_save(forecast_prec, glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/VA/summaries/corymodel_prec_{timestamp}.qs2"))
  qs_save(forecasts, glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/VA/summaries/corymodel_{timestamp}.qs2"))

  return(forecasts$full$pred)

}