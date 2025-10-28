# Convert a state abbreviation to its full name
lookup_state_name <- function(abbreviation) {
  state.name[match(abbreviation, state.abb)]
}

get_gsheet <- function(spreadsheet, sheet, ...) {
  googlesheets4::read_sheet(
    ss = spreadsheet,
    sheet = sheet,
    ...
  )
}

#' Residualize a variable in a data frame
#' 
#' @param data A data frame
#' @param left The left-hand side variable (outcome)
#' @param right The right-hand side variable (predictor)
#' @return A data frame with a new column `resid` containing the residuals
add_resid <- function(data, left, right){
  mutate(
    data,
    resid = ({{ left }} - {{ right }}) / {{ right }},
    resid = if_else(is.infinite(resid), NA_real_, resid),
    resid = pmax(-5, pmin(resid, 5))
  ) |> 
    drop_na(resid)
}

add_obs <- function(data, outcome, covars, obs_function="past", ...){
  checkmate::assert_choice(obs_function, c("past", "any", "test_ind", "test_rank"))

  if (obs_function == "past") {
    obsf = enightmodels:::add_obs_past
  } else if (obs_function == "any") {
    obsf = enightmodels:::add_obs_any
  } else if (obs_function == "test_ind") {
    obsf = enightmodels:::add_obs_test_ind
  } else if (obs_function == "test_rank") {
    obsf = enightmodels:::add_obs_test_rank
  }

  obsf(data, !!sym(outcome), !!sym(covars[1])) |>
    mutate(
      obs = case_when(
        is.nan(obs) ~ 0,
        is.na(obs) ~ if_else(.data[[outcome]] > 0, 1, 0),
        .default = obs
      )
    )
}

impute_missing <- function(d){
  rec <- recipe(1 ~ ., data = d) |> 
    step_impute_median(all_numeric_predictors())

  prep(rec) |> bake(new_data = d)
}
