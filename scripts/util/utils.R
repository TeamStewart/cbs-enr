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
  )
}

add_obs <- function(data, outcome, covars, obs_function="past"){
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

impute_missing <- function(base, group = NULL, covars) {
  base |>
    group_by(!!sym(group)) |>
    mutate(
      across(where(is.numeric), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))
    ) |>
    ungroup() |>
    mutate(
      across(where(is.numeric), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))
    )
}

minimal_datetime_labels <- function(breaks) {
  print(breaks)
  # Remove NA values
  breaks <- breaks[!is.na(breaks)]
  
  if (length(breaks) == 0) {
    return(character(0))
  }

  labels <- character(length(breaks))
  prev_date <- as.Date("1900-01-01")
  prev_ampm <- ""
  
  for (i in seq_along(breaks)) {
    current_date <- as.Date(breaks[i])
    current_hour <- format(breaks[i], "%I:%M")
    current_ampm <- format(breaks[i], "%p")
    
    # Build label components
    parts <- character(0)
    
    # Add date if it's a new day
    if (current_date != prev_date) {
      parts <- c(parts, format(breaks[i], "%b %d"))
      prev_date <- current_date
      prev_ampm <- ""  # Reset AM/PM on new day
    }
    
    # Add time
    parts <- c(parts, current_hour)
    
    # Add AM/PM only if it changed
    if (current_ampm != prev_ampm) {
      parts <- c(parts, current_ampm)
      prev_ampm <- current_ampm
    }
    
    labels[i] <- paste(parts, collapse = "\n")
  }
  
  return(labels)
}

smart_datetime_axis <- function(date_range) {
  time_span <- as.numeric(difftime(max(date_range), min(date_range), units = "hours"))
  
  if (time_span <= 6) {
    breaks <- "1 hour"
  } else if (time_span <= 24) {
    breaks <- "3 hours"
  } else if (time_span <= 72) {
    breaks <- "6 hours"
  } else if (time_span <= 168) {
    breaks <- "12 hours"
  } else {
    breaks <- "1 day"
  }
  
  return(breaks)
}