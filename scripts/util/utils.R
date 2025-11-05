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
        is.na(obs) | is.nan(obs) ~ if_else(.data[[outcome]] > 0, 1, 0),
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
  # Handle NULL or empty breaks
  if (is.null(breaks) || length(breaks) == 0) {
    return(character(0))
  }
  
  # Ensure breaks are POSIXct and remove NAs
  breaks <- as.POSIXct(breaks, origin = "1970-01-01")
  valid_idx <- !is.na(breaks)
  
  # Initialize all labels as empty
  labels <- rep("", length(breaks))
  
  if (!any(valid_idx)) {
    return(labels)
  }
  
  # Only process valid breaks
  valid_breaks <- breaks[valid_idx]
  prev_date <- as.Date("1900-01-01")
  prev_ampm <- ""
  
  for (i in seq_along(valid_breaks)) {
    current_date <- as.Date(valid_breaks[i])
    current_hour <- format(valid_breaks[i], "%I") |> as.numeric() |> as.character()
    current_ampm <- str_to_lower(format(valid_breaks[i], "%p"))
    
    parts <- c(character())
    
    # Add date if it's a new day
    if (!is.na(current_date) && current_date != prev_date) {
      parts <- c(format(valid_breaks[i], "%a"), parts)
      prev_date <- current_date
      prev_ampm <- ""
    }

    # Add AM/PM only if it changed
    if (current_ampm != prev_ampm) {
      parts <- c(parts, paste0(current_hour, current_ampm))
      prev_ampm <- current_ampm
    } else {
      parts <- c(parts, current_hour)
    }
    
    # Find the position in the original breaks vector
    labels[valid_idx][i] <- paste(parts, collapse = "\n")
  }
  
  return(labels)
}

smart_datetime_axis <- function(date_range) {
  time_span <- as.numeric(difftime(max(date_range), min(date_range), units = "hours"))
  
  if (time_span <= 6) {
    breaks <- "2 hour"
  } else if (time_span <= 24) {
    breaks <- "6 hours"
  } else if (time_span <= 72) {
    breaks <- "12 hours"
  } else if (time_span <= 168) {
    breaks <- "1 day"
  } else {
    breaks <- "1 week"
  }
  
  return(breaks)
}
