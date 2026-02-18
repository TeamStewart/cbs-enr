#' Get a Google Sheet as a data frame
#'
#' @param spreadsheet The ID or URL of the Google Sheet
#' @param sheet The name or index of the sheet within the Google Sheet
#' @param ...
#'
#' @returns A dataframe
#'
#' @export
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

manual_runmodels <- function(){

  files = list.files(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/VA/clean"), pattern = "VA_2025_11.*?\\d+\\.csv$", full.names = TRUE)

  model_meta = tidyr::expand_grid(
    method = c("lm", "xgboost"),
    uncertainty = "conformal",
    subset = c('vote_mode == "Election Day"'),
    outcome = c(
      # "turnout",
      "votes_governor_25_dem_share",
      "votes_governor_25_rep_share"
    ),
    c1 = list(
      ## all combined with past vote/turnout
      NULL,
      # cory's full model
      c(
        "l_votes21",
        "l_votes24",
        "p_age_18_29",
        "p_age_65_up",
        "p_white_col",
        "p_white_noncol",
        "p_black_col",
        "p_black_noncol",
        "p_hisp",
        "p_noncol_male",
        "p_noncol_fem",
        "p_dem",
        "p_ind"
      )
    )
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      c2 = dplyr::case_when(
        outcome == "votes_governor_25_dem_precinct_total" ~ "votes_potus_24_dem",
        outcome == "votes_governor_25_rep_precinct_total" ~ "votes_potus_24_rep",
        outcome == "votes_governor_25_dem_share" ~ "votePct_potus_24_dem",
        outcome == "votes_governor_25_rep_share" ~ "votePct_potus_24_rep",
        outcome == "turnout" ~ "votes_precFinal_24",
        .default = NA_character_
      ),
      covars = list(c(c2, c1))
    ) |>
    dplyr::select(-c1, -c2) |>
    dplyr::ungroup()

  HISTORY = tar_read(history_VA_NA)
  
  run_singlemodel <- function(data, method, uncertainty, outcome, subset, covars, timestamp){
    run_models(
      data, "VA", NA, timestamp, history=HISTORY, office = "Governor", 
      method = method, uncertainty = uncertainty, outcome = outcome, residualize = TRUE,
      subset = subset, covars = covars, weight_var = "turnout"
    ) |> 
      get_model_summary()
  }

  precs = map(files, fread) |> rbindlist(fill=TRUE) |> 
    filter(timestamp > ymd_hm("2025-11-04 14:00"), race_name == "Governor") |> 
    mutate(file = timestamp) |> 
    nest(data = -file)

  ci_new = precs |>
    expand_grid(model_meta) |>
    filter(file > ymd_hm("2025-11-04 19:20"), file < ymd("2025-11-05")) |>
    mutate(
      out = pmap(
        list(data, method, uncertainty, outcome, subset, covars, file),
        run_singlemodel,
        .progress = TRUE
      ),
      # out = pmap(
      #   list(data, method, uncertainty, outcome, subset, covars, file),
      #   .f = in_parallel(
      #     \(d, m, y, o, s, c, f) f(d, m, y, o, s, c, f),
      #     f = run_singlemodel,
      #     run_models = run_models,
      #     get_model_summary = get_model_summary,
      #     history = HISTORY
      #   ),
      #   .progress = TRUE
      # )
    ) |> 
    select(out) |> unnest(cols = out) |> 
    mutate(covars = lengths(covars)) |> 
    mutate(
      type = case_when(
        covars == 1 ~ paste(method, "No Covars"),
        covars == 2 ~ paste(method, "Minimal Covars"),
        covars > 2 ~ paste(method, "Full Covars"),
        .default = NA_character_
      ),
      cand = case_when(
        outcome == "votes_governor_25_dem_share" ~ "spanberger",
        outcome == "votes_governor_25_rep_share" ~ "sears",
        .default = NA
      )
    ) |> 
    rename(
      est = estimate,
      file = timestamp
    )
  
  results = precs |> 
    filter(file > ymd_hm("2025-11-04 19:20")) |>
    mutate(
      cory = map2(data, file, \(x, t) cory_modeling(x, history, t), .progress = TRUE)
    )
  
  # ci = map(
  #   list.files(path = glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/VA/summaries"), pattern = "summarymodel.*?/\\d+\\.qs2$", full.names = TRUE),
  #   qs2::qs_read
  # ) |>
  #   bind_rows() |>
  #   distinct() |>
  #   filter(outcome != "turnout", !str_detect(outcome, "precinct_total$"), vote_mode == "Election Day") |>
  #   mutate(covars = lengths(covars)) |>
  #   mutate(
  #     type = case_when(
  #       covars == 1 ~ paste(method, "No Covars"),
  #       covars == 2 ~ paste(method, "Minimal Covars"),
  #       covars > 2 ~ paste(method, "Full Covars"),
  #       .default = NA_character_
  #     ),
  #     cand = case_when(
  #       outcome == "votes_governor_25_dem_share" ~ "spanberger",
  #       outcome == "votes_governor_25_rep_share" ~ "sears",
  #       .default = NA
  #     )
  #   ) |>
  #   rename(
  #     est = estimate,
  #     file = timestamp
  #   )

  results |>
    select(-data) |>
    unnest(cols = cory) |>
    unnest_longer(col = cory) |>
    unnest_wider(col = cory) |>
    drop_na(cand) |>
    bind_rows(ci_new) |>
    filter(
      lower > 0,
      file < ymd("2025-11-05"),
      !str_detect(type, "Minimal")
    ) |>
    mutate(
      type = case_match(
        type,
        .default = type,
        "conf_seq" ~ "Bayes-lm Conformal Seq",
        "cred_int" ~ "Bayes-lm Credible Int"
      )
    ) |> 
    ggplot(aes(x = file, y = est, ymin = lower, ymax = upper, fill = cand, color = cand)) +
    geom_ribbon(alpha = 0.5) +
    geom_point() +
    geom_line() +
    geom_hline(
      yintercept = 0.537,
      linetype = "dashed",
      color = "#3791FF"
    ) +
    geom_hline(
      yintercept = 0.460,
      linetype = "dashed",
      color = "#F6573E"
    ) +
    facet_wrap(~type) +
    scale_discrete_manual(
      aesthetics = c("fill", "color"),
      values = c("spanberger" = "#3791FF", "sears" = "#F6573E")
    ) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1)
    ) +
    labs(
      x = "",
      y = "Vote Share Estimate",
      title = "Election Day Only"
    ) +
    theme_bw(base_size = 18) +
    theme(
      legend.position = "none",
      panel.grid.major = element_line(linetype = "dashed", color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_line(linetype = "dashed", color = "grey90", linewidth = 0.3),
      strip.background = element_rect(fill = NA, color = NA),
      strip.text = element_text(face = "bold")
    )
  
  ggsave("~/Dropbox (MIT)/Research/CBS-MIT Election Data/25_general/_post_analysis/models.png", width = 12, height = 8, dpi = 300)

}

create_dirs <- function(meta) {
  # general dirs
  fs::dir_create("data/cbs_format")

  # state dirs
  create_dir <- function(state) {
    fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw"))
    fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/clean"))
    fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/summaries"))
  }

  purrr::walk(unique(meta$state), create_dir)

  return(NULL)
}