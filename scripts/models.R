# ============================
# Function: execute models
# ============================
run_models <- function(data, state, county, timestamp, preelection_totals) {
  
  dir_create(glue("{PATH_DROPBOX}/24_general/{state}/model_archive"))
  
  history = read_csv(glue("{PATH_DROPBOX}/history/{state}_history.csv"))
  
  dem_candidate_regex = regex("Harris|\\(Dem\\)", ignore_case = TRUE)
  rep_candidate_regex = regex("Trump|\\(Rep\\)", ignore_case = TRUE)
  
  timestamps = data |> 
    filter(race_name == "President") |> 
    rename(
      precinct_24 = precinct_id,
      votes_24 = precinct_total,
      county = jurisdiction
    ) |>
    mutate(precinct_24 = na_if(precinct_24, "")) |> 
    distinct(state, county, precinct_24, vote_mode, timestamp)
  
  data_history = data |> 
    filter(race_name == "President") |> 
    rename(
      precinct_24 = precinct_id,
      votes_24 = precinct_total,
      county = jurisdiction
    ) |>
    mutate(precinct_24 = na_if(precinct_24, "")) |> 
    # compute summary cols
    summarize(
      votes_precTotal_24 = sum(votes_24),
      votes_24_dem = sum(votes_24 * str_detect(candidate_name, dem_candidate_regex)),
      votes_24_rep = sum(votes_24 * str_detect(candidate_name, rep_candidate_regex)),
      votePct_dem_24 = votes_24_dem / votes_precTotal_24,
      votePct_rep_24 = votes_24_rep / votes_precTotal_24,
      .by = c(state, county, precinct_24, vote_mode)
    ) |>
    # filter(vote_mode != "Overvote/Undervote") |>
    drop_na(vote_mode) |>
    left_join(history, by = join_by(county, precinct_24, vote_mode), relationship = "many-to-many") |> 
    # create some helper columns for what reporting status we're at in each precinct
    mutate(
      reported_eday = max(vote_mode == "Election Day" & votes_precTotal_24 > 0) == 1,
      reported_mail = max(vote_mode == "Absentee/Mail" & votes_precTotal_24 > 0) == 1,
      reported_early = max(vote_mode == "Early Voting" & votes_precTotal_24 > 0) == 1,
      reported_any = reported_eday | reported_mail | reported_early,
      reported_all = reported_eday & reported_mail & reported_early,
      .by = c(county, precinct_24)
    ) |> 
    mutate(
      across(where(is.double), ~ ifelse(is.nan(.x), NA, .x))
    )
  
  prec_total = data_history |> distinct(county, precinct_24) |> tally() |> pull()
  prec_reported_all = data_history |> distinct(county, precinct_24, reported_all) |> pull(-1) |> sum(na.rm = TRUE)
  prec_reported_any = data_history |> distinct(county, precinct_24, reported_any) |> pull(-1) |> sum(na.rm = TRUE)
  prec_reported_eday = data_history |> distinct(county, precinct_24, reported_eday) |> pull(-1) |> sum(na.rm = TRUE)
  
  if (!file_exists(glue("{PATH_DROPBOX}/24_general/{state}/prec_reported_any_{county}"))){
    write_file(as.character(prec_reported_any), glue("{PATH_DROPBOX}/24_general/{state}/prec_reported_any_{county}"))
  }
  
  quantile_lower = (prec_reported_all / prec_total) / 2
  quantile_upper = 1 - quantile_lower
  
  if (is.na(county) | county == "NA"){
    history_sets = expand(history, county, vote_mode) |> drop_na(vote_mode) |> mutate(state = .env$state)
  } else {
    history_sets = expand(history, vote_mode) |> drop_na(vote_mode) |> mutate(county = .env$county, state = .env$state)
  }
  
  turnout_summary <- data_history |> 
    filter(reported_all) |> 
    distinct(state, county, precinct_24, vote_mode, votes_precFinal_20, votes_precTotal_24) |> 
    mutate(
      diff = ifelse(vote_mode == "Provisional", 1, votes_precTotal_24 / votes_precFinal_20)
    ) |> 
    summarise(
      turn_top = quantile(diff, quantile_upper, na.rm = TRUE),
      turn_bot = quantile(diff, quantile_lower, na.rm = TRUE),
      turn_med = weighted.mean(diff, votes_precFinal_20, na.rm = TRUE),
      .by = c(county, vote_mode)
    ) |> 
    right_join(
      history_sets, by = join_by(county, vote_mode)
    ) |>
    mutate(
      across(where(is.double), ~ na_if(.x, Inf)),
      across(where(is.double), ~ ifelse(.x > 10, 10, .x)),
      across(where(is.double), ~ ifelse(.x < -10, -10, .x)),
      across(where(is.double), ~ replace_na(.x, mean(.x, na.rm=TRUE))), 
      .by = vote_mode)
  
  swing_summary <- data_history |> 
    filter(reported_all) |> 
    distinct(state, county, precinct_24, vote_mode, votePct_dem_24, votePct_rep_24, votes_precFinal_20) |> 
    filter(!(is.nan(votePct_dem_24) & is.nan(votePct_rep_24))) |> 
    summarise(
      dem_top = quantile(votePct_dem_24, quantile_upper, na.rm=TRUE),
      dem_bot = quantile(votePct_dem_24, quantile_lower, na.rm=TRUE),
      dem_med = weighted.mean(votePct_dem_24, votes_precFinal_20, na.rm=TRUE),
      rep_top = quantile(votePct_rep_24, quantile_upper, na.rm=TRUE),
      rep_bot = quantile(votePct_rep_24, quantile_lower, na.rm=TRUE),
      rep_med = weighted.mean(votePct_rep_24, votes_precFinal_20, na.rm=TRUE),
      .by = c(county, vote_mode)
    ) |> 
    right_join(
      history_sets, by = join_by(county, vote_mode)
    ) |>
    mutate(across(where(is.double), ~ replace_na(.x, mean(.x, na.rm=TRUE))), .by = vote_mode)
  
  estimates <- data_history |> 
    full_join(turnout_summary, join_by(state, county, vote_mode)) |> 
    full_join(swing_summary, join_by(state, county, vote_mode)) |> 
    mutate(
      votePct_dem_20 = replace_na(votePct_dem_20, mean(votePct_dem_20, na.rm = TRUE)),
      votePct_rep_20 = replace_na(votePct_rep_20, mean(votePct_rep_20, na.rm = TRUE)),
      # votes_20_dem = replace_na(votes_20_dem, mean(votes_20_dem, na.rm = TRUE)),
      # votes_20_rep = replace_na(votes_20_rep, mean(votes_20_rep, na.rm = TRUE)),
      votes_precFinal_20 = replace_na(votes_precFinal_20, mean(votes_precFinal_20, na.rm = TRUE)),
      .by = c(county, vote_mode)
    ) |> 
    mutate(
      votePct_dem_20 = replace_na(votePct_dem_20, mean(votePct_dem_20, na.rm = TRUE)),
      votePct_rep_20 = replace_na(votePct_rep_20, mean(votePct_rep_20, na.rm = TRUE)),
      # votes_20_dem = replace_na(votes_20_dem, mean(votes_20_dem, na.rm = TRUE)),
      # votes_20_rep = replace_na(votes_20_rep, mean(votes_20_rep, na.rm = TRUE)),
      votes_precFinal_20 = replace_na(votes_precFinal_20, mean(votes_precFinal_20, na.rm = TRUE))
    ) |>
    mutate(
      votes_24_precTotalEst = case_when(
        reported_all ~ votes_precTotal_24,
        votes_precTotal_24 > 0 ~ votes_precTotal_24,
        .default = votes_precFinal_20 * turn_med
      ),
      votes_24_precTotalBot = case_when(
        reported_all ~ votes_precTotal_24,
        votes_precTotal_24 > 0 ~ votes_precTotal_24,
        .default = votes_precFinal_20 * turn_bot
      ),
      votes_24_precTotalTop = case_when(
        reported_all ~ votes_precTotal_24,
        votes_precTotal_24 > 0 ~ votes_precTotal_24,
        .default = votes_precFinal_20 * turn_top
      ),
      votes_24_precTotalEst = replace_na(votes_24_precTotalEst, 0),
      votes_24_precTotalBot = ifelse(is.na(votes_24_precTotalBot), votes_24_precTotalEst, votes_24_precTotalBot),
      votes_24_precTotalTop = ifelse(is.na(votes_24_precTotalTop), votes_24_precTotalEst, votes_24_precTotalTop),
      votes_24_demEst = case_when(
        reported_all ~ votes_24_dem,
        # keep early or mail votes that are already in
        votes_24_dem > 0 ~ votes_24_dem,
        .default = (votes_20_dem + (votes_20_dem * (dem_med - votePct_dem_20))) * turn_med
      ),
      votes_24_demBot = case_when(
        reported_all ~ votes_24_dem,
        # keep early, mail, or provisional votes that are already in
        votes_24_dem > 0 ~ votes_24_dem,
        .default = (votes_20_dem + (votes_20_dem * (dem_bot - votePct_dem_20))) * turn_bot
      ),
      votes_24_demTop = case_when(
        reported_all ~ votes_24_dem,
        # keep early or mail votes that are already in
        votes_24_dem > 0 ~ votes_24_dem,
        .default = (votes_20_dem + (votes_20_dem * (dem_top - votePct_dem_20))) * turn_top
      ),
      votes_24_demEst = replace_na(votes_24_demEst, 0),
      votes_24_demBot = ifelse(is.na(votes_24_demBot), votes_24_demEst, votes_24_demBot),
      votes_24_demTop = ifelse(is.na(votes_24_demTop), votes_24_demEst, votes_24_demTop),
      votes_24_repEst = case_when(
        reported_all ~ votes_24_rep,
        # keep early or mail votes that are already in
        votes_24_rep > 0 ~ votes_24_rep,
        .default = (votes_20_rep + (votes_20_rep * (rep_med - votePct_rep_20))) * turn_med
      ),
      votes_24_repBot = case_when(
        reported_all ~ votes_24_rep,
        # keep early or mail votes that are already in
        votes_24_rep > 0 ~ votes_24_rep,
        .default = (votes_20_rep + (votes_20_rep * (rep_bot - votePct_rep_20))) * turn_bot
      ),
      votes_24_repTop = case_when(
        reported_all ~ votes_24_rep,
        # keep early or mail votes that are already in
        votes_24_rep > 0 ~ votes_24_rep,
        .default = (votes_20_rep + (votes_20_rep * (rep_top - votePct_rep_20))) * turn_top
      ),
      votes_24_repEst = replace_na(votes_24_repEst, 0),
      votes_24_repBot = ifelse(is.na(votes_24_repBot), votes_24_repEst, votes_24_repBot),
      votes_24_repTop = ifelse(is.na(votes_24_repTop), votes_24_repEst, votes_24_repTop),
    )
  
  summaries_byCounty_byMode = estimates |> 
    left_join(timestamps, join_by(state, county, precinct_24, vote_mode)) |> 
    mutate(
      across(where(is.double) & !timestamp, ~ na_if(.x, Inf)),
      across(where(is.double) & !timestamp, ~ na_if(.x, -Inf))
    ) |>
    summarize(
      timestamp = max(timestamp, na.rm = TRUE),
      votes_total_20 = sum(votes_precFinal_20, na.rm = TRUE),
      votes_total_24_lower = sum(votes_24_precTotalBot, na.rm = TRUE),
      votes_total_24_estimate = sum(votes_24_precTotalEst, na.rm = TRUE),
      votes_total_24_upper = sum(votes_24_precTotalTop, na.rm = TRUE),
      demVotes_lower = sum(votes_24_demBot, na.rm = TRUE),
      demVotes_estimate = sum(votes_24_demEst, na.rm = TRUE),
      demVotes_upper = sum(votes_24_demTop, na.rm = TRUE),
      repVotes_lower = sum(votes_24_repBot, na.rm = TRUE),
      repVotes_estimate = sum(votes_24_repEst, na.rm = TRUE),
      repVotes_upper = sum(votes_24_repTop, na.rm = TRUE),
      demShare_20 = weighted.mean(votePct_dem_20, votes_precFinal_20),
      repShare_20 = weighted.mean(votePct_rep_20, votes_precFinal_20),
      demShare_lower = demVotes_lower / (demVotes_estimate + repVotes_estimate),
      demShare_estimate = demVotes_estimate / (demVotes_estimate + repVotes_estimate),
      demShare_upper = demVotes_upper / (demVotes_estimate + repVotes_estimate),
      repShare_lower = repVotes_lower / (demVotes_estimate + repVotes_estimate),
      repShare_estimate = repVotes_estimate / (demVotes_estimate + repVotes_estimate),
      repShare_upper = repVotes_upper / (demVotes_estimate + repVotes_estimate),
      swing_lower = (demShare_lower - repShare_upper) - (demShare_20 - repShare_20),
      swing_estimate = (demShare_estimate - repShare_estimate) - (demShare_20 - repShare_20),
      swing_upper = (demShare_upper - repShare_lower) - (demShare_20 - repShare_20),
      .by = c(state, county, vote_mode)
    ) |> 
    mutate(
      across(where(is.double) & !timestamp, ~ na_if(.x, Inf)),
      across(where(is.double) & !timestamp, ~ na_if(.x, -Inf)),
      across(demShare_lower:swing_upper, ~ ifelse(vote_mode == "Provisional", NA, .x))
    )
  
  summaries_byCounty = summaries_byCounty_byMode |> 
    summarise(
      timestamp = max(timestamp),
      across(c(contains("swing"), contains("Share")), ~ weighted.mean(.x, votes_total_24_estimate, na.rm = TRUE)),
      across(votes_total_20:repVotes_upper, ~ sum(.x, na.rm=TRUE)),
      .by = c(state, county)
    )
  
  summaries_byMode = summaries_byCounty_byMode |> 
    summarise(
      across(c(contains("swing"), contains("Share")), ~ weighted.mean(.x, votes_total_24_estimate, na.rm = TRUE)),
      across(votes_total_20:repVotes_upper, ~ sum(.x, na.rm=TRUE)),
      .by = c(state, vote_mode)
    ) |> 
    mutate(
      timestamp = ymd_hms(.env$timestamp),
      confidence = demShare_lower != demShare_estimate & demShare_upper != demShare_estimate & repShare_lower != repShare_estimate & repShare_upper != repShare_estimate
    )
  
  summaries = summaries_byCounty_byMode |> 
    summarise(
      across(c(contains("swing"), contains("Share")), ~ weighted.mean(.x, votes_total_24_estimate, na.rm = TRUE)),
      across(votes_total_20:repVotes_upper, ~ sum(.x, na.rm=TRUE)),
      .by = c(state)
    )
  
  file_create(glue("{PATH_DROPBOX}/24_general/{state}/{state}_{county}_modeling.csv"))
  
  # rather than append=TRUE, combine and distinct() in case we run the model twice or
  # something else happens
  write_csv(summaries_byMode, glue("{PATH_DROPBOX}/24_general/{state}/{state}_{county}_modeling.csv"), append = TRUE)
  
  # bind_rows(
  #   summaries_byMode,
  #   read_csv(, col_types = "ccnnnnnnnnnnnnnnnnnnnnnTl")
  # ) |> 
  #   distinct() |> 
  #   write_csv(glue("{PATH_DROPBOX}/24_general/{state}/{state}_{county}_modeling.csv"))
  
  # return computed values for plotting
  m = list(
    "state" = state,
    "county" = county,
    "timestamp" = timestamp,
    "data_history" = data_history,
    "quantile_upper" = quantile_upper,
    "quantile_lower" = quantile_lower,
    "prec_total" = prec_total,
    "prec_reported_all" = prec_reported_all,
    "prec_reported_any" = prec_reported_any,
    "prec_reported_eday" = prec_reported_eday,
    "summaries" = summaries,
    "summaries_byCounty" = summaries_byCounty,
    "summaries_byMode" = summaries_byMode,
    "summaries_byCounty_byMode" = summaries_byCounty_byMode
  )
  
  qd_save(m, glue("{PATH_DROPBOX}/24_general/{state}/model_archive/{state}_{county}_modeling_{timestamp}.qs2"))
  
  return(m)
  
}