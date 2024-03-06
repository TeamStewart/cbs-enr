# ============================
# Function: execute models
# used in `scripts/functions.R`
# ============================
execute_model <- function(data, state, party, office, time){
  timestamp <- ymd_hms(time, tz = "America/New_York")
  
  # Read in crosswalks
  precinct_xwalk <- read_csv(sprintf("data/input/%s/precinct_party_xwalk.csv",state)) |> filter(.data$party == .env$party)
  county_xwalk <- read_csv(sprintf("data/input/%s/county_party_xwalk.csv",state)) |> filter(.data$party == .env$party)
  
  # Clean up results df
  results <- data |> 
    mutate(
      vote_mode = case_when(
        vote_mode == "Election Day" ~ "election_day",
        vote_mode == "Provisional" ~ "provisional",
        vote_mode == "Absentee/Mail" ~ "absentee",
        vote_mode == "Early Voting" ~ "early_voting",
        vote_mode == "Other" ~ 'other'
      )
    ) |>
    mutate(precinct_total = ifelse(precinct_total == 0, NA, precinct_total))
  
  # make analysis tables
  analysis_precinct <- results |>
    summarise(precinct_total = sum(precinct_total, na.rm = T),
              .by = c(jurisdiction, precinct_id, virtual_precinct, vote_mode)) |>
    pivot_wider(names_from = "vote_mode", values_from = "precinct_total", values_fn = sum, names_prefix = "mode_total_") |> 
    left_join(precinct_xwalk) |>
    rowwise() |>
    mutate(
      precinct_total_turnout = sum(c_across(starts_with("mode_total")), na.rm = TRUE),
      precinct_pct_party_election_day = mode_total_election_day / precinct_total_reg,
      precinct_reported = precinct_total_turnout > 0) |>
    select(cbs_region, jurisdiction, precinct_id, virtual_precinct, precinct_reported,
           precinct_total_turnout, precinct_total_election_day = mode_total_election_day, 
           precinct_total_early = mode_total_early_voting, precinct_total_absentee = mode_total_absentee,
           precinct_pct_party_election_day, precinct_pct_party_election_day_lag,
           precinct_total_party_reg, precinct_total_reg, precinct_pct_party_black,
           precinct_pct_party_white, precinct_pct_party_nonwhite, precinct_pct_black,
           precinct_pct_white, precinct_pct_nonwhite) |>
    filter(!virtual_precinct) |> 
    ungroup()
  
  analysis_county <- results |>
    summarise(county_total = sum(precinct_total, na.rm = T),
              .by = c(jurisdiction, vote_mode)) |>
    pivot_wider(names_from = "vote_mode", values_from = "county_total", values_fn = sum, names_prefix = "mode_total_") |> 
    left_join(county_xwalk) |>
    rowwise() |>
    mutate(
      county_total_party_turnout = sum(c_across(starts_with("mode_total"))),
      county_pct_party_election_day = mode_total_election_day / county_total_reg,
      county_pct_party_absentee = mode_total_absentee / county_total_reg,
      county_pct_party_early = mode_total_early_voting / county_total_reg,
      county_pct_party_turnout = county_total_party_turnout / county_total_party_reg) |>
    select(cbs_region, jurisdiction, 
           county_pct_party_election_day, county_pct_party_election_day_lag,
           county_pct_party_absentee, county_pct_party_absentee_lag,
           county_pct_party_early, county_pct_party_early_lag,
           county_pct_party_turnout, county_pct_party_turnout_lag,
           county_total_party_reg, county_total_reg, county_pct_party_black,
           county_pct_party_white, county_pct_party_nonwhite, county_pct_black,
           county_pct_white, county_pct_nonwhite) |> 
    ungroup()
  
  # Prepare for output tables
  county_output <- analysis_precinct |> group_by(cbs_region, jurisdiction) |>
    summarise(
      precincts_reported = sum(precinct_reported, na.rm = T),
      precinct_pct_reported = precincts_reported / n_distinct(precinct_id),
      county_total_turnout = sum(precinct_total_turnout, na.rm = T),
      county_total_early = sum(precinct_total_early, na.rm = T),
      county_total_absentee = sum(precinct_total_absentee, na.rm = T),
      county_total_election_day = sum(precinct_total_election_day, na.rm = T)
      ) |> ungroup() |>
    mutate(timestamp = timestamp)
  
  # Execute models
  ## Election day turnout - region FE
  eday_eday_lagged_region <- feols(precinct_pct_party_election_day ~ precinct_pct_party_election_day_lag | cbs_region, data = analysis_precinct, weights = ~precinct_total_party_reg)
  eday_party_demo_region <- feols(precinct_pct_party_election_day ~ precinct_pct_party_black + precinct_pct_party_white | cbs_region, data = analysis_precinct,weights = ~precinct_total_party_reg)
  eday_demo_region <- feols(precinct_pct_party_election_day ~ precinct_pct_black + precinct_pct_white | cbs_region, data = analysis_precinct, weights = ~precinct_total_party_reg)
  
  ## Election day turnout - county FE
  eday_eday_lagged_county <- feols(precinct_pct_party_election_day ~ precinct_pct_party_election_day_lag | jurisdiction, data = analysis_precinct, weights = ~precinct_total_party_reg)
  eday_party_demo_county <- feols(precinct_pct_party_election_day ~ precinct_pct_party_black + precinct_pct_party_white | jurisdiction, data = analysis_precinct,weights = ~precinct_total_party_reg)
  eday_demo_county <- feols(precinct_pct_party_election_day ~ precinct_pct_black + precinct_pct_white | jurisdiction, data = analysis_precinct, weights = ~precinct_total_party_reg)
  
  ## Absentee
  absentee_absentee_lagged <- feols(county_pct_party_absentee ~ county_pct_party_absentee_lag, data = analysis_county, weights = ~county_total_party_reg)
  absentee_party_demo <- feols(county_pct_party_absentee ~ county_pct_party_black + county_pct_party_white, data = analysis_county,weights = ~county_total_party_reg)
  absentee_demo <- feols(county_pct_party_absentee ~ county_pct_black + county_pct_white, data = analysis_county,weights = ~county_total_party_reg)
  
  ## Early
  early_early_lagged <- feols(county_pct_party_early ~ county_pct_party_early_lag, data = analysis_county, weights = ~county_total_party_reg)
  early_party_demo <- feols(county_pct_party_early ~ county_pct_party_black + county_pct_party_white, data = analysis_county,weights = ~county_total_party_reg)
  early_demo <- feols(county_pct_party_early ~ county_pct_black + county_pct_white, data = analysis_county,weights = ~county_total_party_reg)
  
  # Produce estimates from models
  estimate_eday_eday_lagged <- produce_estimates(county_fe_model = eday_eday_lagged_county, region_fe_model = eday_eday_lagged_region, analysis_table = analysis_precinct)
  estimate_eday_party_demo <- produce_estimates(county_fe_model = eday_party_demo_county, region_fe_model = eday_party_demo_region, analysis_table = analysis_precinct)
  estimate_eday_demo <- produce_estimates(county_fe_model = eday_demo_county, region_fe_model = eday_demo_region, analysis_table = analysis_precinct)
  
  estimate_absentee_absentee_lagged <- produce_estimates(model = absentee_absentee_lagged, analysis_table = analysis_county)
  estimate_absentee_party_demo <- produce_estimates(model = absentee_party_demo, analysis_table = analysis_county)
  estimate_absentee_demo <- produce_estimates(model = absentee_demo, analysis_table = analysis_county)
  
  estimate_early_early_lagged <- produce_estimates(model = early_early_lagged, analysis_table = analysis_county)
  estimate_early_party_demo <- produce_estimates(model = early_party_demo, analysis_table = analysis_county)
  estimate_early_demo <- estimate_early_early_lagged <- produce_estimates(model = early_demo, analysis_table = analysis_county)
  
  output <- county_output |>
    left_join(estimate_eday_eday_lagged |> rename(estimate_eday_eday_lagged = county_vote), by = "jurisdiction") |>
    left_join(estimate_eday_party_demo |> rename(estimate_eday_party_demo = county_vote), by = "jurisdiction") |>
    left_join(estimate_eday_demo |> rename(estimate_eday_demo = county_vote), by = "jurisdiction") |>
    left_join(estimate_absentee_absentee_lagged |> rename(estimate_absentee_absentee_lagged = county_vote), by = "jurisdiction") |>
    left_join(estimate_absentee_party_demo |> rename(estimate_absentee_party_demo = county_vote), by = "jurisdiction") |>
    left_join(estimate_absentee_demo |> rename(estimate_absentee_demo = county_vote), by = "jurisdiction") |>
    left_join(estimate_early_early_lagged |> rename(estimate_early_early_lagged = county_vote), by = "jurisdiction") |>
    left_join(estimate_early_party_demo |> rename(estimate_early_party_demo = county_vote), by = "jurisdiction") |>
    left_join(estimate_early_demo |> rename(estimate_early_demo = county_vote), by = "jurisdiction") |> 
    mutate(race_name = office,
           party = party,
           state = state)
  
  if(!dir.exists(sprintf("data/model_estimates/%s",state))){dir.create(sprintf("data/model_estimates/%s",state))}
  
  # Write to latest file
  write_csv(output, file = sprintf("data/model_estimates/%s/%s_%s_model_estimate_latest.csv", state, office, party))
  
  # Write to cumulative file
  if(file.exists(sprintf("data/model_estimates/%s/%s_%s_model_estimate_cumulative.csv", state, office, party))){
    write_csv(output, file = sprintf("data/model_estimates/%s/%s_%s_model_estimate_cumulative.csv", state, office, party), append = T)
  } else{
    write_csv(output, file = sprintf("data/model_estimates/%s/%s_%s_model_estimate_cumulative.csv", state, office, party))
  }
  
  # Produce cumulative plots for model estimates
  if(!dir.exists("model_summaries")){dir.create("model_summaries")}
  make_plots(sprintf("data/model_estimates/%s/%s_%s_model_estimate_cumulative.csv", state, office, party))
}

# ============================
# Function: produce estimates
# internal use only
# ============================
produce_estimates <- function(model = NULL, county_fe_model = NULL, region_fe_model = NULL, analysis_table){
  # if we have both a county FE and region FE model, create preds that combine info from both
  if(!is_null(county_fe_model) & !is_null(region_fe_model)){
    preds_counties <- analysis_table |> 
      # get jurisdictions that are not entirely NA
      filter(!all(is.na(precinct_pct_party_election_day)), .by = jurisdiction) |> 
      # calculate predictions using marginaleffects
      predictions(county_fe_model, newdata = _) |> 
      select(jurisdiction, precinct_id, precinct_total_party_reg, estimate) |> 
      # calculate vote at the precinct level by multiplying the coefficient (percent estimate) by the total
      # party registration at the precinct level. If that precinct is missing data, use the county-level
      # average coefficient
      mutate(precinct_vote = case_when(
        is.na(estimate) ~ precinct_total_party_reg*mean(estimate, na.rm = TRUE),
        .default = estimate*precinct_total_party_reg
      ), .by = jurisdiction) |> 
      # sum to the county level
      summarise(county_vote = sum(precinct_vote), .by = jurisdiction) |> 
      as_tibble()
    
    # predictions for remaining counties, based on their region averages
    preds_regions <- analysis_table |> 
      # filter to only the counties we didn't use above
      filter(!(jurisdiction %in% pull(preds_counties, jurisdiction))) |> 
      # predict using marginal effects again, still at the precinct level
      predictions(region_fe_model, newdata = _) |> 
      select(cbs_region, jurisdiction, precinct_id, precinct_total_party_reg, estimate) |> 
      # calculate vote totals like above, but do it in two stages. First, try to make use of 
      # some county-level averages, where available. If not, then use the region-level average
      mutate(
        .by = jurisdiction,
        precinct_vote = case_when(
          is.na(estimate) ~ precinct_total_party_reg*mean(estimate, na.rm = TRUE),
          .default = estimate*precinct_total_party_reg)
      ) |>
      mutate(
        .by = cbs_region,
        precinct_vote = case_when(
          is.na(precinct_vote) ~ precinct_total_party_reg*mean(estimate, na.rm = TRUE),
          .default = precinct_vote
        )
      ) |> 
      # sum everything to the county-level
      summarise(county_vote = sum(precinct_vote), .by = jurisdiction) |> 
      as_tibble()
    
    # combine the two levels of estimates and sum to a grand total
    return(bind_rows(preds_counties, preds_regions))
  } else{
    preds <- analysis_table |>
      # get jurisdictions that are not entirely NA
      filter(!all(is.na(county_pct_party_election_day)), .by = jurisdiction) |> 
      # calculate predictions using marginaleffects
      predictions(model, newdata = _) |> 
      select(jurisdiction, cbs_region, county_total_party_reg, estimate) |> 
      # calculate vote at county level by multiplying the coefficient (percent estimate) by total
      # party registration at the county level. If that county is missing data, use the region-level
      # average coefficient
      mutate(county_vote = case_when(
        is.na(estimate) ~ county_total_party_reg*mean(estimate, na.rm = TRUE),
        .default = estimate*county_total_party_reg
      ), .by = cbs_region) |>
      select(jurisdiction, county_vote)
  }
}

# ============================
# Function: make cumulative plot
# internal use only
# ============================
make_plots <- function(estimates_cumulative_file_path){
  cumulative_file <- read_csv(estimates_cumulative_file_path)
  
  state <- unique(cumulative_file$state)
  office <- unique(cumulative_file$race_name)
  
  plotting_vars <- colnames(cumulative_file)[str_detect(colnames(cumulative_file),"estimate")]
  
  # retrieve current statewide totals of vote types, use case_when to assign to "current"
  
  for(variable in plotting_vars){
    message(sprintf("plotting: %s", variable))
    
    # Prep for adding an hline for current total of vote category
    if(variable %in% c('estimate_eday_eday_lagged', 'estimate_eday_party_demo', 'estimate_eday_demo')){
      current_total <- sum(cumulative_file$county_total_election_day[cumulative_file$timestamp == max(cumulative_file$timestamp)])
      current_estimate <- sum(cumulative_file[[variable]][cumulative_file$timestamp == max(cumulative_file$timestamp)])
      current_label <- 'Election Day'
      current_color <- 'darkgreen'
    } else if(variable %in% c('estimate_absentee_absentee_lagged', 'estimate_absentee_party_demo', 'estimate_absentee_demo')){
      current_total <- sum(cumulative_file$county_total_absentee[cumulative_file$timestamp == max(cumulative_file$timestamp)])
      current_estimate <- sum(cumulative_file[[variable]][cumulative_file$timestamp == max(cumulative_file$timestamp)])
      current_label <- 'Absentee'
      current_color <- 'purple'
    } else if(variable %in% c('estimate_early_early_lagged', 'estimate_early_party_demo', 'estimate_early_demo')){
      current_total <- sum(cumulative_file$county_total_early[cumulative_file$timestamp == max(cumulative_file$timestamp)])
      current_estimate <- sum(cumulative_file[[variable]][cumulative_file$timestamp == max(cumulative_file$timestamp)])
      current_label <- 'Early'
      current_color <- 'orange'
    }
    
    
    jpeg(filename = sprintf("model_summaries/%s_%s_%s_cumlative.jpeg",state,office,variable),width = 6, height = 4, units = 'in', res = 600)
    print(
      cumulative_file |> group_by(timestamp) |>
        summarise(total = sum(!!sym(variable), na.rm = T)) |>
        ungroup() |>
        ggplot(aes(x = timestamp, y = total)) +
        geom_point() +
        geom_line() +
        geom_hline(aes(yintercept = current_total, linetype = 'dotted'), color = current_color) +
        theme_bw(base_family = "StyreneB-Regular") +
        theme(plot.subtitle = element_text(color = current_color), legend.position = "none") +
        labs(y = 'Estimated Vote', x = "Report Time", 
             title = variable, subtitle = sprintf("Current %s Total: %i. %s Estimate: %i", current_label, current_total, current_label, round(current_estimate,0)),
             caption = "Horizontal line indicates current reported total. Points indicate model estimate at given time.")
      )
    dev.off()
  }
}

