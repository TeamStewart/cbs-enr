# ============================
# Function: execute models
# used in `scripts/functions.R`
# ============================
execute_model <- function(data, state, jurisdiction, type, timestamp, target_office){
  if(state == 'AZ' & jurisdiction == 'MARICOPA' & target_office == "US SENATE-Republican"){
    timestamp_clean <- ymd_hms(timestamp,tz = "America/New_York")
    xwalk <- read_csv(glue("data/input/{state}/{state}_{jurisdiction}_{type}_xwalk.csv"))
    
    ### For testing
    #data$precinct_total <- round(runif(nrow(data), 0, 400), 0)
    ## Set a random subset of precinct_total values to NA (10% of the rows)
    #data$precinct_total[sample(seq_len(nrow(data)), size = 0.7 * nrow(data))] <- 0
    
    # Clean data, produce current % values
    cur <- data |> filter(race_name == target_office) |> clean_current_results()
    
    # Estimate current turnout
    turnout_estimate <- produce_predictions_turnout(cur, xwalk, dv = 'turnout_current', weight = 'tot_reg_rep', covars = 'total_2022')
    
    # Estimate vote shares
    vote_share_gov_2022 <- produce_predictions_vote_share(
      cur, xwalk, 
      dv = 'vote_share_current', 
      weight = 'turnout_current', 
      covars = c("governor_republican_lake_pct","governor_republican_neely_pct", "governor_republican_salmon_pct",
                 "governor_republican_robson_pct", "governor_republican_tulliani_pct","governor_republican_w_pct",
                 "governor_republican_finerd_pct","governor_republican_roldan_pct","governor_republican_schatz_pct",  
                 "governor_republican_nq_pct")) |>
      rename(vote_share_modeled_gov_2022 = vote_share_modeled)
    
    vote_share_sen_2022 <- produce_predictions_vote_share(
      cur, xwalk, 
      dv = 'vote_share_current', 
      weight = 'turnout_current', 
      covars = c("us_senate_republican_brnovich_pct","us_senate_republican_lamon_pct","us_senate_republican_masters_pct","us_senate_republican_mcguire_pct",
                 "us_senate_republican_olson_pct","us_senate_republican_w_pct","us_senate_republican_bertone_pct","us_senate_republican_bozic_pct",
                 "us_senate_republican_nq_pct")) |>
      rename(vote_share_modeled_sen_2022 = vote_share_modeled)
    
    vote_share_magafactor <- produce_predictions_vote_share(
      cur, xwalk, 
      dv = 'vote_share_current', 
      weight = 'turnout_current', 
      covars = "magafactor") |>
      rename(vote_share_modeled_magafactor = vote_share_modeled)
    
    vote_share_lake_2022_demos <- produce_predictions_vote_share(
      cur, xwalk, 
      dv = 'vote_share_current', 
      weight = 'turnout_current', 
      covars = c("governor_republican_lake_pct","p_race1","p_noncol")) |>
      rename(vote_share_modeled_lake_2022_demos = vote_share_modeled)
    
    vote_share <- vote_share_gov_2022 |> 
      left_join(vote_share_sen_2022, by = c("state","race_id","race_name","jurisdiction","candidate_name",     
                                            "candidate_party","precinct_id","precinct_cbs","vote_mode")) |>
      left_join(vote_share_magafactor, by = c("state","race_id","race_name","jurisdiction","candidate_name",     
                                              "candidate_party","precinct_id","precinct_cbs","vote_mode")) |>
      left_join(vote_share_lake_2022_demos,  by = c("state","race_id","race_name","jurisdiction","candidate_name",     
                                                    "candidate_party","precinct_id","precinct_cbs","vote_mode")) |>
      select(c(-starts_with("vote_share_current")))
    
    vote_share_estimate <- cur |>
      filter(vote_mode != 'Aggregated') |>
      left_join(vote_share, by = c("state","race_id","race_name","jurisdiction","candidate_name",     
                                   "candidate_party","precinct_id","vote_mode")) |>
      left_join(turnout_estimate |> select(-turnout_current), by = c("state","race_id","race_name","jurisdiction","precinct_id","precinct_cbs","vote_mode")) |>
      mutate(
        cvotes_modeled_gov_2022 = turnout_modeled * vote_share_modeled_gov_2022,
        cvotes_modeled_sen_2022 = turnout_modeled * vote_share_modeled_sen_2022,
        cvotes_modeled_magafactor = turnout_modeled * vote_share_modeled_magafactor,
        cvotes_modeled_lake_2022_demos = turnout_modeled * vote_share_modeled_lake_2022_demos
      ) |>
      rename(cvotes_current = precinct_total) |>
      select(state, race_id, race_name, candidate_name, candidate_party, jurisdiction,
             precinct_id, precinct_cbs, vote_mode, cvotes_current, turnout_current, vote_share_current,
             turnout_modeled, starts_with("cvotes_modeled"))
    
    ## Prepare summary output and save
    turnout_output <- turnout_estimate |>
      summarise(
        turnout_current = sum(turnout_current, na.rm = TRUE),
        turnout_modeled = sum(turnout_modeled, na.rm = TRUE),
        .by = c("state","race_id","race_name","jurisdiction","vote_mode")
      ) |>
      mutate(
        turnout_modeled = round(turnout_modeled),
        timestamp = timestamp_clean)
    
    if(file.exists(glue("data/model_estimates/{state}/{state}_{jurisdiction}_{type}_{target_office}_turnout.csv"))){
      existing <- read_csv(glue("data/model_estimates/{state}/{state}_{jurisdiction}_{type}_{target_office}_turnout.csv"))
      write_csv(bind_rows(existing, turnout_output), glue("data/model_estimates/{state}/{state}_{jurisdiction}_{type}_{target_office}_turnout.csv"))
    } else{
      write_csv(turnout_output, glue("data/model_estimates/{state}/{state}_{jurisdiction}_{type}_{target_office}_turnout.csv"))
    }
    
    cvotes <- vote_share_estimate |>
      summarise(
        cvotes_current = sum(cvotes_current, na.rm = TRUE),
        turnout_current = sum(turnout_current, na.rm = TRUE),
        vote_share_current = cvotes_current / turnout_current,
        turnout_modeled = round(sum(turnout_modeled, na.rm = TRUE),0),
        cvotes_modeled_gov_2022 = round(sum(cvotes_modeled_gov_2022, na.rm = TRUE),0),
        cvotes_modeled_sen_2022 = round(sum(cvotes_modeled_sen_2022, na.rm = TRUE),0),
        cvotes_modeled_magafactor = round(sum(cvotes_modeled_magafactor, na.rm = TRUE),0),
        cvotes_modeled_lake_2022_demos = round(sum(cvotes_modeled_lake_2022_demos, na.rm = TRUE),0),
        .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","vote_mode")
      ) |>
      mutate(timestamp = timestamp_clean)
    
    if(file.exists(glue("data/model_estimates/{state}/{state}_{jurisdiction}_{type}_{target_office}_cvotes.csv"))){
      existing <- read_csv(glue("data/model_estimates/{state}/{state}_{jurisdiction}_{type}_{target_office}_cvotes.csv"))
      write_csv(bind_rows(existing, cvotes), glue("data/model_estimates/{state}/{state}_{jurisdiction}_{type}_{target_office}_cvotes.csv"))
    } else{
      write_csv(cvotes, glue("data/model_estimates/{state}/{state}_{jurisdiction}_{type}_{target_office}_cvotes.csv"))
    }
  } else{
    message('skipping model')
  }
}

clean_current_results <- function(data){
  ## Vote share is the total votes for each candidate in each precinct, by vote mode
  ## Total Vote Share is total across all modes, excluding "Aggregated" (overvote/undervote)
  data |>
    bind_rows(
      data |>
        filter(vote_mode != "Aggregated") |>
        summarise(precinct_total = sum(precinct_total, na.rm = TRUE), .by = c("state", "race_id", "race_name", "candidate_name", "candidate_party", "jurisdiction", "precinct_id", "virtual_precinct")) |>
        mutate(vote_mode = "Total Vote Share")
    ) |>
    group_by(state, race_id, race_name, jurisdiction, precinct_id, virtual_precinct, vote_mode) |>
    mutate(turnout_current = sum(precinct_total, na.rm = TRUE)) |>
    ungroup() |>
    mutate(vote_share_current = (precinct_total / turnout_current)) |>
    arrange(state, race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, vote_mode)
}

run_model <- function(df, model_type, dv, covars, weight, target_mode){
  covars <- paste(covars, collapse = " + ")
  mode <- df |> filter(vote_mode == target_mode)
  
  # Run model
  model <- lm(as.formula(paste(dv,"~", covars)), data = mode, weights = mode[[weight]])
  # Produce estimates
  mode$model_estimate <- predict(model, newdata = mode)
  
  if(model_type == 'turnout'){
    return(mode |> select(state, race_id, race_name, jurisdiction, precinct_id, precinct_cbs, vote_mode, !!sym(dv), model_estimate))
    
  } else if(model_type == 'vote_share'){
    return(mode |> select(state, race_id, race_name, jurisdiction, candidate_name, candidate_party, precinct_id, precinct_cbs, vote_mode, !!sym(dv), model_estimate))
  }
}

produce_predictions_turnout <- function(cur, xwalk, dv, covars, weight){
  # Create roll up by precinct for turnout, join to xwalk
  rollup <- cur |> summarise(turnout_current = sum(precinct_total, na.rm = TRUE), .by = c("state", "race_id", "race_name", "jurisdiction", "precinct_id", "virtual_precinct","vote_mode"))
  
  turnout <- bind_rows(
      rollup,
      cur |>
        filter(vote_mode %in% c("Total Vote Share","Aggregated")) |>
        summarise(turnout_current = sum(precinct_total, na.rm = TRUE), .by = c("state", "race_id", "race_name", "jurisdiction", "precinct_id", "virtual_precinct")) |>
        # This is so we have a total turnout value for the contest, includes under/overvotes
        mutate(vote_mode = "Total Turnout") |>
        select(colnames(rollup))
    ) |>
    arrange(state, jurisdiction, race_name, precinct_id, vote_mode) |>
    left_join(xwalk, by = c("state","race_name","jurisdiction","precinct_id","vote_mode")) |>
    # If we don't have any votes, treat as missing
    mutate(turnout_current = ifelse(turnout_current == 0, NA, turnout_current))
  
  # Run models, by vote_mode
  model_estimate <- map_dfr(unique(turnout$vote_mode), ~run_model(turnout, "turnout", dv, covars, weight, .x)) |>
    # For calculations, use estimate if nothing reported; otherwise use reported
    mutate(turnout_modeled = ifelse(is.na(turnout_current), model_estimate, turnout_current)) |>
    select(-c(model_estimate))
}

produce_predictions_vote_share <- function(cur, xwalk, dv, covars, weight){
  # join to xwalk
  vote_share <- cur |> left_join(xwalk, by = c("state","race_name","jurisdiction","precinct_id","vote_mode")) |>
    filter(vote_mode != 'Aggregated')
  
  # Run models, by vote_mode
  model_estimate <- map_dfr(unique(vote_share$vote_mode), ~run_model(vote_share, "vote_share", dv, covars, weight, .x)) |>
    # For calculations, use estimate if nothing reported; otherwise use reported
    mutate(vote_share_modeled = ifelse(is.na(vote_share_current), model_estimate, vote_share_current)) |>
    select(-c(model_estimate))
}

