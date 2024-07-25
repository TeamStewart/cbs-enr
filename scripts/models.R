# ============================
# Function: execute models
# used in `scripts/functions.R`
# ============================
execute_model <- function(data, state, county, race_name, time){

  if(state == 'AZ' & county == 'MARICOPA') {
    if(race_name == 'US SENATE-Republican'){
      cur_cleaned <- clean_current_results(data)
      cur_vote_share <- cur_cleaned$vote_share
      cur_turnout <- cur_cleaned$turnout
      
    } else{
      return(NULL)
    }
  }

}

clean_current_results <- function(data){
  
}

produce_predictions_vote_share <- function(curr_data, xwalk_vote_share, covars){
  
}

produce_predictions_turnout <- function(curr_data, xwalk_turnout, covars){
  
}

compile_estimates <- function(){
  
}

make_plots <- function(){
  
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
      current_estimate <- sum(cumulative_file[[variable]][cumulative_file$timestamp == max(cumulative_file$timestamp)], na.rm=T)
      current_label <- 'Election Day'
      current_color <- 'darkgreen'
    } else if(variable %in% c('estimate_absentee_absentee_lagged', 'estimate_absentee_party_demo', 'estimate_absentee_demo')){
      current_total <- sum(cumulative_file$county_total_absentee[cumulative_file$timestamp == max(cumulative_file$timestamp)])
      current_estimate <- sum(cumulative_file[[variable]][cumulative_file$timestamp == max(cumulative_file$timestamp)], na.rm=T)
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

