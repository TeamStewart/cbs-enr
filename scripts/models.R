# ============================
# Function: execute models
# ============================
run_models <- function(data, state, county, model_swing, model_turnout) {
  if (model_swing) run_model_swing(data, state, county)
  if (model_turnout) run_model_turnout(data, state, county)

  return(NULL)
}

run_model_swing <- function(data, state, county){
  
  
}

run_model_turnout <- function(data, state, county){
  
  
}
