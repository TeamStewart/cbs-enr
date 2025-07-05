##################################################
## Project: CBS ENR
## Script purpose: Helper functions
## Date: November 2024
##################################################

# Total votes by timestamp and vote mode
total_votes_time_mode <- function(state, county){
  if(!is.na(county)){
    files <- list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/clean"), pattern = "csv", full.names = TRUE)
  } else {
    files <- list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/clean"), paste0(county, ".*\\.csv$"), full.names = TRUE)
  }
  
  lapply(files, fread) |> 
    rbindlist() |>
    summarise(total_votes = sum(precinct_total), .by = c("timestamp", "vote_mode"))
}
  
total_precincts <- function(){
  if(!is.na(county)){
    files <- list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/clean"), pattern = "csv", full.names = TRUE)
  } else {
    files <- list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/clean"), paste0(county, ".*\\.csv$"), full.names = TRUE)
  }
  
  all_results <- lapply(files, fread) |> rbindlist()
  
  all_results |>
    summarise(
      total_precincts = n_distinct(precinct_id),
      precincts_with_votes = n_distinct(precinct_id[precinct_total > 0]),
      .by = "timestamp")
}

