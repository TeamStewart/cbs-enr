##################################################
## Project: CBS ENR
## Script purpose: Automate tar_make()
## Date: March 2024
## Author:
##################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(targets)
  library(tarchetypes)
})

# Function to execute the R script
execute_script <- function() {
  message(sprintf("%s: Running tar_make", format(Sys.time(), format="%Y-%m-%d %H:%M:%S")))
  # Use the system function to execute the R script
  tar_make()
}

# Main loop to schedule and execute the script every 30 minutes
while (TRUE) {

  print(glue::glue("Started at {format(Sys.time(), format='%Y-%m-%d %H:%M:%S')}, waiting..."))

  start_time <- Sys.time()
  
  run_time <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S") |> str_replace_all("-|:| ", "_")
  
  fs::dir_create("logs/")
  
  con <- file(glue::glue("logs/automation_{run_time}.log"))
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  
  execute_script()
  
  # Restore output to console
  sink() 
  sink(type="message")
  
  # push and commit to git
  # to get this to work, (1) https://docs.ropensci.org/git2r/reference/cred_token.html
  # (2) https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
  # (3) run git config --global http.followRedirects true
  cred <- git2r::cred_token()
  repo <- git2r::repository(".")
  git2r::add(path = "*")
  git2r::commit(message = glue::glue("latest pull {run_time}"))
  git2r::push()
  # git2r::push(repo, 
  #             name = "origin", 
  #             refspec = "refs/heads/main:refs/heads/main", 
  #             credentials = cred)
  
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  sleep_time <- max(0, 60*30 - elapsed)

  print("Finished... Zzzzz")
  
  Sys.sleep(sleep_time)
}
