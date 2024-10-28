##################################################
## Project: CBS ENR
## Script purpose: Automate tar_make()
## Date: March 2024
## Author:
##################################################

library(tidyverse)
library(targets)
library(tarchetypes)
library(lubridate)
library(glue)

# Function to execute the R script
execute_script <- function() {
  message(sprintf("%s: Running tar_make", format(Sys.time(), format="%Y-%m-%d %H:%M:%S")))
  # Use the system function to execute the R scrip
  tar_make()
}

# Main loop to schedule and execute the script every 4 minutes
while (TRUE) {
  
  run_time <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S") |> str_replace_all("-|:| ", "_")
  
  dir_create("logs/")
  
  con <- file(glue("automation_{run_time}.log"))
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
  git2r::add(path = "*")
  git2r::commit(message = glue("latest pull {run_time}"))
  git2r::push(credentials = git2r::cred_token())
  
  Sys.sleep(3600)  # Sleep for 60 minutes (3600 seconds)
}
