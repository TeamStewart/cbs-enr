##################################################
## Project: CBS ENR Super Tuesday 2024
## Script purpose: Automate tar_make()
## Date: March 2024
## Author:
##################################################

library(targets)
library(tarchetypes)
library(lubridate)

# Function to execute the R script
execute_script <- function() {
  message(sprintf("%s: Running tar_make", format(Sys.time(), format="%Y-%m-%d %H:%M:%S")))
  # Use the system function to execute the R scrip
  tar_make()
}

# Main loop to schedule and execute the script every 4 minutes
while (TRUE) {
  execute_script()

  # push and commit to git
  # to get this to work, (1) https://docs.ropensci.org/git2r/reference/cred_token.html
  # (2) https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
  # (3) run git config --global http.followRedirects true
  git2r::add(path = "*")
  git2r::commit(message = "latest change")
  git2r::push(credentials = git2r::cred_token())
  
  Sys.sleep(480)  # Sleep for 8 minutes (300 seconds)
}
