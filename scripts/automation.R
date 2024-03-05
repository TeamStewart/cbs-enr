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
execute <- function() {
  message(sprintf("%s: Running tar_make", format(Sys.time(), format="%Y-%m-%d %H:%M:%S")))
  # Use the system function to execute the R scrip
  tar_make()
}

# Main loop to schedule and execute the script every 4 minutes
while (TRUE) {
  execute_script()

  # push and commit to git
  git2r::add(path = "*")
  git2r::commit(message = "latest change")
  
  Sys.sleep(900)  # Sleep for 15 minutes (900 seconds)
}