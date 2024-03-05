library(targets)
library(tarchetypes)
library(lubridate)

# schedule_script.R
# This R script will run tar_make every 4 minutes, commit to git.

# Path to the R script you want to run
script_to_run <- "scripts/media_export_cleaner.R"

# Function to execute the R script
execute <- function() {
  # Use the system function to execute the R scrip
  system(paste("Rscript", script_to_run))
}

# Main loop to schedule and execute the script every 4 minutes
while (TRUE) {
  message(sprintf("\n\nexecuting script: %s\n\n",format(Sys.time(), format="%Y-%m-%d %H:%M:%S")))
  execute_script()
  message(sprintf("\n\nscript finished, waiting 4 minutes: %s\n\n",format(Sys.time(), format="%Y-%m-%d %H:%M:%S")))
  Sys.sleep(240)  # Sleep for 4 minutes (240 seconds)
}