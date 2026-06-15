##################################################
## Project: CBS ENR
## Script purpose: Automate tar_make()
## Date: March 2024
## Author: Joseph R. Loffredo and Mason Reece
##################################################

INTERVAL = 480 # minutes
googlesheets4::gs4_auth(email = TRUE)
googledrive::drive_auth(email = TRUE)

# Function to execute the R script
execute_script <- function() {
  message(sprintf("%s: Running tar_make", format(Sys.time(), format="%Y-%m-%d %H:%M:%S")))
  # Use the system function to execute the R script
  targets::tar_make()
}

# Main loop to schedule and execute the script every INTERVAL minutes
while (TRUE) {
  print(glue::glue(
    "Started at {format(Sys.time(), format='%Y-%m-%d %H:%M:%S')}, waiting..."
  ))

  start_time <- Sys.time()

  run_time <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%S") |>
    stringr::str_replace_all("-|:| ", "_")

  fs::dir_create("logs/")

  # pull down latest `enightdata`
  cred <- git2r::cred_token()
  git2r::pull("../enightdata/", credentials = cred)

  con <- file(glue::glue("logs/automation_{run_time}.log"))
  sink(con, append = TRUE)
  sink(con, append = TRUE, type = "message")

  execute_script()

  # Restore output to console
  sink()
  sink(type = "message")

  # push and commit to git
  # to get this to work, (1) https://docs.ropensci.org/git2r/reference/cred_token.html
  # (2) https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
  # (3) run git config --global http.followRedirects true making some changes here
  git2r::add(path = "*")
  git2r::commit(message = glue::glue("latest pull {run_time}"))
  git2r::push(".")
  # git2r::push(repo,
  #             name = "origin",
  #             refspec = "refs/heads/main:refs/heads/main",
  #             credentials = cred)

  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1zWKr_cXeDHnw6sbxzAPpGG92E-Di6nmB4Q4fa-OWuic/edit?gid=0#gid=0",
    sheet = "Sheet1",
    col_types = "ccccllll"
  ) |>
    enightdata::write_local("metadata.csv") |>
    invisible()

  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
  sleep_time <- max(0, 60 * INTERVAL - elapsed)

  print("Finished... Zzzzz")

  Sys.sleep(sleep_time)
}
