##################################################
## Project: CBS ENR 2025
## Script purpose: Check VA State Leg
## Date: October 2025
##################################################

library(tidyverse)
library(readxl)
library(glue)

DATA_DIR <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data'
candidates_list <- read_excel(glue("{DATA_DIR}/25_general/input_data/VA/2025NG-House-09-30-2025.xlsx"))

# Check which districts have more than 1 candidate
competitive_districts <- candidates_list |>
  summarise(n = n(),.by = c('Office Title','District')) |>
  filter(n > 1) |>
  mutate(
    `Office Title` = str_remove(`Office Title`, "Member, "),
    race_name = glue("{`Office Title`}, District {District}")
  ) |>
  pull(race_name)

saveRDS(competitive_districts, file = glue("{DATA_DIR}/25_general/input_data/VA/contested_hod_races.rds"))
