##################################################
## Project: CBS Election Night
## Script purpose: Build AZ State Primary xwalks
## Date: July 2024
##################################################

library(tidyverse)
library(data.table)
library(janitor)

rm(list = ls())
gc()

previous_results <- "data/input/AZ/08-02-2022-2b Final SOV and Official Canvass Report AUG2022.txt"
precinct_demos <- "data/input/AZ/az_04013_20240524_demo_prec_20240723.csv"

# Clean previous election results
clean_previous_results <- read.table(previous_results, header = TRUE, sep = "\t", quote = "") |>
  # recode variables
  mutate(
    state = 'AZ',
    race_name = case_match(
      ContestName,
      "REP US Senate" ~ "US SENATE-Republican", 
      "REP Governor" ~ "GOVERNOR-Republican",
      .default = NA_character_
    ),
    candidate_lastname = case_match(
      CandidateName,
      "BERTONE, FRANK" ~ "BERTONE",
      "BOZIC, DAVID SAMUEL" ~ "BOZIC",
      "BRNOVICH, MARK" ~ "BRNOVICH",
      "FINERD, PATRICK" ~ "FINERD",
      "LAKE, KARI" ~ "LAKE",
      "LAMON, JIM" ~ "LAMON",
      "MASTERS, BLAKE" ~ "MASTERS",
      "MCGUIRE, MICHAEL \"MICK\"" ~ "MCGUIRE",
      "NEELY, SCOTT DAVID" ~ "NEELY",
      "NOT QUALIFIED" ~ "NQ",
      "OLSON, JUSTIN" ~ "OLSON",
      "ROLDAN, CARLOS" ~ "ROLDAN",
      "SALMON, MATT" ~ "SALMON",
      "SCHATZ, ALEX" ~ "SCHATZ",
      "TAYLOR ROBSON, KARRIN" ~ "ROBSON",
      "TULLIANI-ZEN, PAOLA \"Z.\"" ~ "TULLIANI",
      "Write-in" ~ "W",
      .default = CandidateName
    ),
    candidate_party = case_when(
      CandidateAffiliation == "DEM" ~ "Democrat",
      CandidateAffiliation == "REP" ~ "Republican",
      str_detect(race_name,"Democrat") ~ "Democrat",
      str_detect(race_name,"Republican") ~ "Republican",
      CandidateName %in% c("W", "NQ") & str_detect(race_name,"Democrat") ~ "Democrat",
      CandidateName %in% c("W", "NQ") & str_detect(race_name,"Republican") ~ "Republican",
      TRUE ~ NA_character_
    ),
    jurisdiction = 'MARICOPA',
    virtual_precinct = FALSE,
    Aggregated = Overvotes + Undervotes,
    `Election Day` = Votes_ELECTION.DAY,
    `Early Voting` = Votes_EARLY.VOTE,
    Provisional = Votes_PROVISIONAL
  ) |>
  filter(!is.na(race_name)) |>
  pivot_longer(cols = c("Aggregated","Election Day", "Early Voting", "Provisional"),names_to = "vote_mode", values_to = "precinct_total") |>
  select(state, race_id = ContestId, race_name, candidate_lastname,
         candidate_party, jurisdiction, precinct_id = PrecinctName, virtual_precinct,vote_mode, precinct_total)

# Create vote total precinct roll up
## Total Turnout included undervotes and overvotes; Total Vote Share does not, per Charles
vote_totals <- clean_previous_results |>
  filter(vote_mode != "Aggregated") |>
  summarise(grand_total = sum(precinct_total, na.rm = TRUE), .by = c("state", "jurisdiction", "race_name", "precinct_id")) |>
  mutate(vote_mode = "Total Vote Share") |>
  bind_rows(
    clean_previous_results |>
      summarise(grand_total = sum(precinct_total, na.rm = TRUE), .by = c("state", "jurisdiction", "race_name", "precinct_id")) |>
      mutate(vote_mode = "Total Turnout"),
    clean_previous_results |>
      summarise(grand_total = sum(precinct_total, na.rm = TRUE), .by = c("state", "jurisdiction", "race_name", "precinct_id", "vote_mode"))
  ) |>
  select(state, jurisdiction, race_name, precinct_id, vote_mode, grand_total) |>
  arrange(state, jurisdiction, race_name, precinct_id, vote_mode)

# Create a Candidate total vote share row
candidate_totals <- clean_previous_results |>
  filter(vote_mode != "Aggregated") |>
  summarise(
    precinct_total = sum(precinct_total, na.rm = TRUE), 
    .by = c("state","race_id","race_name","candidate_lastname","candidate_party","jurisdiction",
            "precinct_id","virtual_precinct")) |>
  mutate(vote_mode = "Total Vote Share") |>
  select(colnames(clean_previous_results))

# Dataframe for predicting vote shares
vote_shares <- bind_rows(clean_previous_results, candidate_totals) |>
  left_join(vote_totals, by = c("state", "jurisdiction", "race_name", "precinct_id", "vote_mode")) |>
  filter(vote_mode != 'Aggregated') |>
  mutate(pct = (precinct_total / grand_total) * 100) |>
  unite("race_candidate", race_name, candidate_lastname, sep = "_", remove = FALSE) |>
  pivot_wider(id_cols = c(state, jurisdiction, precinct_id, vote_mode), names_from = race_candidate, values_from = pct, names_glue = "{race_candidate}_pct") |>
  arrange(state, jurisdiction, precinct_id, vote_mode) |>
  clean_names()

output <- vote_totals |> left_join(vote_shares, by = c("state","jurisdiction","precinct_id","vote_mode")) |>
  rename(total_2022 = grand_total)

# Append Kabir's precinct demos
precinct_demos <- read_csv(precinct_demos) |> clean_names() |>select(precinct_id = precinct, precinct_cbs, total_reg = n, total_reg_dem = dem, total_reg_rep = rep, p_male:p_race1_col)
output <- output |> left_join(precinct_demos, by = c("precinct_id"))


# Add Charles magafactor
magafactor <- read_csv("data/input/AZ/magafactor.csv") |> select(precinct_id = precinctname, magafactor, magamean)
output <- output |> left_join(magafactor, by = c("precinct_id"))

output <- output |>
  select(state, jurisdiction, precinct_id, precinct_cbs, race_name, vote_mode, total_2022, total_reg, total_reg_dem, total_reg_rep, p_male:magamean)

# Save 
write_csv(output, "data/input/AZ/AZ_MARICOPA_state_primary_xwalk.csv")
