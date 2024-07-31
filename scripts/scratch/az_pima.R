library(rvest)
library(tidyverse)

path = "https://www.pima.gov/2865/Election-Results"
state = 'AZ'
county = 'PIMA'

csv_link <- (read_html(path) |>
  # TODO: Find node identifier
  html_nodes("ul:nth-child(2) li:nth-child(4) a") |>
  html_attr("href"))[1]

raw_csv <- read_csv(csv_link, col_names = FALSE)

# Write raw files
write_csv(raw_csv, glue("raw_csv/raw/AZ/{state}_{county}_{type}_raw_{timestamp}.csv"))


# TODO: Fix start row, col vals
start_row <- 4
start_column <- 19

# Extract metaraw_csv from the CSV
vote_modes <- raw_csv[[1]][start_row:nrow(raw_csv)]
precinct_ids <- raw_csv[[3]][start_row:nrow(raw_csv)]
race_name <- as.character(raw_csv[1, start_column:ncol(raw_csv)])
candidate_party <- as.character(raw_csv[2, start_column:ncol(raw_csv)])
candidate_name <- as.character(raw_csv[3, start_column:ncol(raw_csv)])
# Generate unique race_id for each unique race_name
unique_race_names <- unique(race_name)
race_ids <- match(race_name, unique_race_names)

# Create a new raw_csvframe to hold the cleaned raw_csv
cleaned_raw_csv <- data.frame(
  state = state,
  race_id = rep(race_ids, each = length(precinct_ids)),
  race_name = rep(race_name, each = length(precinct_ids)),
  candidate_name = rep(candidate_name, each = length(precinct_ids)),
  candidate_party = rep(candidate_party, each = length(precinct_ids)),
  jurisdiction = county,
  precinct_id = rep(precinct_ids, times = length(race_name)),
  virtual_precinct = FALSE,
  vote_mode = rep(vote_modes, times = length(race_name)),
  precinct_total = integer(length(precinct_ids) * length(race_name)),
  stringsAsFactors = FALSE
)

# Process each column containing precinct totals
cand_index <- 1
for (col_index in start_column:ncol(raw_csv)) {
  precinct_totals <- raw_csv[start_row:nrow(raw_csv), col_index] |> as_vector() |> as.integer()
  cleaned_raw_csv$precinct_total[cleaned_raw_csv$candidate_name == candidate_name[cand_index]] <- precinct_totals
  cand_index <- cand_index + 1
}

cleaned_raw_csv |> filter(precinct_id != 'COUNTY TOTALS') |>
  mutate(
    precinct_total = as.integer(precinct_total),
    race_name = case_match(
      race_name,
      "DEM U.S. Senator" ~ "US SENATE-Democrat", 
      "U.S. REPRESENTATIVE IN CONGRESS DIST. 6 - DEM" ~ "US HOUSE-06-Democrat",
      "U.S. REPRESENTATIVE IN CONGRESS DIST. 7 - DEM" ~ "US HOUSE-07-Democrat",
      "REP U.S. Senator" ~ "US SENATE-Republican",
      "REP U.S. Representative in Congress District 6" ~ "US HOUSE-06-Republican",
      "REP U.S. Representative in Congress District 7" ~ "US HOUSE-07-Republican",
      .default = NA_character_
    ),
    candidate_name = case_when(
      str_detect(candidate_name, regex("Write-in", ignore_case = T)) ~ "Write-ins",
      str_detect(candidate_name, regex("ENGEL", ignore_case = T)) ~ "Kirsten Engel",
      str_detect(candidate_name, regex("Ciscomani", ignore_case = T)) ~ "Juan Ciscomani",
      str_detect(candidate_name, regex("Winn", ignore_case = T)) ~ "Kathleen Winn",
      str_detect(candidate_name, regex("GRIJALVA", ignore_case = T)) ~ "Raúl Grijalva",
      str_detect(candidate_name, regex("BUTIEREZ", ignore_case = T)) ~ "Daniel Butierez",
      str_detect(candidate_name, regex("GALLEGO", ignore_case = T)) ~ "Ruben Gallego",
      str_detect(candidate_name, regex("LAKE", ignore_case = T)) ~ "Kari Lake",
      str_detect(candidate_name, regex("LAMB", ignore_case = T)) ~ "Mark Lamb",
      str_detect(candidate_name, regex("REYE", ignore_case = T)) ~ "Elizabeth Reye",
      candidate_name == "UNDER VOTES" ~ "Undervote",
      candidate_name == "OVER VOTES"~ "Overvote",
      TRUE ~ candidate_name
    ),
    candidate_party = case_match(
      candidate_party,
      "DEM" ~ "Democrat",
      "REP" ~ "Republican",
      .default = NA_character_
    ),
    vote_mode = case_when(
      candidate_name == "Undervote" ~ "Aggregated",
      candidate_name == "Overvote" ~ "Aggregated",
      vote_mode == "POLLS" ~ "Election Day",
      vote_mode == "EARLY" ~ "Early Voting",
      vote_mode == "PROVISIONAL" ~ "Provisional"
    )
  ) |>
  filter(!is.na(race_name)) |>
  summarise(precinct_total = sum(precinct_total, na.rm = T), .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","vote_mode"))
