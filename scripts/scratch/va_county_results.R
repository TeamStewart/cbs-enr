library(tidyverse)
library(jsonlite)
library(fs)
library(glue)

# Raw files
raw_jsons <- dir_ls(
  "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data/25_general/VA/raw/"
)

retrieve_county_results <- function(json) {
  json_data <- tryCatch(
    read_json(json),
    error = function(e) {
      message(glue("Invalid JSON file: {json}"))
      return(NULL)
    }
  )
  
  if (is.null(json_data)) {
    return(NULL)
  }
  
  message(glue("Processing file: {json}"))
  timestamp = basename(json) |>
    str_remove_all("VA_|\\.json$") |>
    ymd_hms() |>
    with_tz("America/New_York")
  timestamp_filename = basename(json) |>
    str_remove_all("VA_|\\.json$")
  
  base = tibble(data = json_data |> pluck('localResults')) |>
    hoist(data, jurisdiction = "name", items = "ballotItems") |>
    select(-data) |>
    unnest_longer(items)
  
  base |>
    hoist(
      items,
      race_id = "id",
      race_name = "name",
      options = "ballotOptions",
      magnitude = "voteFor"
    ) |>
    select(-items) |>
    unnest_longer(options) |>
    hoist(
      options,
      candidate_name = "name",
      candidate_party = "politicalParty",
      county_results = "groupResults"
    ) |>
    select(-options) |>
    unnest_longer(county_results) |>
    hoist(
      county_results,
      vote_mode = "groupName",
      county_total = "voteCount",
      virtual_precinct = "isFromVirtualPrecinct"
    ) |>
    mutate(
      timestamp = timestamp,
      state = 'VA',
      jurisdiction = ifelse(
        jurisdiction == "KING & QUEEN COUNTY",
        "KING AND QUEEN COUNTY",
        jurisdiction
      ),
      # jurisdiction = jurisdiction |> str_remove(regex("County$", ignore_case = TRUE)) |> str_squish() |> str_to_title(),
      race_name = case_when(
        str_detect(race_name, regex("Lieutenant Governor", ignore_case = TRUE)) ~ "Lt Governor",
        str_detect(race_name, regex("House of Delegates", ignore_case = TRUE)) ~ paste("State House", str_extract(race_name, "\\d+")),
        .default = race_name
      ),
      candidate_party = case_when(
        str_detect(candidate_party, regex("Democrat|Dem", ignore_case = TRUE)) ~ "Democrat",
        str_detect(candidate_party, regex("Repub|Rep", ignore_case = TRUE)) ~ "Republican",
        str_detect(candidate_party, regex("Liber|Lib", ignore_case = TRUE)) ~ "Libertarian",
        str_detect(candidate_party, regex("Green|Grn", ignore_case = TRUE)) ~ "Green",
        str_detect(
          candidate_party,
          regex("Ind|Independent", ignore_case = TRUE)
        ) ~ "Independent",
        .default = "Other"
      ),
      # Recode candidate names
      candidate_name = case_when(
        race_name == "Governor" &
          str_detect(candidate_name, regex("Spanberger", TRUE)) ~ "Abigal Spanberger",
        race_name == "Governor" &
          str_detect(candidate_name, regex("Winsome", TRUE)) ~ "Winsome Earle-Sears",
        race_name == "Attorney General" &
          str_detect(candidate_name, regex("Jones", TRUE)) ~ "Jay Jones",
        race_name == "Attorney General" &
          str_detect(candidate_name, regex("Miyares", TRUE)) ~ "Jason Miyares",
        candidate_name == "Write-In" ~ "Write-ins",
        .default = candidate_name
      ),
      vote_mode = case_when(
        str_detect(vote_mode, regex("Election Day", ignore_case = TRUE)) ~ "Election Day",
        str_detect(
          vote_mode,
          regex("Early Voting|Advanced Voting", ignore_case = TRUE)
        ) ~ "Early Voting",
        str_detect(
          vote_mode,
          regex("Mail|Absentee|Post-Election", ignore_case = TRUE)
        ) ~ "Absentee/Mail",
        str_detect(vote_mode, regex("Provisional", ignore_case = TRUE)) ~ "Provisional",
        .default = str_to_title(vote_mode)
      )
    ) |>
    summarize(county_total = sum(county_total, na.rm = TRUE),
              .by = -county_total) |>
    select(
      state,
      race_id,
      race_name,
      candidate_name,
      candidate_party,
      magnitude,
      jurisdiction,
      timestamp,
      vote_mode,
      county_total
    ) |>
    arrange(race_name, candidate_party, candidate_name, jurisdiction) |>
    write_csv(
      glue::glue(
        "/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data/25_general/VA/clean_county/VA_{timestamp_filename}.csv"
      )
    )
}

walk(raw_jsons, retrieve_county_results)