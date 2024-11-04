times = read_csv(glue("{PATH_DROPBOX}/22_general/GA_allReports.csv")) |> 
  filter(race_name == "US Senate", timestamp > ymd_hm("2022-11-08 12:00PM", tz = "US/Eastern"), timestamp < ymd("2023-01-01")) |> 
  mutate(
    timestamp = round_date(timestamp, unit = "5 min")
  ) |> 
  distinct(timestamp) |> 
  pull() |> 
  sort()

get_data <- function(time){
  
  read_csv(glue("{PATH_DROPBOX}/22_general/GA_allReports_senate.csv")) |> 
    filter(timestamp <= time, timestamp > ymd("2022-11-06")) |> 
    filter(timestamp == max(timestamp), .by = c(state, jurisdiction, precinct_id)) |>
    select(state, jurisdiction, precinct_id, candidate_name, vote_mode, precinct_total, timestamp) |>
    mutate(
      candidate_name = case_when(
        str_detect(vote_mode, fixed("Undervote", ignore_case=TRUE)) ~ "Undervote",
        str_detect(vote_mode, fixed("Overvote", ignore_case=TRUE)) ~ "Overvote",
        .default = candidate_name
      ),
      vote_mode = case_match(
        vote_mode,
        "Absentee by Mail Votes" ~ "Absentee/Mail",
        "Advance Voting Votes" ~ "Early Voting",
        "Election Day Votes" ~ "Election Day",
        "Provisional Votes" ~ "Provisional",
        .default = NA_character_
      ),
      jurisdiction = str_to_upper(jurisdiction),
      precinct_id = str_to_upper(precinct_id)
    )
  
}

file_delete(glue("{PATH_DROPBOX}/24_general/GA/GA_NA_modeling.csv"))

walk(times[35:75],
  \(x) get_data(x) |> run_models("GA", "NA", x, FALSE)
)

tar_invalidate(plot_margin2020_GA_NA)
tar_make(plot_margin2020_GA_NA)
tar_read(plot_margin2020_GA_NA)

tar_invalidate(plot_voteShare_GA_NA)
tar_make(plot_voteShare_GA_NA)
tar_read(plot_voteShare_GA_NA)

read_csv(
  glue("{PATH_DROPBOX}/24_general/GA/GA_NA_modeling.csv"), 
  locale = locale(tz="US/Eastern")
) |> 
  filter(timestamp < ymd("2023-01-01")) |> 
  select(demShare_lower:repShare_upper)

m = get_data(times[50]) |> run_models("GA", "NA", times[50], FALSE)

data = get_data(times[58])

m$data_history |> 
  filter(reported_all) |> 
  select(county, precinct_24, vote_mode, reported_eday:reported_all)

m$summaries_byMode
