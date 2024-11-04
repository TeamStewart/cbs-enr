times = read_csv(glue("{PATH_DROPBOX}/22_general/GA_allReports.csv")) |> 
  filter(race_name == "US Senate", timestamp > ymd_hm("2022-11-05 4:00PM", tz = "US/Eastern"), timestamp < ymd("2023-01-01")) |> 
  mutate(
    timestamp = round_date(timestamp, unit = "5 min")
  ) |> 
  distinct(timestamp) |> 
  pull() |> 
  sort()

walk(times[1:100], 
  \(x) get_data("GA", "NA", x, path = "https://app.enhancedvoting.com/cdn/results/Georgia/export-2024NovGen.json") |> 
    run_models("GA", "NA", x, FALSE)
)


data = get_data("GA", "NA", times[10], path = "https://app.enhancedvoting.com/cdn/results/Georgia/export-2024NovGen.json")
