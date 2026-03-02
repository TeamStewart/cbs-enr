
files = list.files(
  "~/Dropbox (MIT)/Research/CBS-MIT Election Data/26_primaries/TX/raw", 
  pattern = "\\d+\\.zip$",
  recursive = TRUE,
  full.names = TRUE
)
files = files[fs::file_size(files) > 3000]

base = map2(fs::path_dir(files), files, enightdata:::get_data_clarity) |> list_rbind()

d = base |> 
  select(-percent_reporting) |> 
  filter(
    year(time) == 2024,
    votes > 0,
    str_detect(race_name, regex("President", TRUE))
  ) |> 
  mutate(
    candidate = str_remove(candidate, regex("^Rep|^Dem|^Lib|^Grn", TRUE)) |> str_squish() |> str_to_lower(),
    candidate = case_when(
      str_detect(candidate, "harris") ~ "harris",
      str_detect(candidate, "trump") ~ "trump",
      str_detect(candidate, "stein") ~ "stein",
      .default = NA
    )
  ) |> 
  drop_na(candidate) |> 
  distinct()

diff = d |> 
  select(jurisdiction, precinct_id, time, method, candidate, votes) |> 
  mutate(
    method = case_when(
      str_detect(method, "Mail|Absentee|MAIL") ~ "Mail",
      str_detect(method, "Election|ELECTION") ~ "Election",
      str_detect(method, "Early|EARLY") ~ "Early",
      .default = NA
    )
  ) |> 
  drop_na(method) |> 
  distinct() |> 
  mutate(
    perc = round(votes / sum(votes), 2),
    .by = c(jurisdiction, precinct_id, method, time)
  ) |> 
  select(-votes) |> 
  pivot_wider(names_from = candidate, values_from = perc)

diff |>
  filter(time < ymd_hms("2024-11-06 18:00:00")) |> 
  distinct(jurisdiction, precinct_id, method, trump, .keep_all = TRUE) |>
  drop_na(trump) |>
  summarize(
    diff = max(trump) - min(trump), 
    .by = c(jurisdiction, precinct_id, method)
  ) |>
  ggplot(aes(x = diff)) +
  geom_histogram() +
  facet_wrap(~method) +
  theme_bw(base_size = 14) +
  labs(
    x = "Difference in Max and Min Trump Vote Share",
    y = "# Precincts"
  )
