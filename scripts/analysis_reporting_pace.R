##################################################
## Project: CBS ENR Super Tuesday 2024
## Script purpose: Visualize reporting pace
## Date: March 2024
## Author: Joe Loffredo
##################################################

rm(list = ls())
gc()

library(tidyverse)


# Georgia -----------------------------------------------------------------
result_files <- list.files(path = "data/clean/GA", pattern = "_[0-9]{2}.csv$", full.names = T)

batches <- data.frame()
for(file_path in result_files){
  batch <- read_csv(file_path) |> ungroup()
  
  batch_time <- case_when(
    str_detect(file_path, "[0-9]{4}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") ~ str_extract(file_path, "[0-9]{4}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ymd_hms(tz = "America/New_York"),
    str_detect(file_path, "[0-9]{4}_[0-9]{2}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}") ~ str_extract(file_path, "[0-9]{4}_[0-9]{2}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ydm_hms(tz = "America/New_York")
  )
  
  batch_summary <- batch |>
    mutate(timestamp = batch_time) |>
    filter(race_name %in% c('President-Republican', 'President-Democrat')) |>
    group_by(timestamp, race_name, vote_mode) |>
    summarise(
      total = sum(precinct_total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping for further manipulation
    )
  
  # Creating a summary for each timestamp and race_name across all vote_modes
  total_summary <- batch_summary |>
    group_by(timestamp, race_name) |>
    summarise(
      total = sum(total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping since we don't need it anymore
    ) |>
    mutate(vote_mode = 'Total') # Add the new vote_mode value
  
  # Combining both summaries
  final_summary <- bind_rows(batch_summary, total_summary) |>
    arrange(timestamp, race_name, desc(vote_mode)) # Optional: arrange the data as needed
  
  batches <- rbind(batches, final_summary)
}

# relabel 
batches_clean <- batches |> ungroup() |>
  mutate(
    race_name = case_match(
      race_name,
      "President-Democrat" ~ "Democrat",
      "President-Republican" ~ "Republican"
    ),
    vote_mode = factor(vote_mode) |> fct_relevel("Total", "Election Day", "Early Voting", "Absentee/Mail")
  ) |>
  filter(!(vote_mode %in% c("Provisional","Aggregated")) & !(timestamp %in% c('2024-03-13 09:42:36', '2024-03-12 18:08:01'))) |>
  arrange(timestamp, vote_mode) |>
  group_by(vote_mode, race_name) |>
  mutate(cumulative_total = cumsum(total)) |>
  ungroup()

# Build plot
ggplot(batches_clean, aes(x = timestamp, y = total, color = vote_mode, group = vote_mode)) +
  geom_point() +
  geom_line() +
  facet_wrap(~race_name) +
  theme_bw(base_family = "StyreneB-Regular") +
  theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7)) +
  labs(x = 'Time Reported', y = "Reported Total",title = "Georgia Presidential Preference Primary (3/12/2024)",
       subtitle = "Pace of Ballot Reporting", caption = "Graph Source: MIT Election Data and Science Lab",
       color = "Type") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%d %b %H:%M") +
  scale_color_manual(
    values = c("Total" = 'black', "Election Day" = "#C0BA79", "Early Voting" = "#37C256", "Absentee/Mail" = "#F6573E")
  )


