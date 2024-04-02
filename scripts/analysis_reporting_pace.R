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
ga_result_files <- list.files(path = "data/clean/GA", pattern = "_[0-9]{2}.csv$", full.names = T)

batches <- data.frame()
for(file_path in ga_result_files){
  batch <- read_csv(file_path) |> ungroup()
  
  batch_time <- case_when(
    str_detect(file_path, "[0-9]{4}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") ~ str_extract(file_path, "[0-9]{4}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ymd_hms(tz = "America/New_York"),
    str_detect(file_path, "[0-9]{4}_[0-9]{2}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}") ~ str_extract(file_path, "[0-9]{4}_[0-9]{2}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ydm_hms(tz = "America/New_York")
  )
  
  batch_summary <- batch |>
    mutate(timestamp = batch_time) |>
    filter(race_name %in% c('President-Republican', 'President-Democrat')) |>
    group_by(timestamp, vote_mode) |>
    summarise(
      total = sum(precinct_total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping for further manipulation
    )
  
  # Creating a summary for each timestamp and race_name across all vote_modes
  total_summary <- batch_summary |>
    group_by(timestamp) |>
    summarise(
      total = sum(total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping since we don't need it anymore
    ) |>
    mutate(vote_mode = 'Total') # Add the new vote_mode value
  
  # Combining both summaries
  final_summary <- bind_rows(batch_summary, total_summary) |>
    arrange(timestamp, desc(vote_mode)) # Optional: arrange the data as needed
  
  batches <- rbind(batches, final_summary)
}

# relabel 
batches_clean <- batches |> ungroup() |>
  filter(!(vote_mode %in% c("Provisional","Aggregated")) & !(timestamp %in% c('2024-03-13 09:42:36', '2024-03-12 18:08:01'))) |>
  arrange(timestamp, vote_mode)

total <- batches_clean$total[batches_clean$vote_mode=='Total' & batches_clean$timestamp == max(batches_clean$timestamp)]
eday <- batches_clean$total[batches_clean$vote_mode=="Election Day" & batches_clean$timestamp == max(batches_clean$timestamp)]
early <- batches_clean$total[batches_clean$vote_mode=="Early Voting" & batches_clean$timestamp == max(batches_clean$timestamp)]
mail <- batches_clean$total[batches_clean$vote_mode=="Absentee/Mail" & batches_clean$timestamp == max(batches_clean$timestamp)]

maxes <- data.frame(
  type = c('Total', "Election Day", "Early Voting", "Absentee/Mail"),
  max = c(total, eday, early, mail)
)

batches_clean <- batches_clean |> 
  left_join(maxes, by = c("vote_mode" = 'type')) |>
  mutate(vote_mode = factor(vote_mode) |> fct_relevel("Total", "Election Day", "Early Voting", "Absentee/Mail")) |>
  mutate(complete = ifelse(total > 0.99*max,T,F))

# Build plot
png("analysis/reporting_pace_ga_primary_all.png",width = 9,height = 5,units = 'in', res = 300)
ggplot(batches_clean, aes(x = timestamp, y = total, color = vote_mode, group = vote_mode)) +
  geom_point(aes(shape = complete)) +
  geom_line() +
  theme_bw(base_family = "StyreneB-Regular") +
  theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7),plot.caption = element_text(size = 7)) +
  labs(x = 'Time Reported', y = "Reported Total",title = "Georgia Presidential Preference Primary (3/12/2024)",
       subtitle = "Pace of Ballot Reporting", caption = "Data Source: Georgia Secretary of State\nGraph Source: MIT Election Data and Science Lab\n\nX indicates >99% reporting.",
       color = "") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%d %b %H:%M") +
  scale_color_manual(
    values = c("Total" = 'black', "Election Day" = "#C0BA79", "Early Voting" = "#37C256", "Absentee/Mail" = "#F6573E")
  ) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4)) +
  guides(shape = F)
dev.off()

### By party
batches <- data.frame()
for(file_path in ga_result_files){
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
  arrange(timestamp, vote_mode)

total_dem <- batches_clean$total[batches_clean$vote_mode=='Total' & batches_clean$timestamp == max(batches_clean$timestamp) & batches_clean$race_name=='Democrat']
eday_dem <- batches_clean$total[batches_clean$vote_mode=="Election Day" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Democrat']
early_dem <- batches_clean$total[batches_clean$vote_mode=="Early Voting" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Democrat']
mail_dem <- batches_clean$total[batches_clean$vote_mode=="Absentee/Mail" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Democrat']

total_rep <- batches_clean$total[batches_clean$vote_mode=='Total' & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Republican']
eday_rep <- batches_clean$total[batches_clean$vote_mode=="Election Day" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Republican']
early_rep <- batches_clean$total[batches_clean$vote_mode=="Early Voting" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Republican']
mail_rep <- batches_clean$total[batches_clean$vote_mode=="Absentee/Mail" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Republican']

maxes <- data.frame(
  race = c(rep('Democrat',4), rep('Republican',4)),
  type = rep(c('Total', "Election Day", "Early Voting", "Absentee/Mail"),2),
  max = c(total_dem, eday_dem, early_dem, mail_dem, total_rep, eday_rep, early_rep, mail_rep)
)

batches_clean <- batches_clean |> 
  left_join(maxes, by = c("vote_mode" = 'type', 'race_name' = 'race')) |>
  mutate(vote_mode = factor(vote_mode) |> fct_relevel("Total", "Election Day", "Early Voting", "Absentee/Mail")) |>
  mutate(complete = ifelse(total > 0.99*max,T,F))

# Build plot
png("analysis/reporting_pace_ga_primary.png",width = 9,height = 5,units = 'in', res = 300)
ggplot(batches_clean, aes(x = timestamp, y = total, color = vote_mode, group = vote_mode)) +
  geom_point(aes(shape = complete)) +
  geom_line() +
  facet_wrap(~race_name) +
  theme_bw(base_family = "StyreneB-Regular") +
  theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7)) +
  labs(x = 'Time Reported', y = "Reported Total",title = "Georgia Presidential Preference Primary (3/12/2024)",
       subtitle = "Pace of Ballot Reporting", caption = "Data Source: Georgia Secretary of State\nGraph Source: MIT Election Data and Science Lab\n\nX indicates >99% reporting.",
       color = "") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%d %b %H:%M") +
  scale_color_manual(
    values = c("Total" = 'black', "Election Day" = "#C0BA79", "Early Voting" = "#37C256", "Absentee/Mail" = "#F6573E")
  ) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4)) +
  guides(shape = F)
dev.off()

# North Carolina ----------------------------------------------------------
nc_result_files <- list.files(path = "data/clean/NC", pattern = "_[0-9]{2}.csv$", full.names = T)

batches <- data.frame()
for(file_path in nc_result_files){
  batch <- read_csv(file_path) |> ungroup()
  
  batch_time <- str_extract(file_path, "[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ymd_hms(tz = "America/New_York") 
  batch_summary <- batch |>
    mutate(timestamp = batch_time) |>
    filter(race_name %in% c('President-Republican', 'President-Democrat')) |>
    group_by(timestamp, vote_mode) |>
    summarise(
      total = sum(precinct_total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping for further manipulation
    )
  
  # Creating a summary for each timestamp and race_name across all vote_modes
  total_summary <- batch_summary |>
    group_by(timestamp) |>
    summarise(
      total = sum(total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping since we don't need it anymore
    ) |>
    mutate(vote_mode = 'Total') # Add the new vote_mode value
  
  # Combining both summaries
  final_summary <- bind_rows(batch_summary, total_summary) |>
    arrange(timestamp, desc(vote_mode)) # Optional: arrange the data as needed
  
  batches <- rbind(batches, final_summary)
}

# relabel 
batches_clean <- batches |> ungroup() |>
  filter(!(vote_mode %in% c("Provisional","Aggregated")) & !(timestamp %in% c('2024-02-01 15:11:28', "2024-03-12 17:05:13", "2024-03-12 18:22:29", "2024-03-13 10:04:22"))) |>
  arrange(timestamp, vote_mode)

total <- batches_clean$total[batches_clean$vote_mode=='Total' & batches_clean$timestamp == max(batches_clean$timestamp)]
eday <- batches_clean$total[batches_clean$vote_mode=="Election Day" & batches_clean$timestamp == max(batches_clean$timestamp)]
early <- batches_clean$total[batches_clean$vote_mode=="Early Voting" & batches_clean$timestamp == max(batches_clean$timestamp)]
mail <- batches_clean$total[batches_clean$vote_mode=="Absentee/Mail" & batches_clean$timestamp == max(batches_clean$timestamp)]

maxes <- data.frame(
  type = c('Total', "Election Day", "Early Voting", "Absentee/Mail"),
  max = c(total, eday, early, mail)
)

batches_clean <- batches_clean |> 
  left_join(maxes, by = c("vote_mode" = 'type')) |>
  mutate(vote_mode = factor(vote_mode) |> fct_relevel("Total", "Election Day", "Early Voting", "Absentee/Mail")) |>
  mutate(complete = ifelse(total > 0.99*max,T,F))

# Build plot
png("analysis/reporting_pace_nc_primary_all.png",width = 9,height = 5,units = 'in', res = 300)
ggplot(batches_clean, aes(x = timestamp, y = total, color = vote_mode, group = vote_mode)) +
  geom_point(aes(shape = complete)) +
  geom_line() +
  theme_bw(base_family = "StyreneB-Regular") +
  theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7),plot.caption = element_text(size = 7)) +
  labs(x = 'Time Reported', y = "Reported Total",title = "North Carolina Presidential Preference Primary (3/5/2024)",
       subtitle = "Pace of Ballot Reporting", caption = "Data Source: North Carolina State Board of Elections\nGraph Source: MIT Election Data and Science Lab\n\nX indicates >99% reporting.",
       color = "") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%d %b %H:%M") +
  scale_color_manual(
    values = c("Total" = 'black', "Election Day" = "#C0BA79", "Early Voting" = "#37C256", "Absentee/Mail" = "#F6573E")
  ) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4)) +
  guides(shape = F)
dev.off()

### By party
batches <- data.frame()
for(file_path in nc_result_files){
  batch <- read_csv(file_path) |> ungroup()
  
  batch_time <- str_extract(file_path, "[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ymd_hms(tz = "America/New_York") 
  
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
  filter(!(vote_mode %in% c("Provisional","Aggregated")) & !(timestamp %in% c('2024-02-01 15:11:28', "2024-03-12 17:05:13", "2024-03-12 18:22:29", "2024-03-13 10:04:22"))) |>
  arrange(timestamp, vote_mode)

total_dem <- batches_clean$total[batches_clean$vote_mode=='Total' & batches_clean$timestamp == max(batches_clean$timestamp) & batches_clean$race_name=='Democrat']
eday_dem <- batches_clean$total[batches_clean$vote_mode=="Election Day" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Democrat']
early_dem <- batches_clean$total[batches_clean$vote_mode=="Early Voting" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Democrat']
mail_dem <- batches_clean$total[batches_clean$vote_mode=="Absentee/Mail" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Democrat']

total_rep <- batches_clean$total[batches_clean$vote_mode=='Total' & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Republican']
eday_rep <- batches_clean$total[batches_clean$vote_mode=="Election Day" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Republican']
early_rep <- batches_clean$total[batches_clean$vote_mode=="Early Voting" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Republican']
mail_rep <- batches_clean$total[batches_clean$vote_mode=="Absentee/Mail" & batches_clean$timestamp == max(batches_clean$timestamp)& batches_clean$race_name=='Republican']

maxes <- data.frame(
  race = c(rep('Democrat',4), rep('Republican',4)),
  type = rep(c('Total', "Election Day", "Early Voting", "Absentee/Mail"),2),
  max = c(total_dem, eday_dem, early_dem, mail_dem, total_rep, eday_rep, early_rep, mail_rep)
)

batches_clean <- batches_clean |> 
  left_join(maxes, by = c("vote_mode" = 'type', 'race_name' = 'race')) |>
  mutate(vote_mode = factor(vote_mode) |> fct_relevel("Total", "Election Day", "Early Voting", "Absentee/Mail")) |>
  mutate(complete = ifelse(total > 0.99*max,T,F))

# Build plot
png("analysis/reporting_pace_nc_primary.png",width = 9,height = 5,units = 'in', res = 300)
ggplot(batches_clean, aes(x = timestamp, y = total, color = vote_mode, group = vote_mode)) +
  geom_point(aes(shape = complete)) +
  geom_line() +
  facet_wrap(~race_name) +
  theme_bw(base_family = "StyreneB-Regular") +
  theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7)) +
  labs(x = 'Time Reported', y = "Reported Total",title = "North Carolina Presidential Preference Primary (3/5/2024)",
       subtitle = "Pace of Ballot Reporting", caption = "Data Source: North Carolina State Board of Elections\nGraph Source: MIT Election Data and Science Lab\n\nX indicates >99% reporting.",
       color = "") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%d %b %H:%M") +
  scale_color_manual(
    values = c("Total" = 'black', "Election Day" = "#C0BA79", "Early Voting" = "#37C256", "Absentee/Mail" = "#F6573E")
  ) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4)) +
  guides(shape = F)
dev.off()


# Florida -----------------------------------------------------------------
## Orange
fl_result_files <- list.files(path = "data/clean/FL", pattern = "_[0-9]{2}.csv$", full.names = T)

orange_result_files <- fl_result_files[str_detect(fl_result_files,"FL_ORANGE")]

batches <- data.frame()
for(file_path in orange_result_files){
  batch <- read_csv(file_path) |> ungroup()
  
  batch_time <- case_when(
    str_detect(file_path, "[0-9]{4}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") ~ str_extract(file_path, "[0-9]{4}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ymd_hms(tz = "America/New_York"),
    str_detect(file_path, "[0-9]{4}_[0-9]{2}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}") ~ str_extract(file_path, "[0-9]{4}_[0-9]{2}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ydm_hms(tz = "America/New_York")
  )
  
  batch_summary <- batch |>
    mutate(
      timestamp = batch_time,
      precinct_total = ifelse(precinct_total=='-',NA,precinct_total) |> as.integer()
      ) |>
    filter(race_name %in% c('REP President')) |>
    group_by(timestamp, vote_mode) |>
    summarise(
      total = sum(precinct_total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping for further manipulation
    )
  
  # Creating a summary for each timestamp and race_name across all vote_modes
  total_summary <- batch_summary |>
    group_by(timestamp) |>
    summarise(
      total = sum(total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping since we don't need it anymore
    ) |>
    mutate(vote_mode = 'Total') # Add the new vote_mode value
  
  # Combining both summaries
  final_summary <- bind_rows(batch_summary, total_summary) |>
    arrange(timestamp, desc(vote_mode)) # Optional: arrange the data as needed
  
  batches <- rbind(batches, final_summary)
}

# relabel 
batches_clean <- batches |> ungroup() |>
  filter(!(vote_mode %in% c("Provisional","Aggregated")) & !(timestamp %in% c('2024-03-13 09:42:36', '2024-03-12 18:08:01'))) |>
  arrange(timestamp, vote_mode)

total <- batches_clean$total[batches_clean$vote_mode=='Total' & batches_clean$timestamp == max(batches_clean$timestamp)]
eday <- batches_clean$total[batches_clean$vote_mode=="Election Day" & batches_clean$timestamp == max(batches_clean$timestamp)]
early <- batches_clean$total[batches_clean$vote_mode=="Early Voting" & batches_clean$timestamp == max(batches_clean$timestamp)]
mail <- batches_clean$total[batches_clean$vote_mode=="Absentee/Mail" & batches_clean$timestamp == max(batches_clean$timestamp)]

maxes <- data.frame(
  type = c('Total', "Election Day", "Early Voting", "Absentee/Mail"),
  max = c(total, eday, early, mail)
)

batches_clean <- batches_clean |> 
  left_join(maxes, by = c("vote_mode" = 'type')) |>
  mutate(vote_mode = factor(vote_mode) |> fct_relevel("Total", "Election Day", "Early Voting", "Absentee/Mail")) |>
  mutate(complete = ifelse(total > 0.99*max,T,F))

# Build plot
png("analysis/reporting_pace_fl_primary_orange.png",width = 9,height = 5,units = 'in', res = 300)
ggplot(batches_clean, aes(x = timestamp, y = total, color = vote_mode, group = vote_mode)) +
  geom_point(aes(shape = complete)) +
  geom_line() +
  theme_bw(base_family = "StyreneB-Regular") +
  theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7),plot.caption = element_text(size = 7)) +
  labs(x = 'Time Reported', y = "Reported Total",title = "Orange County, FL Republican Presidential Preference Primary (3/19/2024)",
       subtitle = "Pace of Ballot Reporting", caption = "Data Source: Orange County Supervisor of Elections\nGraph Source: MIT Election Data and Science Lab\n\nX indicates >99% reporting.",
       color = "") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%d %b %H:%M") +
  scale_color_manual(
    values = c("Total" = 'black', "Election Day" = "#C0BA79", "Early Voting" = "#37C256", "Absentee/Mail" = "#F6573E")
  ) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4)) +
  guides(shape = F)
dev.off()

miami_result_files <- fl_result_files[str_detect(fl_result_files,"FL_MIAMI")]

batches <- data.frame()
for(file_path in miami_result_files){
  batch <- read_csv(file_path) |> ungroup()
  
  batch_time <- case_when(
    str_detect(file_path, "[0-9]{4}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") ~ str_extract(file_path, "[0-9]{4}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ymd_hms(tz = "America/New_York"),
    str_detect(file_path, "[0-9]{4}_[0-9]{2}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}") ~ str_extract(file_path, "[0-9]{4}_[0-9]{2}_03_[0-9]{2}_[0-9]{2}_[0-9]{2}") |> ydm_hms(tz = "America/New_York")
  )
  
  batch_summary <- batch |>
    mutate(
      timestamp = batch_time,
      precinct_total = ifelse(precinct_total=='-',NA,precinct_total) |> as.integer()
    ) |>
    filter(race_name %in% c('President-Republican')) |>
    group_by(timestamp, vote_mode) |>
    summarise(
      total = sum(precinct_total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping for further manipulation
    )
  
  # Creating a summary for each timestamp and race_name across all vote_modes
  total_summary <- batch_summary |>
    group_by(timestamp) |>
    summarise(
      total = sum(total, na.rm = TRUE),
      .groups = 'drop' # Drop grouping since we don't need it anymore
    ) |>
    mutate(vote_mode = 'Total') # Add the new vote_mode value
  
  # Combining both summaries
  final_summary <- bind_rows(batch_summary, total_summary) |>
    arrange(timestamp, desc(vote_mode)) # Optional: arrange the data as needed
  
  batches <- rbind(batches, final_summary)
}

# relabel 
batches_clean <- batches |> ungroup() |>
  filter(!(vote_mode %in% c("Provisional","Aggregated")) & !(timestamp %in% c('2024-03-13 09:42:36', '2024-03-12 18:08:01'))) |>
  arrange(timestamp, vote_mode)

total <- batches_clean$total[batches_clean$vote_mode=='Total' & batches_clean$timestamp == max(batches_clean$timestamp)]
eday <- batches_clean$total[batches_clean$vote_mode=="Election Day" & batches_clean$timestamp == max(batches_clean$timestamp)]
early <- batches_clean$total[batches_clean$vote_mode=="Early Voting" & batches_clean$timestamp == max(batches_clean$timestamp)]
mail <- batches_clean$total[batches_clean$vote_mode=="Absentee/Mail" & batches_clean$timestamp == max(batches_clean$timestamp)]

maxes <- data.frame(
  type = c('Total', "Election Day", "Early Voting", "Absentee/Mail"),
  max = c(total, eday, early, mail)
)

batches_clean <- batches_clean |> 
  left_join(maxes, by = c("vote_mode" = 'type')) |>
  mutate(vote_mode = factor(vote_mode) |> fct_relevel("Total", "Election Day", "Early Voting", "Absentee/Mail")) |>
  mutate(complete = ifelse(total > 0.99*max,T,F))

# Build plot
png("analysis/reporting_pace_fl_primary_miami.png",width = 9,height = 5,units = 'in', res = 300)
ggplot(batches_clean, aes(x = timestamp, y = total, color = vote_mode, group = vote_mode)) +
  geom_point(aes(shape = complete)) +
  geom_line() +
  theme_bw(base_family = "StyreneB-Regular") +
  theme(legend.position = 'bottom',axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size = 7),plot.caption = element_text(size = 7)) +
  labs(x = 'Time Reported', y = "Reported Total",title = "Miami-Dade County, FL Republican Presidential Preference Primary (3/19/2024)",
       subtitle = "Pace of Ballot Reporting", caption = "Data Source: Miami-Dade County Supervisor of Elections\nGraph Source: MIT Election Data and Science Lab\n\nX indicates >99% reporting.",
       color = "") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_datetime(date_breaks = "30 mins", date_labels = "%d %b %H:%M") +
  scale_color_manual(
    values = c("Total" = 'black', "Election Day" = "#C0BA79", "Early Voting" = "#37C256", "Absentee/Mail" = "#F6573E")
  ) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4)) +
  guides(shape = F)
dev.off()
