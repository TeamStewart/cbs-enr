tar_load_globals()

covars = c('age1', 'age2', 'age3', 'white', 'black', 'col')
# covars = c("vote_mode")

# some testing data to quickly load if needed
# data = tests[2]; state = "VA"; county = NA; timestamp = tar_read(timestamp_VA_NA); history = tar_read(history_VA_NA); covars = c("votes_potus_24_dem"); method = "lm"; uncertainty = "conformal"; outcome = "votes_governor_25_dem_precinct_total"; subset = ''; obs_cutoff = 0.25; residualize = TRUE; office = "Governor"

tests = list.files(
  "~/Dropbox (MIT)/Research/CBS-MIT Election Data/25_general/input_data/VA/test_files",
  pattern = "VA_.*?$",
  full.names = TRUE
)

truth = read_csv(tests[length(tests)]) |>
  summarize(
    truth = sum(precinct_total, na.rm = TRUE),
    nobs = n(),
    .by = c(candidate_party, vote_mode)
  ) |>
  filter(
    candidate_party %in% c("Democrat", "Republican"),
    vote_mode %in% c("Early Voting", "Absentee/Mail", "Election Day")
  ) |>
  mutate(
    share = truth / sum(truth, na.rm = TRUE),
    outcome = case_match(
      candidate_party,
      "Democrat" ~ "votes_governor_25_dem_precinct_total",
      "Republican" ~ "votes_governor_25_rep_precinct_total"
    ),
    .by = vote_mode
  )

models = expand_grid(
  method = c("lm", "xgboost"),
  uncertainty = "conformal",
  subset = c('vote_mode == "Election Day"', 'vote_mode == "Early Voting"', 'vote_mode == "Absentee/Mail"'),
  outcome = c("turnout", "votes_governor_25_dem_precinct_total", "votes_governor_25_rep_precinct_total"),
  c1 = list(NULL, covars)
) |>
  rowwise() |>
  mutate(
    c2 = case_when(
      outcome == "votes_governor_25_dem_precinct_total" ~ "votes_potus_24_dem",
      outcome == "votes_governor_25_rep_precinct_total" ~ "votes_potus_24_rep",
      outcome == "votes_governor_25_dem_share" ~ "votePct_potus_24_dem",
      outcome == "votes_governor_25_rep_share" ~ "votePct_potus_24_rep",
      outcome == "turnout" ~ "votes_precFinal_24",
      .default = NA_character_
    ),
    covars = list(c(c2, c1))
  ) |>
  expand_grid(paths = tests) |>
  select(-c1, -c2) |>
  mutate(
    out = pmap(
      list(paths, method, uncertainty, outcome, covars, subset),
      \(data, method, uncertainty, outcome, covars, subset) {
        run_models(
          data = data,
          state = "VA",
          county = NA,
          timestamp = tar_read(timestamp_VA_NA),
          history = tar_read(history_VA_NA),
          covars = covars,
          method = method,
          uncertainty = uncertainty,
          outcome = outcome,
          subset = subset,
          obs_cutoff = 0.001,
          weight_var = "turnout",
          residualize = TRUE
        )
      },
      .progress = TRUE
    )
  )

summaries = models |>
  mutate(
    out = map(out, "out")
  ) |>
  unnest(cols = out) |>
  summarize(
    across(matches("^(estimate|lower|upper)$"), sum),
    nobs = n(),
    .by = c(method, uncertainty, subset, outcome, covars, vote_mode, timestamp)
  )

summaries |>
  filter(lengths(covars) == 1, outcome != "turnout", method == "xgboost", uncertainty == "conformal", subset != '') |>
  ggplot(aes(x = timestamp, y = estimate, color = outcome)) +
  # geom_point() +
  # geom_line() +
  # geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 800), size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = outcome), alpha = 0.2, color = NA) +
  geom_hline(
    data = truth,
    aes(yintercept = truth, color = candidate_party),
    linetype = "dashed"
  ) +
  facet_wrap(~vote_mode) +
  labs(
    x = "Time (EST)",
    y = "Votes",
    color = ""
  ) +
  theme_bw(base_size = 14) +
  scale_y_continuous(
    labels = scales::label_comma(),
    n.breaks = 7
  ) +
  scale_x_datetime(
    date_breaks = "2 hour",
    labels = scales::label_date(format = "%m-%d \n %I:%M %p", tz = "US/Eastern")
  ) +
  scale_color_manual(
    values = c(
      "votes_governor_25_dem_precinct_total" = "#005599", 
      "votes_governor_25_dem_share" = "#005599",
      "votes_governor_25_rep_precinct_total" = "#ce0008", 
      "votes_governor_25_rep_share" = "#ce0008",
      "turnout" = "grey50"
    ),
    labels = c(
      "votes_governor_25_dem_precinct_total" = "Democrat", 
      "votes_governor_25_dem_share" = "Democrat", 
      "votes_governor_25_rep_precinct_total" = "Republican", 
      "votes_governor_25_rep_share" = "Republican", 
      "turnout" = "Turnout"
    )
  ) +
  scale_fill_manual(
    values = c(
      "votes_governor_25_dem_precinct_total" = "#005599", 
      "votes_governor_25_dem_share" = "#005599",
      "votes_governor_25_rep_precinct_total" = "#ce0008", 
      "votes_governor_25_rep_share" = "#ce0008",
      "turnout" = "grey50"
    ),
    labels = c(
      "votes_governor_25_dem_precinct_total" = "Democrat", 
      "votes_governor_25_dem_share" = "Democrat", 
      "votes_governor_25_rep_precinct_total" = "Republican", 
      "votes_governor_25_rep_share" = "Republican", 
      "turnout" = "Turnout"
    )
  )
