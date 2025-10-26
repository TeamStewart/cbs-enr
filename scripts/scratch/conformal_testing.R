tar_load_globals()

# covars = c("vote_mode", 'age1', 'age2', 'age3', 'white', 'black', 'col')
covars = c("vote_mode")

tests = list.files(
  "~/Dropbox (MIT)/Research/CBS-MIT Election Data/25_general/input_data/VA/test_files",
  pattern = "VA_.*?$",
  full.names = TRUE
)

# data = tests[2]; state = "VA"; county = NA; timestamp = tar_read(timestamp_VA_NA); history = tar_read(history_VA_NA); covars = covars; method = "lm"; uncertainty = ""

models = tribble(
  ~ method, ~uncertainty, ~outcome, ~covars,
  "lm", "", "turnout", c("votes_precFinal_24", covars),
  "lm", "conformal", "turnout", c("votes_precFinal_24", covars),
  "quantreg", "conformal", "turnout", c("votes_precFinal_24", covars),
  "lm", "", "votes_Governor_25_dem", c("votes_potus_24_dem", covars),
  "lm", "conformal", "votes_Governor_25_dem", c("votes_potus_24_dem", covars),
  "quantreg", "conformal", "votes_Governor_25_dem", c("votes_potus_24_dem", covars),
  "lm", "", "votes_Governor_25_rep", c("votes_potus_24_rep", covars),
  "lm", "conformal", "votes_Governor_25_rep", c("votes_potus_24_rep", covars),
  "quantreg", "conformal", "votes_Governor_25_rep", c("votes_potus_24_rep", covars)
) |> 
  expand_grid(
    paths = tests
  ) |> 
  mutate(
    out = pmap(
      list(paths, method, uncertainty, outcome, covars),
      \(data, method, uncertainty, outcome, covars) {
        run_models(
          data = data, state = "VA", county = NA, timestamp = tar_read(timestamp_VA_NA), 
          history = tar_read(history_VA_NA), 
          covars = covars, 
          method = method,
          uncertainty = uncertainty,
          outcome = outcome,
          weight_var = "reg24"
        )
      }
    )
  )

outs = tibble(
  path = tests,
  out = map(path, \(x) {
    run_models(
      data = x, state = "VA", county = NA, timestamp = tar_read(timestamp_VA_NA), 
      history = tar_read(history_VA_NA), 
      covars = covars, 
      method = "lm",
      uncertainty = "",
      outcome = "turnout",
      weight_var = "reg24"
    )
  })
)

summaries = models |> 
  select(-paths) |> 
  unnest(cols = out) |> 
  summarize(
    across(matches("^(estimate|lower|upper)$"), sum),
    .by = c(method, uncertainty, outcome, timestamp)
  )

summaries |>
  filter(outcome != "turnout") |> 
  ggplot(aes(x = timestamp, y = estimate, color = outcome, shape = method)) + 
  geom_point(position = position_dodge(width=0.1)) +
  geom_line(aes(linetype = method)) +
  # geom_pointrange(
  #   aes(y = estimate, ymin = lower, ymax = upper, color = "Dem")
  # ) +
  geom_hline(yintercept = 1928516, linetype = "dashed", color = "#3791FF") +
  geom_hline(yintercept = 1251256, linetype = "dashed", color = "#F6573E") +
  # geom_hline(yintercept = 3179772, linetype = "dashed", color = "black") +
  labs(
    x = "Time (EST)",
    y = "Votes",
    color = "Party",
    shape = "Method",
    linetype = "Method"
  ) +
  theme_bw() +
  scale_y_continuous(
    labels = scales::label_comma(),
    n.breaks = 7
  ) +
  scale_x_datetime(
    date_breaks = "2 hour",
    labels = scales::label_date(format = "%m-%d \n %I:%M %p", tz = "US/Eastern")
  ) +
  scale_color_manual(
    values = c("votes_Governor_25_dem" = "#3791FF", "votes_Governor_25_rep" = "#F6573E"),
    labels = c("votes_Governor_25_dem" = "Democrat", "votes_Governor_25_rep" = "Republican")
  ) +
  scale_shape_manual(
    values = c("lm" = 16, "quantreg" = 17),
    labels = c("lm" = "Linear Model", "quantreg" = "Quantile Regression")
  ) +
  scale_linetype_manual(
    values = c("lm" = "solid", "quantreg" = "dashed"),
    labels = c("lm" = "Linear Model", "quantreg" = "Quantile Regression")
  )

summaries |>
  filter(outcome == "turnout") |> 
  ggplot(aes(x = timestamp, y = estimate, shape = method)) + 
  geom_point(position = position_dodge(width=0.1)) +
  geom_line(aes(linetype = method)) +
  # geom_hline(yintercept = 1928516, linetype = "dashed", color = "#3791FF") +
  # geom_hline(yintercept = 1251256, linetype = "dashed", color = "#F6573E") +
  geom_hline(yintercept = 3179772, linetype = "dashed", color = "black") +
  labs(
    x = "Time (EST)",
    y = "Votes",
    color = "Party",
    shape = "Method",
    linetype = "Method"
  ) +
  theme_bw() +
  scale_y_continuous(
    labels = scales::label_comma(),
    n.breaks = 7
  ) +
  scale_x_datetime(
    date_breaks = "2 hour",
    labels = scales::label_date(format = "%m-%d \n %I:%M %p", tz = "US/Eastern")
  ) +
  # scale_color_manual(
  #   values = c("votes_Governor_25_dem" = "#3791FF", "votes_Governor_25_rep" = "#F6573E"),
  #   labels = c("votes_Governor_25_dem" = "Democrat", "votes_Governor_25_rep" = "Republican")
  # ) +
  scale_shape_manual(
    values = c("lm" = 16, "quantreg" = 17),
    labels = c("lm" = "Linear Model", "quantreg" = "Quantile Regression")
  ) +
  scale_linetype_manual(
    values = c("lm" = "solid", "quantreg" = "dashed"),
    labels = c("lm" = "Linear Model", "quantreg" = "Quantile Regression")
  )

summaries |>
  filter(outcome != "turnout", method == "lm", uncertainty == "conformal") |> 
  ggplot(aes(x = timestamp, y = estimate, color = outcome)) + 
  # geom_point(position = position_dodge(width=0.1)) +
  # geom_line(aes(linetype = method)) +
  geom_pointrange(
    aes(y = estimate, ymin = lower, ymax = upper),
    position = position_dodge(width=1000)
  ) +
  geom_hline(yintercept = 1928516, linetype = "dashed", color = "#3791FF") +
  geom_hline(yintercept = 1251256, linetype = "dashed", color = "#F6573E") +
  # geom_hline(yintercept = 3179772, linetype = "dashed", color = "black") +
  labs(
    x = "Time (EST)",
    y = "Votes",
    color = "Party",
    shape = "Method",
    linetype = "Method"
  ) +
  theme_bw() +
  scale_y_continuous(
    labels = scales::label_comma(),
    n.breaks = 7
  ) +
  scale_x_datetime(
    date_breaks = "2 hour",
    labels = scales::label_date(format = "%m-%d \n %I:%M %p", tz = "US/Eastern")
  ) +
  scale_color_manual(
    values = c("votes_Governor_25_dem" = "#3791FF", "votes_Governor_25_rep" = "#F6573E"),
    labels = c("votes_Governor_25_dem" = "Democrat", "votes_Governor_25_rep" = "Republican")
  )
