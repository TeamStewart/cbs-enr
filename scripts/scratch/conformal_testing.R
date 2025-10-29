tar_load_globals()

covars = c('age1', 'age2', 'age3', 'white', 'black', 'col')
# covars = c("vote_mode")

# some testing data to quickly load if needed
# data = tests[2]; state = "VA"; county = NA; timestamp = tar_read(timestamp_VA_NA); history = tar_read(history_VA_NA); covars = covars; method = "lm"; uncertainty = ""

tests = list.files(
  "~/Dropbox (MIT)/Research/CBS-MIT Election Data/25_general/input_data/VA/test_files",
  pattern = "VA_.*?$",
  full.names = TRUE
)

truth = read_csv(tests[length(tests)]) |>
  summarize(
    truth = sum(precinct_total, na.rm = TRUE),
    .by = c(candidate_party, vote_mode)
  ) |>
  filter(
    candidate_party %in% c("Democrat", "Republican"),
    vote_mode %in% c("Early Voting", "Absentee/Mail", "Election Day")
  ) |>
  mutate(
    outcome = case_match(
      candidate_party,
      "Democrat" ~ "votes_governor_25_dem",
      "Republican" ~ "votes_governor_25_rep"
    )
  )

models = expand_grid(
  method = "quantreg",
  uncertainty = "conformal",
  subset = c(''),
  outcome = c("turnout", "votes_governor_25_dem", "votes_governor_25_rep"),
  c1 = list(NULL, c(covars))
) |>
  rowwise() |>
  mutate(
    c2 = case_when(
      outcome == "votes_governor_25_dem" ~ "votes_potus_24_dem",
      outcome == "votes_governor_25_rep" ~ "votes_potus_24_rep",
      outcome == "turnout" ~ "votes_precFinal_21",
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
          obs_cutoff = 0.25,
          weight_var = "reg24"
        )
      }
    )
  )

t = models |>
  mutate(
    train = map(out, "train"),
    valid = map(out, "valid"),
    test = map(out, "test"),
    out = map(out, "out")
  ) |>
  filter(
    outcome == "votes_governor_25_dem",
    paths == tests[5],
    subset == "vote_mode == \"Early Voting\"",
    uncertainty == "",
    lengths(covars) == 1
  )

t |> pluck("train", 1, "votes_governor_25_dem") |> sum()
t |> pluck("test", 1, "votes_governor_25_dem") |> sum()
t |> pluck("valid", 1, "votes_governor_25_dem") |> sum()
t |> pluck("out", 1, "estimate") |> sum()

summaries = models |>
  mutate(
    out = map(out, "out")
  ) |>
  unnest(cols = out) |>
  summarize(
    across(matches("^(estimate|lower|upper)$"), sum),
    .by = c(method, uncertainty, subset, outcome, covars, vote_mode, timestamp)
  )


summaries |>
  filter(lengths(covars) == 1, outcome != "turnout", method == "lm") |>
  ggplot(aes(x = timestamp, y = estimate, color = outcome, shape = method)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 800), size = 1) +
  geom_segment(aes(y = lower, yend = upper, x = timestamp), data = out, color = "black", inherit.aes = FALSE) +
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
    values = c("votes_governor_25_dem" = "#005599", "votes_governor_25_rep" = "#ce0008", "turnout" = "grey50"),
    labels = c("votes_governor_25_dem" = "Democrat", "votes_governor_25_rep" = "Republican", "turnout" = "Turnout")
  )


### CQR testing

data_VA = merge_data(
  # data = tar_read(data_VA_NA),
  data = read_csv(tests[3]),
  history = tar_read(history_VA_NA),
  office = c("Governor", "Attorney General", "Lt Governor"),
  impute = FALSE
) |>
  add_obs("votes_governor_25_dem", "votes_potus_24_dem") |> 
  drop_na(votes_governor_25_dem, votes_potus_24_dem)

data_VA = add_resid(data_VA, votes_governor_25_dem, votes_potus_24_dem)

train = filter(data_VA, obs > 0.25)
test = filter(data_VA, obs <= 0.25)

fit = enightmodels::forecast_prec(
  data = data_VA,
  cands = c(votes_governor_25_dem, votes_governor_25_rep),
  turn = turnout,
  obs = obs < 0.25,
  ctrl_form = ~ log_s(votes_potus_24_dem) + log_s(white) + log_s(col),
  prior_turn = votes_precFinal_21,
  prior_n_turn = 1,
  prior_n_cands = 1,
  prior_cands = c(votes_potus_24_dem, votes_potus_24_rep)
  # weights = data_VA$reg24
)

forecast_summarize(fit)

fit = enightmodels::lm_bayes(
  resid ~ votes_potus_24_dem + age1 + age2 + age3 + white + black + col,
  data = train, 
  weights = train$reg24
)

preds = predict(fit, newdata = test)

out = bind_rows(
  train, 
  test |> mutate(
    estimate = preds[, "estimate"] * votes_potus_24_dem + votes_potus_24_dem,
    lower = preds$lower * votes_potus_24_dem + votes_potus_24_dem,
    upper = preds$upper * votes_potus_24_dem + votes_potus_24_dem
  )
) |> 
  mutate(
    estimate = coalesce(estimate, votes_governor_25_dem),
    lower = coalesce(lower, votes_governor_25_dem),
    upper = coalesce(upper, votes_governor_25_dem)
  ) |> 
    summarize(
      across(c(estimate, lower, upper), sum),
      .by = c(timestamp, vote_mode)
    )
