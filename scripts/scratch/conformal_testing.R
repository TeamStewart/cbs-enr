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

# models with `vote_mode` separate outcomes
models = expand_grid(
  method = "lm",
  uncertainty = c("", "conformal"),
  subset = c('vote_mode == "Early Voting"', 'vote_mode == "Absentee/Mail"', 'vote_mode == "Election Day"', ''),
  outcome = expand_grid(party = c("dem", "rep")) |>
    mutate(
      o = glue({
        "votes_governor_25_{party}"
      })
    ) |>
    pull(o),
  covariates = list(NULL, c(covars))
) |>
  rowwise() |>
  mutate(
    covar = str_replace(outcome, "governor_25", "potus_24"),
    covars = list(c(covar, covariates))
  ) |>
  expand_grid(
    paths = tests
  ) |>
  select(-covar, -covariates) |>
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
          obs_cutoff = 0.1,
          weight_var = "reg24"
        )
      }
    )
  )

summaries = models |>
  mutate(
    out = map(out, "out")
  ) |>
  unnest(cols = out) |>
  summarize(
    across(matches("^(estimate|lower|upper)$"), sum),
    .by = c(method, uncertainty, subset, outcome, vote_mode, covars, timestamp)
  )

make_plot_voteShare_byMode <- function(d) {
  d |>
    filter(outcome != "turnout", uncertainty != "conformal") |>
    mutate(
      co = ifelse(lengths(covars) > 1, "with covariates", "without covariates")
    ) |>
    ggplot(aes(x = timestamp, y = estimate, color = outcome, shape = co)) +
    geom_point(position = position_dodge(width = 0.1)) +
    geom_line() +
    geom_hline(
      data = truth,
      aes(yintercept = truth, color = outcome),
      linetype = "dashed"
    ) +
    facet_wrap(~vote_mode) +
    # geom_hline(yintercept = 1928516, linetype = "dashed", color = "#3791FF") +
    # geom_hline(yintercept = 1251256, linetype = "dashed", color = "#F6573E") +
    # geom_hline(yintercept = 3179772, linetype = "dashed", color = "black") +
    labs(
      x = "Time (EST)",
      y = "Votes",
      color = "Party",
      shape = ""
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
      values = c("votes_governor_25_dem" = "#3791FF", "votes_governor_25_rep" = "#F6573E"),
      labels = c("votes_governor_25_dem" = "Democrat", "votes_governor_25_rep" = "Republican")
    )
}

make_plot_voteShare_byMode(filter(summaries, subset == '')) # combined `vote_mode`
make_plot_voteShare_byMode(filter(summaries, subset != '')) # separate `vote_mode`

# total vote share plots, summed across modes
summaries |>
  filter(outcome != "turnout", uncertainty != "conformal", lengths(covars) == 1, subset != '') |>
  summarize(
    across(matches("^(estimate|lower|upper)$"), sum),
    .by = c(outcome, timestamp)
  ) |>
  ggplot(aes(x = timestamp, y = estimate, color = outcome)) +
  geom_point(position = position_dodge(width = 0.1)) +
  geom_line() +
  geom_hline(
    data = summarize(truth, truth = sum(truth), .by = outcome),
    aes(yintercept = truth, color = outcome),
    linetype = "dashed"
  ) +
  labs(
    x = "Time (EST)",
    y = "Votes",
    color = "Party"
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
    values = c("votes_governor_25_dem" = "#3791FF", "votes_governor_25_rep" = "#F6573E"),
    labels = c("votes_governor_25_dem" = "Democrat", "votes_governor_25_rep" = "Republican")
  )

# plots with intervals
summaries |>
  filter(outcome != "turnout", uncertainty == "conformal", lengths(covars) == 1, method == "lm", subset != '') |>
  ggplot(aes(x = timestamp, y = estimate, color = outcome)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 500)) +
  # geom_point(position = position_dodge(width = 0.1)) +
  # geom_line() +
  geom_hline(
    data = truth,
    aes(yintercept = truth, color = outcome),
    linetype = "dashed"
  ) +
  facet_wrap(~vote_mode) +
  labs(
    x = "Time (EST)",
    y = "Votes",
    color = "Party"
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
    values = c("votes_governor_25_dem" = "#3791FF", "votes_governor_25_rep" = "#F6573E"),
    labels = c("votes_governor_25_dem" = "Democrat", "votes_governor_25_rep" = "Republican")
  )
