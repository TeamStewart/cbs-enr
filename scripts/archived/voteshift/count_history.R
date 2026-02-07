rm(list=ls())
gc()

library(tidyverse)
# library(lme4)
# library(marginaleffects)
# library(modelsummary)
# library(brms)
# library(ggdist)

source("~/Dropbox (MIT)/Research/medsl_theme.R")

hist_20 = read_csv("data/voteshift/voteshift_2020_byCounty_raw.csv") |>
  select(state, county, timestamp, pct_forCand_atTimestamp, pct_atTimestamp) |>
  mutate(
    last_drop = max(timestamp) == timestamp,
    pct_final = max(pct_forCand_atTimestamp*last_drop),
    pct_forCand_atTimestamp = max(pct_forCand_atTimestamp*last_drop) - pct_forCand_atTimestamp,
    .by = c(state, county)
  ) |>
  mutate(
    year = "2020"
  )

hist_22 = read_csv("data/voteshift/voteshift_2022_byCounty_raw.csv") |>
  select(state, county, office, candidate, timestamp, pct_forCand_atTimestamp, pct_atTimestamp) |>
  mutate(
    last_drop = max(timestamp) == timestamp,
    pct_final = max(pct_forCand_atTimestamp*last_drop),
    pct_forCand_atTimestamp = max(pct_forCand_atTimestamp*last_drop) - pct_forCand_atTimestamp,
    .by = c(state, county, office, candidate)
  ) |>
  mutate(
    year = "2022"
  )

hist = bind_rows(hist_20, hist_22)

###################################
## Model Experiments
###################################

mod_22_lmer = lmer(
  pct_forCand_atTimestamp ~ 1 + (pct_atTimestamp | state/county),
  data = hist_22
)

mod_yearFE = brm(
  # pct_forCand_atTimestamp ~ 1 + (pct_atTimestamp | state/county),
  pct_forCand_atTimestamp ~ 1 + year + (pct_atTimestamp | state/county),
  cores = 4,
  file = "mod_yearFE",
  file_refit = "on_change",
  backend = "cmdstanr",
  data = hist
)

mod_yearFE = readRDS("data/voteshift/mod_yearFE.rds")

modelsummary(mod_22, mc.cores = 4)

mod_20 = brm(
  pct_forCand_atTimestamp ~ s(pct_atTimestamp) + (1 | state/county),
  cores = 2,
  backend = "cmdstanr",
  data = hist_20
)

predictions(
  mod_yearFE,
  newdata = datagrid(pct_atTimestamp = seq(0, 1, by = 0.1), state = distinct(hist_22, state) |> pull(), year = "2022"),
  allow_new_levels = TRUE
) |>
  posterior_draws() |>
  ggplot(aes(x = pct_atTimestamp, y = draw)) +
  facet_wrap(~ state, ncol = 2) +
  stat_lineribbon() +
  scale_fill_brewer() +
  labs(
    x = "% of Total Votes Reported",
    y = "Vote Shift (predicted)",
    fill = "",
    title = "Vote Shift (towards Democrats)"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_medsl() +
  scale_color_medsl() +
  theme(
    plot.margin = unit(rep(0.5, 4), "cm"),
    axis.title.y = element_text(margin = margin(r = 0)),
    axis.title.x = element_text(margin = margin(t = 0)),
    plot.title = element_text(margin = margin(b = 0)),
    panel.spacing.x = unit(5, "mm")
  )

ggsave("~/Downloads/voteshift_yearFE.jpg", width = 8, height = 24, units = "in")

p2 = predictions(mod_20,
  newdata = datagrid(pct_atTimestamp = seq(0, 1, by = 0.1), state = unique), allow_new_levels = TRUE) |>
  posterior_draws() |>
  ggplot(aes(x = pct_atTimestamp, y = draw)) +
  facet_wrap(~ state) +
  stat_lineribbon() +
  scale_fill_brewer() +
  labs(
    x = "% of Total Votes Reported",
    y = "Vote Shift (predicted)",
    fill = "",
    title = "Vote Shift (towards Democrats) for the 2020 Elections"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw()

ggsave("~/Downloads/voteshift_2020_model.png", p2, width = 12, units = "in")

###################################
## Plot Shift by County
###################################

plot_countyPace_shift <- function(st){

  print(st)

  p1 = hist |>
    filter(state == st) |>
    mutate(pct_forCand_atTimestamp = case_when(
      pct_forCand_atTimestamp < -0.15 ~ -0.15,
      pct_forCand_atTimestamp > 0.15 ~ 0.15,
      .default = pct_forCand_atTimestamp
    )) |> 
    ggplot(aes(x = pct_atTimestamp, y = pct_forCand_atTimestamp, color = year, group = year)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    scale_y_continuous(
      limits = c(-0.15, 0.15), 
      minor_breaks = seq(-0.15, 0.15, by = 0.01),
      breaks = c(-0.15, -0.10, -0.05, 0, 0.05, 0.1, 0.15)) +
    facet_wrap(~ county) +
    labs(x = "% In", y = "Dem Vote Shift (pp)", color = "Election", title = str_c("Dem Vote Shift in ", st)) +
    theme_medsl() +
    scale_color_medsl() +
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"),
      axis.title.y = element_text(margin = margin(r = 0)),
      axis.title.x = element_text(margin = margin(t = 0)),
      plot.title = element_text(margin = margin(b = 0)),
      panel.grid.minor = element_line(color = "#e0e0e0"),
      panel.spacing.x = unit(5, "mm")
    )
  
  # return(p1)

  ggsave(sprintf("figs/voteshift/shift_byCounty_%s.jpg", st), width = 16, height = 12, units = "in")

}

plot_countyPace_share <- function(st){

  print(st)

  p1 = hist |>
    filter(state == st) |>
    ggplot(aes(x = pct_atTimestamp, y = pct_forCand_atTimestamp, color = year, group = year)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ylim(c(0, 1)) +
    scale_x_continuous(labels = scales::label_percent(), limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    facet_wrap(~ county) +
    labs(x = "% In", y = "Dem Vote Share", color = "Election", title = str_c("Dem Vote Share in ", st)) +
    theme_medsl() +
    scale_color_medsl() +
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"),
      axis.title.y = element_text(margin = margin(r = 0)),
      axis.title.x = element_text(margin = margin(t = 0)),
      plot.title = element_text(margin = margin(b = 0)),
      panel.spacing.x = unit(5, "mm")
    )

  ggsave(sprintf("~/Downloads/cbs_figs/share_byCounty_%s.jpg", st), width = 16, height = 12, units = "in")

}

distinct(hist, state) |> pull() |> walk(plot_countyPace_shift)
distinct(hist, state) |> pull() |> walk(plot_countyPace_share)

plot_countyPace_shift("Maryland")
