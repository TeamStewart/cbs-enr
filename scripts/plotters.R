# ============================
# File: Build Plots and Tables for Website
# ============================

plot_voteShare_byMode <- function(summaries, uncertainty = FALSE) {
  p = ggplot(summaries, aes(x = timestamp, y = estimate, color = outcome))

  if (uncertainty) {
    p = p +
      geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 500), size = 1)
  } else {
    p = p + geom_point() + geom_line()
  }

  p = p +
    facet_wrap(~vote_mode) +
    labs(
      x = "Time (EST)",
      y = "Votes",
      color = ""
    ) +
    theme_bw(base_size = 24) +
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

  return(p)
}

plot_voteShare <- function(summaries, uncertainty = FALSE) {
  p = summaries |>
    summarize(
      across(matches("^(estimate|lower|upper)$"), sum),
      .by = c(outcome, timestamp)
    ) |>
    ggplot(aes(x = timestamp, y = estimate, color = outcome))

  if (uncertainty) {
    p = p +
      geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 500), size = 1)
  } else {
    p = p + geom_point() + geom_line()
  }

  p = p +
    labs(
      x = "Time (EST)",
      y = "Votes",
      color = ""
    ) +
    theme_bw(base_size = 24) +
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

  return(p)
}

make_tbl_countyMode <- function(summaries, state) {
  t = summaries |>
    mutate(
      timestamp = ymd_hms(timestamp) |> with_tz("US/Eastern"),
      tmp = swing_estimate,
      across(c(contains("swing"), contains("Share")), ~ scales::label_percent(accuracy = 0.01)(.x)),
      across(votes_total_20:repVotes_upper, ~ round(.x, digits = 0) |> format(big.mark = ",") |> as.character() |> str_squish())
    ) |>
    mutate(
      votes_total_24 = case_when(
        votes_total_24_estimate == votes_total_24_lower & votes_total_24_estimate == votes_total_24_upper ~ votes_total_24_estimate,
        .default = glue("{votes_total_24_estimate} <br> [{votes_total_24_lower} to {votes_total_24_upper}]")
      ),
      votes_dem_24 = case_when(
        demVotes_estimate == demVotes_lower & demVotes_estimate == demVotes_upper ~ demVotes_estimate,
        .default = glue("{demVotes_estimate} <br> [{demVotes_lower} to {demVotes_upper}]")
      ),
      votes_rep_24 = case_when(
        repVotes_estimate == repVotes_lower & repVotes_estimate == repVotes_upper ~ repVotes_estimate,
        .default = glue("{repVotes_estimate} <br> [{repVotes_lower} to {repVotes_upper}]")
      ),
      swing_24 = case_when(
        is.na(swing_estimate) | vote_mode == "Provisional" ~ NA,
        swing_estimate == swing_lower & swing_estimate == swing_upper ~ swing_estimate,
        .default = glue("{swing_estimate} <br> [{swing_lower} to {swing_upper}]")
      )
    ) |>
    select(vote_mode, timestamp, votes_total_20, votes_total_24:swing_24, tmp) |>
    drop_na(timestamp) |>
    gt() |>
    cols_hide(tmp) |>
    fmt_datetime(columns = timestamp, date_style = "MMMd", time_style = "h_m_s_p") |>
    sub_missing() |>
    fmt_markdown(columns = votes_total_24:swing_24) |>
    data_color(
      columns = tmp,
      target_columns = swing_24,
      palette = c("#ce0008", "white", "#005599"),
      domain = c(-0.25, 0.25),
      na_color = "white"
    ) |>
    cols_width(
      timestamp ~ px(185)
    ) |>
    tab_style_body(
      columns = timestamp:swing_24,
      style = cell_text(font = "Hack"),
      fn = \(x) TRUE
    ) |>
    cols_align(align = "center", columns = votes_total_20:swing_24) |>
    cols_label(
      # county = "County",
      vote_mode = "Vote Mode",
      timestamp = "Last Reported",
      votes_total_20 = "2020 Votes",
      votes_total_24 = "Votes",
      votes_dem_24 = "Dem Votes",
      votes_rep_24 = "Rep Votes",
      swing_24 = "Change in Margin vs 2020"
    )

  return(t)
}

make_tbl_county <- function(m, state, county) {
  t = m$summaries_byCounty |>
    mutate(
      timestamp = ymd_hms(timestamp) |> with_tz("US/Eastern"),
      tmp = swing_estimate,
      county = str_to_title(county),
      across(c(contains("swing"), contains("Share")), ~ scales::label_percent(accuracy = 0.01)(.x)),
      across(votes_total_20:repVotes_upper, ~ round(.x, digits = 0) |> format(big.mark = ",") |> as.character() |> str_squish())
    ) |>
    mutate(
      votes_total_24 = case_when(
        votes_total_24_estimate == votes_total_24_lower & votes_total_24_estimate == votes_total_24_upper ~ votes_total_24_estimate,
        .default = glue("{votes_total_24_estimate} <br> [{votes_total_24_lower} to {votes_total_24_upper}]")
      ),
      votes_dem_24 = case_when(
        demVotes_estimate == demVotes_lower & demVotes_estimate == demVotes_upper ~ demVotes_estimate,
        .default = glue("{demVotes_estimate} <br> [{demVotes_lower} to {demVotes_upper}]")
      ),
      votes_rep_24 = case_when(
        repVotes_estimate == repVotes_lower & repVotes_estimate == repVotes_upper ~ repVotes_estimate,
        .default = glue("{repVotes_estimate} <br> [{repVotes_lower} to {repVotes_upper}]")
      ),
      swing_24 = case_when(
        is.na(swing_estimate) ~ NA,
        swing_estimate == swing_lower & swing_estimate == swing_upper ~ swing_estimate,
        .default = glue("{swing_estimate} <br> [{swing_lower} to {swing_upper}]")
      )
    ) |>
    select(county, timestamp, votes_total_20, votes_total_24:swing_24, tmp) |>
    gt() |>
    cols_hide(tmp) |>
    fmt_datetime(columns = timestamp, date_style = "MMMd", time_style = "h_m_s_p") |>
    sub_missing() |>
    fmt_markdown(columns = votes_total_24:swing_24) |>
    data_color(
      columns = tmp,
      target_columns = swing_24,
      palette = c("#ce0008", "white", "#005599"),
      domain = c(-0.25, 0.25),
      na_color = "white"
    ) |>
    cols_width(
      timestamp ~ px(185)
    ) |>
    tab_style_body(
      columns = timestamp:swing_24,
      style = cell_text(font = "Hack"),
      fn = \(x) TRUE
    ) |>
    cols_align(align = "center", columns = votes_total_20:swing_24) |>
    cols_label(
      county = "County",
      timestamp = "Last Reported",
      votes_total_20 = "2020 Votes",
      votes_total_24 = "Votes",
      votes_dem_24 = "Dem Votes",
      votes_rep_24 = "Rep Votes",
      swing_24 = "Change in Margin vs 2020"
    )

  return(t)
}

pmargins_hist <- function(merged, x) {
  merged |>
    ggplot(
      aes(x = ({{ x }}))
    ) +
    annotate(
      "rect",
      ymin = -Inf,
      xmin = 0,
      xmax = Inf,
      ymax = Inf,
      fill = "#005599",
      alpha = 0.4,
      color = NA
    ) +
    annotate(
      "rect",
      ymin = -Inf,
      xmin = -Inf,
      xmax = 0,
      ymax = Inf,
      fill = "#ce0008",
      alpha = 0.4,
      color = NA
    ) +
    geom_histogram(bins = 30) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~vote_mode, nrow = 1) +
    scale_x_continuous(
      n.breaks = 5,
      labels = scales::label_percent(accuracy = 1, suffix = "pp", style_positive = "plus", )
    ) +
    theme_bw(base_size = 24) +
    labs(
      y = "Number of Precincts"
    )
}

pmargins_scatter <- function(merged, x, y) {
  merged |>
    ggplot(
      aes(x = ({{ x }}), y = ({{ y }}))
    ) +
    annotate(
      "polygon",
      x = c(-Inf, Inf, Inf),
      y = c(-Inf, Inf, -Inf),
      fill = "#005599",
      alpha = 0.4,
      color = NA
    ) +
    annotate(
      "polygon",
      x = c(-Inf, Inf, -Inf),
      y = c(-Inf, Inf, Inf),
      fill = "#ce0008",
      alpha = 0.4,
      color = NA
    ) +
    geom_point(alpha = 0.3, size = 0.7) +
    geom_abline(linetype = "dashed", color = "red", slope = 1, intercept = 0) +
    facet_wrap(~vote_mode, nrow = 1) +
    scale_x_continuous(
      n.breaks = 5
    ) +
    theme_bw(base_size = 24) +
    theme(
      panel.spacing = unit(1, "lines")
    ) +
    tune::coord_obs_pred()
}
