# ============================
# File: Build Plots and Tables for Website
# ============================

get_modelHist <- function(state, county){
  
  read_csv(
    glue("{PATH_DROPBOX}/24_general/{state}/{state}_{county}_modeling.csv"), 
    locale = locale(tz="US/Eastern")
  )
  
}

make_plot_voteShare <- function(m, state, county){
  
  models_cumulative = get_modelHist(state, county) |> 
    summarise(
      confidence = all(confidence),
      across(c(contains("swing"), contains("Share")), ~ weighted.mean(.x, votes_total_20, na.rm = TRUE)),
      across(votes_total_20:repVotes_upper, sum),
      .by = c(state, timestamp)
    ) |> 
    mutate(
      across(where(is.double) & !timestamp, ~ na_if(.x, Inf))
    )
  
  p = models_cumulative |> 
    ggplot(aes(x = timestamp)) +
    geom_pointrange(
      aes(y = demShare_estimate, ymin = demShare_lower, ymax = demShare_upper, color = "Dem"), 
      fatten = 1,
      alpha = 0.7
    ) +
    geom_pointrange(
      aes(y = repShare_estimate, ymin = repShare_lower, ymax = repShare_upper, color = "Rep"), 
      fatten = 1,
      alpha = 0.7
    ) +
    geom_pointrange(
      data = filter(models_cumulative, !confidence),
      aes(y = repShare_estimate, ymin = repShare_lower, ymax = repShare_upper, x=timestamp),
      color = "grey",
      fatten = 1
    ) +
    labs(
      x = "Time (EST)",
      y = "Vote Share",
      color = ""
    ) +
    theme_bw() +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1.1),
      n.breaks = 7
    ) + 
    scale_x_datetime(
      date_breaks = "2 hour",
      labels = scales::label_date(format = "%m-%d \n %I:%M %p", tz = "US/Eastern")
    ) +
    scale_color_manual(values = c("Dem" = "#3791FF", "Rep" = "#F6573E")) +
    geom_hline(yintercept = 0.5, linetype = "dashed")
  
  return(p)
  
}

make_plot_margin2020 <- function(m, state, county){
  
  models_cumulative = get_modelHist(state, county) |> 
    summarise(
      confidence = all(confidence),
      across(c(contains("swing"), contains("Share")), ~ weighted.mean(.x, votes_total_20, na.rm = TRUE)),
      across(votes_total_20:repVotes_upper, sum),
      .by = c(state, timestamp)
    ) |> 
    mutate(
      across(where(is.double) & !timestamp, ~ na_if(.x, Inf))
    )
  
  p = models_cumulative |> 
    ggplot() +
    annotate(
      "rect",
      ymin=0, xmin = as.POSIXct(-Inf, origin = '2014-10-15'), xmax=as.POSIXct(Inf, origin = '2014-10-15'), ymax = Inf,
      fill = "#3791FF", alpha = 0.4, color = NA
    ) +
    annotate(
      "rect",
      ymin=-Inf, xmin = as.POSIXct(-Inf, origin = '2014-10-15'), xmax=as.POSIXct(Inf, origin = '2014-10-15'), ymax = 0,
      fill = "#F6573E", alpha = 0.4, color = NA
    ) +
    geom_pointrange(
      aes(x = timestamp, y = swing_estimate, ymin = swing_lower, ymax = swing_upper), 
      fatten = 1
    ) +
    geom_pointrange(
      data = filter(models_cumulative, !confidence),
      aes(y = repShare_estimate, ymin = repShare_lower, ymax = repShare_upper, x=timestamp),
      color = "grey",
      fatten = 1
    ) +
    labs(
      x = "Time (EST)",
      y = "Margin vs 2020",
    ) +
    theme_bw() +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1.1),
      n.breaks = 7
    ) +
    scale_x_datetime(
      date_breaks = "2 hour",
      labels = scales::label_date(format = "%m-%d \n %I:%M %p", tz = "US/Eastern")
    ) +
    geom_hline(yintercept = 0, linetype = "dashed")
  
  return(p)
  
}

make_plot_votesEDay <- function(m, state, county){
  
  p = get_modelHist(state, county) |> 
    filter(vote_mode == "Election Day") |> 
    summarize(
      across(starts_with("votes_total_24"), sum),
      confidence = all(confidence),
      .by = c(state, timestamp)
    ) |> 
    filter(
      votes_total_24_lower > 0 & votes_total_24_estimate > 0, votes_total_24_upper > 0
    ) |> 
    mutate(
      across(where(is.double) & !timestamp, ~ na_if(.x, Inf))
    ) |> 
    ggplot() +
    geom_pointrange(
      aes(x = timestamp, y = votes_total_24_estimate, ymin = votes_total_24_lower, ymax = votes_total_24_upper, color = confidence), 
      fatten = 1
    ) +
    labs(
      x = "Time (EST)",
      y = "Projected Election Day Votes",
    ) +
    theme_bw() +
    guides(color = "none") +
    scale_y_continuous(
      labels = scales::label_comma()
    ) +
    scale_x_datetime(
      date_breaks = "2 hour",
      labels = scales::label_date(format = "%m-%d \n %I:%M %p", tz = "US/Eastern")
    ) +
    scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "black")) +
    geom_hline(yintercept = 0, linetype = "dashed")
  
  return(p)
  
}

make_plot_votesAll <- function(m, state, county){
  
  p = get_modelHist(state, county) |> 
    summarize(
      across(starts_with("votes_total_24"), sum),
      confidence = all(confidence),
      .by = c(state, timestamp)
    ) |> 
    mutate(
      across(where(is.double) & !timestamp, ~ na_if(.x, Inf))
    ) |> 
    filter(
      votes_total_24_lower > 0 & votes_total_24_estimate > 0, votes_total_24_upper > 0
    ) |> 
    ggplot() +
    geom_pointrange(
      aes(x = timestamp, y = votes_total_24_estimate, ymin = votes_total_24_lower, ymax = votes_total_24_upper, color = confidence), 
      fatten = 1
    ) +
    labs(
      x = "Time (EST)",
      y = "Projected Total Votes",
    ) +
    theme_bw() +
    guides(color = "none") +
    scale_y_continuous(
      labels = scales::label_comma()
    ) +
    scale_x_datetime(
      labels = scales::label_date(format = "%m-%d \n %I:%M %p", tz = "US/Eastern")
    ) +
    scale_color_manual(values = c("FALSE" = "grey90", "TRUE" = "black")) +
    geom_hline(yintercept = 0, linetype = "dashed")
  
  return(p)
  
}

make_tbl_countyMode <- function(m, state, county){
  
  t = m$summaries_byCounty_byMode |> 
    mutate(
      tmp = swing_estimate,
      county = str_to_title(county),
      across(c(contains("swing"), contains("Share")), ~ scales::label_percent(accuracy = 0.01)(.x)),
      across(votes_total_20:repVotes_upper, ~ round(.x, digits = 0) |> format(big.mark = ",") |> as.character() |> str_squish())
    ) |> 
    mutate(
      votes_total_24 = case_when(
        votes_total_24_estimate == votes_total_24_lower & votes_total_24_estimate == votes_total_24_upper ~  votes_total_24_estimate,
        .default = glue("{votes_total_24_estimate} <br> [{votes_total_24_lower} to {votes_total_24_upper}]")
      ),
      votes_dem_24 = case_when(
        demVotes_estimate == demVotes_lower & demVotes_estimate == demVotes_upper ~  demVotes_estimate,
        .default = glue("{demVotes_estimate} <br> [{demVotes_lower} to {demVotes_upper}]")
      ),
      votes_rep_24 = case_when(
        repVotes_estimate == repVotes_lower & repVotes_estimate == repVotes_upper ~  repVotes_estimate,
        .default = glue("{repVotes_estimate} <br> [{repVotes_lower} to {repVotes_upper}]")
      ),
      swing_24 = case_when(
        is.na(swing_estimate) | vote_mode == "Provisional" ~ NA,
        swing_estimate == swing_lower & swing_estimate == swing_upper ~ swing_estimate,
        .default = glue("{swing_estimate} <br> [{swing_lower} to {swing_upper}]")
      )
    ) |> 
    select(county, vote_mode, timestamp, votes_total_20, votes_total_24:swing_24, tmp) |> 
    gt() |> 
    cols_hide(tmp) |>
    fmt_datetime(columns = timestamp, date_style = "MMMd", time_style = "h_m_s_p") |> 
    sub_missing() |> 
    fmt_markdown(columns = votes_total_24:swing_24) |> 
    data_color(
      columns = tmp,
      target_columns = swing_24,
      palette = c("#F6573E", "white", "#3791FF"),
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

make_tbl_county <- function(m, state, county){
  
  t = m$summaries_byCounty |> 
    mutate(
      tmp = swing_estimate,
      county = str_to_title(county),
      across(c(contains("swing"), contains("Share")), ~ scales::label_percent(accuracy = 0.01)(.x)),
      across(votes_total_20:repVotes_upper, ~ round(.x, digits = 0) |> format(big.mark = ",") |> as.character() |> str_squish())
    ) |> 
    mutate(
      votes_total_24 = case_when(
        votes_total_24_estimate == votes_total_24_lower & votes_total_24_estimate == votes_total_24_upper ~  votes_total_24_estimate,
        .default = glue("{votes_total_24_estimate} <br> [{votes_total_24_lower} to {votes_total_24_upper}]")
      ),
      votes_dem_24 = case_when(
        demVotes_estimate == demVotes_lower & demVotes_estimate == demVotes_upper ~  demVotes_estimate,
        .default = glue("{demVotes_estimate} <br> [{demVotes_lower} to {demVotes_upper}]")
      ),
      votes_rep_24 = case_when(
        repVotes_estimate == repVotes_lower & repVotes_estimate == repVotes_upper ~  repVotes_estimate,
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
      palette = c("#F6573E", "white", "#3791FF"),
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