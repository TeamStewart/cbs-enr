source("scripts/scrapers.R")

process_data <- function(state, county, type, timestamp, path = NULL, success = NULL) {
  
  if (state == "NC"){
    d <- scrape_nc(state, county, type, path, timestamp)
  } else if (state == "TX"){
    d <- scrape_tx(state, county, type, path, timestamp)
  }
  
  # save latest version
  write_csv(d, sprintf("data/clean/%s/%s_%s_latest.csv", state, county, type))

  # save timestamped version
  write_csv(d, sprintf("data/clean/%s/%s_%s_%s.csv", state, county, type, timestamp))
  
  return(d)
}

get_timestamp <- function(state, county, type) {
  nc_timestamp <- function() {
    read_xml("https://s3.amazonaws.com/dl.ncsbe.gov?delimiter=/&prefix=ENRS/2024_03_05/") |>
      as_list() |>
      pluck("ListBucketResult", function(x) x[names(x) == "Contents"]) |>
      map(
        ~ tibble(
          Key = pluck(.x, "Key", 1),
          LastModified = pluck(.x, "LastModified", 1)
        )
      ) |>
      list_rbind() |>
      filter(Key == "ENRS/2024_03_05/results_pct_20240305.zip") |>
      pull(LastModified) |>
      ymd_hms(tz = "America/New_York") |>
      str_replace_all("-|:| ", "_")
  }
  
  if (state == "NC" & type == "primary"){
    return(nc_timestamp())
  } else if (state == "TX"){
    return(nc_timestamp())
  }
}

general_table <- function(data, state, county, type, timestamp) {
  
  if(county != "all"){
    locale <- str_c(str_to_title(county), ", ", str_to_upper(state))
  } else {
    locale <- str_to_upper(state)
  }
  
  data |>
    summarise(count = sum(precinct_total, na.rm = TRUE), .by = c(race_name, candidate_party, candidate_name)) |>
    arrange(race_name, candidate_party, desc(count)) |>
    mutate(percent = count / sum(count), .by = race_name) |>
    gt() |> 
    tab_header(
      title = sprintf("2024 %s Results in %s", str_to_title(type), locale),
      subtitle = str_c("Last updated: ", ymd_hms(timestamp, tz = "America/New_York"))
    ) |>
    opt_interactive(
      use_filters = TRUE,
      use_resizers = TRUE,
      use_compact_mode = TRUE,
      use_text_wrapping = TRUE,
      use_page_size_select = TRUE
    ) |>
    cols_label(
      race_name = "Contest",
      candidate_party = "Party",
      candidate_name = "Candidate",
      count = "Count",
      percent = "Vote %"
    ) |> 
    fmt_number(columns = count, decimals = 0, drop_trailing_zeros = TRUE) |> 
    fmt_percent(columns = percent) |> 
    tab_style(
      style = cell_text(
        v_align = "middle",
        align = "center"
      ),
      locations = cells_body(
        columns = race_name:candidate_name
      )
    ) |> 
    data_color(
      columns = count,
      target_columns = everything(),
      rows = candidate_party == "Democrat",
      palette = c("white", "#9FDDF3","#59CBF5", "#00BAFF", "#3791FF")
    ) |>
    data_color(
      columns = count,
      target_columns = everything(),
      rows = candidate_party == "Republican",
      palette = c("white", "#E8ADA4", "#F59181", "#FF715A", "#F6573E")
    ) |>
    # data_color(
    #   columns = count,
    #   target_columns = everything(),
    #   rows = !(candidate_party %in% c("Democrat", "Republican")),
    #   palette = c("white", "#d8d5ad", "#d0cc9c", "#c8c38a", "#c0ba79", "#b8b168", "#b0a856", "#a19a4c")
    # ) |>
    # data_color(
    #   columns = count,
    #   target_columns = everything(),
    #   rows = candidate_party == "Constitution",
    #   palette = c("white", "#d8d5ad", "#d0cc9c", "#c8c38a", "#c0ba79", "#b8b168", "#b0a856", "#a19a4c")
    # ) |>
    # data_color(
    #   columns = count,
    #   target_columns = everything(),
    #   rows = candidate_party == "Green",
    #   palette = c("white", "#e7f6ee", "#d0eede", "#b9e5ce", "#a2ddbd", "#8bd4ad", "#73cc9d", "#5cc38c", "#45bb7c", "#2eb26c", "#17aa5c")
    # ) |>
    tab_style(
      style = list(
        cell_text(align = "center", v_align = "middle")
      ),
      locations = cells_body()
    ) |>
    cols_align(align = "center")
}

download_file <- function(url, local_path){
  
  download.file(url, destfile = local_path)
  
  return(local_path)
  
}

convert_cbs <- function(data, state, county, type, timestamp, upload=FALSE){
  
  sf <- suppressMessages(stamp("2022-11-09T11:26:47Z"))
  
  if (state == "TX"){
    return(NULL)
  }
  
  lookup_geo <- read_csv("data/input/cbs_lookups/All States and Counties.csv",
                         skip = 1,
                         show_col_types = FALSE,
                         col_names = c("state_name", "postalCode", "st", "state_fips", "county_name", "cnty", "county_fips")) |> 
    filter(postalCode == state) |> 
    select(-state_fips) |> 
    mutate(county_name = str_to_upper(county_name))
  
  lookup_cands <- read_csv("data/input/cbs_lookups/primary_cands.csv", 
                           show_col_types = FALSE,
                           col_select = c(-candidate_lastname),
                           col_types = cols(.default = col_character()))
  
  formatted <- data |> 
    filter(str_detect(race_name, regex("^Governor|President", ignore_case=TRUE))) |> 
    filter(candidate_party %in% c("Democrat", "Republican")) |> 
    mutate(
      jCde = case_when(
        str_detect(race_name, "^US House") ~ str_extract(race_name, "\\d+") |> as.numeric() |> as.character(),
        .default = "0"
      ),
      # race_name = case_when(
      #   str_detect(race_name, regex("^US House", ignore_case=TRUE)) ~ "House",
      #   str_detect(race_name, regex("^Secretary of State", ignore_case=TRUE)) ~ "Secretary Of State",
      #   str_detect(race_name, regex("^Attorney General", ignore_case=TRUE)) ~ "Attorney General",
      #   str_detect(race_name, regex("^Lieutenant Governor", ignore_case=TRUE)) ~ "Lt Governor",
      #   .default = race_name
      # )
    ) |> 
    rename(
      office = race_name
    ) |> 
    mutate(eDate = "2024-03-05",
           real_precinct = ifelse(virtual_precinct, "N", "Y")) |> 
    left_join(lookup_geo, join_by(state == postalCode, jurisdiction == county_name)) |> 
    left_join(lookup_cands, join_by(office, jCde, candidate_party, candidate_name)) |> 
    mutate(jType = ifelse(jCde == "0", "SW", "CD")) |> 
    mutate(precinct_id = str_c(county_fips, precinct_id, sep = "_")) |> 
    rename(pcntName = precinct_id) |> 
    mutate(pcnt = pcntName,
           pcntUUID = pcntName) |> 
    pivot_wider(names_from = vote_mode, values_from = precinct_total) |> 
    rename(edayVote = `Election Day`,
           earlyInPersonVote = `Early Voting`,
           earlyByMailVote = `Absentee/Mail`,
           provisionalVote = Provisional,
           cName = candidate_name) |> 
    mutate(cVote = edayVote + earlyInPersonVote + earlyByMailVote + provisionalVote,
           ts = ymd_hms(timestamp, tz = "America/New_York") |> force_tz(tzone = "UTC") |> sf()) |> 
    select(eDate, jType, real_precinct, st, eType, jCde, ofc, cnty, pcnt, pcntUUID, pcntName, 
           cId, cName, cVote, edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote,
           ts) |> 
    mutate(across(c(jCde, st, cnty, cId), as.numeric)) |> 
    nest(candidates = c(cId, cName, cVote, edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote)) |> 
    nest(key = c(eDate, st, eType, jType, jCde, ofc, cnty, pcnt, pcntUUID)) |> 
    mutate(key2 = key) |> 
    unnest(cols = key2) |> 
    mutate(key = map(key, unbox)) |>
    select(eDate, jType, real_precinct, key, pcntName, candidates, jCde, ts, st, ofc, eType, pcntUUID, cnty)
  
  write_json(formatted, sprintf("data/cbs_format/%s/%s_%s_latest.json", state, county, type))
  
  data |> 
    filter(str_detect(race_name, regex("^Governor|President", ignore_case=TRUE))) |> 
    filter(candidate_party %in% c("Democrat", "Republican")) |> 
    write_csv(sprintf("data/cbs_format/%s/%s_%s_latest.csv", state, county, type))
  
  if (upload){
    put_object(file = sprintf("data/cbs_format/%s/%s_%s_latest.json", state, county, type),
               object = "Precincts/20240305-NC-P/nc_results.json",
               bucket = "cbsn-elections-external-models",
               multipart = TRUE)
    
    put_object(file = sprintf("data/cbs_format/%s/%s_%s_latest.csv", state, county, type),
               object = "Precincts/20240305-NC-P/nc_results.csv",
               bucket = "cbsn-elections-external-models",
               multipart = TRUE)
    
    drive_put(media = sprintf("data/cbs_format/%s/%s_%s_latest.csv", state, county, type),
              path = "https://drive.google.com/drive/folders/1mRGSpxHzfgF6pkXqOUPHvDmyG7QQUywM",
              name = "nc_results.csv") |> 
      drive_share_anyone()
    
  }

  return(formatted)
  
}
