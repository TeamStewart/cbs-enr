source("scripts/scrapers.R")
source("scripts/models.R")

process_data <- function(state, county, type, timestamp, path = NULL) {
  
  if (state == "NC"){
    d <- scrape_nc(state, county, type, path, timestamp)
  } else if (state == "TX"){
    d <- scrape_tx(state, county, type, path, timestamp)
  } else if (state == "GA"){
    d <- scrape_ga(path)
  }
  
  # save latest version
  write_csv(d, sprintf("data/clean/%s/%s_%s_%s_latest.csv", state, state, county, type))

  # save timestamped version
  write_csv(d, sprintf("data/clean/%s/%s_%s_%s_%s.csv", state, state, county, type, timestamp))
  
  return(d)
}

get_timestamp <- function(state, county, type) {
  nc_timestamp <- function() {
    read_xml("https://s3.amazonaws.com/dl.ncsbe.gov?delimiter=/&prefix=ENRS/2024_03_05/") |>
      xml2::as_list() |>
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
  
  ga_timestamp <- function(){
    
    version <- request("https://results.enr.clarityelections.com/GA/113667/current_ver.txt") |> 
      req_headers("Accept" = "application/txt") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_perform() |> 
      resp_body_string()
    
    request(sprintf("https://results.enr.clarityelections.com/GA/113667/%s/json/en/electionsettings.json", version)) |> 
      req_headers("Accept" = "application/json") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_perform() |> 
      resp_body_json() |>
      pluck("websiteupdatedat") |> 
      dmy_hms(tz = "America/New_York")
  }
  
  if (state == "NC" & type == "primary"){
    return(nc_timestamp())
  } else if (state == "TX"){
    return(NULL)
  } else if (state == "GA"){
    return(ga_timestamp())
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
    tab_style(
      style = list(
        cell_text(align = "center", v_align = "middle")
      ),
      locations = cells_body()
    ) |>
    cols_align(align = "center")
}

convert_cbs <- function(data, state, county, type, timestamp, upload=FALSE){
  
  sf <- suppressMessages(stamp("2022-11-09T11:26:47Z"))
  
  if (state == "TX"){
    return("NOT IMPLEMENTED")
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
    mutate(race_name = str_remove_all(race_name, "-Democrat|-Republican"),
           jurisdiction = str_to_upper(jurisdiction)) |>
    filter(candidate_party %in% c("Democrat", "Republican")) |> 
    mutate(
      jCde = case_when(
        str_detect(race_name, "^US House") ~ str_extract(race_name, "\\d+") |> as.numeric() |> as.character(),
        .default = "0"
      )
    ) |> 
    rename(
      office = race_name
    ) |> 
    mutate(eDate = "2024-03-12",
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
    # importantly, ensure that Aggregated is here
    # bind_rows(tibble(Aggregated = numeric())) |> 
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
    mutate(key = map(key, jsonlite::unbox)) |>
    select(eDate, jType, real_precinct, key, pcntName, candidates, jCde, ts, st, ofc, eType, pcntUUID, cnty)
  
  jsonlite::write_json(formatted, sprintf("data/cbs_format/%s/%s_%s_latest.json", state, county, type))
  
  data |> 
    filter(str_detect(race_name, regex("^Governor|President", ignore_case=TRUE))) |> 
    mutate(race_name = str_remove_all(race_name, "-Democrat|-Republican")) |>
    filter(candidate_party %in% c("Democrat", "Republican")) |> 
    write_csv(sprintf("data/cbs_format/%s/%s_%s_latest.csv", state, county, type))
  
  if (upload){
    put_object(file = sprintf("data/cbs_format/%s/%s_%s_latest.json", state, county, type),
               object = sprintf("Precincts/20240312-GA-P/%s_results.json", str_to_lower(state)),
               bucket = "cbsn-elections-external-models",
               multipart = TRUE)
    
    put_object(file = sprintf("data/cbs_format/%s/%s_%s_latest.csv", state, county, type),
               object = sprintf("Precincts/20240312-GA-P/%s_results.csv", str_to_lower(state)),
               bucket = "cbsn-elections-external-models",
               multipart = TRUE)
    
    drive_put(media = sprintf("data/cbs_format/%s/%s_%s_latest.csv", state, county, type),
              path = "https://drive.google.com/drive/folders/1nyEtfAnhw-G8e1krk7D4LvOPeBdIY94n",
              name = sprintf("%s_results.csv", str_to_lower(state)))
    
  }

  return(formatted)
  
}

run_models <- function(data, st, timestamp){
  
  if (st == "TX" | st == "GA"){
    return(NULL)
  }
  
  data |> 
    filter(str_detect(race_name, "^Governor|President"), candidate_party %in% c("Democrat", "Republican")) |> 
    nest(.by = c(state, race_name, candidate_party)) |> 
    rename(
      party = candidate_party,
      office = race_name
    ) |> 
    mutate(time = timestamp) |> 
    pwalk(execute_model)
  
}
