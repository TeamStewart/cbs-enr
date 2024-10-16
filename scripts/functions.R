source("scripts/util/utils.R")
source("scripts/util/globals.R")
source("scripts/scrapers.R")
source("scripts/models.R")

get_timestamp <- function(state, county, path) {
  clarity_timestamp <- function() {
    
    if (is.na(county)) {
      version = request(glue("https://results.enr.clarityelections.com/{state}/{path}/current_ver.txt")) |>
        req_headers("Accept" = "application/txt") |>
        req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |>
        req_perform() |>
        resp_body_string()
      
      settings = glue("https://results.enr.clarityelections.com/{state}/{path}/{version}/json/en/electionsettings.json")
    } 
    else {
      county_tmp <- str_to_title(county) |> str_replace_all(" ", "_")
      
      version <- request(glue("https://results.enr.clarityelections.com/{state}/{county_tmp}/{path}/current_ver.txt")) |>
        req_headers("Accept" = "application/txt") |>
        req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |>
        req_perform() |>
        resp_body_string()
      
      settings = glue("https://results.enr.clarityelections.com/{state}/{county_tmp}/{path}/{version}/json/en/electionsettings.json")
    }
    
    request(settings) |>
      req_headers("Accept" = "application/json") |>
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |>
      req_perform() |>
      resp_body_json() |>
      pluck("websiteupdatedat") |>
      mdy_hms(tz = "America/New_York") |>
      str_replace_all("-|:| ", "_")
  }

  az_timestamp <- function() {
    if (county == "MARICOPA") {
      read_html("https://elections.maricopa.gov/results-and-data/election-results.html#ElectionResultsSearch") |>
        html_element(":nth-child(8) small") |>
        html_text2() |>
        str_extract("\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} [AP]M") |>
        mdy_hms(tz = "America/Phoenix") |>
        with_tz(tzone = "America/New_York") |>
        str_replace_all("-|:| ", "_")
    } else if (county == "PINAL") {
      clarity_timestamp()
    } else if (county == "PIMA") {
      Sys.time() |>
        str_extract("\\d{4}-\\d{2}-\\d{2} \\d{1,2}:\\d{2}:\\d{2}") |>
        str_replace_all("-|:| ", "_")
    }
  }

  fl_timestamp <- function() {
    if (county %in% c("MARTIN", "PINELLAS")) {
      clarity_timestamp()
    } else {
      read_html(path) |>
        html_element("#LastUpdated") |>
        html_text2() |>
        str_extract("\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} [ap]m") |>
        mdy_hms(tz = "America/New_York") |>
        str_replace_all("-|:| ", "_")
    }
  }

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

  pa_timestamp <- function() {
    if (county == "ALLEGHENY") {
      clarity_timestamp()
    } else if (county == "PHILADELPHIA") {
      read_html("https://philadelphiaresults.azurewebsites.us/ResultsExport.aspx?") |>
        html_element(".turnout-last-updated-time") |>
        html_text() |>
        mdy_hms(tz = "America/New_York") |>
        str_replace_all("-|:| ", "_")
    }
  }

  switch(state,
    "AZ" = az_timestamp(),
    "GA" = clarity_timestamp(),
    "FL" = fl_timestamp(),
    "NC" = nc_timestamp(),
    "PA" = pa_timestamp(),
  )
}

get_data <- function(state, county, timestamp, path = NULL) {
  
  dir_create(glue("data/raw/{state}"))
  dir_create(glue("data/input/{state}"))
  
  d <- switch(state,
    "AZ" = scrape_az(state, county, path, timestamp),
    "GA" = scrape_ga(state, county, path, timestamp),
    "FL" = scrape_fl(state, county, path, timestamp),
    "NC" = scrape_nc(state, county, path, timestamp),
    "PA" = scrape_pa(state, county, path, timestamp),
  )

  # save latest version
  dir_create(glue("data/clean/{state}"))
  write_csv(d, glue("data/clean/{state}/{state}_{county}_{ELECTION_TYPE}_latest.csv"))

  # save timestamped version
  write_csv(d, glue("data/clean/{state}/{state}_{county}_{ELECTION_TYPE}_{timestamp}.csv"))

  return(d)
}

create_table_generic <- function(data, state, county, type, timestamp) {
  if (!is.na(county)) {
    locale <- str_c(str_to_title(county), ", ", str_to_upper(state))
  } else {
    locale <- str_to_upper(state)
  }

  if (state %in% c("FL", "PA", "AZ")) {
    return(NULL)
  } else {
    data |>
      summarise(count = sum(precinct_total, na.rm = TRUE), .by = c(race_name, candidate_party, candidate_name)) |>
      arrange(race_name, candidate_party, desc(count)) |>
      mutate(percent = count / sum(count), .by = race_name) |>
      gt() |>
      tab_header(
        title = glue("2024 {str_to_title(type)} Results in {locale}"),
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
        palette = c("white", "#9FDDF3", "#59CBF5", "#00BAFF", "#3791FF")
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
}

create_table_cbs <- function(data, state, county, type, timestamp, upload = FALSE) {
  sf <- suppressMessages(stamp("2022-11-09T11:26:47Z"))

  lookup_geo <- read_csv("data/input/cbs_lookups/All States and Counties.csv",
    skip = 1,
    col_names = c("state_name", "postalCode", "st", "state_fips", "county_name", "cnty", "county_fips")
  ) |>
    filter(postalCode == state) |>
    select(-state_fips) |>
    mutate(county_name = str_to_upper(county_name))

  lookup_cands <- read_csv(glue("data/input/cbs_lookups/{cbs_lookup}"),
    col_select = c(-LastName),
    name_repair = janitor::make_clean_names,
    col_types = cols(.default = col_character())
  )

  # check if we have Absentee/Mail
  missing_absentee <- "Absentee/Mail" %in% unique(data$vote_mode)

  formatted <- data |>
    mutate(
      race_name = str_remove_all(race_name, "-Democrat|-Republican"),
      jurisdiction = str_to_upper(jurisdiction)
    ) |>
    mutate(
      jCde = case_when(
        str_detect(race_name, "^US HOUSE|^STATE HOUSE") ~ str_extract(race_name, "\\d+") |> as.numeric() |> as.character(),
        .default = "0"
      ),
      ofc = case_when(
        str_detect(race_name, "^US HOUSE") ~ "H",
        str_detect(race_name, "^US SENATE") ~ "S"
      ),
      race_name = case_when(
        str_detect(race_name, "^US HOUSE") ~ "House",
        str_detect(race_name, "^US SENATE") ~ "Senate"
      )
    ) |>
    rename(
      office = race_name
    ) |>
    mutate(
      eDate = ELECTION_DATE,
      real_precinct = ifelse(virtual_precinct, "N", "Y")
    ) |>
    left_join(lookup_geo, join_by(state == postalCode, jurisdiction == county_name)) |>
    left_join(lookup_cands, by = c("state_name" = "state", "office", "jCde" = "jurisdiction_code", "candidate_name" = "full_name")) |>
    # This filter is added to deal with fact that some candidates don't appear in CBS xwalk
    filter(!is.na(election_type)) |>
    mutate(jType = ifelse(jCde == "0", "SW", "CD")) |>
    mutate(precinct_id = str_c(county_fips, precinct_id, sep = "_")) |>
    rename(pcntName = precinct_id) |>
    mutate(
      pcnt = pcntName,
      pcntUUID = pcntName
    ) |>
    pivot_wider(names_from = vote_mode, values_from = precinct_total) |>
    # in case jurisdiction does not report
    mutate(`Absentee/Mail` = ifelse(!missing_absentee, NA, `Absentee/Mail`)) |>
    rename(
      edayVote = `Election Day`,
      earlyInPersonVote = `Early Voting`,
      earlyByMailVote = `Absentee/Mail`,
      provisionalVote = Provisional,
      cName = candidate_name,
      eType = election_type_code,
      cId = candidate_id
    ) |>
    # importantly, ensure that Aggregated is here
    # bind_rows(tibble(Aggregated = numeric())) |>
    mutate(
      cVote = rowSums(across(c(edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote), ~ replace_na(., 0))),
      ts = ymd_hms(timestamp, tz = "America/New_York") |> force_tz(tzone = "UTC") |> sf()
    ) |>
    select(
      eDate, jType, real_precinct, st, eType, jCde, ofc, cnty, pcnt, pcntUUID, pcntName,
      cId, cName, cVote, edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote,
      ts
    ) |>
    mutate(across(c(jCde, st, cnty, cId), as.numeric)) |>
    nest(candidates = c(cId, cName, cVote, edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote)) |>
    nest(key = c(eDate, st, eType, jType, jCde, ofc, cnty, pcnt, pcntUUID)) |>
    mutate(key2 = key) |>
    unnest(cols = key2) |>
    mutate(key = map(key, jsonlite::unbox)) |>
    select(eDate, jType, real_precinct, key, pcntName, candidates, jCde, ts, st, ofc, eType, pcntUUID, cnty)

  write_json(formatted, glue("data/cbs_format/{state}/{county}_{type}_latest.json"))
  write_csv(data, glue("data/cbs_format/{state}/{county}_{type}_latest.csv"))

  if (upload) {
    put_object(
      file = glue("data/cbs_format/{state}/{county}_{type}_latest.json"),
      object = glue("{cbs_s3_path}/{state}_{county}_{type}_results.json"),
      bucket = PATH_CBS_S3,
      multipart = TRUE
    )

    put_object(
      file = glue("data/cbs_format/{state}/{county}_{type}_latest.csv"),
      object = glue("{cbs_s3_path}/{state}_{county}_{type}_results.csv"),
      bucket = PATH_CBS_S3,
      multipart = TRUE
    )

    drive_put(
      media = glue("data/cbs_format/{state}/{county}_{type}_latest.csv"),
      path = PATH_GDRIVE,
      name = glue("{state}_{county}_{type}_latest_results.csv")
    )
  }

  return(formatted)
}
