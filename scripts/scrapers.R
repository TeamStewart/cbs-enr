scrape_nc <- function(state, county, type, path, timestamp) {
  
  download.file(path, destfile = "data/raw/nc_primary.zip")
  
  # read the data
  fread(cmd = "unzip -p data/raw/nc_primary.zip") |>
    mutate(
      timestamp = timestamp,
      state = "NC",
      # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
      # If primary, append election label: str_extract(`Contest Name`, "\\(.*?\\)") -- remove for general
      candidate_party = case_when(
        `Choice Party` == "DEM" ~ "Democrat",
        `Choice Party` == "REP" ~ "Republican",
        `Choice Party` == "LIB" ~ "Libertarian",
        `Choice Party` == "GRE" ~ "Green",
        `Choice Party` == "UNA" ~ "Independent",
        TRUE ~ "Other"
      ),
      # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
      # If primary, append election label: str_extract(`Contest Name`, "\\(.*?\\)") -- remove for general
      race_name = case_when(
        str_detect(`Contest Name`, "PRESIDENTIAL PREFERENCE") ~ str_c("President-", candidate_party),
        str_detect(`Contest Name`, "US SENATE") ~ str_c("US Senate-", candidate_party),
        str_detect(`Contest Name`, "US HOUSE OF REPRESENTATIVES") ~ str_c("US House-", str_extract(`Contest Name`, "DISTRICT [0-9]+"), "-", candidate_party),
        str_detect(`Contest Name`, "NC GOVERNOR") ~ str_c("Governor-", candidate_party),
        str_detect(`Contest Name`, "NC HOUSE OF REPRESENTATIVES") ~ str_c("State Legislature-Lower District ", str_extract(`Contest Name`, "[0-9]+"), "-", candidate_party),
        str_detect(`Contest Name`, "NC STATE SENATE") ~ str_c("State Legislature-Upper District ", str_extract(`Contest Name`, "[0-9]+"), "-", candidate_party)
      ) |> str_replace_all(c("\\(|\\)" = "", "DISTRICT" = "District")),
      # Create virtual precinct column: real == TRUE, administrative == FALSE
      virtual_precinct = `Real Precinct` == "N"
    ) |>
    rename(
      race_id = `Contest Group ID`,
      candidate_name = Choice,
      jurisdiction = County,
      precinct_id = Precinct
    ) |>
    select(
      state, race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, `Election Day`:`Provisional`
    ) |>
    # filter(!is.na(race_name)) |>
    pivot_longer(cols = `Election Day`:`Provisional`, names_to = "vote_mode", values_to = "precinct_total") |>
    mutate(
      # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
      vote_mode = case_when(
        vote_mode == "Election Day" ~ "Election Day",
        vote_mode == "Provisional" ~ "Provisional",
        vote_mode == "Absentee by Mail" ~ "Absentee/Mail",
        vote_mode == "One Stop" ~ "Early Voting",
        vote_mode == "Early Voting" ~ "Early Voting",
        TRUE ~ "Other"
      )
    ) |>
    arrange(race_name, candidate_party, candidate_name, jurisdiction)
  
  # save latest version
  # write_csv(data, sprintf("data/clean/%s/%s_latest.csv", state, type))
  # 
  # # save timestamped version
  # write_csv(data, sprintf("data/clean/%s/%s_%s.csv", state, type, timestamp))
  
  # return the data for `targets` to track
  # return(data)
}

scrape_ga <- function(path = NULL){
  
  get_counties <- function(counties = tibble(), clarity_num){
    
    version <- request(sprintf("https://results.enr.clarityelections.com/GA/%s/current_ver.txt", clarity_num)) |> 
      req_headers("Accept" = "application/txt") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_perform() |> 
      resp_body_string() 
    
    request(sprintf("https://results.enr.clarityelections.com/GA/%s/%s/json/en/electionsettings.json", clarity_num, version)) |> 
      req_headers("Accept" = "application/json") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_perform() |> 
      resp_body_json() |>
      pluck("settings", "electiondetails", "participatingcounties") |> 
      as_tibble_col() |> 
      unnest(cols = value) |> 
      separate_wider_delim(cols = value, delim = "|", names = c("county", "sitenum", "version", "timestamp", "unknown")) |> 
      mutate(county_url = sprintf("https://results.enr.clarityelections.com/GA/%s/%s/current_ver.txt", county, sitenum)) |> 
      mutate(version = map_chr(county_url, ~ request(.x) |> 
                             req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
                             req_perform() |> 
                             resp_body_string()
                           )) |> 
      mutate(url = sprintf("https://results.enr.clarityelections.com/GA/%s/%s/%s/reports/detailxml.zip", county, sitenum, version)) |> 
      select(county, version, timestamp, url)
      # drop counties that we already have the latest version for
      # anti_join(counties, join_by(county, version))
    
  }
  
  counties <- get_counties(clarity_num = path) |> 
    mutate(local = str_c("data/raw/ga/", county, ".zip"))
  
  map(counties$url, ~ request(.x) |> 
        req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |>
        req_retry(max_tries = 5) |> 
        req_perform(path = str_c("data/raw/ga/", str_extract(.x, "(GA/)(.*?)(/)", group = 2), ".zip"))
      ) |> 
    suppressMessages()
  
  reticulate::source_python("scripts/clarity_scraper.py")
  
  pull(counties, local) |> walk(get_data)
  
  list.files("data/raw/ga", pattern = "*.csv", full.names = TRUE) |> 
    lapply(fread) |> 
    rbindlist(use.names = TRUE) |> 
    as_tibble() |> 
    mutate(state = "GA",
           virtual_precinct = FALSE) |> 
    mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
    mutate(vote_mode = case_match(
      vote_mode, 
      "Election Day Votes" ~ "Election Day",
      "Advanced Voting Votes" ~ "Early Voting",
      "Absentee by Mail Votes" ~ "Absentee/Mail",
      "Provisional Votes" ~ "Provisional",
      .default = NA_character_
    )) |> 
    mutate(candidate_party = case_match(
      candidate_party,
      "REP" ~ "Republican",
      "DEM" ~ "Democrat",
      .default = candidate_party
    )) |> 
    select(state, race_id, race_name, candidate_name, candidate_party, 
           jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)

}

scrape_tx <- function(state, county, type, path = NULL, timestamp){
  
  if (county == "BEXAR"){
    
    drop_cols <- "Precinct Summary|Electionware Copyright|Summary Results|Joint Primary Election|1-Mar-22|Total Votes Cast|Statistics|Registered Voters|Ballots Cast|Voter Turnout|March 1, 2022"
    
    d <- fread(path, 
               select = c(1, 2, 7, 9, 12),
               skip = 3,
               fill = TRUE,
               blank.lines.skip = TRUE,
               colClasses = list(character=1:12),
               na.strings = "",
               col.names = c("candidate_name", "race_name", "Election Day", "Absentee/Mail", "Early Voting")) |> 
      as_tibble() |> 
      mutate(race_name = na_if(race_name, "Vote For 1")) |> 
      fill(race_name, .direction = "down") |> 
      filter(!(str_detect(candidate_name, drop_cols))) |> 
      mutate(precinct_id = str_extract(candidate_name, "\\d{4}")) |> 
      fill(precinct_id, .direction = "down") |> 
      filter(candidate_name != precinct_id) |> 
      mutate(candidate_party = str_extract(race_name, ".*?\\s") |> str_squish(),
             race_name = str_remove(race_name, candidate_party) |> str_squish(),
             candidate_party = case_match(candidate_party,
                                          "DEM" ~ "Democrat",
                                          "REP" ~ "Republican"),
             jurisdiction = county,
             state = state) |> 
      pivot_longer(cols = c(`Election Day`, `Absentee/Mail`, `Early Voting`), values_to = "precinct_total", names_to = "vote_mode") |> 
      # mutate(race_name = case_when(
      #   str_detect(race_name, "United States President") ~ "President",
      #   str_detect(race_name, "United States Senator") ~ "US Senate",
      #   str_detect(race_name, "United States Representative") ~ "US House",
      #   str_detect(race_name, "Governor") ~ "Governor",
      #   str_detect(race_name, "State Representative") ~ "State Legislature-Lower District",
      #   str_detect(race_name, "State Senator") ~ "State Legislature-Upper District",
      #   .default = race_name
      # )) |> 
      mutate(precinct_total = as.numeric(precinct_total),
             precinct_total = replace_na(precinct_total, 0)) |> 
      mutate(virtual_precinct = FALSE) |> 
      filter(max(precinct_total) > 0, .by = c(race_name, candidate_name, candidate_party)) |> 
      select(state, jurisdiction, precinct_id, race_name, candidate_name, candidate_party, vote_mode, precinct_total, virtual_precinct)
    
  } else if (county == "HIDALGO"){
    
    d <- NULL
    
  } else if (county == "EL PASO"){
    
    d <- NULL
    
  }
  
  return(d)
  
}

