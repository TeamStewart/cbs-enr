scrape_nc <- function(state, county, type, path, timestamp) {
  
  # read the data
  fread(cmd = sprintf("unzip -p %s", path)) |>
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

scrape_ga <- function(state, county, type, path = NULL){
  
  request(path) |> 
    req_headers("Accept" = "application/zip") |> 
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
    req_perform(re, path = sprintf("data/raw/ga_%s.zip", county))
  
  d <- unzip(sprintf("data/raw/ga_%s.zip", county)) |> 
    xml2::read_xml()
    
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

