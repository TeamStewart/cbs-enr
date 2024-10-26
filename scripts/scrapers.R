## Arizona
scrape_az <- function(state, county, path, timestamp){
  
  # TODO: Update Maricopa for 2024 general election
  if(county == 'Maricopa'){
    path <- read_html(path) |>
      html_nodes(xpath = "//a[contains(text(), '2024 November General Election Results.txt')]") |>
      html_attr('href')
    
    path <- str_c('https://elections.maricopa.gov',path)
    
    # Download file
    source_python("scripts/util/dynamic_download.py")
    get_file(path, county, state)
    
    # Rename raw file to include identifiers and timestamp
    ## Get list of raw files
    files <- list.files(path = "data/raw/AZ", full.names = TRUE)
    ## Get file info and sort by modification time in descending order
    most_recent_file <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
    ## Rename file
    raw_file_path = glue('data/raw/AZ/az_maricopa_{timestamp}.txt')
    file.rename(most_recent_file, raw_file_path)
    
    cleaned <- fread(raw_file_path, header = TRUE, sep = "\t", quote = "") |>
      # clean column names
      clean_names() |> 
      # Filter to target contests: President, Governor
      filter(contest_name %in% c('Presidential Electors', 'US Senate')) |>
      # recode variables
      mutate(
        timestamp = timestamp |> ymd_hms(tz = "America/New_York"),
        state = 'AZ',
        county = 'Maricopa',
        # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
        contest_name = case_match(
          contest_name,
          "Presidential Electors" ~ "President",
          "US Senate" ~ "Senate"),
        # Recode candidate party: Democrat, Republican, Libertarian, Constitution, Green, Independent, Justice for All
        candidate_affiliation = case_match(
          candidate_affiliation,
          "DEM" ~ "Democrat",
          "REP" ~ "Republican",
          "LBT" ~ "Libertarian",
          "GRN" ~ "Green",
          .default = "Other"
        ),
        # Recode candidate names
        candidate_name = case_match(
          candidate_name,
          # AZ presidential candidates
          "OLIVER / TER MAAT" ~ "Chase Oliver",
          "TRUMP / VANCE" ~ "Donald Trump",
          "STEIN / WARE" ~ "Jill Stein",
          "HARRIS / WALZ" ~ "Kamala Harris",
          "Write-in" ~ "Write-ins",
          # AZ senate candidates
          "GALLEGO, RUBEN" ~ "Ruben Gallego",
          "LAKE, KARI" ~ "Kari Lake",
          "QUINTANA, EDUARDO" ~ "Eduardo Quintana",
        ), 
        virtual_precinct = FALSE,
      ) |>
      pivot_longer(cols = c(starts_with("votes_"), "undervotes","overvotes"), 
                   names_to = "vote_mode", 
                   values_to = "precinct_total") |>
      # Remove the "votes_" prefix from the vote_mode values
      mutate(vote_mode = str_replace(vote_mode, "votes_", "")) |>
      select(
        state, race_id = contest_id, race_name = contest_name , candidate_name,
        candidate_party = candidate_affiliation, jurisdiction = county, precinct_id = precinct_name, 
        virtual_precinct, timestamp, vote_mode, precinct_total)
    
    # Produce precinct-level under/overvote totals
    over_under <- cleaned |>
      filter(vote_mode %in% c("undervotes", "overvotes")) |>
      mutate(
        candidate_name = ifelse(vote_mode == "undervotes", "Undervotes", "Overvotes"),
        candidate_party = NA,
        vote_mode = "Overvote/Undervote") |>
      summarise(
        precinct_total = sum(precinct_total, na.rm = T), 
        .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","timestamp","vote_mode"))
    
    # Combine cleaned data with over/undervote totals
    cleaned |>
      filter(!vote_mode %in% c("undervotes", "overvotes")) |>
      bind_rows(over_under) |>
      arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
  }
  # TODO: Update Pima for 2024 general election
  else if(county == 'PIMA'){
    csv_link <- (read_html(path) |>
                   # TODO: Find node identifier
                   html_nodes("ul:nth-child(2) li:nth-child(4) a") |>
                   html_attr("href"))[1]
    
    raw_csv <- read_csv(csv_link, col_names = FALSE)
    
    # Write raw files
    write_csv(raw_csv, glue("data/raw/AZ/{state}_{county}_{type}_raw_{timestamp}.csv"))
    
    # TODO: Fix start row, col vals
    start_row <- 4
    start_column <- 19
    
    # Extract metaraw_csv from the CSV
    vote_modes <- raw_csv[[1]][start_row:nrow(raw_csv)]
    precinct_ids <- raw_csv[[3]][start_row:nrow(raw_csv)]
    race_name <- as.character(raw_csv[1, start_column:ncol(raw_csv)])
    candidate_party <- as.character(raw_csv[2, start_column:ncol(raw_csv)])
    candidate_name <- as.character(raw_csv[3, start_column:ncol(raw_csv)])
    # Generate unique race_id for each unique race_name
    unique_race_names <- unique(race_name)
    race_ids <- match(race_name, unique_race_names)
    
    # Create a new raw_csvframe to hold the cleaned raw_csv
    cleaned_raw_csv <- data.frame(
      state = state,
      race_id = rep(race_ids, each = length(precinct_ids)),
      race_name = rep(race_name, each = length(precinct_ids)),
      candidate_name = rep(candidate_name, each = length(precinct_ids)),
      candidate_party = rep(candidate_party, each = length(precinct_ids)),
      jurisdiction = county,
      precinct_id = rep(precinct_ids, times = length(race_name)),
      virtual_precinct = FALSE,
      vote_mode = rep(vote_modes, times = length(race_name)),
      precinct_total = integer(length(precinct_ids) * length(race_name)),
      stringsAsFactors = FALSE
    )
    
    # Process each column containing precinct totals
    cand_index <- 1
    for (col_index in start_column:ncol(raw_csv)) {
      precinct_totals <- raw_csv[start_row:nrow(raw_csv), col_index] |> as_vector() |> as.integer()
      cleaned_raw_csv$precinct_total[cleaned_raw_csv$candidate_name == candidate_name[cand_index]] <- precinct_totals
      cand_index <- cand_index + 1
    }
    
    cleaned_raw_csv |> filter(precinct_id != 'COUNTY TOTALS') |>
      mutate(
        precinct_total = as.integer(precinct_total),
        race_name = case_match(
          race_name,
          .default = NA_character_
        ),
        candidate_name = case_when(
          .default = candidate_name
        ),
        candidate_party = case_match(
          candidate_party,
          "DEM" ~ "Democrat",
          "REP" ~ "Republican",
          .default = NA_character_
        ),
        vote_mode = case_when(
          candidate_name == "Undervote" ~ "Aggregated",
          candidate_name == "Overvote" ~ "Aggregated",
          vote_mode == "POLLS" ~ "Election Day",
          vote_mode == "EARLY" ~ "Early Voting",
          vote_mode == "PROVISIONAL" ~ "Provisional"
        )
      ) |>
      filter(!is.na(race_name)) |>
      summarise(precinct_total = sum(precinct_total, na.rm = T), .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","vote_mode"))
  }
}

## Georgia
scrape_ga <- function(state, county, path, timestamp){
  # Download the raw json
  raw_file_path = glue('data/raw/GA/ga_{timestamp}.json')
  download.file(path, destfile = raw_file_path)
  
  # Clean the raw json
  read_json(raw_file_path) |> 
    pluck('localResults') |> 
    map_df(~.x) |>
    unnest_wider(col = ballotItems, names_repair = "unique") |>
    filter(`name...5` == 'President of the US') |>
    unnest_longer(col = ballotOptions) |>
    unnest_wider(col = ballotOptions, names_repair = "unique") |>
    select(-groupResults) |>
    # Unnest the precinctResults to get individual rows for each precinct
    unnest_longer(col = precinctResults) |>
    unnest_wider(col = precinctResults, names_repair = "unique") |>
    # Now unnest the groupResults within each precinct
    unnest_longer(col = groupResults) |>
    unnest_wider(col = groupResults, names_repair = "unique") |>
    clean_names() |>
    select(-c(id_1, type, vote_for:contest_type, id_11, ballot_order_13, vote_count_14, vote_count_19, reporting_status, ranked_choice_results, is_virtual)) |>
    rename(
      jurisdiction = name_2,
      race_id = id_4,
      race_name = name_5,
      candidate_name = name_12,
      candidate_party = political_party,
      precinct_id = id_16,
      virtual_precinct = is_from_virtual_precinct,
      vote_mode = group_name,
      precinct_total = vote_count_22
    ) |>
    mutate(
      timestamp = timestamp |> ymd_hms(tz = "America/New_York"),
      state = 'GA',
      jurisdiction = jurisdiction |> str_remove(" County"),
      # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
      race_name = case_match(race_name,"President of the US" ~ "President"),
      # Recode candidate party: Democrat, Republican, Libertarian, Constitution, Green, Independent, Justice for All
      ## Fix an issue from one county
      candidate_party = ifelse(
        candidate_party == 'DEM' | candidate_party == "", 
        str_extract(candidate_name, "\\(.*?\\)") |> str_remove_all("[()]"),
        candidate_party), 
      ## Now recode after fix
      candidate_party = case_when(
        candidate_party == "Dem" ~ "Democrat",
        candidate_party == "Rep" ~ "Republican",
        candidate_party == "Lib" ~ "Libertarian",
        candidate_party == "Grn" ~ "Green",
        candidate_party == "Ind" ~ "Independent",
        TRUE ~ "Other"
      ),
      # Recode candidate names
      candidate_name = case_match(
        candidate_name,
        # GA presidential candidates
        "Chase Oliver (Lib)" ~ "Chase Oliver",
        "Donald J. Trump (Rep)" ~ "Donald Trump",
        "Jill Stein (Grn)" ~ "Jill Stein",
        "Kamala D. Harris (Dem)" ~ "Kamala Harris",
        "Claudia De la Cruz (Ind)" ~ "Claudia De la Cruz",
        "Cornel West (Ind)" ~ "Cornel West",
        "Write-in" ~ "Write-ins",
      )
    ) |>
    select(state, race_id, race_name, candidate_name, candidate_party,
           jurisdiction, precinct_id, virtual_precinct, timestamp, vote_mode, precinct_total) |>
    arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
}

## Michigan
scrape_mi <- function(state, county, path, timestamp){
  if(county == 'Oakland'){
    # Download Clarity files
    get_clarity(state, county, path)
    
    # Build list of Clarity files
    raw_files <- list.files(path = glue('data/raw/{state}'), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE)
    
    clean_oakland_mi <- function(file){
      cleaned <- read_csv(file) |>
        filter(race_name == "Electors of President and Vice-President of the United States") |>
        mutate(
          timestamp = timestamp |> ymd_hms() |> with_tz(tzone = "America/New_York"),
          state = "MI",
          jurisdiction = "Oakland",
          # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
          race_name = case_match(
            race_name,
            "Electors of President and Vice-President of the United States" ~ "President"), 
          # Recode candidate party: Democrat, Republican, Libertarian, US Taxpayers, Green, Natural Law, Justice for All
          candidate_party = case_match(
            candidate_party,
            "DEM" ~ "Democrat",
            "REP" ~ "Republican",
            "LIB" ~ "Libertarian",
            "UST" ~ "US Taxpayers",
            "GRN" ~ "Green",
            "NL" ~ "Natural Law",
            .default = "Other"),
          # Recode candidate names
          candidate_name = case_match(
            candidate_name,
            # Oakland, MI presidential candidates
            "Chase Oliver/Mike ter Maat" ~ "Chase Oliver",
            "Cornel West/Melina Abdullah" ~ "Cornel West",
            "Donald J. Trump/J. D. Vance" ~ "Donald Trump",
            "Jill Stein/Rudolph Ware" ~ "Jill Stein",
            "Kamala D. Harris/Tim Walz" ~ "Kamala Harris",
            "Randall Terry/Stephen E. Broden" ~ "Randall Terry",
            "Joseph Kishore/Jerry White" ~ "Joseph Kishore",
            "Rejected write-ins" ~ "Write-ins",
            "Unassigned write-ins" ~ "Write-ins",
          ), 
          # Create virtual precinct column: real == TRUE, administrative == FALSE
          virtual_precinct = F,
          # Deal with under/overvotes
          candidate_name = case_when(
            vote_mode == "Undervotes" ~ "Undervotes",
            vote_mode == "Overvotes" ~ "Overvotes",
            TRUE ~ candidate_name
          ),
          candidate_party = ifelse(vote_mode %in% c("Undervotes", "Overvotes"), NA_character_, candidate_party),
          vote_mode = ifelse(vote_mode %in% c("Undervotes", "Overvotes"), "Overvote/Undervote", vote_mode)
        ) |>
        mutate(
          # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
          vote_mode = case_match(
            vote_mode,
            "Election" ~ "Election Day",
            "Absentee - Local" ~ "Absentee/Mail",
            "Absentee - County" ~ "Absentee/Mail",
            "Early Voting - Regional" ~ "Early Voting",
            "Early Voting - Central" ~ "Early Voting",
            .default = "Other"
          )
        ) |>
        summarise(
          precinct_total = sum(precinct_total, na.rm = T),
          .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","timestamp","vote_mode")) |>
        arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
      
      file_timestamp <- cleaned |> pull(timestamp) |> unique() |> max() |> str_replace_all("-|:| ", "_")
      
      write_csv(cleaned, file = glue("data/clean/{state}/{county}_{file_timestamp}.csv"))
    }
    
    cleaned_files <- lapply(raw_files, clean_oakland_mi)
    
    # Return latest timestamped version
    return(read_csv(list.files(path = glue("data/clean/{state}"), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE) |> max()))
  }
  
  
  # Build list of Clarity files
  
  # Function to clean Clarity files
  
  # ifelse for Oakland vs. Macomb
}

## North Carolina
scrape_nc <- function(state, county, path, timestamp) {
  
  raw_file_path = glue('data/raw/NC/nc_{timestamp}.zip')
  
  download.file(path, destfile = raw_file_path)
  
  # read the data
  fread(cmd = glue("unzip -p {raw_file_path}")) |>
    # Clean raw file variable names
    clean_names() |>
    # Filter to target contests: President, Governor
    filter(contest_name %in% c('US PRESIDENT', 'NC GOVERNOR')) |>
    mutate(
      timestamp = timestamp |> ymd_hms(tz = "America/New_York"),
      state = "NC",
      # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
      contest_name = case_match(
        contest_name,
        "US PRESIDENT" ~ "President",
        "NC GOVERNOR" ~ "Governor"), 
      # Recode candidate party: Democrat, Republican, Libertarian, Constitution, Green, Independent, Justice for All
      choice_party = case_match(
        choice_party,
        "DEM" ~ "Democrat",
        "REP" ~ "Republican",
        "LIB" ~ "Libertarian",
        "CST" ~ "Constitution",
        "GRE" ~ "Green",
        "UNA" ~ "Independent",
        "JFA" ~ "Justice for All",
        .default = "Other"),
      # Recode candidate names
      choice = case_match(
        choice,
        # NC presidential candidates
        "Chase Oliver" ~ "Chase Oliver",
        "Cornel West" ~ "Cornel West",
        "Donald J. Trump" ~ "Donald Trump",
        "Jill Stein" ~ "Jill Stein",
        "Kamala D. Harris" ~ "Kamala Harris",
        "Randall Terry" ~ "Randall Terry",
        "Write-In (Miscellaneous)" ~ "Write-ins",
        # NC gubernatorial candidates
        "Josh Stein" ~ "Josh Stein",
        "Mark Robinson" ~ "Mark Robinson",
        "Mike Ross" ~ "Mike Ross",
        "Vinny Smith" ~ "Vinny Smith",
        "Wayne Turner" ~ "Wayne Turner"
      ), 
      # Create virtual precinct column: real == TRUE, administrative == FALSE
      virtual_precinct = (real_precinct == "N")
    ) |>
    rename(
      race_id = contest_group_id,
      race_name = contest_name,
      candidate_name = choice,
      candidate_party = choice_party,
      jurisdiction = county,
      precinct_id = precinct
    ) |>
    select(
      state, race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, election_day:provisional, timestamp
    ) |>
    pivot_longer(cols = election_day:provisional, names_to = "vote_mode", values_to = "precinct_total") |>
    mutate(
      # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
      vote_mode = case_match(
        vote_mode,
        "election_day" ~ "Election Day",
        "early_voting" ~ "Early Voting",
        "absentee_by_mail" ~ "Absentee/Mail",
        "provisional" ~ "Provisional",
        .default = "Other"
      )
    ) |>
    arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
}

## Pennsylvania
scrape_pa <- function(state, county, path, timestamp){
  if (county == "ALLEGHENY"){
  } 
  else if (county == "Philadelphia"){
    # dynamically retrieve file via python script
    source_python("scripts/util/dynamic_download.py")
    get_file(path, county, state)
    
    # Rename latest file
    raw_file_path <- glue("data/raw/{state}/{county}_{timestamp}.csv")
    most_recent_file <- dir_info(glue("data/raw/{state}")) |> arrange(desc(modification_time)) |> slice(1) |> pull(path)
    file.rename(most_recent_file, raw_file_path)
    
    read_csv(raw_file_path) |>
      # deal with weird Philly formatting
      mutate(across(everything(), ~ gsub('^="|"$|",$', '', .))) |>
      clean_names() |>
      filter(race_name == 'PRESIDENT AND VICE-PRESIDENT OF THE UNITED STATES (VOTE FOR 1)') |>
      mutate(
        current_date_time = current_date_time |> mdy_hms(tz = "America/New_York"),
        state = "PA",
        jurisdiction = "Philadelphia",
        # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
        race_name = case_match(
          race_name,
          "PRESIDENT AND VICE-PRESIDENT OF THE UNITED STATES (VOTE FOR 1)" ~ "President"),
        # Recode candidate party: Democrat, Republican, Libertarian, US Taxpayers, Green, Natural Law, Justice for All
        party_code = case_match(
          party_code,
          "DEM" ~ "Democrat",
          "REP" ~ "Republican",
          "LIB" ~ "Libertarian",
          "GRN" ~ "Green",
          .default = 'Other'),
        # Recode candidate names
        candidate_name = case_match(
          candidate_name,
          "CHASE OLIVER & MIKE TER MAAT LIB" ~ "Chase Oliver",
          "DONALD J TRUMP & JD VANCE REP" ~ "Donald Trump",
          "JILL STEIN & RUDOLPH WARE GRN" ~ "Jill Stein",
          "KAMALA D HARRIS & TIM WALZ DEM" ~ "Kamala Harris",
          "Write-in" ~ "Write-ins",
          .default = NA_character_),
        virtual_precinct = FALSE,
        race_id = NA
      ) |>
      pivot_longer(cols = c(election_day:provisional), names_to = "vote_mode", values_to = "precinct_total") |>
      mutate(
        vote_mode = case_match(
          vote_mode,
          "election_day" ~ "Election Day",
          "mail_votes" ~ "Absentee/Mail",
          "provisional" ~ "Provisional")) |>
      rename(
        candidate_party = party_code,
        precinct_id = precinct_name,
        timestamp = current_date_time
      ) |>
      select(state, race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, timestamp, vote_mode, precinct_total) |>
      arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
  }
}

## generic function to get clarity files
get_clarity <- function(state, county, path){
  
  if(is.na(county)){
    # Statewide clarity site
    version <- request(glue("https://results.enr.clarityelections.com/{state}/{path}/current_ver.txt")) |> 
      req_headers("Accept" = "application/txt") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_retry(max_tries = 5) |> 
      req_perform() |> 
      resp_body_string()
    
    counties <- request(glue("https://results.enr.clarityelections.com/{state}/{path}/{version}/json/en/electionsettings.json")) |> 
      req_headers("Accept" = "application/json") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_retry(max_tries = 5) |> 
      req_perform() |> 
      resp_body_json() |> 
      pluck("settings", "electiondetails", "participatingcounties") |> 
      as_tibble_col() |> 
      unnest(cols = value) |> 
      separate_wider_delim(cols = value, delim = "|", names = c("county", "sitenum", "version", "timestamp", "unknown")) |> 
      mutate(county_url = glue("https://results.enr.clarityelections.com/{state}/{county}/{sitenum}/current_ver.txt")) |> 
      mutate(version = map_chr(county_url, ~ request(.x) |> 
                                 req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
                                 req_perform() |> 
                                 resp_body_string()
      )) |> 
      mutate(url = glue("https://results.enr.clarityelections.com/{state}/{county}/{sitenum}/{version}/json/en/electionsettings.json")) |> 
      select(county, sitenum, timestamp, url) |> 
      mutate(
        version = map(url, ~ request(.x) |> 
                        req_headers("Accept" = "application/json") |> 
                        req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
                        req_retry(max_tries = 5) |> 
                        req_perform() |> 
                        resp_body_json() |> 
                        pluck("versions")
        )
      ) |> 
      unnest_longer(col = version) |> 
      mutate(
        url = glue("https://results.enr.clarityelections.com/{state}/{county}/{sitenum}/{version}/reports/detailxml.zip"),
        raw_file_path = glue("data/raw/{state}/{county}_{version}.zip"))
    
    download_file <- function(url, version) {
      tryCatch(
        # Send the request to download the file
        request(url) |> 
          req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
          req_retry(max_tries = 5) |> 
          # Define the path with the `version` suffix for each file
          req_perform(
            path = str_c(
              glue("data/raw/{state}/"),
              str_extract(url, glue("({state}/)(.*?)(/)"), group = 2),
              "_", version, ".zip")),
        # Handle 404 error silently
        httr2_http_404 = function(cnd) NULL
      )
    }
    
    # Apply the function using map2 on url and version columns from `counties`
    map2(counties$url, counties$version, download_file)
    
    # Check which versions already downloaded, omit from the list to scrape
    counties <- counties |> mutate(
      state = state,
      csv_downloaded = file.exists(glue("data/raw/{state}/{county}_{version}.csv")))
    
    source_python("scripts/util/clarity_scraper.py")
    
    counties |> filter(!csv_downloaded) |> pull(raw_file_path) |> walk(.f = \(x) get_data_clarity(state, x))
  } else{
    # County level clarity site
    county = str_to_title(county) |> str_replace_all(" ", "_")
    
    version <- request(glue("https://results.enr.clarityelections.com/{state}/{county}/{path}/current_ver.txt")) |> 
      req_headers("Accept" = "application/txt") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_retry(max_tries = 5) |> 
      req_perform() |> 
      resp_body_string()
    
    version_files <- request(glue("https://results.enr.clarityelections.com/{state}/{county}/{path}/{version}/json/en/electionsettings.json")) |> 
      req_headers("Accept" = "application/json") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_retry(max_tries = 5) |> 
      req_perform() |> 
      resp_body_json() |> 
      pluck("versions") |> 
      as_tibble_col() |> 
      unnest(cols = value) |> 
      mutate(
        state = state, 
        county = county,
        url = glue("https://results.enr.clarityelections.com/{state}/{county}/{path}/{value}/reports/detailxml.zip"),
        raw_file_path = glue('data/raw/{state}/{county}_{value}.zip'))
    
    download_file <- function(url, version) {
      tryCatch(
        # Send the request to download the file
        request(url) |> 
          req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
          req_retry(max_tries = 5) |> 
          # Define the path with the `version` suffix for each file
          req_perform(
            path = str_c(
              glue("data/raw/{state}/"),
              str_extract(url, glue("({state}/)(.*?)(/)"), group = 2),
              "_", version, ".zip")),
        # Handle 404 error silently
        httr2_http_404 = function(cnd) NULL
      )
    }
    
    map2(version_files$url, version_files$value, download_file)
    
    # Check which versions already downloaded, omit from the list to scrape
    version_files <- version_files |> mutate(
      state = state,
      csv_downloaded = file.exists(glue("data/raw/{state}/{county}_{value}.csv")))
    
    source_python("scripts/util/clarity_scraper.py")
    
    version_files |> filter(!csv_downloaded) |> pull(raw_file_path) |> walk(.f = \(x) get_data_clarity(state, x))
  }
  
}
