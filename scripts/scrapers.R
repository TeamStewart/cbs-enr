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
  
  list.files("data/raw/GA", pattern = "*.csv", full.names = TRUE) |> 
    lapply(fread) |> 
    rbindlist(use.names = TRUE) |> 
    as_tibble() |> 
    mutate(state = "GA",
      virtual_precinct = FALSE) |> 
    mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
    mutate(candidate_name = case_match(
      vote_mode,
      "Undervotes" ~ "Undervote",
      "Overvotes" ~ "Overvote",
      .default = candidate_name
    ),
      # remove incumbent indicator from Biden
      candidate_name = str_remove_all(candidate_name, "\\(I\\)$") |> str_trim() |> str_squish()
    ) |> 
    mutate(vote_mode = case_match(
      vote_mode, 
      "Election Day Votes" ~ "Election Day",
      "Advance Voting Votes" ~ "Early Voting",
      "Absentee by Mail Votes" ~ "Absentee/Mail",
      "Provisional Votes" ~ "Provisional",
      c("Undervotes", "Overvotes") ~ "Aggregated",
      .default = NA_character_
    )) |> 
    mutate(candidate_party = case_match(
      candidate_party,
      "REP" ~ "Republican",
      "DEM" ~ "Democrat",
      .default = candidate_party
    )) |> 
    # mutate(race_name = case_match(
    #   race_name,
    #   .default = race_name
    # )) |> 
    select(state, race_id, race_name, candidate_name, candidate_party, 
      jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
  
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

## Pennsylvania
scrape_pa <- function(state, county, path, timestamp){
  
  if (county == "ALLEGHENY"){
    get_clarity(state, county, path) |> 
      read_csv() |> 
      mutate(across(where(is.character), ~ na_if(.x, ""))) |> 
      mutate(
        state = "PA",
        virtual_precinct = FALSE,
        candidate_name = case_match(
          vote_mode,
          "Undervotes" ~ "Undervote",
          "Overvotes" ~ "Overvote",
          .default = candidate_name
        ),
        # remove incumbent indicator from Biden
        candidate_name = str_remove_all(candidate_name, "\\(I\\)$") |> str_trim() |> str_squish(),
        vote_mode = case_match(
          vote_mode, 
          "Election Day Votes" ~ "Election Day",
          "Advance Voting Votes" ~ "Early Voting",
          "Absentee by Mail Votes" ~ "Absentee/Mail",
          "Provisional Votes" ~ "Provisional",
          c("Undervotes", "Overvotes") ~ "Aggregated",
          .default = NA_character_
        ),
        candidate_party = case_match(
          candidate_party,
          "REP" ~ "Republican",
          "DEM" ~ "Democrat",
          .default = candidate_party
        ),
        # TODO: Update for general
        race_name = case_match(
          race_name,
          "President of the US - Rep" ~ "President-Republican",
          "President of the US - Dem" ~ "President-Democrat",
          "President of the US/Presidente de los Estados Unidos - Rep" ~ "President-Republican", 
          "President of the US/Presidente de los Estados Unidos - Dem" ~ "President-Democrat",
          .default = race_name
        )
      ) |> 
      select(state, race_id, race_name, candidate_name, candidate_party, 
        jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
    
    
  } 
  else if (county == "PHILADELPHIA"){
    # dynamically retrieve file via python script
    source_python("scripts/util/dynamic_download.py")
    get_file(path, county, state)
    
    # Get file name of the most recently downloaded raw file
    files <- list.files(path = "data/raw/PA", full.names = TRUE, pattern = "^Philadelphia")
    
    # Get file info and sort by modification time in descending order
    most_recent_file <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
    
    read_csv(most_recent_file) |>
      # TODO: Update for general
      filter(RaceName %in% c("=\"PRESIDENT OF THE UNITED STATES DEM Democratic (VOTE FOR 1)\"", "=\"PRESIDENT OF THE UNITED STATES REP Republican (VOTE FOR 1)\"") & !str_detect(PrecinctName,"Congressional")) |>
      mutate(
        # TODO: Update for general
        RaceName = case_match(
          RaceName,
          "=\"PRESIDENT OF THE UNITED STATES DEM Democratic (VOTE FOR 1)\"" ~ "President-Democrat",
          "=\"PRESIDENT OF THE UNITED STATES REP Republican (VOTE FOR 1)\"" ~ "President-Republican",
          .default = NA_character_),
        PartyCode = case_match(
          PartyCode,
          "=\"DEM\"" ~ "Democrat",
          "=\"REP\"" ~ "Republican",
          .default = NA_character_),
        # TODO: Update for general
        CandidateName = case_match(
          CandidateName,
          "=\"JOSEPH R BIDEN JR DEM\"" ~ "Joseph R. Biden",
          "=\"DEAN PHILLIPS DEM\"" ~ "Dean Phillips",
          "=\"Write-in\"" ~ "Write-in",
          "=\"NIKKI R HALEY REP\"" ~ "Nikki Haley",
          "=\"DONALD J TRUMP REP\"" ~ "Donald J. Trump",
          .default = NA_character_),
        PrecinctName = str_replace_all(PrecinctName, "\"|=", ""),
        CandidateVotes = str_replace_all(CandidateVotes, "\"|=", ""),
        state = .env$state,
        jurisdiction = .env$county,
        race_id = NA,
        virtual_precinct = FALSE,
        vote_mode = 'Total'
      ) |>
      rename(
        race_name = RaceName,
        candidate_name = CandidateName,
        candidate_party = PartyCode,
        precinct_id = PrecinctName,
        precinct_total = CandidateVotes
      ) |>
      select(state, race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
    
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
