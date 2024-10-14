## Arizona
scrape_az <- function(state, county, type, path = NULL, timestamp){
  
  # TODO: Update Maricopa for 2024 general election
  if(county == 'MARICOPA'){
    # retrieve file link -- they might change it over the course of the night
    path <- read_html(path) |>
      html_nodes(xpath = "//a[contains(text(), '2024 July Primary Election Results.txt')]") |>
      html_attr('href')
    
    path <- str_c('https://elections.maricopa.gov',path)
    
    source_python("scripts/dynamic_download.py")
    get_file(path, county, state)
    
    # Get file name of the most recently downloaded raw file
    files <- list.files(path = "data/raw/AZ", full.names = TRUE)
    
    # Get file info and sort by modification time in descending order
    most_recent_file <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
    
    read.table(most_recent_file, header = TRUE, sep = "\t", quote = "") |>
      # recode variables
      mutate(
        state = .env$state,
        race_name = case_match(
          ContestName,
          .default = NA_character_
        ),
        candidate_name = case_match(
          CandidateName,
          .default = CandidateName
        ),
        candidate_party = case_when(
          CandidateAffiliation == "DEM" ~ "Democrat",
          CandidateAffiliation == "REP" ~ "Republican",
          str_detect(race_name,"Democrat") ~ "Democrat",
          str_detect(race_name,"Republican") ~ "Republican",
          CandidateName %in% c("Write-in", "*NOT QUALIFIED*") & str_detect(race_name,"Democrat") ~ "Democrat",
          CandidateName %in% c("Write-in", "*NOT QUALIFIED*") & str_detect(race_name,"Republican") ~ "Republican",
          TRUE ~ NA_character_
        ),
        jurisdiction = county,
        virtual_precinct = FALSE,
        Aggregated = Overvotes + Undervotes,
        `Election Day` = Votes_ELECTION.DAY,
        `Early Voting` = Votes_EARLY.VOTE,
        Provisional = Votes_PROVISIONAL
      ) |>
      filter(!is.na(race_name)) |>
      pivot_longer(cols = c("Aggregated","Election Day", "Early Voting", "Provisional"),names_to = "vote_mode", values_to = "precinct_total") |>
      select(state, race_id = ContestId, race_name, candidate_name,
        candidate_party, jurisdiction, precinct_id = PrecinctName, virtual_precinct,vote_mode, precinct_total)
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
scrape_ga <- function(state, county, type, path = NULL, timestamp){
  
  get_counties <- function(clarity_num){
    
    version <- request(sprintf("https://results.enr.clarityelections.com/GA/%s/current_ver.txt", clarity_num)) |> 
      req_headers("Accept" = "application/txt") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_retry(max_tries = 5) |> 
      req_perform() |> 
      resp_body_string() 
    
    if (file.exists("data/input/GA/county_versions.csv")) {
      counties <- read_csv("data/input/GA/county_versions.csv", col_types = "cccc", show_col_types = FALSE)
    } else {
      counties <- tibble(county = "", version = "")
    }
    
    cntys <- request(sprintf("https://results.enr.clarityelections.com/GA/%s/%s/json/en/electionsettings.json", clarity_num, version)) |> 
      req_headers("Accept" = "application/json") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_retry(max_tries = 5) |> 
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
      select(county, version, timestamp, url) |> 
      # don't update counties where we already have the latest version
      anti_join(counties, join_by(county, version))
    
    # update the version file with the latest versions
    counties |> 
      anti_join(cntys, join_by(county, version)) |> 
      write_csv("data/input/GA/county_versions.csv")
    
    return(cntys)
    
  }
  
  counties <- get_counties(clarity_num = path) |> 
    mutate(local = str_c("data/raw/GA/", county, ".zip"))
  
  download_file <- function(url){
    tryCatch(
      request(url) |> 
        req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |>
        req_retry(max_tries = 5) |> 
        req_perform(path = str_c("data/raw/ga/", str_extract(url, "(GA/)(.*?)(/)", group = 2), ".zip")),
      httr2_http_404 = function(cnd) NULL
    )
  }
  
  map(counties$url, download_file)
  
  source_python("scripts/clarity_scraper.py")
  
  pull(counties, local) |> walk(state, get_data)
  
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
    mutate(race_name = case_match(
      race_name,
      .default = race_name
    )) |> 
    select(state, race_id, race_name, candidate_name, candidate_party, 
      jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
  
}

## Florida
scrape_fl <- function(state, county, type, path = NULL, timestamp) {
  process_clarity <- function(state, county, path) {
    get_clarity(state, county, path) |>
      read_csv() |>
      mutate(
        state = state,
        virtual_precinct = FALSE,
        candidate_name = case_match(
          vote_mode,
          "Undervotes" ~ "Undervote",
          "Overvotes" ~ "Overvote",
          .default = candidate_name
        ),
        candidate_name = str_remove_all(candidate_name, "\\(I\\)$") |> str_trim() |> str_squish(),
        vote_mode = case_match(
          vote_mode,
          "Election Day Votes" ~ "Election Day",
          "Early Vote" ~ "Early Voting",
          "Vote by Mail" ~ "Absentee/Mail",
          c("Undervotes", "Overvotes") ~ "Aggregated",
          .default = NA_character_
        ),
        candidate_party = case_match(
          candidate_party,
          "REP" ~ "Republican",
          "DEM" ~ "Democrat",
          .default = candidate_party
        ),
        race_name = case_when(
          TRUE ~ race_name
        )
      ) |>
      select(state, race_id, race_name, candidate_name, candidate_party, 
             jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
  }
  
  process_other <- function(state, county, path, timestamp) {
    csv_path <- read_html(path) |>
      html_node(xpath = '//a[contains(@href, "CandidateResultsbyPrecinctandParty_") and contains(@href, ".csv")]') |>
      html_attr("href")
    
    input <- read_csv(csv_path) |> write_csv(glue("data/raw/{state}/{county}_{timestamp}.csv"))
    
    common_mutate <- input |>
      mutate(
        state = state,
        race_id = NA,
        candidate_party = case_match(
          Party,
          'REP' ~ "Republican",
          'DEM' ~ "Democrat",
          .default = Party
        ),
        `Candidate Issue` = case_match(
          `Candidate Issue`,
          "Undervotes" ~ "Undervote",
          "Overvotes" ~ "Overvote",
          "UnderVotes" ~ "Undervote",
          "OverVotes" ~ "Overvote",
          .default = `Candidate Issue`
        ),
        jurisdiction = county,
        precinct_id = str_remove(`Precinct Name`, "^PRECINCT "),
        virtual_precinct = FALSE
      )
    
    if ("Mail Votes" %in% colnames(input)) {
      common_mutate |>
        select(-any_of("Total Votes")) |>
        mutate(
          # TODO: update `race_name` for general
          race_name = contest
        )
        pivot_longer(cols = c("Mail Votes", "Early Votes", "Election Day Votes"), names_to = "vote_mode", values_to = "precinct_total") |>
        mutate(
          vote_mode = case_match(
            vote_mode,
            "Election Day Votes" ~ "Election Day",
            "Early Votes" ~ "Early Voting",
            "Mail Votes" ~ "Absentee/Mail"
          ),
          precinct_total = ifelse(precinct_total == "-", NA, precinct_total) |> as.numeric()
        ) |>
        filter(!is.na(race_name) & vote_mode %in% c("Election Day", "Early Voting", "Absentee/Mail")) |>
        select(state, race_id, race_name, candidate_name = `Candidate Issue`,
               candidate_party, jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
    } else {
      common_mutate |>
        mutate(
          # TODO: update `race_name` for general
          race_name = contest,
          vote_mode = 'Total',
          precinct_total = `Total Votes`,
          precinct_total = ifelse(precinct_total == "-", NA, precinct_total) |> as.numeric()
        ) |>
        filter(!is.na(race_name)) |>
        select(state, race_id, race_name, candidate_name = `Candidate Issue`,
               candidate_party, jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
    }
  }
  
  if (county %in% c('MARTIN', 'PINELLAS')) {
    process_clarity(state, county, path)
  } else {
    process_other(state, county, path, timestamp)
  }
}

## North Carolina
scrape_nc <- function(state, county, type, path = NULL, timestamp) {
  
  raw_file_path = glue('data/raw/NC/nc_primary_{timestamp}.zip')
  
  download.file(path, destfile = raw_file_path)
  
  # read the data
  fread(cmd = glue("unzip -p {raw_file_path}")) |>
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
      # TODO: Update for 2024
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
  
}

## Pennsylvania
scrape_pa <- function(state, county, type, path = NULL, timestamp){
  
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
    source_python("scripts/dynamic_download.py")
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
get_clarity <- function(state, county, path, type, timestamp){
  
  county = str_to_title(county) |> str_replace_all(" ", "_")
  download_path = glue("data/raw/{state}/{state}_{county}_{type}_raw_{timestamp}.zip")
  
  dir_create(glue("data/raw/{state}"))
  
  # get the latest version number of the file
  version = request(glue("https://results.enr.clarityelections.com/{state}/{county}/{path}/current_ver.txt")) |> 
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
    req_perform() |> 
    resp_body_string()
  
  # build the url
  url = glue("https://results.enr.clarityelections.com/{state}/{county}/{path}/{version}/reports/detailxml.zip")
  
  # try to download the file
  tryCatch(
    request(url) |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |>
      req_retry(max_tries = 5) |> 
      req_perform(path = download_path),
    httr2_http_404 = function(cnd) NULL
  )
  
  # run clarity scraper
  source_python("scripts/clarity_scraper.py")
  get_data(state, download_path)
  
  download_path |> str_replace("zip$", "csv")
  
}
