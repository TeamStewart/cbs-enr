## Arizona
scrape_az <- function(state, county, type, path = NULL, timestamp){
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
          "DEM US Senate" ~ "US SENATE-Democrat", 
          "DEM US Rep Dist CD-1" ~ "US HOUSE-01-Democrat",
          "DEM US Rep Dist CD-2" ~ "US HOUSE-02-Democrat",
          "DEM US Rep Dist CD-3" ~ "US HOUSE-03-Democrat",
          "DEM US Rep Dist CD-4" ~ "US HOUSE-04-Democrat",
          "DEM US Rep Dist CD-5" ~ "US HOUSE-05-Democrat",
          "DEM US Rep Dist CD-7" ~ "US HOUSE-07-Democrat",
          "DEM US Rep Dist CD-8" ~ "US HOUSE-08-Democrat",
          "DEM US Rep Dist CD-9" ~ "US HOUSE-09-Democrat",
          "DEM County Recorder" ~ "COUNTY RECORDER-Democrat",
          "REP US Senate" ~ "US SENATE-Republican",
          "REP US Rep Dist CD-1" ~ "US HOUSE-01-Republican",
          "REP US Rep Dist CD-2" ~ "US HOUSE-02-Republican",
          "REP US Rep Dist CD-3" ~ "US HOUSE-03-Republican",
          "REP US Rep Dist CD-4" ~ "US HOUSE-04-Republican",
          "REP US Rep Dist CD-5" ~ "US HOUSE-05-Republican",
          "REP US Rep Dist CD-7" ~ "US HOUSE-07-Republican",
          "REP US Rep Dist CD-8" ~ "US HOUSE-08-Republican",
          "REP US Rep Dist CD-9" ~ "US HOUSE-09-Republican",
          "REP County Recorder" ~ "COUNTY RECORDER-Republican",
          .default = NA_character_
        ),
        candidate_name = case_match(
          CandidateName,
          "CHERNY, ANDREI" ~ "Andrei Cherny",
          "GALÁN-WOODS, MARLENE" ~ "Marlene Galán-Woods",
          "HORNE, ANDREW" ~ "Andrew Horne",
          "KROEMER, KURT" ~ "Kurt Kroemer",
          "O'CALLAGHAN, CONOR" ~ "Conor O'Callaghan",
          "SHAH, AMISH" ~ "Amish Shah",
          "BACKIE, ROBERT" ~ "Robert Backie",
          "GEORGE, KIM" ~ "Kim George",
          "SCHWEIKERT, DAVID" ~ "David Schweikert",
          "NEZ, JONATHAN" ~ "Jonathan Nez",
          "CRANE, ELI" ~ "Eli Crane",
          "SMITH, JACK" ~ "Jack Smith",
          "ANSARI, YASSAMIN" ~ "Yassamin Ansari",
          "TERÁN, RAQUEL" ~ "Raquel Terán",
          "WOOTEN, DUANE M." ~ "Duane Wooten",
          "MENDOZA, JESUS DAVID" ~ "Jesus Mendoza",
          "Write-in" ~ "Write-ins",
          "ZINK, JEFF" ~ "Jeff Zink",
          "STANTON, GREG" ~ "Greg Stanton",
          "COOPER, KELLY" ~ "Kelly Cooper",
          "DAVISON, JEROME" ~ "Jerone Davison",
          "GILES, DAVE" ~ "Dave Giles",
          "JASSER, ZUHDI" ~ "Zuhdi Jasser",
          "SCHAFFNER, KATRINA" ~ "Katrina Schaffner",
          "BIGGS, ANDY" ~ "Andy Biggs",
          "GRIJALVA, RAÚL M." ~ "Raúl Grijalva",
          "BUTIEREZ SR., DANIEL FRANCIS" ~ "Daniel Butierez",
          "WHITTEN, GREGORY" ~ "Greg Whitten",
          "BRIODY, PATRICK \"PAT\"" ~ "Pat Briody",
          "FRANKS, TRENT" ~ "Trent Franks",
          "HAMADEH, ABRAHAM \"ABE\""~ "Abe Hamadeh",
          "KERN, ANTHONY" ~ "Anthony Kern",
          "MASTERS, BLAKE" ~ "Blake Masters",
          "TOMA, BEN" ~ "Ben Toma",
          "SMITH, QUACY" ~ "Quacy Smith",
          "GOSAR, PAUL" ~ "Paul Gosar",
          "GALLEGO, RUBEN" ~ "Ruben Gallego",
          "LAKE, KARI" ~ "Kari Lake",
          "LAMB, MARK" ~ "Mark Lamb",
          "REYE, ELIZABETH JEAN" ~ "Elizabeth Reye",
          "GALLEGOS, ISIAH" ~ "Isiah Gallegos",
          "GLENN, NICHOLAS N." ~ "Nicholas Glenn",
          "HEAP, JUSTIN" ~ "Justin Heap",
          "HIATT, DON W" ~ "Don Hiatt",
          "RICHER, STEPHEN" ~ "Stephen Richer",
          "STRINGHAM, TIM" ~ "Tim Stringham",
          "WILLIAMS, DUSTIN PAUL" ~ "Dustin Williams",
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
  } else if(county == 'PIMA'){
    message('No results to clean')
  } else if(county == 'PINAL'){
    message('No results to clean')
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
      # drop counties where we already have the latest version
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
      "President of the US - Rep" ~ "President-Republican",
      "President of the US - Dem" ~ "President-Democrat",
      "President of the US/Presidente de los Estados Unidos - Rep" ~ "President-Republican", 
      "President of the US/Presidente de los Estados Unidos - Dem" ~ "President-Democrat",
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
          race_name == "President" & candidate_party == 'Republican' ~ "President-Republican",
          race_name == "President" & candidate_party == 'Democrat' ~ "President-Democrat",
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
          race_name = case_when(
            Contest %in% c("Republican President", "REP President", "UNITED STATES PRESIDENT (REP)", "President (REP)") ~ "President-Republican",
            Contest %in% c("Democrat President", "DEM President", "UNITED STATES PRESIDENT (DEM)", "President (DEM)") ~ "President-Democrat",
            Contest == "President" & Party == "REP" ~ "President-Republican",
            Contest == "President" & Party == "DEM" ~ "President-Democrat"
          )
        ) |>
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
          race_name = case_when(
            Contest %in% c("Republican President", "REP President", "UNITED STATES PRESIDENT (REP)", "President (REP)") ~ "President-Republican",
            Contest %in% c("Democrat President", "DEM President", "UNITED STATES PRESIDENT (DEM)", "President (DEM)") ~ "President-Democrat",
            Contest == "President" & Party == "REP" ~ "President-Republican",
            Contest == "President" & Party == "DEM" ~ "President-Democrat"
          ),
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
      mutate(
        state = "PA",
        virtual_precinct = FALSE
      ) |> 
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
        "President of the US - Rep" ~ "President-Republican",
        "President of the US - Dem" ~ "President-Democrat",
        "President of the US/Presidente de los Estados Unidos - Rep" ~ "President-Republican", 
        "President of the US/Presidente de los Estados Unidos - Dem" ~ "President-Democrat",
        .default = race_name
      )) |> 
      select(state, race_id, race_name, candidate_name, candidate_party, 
        jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
    
    
  } else if (county == "PHILADELPHIA"){
    # dynamically retrive file via python script
    source_python("scripts/dynamic_download.py")
    get_file(path, county, state)
    
    # Get file name of the most recently downloaded raw file
    files <- list.files(path = "data/raw/PA", full.names = TRUE, pattern = "^Philadelphia")
    
    # Get file info and sort by modification time in descending order
    most_recent_file <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
    
    read_csv(most_recent_file) |>
      filter(RaceName %in% c("=\"PRESIDENT OF THE UNITED STATES DEM Democratic (VOTE FOR 1)\"", "=\"PRESIDENT OF THE UNITED STATES REP Republican (VOTE FOR 1)\"") & !str_detect(PrecinctName,"Congressional")) |>
      mutate(
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

## Texas
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
    
  } 
  else if (county == "HIDALGO"){
    
    d = get_clarity(state, county, path) |> 
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
        vote_mode = case_match(
          vote_mode,
          "Election" ~ "Election Day",
          "Early" ~ "Early Voting",
          "Absentee" ~ "Absentee/Mail",
          c("Undervotes", "Overvotes") ~ "Aggregated",
          .default = vote_mode
        ),
        candidate_party = case_match(
          candidate_party,
          "REP" ~ "Republican",
          "DEM" ~ "Democrat",
          .default = NA_character_
        ),
        race_name = case_match(
          race_name,
          "(R) President" ~ "President-Republican",
          "(D) President" ~ "President-Democrat",
          "(R) US Senator" ~ "US Senate-Republican",
          "(D) US Senator" ~ "US Senate-Democrat",
          "(D) US Rep 16" ~ "US House-16-Democrat",
          "(D) US Rep 23" ~ "US House-23-Democrat",
          "(R) US Rep 16" ~ "US House-16-Republican",
          "(R) US Rep 23" ~ "US House-23-Republican",
          .default = race_name
        )
      ) |> 
      select(state, race_id, race_name, candidate_name, candidate_party, 
        jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
    
  } 
  else if (county == "EL PASO"){
    
    d = get_clarity(state, county, path) |> 
      read_csv() |> 
      filter(vote_mode != "regVotersCounty") |> 
      mutate(
        state = "TX",
        virtual_precinct = FALSE,
        vote_mode = case_match(
          vote_mode,
          "Absentee By Mail" ~ "Absentee/Mail",
          .default = vote_mode
        ),
        candidate_party = case_when(
          str_detect(candidate_name, fixed("(R)")) ~ "Republican",
          str_detect(candidate_name, fixed("(D)")) ~ "Democrat",
          .default = NA_character_
        ),
        race_name = case_match(
          race_name,
          "(R) President" ~ "President-Republican",
          "(D) President" ~ "President-Democrat",
          "(R) US Senator" ~ "US Senate-Republican",
          "(D) US Senator" ~ "US Senate-Democrat",
          "(D) US Rep 16" ~ "US House-16-Democrat",
          "(D) US Rep 23" ~ "US House-23-Democrat",
          "(R) US Rep 16" ~ "US House-16-Republican",
          "(R) US Rep 23" ~ "US House-23-Republican",
          .default = race_name
        )
      ) |> 
      select(state, race_id, race_name, candidate_name, candidate_party, 
        jurisdiction, precinct_id, virtual_precinct, vote_mode, precinct_total)
    
  }
  else {
    stop("Invalid county given to Texas scraper")
  }
  return(d)
  
}

## Wisconsin
scrape_wi <- function(state, county, type, path = NULL, timestamp){
  if(county == 'MILWAUKEE'){
    precinct_text = read_html(path) |>
      html_elements('.precinctTable td') |> html_text()
    
    # save raw text
    writeLines(precinct_text, sprintf("data/raw/%s/%s_%s_raw_%s.txt",state, state, county, timestamp))
    
    dem_results <- matrix(precinct_text[4241:7420],ncol = 6,byrow = T) |>
      as.data.frame() |>
      select(-1) |>
      slice(-1)
    colnames(dem_results) <- c("Ward","Joe Biden", "Dean Phillips", "No Preference","Write-in")
    dem_results <- dem_results |>
      pivot_longer(cols = c("Joe Biden", "Dean Phillips", "No Preference","Write-in"),names_to = "candidate_name", values_to = "precinct_total") |>
      mutate(
        state = .env$state,
        jurisdiction = .env$county,
        race_id = NA,
        race_name = "President-Democrat",
        candidate_party = "Democrat",
        precinct_id = Ward,
        virtual_precinct = FALSE,
        vote_mode = "Total"
      ) |> 
      select(state, race_id, race_name, candidate_name,
             candidate_party, jurisdiction, precinct_id, virtual_precinct,vote_mode, precinct_total)
    
    
    rep_results <- matrix(precinct_text[7421:12190],ncol = 9,byrow = T) |>
      as.data.frame() |>
      select(-1) |>
      slice(-1)
    colnames(rep_results) <- c("Ward",'Chris Christie','Vivek Ramaswamy', 'Ron DeSantis', 'Nikki Haley','Donald Trump', "No Preference","Write-in")
    rep_results <- rep_results |>
      pivot_longer(cols = c('Chris Christie','Vivek Ramaswamy', 'Ron DeSantis', 'Nikki Haley','Donald Trump', "No Preference","Write-in"), names_to = "candidate_name", values_to = "precinct_total") |>
      mutate(
        state = .env$state,
        jurisdiction = .env$county,
        race_id = NA,
        race_name = "President-Republican",
        candidate_party = "Republican",
        precinct_id = Ward,
        virtual_precinct = FALSE,
        vote_mode = "Total"
      ) |> 
      select(state, race_id, race_name, candidate_name,
             candidate_party, jurisdiction, precinct_id, virtual_precinct,vote_mode, precinct_total)
    
    rbind(dem_results, rep_results) 
  }
}

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
