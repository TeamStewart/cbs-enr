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
    get_file(path, county, state, PATH_DROPBOX)
    
    # Rename raw file to include identifiers and timestamp
    ## Get list of raw files
    files <- list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/raw"), pattern = ".*\\.txt$", full.names = TRUE)
    ## Get file info and sort by modification time in descending order
    most_recent_file <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
    ## Rename file
    raw_file_path = glue('{PATH_DROPBOX}/24_general/{state}/raw/AZ_Maricopa_{timestamp}.txt')
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
      mutate(
        vote_mode = case_match(
          vote_mode,
          "election_day" ~ "Election Day",
          "early_vote" ~ "Early Voting",
          "absentee_by_mail" ~ "Absentee/Mail",
          "provisional" ~ "Provisional"
        )
      ) |>
      bind_rows(over_under) |>
      arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
  }
  # TODO: Update Pima for 2024 general election
  else if(county == 'Pima'){
    csv_link <- read_html(path) |>
      html_nodes(xpath = "//a[contains(text(), '2024 General Election') and contains(text(), 'Excel')]") |>
      html_attr('href')
    
    raw_file <- fread(csv_link, select=1:19, header=TRUE)
    
    write_csv(raw_file, glue("{PATH_DROPBOX}/24_general/{state}/raw/AZ_Pima_{timestamp}.csv"))
      
    colnames(raw_file)[7:19] <- paste(raw_file[2, 7:19], names(raw_file)[7:19], sep = " - ")
    
    raw_file |>
      clean_names() |>
      slice(-1, -2) |>
      select(-c(registered_voters_total, ballots_cast_total, precinct_name)) |>
      # remove total row
      filter(precinct_code != 'ZZZ') |>
      pivot_longer(cols = harris_kamala_d_presidential_electors:under_votes_u_s_senator, names_to = "candidate_name", values_to = "precinct_total") |>
      rename(
        jurisdiction = county_number,
        precinct_id = precinct_code,
      ) |>
      mutate(
        state = state,
        race_id = NA,
        virtual_precinct = NA,
        timestamp = timestamp |> ymd_hms(),
        precinct_id = str_replace(precinct_id, "^0+", ""),
        race_name = case_when(
          str_detect(candidate_name, "presidential") ~ "President",
          str_detect(candidate_name, "senator") ~ "Senate",
          TRUE ~ NA_character_
        ),
        candidate_name = case_match(
          candidate_name,
          "harris_kamala_d_presidential_electors" ~ "Kamala Harris",
          "trump_donald_j_presidential_electors" ~ "Donald Trump",
          "oliver_chase_presidential_electors" ~ "Chase Oliver",
          "stein_jill_presidential_electors" ~ "Jill Stein",
          "write_in_presidential_electors" ~ "Write-ins",     
          "over_votes_presidential_electors" ~ "Overvotes",
          "under_votes_presidential_electors" ~ "Undervotes",
          "gallego_ruben_u_s_senator" ~ "Ruben Gallego",           
          "lake_kari_u_s_senator" ~ "Kari Lake",
          "quintana_eduardo_u_s_senator" ~ "Eduardo Quintana",
          "write_in_u_s_senator" ~ "Write-ins",                
          "over_votes_u_s_senator" ~ "Overvotes",
          "under_votes_u_s_senator" ~ "Undervotes",
          .default = "Other"
        ),
        candidate_party = case_match(
          candidate_name,
          "Kamala Harris" ~ "Democrat",
          "Donald Trump" ~ "Republican",
          "Chase Oliver" ~ "Libertarian",
          "Jill Stein" ~ "Green",
          "Ruben Gallego" ~ "Democrat",
          "Kari Lake" ~ "Republican",
          "Eduardo Quintana" ~ "Green",
          "Write-ins" ~ NA_character_,
        ),
        vote_mode = rep_len(c("Election Day", "Early Voting", "Provisional"), n()),
        vote_mode = ifelse(candidate_name %in% c('Overvotes','Undervotes'), "Overvote/Undervote", vote_mode)
      ) |>
      select(state,race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, timestamp, vote_mode, precinct_total) |>
      arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
  }
}

## Georgia
scrape_ga <- function(state, county, path, timestamp){
  # Download the raw json
  raw_file_path = glue('{PATH_DROPBOX}/24_general/{state}/raw/GA_{timestamp}.json')
  download.file(path, destfile = raw_file_path)
  
  if (is.na(county)) {
    #### State-level ####
    base = tibble(data = read_json(raw_file_path) |> pluck('localResults')) |> 
      hoist(
        data,
        jurisdiction = "name",
        items = "ballotItems"
      ) |> 
      select(-data) |> 
      unnest_longer(items) 
    
  } else {
    #### County-Level ####
    base = tibble(items = read_json(raw_file_path) |> pluck('results', 3)) |> 
      mutate(jurisdiction = .env$county)
  }
  
  base |>
    hoist(
      items, 
      race_id = "id",
      race_name = "name",
      options = "ballotOptions",
      ballotOrder = "ballotOrder"
    ) |>
    filter(str_detect(race_name, regex("President|Presi", ignore_case=TRUE))) |> 
    filter(ballotOrder == min(ballotOrder)) |> 
    select(-items, -ballotOrder) |> 
    unnest_longer(options) |> 
    hoist(
      options,
      candidate_name = "name",
      candidate_party = "politicalParty",
      precinct_results = "precinctResults"
    ) |> 
    select(-options) |> 
    unnest_longer(precinct_results) |> 
    hoist(
      precinct_results,
      precinct_id = "name",
      groupResults = "groupResults",
      virtual_precinct_pct = "isVirtual",
      precinct_total_pct = "voteCount"
    ) |> 
    mutate(
      groupResults = case_when(
        is.na(groupResults) ~ list(list(list(groupName = character(), voteCount = integer(), isFromVirtualPrecinct = logical()))),
        .default = groupResults
      )
    ) |> 
    select(-precinct_results) |> 
    unnest_longer(groupResults) |> 
    hoist(
      groupResults,
      vote_mode = "groupName",
      precinct_total = "voteCount",
      virtual_precinct = "isFromVirtualPrecinct"
    ) |> 
    mutate(
      precinct_total = coalesce(precinct_total, precinct_total_pct),
      virtual_precinct = coalesce(virtual_precinct, virtual_precinct_pct),
      precinct_total_pct = NULL,
      virtual_precinct_pct = NULL,
      timestamp = .env$timestamp |> ymd_hms(),
      state = .env$state,
      timestamp = timestamp |> ymd_hms(tz = "America/New_York"),
      state = state,
      jurisdiction = jurisdiction |> str_remove(regex("County", ignore_case=TRUE)) |> str_squish() |> str_to_upper(),
      # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
      race_name = case_when(
        str_detect(race_name, regex("President|Presi", ignore_case=TRUE)) ~ "President",
        .default = race_name
      ),
      # Recode candidate party: Democrat, Republican, Libertarian, Constitution, Green, Independent, Justice for All
      ## Fix an issue from one county
      candidate_party = ifelse(
        candidate_party == 'DEM' | candidate_party == "",
        str_extract(candidate_name, "\\(.*?\\)") |> str_remove_all("[()]"),
        candidate_party),
      ## Now recode after fix
      candidate_party = case_when(
        str_detect(candidate_party, regex("Democrat|Dem", ignore_case=TRUE)) ~ "Democrat",
        str_detect(candidate_party, regex("Repub|Rep", ignore_case=TRUE)) ~ "Republican",
        str_detect(candidate_party, regex("Liber|Lib", ignore_case=TRUE)) ~ "Libertarian",
        str_detect(candidate_party, regex("Green|Grn", ignore_case=TRUE)) ~ "Green",
        str_detect(candidate_party, regex("Ind|Independent", ignore_case=TRUE)) ~ "Independent",
        .default = "Other"
      ),
      # Recode candidate names
      candidate_name = case_when(
        candidate_name %in% c("Chase Oliver (Lib)", "Chase R. Oliver", "CHASE OLIVER / MIKE TER MAAT") ~ "Chase Oliver",
        candidate_name %in% c("Donald J. Trump (Rep)", "DONALD J. TRUMP / JD VANCE", "Donald J. Trump") ~ "Donald Trump",
        candidate_name %in% c("Jill Stein (Grn)", "Jill E. Stein", "JILL STEIN / RUDOLPH WARE") ~ "Jill Stein",
        candidate_name %in% c("Kamala D. Harris (Dem)", "KAMALA D. HARRIS / TIM WALZ", "Kamala D. Harris") ~ "Kamala Harris",
        candidate_name %in% c("Claudia De la Cruz (Ind)", "CLAUDIA DE LA CRUZ / KARINA ALEXANDRA GARCIA") ~ "Claudia De la Cruz",
        candidate_name %in% c("Cornel West (Ind)", "CORNEL RONALD WEST / MELINA ABDULLAH", "Cornel R. West") ~ "Cornel West",
        candidate_name == "Write-in" ~ "Write-ins",
        candidate_name == "JOEL SKOUSEN / RIK COMBS" ~ "Joel Skousen",
        candidate_name == "LUCIFER \"JUSTIN CASE\" EVERYLOVE" ~ "Lucifer Everylove",
        .default = candidate_name
      ),
      vote_mode = case_when(
        str_detect(vote_mode, regex("Election Day", ignore_case = TRUE)) ~ "Election Day",
        str_detect(vote_mode, regex("Early Voting|Advanced Voting", ignore_case = TRUE)) ~ "Early Voting",
        str_detect(vote_mode, regex("Mail|Absentee", ignore_case = TRUE)) ~ "Absentee/Mail",
        str_detect(vote_mode, regex("Provisional", ignore_case = TRUE)) ~ "Provisional",
        .default = vote_mode
      )
    )
}

## Michigan
scrape_mi <- function(state, county, path, timestamp){
  if(county == 'Oakland'){
    # Download Clarity files
    get_clarity(state, county, path)
    
    # Build list of Clarity files
    raw_files <- list.files(path = glue('{PATH_DROPBOX}/24_general/{state}/raw'), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE)
    
    clean_oakland_mi <- function(file){
      cleaned <- read_csv(file) |>
        filter(race_name %in% c("Electors of President and Vice-President of the United States", "United States Senator")) |>
        mutate(
          timestamp = timestamp |> ymd_hms() |> with_tz(tzone = "America/New_York"),
          state = "MI",
          jurisdiction = "Oakland",
          # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
          race_name = case_match(
            race_name,
            "Electors of President and Vice-President of the United States" ~ "President",
            "United States Senator" ~ "Senate"), 
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
            # Oakland, MI senate candidates
            "Elissa Slotkin" ~ "Elissa Slotkin",
            "Mike Rogers" ~ "Mike Rogers",
            "Joseph Solis-Mullen" ~ "Joseph Solis-Mullen",
            "Dave Stein" ~ "Dave Stein",
            "Douglas P. Marsh" ~ "Douglas Marsh",
            "Doug Dern" ~ "Doug Dern"
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
            "Overvote/Undervote" ~ "Overvote/Undervote",
            .default = "Provisional"
          )
        ) |>
        summarise(
          precinct_total = sum(precinct_total, na.rm = T),
          .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","timestamp","vote_mode")) |>
        arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
      
      file_timestamp <- cleaned |> pull(timestamp) |> unique() |> max() |> str_replace_all("-|:| ", "_")
      
      write_csv(cleaned, file = glue("{PATH_DROPBOX}/24_general/{state}/clean/{state}_{county}_{file_timestamp}.csv"))
    }
    
    cleaned_files <- lapply(raw_files, clean_oakland_mi)
    
    # Return latest timestamped version
    return(read_csv(list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/clean"), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE) |> max()))
  } else if(county == 'Ingham'){
    # Download the raw json
    raw_file_path = glue('{PATH_DROPBOX}/24_general/{state}/raw/{state}_{county}_{timestamp}.json')
    download.file(path, destfile = raw_file_path)
    
    # Clean the raw json
    read_json(raw_file_path) |> 
      pluck('results', "ballotItems") |>
      map_df(~.x) |>
      filter(name %in% c('President/Vice-President of the United States', 'United States Senator')) |>
      unnest_wider(col = ballotOptions, names_repair = "unique") |>
      select(-groupResults) |>
      # Unnest the precinctResults to get individual rows for each precinct
      unnest_longer(col = precinctResults) |>
      unnest_wider(col = precinctResults, names_repair = "unique") |>
      # Now unnest the groupResults within each precinct
      unnest_longer(col = groupResults) |>
      unnest_wider(col = groupResults, names_repair = "unique") |>
      clean_names() |>
      select(-c(type, vote_for:contest_type, ballot_order_11, vote_count_12, vote_count_17, reporting_status, is_virtual)) |>
      rename(
        race_id = id_2,
        race_name = name_3,
        candidate_name = name_10,
        candidate_party = political_party,
        precinct_id = name_15,
        virtual_precinct = is_from_virtual_precinct,
        vote_mode = group_name,
        precinct_total = vote_count_20
      ) |>
      mutate(
        timestamp = timestamp |> ymd_hms(tz = "America/New_York"),
        state = 'MI',
        jurisdiction = 'Ingham',
        # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
        race_name = case_match(
          race_name,
          'President/Vice-President of the United States' ~ "President",
          'United States Senator' ~ "Senate",
          ),
        # Recode candidate party: Democrat, Republican, Libertarian, Constitution, Green, Independent, Justice for All
        candidate_party = case_match(
          candidate_party,
          "Democrat" ~ "Democrat",
          "Republican" ~ "Republican",
          "Libertarian" ~ "Libertarian",
          "U.S. Taxpayers" ~ "US Taxpayers",
          "Green" ~ "Green",
          "Natural Law" ~ "Natural Law",
          .default = "Other"
        ),
        # Recode candidate names
        candidate_name = case_match(
          candidate_name,
          # Oakland, MI presidential candidates
          "Chase Oliver" ~ "Chase Oliver",
          "Cornel West" ~ "Cornel West",
          "Donald J. Trump" ~ "Donald Trump",
          "Jill Stein" ~ "Jill Stein",
          "Kamala D. Harris" ~ "Kamala Harris",
          "Randall Terry" ~ "Randall Terry",
          "Joseph Kishore" ~ "Joseph Kishore",
          "Robert F. Kennedy, Jr." ~ "Robert F. Kennedy Jr.",
          "Write-in" ~ "Write-ins",
          # Oakland, MI senate candidates
          "Elissa Slotkin" ~ "Elissa Slotkin",
          "Mike Rogers" ~ "Mike Rogers",
          "Joseph Solis-Mullen" ~ "Joseph Solis-Mullen",
          "Dave Stein" ~ "Dave Stein",
          "Douglas P. Marsh" ~ "Douglas Marsh",
          "Doug Dern" ~ "Doug Dern"
        ),
        vote_mode = case_match(
          vote_mode,
          "Election Day" ~ "Election Day",
          "Early Voting" ~ "Early Voting",
          "AV Counting Boards" ~ "Absentee/Mail"
        )
      ) |>
      select(state, race_id, race_name, candidate_name, candidate_party,
             jurisdiction, precinct_id, virtual_precinct, timestamp, vote_mode, precinct_total) |>
      arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
  } else if(county == 'Eaton'){
    # Download Clarity files
    get_clarity(state, county, path)
    
    # Build list of Clarity files
    raw_files <- list.files(path = glue('{PATH_DROPBOX}/24_general/{state}/raw'), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE)
    
    clean_eaton_mi <- function(file){
      cleaned <- read_csv(file) |>
        filter(race_name %in% c("Electors of President and Vice-President of the United States", "United States Senator")) |>
        mutate(
          timestamp = timestamp |> ymd_hms() |> with_tz(tzone = "America/New_York"),
          state = "MI",
          jurisdiction = "Eaton",
          # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
          race_name = case_match(
            race_name,
            "Electors of President and Vice-President of the United States" ~ "President",
            "United States Senator" ~ "Senate"), 
          # Recode candidate party: Democrat, Republican, Libertarian, US Taxpayers, Green, Natural Law, Justice for All
          candidate_party = case_match(
            candidate_party,
            "DEM" ~ "Democrat",
            "REP" ~ "Republican",
            "LIB" ~ "Libertarian",
            "UST" ~ "US Taxpayers",
            "GRN" ~ "Green",
            "NLP" ~ "Natural Law",
            .default = "Other"),
          # Recode candidate names
          candidate_name = case_match(
            candidate_name,
            # Oakland, MI presidential candidates
            "Chase Oliver Mike ter Maat" ~ "Chase Oliver",
            "Cornel West Melina Abdullah" ~ "Cornel West",
            "Donald J. Trump JD Vance" ~ "Donald Trump",
            "Jill Stein Rudolph Ware" ~ "Jill Stein",
            "Kamala D. Harris Tim Walz" ~ "Kamala Harris",
            "Randall Terry Stephen E. Broden" ~ "Randall Terry",
            "Joseph Kishore Jerry White" ~ "Joseph Kishore",
            "Robert F. Kennedy, Jr. Nicole Shanahan" ~ "Robert F. Kennedy Jr.",
            "Unassigned write-ins" ~ "Write-ins",
            # Oakland, MI senate candidates
            "Elissa Slotkin" ~ "Elissa Slotkin",
            "Mike Rogers" ~ "Mike Rogers",
            "Joseph Solis-Mullen" ~ "Joseph Solis-Mullen",
            "Dave Stein" ~ "Dave Stein",
            "Douglas P. Marsh" ~ "Douglas Marsh",
            "Doug Dern" ~ "Doug Dern"
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
            "Absentee" ~ "Absentee/Mail",
            "Early Voting" ~ "Early Voting",
            "Overvote/Undervote" ~ "Overvote/Undervote",
            .default = "Provisional"
          )
        ) |>
        summarise(
          precinct_total = sum(precinct_total, na.rm = T),
          .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","timestamp","vote_mode")) |>
        arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
      
      file_timestamp <- cleaned |> pull(timestamp) |> unique() |> max() |> str_replace_all("-|:| ", "_")
      
      write_csv(cleaned, file = glue("{PATH_DROPBOX}/24_general/{state}/clean/{state}_{county}_{file_timestamp}.csv"))
    }
    
    cleaned_files <- lapply(raw_files, clean_eaton_mi)
    
    # Return latest timestamped version
    return(read_csv(list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/clean"), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE) |> max()))
  } else if(county == 'Macomb'){
    # Download Clarity files
    get_clarity(state, county, path)
    
    # Build list of Clarity files
    raw_files <- list.files(path = glue('{PATH_DROPBOX}/24_general/{state}/raw'), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE)
    
    clean_macomb_mi <- function(file){
      cleaned <- read_csv(file) |>
        filter(race_name %in% c("Electors of President and Vice-President of the United S", "United States Senator") & 
                 vote_mode != 'regVotersCounty') |>
        mutate(
          timestamp = timestamp |> ymd_hms() |> with_tz(tzone = "America/New_York"),
          state = "MI",
          jurisdiction = "Macomb",
          # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
          race_name = case_match(
            race_name,
            "Electors of President and Vice-President of the United S" ~ "President",
            "United States Senator" ~ "Senate"), 
          # Recode candidate party: Democrat, Republican, Libertarian, US Taxpayers, Green, Natural Law, Justice for All
          candidate_party = case_match(
            candidate_party,
            "DEM" ~ "Democrat",
            "REP" ~ "Republican",
            "LIB" ~ "Libertarian",
            "UST" ~ "US Taxpayers",
            "GRN" ~ "Green",
            "NLP" ~ "Natural Law",
            .default = "Other"),
          # Recode candidate names
          candidate_name = case_match(
            candidate_name,
            # Macomb, MI presidential candidates
            "LIB Chase Oliver" ~ "Chase Oliver",
            "NPA Cornel West" ~ "Cornel West",
            "REP Donald J. Trump" ~ "Donald Trump",
            "GRN Jill Stein" ~ "Jill Stein",
            "DEM Kamala D. Harris" ~ "Kamala Harris",
            "UST Randall Terry" ~ "Randall Terry",
            "NPA Joseph Kishore" ~ "Joseph Kishore",
            "NLP Robert F. Kennedy, Jr." ~ "Robert F. Kennedy Jr.",
            "Write-in" ~ "Write-ins",
            # Macomb, MI senate candidates
            "DEM Elissa Slotkin" ~ "Elissa Slotkin",
            "REP Mike Rogers" ~ "Mike Rogers",
            "LIB Joseph Solis-Mullen" ~ "Joseph Solis-Mullen",
            "UST Dave Stein" ~ "Dave Stein",
            "GRN Douglas P. Marsh" ~ "Douglas Marsh",
            "NLP Doug Dern" ~ "Doug Dern"
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
            "Election Day" ~ "Election Day",
            "Absentee" ~ "Absentee/Mail",
            "Early Voting" ~ "Early Voting",
            .default = "Provisional"
          )
        ) |>
        summarise(
          precinct_total = sum(precinct_total, na.rm = T),
          .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","timestamp","vote_mode")) |>
        arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
      
      file_timestamp <- cleaned |> pull(timestamp) |> unique() |> max() |> str_replace_all("-|:| ", "_")
      
      write_csv(cleaned, file = glue("{PATH_DROPBOX}/24_general/{state}/clean/{state}_{county}_{file_timestamp}.csv"))
    }
    
    cleaned_files <- lapply(raw_files, clean_macomb_mi)
    
    # Return latest timestamped version
    return(read_csv(list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/clean"), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE) |> max()))
  }
}

## North Carolina
scrape_nc <- function(state, county, path, timestamp) {
  raw_file_path <- glue('{PATH_DROPBOX}/24_general/{state}/raw/{state}_{timestamp}.zip')
  download.file(path, destfile = raw_file_path)
  raw_file_path <- unzip(raw_file_path, exdir = glue('{PATH_DROPBOX}/24_general/{state}/raw'))
  
  # read the data
  fread(raw_file_path) |>
    # Clean raw file variable names
    clean_names() |>
    # Filter to target contests: President, Governor
    filter(contest_name %in% c('US PRESIDENT', 'NC GOVERNOR')) |>
    mutate(
      timestamp = timestamp |> ymd_hms(tz = "America/New_York"),
      state = "NC",
      # Change to title case from upper case for joining to CBS meta
      county = str_to_title(county),
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
        "provisional" ~ "Provisional"
      )
    ) |>
    arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
}

## Pennsylvania
scrape_pa <- function(state, county, path, timestamp){
  if (county == "Allegheny"){
    # Download Clarity files
    get_clarity(state, county, path)
    
    # Build list of Clarity files
    raw_files <- list.files(path = glue('{PATH_DROPBOX}/24_general/{state}/raw'), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE)
    
    clean_allegheny_pa <- function(file){
      cleaned <- read_csv(file) |>
        filter(race_name %in% c("Presidential Electors", "United States Senator") & vote_mode != 'regVotersCounty') |>
        mutate(
          timestamp = timestamp |> ymd_hms() |> with_tz(tzone = "America/New_York"),
          state = "PA",
          jurisdiction = "Allegheny",
          # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
          race_name = case_match(
            race_name,
            "Presidential Electors" ~ "President",
            "United States Senator" ~ "Senate"), 
          # Recode candidate party: Democrat, Republican, Libertarian, US Taxpayers, Green, Natural Law, Justice for All
          candidate_party = case_match(
            candidate_party,
            "DEM" ~ "Democrat",
            "REP" ~ "Republican",
            "LIB" ~ "Libertarian",
            "GRN" ~ "Green",
            .default = "Other"),
          # Recode candidate names
          candidate_name = case_match(
            candidate_name,
            # PA presidential candidates
            "Chase Oliver" ~ "Chase Oliver",
            "Donald J. Trump" ~ "Donald Trump",
            "Jill Stein" ~ "Jill Stein",
            "Kamala D. Harris" ~ "Kamala Harris",
            "Write-in" ~ "Write-ins",
            # PA senate candidates
            "Robert P. Casey, Jr." ~ "Bob Casey",
            "Leila Hazou" ~ "Leila Hazou",
            "Dave McCormick" ~ "Dave McCormick",
            "Marty Selker" ~ "Marty Selker",
            "John C. Thomas" ~ "John Thomas",
          ), 
          # Create virtual precinct column: real == TRUE, administrative == FALSE
          virtual_precinct = F
        ) |>
        mutate(
          # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
          vote_mode = case_match(
            vote_mode,
            "Election Day" ~ "Election Day",
            "Absentee" ~ "Absentee/Mail",
            "Provisional" ~ "Provisional"
          )
        ) |>
        summarise(
          precinct_total = sum(precinct_total, na.rm = T),
          .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","timestamp","vote_mode")) |>
        arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
      
      file_timestamp <- cleaned |> pull(timestamp) |> unique() |> max() |> str_replace_all("-|:| ", "_")
      
      write_csv(cleaned, file = glue("{PATH_DROPBOX}/24_general/{state}/clean/{state}_{county}_{file_timestamp}.csv"))
    }
    
    cleaned_files <- lapply(raw_files, clean_allegheny_pa)
    
    # Return latest timestamped version
    return(read_csv(list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/clean"), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE) |> max()))
    
  } else if (county == "Delaware"){
    # Download Clarity files
    get_clarity(state, county, path)
    
    # Build list of Clarity files
    raw_files <- list.files(path = glue('{PATH_DROPBOX}/24_general/{state}/raw'), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE)
    
    clean_delaware_pa <- function(file){
      cleaned <- read_csv(file) |>
        filter(race_name %in% c("Presidential Electors", "United States Senator")) |>
        mutate(
          timestamp = timestamp |> ymd_hms() |> with_tz(tzone = "America/New_York"),
          state = "PA",
          jurisdiction = "Delaware",
          # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
          race_name = case_match(
            race_name,
            "Presidential Electors" ~ "President",
            "United States Senator" ~ "Senate"), 
          # Recode candidate party: Democrat, Republican, Libertarian, US Taxpayers, Green, Natural Law, Justice for All
          candidate_party = case_match(
            candidate_party,
            "DEM" ~ "Democrat",
            "REP" ~ "Republican",
            "LIB" ~ "Libertarian",
            "GRN" ~ "Green",
            "CST" ~ "Constitution",
            .default = "Other"),
          # Recode candidate names
          candidate_name = case_match(
            candidate_name,
            # Delaware, PA presidential candidates
            "(13) Chase Oliver / Mike ter Maat" ~ "Chase Oliver",
            "(12) Donald J. Trump /  JD Vance" ~ "Donald Trump",
            "(14) Jill Stein / Rudolph Ware" ~ "Jill Stein",
            "(11) Kamala D. Harris / Tim Walz" ~ "Kamala Harris",
            # Delaware, PA senate candidates
            "(21) Robert P. Casey Jr." ~ "Bob Casey",
            "(24) Leila Hazou" ~ "Leila Hazou",
            "(22) Dave McCormick" ~ "Dave McCormick",
            "(25) Marty Selker" ~ "Marty Selker",
            "(23) John C. Thomas" ~ "John Thomas",
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
          # Specify vote modes: Election Day, Provisional, Absentee/Mail, Early Voting, Other
          vote_mode = case_match(
            vote_mode,
            "Election Day Votes" ~ "Election Day",
            "Mail Votes" ~ "Absentee/Mail",
            "Provisional Votes" ~ "Provisional",
            "Undervotes" ~ "Overvote/Undervote",
            "Overvotes" ~ "Overvote/Undervote"
          )
        ) |>
        summarise(
          precinct_total = sum(precinct_total, na.rm = T),
          .by = c("state","race_id","race_name","candidate_name","candidate_party","jurisdiction","precinct_id","virtual_precinct","timestamp","vote_mode")) |>
        arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
      
      file_timestamp <- cleaned |> pull(timestamp) |> unique() |> max() |> str_replace_all("-|:| ", "_")
      
      write_csv(cleaned, file = glue("{PATH_DROPBOX}/24_general/{state}/clean/{state}_{county}_{file_timestamp}.csv"))
    }
    
    cleaned_files <- lapply(raw_files, clean_delaware_pa)
    
    # Return latest timestamped version
    return(read_csv(list.files(path = glue("{PATH_DROPBOX}/24_general/{state}/clean"), pattern = paste0(county, ".*\\.csv$"), full.names = TRUE) |> max()))
    
  } else if (county == "Philadelphia"){
    # dynamically retrieve file via python script
    source_python("scripts/util/dynamic_download.py")
    get_file(path, county, state, PATH_DROPBOX)
    
    # Rename latest file
    raw_file_path <- glue("{PATH_DROPBOX}/24_general/{state}/raw/{state}_{county}_{timestamp}.csv")
    most_recent_file <- dir_info(glue("{PATH_DROPBOX}/24_general/{state}/raw")) |> filter(str_detect(path, glue("Philadelphia"))) |> arrange(desc(modification_time)) |> slice(1) |> pull(path)
    file.rename(most_recent_file, raw_file_path)
    
    read_csv(raw_file_path) |>
      # deal with weird Philly formatting
      mutate(across(everything(), ~ gsub('^="|"$|",$', '', .))) |>
      clean_names() |>
      filter(race_name %in% c('PRESIDENT AND VICE-PRESIDENT OF THE UNITED STATES (VOTE FOR 1)','UNITED STATES SENATOR (VOTE FOR 1)')) |>
      mutate(
        current_date_time = current_date_time |> mdy_hms(tz = "America/New_York"),
        state = "PA",
        jurisdiction = "Philadelphia",
        # Recode contest names: President, Senator, US House, Governor, State Legislature - [Upper/Lower] District
        race_name = case_match(
          race_name,
          "PRESIDENT AND VICE-PRESIDENT OF THE UNITED STATES (VOTE FOR 1)" ~ "President",
          "UNITED STATES SENATOR (VOTE FOR 1)" ~ "Senate"),
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
          # PA presidential candidates
          "CHASE OLIVER & MIKE TER MAAT LIB" ~ "Chase Oliver",
          "DONALD J TRUMP & JD VANCE REP" ~ "Donald Trump",
          "JILL STEIN & RUDOLPH WARE GRN" ~ "Jill Stein",
          "KAMALA D HARRIS & TIM WALZ DEM" ~ "Kamala Harris",
          "Write-in" ~ "Write-ins",
          # PA senate candidates
          "ROBERT P CASEY JR DEM" ~ "Bob Casey",
          "LEILA HAZOU GRN" ~ "Leila Hazou",
          "DAVE MCCORMICK REP" ~ "Dave McCormick",
          "MARTY SELKER CST" ~ "Marty Selker",
          "JOHN C THOMAS LIB"~"John Thomas",
          .default = NA_character_),
        virtual_precinct = FALSE,
        race_id = NA,
      ) |>
      pivot_longer(cols = c(election_day:provisional), names_to = "vote_mode", values_to = "precinct_total") |>
      mutate(
        precinct_total = as.numeric(precinct_total),
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
        raw_file_path = glue("{PATH_DROPBOX}/24_general/{state}/raw/{county}_{version}.zip"))
    
    download_file <- function(url, version) {
      tryCatch(
        # Send the request to download the file
        request(url) |> 
          req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
          req_retry(max_tries = 5) |> 
          # Define the path with the `version` suffix for each file
          req_perform(
            path = str_c(
              glue("{PATH_DROPBOX}/24_general/{state}/raw/"),
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
      csv_downloaded = file.exists(glue("{PATH_DROPBOX}/24_general/{state}/raw/{county}_{version}.csv")))
    
    source_python("scripts/util/clarity_scraper.py")
    
    counties |> filter(!csv_downloaded) |> pull(raw_file_path) |> walk(.f = \(x) get_data_clarity(state, x, PATH_DROPBOX))
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
        raw_file_path = glue('{PATH_DROPBOX}/24_general/{state}/raw/{county}_{value}.zip'))
    
    download_file <- function(url, version) {
      tryCatch(
        # Send the request to download the file
        request(url) |> 
          req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
          req_retry(max_tries = 5) |> 
          # Define the path with the `version` suffix for each file
          req_perform(
            path = str_c(
              glue("{PATH_DROPBOX}/24_general/{state}/raw/"),
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
      csv_downloaded = file.exists(glue("{county}_{value}.csv")))
    
    source_python("scripts/util/clarity_scraper.py")
    
    version_files |> filter(!csv_downloaded) |> pull(raw_file_path) |> walk(.f = \(x) get_data_clarity(state, x, PATH_DROPBOX))
  }
  
}
