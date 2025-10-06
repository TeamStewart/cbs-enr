## New York City (labeled as New York but get over it)
scrape_ny <- function(state, county, path, timestamp){
  
  # start with the "Total" page
  base = read_html(path)
  
  links = tibble(
    link = base |> html_elements("a") |> html_attr("href"),
    ad = base |> html_elements("a") |> html_attr("title")
  ) |> 
    filter(str_detect(ad, "^AD \\d+$")) |> 
    mutate(
      link = glue("https://enr.boenyc.gov/{link}")
    )
  
  get_ad <- function(link){
    
    t = read_html(link) |> 
      html_element(".underline") |> 
      html_table(head=TRUE) |> 
      janitor::clean_names(case = "title")
    
    t[-1,]
    
  }
  
  county_to_ad <- tribble(
    ~jurisdiction,     ~ad,
    "New York",       37,
    "New York",       61,
    "New York",       65,
    "New York",       66,
    "New York",       67,
    "New York",       68,
    "New York",       69,
    "New York",       70,
    "New York",       71,
    "New York",       72,
    "New York",       73,
    "New York",       74,
    "New York",       75,
    "New York",       76,
    "Bronx",           77,
    "Bronx",           78,
    "Bronx",           79,
    "Bronx",           80,
    "Bronx",           81,
    "Bronx",           82,
    "Bronx",           83,
    "Bronx",           84,
    "Bronx",           85,
    "Bronx",           86,
    "Bronx",           87,
    "Kings",        41,
    "Kings",        42,
    "Kings",        43,
    "Kings",        44,
    "Kings",        45,
    "Kings",        46,
    "Kings",        47,
    "Kings",        48,
    "Kings",        49,
    "Kings",        50,
    "Kings",        51,
    "Kings",        52,
    "Kings",        53,
    "Kings",        54,
    "Kings",        55,
    "Kings",        56,
    "Kings",        57,
    "Kings",        58,
    "Kings",        59,
    "Kings",        60,
    "Kings",        61,
    "Kings",        64,
    "Queens",          23,
    "Queens",          24,
    "Queens",          25,
    "Queens",          26,
    "Queens",          27,
    "Queens",          28,
    "Queens",          29,
    "Queens",          30,
    "Queens",          31,
    "Queens",          32,
    "Queens",          33,
    "Queens",          34,
    "Queens",          35,
    "Queens",          36,
    "Queens",          37,
    "Queens",          38,
    "Queens",          39,
    "Queens",          40,
    "Richmond",   61,
    "Richmond",   62,
    "Richmond",   63,
    "Richmond",   64
  )
  
  
  links |> 
    mutate(
      results = map(link, get_ad)
    ) |> 
    select(ad, results) |> 
    unnest_longer(results) |> 
    unnest_wider(results) |> 
    rename(
      precinct = X,
      pct_reported = X_2
    ) |> 
    mutate(
      pct_reported = str_remove(pct_reported, "%") |> as.numeric() / 100,
      precinct = str_squish(precinct),
    ) |> 
    select(-matches("^X_\\d+$")) |> 
    pivot_longer(
      cols = -c(ad, precinct, pct_reported), 
      names_to = "candidate_name", 
      values_to = "precinct_total"
    ) |> 
    mutate(
      candidate_name = case_when(
        .default = candidate_name,
        str_detect(candidate_name, regex("Mamdani", TRUE)) ~ "Zohran Kwame Mamdani",
        str_detect(candidate_name, regex("Cuomo", TRUE)) ~ "Andrew Cuomo",
        str_detect(candidate_name, regex("Adams", TRUE)) ~ "Eric Adams",
        str_detect(candidate_name, regex("Walden", TRUE)) ~ "Jim Walden",
        str_detect(candidate_name, regex("Sliwa", TRUE)) ~ "Curtis Sliwa",
      ),
      candidate_party = case_match(
        candidate_name, 
        "Zohran Kwame Mamdani" ~ "Democrat",
        "Andrew Cuomo" ~ "Independent",
        "Eric Adams" ~ "Independent",
        "Jim Walden" ~ "Independent",
        "Curtis Sliwa" ~ "Republican",
      ),
      state = "NY",
      office = "Mayor",
      vote_mode = "Total"
    ) |> 
    left_join(
      mutate(county_to_ad, ad=paste("AD ", ad)),
      join_by(ad)
    )
  
}

## Virginia
scrape_va <- function(state, county, path, timestamp){
  
  # Download the raw json
  raw_file_path = glue('{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw/{state}_{timestamp}.json')
  download.file(path, destfile = raw_file_path)

  base = tibble(data = read_json(raw_file_path) |> pluck('localResults')) |> 
    hoist(
      data,
      jurisdiction = "name",
      items = "ballotItems"
    ) |> 
    select(-data) |> 
    unnest_longer(items)
  
  base |>
    hoist(
      items, 
      race_id = "id",
      race_name = "name",
      options = "ballotOptions",
      ballotOrder = "ballotOrder"
    ) |>
    filter(str_detect(race_name, regex("Governor|Gov", ignore_case=TRUE))) |> 
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
      timestamp = .env$timestamp |> ymd_hms(),
      state = .env$state,
      jurisdiction = jurisdiction |> str_remove(regex("County", ignore_case=TRUE)) |> str_squish() |> str_to_title(),
      race_name = case_when(
        str_detect(race_name, regex("Governor|Gov", ignore_case=TRUE)) ~ "Governor",
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
        # candidate_name %in% c("Chase Oliver (Lib)", "Chase R. Oliver", "CHASE OLIVER / MIKE TER MAAT") ~ "Chase Oliver",
        # candidate_name %in% c("Donald J. Trump (Rep)", "DONALD J. TRUMP / JD VANCE", "Donald J. Trump") ~ "Donald Trump",
        # candidate_name %in% c("Jill Stein (Grn)", "Jill E. Stein", "JILL STEIN / RUDOLPH WARE") ~ "Jill Stein",
        # candidate_name %in% c("Kamala D. Harris (Dem)", "KAMALA D. HARRIS / TIM WALZ", "Kamala D. Harris") ~ "Kamala Harris",
        # candidate_name %in% c("Claudia De la Cruz (Ind)", "CLAUDIA DE LA CRUZ / KARINA ALEXANDRA GARCIA") ~ "Claudia De la Cruz",
        # candidate_name %in% c("Cornel West (Ind)", "CORNEL RONALD WEST / MELINA ABDULLAH", "Cornel R. West") ~ "Cornel West",
        candidate_name == "Write-in" ~ "Write-ins",
        .default = candidate_name
      ),
      vote_mode = case_when(
        str_detect(vote_mode, regex("Election Day", ignore_case = TRUE)) ~ "Election Day",
        str_detect(vote_mode, regex("Early Voting|Advanced Voting", ignore_case = TRUE)) ~ "Early Voting",
        str_detect(vote_mode, regex("Mail|Absentee", ignore_case = TRUE)) ~ "Absentee/Mail",
        str_detect(vote_mode, regex("Provisional", ignore_case = TRUE)) ~ "Provisional",
        .default = str_to_title(vote_mode)
      )
    ) |>
    select(
      state, race_id, race_name, candidate_name, candidate_party,
      jurisdiction, precinct_id, virtual_precinct, timestamp, 
      vote_mode, precinct_total
    ) |>
    arrange(race_name, candidate_party, candidate_name, jurisdiction, precinct_id)
  
  
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
        raw_file_path = glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw/{county}_{version}.zip"),
        downloaded = file_exists(raw_file_path) & file_size(raw_file_path) > 3000
      )
    
    download_file <- function(url, version) {
      tryCatch(
        # Send the request to download the file
        request(url) |> 
          req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
          req_retry(max_tries = 5) |> 
          # Define the path with the `version` suffix for each file
          req_perform(
            path = str_c(
              glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw/"),
              str_extract(url, glue("({state}/)(.*?)(/)"), group = 2),
              "_", version, ".zip")),
        # Handle 404 error silently
        httr2_http_404 = function(cnd) NULL
      )
    }
    
    counties |> 
      filter(!downloaded) |> 
      mutate(out = walk2(url, version, download_file))
    
    # Apply the function using map2 on url and version columns from `counties`
    map2(counties$url, counties$version, download_file)
    
    # Check which versions already downloaded, omit from the list to scrape
    counties <- counties |> mutate(
      state = state,
      update_csv = !file_exists(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw/{county}_{version}.csv")) & file_size(raw_file_path) > 3000
    )
    
    source_python("scripts/util/clarity_scraper.py")
    
    counties |> filter(update_csv) |> pull(raw_file_path) |> walk(.f = \(x) get_data_clarity(state, x, PATH_DROPBOX))
    
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
        raw_file_path = glue('{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw/{county}_{value}.zip'),
        downloaded = file_exists(raw_file_path) & file_size(raw_file_path) > 3000
      )
    
    download_file <- function(url, version) {
      tryCatch(
        # Send the request to download the file
        request(url) |> 
          req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
          req_retry(max_tries = 5) |> 
          # Define the path with the `version` suffix for each file
          req_perform(
            path = str_c(
              glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw/"),
              str_extract(url, glue("({state}/)(.*?)(/)"), group = 2),
              "_", version, ".zip")),
        # Handle 404 error silently
        httr2_http_404 = function(cnd) NULL
      )
    }
    
    version_files |> 
      filter(!downloaded) |> 
      mutate(out = walk2(url, value, download_file))
    
    # Check which versions already downloaded, omit from the list to scrape
    version_files <- version_files |> mutate(
      state = state,
      update_csv = !file_exists(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw/{county}_{value}.csv")) & file_size(raw_file_path) > 3000
    )
    
    source_python("scripts/util/clarity_scraper.py")
    
    version_files |> filter(update_csv) |> pull(raw_file_path) |> walk(.f = \(x) get_data_clarity(state, x, PATH_DROPBOX))
  }
  
}
