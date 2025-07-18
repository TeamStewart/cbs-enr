source("scripts/util/utils.R")
source("scripts/util/globals.R")
source("scripts/scrapers.R")
source("scripts/models.R")
source("scripts/plotters.R")

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
    if (county == "Maricopa") {
      read_html("https://elections.maricopa.gov/results-and-data/election-results.html#ElectionResultsSearch") |>
        html_element(":nth-child(8) small") |>
        html_text2() |>
        str_extract("\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} [AP]M") |>
        mdy_hms(tz = "America/Phoenix") |>
        with_tz(tzone = "America/New_York") |>
        str_replace_all("-|:| ", "_")
    } else if (county == "Pima") {
      now() |> format("%Y_%m_%d_%H_%M_%S", tz = "UTC")
    }
  }
  
  ga_timestamp <- function(){
    read_html("https://app.enhancedvoting.com/results/public/api/elections/Georgia/2024NovGen") |>
      html_text() |>
      fromJSON() |>
      pluck("lastUpdated") |>
      ymd_hms() |>
      floor_date(unit = "second") |>
      with_tz(tzone = "America/New_York") |>
      str_replace_all("-|:| ", "_")
  }
  
  mi_timestamp <- function(){
    if(county %in% c("Eaton", "Oakland", "Macomb")){
      clarity_timestamp()
    } else if(county == "Ingham"){
      read_html("https://app.enhancedvoting.com/results/public/api/elections/ingham-county-mi/general11052024") |>
        html_text() |>
        fromJSON() |>
        pluck("lastUpdated") |>
        ymd_hms() |>
        floor_date(unit = "second") |>
        with_tz(tzone = "America/New_York") |>
        str_replace_all("-|:| ", "_")
    }
  }

  nc_timestamp <- function() {
    read_xml("https://s3.amazonaws.com/dl.ncsbe.gov?delimiter=/&prefix=ENRS/2024_11_05/") |>
      xml2::as_list() |>
      pluck("ListBucketResult", function(x) x[names(x) == "Contents"]) |>
      map(
        ~ tibble(
          Key = pluck(.x, "Key", 1),
          LastModified = pluck(.x, "LastModified", 1)
        )
      ) |>
      list_rbind() |>
      filter(Key == "ENRS/2024_11_05/results_pct_20241105.zip") |>
      pull(LastModified) |>
      ymd_hms(tz = "America/New_York") |>
      str_replace_all("-|:| ", "_")
  }

  pa_timestamp <- function() {
    if (county %in% c("Allegheny","Delaware")) {
      clarity_timestamp()
    } else if (county == "Philadelphia") {
      read_html("https://philadelphiaresults.azurewebsites.us/ResultsExport.aspx?") |>
        html_element(".turnout-last-updated-time") |>
        html_text() |>
        mdy_hms(tz = "America/New_York") |>
        str_replace_all("-|:| ", "_")
    }
  }

  ny_timestamp <- function() {
    
    read_html(path) |> 
      html_text() |> 
      str_extract("Information As Of: (.*?)AD:", group=1) |> 
      ymd_hms(tz = "America/New_York")
    
  }
  
  switch(state,
    "AZ" = az_timestamp(),
    "GA" = ga_timestamp(),
    "MI" = mi_timestamp(),
    "NC" = nc_timestamp(),
    "PA" = pa_timestamp(),
    "NY" = ny_timestamp(),
  )
}

get_data <- function(state, county, timestamp, path = NULL) {
  
  dir_create(glue("{PATH_DROPBOX}/24_general/{state}/raw"))
  dir_create(glue("{PATH_DROPBOX}/24_general/{state}/clean"))
  
  d <- switch(state,
    "AZ" = scrape_az(state, county, path, timestamp),
    "GA" = scrape_ga(state, county, path, timestamp),
    "MI" = scrape_mi(state, county, path, timestamp),
    "NC" = scrape_nc(state, county, path, timestamp),
    "PA" = scrape_pa(state, county, path, timestamp),
    "NY" = scrape_ny(state, county, path, timestamp)
  )
 
  # Upstream fix to precinct_total
  d$precinct_total <- as.numeric(d$precinct_total)

  local_file_name <- ifelse(is.na(county), state, glue("{state}_{county}"))
  clarity_counties <- list(
    "MI" = c("Oakland", "Macomb", "Eaton"),
    "PA" = c("Delaware", "Allegheny")
  )
  if (state %in% names(clarity_counties) && county %in% clarity_counties[[state]]) {
    # save latest version; timestamped version already saved
    write_csv(d, glue("{PATH_DROPBOX}/24_general/{state}/clean/{local_file_name}_latest.csv"))
  } else{
    # save latest version
    write_csv(d, glue("{PATH_DROPBOX}/24_general/{state}/clean/{local_file_name}_latest.csv"))
    # save timestamped version
    write_csv(d, glue("{PATH_DROPBOX}/24_general/{state}/clean/{local_file_name}_{timestamp}.csv"))
  }

  return(d)
}

create_table_cbs <- function(data, state, county, timestamp, upload = FALSE) {
  # Prepare lookups
  lookup_state <- lookup_state_name(state)
  
  lookup_geo <- read_csv("data/input/cbs_lookups/All States and Counties.csv") |>
    clean_names() |>
    rename(state_name = 1, jurisdiction = 5) |>
    filter(state_name == lookup_state) |>
    select(-state_fips)

  lookup_cands <- read_csv("data/input/cbs_lookups/2024-11-05 General Election Candidates.csv") |>
    clean_names() |>
    filter(state == lookup_state) |>
    mutate(jurisdiction_code = as.character(jurisdiction_code))
  
  # Append CBS lookup info
  formatted <- data |>
    # For the purposes of CBS, remove undervote/overvotes
    filter(vote_mode != "Overvote/Undervote" & candidate_name!='Other') |> 
    mutate(
      jCde = "0",
      ofc = case_match(
        race_name,
        "President" ~ "P",
        "Senate" ~ "S",
        "Governor" ~ "G",
        .default = NA_character_)) |>
    rename(office = race_name) |>
    mutate(
      eDate = ELECTION_DATE,
      real_precinct = ifelse(virtual_precinct, "N", "Y")) |>
    left_join(lookup_geo, join_by(state == postal_code, jurisdiction == jurisdiction)) |>
    left_join(lookup_cands, by = c("state_name" = "state", "office", "jCde" = "jurisdiction_code", "candidate_name" = "full_name")) |>
    mutate(
      jType = ifelse(jCde == "0", "SW", "CD"),
      precinct_id = ifelse(jurisdiction == 'Philadelphia', precinct_id |> str_remove_all("-") |> str_squish(), precinct_id),
      pcntName = str_c(county_fips, precinct_id, sep = "_"),
      pcnt = pcntName,
      pcntUUID = pcntName) |>
    pivot_wider(names_from = vote_mode, values_from = precinct_total)
  
  # Verify if we have all target vote_modes; if not, add NA column
  target_vote_modes <- c("Election Day", "Early Voting", "Absentee/Mail", "Provisional")
  missing_cols <- setdiff(target_vote_modes, names(formatted))
  formatted[missing_cols] <- NA
   
  formatted <- formatted |>
    rename(
      st = cbs_state_id,
      cnty = cbs_county_id,
      edayVote = `Election Day`,
      earlyInPersonVote = `Early Voting`,
      earlyByMailVote = `Absentee/Mail`,
      provisionalVote = Provisional,
      cName = candidate_name,
      eType = election_type_code,
      cId = candidate_id) |>
    mutate(across(c(edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote), as.integer)) |> 
    mutate(
      cVote = rowSums(across(c(edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote), ~ replace_na(., 0))),
      ts = ymd_hms(timestamp) |> format("%Y-%m-%dT%H:%M:%SZ")) |>
    select(
      eDate, jType, real_precinct, st, eType, jCde, ofc, cnty, pcnt, pcntUUID, pcntName,
      cId, cName, cVote, edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote,ts) |>
    mutate(across(c(jCde, st, cnty, cId), as.numeric)) |>
    nest(candidates = c(cId, cName, cVote, edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote)) |>
    nest(key = c(eDate, st, eType, jType, jCde, ofc, cnty, pcnt, pcntUUID)) |>
    mutate(key2 = key) |>
    unnest(cols = key2) |>
    mutate(key = map(key, jsonlite::unbox)) |>
    select(eDate, jType, real_precinct, key, pcntName, candidates, jCde, ts, st, ofc, eType, pcntUUID, cnty)

  if (upload) {
    # Define the file name
    local_file_name <- ifelse(is.na(county), state, glue("{state}_{county}"))
    
    #### Upload to S3 ####
    # Function to handle upload by office type
    upload_files <- function(state, office, local_file_name, formatted_data, raw_data, race_name, bucket) {
      # Define paths
      json_file <- glue("data/cbs_format/{local_file_name}_{office}_latest.json")
      csv_file <- glue("data/cbs_format/{local_file_name}_{office}_latest.csv")
      s3_json_path <- glue("Precincts/{ELECTION_DATE}-{state}-{office}/{local_file_name}_{office}_results.json")
      s3_csv_path <- glue("Precincts/{ELECTION_DATE}-{state}-{office}/{local_file_name}_{office}_results.csv")
      
      # Write JSON and upload
      formatted_data |> filter(ofc == office) |> write_json(json_file)
      put_object(file = json_file, object = s3_json_path, bucket = bucket, multipart = TRUE)
      
      # Write CSV and upload
      raw_data |> filter(race_name == race_name) |> write_csv(csv_file)
      put_object(file = csv_file, object = s3_csv_path, bucket = bucket, multipart = TRUE)
    }
    
    # Conditional logic to determine which files to upload based on the state
    if (state %in% c('AZ', 'MI', 'PA')) {
      # Upload for President and Senate races
      upload_files(state, "P", local_file_name, formatted, data, "President", PATH_CBS_S3)
      upload_files(state, "S", local_file_name, formatted, data, "Senate", PATH_CBS_S3)
      
    } else if (state == 'NC') {
      # Upload for President and Governor races
      upload_files(state, "P", local_file_name, formatted, data, "President", PATH_CBS_S3)
      upload_files(state, "G", local_file_name, formatted, data, "Governor", PATH_CBS_S3)
      
    } else {
      # Upload for President race only
      upload_files(state, "P", local_file_name, formatted, data, "President", PATH_CBS_S3)
    }
    
    #### Upload to Google Drive ####
    drive_put(
      media = glue("{PATH_DROPBOX}/24_general/{state}/clean/{local_file_name}_latest.csv"),
      path = PATH_GDRIVE,
      name = glue("{local_file_name}_results.csv")
    )
  }

  return(formatted)
}

upload_html <- function(){
  
upload_single <- function(path){
    
    state = str_extract(path, "(pages/)(.*?)\\.html", group = 2) |> str_extract("^[^_]+")
    juris = str_extract(path, "(pages/)(.*?)\\.html", group = 2)
    
    put_object(file = path, object = glue("20241105-{state}-P/model_{juris}.html"), bucket = PATH_CBS_S3, multipart = TRUE)
  }
  
  files = list.files(path = "pages", pattern = "*html$", full.names = TRUE, recursive = TRUE)
  files = files[basename(files) != "metadata.html"]
  
  file.copy(from=files, to="docs/pages/", overwrite = FALSE, recursive = FALSE, copy.mode = TRUE) |> invisible()
  
  walk(files, upload_single)
}