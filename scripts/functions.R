source("scripts/util/utils.R")
source("scripts/util/globals.R")
source("scripts/scrapers.R")
source("scripts/models.R")
source("scripts/plotters.R")

get_timestamp <- function(state, county, path) {
  ny_timestamp <- function() {
    read_html(path) |>
      html_text() |>
      str_extract("Information As Of: (.*?)AD:", group = 1) |>
      ymd_hms(tz = "America/New_York") |>
      str_replace_all("-|:| ", "_")
  }

  va_timestamp <- function() {
    read_html("https://enr.elections.virginia.gov/results/public/api/elections/Virginia/2025-November-General") |>
      html_text() |>
      fromJSON() |>
      pluck("lastUpdated") |>
      ymd_hms() |>
      floor_date(unit = "second") |>
      with_tz(tzone = "America/New_York") |>
      str_replace_all("-|:| ", "_")
  }

  f = get(paste0(tolower(state), "_timestamp"))

  f()
}

get_data <- function(state, county, timestamp, path = NULL) {
  dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/raw"))
  dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/clean"))

  d = switch(
    state,
    "VA" = scrape_va(state, county, path, timestamp),
    "NY" = scrape_ny(state, county, path, timestamp)
  )

  # f = get(paste0("scrape_", tolower(state)))
  # d = f(state, county, path, timestamp)

  # Upstream fix to precinct_total
  d$precinct_total <- as.numeric(d$precinct_total)

  local_file_name <- ifelse(is.na(county), state, glue("{state}_{county}"))

  # save latest version
  fwrite(d, glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/clean/{local_file_name}_latest.csv"))

  # save timestamped version
  fwrite(d, glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/clean/{local_file_name}_{timestamp}.csv"))

  return(d)
}

format_wide <- function(data, state, office) {
  if(state == 'VA'){prefix = "gov_25"}
  
  data |>
    filter(race_name == office) |>
    mutate(
      vote_mode = case_match(
        vote_mode,
        "Early Voting" ~ "early",
        "Election Day" ~ "eday",
        "Absentee/Mail" ~ "mail",
        "Provisional" ~ "provisional",
        .default = 'other'
      )
    ) |>
    pivot_wider(
      names_from = vote_mode,
      values_from = precinct_total,
      values_fill = 0,
      values_fn = sum
    ) |>
    mutate(
      pivot_column = word(candidate_name, -1) |> str_replace("-","_"),
      pivot_column = glue("{prefix}_{pivot_column}")) |>
    pivot_wider(
      id_cols = c(state, race_id, race_name, jurisdiction, precinct_id, virtual_precinct, timestamp),
      names_from = c(pivot_column),
      values_from = c(early, eday, mail, provisional),
      names_sep = "_"
    )
}

get_history <- function(state, impute = FALSE) {
  l2 = fread(
    file = list.files(
      path = glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/input_data/{state}"),
      pattern = glue("{str_to_lower(state)}_2025.*?.csv"),
      full.names = TRUE
    ) |>
      sort() |>
      pluck(-1)
  ) |>
    select(-starts_with("p_"), -starts_with("precinct_l2"), -matches("^ad$|^ed$"))

  base = left_join(
    read_csv(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/history/{state}_history.csv")),
    l2,
    join_by(county, fips, precinct_cbs)
  ) |> 
    select(-matches("^ad$|^ed$"))

  if (impute) {
    base = impute_missing(base)
  }

  return(base)
}

create_table_cbs <- function(data, state, county, timestamp, upload = FALSE) {
  # Prepare lookups
  lookup_state <- lookup_state_name(state)

  lookup_geo <- read_csv(
    glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/cbs_lookups/All States and Counties.csv"),
    col_types = cols(.default = "c")
  ) |>
    clean_names() |>
    rename(state_name = 1, jurisdiction = 5) |>
    filter(state_name == lookup_state) |>
    select(-state_fips)

  lookup_cands <- read_csv(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/cbs_lookups/2025-11-04 General Election Candidates.csv")) |>
    clean_names() |>
    filter(state == lookup_state) |>
    mutate(jurisdiction_code = as.character(jurisdiction_code))

  local_file_name <- ifelse(is.na(county), state, glue("{state}_{county}"))
  
  # Create wide version
  if(state == 'VA'){
    wide_data <- format_wide(data, state, 'Governor')
    write_csv(wide_data, glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/clean/{local_file_name}_latest_wide.csv"))
  }
  
  # Append CBS lookup info
  formatted <- data |>
    # For the purposes of CBS, remove undervote/overvotes
    filter(vote_mode != "Overvote/Undervote" & candidate_name != 'Other') |>
    mutate(
      jCde = "0",
      ofc = case_match(
        race_name,
        "President" ~ "P",
        "Senate" ~ "S",
        "Governor" ~ "G",
        "Mayor" ~ "M",
        "Attorney General" ~ "A",
        "Lt Governor" ~ "L",
        .default = NA_character_
      ),
      eDate = ELECTION_DATE,
      real_precinct = ifelse(virtual_precinct, "N", "Y"),
      jurisdiction = case_when(
        state == "VA" & str_detect(jurisdiction, "COUNTY$") ~ str_remove(jurisdiction, "COUNTY$") |> str_squish() |> str_to_title(),
        .default = jurisdiction
      )
    ) |>
    rename(office = race_name) |>
    left_join(lookup_geo, join_by(state == postal_code, jurisdiction == jurisdiction)) |>
    inner_join(lookup_cands, by = c("state_name" = "state", "office", "jCde" = "jurisdiction_code", "candidate_name" = "full_name")) |>
    mutate(
      jType = ifelse(jCde == "0", "SW", "CD"),
      pcntName = str_c(county_fips, precinct_id, sep = "_"),
      pcnt = pcntName,
      pcntUUID = pcntName
    ) |>
    pivot_wider(names_from = vote_mode, values_from = precinct_total) |>
    # Verify if we have all target vote_modes; if not, add NA column
    bind_rows(
      tibble(
        `Election Day` = numeric(),
        `Early Voting` = numeric(),
        `Absentee/Mail` = numeric(),
        `Provisional` = numeric(),
        `Total` = numeric()
      )
    ) |>
    rename(
      st = cbs_state_id,
      cnty = cbs_county_id,
      edayVote = `Election Day`,
      earlyInPersonVote = `Early Voting`,
      earlyByMailVote = `Absentee/Mail`,
      provisionalVote = Provisional,
      cName = candidate_name,
      eType = election_type_code,
      cId = candidate_id
    ) |>
    mutate(across(c(edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote, Total), as.integer)) |>
    mutate(
      cVote = rowSums(across(c(edayVote, earlyInPersonVote, earlyByMailVote, provisionalVote, Total), ~ replace_na(., 0))),
      ts = ymd_hms(timestamp) |> format("%Y-%m-%dT%H:%M:%SZ")
    ) |>
    select(
      eDate,
      jType,
      real_precinct,
      st,
      eType,
      jCde,
      ofc,
      cnty,
      pcnt,
      pcntUUID,
      pcntName,
      cId,
      cName,
      cVote,
      edayVote,
      earlyInPersonVote,
      earlyByMailVote,
      provisionalVote,
      ts
    ) |>
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
    upload_files <- function(state, office, local_file_name, formatted_data, long_data, wide_data = NULL, race_name, bucket) {
      # Define paths
      fs::dir_create("data/cbs_format")

      json_file <- glue("data/cbs_format/{local_file_name}_{office}_latest.json")
      csv_file_long <- glue("data/cbs_format/{local_file_name}_{office}_latest.csv")
      s3_json_path <- glue("Precincts/{ELECTION_DATE}-{state}-{office}/{local_file_name}_{office}_results.json")
      s3_csv_path_long <- glue("Precincts/{ELECTION_DATE}-{state}-{office}/{local_file_name}_{office}_results.csv")

      # Write JSON and upload
      formatted_data |> filter(ofc == office) |> write_json(json_file)
      put_object(file = json_file, object = s3_json_path, bucket = bucket, multipart = TRUE)

      # Write CSV and upload
      ## Long
      long_data |> filter(race_name == race_name) |> write_csv(csv_file_long)
      put_object(file = csv_file_long, object = s3_csv_path_long, bucket = bucket, multipart = TRUE)
      
      ## Wide
      if(!is_null(wide_data)){
        csv_file_wide <- glue("data/cbs_format/{local_file_name}_{office}_latest_wide.csv")
        s3_csv_path_wide <- glue("Precincts/{ELECTION_DATE}-{state}-{office}/{local_file_name}_{office}_results_wide.csv")
        
        wide_data |> write_csv(csv_file_wide)
        put_object(file = csv_file_wide, object = s3_csv_path_wide, bucket = bucket, multipart = TRUE)
      }
    }

    if (state == 'VA') {
      # Upload for Governor, AG, Lt Governor
      upload_files(state, "G", local_file_name, formatted, data, wide_data, "Governor", PATH_CBS_S3)
      upload_files(state, "A", local_file_name, formatted, data, NULL, "Attorney General", PATH_CBS_S3)
      upload_files(state, "L", local_file_name, formatted, data, NULL, "Lt Governor", PATH_CBS_S3)
      
      drive_put(
        media = glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/clean/{local_file_name}_latest_wide.csv"),
        path = PATH_GDRIVE,
        name = glue("{local_file_name}_results_wide.csv")
      )
      
    } else if (state == 'NY') {
      # Upload for Mayor
      upload_files(state, "M", local_file_name, formatted, data, NULL, "Mayor", PATH_CBS_S3)
    }

    #### Upload to Google Drive ####
    drive_put(
      media = "../CBS-MIT Election Data/25_general/input_data/NY/NY_test_file.csv",
      # media = glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/clean/{local_file_name}_latest.csv"),
      path = PATH_GDRIVE,
      name = glue("{local_file_name}_results.csv")
    )
  }

  return(formatted)
}

upload_html <- function() {
  upload_single <- function(path) {
    state = str_extract(path, "(pages/)(.*?)\\.html", group = 2) |> str_extract("^[^_]+")
    juris = str_extract(path, "(pages/)(.*?)\\.html", group = 2)

    # TODO: Change file from -P to something else?
    put_object(file = path, object = glue("{ELECTION_DATE}-{state}-P/model_{juris}.html"), bucket = PATH_CBS_S3, multipart = TRUE)
  }

  files = list.files(path = "pages", pattern = "*html$", full.names = TRUE, recursive = TRUE)
  files = files[basename(files) != "metadata.html"]

  file.copy(from = files, to = "docs/pages/", overwrite = FALSE, recursive = FALSE, copy.mode = TRUE) |> invisible()

  walk(files, upload_single)
}

make_summary <- function(data, state, county, timestamp, history){
  fs::dir_create(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/summaries"))

  merge_data(
    data = data,
    history = history,
    office = c("Governor", "Attorney General", "Lt Governor"),
    impute = FALSE
  ) |> 
    summarize(
      across(c(turnout, matches("votes_.*?25_.*?$")), ~ sum(.x, na.rm=TRUE)),
      .by = c(timestamp, vote_mode) 
    ) |> 
    write_csv(
      glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/summaries/{state}_{county}_{timestamp}_summary.csv")
    )
}