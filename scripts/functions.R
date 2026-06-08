source("scripts/util/utils.R")
source("scripts/util/globals.R")
source("scripts/models.R")
source("scripts/plotters.R")

get_timestamp <- function(state, county, path) {

  fn_name <- paste(tolower(state), "timestamp", sep = "_")

  rlang::exec(
    get(fn_name),
    state = state,
    county = county,
    path = path
  )
}

get_data <- function(state, county, timestamp, path = NULL, trust_stage = FALSE, multi = FALSE) {

  fn_name <- paste("scrape", tolower(state), sep = "_")

  d = rlang::exec(
    get(fn_name),
    state = state,
    county = county,
    path = path,
    timestamp = timestamp
  ) |> 
    mutate(
      stage = case_when(
        .default = NA,
        stage == 6 & isFALSE(trust_stage) ~ 5,
        !is.na(stage) ~ stage
      )
    ) |> 
    enightmodels::coerce_elec_data()

  local_file_name <- ifelse(is.na(county), state, glue("{state}_{county}"))

  if (multi) {
    party = d |> drop_na(party) |> distinct(party) |> pull()
    local_file_name <- glue("{local_file_name}_{party}")
  }

  write_local(d, glue("{state}/clean/{local_file_name}_latest.csv"))
  write_local(d, glue("{state}/clean/{local_file_name}_{timestamp}.csv"))

  return(d)
}

get_history <- function(state, impute, county=NULL, impute_group=NULL) {
  l2 = fread(
    file = list.files(
      path = glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/input_data/{state}"),
      pattern = glue("{str_to_lower(state)}_{str_sub(ELECTION_DATE, 1, 4)}.*?.csv"),
      full.names = TRUE
    ) |>
      sort() |>
      pluck(-1)
  ) |>
    select(-starts_with("precinct_l2"), -matches("^ad$|^ed$"), -matches("white|black|hispanic|asian|other"))

  l2 = enightmodels::rename_demo_cbs(l2)

  base <- merge(
    fread(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/history/{state}_history.csv")),
    l2,
    by = c("county", "fips", "precinct_cbs"),
    all.x = TRUE
  )[, !c("ad", "ed")][,
    votes_turnout_21 := max(fifelse(vote_mode == "Total", votes_precFinal_21, NA_real_), na.rm = TRUE),
    by = .(county, precinct_25)
  ]

  if (impute) {
    base = impute_missing(base, impute_group, covars = select(base, where(is.numeric)) |> colnames())
  }

  return(base)
}

upload_html <- function(path, state) {
  write_s3(
    path_local = path,
    path_s3 = glue("{ELECTION_DATE}-{state}-G/model_{state}.html"),
    multipart = TRUE
  )
}

make_summary <- function(data, state, county, timestamp, history){
  merge_data(
    data = data,
    history = history,
    office = c("Governor", "Attorney General", "Lt Governor"),
    impute = FALSE
  ) |> 
    summarize(
      across(c(turnout, matches("votes_.*?2\\d_.*?precinct_total$")), ~ sum(.x, na.rm=TRUE)),
      .by = c(timestamp, vote_mode) 
    ) |> 
    mutate(
      across(matches("votes_.*?2\\d_.*?precinct_total$"), ~ .x / turnout, .names = "{str_replace(.col, '_precinct_total$', '_share')}"),
    ) |> 
    write_csv(
      glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/summaries/summarylive_{state}_{county}_{timestamp}.csv")
    )
}

parse_ca_datetime <- function(x, year = 2026L) {
  m         <- stringr::str_match(x, "^(\\w{3}) (\\d+):(\\d{2}) (a\\.m\\.|p\\.m\\.)$")
  month_str <- m[, 2]
  day_hour  <- m[, 3]
  mins      <- as.integer(m[, 4])
  ampm      <- m[, 5]

  n      <- nchar(day_hour)
  hour_2 <- as.integer(substr(day_hour, n - 1L, n))
  hour_1 <- as.integer(substr(day_hour, n, n))
  use_2  <- n >= 2L & !is.na(hour_2) & hour_2 >= 1L & hour_2 <= 12L

  hour <- ifelse(use_2, hour_2, hour_1)
  day  <- as.integer(ifelse(use_2,
                            substr(day_hour, 1L, n - 2L),
                            substr(day_hour, 1L, n - 1L)))

  hour <- ifelse(!is.na(ampm) & ampm == "p.m." & !is.na(hour) & hour != 12L, hour + 12L,
           ifelse(!is.na(ampm) & ampm == "a.m." & !is.na(hour) & hour == 12L, 0L, hour))

  month_num <- match(month_str, month.abb)
  lubridate::ymd_hms(sprintf("%04d-%02d-%02d %02d:%02d:00",
                             year, month_num, day, hour, mins))
}

get_la_totals <- function(timestamp) {
  path    <- "https://results.lavote.gov/ElectionResults/GetElectionData?electionID=4338"
  out_dir <- fs::path(PATH_DROPBOX, ELECTION_FOLDER, "CA", "clean", "Los Angeles")
  fs::dir_create(out_dir)

  data <- enightdata::scrape_ca("CA", "los angeles", path, timestamp)

  readr::write_csv(data, fs::path(out_dir, glue("CA_los angeles_{timestamp}.csv")))
  readr::write_csv(data, fs::path(out_dir, "CA_los angeles_latest.csv"))

  data
}

get_castatewide <- function() {

  time = Sys.time() |> format(format = "%Y-%m-%d %H:%M:%S") |> str_replace_all("-|:| ", "_")

  rvest::read_html("https://dp.electionresults.sos.ca.gov/returns/status") |>
    rvest::html_table() |>
    pluck(1) |>
    rename_with(.fn = ~ str_remove_all(.x, "\\*") |> str_squish()) |>
    janitor::clean_names() |>
    filter(!(county_name %in% c("Statewide", "County Name"))) |>
    mutate(
      across(c(total_precincts:percent_turnout), ~ str_remove_all(.x, ",|%") |> as.numeric()),
      across(starts_with("percent_"), ~ .x / 100),
      across(c(first_report_date_time, last_report_date_time), parse_ca_datetime)
    ) |>
    write_csv(glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/CA/clean/statewide/ca_statewide_{time}.csv"))
}
