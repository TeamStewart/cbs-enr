source("scripts/util/utils.R")
source("scripts/util/globals.R")
source("scripts/models.R")
source("scripts/plotters.R")

get_timestamp <- function(state, county, path) {
  f = getFromNamespace(paste(tolower(state), "timestamp", sep="_"), ns = "enightdata")
  f()
}

get_data <- function(state, county, timestamp, path = NULL) {
  f = getFromNamespace(paste("scrape", tolower(state), sep = "_"), ns = "enightdata")
  d = f(state, county, path, timestamp)

  # Upstream fix to precinct_total
  d$precinct_total <- as.numeric(d$precinct_total)

  local_file_name <- ifelse(is.na(county), state, glue("{state}_{county}"))

  write_local(d, glue("{state}/clean/{local_file_name}_latest.csv"))
  write_local(d, glue("{state}/clean/{local_file_name}_{timestamp}.csv"))

  return(d)
}

get_history <- function(state, impute, impute_group=NULL) {
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

# TODO: update for 2026 and on
make_summary <- function(data, state, county, timestamp, history){
  merge_data(
    data = data,
    history = history,
    office = c("Governor", "Attorney General", "Lt Governor"),
    impute = FALSE
  ) |> 
    summarize(
      across(c(turnout, matches("votes_.*?25_.*?precinct_total$")), ~ sum(.x, na.rm=TRUE)),
      .by = c(timestamp, vote_mode) 
    ) |> 
    mutate(
      across(matches("votes_.*?25_.*?precinct_total$"), ~ .x / turnout, .names = "{str_replace(.col, '_precinct_total$', '_share')}"),
    ) |> 
    write_csv(
      glue("{PATH_DROPBOX}/{ELECTION_FOLDER}/{state}/summaries/summarylive_{state}_{county}_{timestamp}.csv")
    )
}
