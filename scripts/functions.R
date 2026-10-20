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

# ========================================
## DASHBOARD
# ========================================

# Per-run summary for one jurisdiction. Writes the candidate x method totals for
# every contest to {state}/summaries/ (these accumulate into the dashboard time
# series) and returns the precinct-level table for the latest run.
make_summary <- function(data, state, county) {
  key = if (is.na(county)) state else glue("{state}_{county}")
  # upstream scrape errored (NULL via error = "null") or site posted nothing yet
  if (is.null(data) || nrow(data) == 0) return(NULL)

  d = as_tibble(data) |>
    mutate(
      across(c(jurisdiction, contest, candidate, party, method, geo), as.character),
      stage = suppressWarnings(as.integer(as.character(stage)))
    )

  time = max(d$time, na.rm = TRUE)

  summ = d |>
    summarise(votes = sum(votes, na.rm = TRUE), .by = c(contest, district, candidate, party, method)) |>
    mutate(key = key, time = time)

  dir = fs::path(PATH_DROPBOX, ELECTION_FOLDER, state, "summaries")
  fs::dir_create(dir)
  nanoparquet::write_parquet(summ, fs::path(dir, glue("summary_{key}_{format(time, '%Y_%m_%d_%H_%M_%S')}.parquet")))

  prec = d |>
    summarise(
      votes = sum(votes, na.rm = TRUE),
      stage = suppressWarnings(max(stage, na.rm = TRUE)),
      .by = c(jurisdiction, contest, district, geo, method, candidate, party)
    ) |>
    mutate(stage = if_else(is.finite(stage), stage, NA_integer_))

  list(key = key, state = state, time = time, prec = prec)
}

# "carter, buddy" -> "Buddy Carter"
clean_cand_name <- function(x) {
  x = if_else(str_detect(x, ","), str_replace(x, "^(.*?),\\s*(.*)$", "\\2 \\1"), x)
  str_to_title(str_squish(x))
}

read_history <- function(state) {
  path = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "history", glue("{state}_history.parquet"))
  if (!fs::file_exists(path)) return(NULL)
  h = nanoparquet::read_parquet(path) |> as_tibble()
  if (!"method" %in% names(h)) h$method = NA_character_
  if (!"district" %in% names(h)) h$district = NA_integer_
  if (!"county" %in% names(h)) h$county = NA_character_
  mutate(h, across(c(geo, method, contest, party, county), as.character), district = as.integer(district))
}

read_summaries <- function(state, keys) {
  files = fs::dir_ls(fs::path(PATH_DROPBOX, ELECTION_FOLDER, state, "summaries"), regexp = "summary_.*\\.parquet$", fail = FALSE)
  if (length(files) == 0) return(NULL)
  map(files, nanoparquet::read_parquet) |>
    bind_rows() |>
    filter(key %in% keys) |>
    distinct()
}

# L2 precinct shares for the precinct explorer, keyed on ENR geo through
# input_data/crosswalks/{st}.csv (docs/precinct_crosswalks.md). Variables are the
# frame's (yougov_precinct_frame.parquet) that L2 has. Long by mode: "Total" plus
# eday/early/mail, where turnout uses that mode's vote columns.
read_l2 <- function(state) {
  dir = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "input_data")
  xw_path = fs::path(dir, "crosswalks", glue("{tolower(state)}.csv"))
  l2_path = fs::dir_ls(fs::path(dir, "l2"), regexp = glue("/{tolower(state)}_\\d{{8}}_precincts\\.parquet$"), fail = FALSE) |>
    sort() |>
    tail(1)
  if (!fs::file_exists(xw_path) || length(l2_path) == 0) return(NULL)

  # cbs_counties spells MIAMIDADE; ENR geo carries the cbs county number, not FIPS
  norm = \(x) str_remove_all(toupper(x), "[^A-Z]")
  cty = enightmodels::cbs_counties |>
    filter(.data$state == .env$state) |>
    transmute(cty = norm(county), county_cbs)
  xw = readr::read_csv(xw_path, col_types = readr::cols(.default = "c")) |>
    filter(!is.na(precinct_enr), !is.na(precinct_l2)) |>
    distinct(county, precinct_enr, precinct_l2) |> # frame rows repeat per cd
    mutate(cty = norm(county)) |>
    inner_join(cty, by = "cty") |>
    mutate(geo = tolower(sprintf("%s_%03d_%s", state, county_cbs, precinct_enr))) |>
    select(geo, county, precinct_l2)

  l2 = nanoparquet::read_parquet(l2_path) |>
    as_tibble() |>
    filter(n > 0) |>
    rename(precinct_l2 = precinct)
  base = transmute(l2, county, precinct_l2,
    l2_n = n,
    l2_male = male / n,
    l2_white = race1 / (n - race_unk),
    l2_black = race2 / (n - race_unk),
    l2_hispanic = race3 / (n - race_unk),
    l2_asian = race4 / (n - race_unk)
  )
  turnout = l2 |>
    select(county, precinct_l2, n, matches("^vote(20|22|24)(_(eday|early|mail))?$")) |>
    pivot_longer(starts_with("vote"), names_to = c("year", "mode"), names_pattern = "^vote(\\d+)_?(.*)$") |>
    mutate(mode = if_else(mode == "", "Total", mode), name = paste0("l2_turnout", year), value = value / n) |>
    select(county, precinct_l2, mode, name, value) |>
    pivot_wider()

  xw |>
    inner_join(base, by = c("county", "precinct_l2")) |>
    inner_join(turnout, by = c("county", "precinct_l2"), relationship = "one-to-many") |>
    select(geo, mode, starts_with("l2_")) |>
    mutate(across(starts_with("l2_"), \(x) if_else(is.finite(x), x, NA_real_)))
}

# Build everything index.qmd needs for each row of `contests`.
# `dash` is a list of make_summary() outputs (one per jurisdiction).
build_dashboard_data <- function(dash, contests = DASHBOARD_CONTESTS) {
  contests = contests |>
    mutate(
      key = if_else(is.na(district), paste0(state, "-", contest), paste0(state, "-", contest, "-", district)),
      label = coalesce(label, if_else(is.na(district), paste(state, contest), paste0(state, " ", contest, "-", district))),
      hist_contest = coalesce(hist_contest, contest)
    )

  # drop jurisdictions whose scrape/summary failed; show whatever is left
  dash = purrr::compact(dash)
  prec_all = map(dash, \(x) mutate(x$prec, state = x$state, src = x$key, src_time = x$time)) |>
    unname() |>
    bind_rows(tibble(
      jurisdiction = character(), contest = character(), district = integer(), geo = character(),
      method = character(), candidate = character(), party = character(), votes = numeric(),
      stage = integer(), state = character(), src = character(), src_time = as.POSIXct(character())
    ))
  keys_by_state = split(map_chr(dash, "key"), map_chr(dash, "state"))

  states = unique(contests$state)
  summaries = map(states, \(s) read_summaries(s, keys_by_state[[s]])) |> set_names(states)
  histories = map(states, read_history) |> set_names(states)
  l2s = map(states, read_l2) |> set_names(states)

  out = pmap(contests, \(state, contest, district, key, label, hist_contest, ...) {
    p = filter(prec_all, .env$state == .data$state, .env$contest == .data$contest)
    # statewide rows sometimes carry a dummy district (e.g. NC sen = 1): accept it if it's the only one
    dist = if (is.na(district) && !anyNA(p$district) && n_distinct(p$district) == 1) unique(p$district) else district
    p = filter(p, .data$district %in% dist)
    h = histories[[state]]
    if (!is.null(h)) {
      h = filter(h, contest == hist_contest, district %in% c(district, dist))
      if (nrow(h) == 0) h = NULL else h = filter(h, year == max(year))
    }

    lead = p |>
      filter(party %in% c("dem", "rep")) |>
      summarise(votes = sum(votes), .by = c(candidate, party)) |>
      slice_max(votes, n = 1, by = party, with_ties = FALSE)
    dem_cand = lead$candidate[lead$party == "dem"]
    rep_cand = lead$candidate[lead$party == "rep"]
    if (length(dem_cand) == 0) dem_cand = NA_character_
    if (length(rep_cand) == 0) rep_cand = NA_character_

    prec = p |>
      summarise(
        dem = sum(votes[party == "dem" & candidate == dem_cand]),
        rep = sum(votes[party == "rep" & candidate == rep_cand]),
        total = sum(votes),
        stage = suppressWarnings(max(stage, na.rm = TRUE)),
        .by = c(src, geo, method)
      ) |>
      mutate(stage = if_else(is.finite(stage), stage, NA_integer_))

    hist_eday = NA_real_
    if (!is.null(h)) {
      hwide = h |>
        summarise(
          hist_dem = sum(votes[party == "dem"]),
          hist_rep = sum(votes[party == "rep"]),
          hist_total = sum(votes),
          .by = c(geo, method, county)
        )
      if (all(is.na(hwide$method))) {
        prec = left_join(prec, select(hwide, -method), by = "geo")
      } else {
        prec = left_join(prec, hwide, by = c("geo", "method"))
        if ("eday" %in% hwide$method) hist_eday = sum(hwide$hist_total[hwide$method == "eday"], na.rm = TRUE)
      }
      prec = mutate(prec, county = coalesce(county, src))
    } else {
      prec = mutate(prec, county = src, hist_dem = NA_real_, hist_rep = NA_real_, hist_total = NA_real_)
    }

    geo_stage = summarise(prec, stage = suppressWarnings(max(stage, na.rm = TRUE)), .by = geo)

    # crosswalk geos are lowercased; ENR geos vary in case (prefix and precinct)
    l2 = tibble(geo = character(), mode = character())
    if (!is.null(l2s[[state]])) {
      l2 = tibble(geo = geo_stage$geo, g = tolower(geo_stage$geo)) |>
        inner_join(l2s[[state]], by = c(g = "geo")) |>
        select(-g)
    }

    meta = tibble(
      key = key, label = label, state = state, contest = contest, district = district,
      time = if (nrow(p) > 0) max(p$src_time) else NA,
      n_juris = n_distinct(p$src),
      juris = paste(sort(unique(p$src)), collapse = ", "),
      n_geo = nrow(geo_stage),
      n_complete = sum(geo_stage$stage >= 6, na.rm = TRUE),
      n_partial = sum(geo_stage$stage %in% 1:5),
      total = sum(prec$total),
      eday = if ("eday" %in% prec$method) sum(prec$total[prec$method == "eday"]) else NA_real_,
      dem_name = clean_cand_name(dem_cand),
      dem_votes = sum(prec$dem),
      rep_name = clean_cand_name(rep_cand),
      rep_votes = sum(prec$rep),
      has_hist = !is.null(h),
      hist_by_method = !is.null(h) && !all(is.na(h$method)),
      hist_year = if (is.null(h)) NA_integer_ else as.integer(max(h$year)),
      hist_total = if (is.null(h)) NA_real_ else sum(h$votes),
      hist_eday = hist_eday,
      # share of ENR precincts with an L2 match (0 when the state has no crosswalk)
      l2_match = if (nrow(geo_stage) > 0) n_distinct(l2$geo) / nrow(geo_stage) else NA_real_
    )

    s = summaries[[state]]
    ts = tibble(time = as.POSIXct(character()), method = character(), total = numeric(), dem = numeric(), rep = numeric())
    if (!is.null(s)) {
      s = filter(s, .env$contest == .data$contest, .data$district %in% dist)
      if (nrow(s) > 0) {
        by_key = s |>
          summarise(
            dem = sum(votes[party == "dem" & candidate == dem_cand]),
            rep = sum(votes[party == "rep" & candidate == rep_cand]),
            total = sum(votes),
            .by = c(key, time, method)
          )
        # carry each jurisdiction's latest run forward to every timestamp
        ts = by_key |>
          complete(key, method, time = sort(unique(by_key$time))) |>
          arrange(time) |>
          group_by(key, method) |>
          fill(dem, rep, total) |>
          ungroup() |>
          mutate(across(c(dem, rep, total), \(x) coalesce(x, 0))) |>
          summarise(across(c(dem, rep, total), sum), .by = c(time, method))
      }
    }

    list(
      meta = meta,
      ts = mutate(ts, key = key, .before = 1),
      prec = mutate(prec, key = key, .before = 1),
      l2 = mutate(l2, key = key, .before = 1)
    )
  })

  list(
    meta = map(out, "meta") |> bind_rows(),
    ts = map(out, "ts") |> bind_rows(),
    prec = map(out, "prec") |> bind_rows(),
    l2 = map(out, "l2") |> bind_rows(),
    title = DASHBOARD_TITLE
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
