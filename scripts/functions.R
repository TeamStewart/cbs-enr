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
  write_local(d, glue("{state}/clean/{local_file_name}_{timestamp}.csv"), csv = FALSE)

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

# model name -> model_base / model_spec / model_interval. Names are <base>[/<spec>][/<interval>]
# with default parts left off (scripts/model/live/R/config.R); model_config.csv has every part
# filled in. Names not in it are split on "/", and missing parts are "default".
model_parts <- function(models, config = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "model_config.csv")) {
  intervals = c("bootstrap", "credible", "conf_seq", "conformal", "none")
  parsed = tibble(model = unique(models)) |>
    mutate(
      parts = str_split(model, "/"),
      model_base = map_chr(parts, 1),
      model_interval = map_chr(parts, \(p) coalesce(intersect(p[-1], intervals)[1], "default")),
      model_spec = map_chr(parts, \(p) coalesce(setdiff(p[-1], intervals)[1], "default"))
    ) |>
    select(-parts)
  # spec_default / interval_default: the part is the base model's default (a bare name like "swing")
  if (!fs::file_exists(config)) {
    return(mutate(parsed, spec_default = model_spec == "default", interval_default = model_interval == "default"))
  }
  cfg = readr::read_csv(config, col_types = readr::cols(.default = "c"), progress = FALSE) |>
    distinct(model, model_base = base, model_spec = spec, model_interval = interval)
  defaults = filter(cfg, model == model_base) |> select(model_base, d_spec = model_spec, d_int = model_interval)
  bind_rows(semi_join(cfg, parsed, by = "model"), anti_join(parsed, cfg, by = "model")) |>
    left_join(defaults, by = "model_base") |>
    mutate(
      spec_default = coalesce(model_spec == d_spec, model_spec == "default"),
      interval_default = coalesce(model_interval == d_int, model_interval == "default")
    ) |>
    select(-d_spec, -d_int)
}

# enightmodels output in {ELECTION_FOLDER}/model_results: every predictions_*.csv run
# stacked into a time series (later files win on duplicate rows), and each
# race/model's most recent status_*.csv row; both with model_parts()
read_model_results <- function(dir = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "model_results")) {
  read_runs = \(prefix) {
    # "latest" sorts after the timestamped files, so it wins ties
    files = sort(fs::dir_ls(dir, regexp = glue("/{prefix}_[^/]*\\.csv$"), fail = FALSE))
    if (length(files) == 0) return(NULL)
    map(files, \(f) readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE)) |>
      bind_rows(.id = "file") |>
      mutate(file = match(file, files))
  }

  pred = read_runs("predictions")
  if (!is.null(pred)) {
    pred = pred |>
      mutate(
        time = lubridate::ymd_hms(time, quiet = TRUE),
        district = as.integer(as.numeric(district)),
        across(c(pct_rep, rep, pred, pred_min, pred_max), as.numeric)
      ) |>
      filter(!is.na(time)) |>
      arrange(desc(file)) |>
      distinct(race_id, state, contest, district, model, geo, pred_type, party, candidate, time, .keep_all = TRUE) |>
      select(race_id, state, contest, district, model, model_conf, geo, pred_type, party, candidate,
             time, pct_rep, rep, pred, pred_min, pred_max) |>
      arrange(time)
  }

  # race totals with their interval (CONTRACT.md section 15.4); runs from before the table
  # existed have none, and the dashboard falls back to summing the vote rows
  turnout = read_runs("turnout")
  if (!is.null(turnout)) {
    turnout = turnout |>
      mutate(
        time = lubridate::ymd_hms(time, quiet = TRUE),
        across(c(pct_rep, rep, pred, pred_min, pred_max), as.numeric)
      ) |>
      filter(!is.na(time)) |>
      arrange(desc(file)) |>
      distinct(race_id, model, geo, time, .keep_all = TRUE) |>
      select(race_id, model, geo, time, pred, pred_min, pred_max) |>
      arrange(time)
  }

  status = read_runs("status")
  if (!is.null(status)) {
    status = status |>
      arrange(desc(file)) |>
      distinct(race_id, model, .keep_all = TRUE) |>
      transmute(race_id, model, time = lubridate::ymd_hms(time, quiet = TRUE), status,
                statewide = cli::ansi_strip(statewide), settings = cli::ansi_strip(settings))
  }

  parts = model_parts(c(pred$model, status$model))
  if (!is.null(pred)) pred = left_join(pred, parts, by = "model")
  if (!is.null(status)) status = left_join(status, parts, by = "model")
  list(pred = pred, turnout = turnout, status = status, prec = read_model_precincts(dir))
}

# per-precinct projections (write_precincts() in scripts/model/live/R/output.R): the
# latest run only, from precincts_latest.parquet (else the newest stamped file)
read_model_precincts <- function(dir = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "model_results")) {
  f = fs::path(dir, "precincts_latest.parquet")
  if (!fs::file_exists(f)) f = tail(sort(fs::dir_ls(dir, regexp = "/precincts_\\d{8}_\\d{6}\\.parquet$", fail = FALSE)), 1)
  if (length(f) == 0) return(NULL)
  nanoparquet::read_parquet(f, col_select = c("race_id", "model", "time", "geo", "turn_est", "dem_est", "est_dem2")) |>
    as_tibble() |>
    filter(time == max(time), .by = c(race_id, model))
}

# ENR / model precinct id -> comparable key, as norm_live_geo() in scripts/model/live/R/returns.R
# (case, spacing, a trailing ".0", and leading zeros in the precinct part)
norm_geo <- function(geo) {
  parts = str_match(as.character(geo), "^([A-Za-z]{2}_\\d{3})_(.*)$")
  id = str_remove(str_remove(str_squish(toupper(parts[, 3])), "\\.0$"), "^0+(?=\\d)")
  if_else(is.na(parts[, 1]), toupper(as.character(geo)), paste0(tolower(parts[, 2]), "_", id))
}

# Models page, per precinct key (norm_geo): `unit_id`, the VoteHub unit for the maps, from
# scripts/model/lookups/model_precincts.csv and input_data/crosswalks/votehub/{st}.csv; and
# `frame`, whether the key is in the models' inputs with a past vote
# (input_data/model_inputs/{ST}_precinct.parquet). Both are built once in 26_general.
read_geo_units <- function(state) {
  dir = fs::path(PATH_DROPBOX, "26_general", "input_data")
  frame_path = fs::path(dir, "model_inputs", glue("{state}_precinct.parquet"))
  frame = tibble(g = character())
  if (fs::file_exists(frame_path)) {
    frame = nanoparquet::read_parquet(frame_path, col_select = c("geo", "dem2")) |>
      filter(!is.na(dem2)) |>
      transmute(g = norm_geo(geo))
  }
  mp = readr::read_csv("scripts/model/lookups/model_precincts.csv", col_types = readr::cols(.default = "c"),
                       col_select = c("state", "geo", "unit_id"), progress = FALSE) |>
    filter(.data$state == .env$state, !is.na(unit_id)) |>
    transmute(g = norm_geo(geo), unit_id)
  xw_path = fs::path(dir, "crosswalks", "votehub", glue("{tolower(state)}.csv"))
  xw = tibble(g = character(), unit_id = character())
  if (fs::file_exists(xw_path)) {
    norm = \(x) str_remove_all(toupper(x), "[^A-Z]")
    cty = enightmodels::cbs_counties |>
      filter(.data$state == .env$state) |>
      transmute(cty = norm(county), county_cbs)
    xw = readr::read_csv(xw_path, col_types = readr::cols(.default = "c"), progress = FALSE) |>
      filter(!is.na(unit_id), !is.na(precinct_enr)) |>
      mutate(cty = norm(county)) |>
      inner_join(cty, by = "cty") |>
      transmute(g = norm_geo(sprintf("%s_%03d_%s", tolower(state), as.integer(county_cbs), precinct_enr)), unit_id)
  }
  bind_rows(mp, xw) |>
    distinct(g, .keep_all = TRUE) |>
    full_join(mutate(distinct(frame), frame = TRUE), by = "g") |>
    mutate(frame = coalesce(frame, FALSE))
}

# Models page Shift Map: VoteHub's 2024 presidential Dem / Rep votes per unit_id, estimated
# onto the 2026 precinct boundaries (input_data/votehub-shapefiles/estimated_past_results.csv)
read_pres24 <- function() {
  f = fs::path(PATH_DROPBOX, "26_general", "input_data", "votehub-shapefiles", "estimated_past_results.csv")
  if (!fs::file_exists(f)) return(tibble(unit_id = character(), dem24 = numeric(), rep24 = numeric()))
  readr::read_csv(f, col_types = readr::cols(.default = "c"), col_select = c("unit_id", "pres24_dem", "pres24_rep"),
                  progress = FALSE) |>
    transmute(unit_id, dem24 = as.numeric(pres24_dem), rep24 = as.numeric(pres24_rep)) |>
    filter(dem24 + rep24 > 0)
}

# Models page maps, counties we don't scrape: the latest CBS county snapshot for a race
# (written by cbs_county_pull.R at each model run), one row per county. dem / rep sum the
# per-method rows, which can fall short of the county total (`method_shortfall`), so only
# their ratio is used; total is CBS's county total. NULL if there is no snapshot.
read_cbs_counties <- function(state, contest, district = NA, env = DASHBOARD_CBS_ENV) {
  tag = if (is.na(district)) paste0(state, "_", contest) else paste0(state, "_", contest, "_", district)
  f = tail(sort(fs::dir_ls(fs::path(PATH_DROPBOX, "26_general", "cbs_api", env, tag),
                           regexp = "_\\d{8}_\\d{6}\\.parquet$", fail = FALSE)), 1)
  if (length(f) == 0) return(NULL)
  nanoparquet::read_parquet(f) |>
    summarise(
      county_name = first(county_name),
      dem = sum(votes[party == "dem"], na.rm = TRUE),
      rep = sum(votes[party == "rep"], na.rm = TRUE),
      total = first(county_total_vote),
      pct_exp = first(county_pct_exp_vote),
      cbs_time = max(observed_at),
      .by = c(county_fips, county_cbs)
    )
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
# `dash` is a list of make_summary() outputs (one per jurisdiction);
# `models` is read_model_results() output (NULL for no modeling).
build_dashboard_data <- function(dash, contests = DASHBOARD_CONTESTS, models = NULL) {
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
  geo_units = map(states, read_geo_units) |> set_names(states)
  pres24 = read_pres24()
  mprec_all = bind_rows(tibble(race_id = character(), model = character(), time = as.POSIXct(character()),
                               geo = character(), turn_est = numeric(), dem_est = numeric(), est_dem2 = numeric()),
                        models$prec)

  mp_empty = tibble(
    race_id = character(), state = character(), contest = character(), district = integer(),
    model = character(), model_conf = character(), geo = character(),
    pred_type = character(), party = character(), candidate = character(), time = as.POSIXct(character()),
    pct_rep = numeric(), rep = numeric(), pred = numeric(), pred_min = numeric(), pred_max = numeric(),
    model_base = character(), model_spec = character(), model_interval = character(),
    spec_default = logical(), interval_default = logical()
  )
  ms_empty = tibble(race_id = character(), model = character(), time = as.POSIXct(character()),
                    status = character(), statewide = character(), settings = character(),
                    model_base = character(), model_spec = character(), model_interval = character(),
                    spec_default = logical(), interval_default = logical())
  mp_all = bind_rows(mp_empty, models$pred)
  ms_all = bind_rows(ms_empty, models$status)
  mt_all = bind_rows(tibble(race_id = character(), model = character(), geo = character(), time = as.POSIXct(character()),
                            pred_min = numeric(), pred_max = numeric()),
                     select(models$turnout, any_of(c("race_id", "model", "geo", "time", "pred_min", "pred_max"))))

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

    # model rows: statewide contests may carry NA or the same dummy district as the ENR data
    mp = filter(mp_all, .env$state == .data$state, .env$contest == .data$contest,
                .data$district %in% unique(c(district, dist)))
    race_ids = unique(c(mp$race_id, paste(state, contest, sep = "_"),
                        if (!is.na(district)) paste(state, contest, district, sep = "_")))
    ms = filter(ms_all, race_id %in% race_ids)
    # race turnout intervals (the pred line itself is the vote rows summed, client-side)
    mt = filter(mt_all, race_id %in% unique(mp$race_id), !is.na(pred_min) | !is.na(pred_max)) |>
      select(-race_id)

    # Models page: share of counted votes in precincts the model inputs know (history);
    # for states with a map file (make_dashboard_geo.R), counted votes (with VoteHub's 2024
    # presidential votes, dem24 / rep24) and each model's latest projected remaining vote
    # (rem, with its Dem two-party share d2) per VoteHub unit
    gu = geo_units[[state]]
    gt = summarise(prec, across(c(dem, rep, total), sum), .by = geo) |>
      mutate(g = norm_geo(geo)) |>
      left_join(gu, by = "g")
    # reporting ENR precincts (any votes) found in the model inputs, and their share of counted votes
    has_frame = sum(gt$total) > 0 && any(gu$frame)
    rep_geo = filter(gt, total > 0)
    match_n = if (has_frame) sum(rep_geo$frame %in% TRUE) else NA_integer_
    match_of = if (has_frame) nrow(rep_geo) else NA_integer_
    hist_match = if (has_frame) sum(rep_geo$total[rep_geo$frame %in% TRUE]) / sum(rep_geo$total) else NA_real_
    mres = tibble(unit_id = character(), dem = numeric(), rep = numeric(), total = numeric(), dem24 = numeric(), rep24 = numeric())
    mprec = tibble(model = character(), unit_id = character(), rem = numeric(), d2 = numeric())
    mprec_runs = tibble(model = character(), time = as.POSIXct(character()))
    if (fs::file_exists(fs::path(DASHBOARD_GEO_DIR, glue("{tolower(state)}.json")))) {
      mres = gt |>
        filter(!is.na(unit_id)) |>
        summarise(across(c(dem, rep, total), sum), .by = unit_id) |>
        left_join(pres24, by = "unit_id")
      mr = filter(mprec_all, race_id %in% race_ids)
      mprec = mr |>
        mutate(g = norm_geo(geo), dv = dem_est * turn_est, rv = dv * (1 - est_dem2) / est_dem2) |>
        inner_join(filter(gu, !is.na(unit_id)) |> select(g, unit_id), by = "g") |>
        summarise(tot = sum(turn_est, na.rm = TRUE), dv = sum(dv, na.rm = TRUE), rv = sum(rv, na.rm = TRUE), .by = c(model, unit_id)) |>
        left_join(select(mres, unit_id, dem, rep, total), by = "unit_id") |>
        mutate(
          across(c(dem, rep, total), \(x) coalesce(x, 0)),
          rem = pmax(tot - total, 0), rd = pmax(dv - dem, 0), rr = pmax(rv - rep, 0),
          d2 = if_else(rd + rr > 0, rd / (rd + rr), NA_real_)
        ) |>
        filter(rem >= 1, !is.na(d2)) |>
        transmute(model, unit_id, rem = round(rem), d2 = round(d2, 3))
      mprec_runs = distinct(mr, model, time)
    }
    meta = mutate(meta, hist_match = hist_match, match_n = match_n, match_of = match_of, n_frame = sum(gu$frame))

    # CBS county results for the counties with no ENR precincts (the geo prefix is county_cbs)
    mcty = tibble(county_fips = character(), county_name = character(), dem = numeric(), rep = numeric(),
                  total = numeric(), pct_exp = numeric())
    cbs = read_cbs_counties(state, contest, district)
    meta = mutate(meta, cbs_time = as.POSIXct(NA))
    if (!is.null(cbs)) {
      scraped = unique(str_extract(prec$geo, "(?<=_)[0-9]{3}(?=_)"))
      mcty = filter(cbs, !county_cbs %in% scraped) |> select(names(mcty))
      meta = mutate(meta, cbs_time = max(cbs$cbs_time))
    }

    list(
      meta = meta,
      ts = mutate(ts, key = key, .before = 1),
      models = mp |> select(-any_of(c("state", "contest", "district", "race_id"))) |> mutate(key = key, .before = 1),
      model_status = select(ms, -race_id) |> mutate(key = key, .before = 1),
      model_turnout = mutate(mt, key = key, .before = 1),
      prec = mutate(prec, key = key, .before = 1),
      l2 = mutate(l2, key = key, .before = 1),
      mres = mutate(mres, key = key, .before = 1),
      mprec = mutate(mprec, key = key, .before = 1),
      mprec_runs = mutate(mprec_runs, key = key, .before = 1),
      mcty = mutate(mcty, key = key, .before = 1)
    )
  })

  list(
    meta = map(out, "meta") |> bind_rows(),
    ts = map(out, "ts") |> bind_rows(),
    prec = map(out, "prec") |> bind_rows(),
    l2 = map(out, "l2") |> bind_rows(),
    models = map(out, "models") |> bind_rows(),
    model_status = map(out, "model_status") |> bind_rows(),
    model_turnout = map(out, "model_turnout") |> bind_rows(),
    mres = map(out, "mres") |> bind_rows(),
    mprec = map(out, "mprec") |> bind_rows(),
    mprec_runs = map(out, "mprec_runs") |> bind_rows(),
    mcty = map(out, "mcty") |> bind_rows(),
    geo_url = DASHBOARD_GEO_URL,
    title = DASHBOARD_TITLE,
    top_model = DASHBOARD_MODEL
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

# Election-night models: one refresh of scripts/model/live/run_night.R, in its own R
# process (load.R must set single-threaded BLAS before any matrix work, and the models
# fork their own workers). Runs the run = TRUE rows of `config`
# ({ELECTION_FOLDER}/model_config.csv, scripts/model/live/R/config.R). `sim`: "" for the
# real scraped returns (written to model_results/), or a 26_sim scenario run at `at`
# (written to model_tests/run_night_sim/<sim>/). `after` is unused: it orders this after
# the scrapes. Returns the run's status table, one row per race x model.
run_models <- function(config, sim = MODEL_SIM, at = MODEL_SIM_AT, after = NULL) {
  args = c("scripts/model/live/run_night.R", glue("--config={config}"))
  if (nzchar(sim)) args = c(args, glue("--sim={sim}"), glue("--at={at}"))
  out = suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(args), stdout = TRUE, stderr = TRUE))
  writeLines(out)
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
    stop("run_night.R failed (exit ", attr(out, "status"), "):\n", paste(tail(out, 20), collapse = "\n"))
  }
  dir = fs::path(PATH_DROPBOX, ELECTION_FOLDER)
  dir = if (nzchar(sim)) fs::path(dir, "model_tests", "run_night_sim", sim) else fs::path(dir, "model_results")
  readr::read_csv(fs::path(dir, "status_latest.csv"), col_types = readr::cols(.default = "c"), progress = FALSE)
}
