# ========================================
## Redistricting tab of the dashboard (index.qmd), as targets: each redrawn state's
## congressional districts under the 2024 map (old, 119th Congress) and the 2026 map (new),
## from three kinds of votes, picked on the page. Tonight's votes are the state's top-of-ticket
## race (redist_contest(): the Senate or governor race the models project), not the House
## races, so every model can be compared without modeling the House:
##   pres24  the 2024 presidential vote regrouped into each map's districts
##           (model_results/redistricting/redistricting_districts_2024pres.csv)
##   count   the race's votes counted so far: ENR precinct results where we scrape, CBS county
##           results elsewhere (pulled every cycle, redist_top_votes())
##   model   each model's projected final vote in the race, precinct by precinct
##           (model_results/precincts_latest.parquet)
## Precinct votes go to each map's districts by the precinct's share of each (a precinct
## with no polygon in the prep's map, and a county's CBS votes, by the county's 2024 vote in
## each district).
## The prep, once before the night, is scripts/model/reports/redistricting_compare.R
## without --live (~15 min, needs fourthirtyfive): it writes the 2024 table and, in
## input_data/redistricting/, {ST}_county_district_weights.csv, {ST}_map.gpkg and
## {ST}_unit_districts.csv. Without those this tab is empty; nothing else waits on it.
# ========================================

REDIST_STATES = names(STATE_FIPS)

# The prep's outputs (file target): whichever exist, so a state added later reruns what uses them
redist_files <- function(states = REDIST_STATES) {
  d26 = find_26_general()
  w = file.path(d26, "input_data", "redistricting")
  f = c(
    file.path(d26, "model_results", "redistricting", "redistricting_districts_2024pres.csv"),
    file.path(w, paste0(rep(states, each = 3), c("_county_district_weights.csv", "_map.gpkg", "_unit_districts.csv")))
  )
  f = f[file.exists(f)]
  if (!length(f)) stop("No redistricting inputs: run scripts/model/reports/redistricting_compare.R (no --live) first")
  f
}

# District outlines for the page's map, data/dashboard/geo/{st}_districts.json (uploaded with
# the other geometry by automation.R): both maps' districts from {ST}_map.gpkg, clipped to the
# state outline in {st}_counties.json (TIGER district lines run out into lakes and the sea),
# with a label point (lx, ly) per district. Rebuilt only when a gpkg changes.
redist_geo <- function(files, dir = DASHBOARD_GEO_DIR) {
  suppressPackageStartupMessages(library(sf))
  old_s2 = sf_use_s2(FALSE)
  on.exit(sf_use_s2(old_s2), add = TRUE)
  gp = grep("_map\\.gpkg$", files, value = TRUE)
  out = character()
  for (f in gp) {
    st = sub("_map\\.gpkg$", "", basename(f))
    cty = fs::path(dir, glue("{tolower(st)}_counties.json"))
    outline = if (fs::file_exists(cty)) {
      x = geojsonsf::geojson_sf(cty)
      x = x[x$id == "state", ]
      if (nrow(x)) st_make_valid(st_transform(x, 5070)) |> st_union() else NULL
    }
    d = bind_rows(lapply(c("old", "new"), \(p) mutate(read_sf(f, layer = p, quiet = TRUE), plan = p))) |>
      st_make_valid()
    if (!is.null(outline)) d = suppressWarnings(st_intersection(d, outline))
    d = suppressWarnings(st_collection_extract(st_make_valid(d), "POLYGON")) |>
      group_by(cd, plan) |>
      summarise(.groups = "drop")
    lab = st_coordinates(st_transform(suppressWarnings(st_point_on_surface(d)), 4326))
    d = d |>
      st_transform(4326) |>
      transmute(id = paste(plan, cd), cd, plan, lx = round(lab[, 1], 4), ly = round(lab[, 2], 4))
    p = fs::path(dir, glue("{tolower(st)}_districts.json"))
    writeLines(geojsonsf::sf_geojson(d, atomise = FALSE, digits = 4), p)
    out = c(out, p)
  }
  out
}

# The state's top-of-ticket race for this tab: its statewide race in priors.csv (what the
# models project); TX has two, so TOP_RACE's Senate; CA has none, so its governor
redist_contest <- function(st) {
  pr = tryCatch(filter(races_from_priors(find_26_general()), state == st, is.na(district))$contest,
                error = function(e) character())
  if (length(pr) == 1) pr else if (!length(pr) || TOP_RACE[[st]] %in% pr) TOP_RACE[[st]] else sort(pr)[1]
}
REDIST_RACE_NAME = c(gov = "Governor", sen = "Senate")

# CBS county results for the state's top-of-ticket race (redist_cbs_county(), saved to
# 26_general/cbs_api/{env}/redistricting/): one row per county, with attribute "as_of". NULL in
# a sim, with CBS_MODEL_REDISTRICTING=FALSE, or when CBS has nothing yet.
redist_top_votes <- function(state, env = MODEL_CBS_ENV, sim = MODEL_SIM, enabled = MODEL_REDISTRICTING) {
  if (!enabled || nzchar(sim)) return(NULL)
  contest = redist_contest(state)
  x = tryCatch(redist_cbs_county(state, contest, env = env), error = function(e) {
    cli::cli_alert_info("Redistricting {state}: {conditionMessage(e)}")
    NULL
  })
  if (is.null(x) || !nrow(x)) return(NULL)
  out = x |>
    summarise(dem = sum(votes[party == "dem"]), rep = sum(votes[party == "rep"]), total = sum(votes),
              pct_exp = first(pct_exp), .by = county_fips) |>
    mutate(state = state, contest = contest, .before = 1)
  attr(out, "as_of") = attr(x, "as_of")
  out
}

# Each precinct's (VoteHub unit's) share in each district of each map: the prep's
# {ST}_unit_districts.csv, else the gpkg's main district per precinct (share 1)
redist_unit_districts <- function(st, files) {
  f = grep(paste0("/", st, "_unit_districts\\.csv$"), files, value = TRUE)
  if (length(f)) return(readr::read_csv(f, col_types = "cccd", progress = FALSE))
  g = grep(paste0("/", st, "_map\\.gpkg$"), files, value = TRUE)
  if (!length(g)) return(NULL)
  sf::read_sf(g, query = "SELECT unit_id, cd_old, cd_new FROM precincts", quiet = TRUE) |>
    as_tibble() |>
    pivot_longer(c(cd_old, cd_new), names_to = "plan", values_to = "cd", names_prefix = "cd_") |>
    filter(!is.na(cd)) |>
    mutate(share = 1)
}

# Each county's share of its 2024 presidential two-party vote in each district of each map,
# from the prep's {ST}_county_district_weights.csv, keyed by CBS county code (the model geos'
# prefix) through the county names in data/dashboard/geo/{st}_counties.json
redist_county_districts <- function(st, files, dir = DASHBOARD_GEO_DIR) {
  f = grep(paste0("/", st, "_county_district_weights\\.csv$"), files, value = TRUE)
  cj = fs::path(dir, glue("{tolower(st)}_counties.json"))
  if (!length(f) || !fs::file_exists(cj)) return(tibble(county_fips = character(), county_cbs = integer(), plan = character(), cd = character(), share = numeric()))
  norm = \(x) str_remove_all(toupper(x), "[^A-Z]")
  fips = jsonlite::read_json(cj)$features |>
    map(\(x) x$properties) |>
    bind_rows() |>
    filter(id != "state") |>
    transmute(county_fips = id, k = norm(name)) |>
    inner_join(filter(enightmodels::cbs_counties, state == st) |> transmute(county_cbs, k = norm(county)), by = "k")
  w = readr::read_csv(f, col_types = readr::cols(county_fips = "c", cd_new = "c", cd_old = "c", .default = "d"), progress = FALSE)
  bind_rows(
    summarise(w, v = sum(votes), .by = c(county_fips, cd_old)) |> transmute(county_fips, cd = cd_old, v, plan = "old"),
    summarise(w, v = sum(votes), .by = c(county_fips, cd_new)) |> transmute(county_fips, cd = cd_new, v, plan = "new")
  ) |>
    mutate(share = v / sum(v), .by = c(county_fips, plan)) |>
    inner_join(select(fips, county_fips, county_cbs), by = "county_fips") |>
    select(county_fips, county_cbs, plan, cd, share)
}

# Votes `v` (columns g = norm_geo() precinct key, county_cbs, dv, rv, plus grouping columns
# `by`) into each map's districts: through the precinct's VoteHub unit (`gu`) and its district
# shares (`ud`), else by its county's 2024 vote in each district (`cw`). Returns `by`, plan,
# cd, dem, rep, with attribute "placed": by `by`, the share of the votes placed by precinct.
redist_allocate <- function(v, by, gu, ud, cw) {
  v = v |>
    left_join(gu, by = "g", relationship = "many-to-one") |>
    mutate(unit_id = if_else(unit_id %in% ud$unit_id, unit_id, NA_character_))
  placed = summarise(v, p = sum((dv + rv)[!is.na(unit_id)]) / sum(dv + rv), .by = all_of(by))
  by_unit = v |>
    filter(!is.na(unit_id)) |>
    summarise(dv = sum(dv), rv = sum(rv), .by = all_of(c(by, "unit_id"))) |>
    inner_join(ud, by = "unit_id", relationship = "many-to-many")
  by_cty = v |>
    filter(is.na(unit_id)) |>
    summarise(dv = sum(dv), rv = sum(rv), .by = all_of(c(by, "county_cbs"))) |>
    inner_join(select(cw, -county_fips), by = "county_cbs", relationship = "many-to-many")
  out = bind_rows(by_unit, by_cty) |>
    summarise(dem = sum(dv * share), rep = sum(rv * share), .by = all_of(c(by, "plan", "cd")))
  attr(out, "placed") = placed
  out
}

# The tab's data: dist (state, src, plan, cd, dem, rep, cov) and src (one row per state x vote
# source: kind, label, race, model and its parts, time, note). `top` is the list of
# redist_top_votes() results (NULL for states without any); `models` is model_results; `dash`
# is build_dashboard_data()'s output, for the ENR precinct results.
build_redist_data <- function(files, top = list(), models = NULL, dash = NULL, contests = DASHBOARD_CONTESTS) {
  dist = list()
  src = list()
  add = function(d, s) {
    dist[[length(dist) + 1]] <<- d
    src[[length(src) + 1]] <<- s
  }
  sp = \(n, w) if (n == 1) w else sub("y$", "ie", w) |> paste0("s")
  fmtn = \(x) format(x, big.mark = ",")
  top = Filter(Negate(is.null), top)
  names(top) = vapply(top, \(x) x$state[1], "")

  # 2024 president
  f24 = grep("redistricting_districts_2024pres\\.csv$", files, value = TRUE)
  t24 = NULL
  if (length(f24)) {
    t24 = readr::read_csv(f24, col_types = readr::cols(state = "c", plan = "c", cd = "c", .default = "d"), progress = FALSE)
    for (st in intersect(REDIST_STATES, unique(t24$state))) {
      ud = redist_unit_districts(st, files)
      n_split = if (is.null(ud) || all(ud$share == 1)) NA else
        ud |> summarise(n = n_distinct(cd), .by = c(plan, unit_id)) |> filter(n > 1) |> distinct(unit_id) |> nrow()
      votes = switch(st, CA = "Statewide Database", OH = "MEDSL", "VoteHub")
      add(
        t24 |> filter(state == st) |> transmute(state, src = "pres24", plan, cd, dem, rep),
        tibble(state = st, src = "pres24", kind = "pres24", label = "2024 president", time = as.POSIXct(NA),
               note = paste0("2024 presidential precinct results (", votes, ") regrouped into each map's districts",
                             if (!is.na(n_split)) glue(" · {fmtn(n_split)} split {sp(n_split, 'precinct')} divided by area") else ""))
      )
    }
  }

  # coverage of a source's districts: its two-party vote over the 2024 one, relative to the
  # state's best-covered districts (the 90th percentile); the page greys districts under 0.5
  coverage = function(d, st) {
    if (is.null(t24)) return(d)
    d |>
      left_join(filter(t24, state == st) |> transmute(plan, cd, v24 = dem + rep), by = c("plan", "cd")) |>
      mutate(r = (dem + rep) / v24, cov = pmin(round(r / quantile(r, 0.9, na.rm = TRUE), 2), 1)) |>
      select(-v24, -r)
  }
  mp = models$prec
  parts = if (!is.null(mp) && nrow(mp)) dash_model_parts(unique(mp$model)) else NULL

  for (st in REDIST_STATES) {
    ud = redist_unit_districts(st, files)
    if (is.null(ud)) next
    contest = redist_contest(st)
    race_id = paste(st, contest, sep = "_")
    race = coalesce(filter(contests, state == st, contest == !!contest, is.na(district))$label[1],
                    paste(st, REDIST_RACE_NAME[[contest]]))
    gu = filter(read_geo_units(st), !is.na(unit_id)) |> select(g, unit_id)
    cw = redist_county_districts(st, files)
    cty_of = \(geo) as.integer(str_extract(geo, "(?<=^[A-Za-z]{2}_)\\d{3}"))

    # the race's votes counted so far: ENR precincts in the counties we scrape, CBS's county
    # results in the rest
    key = paste(st, contest, sep = "-")
    enr = if (!is.null(dash$prec)) filter(dash$prec, .data$key == .env$key) else tibble()
    cbs = top[[st]]
    if (!is.null(cbs) && cbs$contest[1] != contest) cbs = NULL
    v = if (nrow(enr)) {
      enr |> summarise(dv = sum(dem), rv = sum(rep), .by = geo) |> transmute(g = norm_geo(geo), county_cbs = cty_of(geo), dv, rv)
    } else {
      tibble(g = character(), county_cbs = integer(), dv = numeric(), rv = numeric())
    }
    scraped = unique(v$county_cbs)
    v = filter(v, dv + rv > 0)
    cbs_rest = if (is.null(cbs)) tibble() else
      cbs |> inner_join(distinct(cw, county_fips, county_cbs), by = "county_fips") |> filter(!county_cbs %in% scraped, dem + rep > 0)
    if (nrow(v) || nrow(cbs_rest)) {
      d = redist_allocate(mutate(v, k = 1), "k", gu, ud, cw)
      p = attr(d, "placed")$p
      if (nrow(cbs_rest)) {
        d = bind_rows(d, cbs_rest |>
          inner_join(select(cw, -county_fips), by = "county_cbs", relationship = "many-to-many") |>
          summarise(dem = sum(dem * share), rep = sum(rep * share), .by = c(plan, cd)))
        d = summarise(d, dem = sum(dem), rep = sum(rep), .by = c(plan, cd))
      }
      n_cbs = n_distinct(cbs_rest$county_cbs)
      t_enr = filter(dash$meta, .data$key == .env$key)$time
      t_cbs = attr(cbs, "as_of")
      pct_cbs = if (nrow(cbs_rest)) with(filter(cbs_rest, pct_exp > 0), sum(total) / sum(total / (pct_exp / 100))) else NA_real_
      add(
        mutate(d, state = st, src = "count") |> select(state, src, plan, cd, dem, rep),
        tibble(state = st, src = "count", kind = "count", label = glue("{race} votes counted"), race_id, race,
               time = if (any(!is.na(c(t_enr, t_cbs)))) max(c(t_enr, t_cbs), na.rm = TRUE) else as.POSIXct(NA),
               note = paste0(
                 glue("Counted {race} vote, not the House vote"),
                 if (nrow(v)) glue(" · precinct results in {length(scraped)} scraped {sp(length(scraped), 'county')}") else "",
                 if (length(p) && !is.na(p) && p < 0.995) glue(" ({scales::percent(1 - p, 1)} placed by county)") else "",
                 if (n_cbs) glue(" · CBS county results in {n_cbs} other {sp(n_cbs, 'county')}",
                                 "{if (is.finite(pct_cbs)) paste0(' (', scales::percent(pct_cbs, 1), ' of expected)') else ''}",
                                 ", split by 2024 vote") else ""
               ))
      )
    }

    # models: each one's projected final vote per precinct
    m = if (is.null(mp)) tibble() else filter(mp, .data$race_id == .env$race_id)
    if (!nrow(m)) next
    mv = m |>
      mutate(g = norm_geo(geo), county_cbs = cty_of(geo), dv = dem_est * turn_est,
             rv = if_else(est_dem2 > 0, dv * (1 - est_dem2) / est_dem2, NA_real_)) |>
      filter(!is.na(rv))
    d = redist_allocate(mv, "model", gu, ud, cw)
    if (!nrow(d)) next
    placed = attr(d, "placed")
    d = bind_rows(lapply(split(d, d$model), coverage, st = st)) |>
      mutate(state = st, src = paste("model", race_id, model, sep = ":"))
    s = distinct(mv, model, time) |>
      summarise(time = max(time), .by = model) |>
      inner_join(placed, by = "model") |>
      semi_join(d, by = "model") |>
      left_join(parts, by = "model") |>
      transmute(state = st, src = paste("model", race_id, model, sep = ":"), kind = "model", label = model,
                race_id, race, model, model_base, model_spec, model_interval, spec_default, interval_default, time,
                note = paste0(glue("Projected {race} vote, not the House vote"),
                              if_else(p < 0.995, glue(" · {scales::percent(1 - p, 1)} of it placed by county, not precinct"), "")))
    add(select(d, state, src, plan, cd, dem, rep, cov), s)
  }

  desc = if (requireNamespace("fourthirtyfive", quietly = TRUE)) {
    tryCatch(fourthirtyfive::cd_names(2027) |> distinct(cd, .keep_all = TRUE) |> transmute(cd, desc = cd_description),
             error = function(e) NULL)
  }
  list(
    dist = bind_rows(tibble(state = character(), src = character(), plan = character(), cd = character(), dem = numeric(), rep = numeric(),
                            cov = numeric()), dist) |>
      mutate(across(c(dem, rep), round)),
    src = bind_rows(tibble(state = character(), src = character(), kind = character(), label = character(), time = as.POSIXct(character()),
                           note = character()), lapply(src, \(x) mutate(x, note = as.character(note)))),
    desc = desc
  )
}
