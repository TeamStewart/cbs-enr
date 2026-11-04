# ========================================
## Redistricting tab of the dashboard (index.qmd), as targets: each redrawn state's
## congressional districts under the 2024 map (old, 119th Congress) and the 2026 map (new),
## from the votes picked on the page:
##   count:sen / count:gov / count:house   each race's votes counted so far: ENR precinct results
##           where we scrape (redist_enr(), every contest in the scrapes), CBS county results
##           elsewhere (redist_cbs_votes()). House: the new districts are the races' own totals;
##           the old ones are estimated. A House race without both a Democrat and a Republican
##           is split by the top-of-ticket race's share (redist_contest())
##   pres24  the 2024 presidential vote regrouped into each map's districts
##           (model_results/redistricting/redistricting_districts_2024pres.csv)
##   model   each model's projected final vote in the top-of-ticket race, precinct by precinct
##           (model_results/precincts_latest.parquet)
## Precinct votes go to each map's districts by the precinct's share of each (a precinct
## with no polygon in the prep's map, and a county's CBS votes, by the county's 2024 vote in
## each district). The page's map shades each dashboard precinct by its own votes and each
## county by the votes placed by county; each source reports the share placed each way (p_prec).
## The prep, once before the night, is scripts/model/reports/redistricting_compare.R
## without --live (~15 min, needs fourthirtyfive): it writes the 2024 table and, in
## input_data/redistricting/, {ST}_county_district_weights.csv, {ST}_map.gpkg,
## {ST}_unit_districts.csv and {ST}_unit_pres24.csv. Without those this tab is empty; nothing else waits on it.
# ========================================

REDIST_STATES = names(STATE_FIPS)

# The prep's outputs (file target): whichever exist, so a state added later reruns what uses them
redist_files <- function(states = REDIST_STATES) {
  d26 = find_26_general()
  w = file.path(d26, "input_data", "redistricting")
  f = c(
    file.path(d26, "model_results", "redistricting", "redistricting_districts_2024pres.csv"),
    file.path(w, paste0(rep(states, each = 4), c("_county_district_weights.csv", "_map.gpkg", "_unit_districts.csv", "_unit_pres24.csv")))
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
REDIST_RACE_NAME = c(gov = "Governor", sen = "Senate", house = "House")
REDIST_RACES = c("sen", "gov", "house")
# A county's scraped precincts are used for a race (House: for a county x district) only when
# they hold at least this share of CBS's votes there (or CBS has nothing there); otherwise CBS's
# county result, so a scrape that stopped early or never filled in does not undercount
REDIST_ENR_MIN = 0.85

# CBS county results for the tab's races (redist_cbs_county(), saved to
# 26_general/cbs_api/{env}/redistricting/): Senate and governor pulled here, each House
# district read from the last pull saved by redistricting_compare.R --live (run_night.R starts
# it every refresh) unless pull_house. One row per race x county (state, contest, district,
# county_fips, dem, rep, total, pct_exp, as_of). NULL in a sim, with
# CBS_MODEL_REDISTRICTING=FALSE, or when CBS has nothing yet.
redist_cbs_votes <- function(state, env = MODEL_CBS_ENV, sim = MODEL_SIM, enabled = MODEL_REDISTRICTING, pull_house = FALSE) {
  if (!enabled || nzchar(sim)) return(NULL)
  one = function(contest, district = NA, pull = TRUE) {
    x = tryCatch(suppressMessages(redist_cbs_county(state, contest, district, env = env, pull = pull)), error = function(e) NULL)
    if (is.null(x) || !nrow(x)) return(NULL)
    x |>
      summarise(dem = sum(votes[party == "dem"]), rep = sum(votes[party == "rep"]), total = sum(votes),
                has_dem = any(party == "dem"), has_rep = any(party == "rep"), # on the ballot, even with 0 votes
                pct_exp = first(pct_exp), .by = county_fips) |>
      mutate(state = state, contest = contest, district = as.integer(district), as_of = attr(x, "as_of"), .before = 1)
  }
  wf = fs::path(find_26_general(), "input_data", "redistricting", glue("{state}_county_district_weights.csv"))
  cds = if (fs::file_exists(wf)) sort(unique(readr::read_csv(wf, col_types = readr::cols(.default = "c"), progress = FALSE)$cd_new)) else character()
  out = bind_rows(one("sen"), one("gov"),
                  lapply(as.integer(sub("^[A-Z]{2}-", "", cds)), \(d) one("house", d, pull = pull_house)))
  if (!nrow(out)) NULL else out
}

# ENR precinct votes for the tab's races from make_summary() outputs (dash_inputs): state,
# contest, district (NA for statewide), geo, dem, rep, total, time (the jurisdiction's scrape),
# has_dem / has_rep (a Democrat / Republican on the ballot there, even with no votes yet)
redist_enr <- function(dash_inputs, states = REDIST_STATES) {
  p = purrr::compact(dash_inputs) |>
    keep(\(x) x$state %in% states) |>
    map(\(x) mutate(x$prec, state = x$state, time = x$time)) |>
    bind_rows()
  if (!nrow(p)) return(tibble(state = character(), contest = character(), district = integer(), geo = character(),
                              dem = numeric(), rep = numeric(), total = numeric(), time = as.POSIXct(character())))
  p |>
    filter(contest %in% REDIST_RACES) |>
    mutate(district = if_else(contest == "house", as.integer(district), NA_integer_)) |>
    summarise(dem = sum(votes[party == "dem"]), rep = sum(votes[party == "rep"]), total = sum(votes), time = max(time),
              has_dem = any(party == "dem"), has_rep = any(party == "rep"),
              .by = c(state, contest, district, geo))
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
# cd, dem, rep, with attributes "placed" (by `by`, the share of the votes placed by precinct),
# "units" (`by`, unit_id, dem, rep: the votes placed by precinct, for the page's map) and
# "counties" (`by`, county_fips, dem, rep: the votes placed by county).
redist_allocate <- function(v, by, gu, ud, cw) {
  v = v |>
    left_join(gu, by = "g", relationship = "many-to-one") |>
    mutate(unit_id = if_else(unit_id %in% ud$unit_id, unit_id, NA_character_))
  placed = summarise(v, p = sum((dv + rv)[!is.na(unit_id)]) / sum(dv + rv), .by = all_of(by))
  units = v |>
    filter(!is.na(unit_id)) |>
    summarise(dv = sum(dv), rv = sum(rv), .by = all_of(c(by, "unit_id")))
  by_unit = inner_join(units, ud, by = "unit_id", relationship = "many-to-many")
  ctys = v |>
    filter(is.na(unit_id)) |>
    summarise(dv = sum(dv), rv = sum(rv), .by = all_of(c(by, "county_cbs")))
  by_cty = inner_join(ctys, select(cw, -county_fips), by = "county_cbs", relationship = "many-to-many")
  out = bind_rows(by_unit, by_cty) |>
    summarise(dem = sum(dv * share), rep = sum(rv * share), .by = all_of(c(by, "plan", "cd")))
  attr(out, "placed") = placed
  attr(out, "units") = rename(units, dem = dv, rep = rv)
  attr(out, "counties") = ctys |>
    inner_join(distinct(cw, county_fips, county_cbs), by = "county_cbs") |>
    transmute(across(all_of(by)), county_fips, dem = dv, rep = rv)
  out
}

# Dashboard precinct ids with a polygon ({st}.json), to keep the page's per-precinct table to
# what its map can draw
redist_geo_ids <- function(st, dir = DASHBOARD_GEO_DIR) {
  f = fs::path(dir, glue("{tolower(st)}.json"))
  if (!fs::file_exists(f)) return(character())
  vapply(jsonlite::read_json(f)$features, \(x) x$properties$id, "")
}

# The tab's data: dist (state, src, plan, cd, dem, rep, cov) and src (one row per state x vote
# source: kind, label, race, model and its parts, time, note, p_prec), plus unit / cty (the
# map's fills). `cbs` is redist_cbs_votes() for every state, bound (NULL for none); `models` is
# model_results; `enr` is redist_enr(), the ENR precinct votes for every race.
build_redist_data <- function(files, cbs = NULL, models = NULL, enr = NULL, contests = DASHBOARD_CONTESTS) {
  dist = list()
  src = list()
  unit = list()
  cty = list()
  # d: district totals; s: the source row; u / k: the votes placed by precinct (unit_id) and by
  # county (county_fips), for the map (state, src, id, dem, rep)
  add = function(d, s, u = NULL, k = NULL) {
    dist[[length(dist) + 1]] <<- d
    src[[length(src) + 1]] <<- s
    unit[[length(unit) + 1]] <<- u
    cty[[length(cty) + 1]] <<- k
  }
  # share of a source's votes placed by precinct (the rest by county): p_prec on its src row
  pprec = \(u, k) { a = sum(u$dem + u$rep); b = sum(k$dem + k$rep); if (a + b > 0) a / (a + b) else NA_real_ }
  sp = \(n, w) if (n == 1) w else sub("y$", "ie", w) |> paste0("s")
  fmtn = \(x) format(x, big.mark = ",")

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
      # map: the 2024 vote on the dashboard's precincts (the prep's {ST}_unit_pres24.csv) over each
      # county's 2024 vote, for counties the precinct map leaves out
      fu = grep(paste0("/", st, "_unit_pres24\\.csv$"), files, value = TRUE)
      u24 = if (length(fu)) readr::read_csv(fu, col_types = "cdd", progress = FALSE) |>
        filter(unit_id %in% redist_geo_ids(st)) |> mutate(state = st, src = "pres24", .before = 1)
      fw = grep(paste0("/", st, "_county_district_weights\\.csv$"), files, value = TRUE)
      k24 = if (length(fw)) readr::read_csv(fw, col_types = readr::cols(county_fips = "c", cd_new = "c", cd_old = "c", .default = "d"), progress = FALSE) |>
        summarise(dem = sum(dem24), rep = sum(rep24), .by = county_fips) |> mutate(state = st, src = "pres24", .before = 1)
      add(
        t24 |> filter(state == st) |> transmute(state, src = "pres24", plan, cd, dem, rep),
        tibble(state = st, src = "pres24", kind = "pres24", label = "2024 president", time = as.POSIXct(NA), p_prec = 1,
               note = paste0("2024 presidential precinct results (", votes, ") regrouped into each map's districts",
                             if (!is.na(n_split)) glue(" · {fmtn(n_split)} split {sp(n_split, 'precinct')} divided by area") else "")),
        u24, k24
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

    gid = redist_geo_ids(st)
    cmap = distinct(cw, county_fips, county_cbs)
    fw = grep(paste0("/", st, "_county_district_weights\\.csv$"), files, value = TRUE)
    ws = if (length(fw)) readr::read_csv(fw, col_types = readr::cols(county_fips = "c", cd_new = "c", cd_old = "c", .default = "d"), progress = FALSE) |>
      mutate(f = votes / sum(votes), .by = c(county_fips, cd_new)) |> select(county_fips, cd_new, cd_old, f) else
      tibble(county_fips = character(), cd_new = character(), cd_old = character(), f = numeric())
    # empty inputs (e.g. bind_rows() of all-NULL redist_cbs before CBS has results) have no
    # `state` column, so treat them like NULL
    e_st = if (is.null(enr) || !nrow(enr)) tibble() else filter(enr, state == st) |> mutate(g = norm_geo(geo), county_cbs = cty_of(geo))
    c_st = if (is.null(cbs) || !nrow(cbs)) tibble() else filter(cbs, state == st) |> inner_join(cmap, by = "county_fips")
    # the top-of-ticket race's Dem two-party share by precinct and by county: splits the votes
    # of House races without both a Democrat and a Republican
    s_g = if (nrow(e_st)) e_st |> filter(contest == .env$contest) |> summarise(s = sum(dem) / sum(dem + rep), tg = sum(total), .by = g) |> filter(is.finite(s)) else tibble(g = character(), s = numeric(), tg = numeric())
    # (by county: the scrape's precincts if they hold REDIST_ENR_MIN of CBS's votes, else CBS's county result)
    s_k = bind_rows(if (nrow(e_st)) e_st |> filter(contest == .env$contest) |> transmute(county_cbs, d = dem, r = rep, src = 1L),
                    if (nrow(c_st)) c_st |> filter(contest == .env$contest) |> transmute(county_cbs, d = dem, r = rep, src = 2L)) |>
      bind_rows(tibble(county_cbs = integer(), d = numeric(), r = numeric(), src = integer())) |>
      summarise(d = sum(d), r = sum(r), .by = c(county_cbs, src)) |>
      filter(d + r > 0) |>
      mutate(use = if (any(src == 2L)) src == 2L | (d + r) >= REDIST_ENR_MIN * sum((d + r)[src == 2L]) else TRUE, .by = county_cbs) |>
      filter(use) |>
      filter(src == min(src), .by = county_cbs) |>
      summarise(sk = sum(d) / sum(d + r), tk = sum(d + r), .by = county_cbs) |> filter(is.finite(sk))

    # each race's votes counted so far: ENR precincts in the counties we scrape for it, CBS's
    # county results in the rest. House: the new districts are the races themselves; the old
    # ones get each precinct's votes by its area shares, CBS county votes by the county x new
    # district's 2024 vote in each old district
    for (ct in REDIST_RACES) {
      e = if (nrow(e_st)) filter(e_st, contest == ct) else tibble()
      k = if (nrow(c_st)) filter(c_st, contest == ct) else tibble()
      if (!nrow(e) && !nrow(k)) next
      # inputs from before has_dem / has_rep: a party with votes was on the ballot
      if (nrow(e) && !"has_dem" %in% names(e)) e = mutate(e, has_dem = dem > 0, has_rep = rep > 0)
      if (nrow(k) && !"has_dem" %in% names(k)) k = mutate(k, has_dem = dem > 0, has_rep = rep > 0)
      # precincts where the scrape holds at least REDIST_ENR_MIN of CBS's votes for this race, by
      # county (House: county x district, so a district the county's scrape leaves out, or is
      # behind on, comes from CBS); CBS's county results everywhere else
      if (nrow(e)) {
        kt = if (nrow(k)) summarise(k, kt = sum(total), .by = c(county_cbs, district)) else
          tibble(county_cbs = integer(), district = integer(), kt = numeric())
        has = e |> summarise(t = sum(total), .by = c(county_cbs, district)) |>
          left_join(kt, by = c("county_cbs", "district")) |>
          filter(t > 0, is.na(kt) | t >= REDIST_ENR_MIN * kt) |>
          select(county_cbs, district)
        e = semi_join(e, has, by = c("county_cbs", "district"))
        if (nrow(k)) k = anti_join(k, has, by = c("county_cbs", "district"))
      }
      scraped = unique(e$county_cbs)
      rname = REDIST_RACE_NAME[[ct]]
      unc = integer()
      # uncontested House races: the party of the only major-party candidate, which wins the seat on
      # the 2026 map whatever the split below (sole = "dem" / "rep"; NA if neither party ran)
      sole = tibble(cd = character(), sole = character())
      if (ct == "house") {
        both = bind_rows(if (nrow(e)) select(e, district, dem, rep, total, has_dem, has_rep),
                         if (nrow(k)) select(k, district, dem, rep, total, has_dem, has_rep)) |>
          summarise(ok = sum(dem) > 0 & sum(rep) > 0, d0 = any(has_dem), r0 = any(has_rep), tot = sum(total), .by = district)
        unc = both$district[!both$ok]
        # a race with only one major party on the ballot (only Democrats / only Republicans):
        # that party's seat on the 2026 map
        sole = both |> filter(!ok, xor(d0, r0)) |>
          transmute(cd = sprintf("%s-%02d", st, as.integer(district)), sole = if_else(d0, "dem", "rep"))
        # a race with no votes reported at all (an unopposed candidate whose votes the state does
        # not count, e.g. FL-10): its voters from the top-of-ticket race, each county's total
        # times the county's share in the district (precincts: their own total), then split
        # below like any uncontested race
        zero = both$district[both$tot == 0]
        if (length(zero)) {
          cdf0 = \(x) sprintf("%s-%02d", st, as.integer(x))
          cn = filter(cw, plan == "new") |> select(county_cbs, cd, share)
          if (nrow(k)) k = k |> mutate(cd = cdf0(district)) |> left_join(s_k |> select(county_cbs, tk), by = "county_cbs") |>
            left_join(cn, by = c("county_cbs", "cd")) |>
            mutate(total = if_else(district %in% zero, coalesce(tk * share, 0), total)) |> select(-cd, -tk, -share)
          if (nrow(e)) e = e |> left_join(s_g |> select(g, tg), by = "g") |>
            mutate(total = if_else(district %in% zero, coalesce(tg, 0), total)) |> select(-tg)
        }
        if (length(unc)) {
          if (nrow(e)) e = e |> left_join(s_g, by = "g") |> left_join(s_k, by = "county_cbs") |>
            mutate(s = coalesce(s, sk, 0.5), u = district %in% unc,
                   dem = if_else(u, total * s, dem), rep = if_else(u, total * (1 - s), rep)) |> select(-s, -sk, -u)
          if (nrow(k)) k = k |> left_join(s_k, by = "county_cbs") |>
            mutate(s = coalesce(sk, 0.5), u = district %in% unc,
                   dem = if_else(u, total * s, dem), rep = if_else(u, total * (1 - s), rep)) |> select(-s, -sk, -u)
        }
      }
      v = if (nrow(e)) e |> filter(dem + rep > 0) |> transmute(g, county_cbs, district, dv = dem, rv = rep) else
        tibble(g = character(), county_cbs = integer(), district = integer(), dv = numeric(), rv = numeric())
      if (nrow(k)) k = filter(k, dem + rep > 0)
      if (!nrow(v) && !nrow(k)) next
      d = redist_allocate(mutate(v, kk = 1), "kk", gu, ud, cw)
      cu = select(attr(d, "units"), unit_id, dem, rep)
      ck = bind_rows(select(attr(d, "counties"), county_fips, dem, rep), if (nrow(k)) select(k, county_fips, dem, rep)) |>
        summarise(dem = sum(dem), rep = sum(rep), .by = county_fips)
      d = select(d, plan, cd, dem, rep)
      if (ct == "house") {
        cdf = \(x) sprintf("%s-%02d", st, as.integer(x))
        new = bind_rows(summarise(v, dem = sum(dv), rep = sum(rv), .by = district),
                        if (nrow(k)) summarise(k, dem = sum(dem), rep = sum(rep), .by = district)) |>
          summarise(dem = sum(dem), rep = sum(rep), .by = district) |>
          transmute(plan = "new", cd = cdf(district), dem, rep)
        old_k = if (nrow(k)) k |> mutate(cd_new = cdf(district)) |>
          inner_join(ws, by = c("county_fips", "cd_new"), relationship = "many-to-many") |>
          summarise(dem = sum(dem * f), rep = sum(rep * f), .by = cd_old) |> transmute(plan = "old", cd = cd_old, dem, rep)
        d = bind_rows(new, filter(d, plan == "old"), old_k) |> summarise(dem = sum(dem), rep = sum(rep), .by = c(plan, cd))
      } else if (nrow(k)) {
        d = bind_rows(d, k |>
          inner_join(select(cw, -county_fips), by = "county_cbs", relationship = "many-to-many") |>
          summarise(dem = sum(dem * share), rep = sum(rep * share), .by = c(plan, cd))) |>
          summarise(dem = sum(dem), rep = sum(rep), .by = c(plan, cd))
      }
      n_cbs = n_distinct(k$county_cbs)
      t_all = c(as.POSIXct(NA), if (nrow(e) && "time" %in% names(e)) max(e$time), if (nrow(k)) max(k$as_of)) # POSIXct first: c() keeps its class
      pct_cbs = if (nrow(k)) with(filter(k, pct_exp > 0), sum(total) / sum(total / (pct_exp / 100))) else NA_real_
      sid = paste0("count:", ct)
      add(
        mutate(d, state = st, src = sid) |> left_join(mutate(sole, plan = "new"), by = c("plan", "cd")) |>
          select(state, src, plan, cd, dem, rep, sole),
        tibble(state = st, src = sid, kind = "count", label = rname, race_id = if (ct == "house") NA_character_ else paste(st, ct, sep = "_"),
               race = paste(st, rname),
               time = if (any(!is.na(t_all))) max(t_all, na.rm = TRUE) else as.POSIXct(NA),
               p_prec = pprec(cu, ck),
               note = paste0(
                 glue("Counted {rname} vote"),
                 if (nrow(v)) glue(" · precinct results in {length(scraped)} scraped {sp(length(scraped), 'county')}") else "",
                 if (n_cbs) glue(" · CBS county results in {n_cbs} {if (nrow(v)) 'other ' else ''}{sp(n_cbs, 'county')}",
                                 "{if (is.finite(pct_cbs)) paste0(' (', scales::percent(pct_cbs, 1), ' of expected)') else ''}") else "",
                 if (length(unc)) glue(" · {length(unc)} uncontested {sp(length(unc), 'district')} split by the {REDIST_RACE_NAME[[contest]]} vote") else ""
               )),
        mutate(filter(cu, unit_id %in% gid), state = st, src = sid, .before = 1),
        mutate(ck, state = st, src = sid, .before = 1)
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
    mu = attr(d, "units")
    mk = attr(d, "counties")
    d = bind_rows(lapply(split(d, d$model), coverage, st = st)) |>
      mutate(state = st, src = paste("model", race_id, model, sep = ":"))
    pp = full_join(summarise(mu, a = sum(dem + rep), .by = model), summarise(mk, b = sum(dem + rep), .by = model), by = "model") |>
      transmute(model, p_prec = coalesce(a, 0) / (coalesce(a, 0) + coalesce(b, 0)))
    # map only: the counties a model projects no precinct in get the top-of-ticket race's counted
    # votes (ENR precincts summed to the county where we scrape, CBS county results elsewhere),
    # flagged `reported`; the district totals stay projections only
    rc = bind_rows(
      if (nrow(e_st)) e_st |> filter(contest == .env$contest) |> transmute(county_cbs, dem, rep),
      if (nrow(c_st)) c_st |> filter(contest == .env$contest, !county_cbs %in% e_st$county_cbs) |> transmute(county_cbs, dem, rep),
      tibble(county_cbs = integer(), dem = numeric(), rep = numeric())
    ) |>
      summarise(dem = sum(dem), rep = sum(rep), .by = county_cbs) |>
      filter(dem + rep > 0) |>
      inner_join(cmap, by = "county_cbs")
    mr = cross_join(distinct(mv, model), rc) |>
      anti_join(distinct(mv, model, county_cbs), by = c("model", "county_cbs")) |>
      transmute(model, county_fips, dem, rep, reported = TRUE)
    mk = bind_rows(mutate(mk, reported = FALSE), mr)
    n_rep = count(mr, model, name = "n_rep")
    s = distinct(mv, model, time) |>
      summarise(time = max(time), .by = model) |>
      inner_join(placed, by = "model") |>
      left_join(pp, by = "model") |>
      semi_join(d, by = "model") |>
      left_join(parts, by = "model") |>
      left_join(n_rep, by = "model") |>
      transmute(state = st, src = paste("model", race_id, model, sep = ":"), kind = "model", label = model,
                race_id, race, model, model_base, model_spec, model_interval, spec_default, interval_default, time, p_prec,
                note = paste0("Projected ", race, " vote, not the House vote",
                              if_else(coalesce(n_rep, 0L) > 0, paste0(" · map: counted votes in ", n_rep, " unprojected ",
                                                                      if_else(n_rep == 1, "county", "counties")), "")))
    tag = \(x) mutate(x, state = st, src = paste("model", race_id, model, sep = ":"), .before = 1) |> select(-model)
    add(select(d, state, src, plan, cd, dem, rep, cov), s, tag(filter(mu, unit_id %in% gid)), tag(mk))
  }

  desc = if (requireNamespace("fourthirtyfive", quietly = TRUE)) {
    tryCatch(fourthirtyfive::cd_names(2027) |> distinct(cd, .keep_all = TRUE) |> transmute(cd, desc = cd_description),
             error = function(e) NULL)
  }
  list(
    dist = bind_rows(tibble(state = character(), src = character(), plan = character(), cd = character(), dem = numeric(), rep = numeric(),
                            cov = numeric(), sole = character()), dist) |>
      mutate(across(c(dem, rep), round)),
    src = bind_rows(tibble(state = character(), src = character(), kind = character(), label = character(), time = as.POSIXct(character()),
                           note = character(), p_prec = numeric()), lapply(src, \(x) mutate(x, note = as.character(note)))),
    # the map's fills: votes placed by precinct (unit_id = the dashboard's VoteHub precincts) and by county
    unit = bind_rows(tibble(state = character(), src = character(), unit_id = character(), dem = numeric(), rep = numeric()), unit) |>
      mutate(across(c(dem, rep), round)) |> filter(dem + rep > 0),
    # (reported: a model source's county shown with counted votes, no projection there)
    cty = bind_rows(tibble(state = character(), src = character(), county_fips = character(), dem = numeric(), rep = numeric(),
                           reported = logical()), cty) |>
      mutate(across(c(dem, rep), round), reported = coalesce(reported, FALSE)) |> filter(dem + rep > 0),
    desc = desc
  )
}

# The tab's counted-vote districts as a CSV in Dropbox, so scripts off this machine (charts,
# tables) use the dashboard's own numbers: model_results/redistricting/
# redistricting_districts_dashboard_latest.csv, one row per state x race (src: count:house,
# count:sen, count:gov) x plan (old = 2024 map, new = 2026 map) x district, with dem, rep,
# sole (the only major party in an uncontested House race: it wins the 2026-map seat) and as_of
# (the source's latest count, ET). Rewritten every build.
redist_export <- function(redist) {
  out = file.path(find_26_general(), "model_results", "redistricting", "redistricting_districts_dashboard_latest.csv")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  times = redist$src |> dplyr::filter(kind == "count") |>
    dplyr::transmute(state, src, as_of = format(time, "%Y-%m-%d %H:%M", tz = "America/New_York"))
  redist$dist |>
    dplyr::filter(startsWith(src, "count:")) |>
    dplyr::select(state, src, plan, cd, dem, rep, sole) |>
    dplyr::left_join(times, by = c("state", "src")) |>
    readr::write_csv(out)
  out
}
