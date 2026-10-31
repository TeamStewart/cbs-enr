# ========================================
## redistricting_lib.R -- constants and the election-night House functions shared by
## scripts/model/reports/redistricting_compare.R (--live) and the targets pipeline
## (scripts/redistricting_targets.R). See redistricting_compare.R's header for the method.
# ========================================

STATE_FIPS = c(NC = "37", FL = "12", TX = "48", OH = "39", CA = "06")
TOP_RACE = c(NC = "sen", FL = "gov", TX = "sen", OH = "gov", CA = "gov") # fills uncontested House races; the Redistricting tab's race where priors.csv has two (TX)
TOP_NAME = c(gov = "governor", sen = "US Senate")
LEAN_SAFE = 10 # margin (points) at or beyond which a district is "Safe"
LEAN_TOSS = 3 # margin within which a district is "Competitive"
SPLIT_MIN = 0.01 # a piece below this share of its precinct / county x district is a boundary sliver

#' Lean category from the Republican two-party share (display cut points)
lean = function(rep2) {
  m = 100 * (2 * rep2 - 1) # R - D margin, points
  cut(m, c(-Inf, -LEAN_SAFE, -LEAN_TOSS, LEAN_TOSS, LEAN_SAFE, Inf),
      labels = c("Safe D", "Lean D", "Competitive", "Lean R", "Safe R"), right = FALSE)
}

#' CBS county results for one race: pull and save (`pull`) or read the last saved pull
#'
#' Pulls go to 26_general/cbs_api/{env}/redistricting/{tag}_{UTC stamp}.parquet; the newest
#' one (at or before `at`, when given) is returned, with attribute "as_of".
redist_cbs_county = function(st, contest, district = NA, d26 = find_26_general(), env = "live", pull = TRUE, at = NULL) {
  tag = if (is.na(district)) paste0(st, "_", contest) else sprintf("%s_%s_%d", st, contest, district)
  d = file.path(d26, "cbs_api", env, "redistricting")
  if (pull && is.null(at)) {
    if (!requireNamespace("cbsapi", quietly = TRUE)) stop("--live needs the cbsapi package (or --no-pull to use saved pulls)")
    a = list(as.Date("2026-11-03"), st, contest, district = if (is.na(district)) NULL else district,
             county = TRUE, addl = TRUE, env = env)
    if ("by_method" %in% names(formals(cbsapi::cbs_election_data))) a$by_method = FALSE # county totals
    raw = tryCatch(do.call(cbsapi::cbs_election_data, a), error = function(e) {
      cat(sprintf("  %s: CBS pull failed (%s); using the last saved pull\n", tag, conditionMessage(e)))
      NULL
    })
    if (!is.null(raw) && nrow(raw)) {
      x = tibble::tibble(
        county_fips = stringr::str_pad(as.character(raw$fips), 5, pad = "0"),
        candidate = as.character(raw$candidate),
        party = as.character(raw$party),
        votes = as.numeric(raw$votes),
        pct_exp = if ("pctExpVote" %in% names(raw)) as.numeric(raw$pctExpVote) else NA_real_
      ) |>
        dplyr::filter(!is.na(raw$fips), nzchar(as.character(raw$fips)))
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      nanoparquet::write_parquet(x, file.path(d, sprintf("%s_%s.parquet", tag, format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC"))))
    }
  }
  fs = list.files(d, paste0("^", tag, "_[0-9]{8}_[0-9]{6}\\.parquet$"), full.names = TRUE)
  ts = as.POSIXct(stringr::str_extract(basename(fs), "[0-9]{8}_[0-9]{6}"), format = "%Y%m%d_%H%M%S", tz = "UTC")
  if (!is.null(at)) {
    fs = fs[ts <= at]
    ts = ts[ts <= at]
  }
  if (!length(fs)) return(NULL)
  x = nanoparquet::read_parquet(fs[which.max(ts)])
  x$party = dplyr::case_when(
    tolower(x$party) %in% c("d", "dem", "democrat", "democratic") ~ "dem",
    tolower(x$party) %in% c("r", "rep", "gop", "republican") ~ "rep",
    TRUE ~ "oth"
  )
  attr(x, "as_of") = max(ts)
  x
}

#' One state's House results by new district (actual) and old district (estimated)
#'
#' Needs input_data/redistricting/{ST}_county_district_weights.csv (redistricting_compare.R
#' without --live). Returns list(tab = state / plan / cd / dem / rep, est_cty, fill, top_ok,
#' as_of, pct); stops if no House race has results.
redist_live_state = function(st, d26 = find_26_general(), env = "live", pull = TRUE, at = NULL, cores = 6L) {
  w_dir = file.path(d26, "input_data", "redistricting")
  wf = file.path(w_dir, paste0(st, "_county_district_weights.csv"))
  if (!file.exists(wf)) stop(basename(wf), " not found: run redistricting_compare.R without --live for ", st, " first")
  w = readr::read_csv(wf, col_types = readr::cols(county_fips = "c", cd_new = "c", cd_old = "c", .default = "d"), progress = FALSE)
  cds = sort(unique(w$cd_new))
  pull1 = function(contest, district = NA) redist_cbs_county(st, contest, district, d26 = d26, env = env, pull = pull, at = at)
  top = pull1(TOP_RACE[[st]])
  top_ok = !is.null(top) && all(c("dem", "rep") %in% top$party)
  top_s = if (top_ok) {
    top |>
      dplyr::summarize(td = sum(votes[party == "dem"]), tr = sum(votes[party == "rep"]), .by = county_fips) |>
      dplyr::mutate(s_top = dplyr::if_else(td + tr > 0, td / (td + tr), NA_real_)) |>
      dplyr::select(county_fips, s_top)
  } else {
    tibble::tibble(county_fips = character(), s_top = numeric())
  }
  # 2024 presidential Dem two-party share in each county x new district (last-resort fill)
  s24 = w |> dplyr::summarize(s24 = sum(dem24) / sum(votes), .by = c(county_fips, cd_new))
  as_of = list()
  fill = list()
  # pull every district at once (each is a web request), then combine in order
  pulled = parallel::mclapply(cds, function(cd) {
    tryCatch(pull1("house", as.integer(sub("^[A-Z]{2}-", "", cd))), error = function(e) NULL)
  }, mc.cores = if (.Platform$OS.type == "windows") 1L else cores)
  names(pulled) = cds
  h = dplyr::bind_rows(lapply(cds, function(cd) {
    x = pulled[[cd]]
    if (is.null(x)) {
      cat(sprintf("  %s: no CBS results\n", cd))
      return(NULL)
    }
    as_of[[cd]] <<- attr(x, "as_of")
    p = x |>
      dplyr::summarize(hd = sum(votes[party == "dem"]), hr = sum(votes[party == "rep"]), ht = sum(votes),
                       pct_exp = dplyr::first(pct_exp), .by = county_fips) |>
      dplyr::mutate(cd_new = cd)
    if (!all(c("dem", "rep") %in% x$party)) {
      # uncontested in two-party terms: split the House votes by the top-of-ticket share,
      # else by 2024 president (redistricting_compare.R header)
      p = p |>
        dplyr::left_join(top_s, by = "county_fips") |>
        dplyr::left_join(s24, by = c("county_fips", "cd_new")) |>
        dplyr::mutate(src = dplyr::if_else(!is.na(s_top), "top", "pres24"), s = dplyr::coalesce(s_top, s24, 0.5),
                      hd = ht * s, hr = ht * (1 - s)) |>
        dplyr::select(-s_top, -s24, -s)
      fill[[cd]] <<- if (any(p$src == "top")) "top" else "pres24"
      p = dplyr::select(p, -src)
    }
    p
  }))
  if (!nrow(h)) stop("No CBS House results for ", st)
  new = h |>
    dplyr::summarize(dem = sum(hd), rep = sum(hr), .by = cd_new) |>
    dplyr::transmute(state = st, plan = "new", cd = cd_new, dem, rep)
  ws = w |> dplyr::mutate(f = votes / sum(votes), .by = c(county_fips, cd_new))
  hj = dplyr::left_join(h, dplyr::select(ws, county_fips, cd_new, cd_old, f), by = c("county_fips", "cd_new"), relationship = "one-to-many")
  unplaced = hj |> dplyr::filter(is.na(cd_old)) |> dplyr::summarize(v = sum(hd + hr)) |> dplyr::pull(v)
  if (unplaced > 0) cat(sprintf("  %s House votes in county x district pieces our maps don't have; left out of the old districts\n", format(round(unplaced), big.mark = ",")))
  old = hj |>
    dplyr::filter(!is.na(cd_old)) |>
    dplyr::summarize(dem = sum(hd * f), rep = sum(hr * f), .by = cd_old) |>
    dplyr::transmute(state = st, plan = "old", cd = cd_old, dem, rep)
  # counties whose House votes are divided among old districts (an estimate)
  est_cty = ws |>
    dplyr::filter(f >= SPLIT_MIN) |>
    dplyr::summarize(n = dplyr::n_distinct(cd_old), .by = c(county_fips, cd_new)) |>
    dplyr::filter(n > 1) |>
    dplyr::distinct(county_fips) |>
    nrow()
  # share of the expected House vote counted: CBS's pctExpVote is 0-100 per county
  pct = h |>
    dplyr::filter(pct_exp > 0) |>
    dplyr::summarize(ct = sum(ht), ex = sum(ht / (pct_exp / 100))) |>
    dplyr::mutate(p = ct / ex)
  n_races = length(as_of)
  as_of = max(do.call(c, as_of))
  cat(sprintf("%s: %d House races, %s House votes counted (%s of expected), as of %s ET\n", st, n_races,
              format(round(sum(h$ht)), big.mark = ","), if (is.finite(pct$p)) sprintf("%.0f%%", 100 * pct$p) else "?",
              format(as_of, "%b %d %H:%M", tz = "America/New_York")))
  list(tab = dplyr::bind_rows(new, old), est_cty = est_cty, fill = fill, top_ok = top_ok, as_of = as_of, pct = pct$p)
}
