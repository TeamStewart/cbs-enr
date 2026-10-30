# ========================================
## redistricting_compare.R -- congressional districts under the OLD map (119th Congress,
## used in 2024) and the NEW map (120th, used in 2026), two charts per run:
##   1. dumbbell, one row per district: the Republican share of the two-party vote under the
##      old lines (open circle) and the new lines (filled); right = more Republican
##   2. seats by lean category, old map vs new map
## for NC, FL, TX, OH and CA (all redrawn for 2026).
##
## TWO MODES
##
## (a) 2024 baseline (default): the 2024 presidential vote regrouped into each map's
##     districts. Run this first: it also writes the county x district weights that (b) uses.
##   Votes: VoteHub's 2024 presidential results on its 2026 precincts
##     (26_general/input_data/votehub-shapefiles/estimated_past_results.csv, pres24_*), with
##     the precinct polygons ({NC26G,FL26P,TX26P,OH26G}/*.shp). Every precinct in the state.
##     CA: VoteHub has no CA results, so CA uses the Statewide Database's 2024 general
##     precincts (SRPRECs) and their statement of vote, from
##     https://statewidedatabase.org/d20/g24.html, saved to --ca-dir (default
##     26_general/input_data/swdb_g24/):
##       srprec_state_g24_v01_shp.zip                 SRPREC polygons
##       state_g24_sov_data_by_g24_srprec.zip (or .csv)   votes by SRPREC (PRSDEM*, PRSREP*,
##                                                    other PRS* columns = third parties)
##     joined on SRPREC_KEY. "***" (counts suppressed in tiny precincts) count as 0.
##   Districts: both maps from the fourthirtyfive package (Kuriwaki,
##     https://www.shirokuriwaki.com/fourthirtyfive/): cd_boundaries(year = 2025) and
##     cd_boundaries(year = 2027), i.e. Census TIGER cd119 and the 2026 TIGER lines for the
##     redrawn states. --old= / --new= take any district file sf can read instead.
##   Allocation: a precinct split by a district line gives each piece its share of the
##     precinct's AREA. Votes in precincts that touch no district are reported.
##   Checks printed: each map's districts add up to the state; old map vs VoteHub's own 2024
##     district assignment (not CA); old map vs fourthirtyfive's 2025 normal vote.
##   Also writes 26_general/input_data/redistricting/{ST}_county_district_weights.csv:
##     county_fips, cd_new, cd_old, votes, dem24, rep24 -- the 2024 presidential two-party
##     votes in each county x new district x old district (a precinct split by both maps is
##     divided as old share x new share).
##
## (b) Election night, --live: tonight's HOUSE vote, COUNTED SO FAR (not a projection).
##   New districts: the actual House results, each county's votes in each district from
##     CBS's county results for every House race (cbsapi), summed.
##   Old districts (ESTIMATED): each county's House votes in a new district are divided among
##     the old districts that county x new district overlaps, in proportion to its 2024
##     presidential two-party votes in each (the weights file from (a)), keeping the party
##     split. I.e. voters moved between districts are assumed to vote for the parties as they
##     did in the House race they actually had. CBS reports House votes by county, not by
##     precinct, so every county split by a line is an estimate; the charts say how many.
##   Uncontested races (no Democrat or no Republican on the ballot): each county's House
##     votes are split by that county's two-party share in the state's top-of-ticket race
##     tonight (TOP_RACE below: governor, or senate in NC), and by its 2024 presidential share
##     if the top-of-ticket race has no Democrat-vs-Republican matchup either (possible in CA's
##     top-two). The charts list the districts filled each way.
##   Pulls: --pull (default on; --no-pull reads the last saved pulls) fetches each House
##     district and the top-of-ticket race from CBS and saves them to
##     26_general/cbs_api/{env}/redistricting/. --env=live|test (default live). --at="YYYY-mm-dd
##     HH:MM" (ET) uses the last saved pulls at or before that time.
##   Needs the cbsapi package and the CBS API key (as for the models).
##
## Lean categories (charts only): Republican two-party margin, R - D, in points: Safe R >=
##   LEAN_SAFE, Lean R [LEAN_TOSS, LEAN_SAFE), Competitive within LEAN_TOSS, Lean D, Safe D
##   likewise. Display cut points, not estimates; change them below.
## District NUMBERS are compared as numbers: NC-01 old and NC-01 new can cover different
## places. The dumbbell says so.
##
## OUTPUT -- 26_general/model_results/redistricting/ (Dropbox; --out= to change). The
## dashboard reads the *_latest files there:
##   redistricting_dumbbell_{ST}_house_latest.png   one per state, replaced every --live run
##   redistricting_seats_house_latest.png           all states
##   redistricting_districts_house_latest.csv       the numbers: state, cd, plan (old / new),
##                                                  dem, rep, rep2 (Republican two-party
##                                                  share), margin (R - D, points), lean
##   history/..._house_YYYYmmdd_HHMM.*              every --live run, kept (ET time of the
##                                                  latest CBS pull)
##   ..._2024pres.png / .csv                        the 2024 baseline (mode a)
## Every file is written under a temporary name and renamed, so the dashboard never reads a
## half-written one.
##
## ELECTION NIGHT: live/run_night.R starts `--live` in the background after every refresh
## (all five states, same CBS feed as the models; --no-redistricting turns it off). A run
## still going when the next refresh ends is not doubled up (lock file .live_running).
## The pulls run 6 at a time (--cores=); a state that fails is skipped, the others still plot.
##
## BEFORE THE NIGHT (once, so the first results chart straight away) -- also in live/README.md:
##   1. remotes::install_github("kuriwaki/fourthirtyfive")  (plus sf, ggplot2, cbsapi + key)
##   2. CA's Statewide Database files in 26_general/input_data/swdb_g24/ (mode a above)
##   3. Rscript scripts/model/reports/redistricting_compare.R
##        mode (a) for all five states, ~15 min: writes the weights in
##        26_general/input_data/redistricting/ that --live needs, and the 2024 charts
##   4. Rscript scripts/model/reports/redistricting_compare.R --live --env=test
##        a dry run on CBS's test feed: check the *_house_latest files appear
##
## Needs sf and ggplot2; fourthirtyfive for (a) unless --old and --new are given.
## From the cbs-enr folder:
##   Rscript scripts/model/reports/redistricting_compare.R                 # (a), all five states
##   Rscript scripts/model/reports/redistricting_compare.R CA              # (a), one state
##   Rscript scripts/model/reports/redistricting_compare.R --live          # (b), pull and plot
##   Rscript scripts/model/reports/redistricting_compare.R CA --live --env=test   # test feed
# ========================================
suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(ggplot2)
})
source("scripts/model/live/R/paths.R")
sf_use_s2(FALSE)

LEAN_SAFE = 10 # margin (points) at or beyond which a district is "Safe"
LEAN_TOSS = 3 # margin within which a district is "Competitive"
SPLIT_MIN = 0.01 # a piece below this share of its precinct / county x district is a boundary sliver
STATE_FIPS = c(NC = "37", FL = "12", TX = "48", OH = "39", CA = "06")
VH_DIR = c(NC = "NC26G", FL = "FL26P", TX = "TX26P", OH = "OH26G", CA = NA) # CA: Statewide Database
TOP_RACE = c(NC = "sen", FL = "gov", TX = "gov", OH = "gov", CA = "gov") # fills uncontested House races
TOP_NAME = c(gov = "governor", sen = "US Senate")

args = commandArgs(trailingOnly = TRUE)
opt = function(k, d = NA) {
  v = sub(paste0("^--", k, "="), "", grep(paste0("^--", k, "="), args, value = TRUE))
  if (length(v)) v[1] else d
}
LIVE = "--live" %in% args
PULL = !"--no-pull" %in% args
ENV = opt("env", "live")
AT = if (is.na(opt("at"))) NULL else as.POSIXct(opt("at"), tz = "America/New_York")
states = toupper(grep("^--", args, value = TRUE, invert = TRUE))
if (!length(states)) states = names(STATE_FIPS)
if (length(bad <- setdiff(states, names(VH_DIR)))) stop("Not set up for: ", paste(bad, collapse = ", "))
d26 = find_26_general()
# the dashboard picks the charts up here (file header, OUTPUT)
out_dir = path.expand(opt("out", file.path(d26, "model_results", "redistricting")))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
PULL_CORES = as.integer(opt("cores", "6")) # House districts pulled at once (web requests)
# one --live run at a time: run_night.R starts one after every refresh, and a slow one must
# not pile up behind the next (a lock older than 20 minutes is a crashed run and is ignored)
if (LIVE) {
  lock = file.path(out_dir, ".live_running")
  if (file.exists(lock) && difftime(Sys.time(), file.mtime(lock), units = "mins") < 20) {
    cat("redistricting_compare.R --live is already running (", lock, "); skipped\n", sep = "")
    quit(save = "no")
  }
  invisible(file.create(lock))
  invisible(reg.finalizer(globalenv(), function(e) unlink(lock), onexit = TRUE))
}
vh_dir = file.path(d26, "input_data", "votehub-shapefiles")
ca_dir = path.expand(opt("ca-dir", file.path(d26, "input_data", "swdb_g24")))
w_dir = file.path(d26, "input_data", "redistricting")

lean = function(rep2) {
  m = 100 * (2 * rep2 - 1) # R - D margin, points
  cut(m, c(-Inf, -LEAN_SAFE, -LEAN_TOSS, LEAN_TOSS, LEAN_SAFE, Inf),
      labels = c("Safe D", "Lean D", "Competitive", "Lean R", "Safe R"), right = FALSE)
}

# ==== (a) 2024 baseline: precinct overlay =====================================================

#' CA precincts with 2024 presidential votes, from the Statewide Database (file header)
ca_precincts = function() {
  zshp = list.files(ca_dir, "^srprec_state_g24.*shp.*\\.zip$", full.names = TRUE)[1]
  fsov = list.files(ca_dir, "^state_g24_sov_data_by_g24_srprec.*\\.(zip|csv|dbf)$", full.names = TRUE)[1]
  if (is.na(zshp) || is.na(fsov)) {
    stop("CA needs srprec_state_g24_v01_shp.zip and state_g24_sov_data_by_g24_srprec.(zip|csv) in ", ca_dir,
         " (https://statewidedatabase.org/d20/g24.html), or --ca-dir=")
  }
  shp = read_sf(paste0("/vsizip/", zshp))
  sov = if (grepl("\\.zip$", fsov)) {
    inner = utils::unzip(fsov, list = TRUE)$Name
    inner = inner[grepl("\\.(csv|dbf)$", inner, ignore.case = TRUE)][1]
    if (grepl("\\.dbf$", inner, ignore.case = TRUE)) {
      st_drop_geometry(read_sf(paste0("/vsizip/", fsov, "/", inner)))
    } else {
      readr::read_csv(unz(fsov, inner), col_types = readr::cols(.default = "c"), progress = FALSE)
    }
  } else if (grepl("\\.dbf$", fsov)) {
    st_drop_geometry(read_sf(fsov))
  } else {
    readr::read_csv(fsov, col_types = readr::cols(.default = "c"), progress = FALSE)
  }
  key = intersect(c("SRPREC_KEY", "srprec_key"), intersect(names(shp), names(sov)))[1]
  if (is.na(key)) stop("No SRPREC_KEY column in both CA files. Shapefile: ", paste(names(shp), collapse = " "),
                       " | SOV: ", paste(head(names(sov), 15), collapse = " "))
  prs = grep("^PRS[A-Z]{3}[0-9]*$", names(sov), value = TRUE)
  dcol = grep("^PRSDEM", prs, value = TRUE)
  rcol = grep("^PRSREP", prs, value = TRUE)
  if (!length(dcol) || !length(rcol)) stop("No PRSDEM / PRSREP columns in the CA statement of vote: ", paste(head(names(sov), 40), collapse = " "))
  # "***" = counts the state suppresses in very small precincts (privacy); counted as 0
  num = function(cols) rowSums(sapply(cols, \(k) suppressWarnings(as.numeric(sov[[k]]))), na.rm = TRUE)
  cat(sprintf("CA statement of vote: %d of %d precincts have suppressed (***) presidential counts (TOTVOTE there: %s)\n",
              sum(sov[[dcol[1]]] == "***", na.rm = TRUE), nrow(sov),
              format(sum(suppressWarnings(as.numeric(sov$TOTVOTE[sov[[dcol[1]]] %in% "***"])), na.rm = TRUE), big.mark = ",")))
  v = tibble(
    unit_id = as.character(sov[[key]]),
    pres24_dem = num(dcol),
    pres24_rep = num(rcol),
    pres24_oth = if (length(setdiff(prs, c(dcol, rcol)))) num(setdiff(prs, c(dcol, rcol))) else 0
  ) |>
    summarize(across(everything(), sum), .by = unit_id) |>
    mutate(pres24_total = pres24_dem + pres24_rep + pres24_oth, cong24_district = NA_character_)
  cat(sprintf("CA statement of vote: Harris %s, Trump %s, others %s (compare with the official canvass)\n",
              format(sum(v$pres24_dem), big.mark = ","), format(sum(v$pres24_rep), big.mark = ","), format(sum(v$pres24_oth), big.mark = ",")))
  out = shp |> transmute(unit_id = as.character(.data[[key]])) |> inner_join(v, by = "unit_id")
  miss = sum(v$pres24_total[!v$unit_id %in% out$unit_id])
  cat(sprintf("CA: votes in SRPRECs with no polygon: %s\n", format(miss, big.mark = ",")))
  out
}

#' District polygons for one state and plan, with a `cd` column like "NC-01"
districts = function(st, plan) {
  f = opt(plan)
  if (!is.na(f)) {
    f = path.expand(f)
    x = read_sf(if (grepl("\\.zip$", f)) paste0("/vsizip/", f) else f)
    if ("STATEFP" %in% names(x)) x = filter(x, STATEFP == STATE_FIPS[[st]])
    col = c(grep("^CD[0-9]+FP$", names(x), value = TRUE), intersect("cd", names(x)))[1]
    if (is.na(col)) stop("No district column in ", f)
    num = suppressWarnings(as.integer(sub("^[A-Z]{2}-", "", x[[col]])))
    return(transmute(x, cd = sprintf("%s-%02d", st, num)) |> filter(!is.na(num)))
  }
  if (!requireNamespace("fourthirtyfive", quietly = TRUE)) {
    stop("Install fourthirtyfive (remotes::install_github(\"kuriwaki/fourthirtyfive\")) or give --old= and --new=")
  }
  fourthirtyfive::cd_boundaries(year = if (plan == "old") 2025 else 2027, state = st) |> select(cd)
}

#' Projected (EPSG 5070, metres), coordinates snapped to 1 m and repaired: reprojection and
#' source-file defects (CA's SRPREC file has wrongly wound rings) otherwise make GEOS fail
#' with "TopologyException: side location conflict" in the overlay
clean_geom = function(x) {
  x |> st_make_valid() |> st_transform(5070) |> st_set_precision(1) |> st_make_valid()
}

#' Precinct x district pieces: one overlay per district, so a geometry GEOS still rejects
#' is retried on a zero-width buffer of that district's precincts instead of stopping the run
overlay = function(prec, dist) {
  bind_rows(lapply(seq_len(nrow(dist)), function(j) {
    d = dist[j, ]
    p = prec[lengths(st_intersects(prec, d)) > 0, ]
    if (!nrow(p)) return(NULL)
    out = tryCatch(suppressWarnings(st_intersection(p, d)), error = function(e) NULL)
    if (is.null(out)) {
      out = suppressWarnings(st_intersection(st_buffer(p, 0), st_buffer(d, 0)))
      cat(sprintf("    %s: overlay retried on buffered geometry\n", d$cd))
    }
    out
  }))
}

#' One state's 2024 overlay: district totals per map, precinct pieces, and the county x
#' new district x old district weights (written to w_dir)
baseline_state = function(st, res) {
  if (st == "CA") {
    prec = ca_precincts() |> clean_geom()
  } else {
    shp = list.files(file.path(vh_dir, VH_DIR[[st]]), "\\.shp$", full.names = TRUE)[1]
    prec = read_sf(shp)
    if (!"UNIQUE_ID" %in% names(prec)) stop(basename(shp), " has no UNIQUE_ID column (VoteHub precinct id)")
    prec = prec |>
      clean_geom() |>
      transmute(unit_id = UNIQUE_ID) |>
      inner_join(filter(res, state_abbr == st) |> select(unit_id, pres24_dem, pres24_rep, pres24_oth, pres24_total, cong24_district), by = "unit_id")
  }
  prec$area = as.numeric(st_area(prec))
  if (any(e <- st_is_empty(prec))) {
    cat(sprintf("  %d precincts have no shape after repair (%s votes); left out\n", sum(e), format(sum(prec$pres24_total[e]), big.mark = ",")))
  }
  if (any(z <- prec$area <= 0 & !e)) cat(sprintf("  %d precincts have zero area (%s votes); split equally among the districts they touch\n", sum(z), format(sum(prec$pres24_total[z]), big.mark = ",")))
  st_total = sum(prec$pres24_total)
  cat(sprintf("%s: %d precincts, %s presidential votes\n", st, nrow(prec), format(st_total, big.mark = ",")))
  pv = st_drop_geometry(prec)
  tabs = list()
  sh = list()
  for (plan in c("old", "new")) {
    dist = districts(st, plan) |> clean_geom()
    pc = overlay(select(prec, unit_id, area), dist)
    pc$a = as.numeric(st_area(pc))
    # each precinct's pieces by area, summing to 1; a precinct whose pieces have no area
    # (a sliver or a point after repair) is split equally among the districts it touches
    sh[[plan]] = st_drop_geometry(pc) |>
      mutate(share = if (sum(a) > 0) a / sum(a) else rep(1 / n(), n()), .by = unit_id) |>
      select(unit_id, cd, share)
    tb = sh[[plan]] |>
      inner_join(pv, by = "unit_id") |>
      summarize(dem = sum(share * pres24_dem), rep = sum(share * pres24_rep), .by = cd) |>
      mutate(state = st, plan = plan, .before = 1)
    tot = sh[[plan]] |> inner_join(pv, by = "unit_id") |> summarize(t = sum(share * pres24_total)) |> pull(t)
    cat(sprintf("  %s map: %d districts; votes in precincts touching no district: %s (%.3f%%)\n",
                plan, nrow(tb), format(round(st_total - tot), big.mark = ","), 100 * (st_total - tot) / st_total))
    if (plan == "old" && st != "CA") {
      vh = pv |>
        filter(!is.na(cong24_district), cong24_district != "") |>
        summarize(vd = sum(pres24_dem), vr = sum(pres24_rep), .by = cong24_district) |>
        mutate(cd = sprintf("%s-%02d", st, as.integer(cong24_district)))
      chk = inner_join(tb, vh, by = "cd") |> mutate(gap = 100 * (dem / (dem + rep) - vd / (vd + vr)))
      cat(sprintf("  check vs VoteHub's own 2024 districts: %d matched, largest two-party gap %.2f pts\n",
                  nrow(chk), max(abs(chk$gap))))
    }
    if (plan == "old" && requireNamespace("fourthirtyfive", quietly = TRUE)) {
      nv = fourthirtyfive::cd_normal_vote(year = 2025) |> filter(state == st)
      k = inner_join(tb, nv, by = "cd")
      cat(sprintf("  check vs fourthirtyfive normal vote (2025): correlation %.3f over %d districts\n",
                  cor(k$dem / (k$dem + k$rep), k$normal_vote), nrow(k)))
    }
    tabs[[plan]] = tb
  }
  # county x new district x old district weights for --live
  w = inner_join(rename(sh$new, cd_new = cd, s_new = share), rename(sh$old, cd_old = cd, s_old = share),
                 by = "unit_id", relationship = "many-to-many") |>
    inner_join(pv, by = "unit_id") |>
    mutate(s = s_new * s_old, county_fips = substr(unit_id, 1, 5)) |>
    summarize(dem24 = sum(s * pres24_dem), rep24 = sum(s * pres24_rep), .by = c(county_fips, cd_new, cd_old)) |>
    mutate(votes = dem24 + rep24) |>
    filter(votes > 0)
  dir.create(w_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(w, file.path(w_dir, paste0(st, "_county_district_weights.csv")))
  # split precincts (for the note): a precinct with pieces in two districts of either map
  split_prec = bind_rows(sh) |>
    filter(share >= SPLIT_MIN) |>
    summarize(n = n_distinct(cd), .by = unit_id) |>
    filter(n > 1) |>
    nrow()
  list(tab = bind_rows(tabs), split_prec = split_prec)
}

# ==== (b) election night: CBS county House results =============================================

#' CBS county results for one race: pull and save (PULL) or read the last saved pull
cbs_county = function(st, contest, district = NA) {
  tag = if (is.na(district)) paste0(st, "_", contest) else sprintf("%s_%s_%d", st, contest, district)
  d = file.path(d26, "cbs_api", ENV, "redistricting")
  if (PULL && is.null(AT)) {
    if (!requireNamespace("cbsapi", quietly = TRUE)) stop("--live needs the cbsapi package (or --no-pull to use saved pulls)")
    a = list(as.Date("2026-11-03"), st, contest, district = if (is.na(district)) NULL else district,
             county = TRUE, addl = TRUE, env = ENV)
    if ("by_method" %in% names(formals(cbsapi::cbs_election_data))) a$by_method = FALSE # county totals
    raw = tryCatch(do.call(cbsapi::cbs_election_data, a), error = function(e) {
      cat(sprintf("  %s: CBS pull failed (%s); using the last saved pull\n", tag, conditionMessage(e)))
      NULL
    })
    if (!is.null(raw) && nrow(raw)) {
      x = tibble(
        county_fips = stringr::str_pad(as.character(raw$fips), 5, pad = "0"),
        candidate = as.character(raw$candidate),
        party = as.character(raw$party),
        votes = as.numeric(raw$votes),
        pct_exp = if ("pctExpVote" %in% names(raw)) as.numeric(raw$pctExpVote) else NA_real_
      ) |>
        filter(!is.na(raw$fips), nzchar(as.character(raw$fips)))
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      nanoparquet::write_parquet(x, file.path(d, sprintf("%s_%s.parquet", tag, format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC"))))
    }
  }
  fs = list.files(d, paste0("^", tag, "_[0-9]{8}_[0-9]{6}\\.parquet$"), full.names = TRUE)
  ts = as.POSIXct(stringr::str_extract(basename(fs), "[0-9]{8}_[0-9]{6}"), format = "%Y%m%d_%H%M%S", tz = "UTC")
  if (!is.null(AT)) {
    fs = fs[ts <= AT]
    ts = ts[ts <= AT]
  }
  if (!length(fs)) return(NULL)
  x = nanoparquet::read_parquet(fs[which.max(ts)])
  x$party = case_when(
    tolower(x$party) %in% c("d", "dem", "democrat", "democratic") ~ "dem",
    tolower(x$party) %in% c("r", "rep", "gop", "republican") ~ "rep",
    TRUE ~ "oth"
  )
  attr(x, "as_of") = max(ts)
  x
}

#' One state's House results by new district (actual) and old district (estimated)
live_state = function(st) {
  wf = file.path(w_dir, paste0(st, "_county_district_weights.csv"))
  if (!file.exists(wf)) stop(basename(wf), " not found: run this script without --live for ", st, " first")
  w = readr::read_csv(wf, col_types = readr::cols(county_fips = "c", cd_new = "c", cd_old = "c", .default = "d"), progress = FALSE)
  cds = sort(unique(w$cd_new))
  top = cbs_county(st, TOP_RACE[[st]])
  top_ok = !is.null(top) && all(c("dem", "rep") %in% top$party)
  top_s = if (top_ok) {
    top |> summarize(td = sum(votes[party == "dem"]), tr = sum(votes[party == "rep"]), .by = county_fips) |>
      mutate(s_top = if_else(td + tr > 0, td / (td + tr), NA_real_)) |> select(county_fips, s_top)
  } else {
    tibble(county_fips = character(), s_top = numeric())
  }
  # 2024 presidential Dem two-party share in each county x new district (last-resort fill)
  s24 = w |> summarize(s24 = sum(dem24) / sum(votes), .by = c(county_fips, cd_new))
  as_of = list()
  fill = list()
  # pull every district at once (each is a web request), then combine in order
  pulled = parallel::mclapply(cds, function(cd) {
    tryCatch(cbs_county(st, "house", as.integer(sub("^[A-Z]{2}-", "", cd))), error = function(e) NULL)
  }, mc.cores = if (.Platform$OS.type == "windows") 1L else PULL_CORES)
  names(pulled) = cds
  h = bind_rows(lapply(cds, function(cd) {
    x = pulled[[cd]]
    if (is.null(x)) {
      cat(sprintf("  %s: no CBS results\n", cd))
      return(NULL)
    }
    as_of[[cd]] <<- attr(x, "as_of")
    p = x |> summarize(hd = sum(votes[party == "dem"]), hr = sum(votes[party == "rep"]), ht = sum(votes),
                       pct_exp = first(pct_exp), .by = county_fips) |>
      mutate(cd_new = cd)
    if (!all(c("dem", "rep") %in% x$party)) {
      # uncontested in two-party terms: split the House votes by the top-of-ticket share,
      # else by 2024 president (file header)
      p = p |>
        left_join(top_s, by = "county_fips") |>
        left_join(s24, by = c("county_fips", "cd_new")) |>
        mutate(src = if_else(!is.na(s_top), "top", "pres24"), s = coalesce(s_top, s24, 0.5),
               hd = ht * s, hr = ht * (1 - s)) |>
        select(-s_top, -s24, -s)
      fill[[cd]] <<- if (any(p$src == "top")) "top" else "pres24"
      p = select(p, -src)
    }
    p
  }))
  if (!nrow(h)) stop("No CBS House results for ", st)
  new = h |> summarize(dem = sum(hd), rep = sum(hr), .by = cd_new) |> transmute(state = st, plan = "new", cd = cd_new, dem, rep)
  ws = w |> mutate(f = votes / sum(votes), .by = c(county_fips, cd_new))
  hj = left_join(h, select(ws, county_fips, cd_new, cd_old, f), by = c("county_fips", "cd_new"), relationship = "one-to-many")
  unplaced = hj |> filter(is.na(cd_old)) |> summarize(v = sum(hd + hr)) |> pull(v)
  if (unplaced > 0) cat(sprintf("  %s House votes in county x district pieces our maps don't have; left out of the old districts\n", format(round(unplaced), big.mark = ",")))
  old = hj |> filter(!is.na(cd_old)) |> summarize(dem = sum(hd * f), rep = sum(hr * f), .by = cd_old) |>
    transmute(state = st, plan = "old", cd = cd_old, dem, rep)
  # counties whose House votes are divided among old districts (an estimate)
  est_cty = ws |> filter(f >= SPLIT_MIN) |> summarize(n = n_distinct(cd_old), .by = c(county_fips, cd_new)) |>
    filter(n > 1) |> distinct(county_fips) |> nrow()
  # share of the expected House vote counted: CBS's pctExpVote is 0-100 per county
  pct = h |> filter(pct_exp > 0) |> summarize(ct = sum(ht), ex = sum(ht / (pct_exp / 100))) |> mutate(p = ct / ex)
  cat(sprintf("%s: %d House races, %s House votes counted (%s of expected), as of %s ET\n", st, length(as_of),
              format(round(sum(h$ht)), big.mark = ","), if (is.finite(pct$p)) sprintf("%.0f%%", 100 * pct$p) else "?",
              format(max(do.call(c, as_of)), "%b %d %H:%M", tz = "America/New_York")))
  list(tab = bind_rows(new, old), est_cty = est_cty, fill = fill, top_ok = top_ok,
       as_of = max(do.call(c, as_of)), pct = pct$p)
}

# ==== run ======================================================================================
if (!LIVE) {
  res = readr::read_csv(file.path(vh_dir, "estimated_past_results.csv"), col_types = readr::cols(.default = "c"), progress = FALSE) |>
    mutate(across(c(pres24_dem, pres24_rep, pres24_oth, pres24_total), as.numeric))
  runs = setNames(lapply(states, baseline_state, res = res), states)
} else {
  runs = setNames(lapply(states, function(st) {
    tryCatch(live_state(st), error = function(e) {
      cat(sprintf("%s: skipped (%s)\n", st, conditionMessage(e)))
      NULL
    })
  }), states)
  runs = Filter(Negate(is.null), runs)
  states = names(runs)
  if (!length(runs)) {
    cat("No state has House results to chart yet\n")
    quit(save = "no")
  }
}
tab = bind_rows(lapply(runs, `[[`, "tab")) |>
  mutate(rep2 = rep / (dem + rep), margin = 100 * (2 * rep2 - 1), lean = lean(rep2)) |>
  arrange(state, cd, plan)
# OUTPUT (file header): the 2024 charts as *_2024pres.*; each --live run writes *_house_latest.*
# (replaced every run -- what the dashboard reads) and a time-stamped copy in history/
sfx = if (LIVE) "_house_latest" else "_2024pres"
hist_sfx = if (LIVE) paste0("_house_", format(max(do.call(c, lapply(runs, `[[`, "as_of"))), "%Y%m%d_%H%M", tz = "America/New_York")) else NULL
save_out = function(name, write) {
  # write to a temp name, then rename: the dashboard never reads a half-written file
  f = file.path(out_dir, name)
  tmp = file.path(out_dir, paste0(".tmp_", name))
  write(tmp)
  file.rename(tmp, f)
  if (!is.null(hist_sfx)) {
    dir.create(file.path(out_dir, "history"), showWarnings = FALSE)
    invisible(file.copy(f, file.path(out_dir, "history", sub("_house_latest", hist_sfx, name)), overwrite = TRUE))
  }
}
save_out(paste0("redistricting_districts", sfx, ".csv"), \(p) readr::write_csv(tab, p))

#' The estimate note for one state's charts
note_text = function(st) {
  r = runs[[st]]
  if (!LIVE) {
    return(sprintf("Estimates: precincts split by a district line (%s in %s) have their votes divided by area. Results are by precinct everywhere, so no county is divided as a whole.",
                   format(r$split_prec, big.mark = ","), st))
  }
  f = unlist(r$fill)
  top_name = TOP_NAME[[TOP_RACE[[st]]]]
  paste0(
    sprintf("Old-district figures are estimates. House results are by county, so in counties split by a district line where results are not available at the precinct level, votes are divided among the old districts by 2024 presidential vote, keeping each county's House party split (%s: %d %s). ",
            st, r$est_cty, if (r$est_cty == 1) "county" else "counties"),
    if (length(f)) paste0(
      "Races without both a Democrat and a Republican are filled in from the ", top_name, " race",
      if (any(f == "pres24")) " or, where it has no D-vs-R matchup, the 2024 presidential vote" else "", ": ",
      paste(names(f), collapse = ", "), ".") else ""
  )
}

# ---- 1. dumbbell: one row per district number ------------------------------------------------
names_new = if (requireNamespace("fourthirtyfive", quietly = TRUE)) {
  fourthirtyfive::cd_names(2027) |> select(cd, cd_description) |> distinct(cd, .keep_all = TRUE)
} else {
  tibble(cd = character(), cd_description = character())
}
stamp = function(st) format(runs[[st]]$as_of, "%b %d, %I:%M %p ET", tz = "America/New_York")
for (st in states) {
  w = tab |>
    filter(state == st) |>
    select(cd, plan, rep2) |>
    tidyr::pivot_wider(names_from = plan, values_from = rep2) |>
    left_join(names_new, by = "cd") |>
    mutate(
      change = 100 * (new - old),
      dir = case_when(is.na(old) | is.na(new) ~ "No match", abs(change) < 1 ~ "Moved < 1 pt",
                      change > 0 ~ "Toward R", TRUE ~ "Toward D"),
      label = if_else(is.na(cd_description), cd, paste0(cd, "  ", stringr::str_trunc(cd_description, 34)))
    ) |>
    arrange(desc(coalesce(new, old))) |>
    mutate(label = factor(label, levels = unique(label)))
  g = ggplot(w, aes(y = label)) +
    geom_vline(xintercept = 50, colour = "grey60", linetype = "dashed", linewidth = 0.3) +
    # Republican share: a district moving toward Democrats moves left, toward Republicans right
    geom_segment(aes(x = 100 * old, xend = 100 * new, yend = label, colour = dir), linewidth = 1.2, lineend = "round") +
    geom_point(aes(x = 100 * old), shape = 21, fill = "white", colour = "grey40", size = 2.2, stroke = 0.8) +
    geom_point(aes(x = 100 * new, colour = dir), size = 2.4) +
    scale_colour_manual(values = c("Toward R" = "#d03b3b", "Toward D" = "#2a78d6", "Moved < 1 pt" = "grey60", "No match" = "grey80"), name = NULL) +
    labs(
      title = if (LIVE) {
        sprintf("%s House: Republican share of the two-party vote COUNTED SO FAR, old map vs new map", st)
      } else {
        sprintf("%s: 2024 Trump share of the two-party vote by district, old map vs new map", st)
      },
      subtitle = if (LIVE) {
        sprintf("Votes counted as of %s (%s of expected) -- not a projection.\nOpen circle = 2024 (old) lines, estimated; filled = 2026 (new) lines, actual",
                stamp(st), if (is.finite(runs[[st]]$pct)) sprintf("%.0f%%", 100 * runs[[st]]$pct) else "share unknown")
      } else {
        "Open circle = 2024 (old) lines, filled = 2026 (new) lines"
      },
      x = paste(if (LIVE) "Republican House candidate's share of the two-party vote (%)" else "Trump share of the two-party vote (%)",
                "   \u2190 more Democratic  |  more Republican \u2192"),
      y = NULL,
      caption = paste0(
        "Districts compared by number: an old and a new district with the same number can cover different places.\n",
        stringr::str_wrap(note_text(st), 125), "\n",
        if (LIVE) "Votes: CBS News county results. " else if (st == "CA") "Votes: CA Statewide Database 2024 precinct results. " else "Votes: VoteHub 2024 precinct results. ",
        "Lines: Census TIGER via fourthirtyfive."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top", panel.grid.major.y = element_blank(), plot.caption = element_text(colour = "grey40", hjust = 0),
          plot.title = element_text(size = if (LIVE) 12 else 13))
  save_out(sprintf("redistricting_dumbbell_%s%s.png", st, sfx), \(p) ggsave(p, g, device = "png",
         width = 10, height = 2 + 0.24 * nrow(w), dpi = 200, bg = "white"))
}

# ---- 2. seats by lean category -----------------------------------------------------------------
seats = tab |>
  count(state, plan, lean, .drop = FALSE) |>
  mutate(plan = factor(plan, c("new", "old"), if (LIVE) c("New map (2026), actual", "Old map (2024), estimated") else c("New map (2026)", "Old map (2024)")))
g2 = ggplot(seats, aes(y = plan, x = n, fill = lean)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.6, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = if_else(n > 0, as.character(n), "")), position = position_stack(vjust = 0.5, reverse = TRUE), size = 3.3,
            colour = ifelse(seats$lean == "Competitive", "grey20", "white")) +
  facet_wrap(~state, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c("Safe D" = "#185fa5", "Lean D" = "#85b7eb", "Competitive" = "#b4b2a9", "Lean R" = "#f09595", "Safe R" = "#a32d2d"),
                    breaks = c("Safe D", "Lean D", "Competitive", "Lean R", "Safe R"), name = NULL, drop = FALSE) +
  labs(
    title = if (LIVE) "House seats by Republican two-party share COUNTED SO FAR, old map vs new map" else "Seats by 2024 presidential lean, old map vs new map",
    subtitle = if (LIVE) paste("Votes counted as of", paste(sprintf("%s %s", states, vapply(states, stamp, "")), collapse = "; "), "-- not a projection") else NULL,
    x = "Districts", y = NULL,
    caption = paste0(
      sprintf("Two-party margin: Safe = %d+ pts, Lean = %d-%d pts, Competitive = within %d pts.\n", LEAN_SAFE, LEAN_TOSS, LEAN_SAFE, LEAN_TOSS),
      paste(vapply(states, \(s) stringr::str_wrap(note_text(s), 125), ""), collapse = "\n"), "\n",
      if (LIVE) "Votes: CBS News county results." else "Votes: VoteHub 2024 precinct results (CA: Statewide Database).",
      " Lines: Census TIGER via fourthirtyfive."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid = element_blank(), axis.text.x = element_blank(), plot.caption = element_text(colour = "grey40", hjust = 0))
save_out(paste0("redistricting_seats", sfx, ".png"), \(p) ggsave(p, g2, device = "png", width = 10,
       height = 1.8 + 1.3 * length(states) + 0.45 * length(states), dpi = 200, bg = "white"))

print(tab |> count(state, plan, lean) |> tidyr::pivot_wider(names_from = lean, values_from = n, values_fill = 0))
cat("Wrote", out_dir, "\n")
