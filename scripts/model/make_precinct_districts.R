# ========================================
## make_precinct_districts.R -- each precinct's OLD and NEW congressional district.
##
## For the redistricting checks: a precinct's prediction can be counted toward the district
## it votes in tonight (new map) or the one it voted in in 2024 (old map). This builds, for
## every precinct in our inputs:
##
##   NEW district (2026 map)  from {ST}_precinct_cd.parquet (prep_model_inputs.R): the
##                            voter-file frame's `cd` is "the district the 2026 election
##                            runs in" (codebook), so the new district comes from where the
##                            precinct's REGISTRANTS live -- split precincts carry their
##                            voter share in each piece.
##   OLD district (2024 map)  the 119th Congress districts (Census TIGER/Line 2024, cd119),
##                            overlaid on VoteHub's 2026 precinct polygons: the precinct's
##                            AREA share in each old district. Our precincts join to the
##                            polygons through scripts/model/lookups/precinct_geo_crosswalk.csv
##                            (make_precinct_topo.R). A precinct with no polygon gets its
##                            county's old district when the whole county was in one district
##                            (old_src = "county"), else none (old_src = NA).
##
## Writes, per state, to 26_general/input_data/model_inputs/:
##   {ST}_precinct_districts.parquet   one row per precinct: geo, unit_id, n (registrants),
##        cd_old, cd_old_share, n_cd_old, old_src, cd_new, cd_new_share, n_cd_new, moved
##        (cd_old != cd_new, main districts)
##   {ST}_precinct_district_pieces.parquet   long: geo, plan ("old" / "new"), cd, share,
##        share_of ("area" / "registrants") -- for allocating a split precinct's votes
##   with --map, also {ST}_precinct_map.gpkg: every VoteHub polygon with geo (NA where it
##        matches none of our precincts), n, cd_old, cd_new -- ready for
##        redist::redist_map(map, existing_plan = cd_new, pop = n) once polygons with no
##        registrants are filled or dropped
## and prints the old x new transition table (registrants), so a big change (TX-35, NC-1,
## ...) can be eyeballed against the enacted map.
##
## Inputs (Dropbox 26_general/input_data/):
##   votehub-shapefiles/{NC26G,FL26P,TX26P}/*.shp     precinct polygons (VoteHub)
##   districts/tl_2024_{state fips}_cd119.zip         old districts (Census, left zipped)
##   model_inputs/{ST}_precinct.parquet, {ST}_precinct_cd.parquet
## CA is not covered: there are no CA precinct polygons in Dropbox (make_precinct_topo.R).
##
## Needs the sf package (not used by the live models). From the cbs-enr folder:
##   Rscript scripts/model/make_precinct_districts.R              # NC, FL, TX
##   Rscript scripts/model/make_precinct_districts.R NC           # one state
##   Rscript scripts/model/make_precinct_districts.R NC --old=<any district file sf can read>
# ========================================
suppressPackageStartupMessages({ library(dplyr); library(sf) })
source("scripts/model/live/R/paths.R")
sf_use_s2(FALSE)   # planar overlay in an equal-area projection (below)

args = commandArgs(trailingOnly = TRUE)
opt = function(k, d = NA) { a = grep(paste0("^--", k, "="), args, value = TRUE); if (length(a)) sub("^[^=]*=", "", a[1]) else d }
STATES = toupper(grep("^--", args, value = TRUE, invert = TRUE))
if (!length(STATES)) STATES = c("NC", "FL", "TX")
VH = c(NC = "NC26G", FL = "FL26P", TX = "TX26P", AK = "AK26G")      # VoteHub layer per state
FIPS = c(NC = "37", FL = "12", TX = "48", CA = "06", AK = "02")
CRS_AREA = 5070            # CONUS Albers equal-area: areas in m^2
MIN_PIECE = 0.005          # area pieces under 0.5% of a precinct are slivers from boundary mismatch
COUNTY_WHOLE = 0.99        # a county counts as wholly in one old district above this area share

d26 = find_26_general()
dir_in = file.path(d26, "input_data")
xw_all = readr::read_csv("scripts/model/lookups/precinct_geo_crosswalk.csv", col_types = readr::cols(.default = "c"))

# old districts: the Census file in input_data/districts/ (zipped is fine), or --old=
read_old = function(st) {
  f = opt("old")
  if (is.na(f)) {
    f = Sys.glob(file.path(dir_in, "districts", paste0("tl_*_", FIPS[[st]], "_cd119.*")))[1]
    if (is.na(f)) stop("No old district file for ", st, ": put tl_2024_", FIPS[[st]], "_cd119.zip in ", file.path(dir_in, "districts"))
    if (grepl("\\.zip$", f)) f = paste0("/vsizip/", f)
  }
  d = read_sf(f)
  id = intersect(c("CD119FP", "CD118FP", "DISTRICT", "District", "cd", "CD"), names(d))[1]
  if (is.na(id)) stop("Can't find the district number column in ", f, " (have: ", paste(names(d), collapse = ", "), ")")
  d |> transmute(cd_old = as.integer(.data[[id]])) |> filter(!is.na(cd_old)) |>
    st_transform(CRS_AREA) |> st_make_valid()
}

pieces_area = function(poly, old, key) {
  ov = suppressWarnings(st_intersection(poly, old))
  ov |> mutate(a = as.numeric(st_area(ov))) |> st_drop_geometry() |>
    summarize(a = sum(a), .by = c(all_of(key), cd_old)) |>
    mutate(share = a / sum(a), .by = all_of(key)) |>
    filter(share >= MIN_PIECE) |>
    mutate(share = share / sum(share), .by = all_of(key)) |>
    select(-a)
}

for (st in STATES) {
  cli::cli_h1("{st}")
  if (is.na(VH[st])) { cli::cli_alert_warning("{st}: no VoteHub precinct polygons in Dropbox -- skipped"); next }
  t0 = Sys.time()
  dir_mi = file.path(dir_in, "model_inputs")
  prec = nanoparquet::read_parquet(file.path(dir_mi, paste0(st, "_precinct.parquet")), col_select = c("geo", "county_name", "n")) |>
    mutate(geo = as.character(geo))
  pcd = nanoparquet::read_parquet(file.path(dir_mi, paste0(st, "_precinct_cd.parquet"))) |>
    mutate(geo = as.character(geo), cd = as.integer(cd))
  xw = filter(xw_all, state == st, !is.na(unit_id), unit_id != "") |> distinct(geo, unit_id, county_fips)

  vh = read_sf(file.path(dir_in, "votehub-shapefiles", VH[[st]], paste0(VH[[st]], ".shp"))) |>
    transmute(unit_id = UNIQUE_ID, county_fips = FIPS) |>
    st_transform(CRS_AREA) |> st_make_valid()
  old = read_old(st)

  # old district by precinct polygon (area shares)
  pa = pieces_area(filter(vh, unit_id %in% xw$unit_id), old, "unit_id")
  old_geo = xw |> inner_join(pa, by = "unit_id", relationship = "many-to-many") |>
    summarize(share = sum(share), .by = c(geo, cd_old)) |>
    mutate(share = share / sum(share), .by = geo)

  # counties wholly inside one old district, for precincts with no polygon
  cty = vh |> group_by(county_fips) |> summarize(.groups = "drop")
  ca = pieces_area(cty, old, "county_fips") |> filter(share >= COUNTY_WHOLE)
  # our 3-digit county code is CBS's, not FIPS: map it through the crosswalk's county_fips
  cty_map = xw_all |> filter(state == st) |> distinct(code = stringr::str_extract(geo, "(?<=_)[0-9]{3}(?=_)"), county_fips) |>
    filter(!is.na(county_fips), county_fips != "") |> distinct(code, .keep_all = TRUE)
  nopoly = prec |> filter(!geo %in% old_geo$geo) |>
    transmute(geo, code = stringr::str_extract(geo, "(?<=_)[0-9]{3}(?=_)")) |>
    left_join(cty_map, by = "code") |>
    inner_join(ca, by = "county_fips") |> transmute(geo, cd_old, share)

  old_long = bind_rows(mutate(old_geo, old_src = "polygon"), mutate(nopoly, old_src = "county"))
  new_long = pcd |> transmute(geo, cd_new = cd, share)

  main = function(d, col) d |> arrange(desc(share)) |>
    summarize(!!col := first(.data[[col]]), "{col}_share" := first(share), "n_{col}" := n(), .by = geo)
  out = prec |>
    left_join(distinct(xw, geo, unit_id) |> summarize(unit_id = paste(unit_id, collapse = "|"), .by = geo), by = "geo") |>
    left_join(main(old_long, "cd_old") |> left_join(distinct(old_long, geo, old_src), by = "geo"), by = "geo") |>
    left_join(main(new_long, "cd_new"), by = "geo") |>
    mutate(moved = cd_old != cd_new) |>
    select(geo, county_name, unit_id, n, cd_old, cd_old_share, n_cd_old, old_src, cd_new, cd_new_share, n_cd_new, moved)
  pieces = bind_rows(
    old_long |> transmute(geo, plan = "old", cd = cd_old, share, share_of = "area"),
    new_long |> transmute(geo, plan = "new", cd = cd_new, share, share_of = "registrants"))

  nanoparquet::write_parquet(out, file.path(dir_mi, paste0(st, "_precinct_districts.parquet")))
  nanoparquet::write_parquet(pieces, file.path(dir_mi, paste0(st, "_precinct_district_pieces.parquet")))

  # ---- report -------------------------------------------------------------------------
  n_all = sum(out$n)
  cli::cli_alert_info("{nrow(out)} precincts; old district from a polygon for {sum(out$old_src == 'polygon', na.rm = TRUE)}, from a whole-county fallback for {sum(out$old_src == 'county', na.rm = TRUE)}, missing for {sum(is.na(out$cd_old))} ({round(100 * sum(out$n[is.na(out$cd_old)]) / n_all, 1)}% of registrants)")
  big_split = function(d) d |> summarize(m = max(share), .by = geo) |> filter(m < 0.95) |> nrow()   # main piece under 95%
  cli::cli_alert_info("precincts split 95/5 or closer: {big_split(rename(old_long, cd = cd_old))} on the old map (by area), {big_split(new_long)} on the new (by registrants)")
  cli::cli_alert_info("precincts whose main district changed number: {sum(out$moved, na.rm = TRUE)} ({round(100 * sum(out$n[out$moved %in% TRUE]) / n_all, 1)}% of registrants)")
  tr = out |> filter(!is.na(cd_old), !is.na(cd_new)) |> count(cd_old, cd_new, wt = n, name = "registrants") |>
    mutate(pct_of_new = round(100 * registrants / sum(registrants), 1), .by = cd_new)
  cat("\nWhere each NEW district's registrants were on the OLD map (main pieces >= 5%):\n")
  print(as.data.frame(tr |> filter(pct_of_new >= 5) |> arrange(cd_new, desc(pct_of_new))), row.names = FALSE)
  if ("--map" %in% args) {
    m = vh |> left_join(distinct(xw, unit_id, geo), by = "unit_id") |>
      left_join(select(out, geo, n, cd_old, cd_new), by = "geo") |>
      st_transform(4326)
    write_sf(m, file.path(dir_mi, paste0(st, "_precinct_map.gpkg")), delete_dsn = TRUE)
    cli::cli_alert_success("Wrote {st}_precinct_map.gpkg ({nrow(m)} polygons, {sum(is.na(m$geo))} with no precinct of ours)")
  }
  cli::cli_alert_success("Wrote {st}_precinct_districts.parquet and {st}_precinct_district_pieces.parquet ({round(as.numeric(difftime(Sys.time(), t0, units = 'secs')))} s)")
}
