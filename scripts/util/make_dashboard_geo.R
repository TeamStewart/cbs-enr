# ========================================
## Precinct maps for the dashboard's Models page (index.qmd): one simplified GeoJSON per
## state, data/dashboard/geo/{st}.json, keyed on VoteHub `unit_id` (feature property `id`),
## with each precinct's point on surface (`cx`, `cy`) for the remaining-vote circles.
##
## Geometry is static for the night, so this runs ONCE, not in the pipeline. The page
## fetches the file at view time (DASHBOARD_GEO_URL in globals.R) instead of embedding it,
## so each dashboard render only carries the per-precinct numbers.
##
## Units kept: every unit_id in input_data/crosswalks/votehub/{st}.csv (ENR precincts) or
## scripts/model/lookups/model_precincts.csv (model precincts). Source layer per state is
## chosen as in scripts/util/crosswalks/votehub.R: the gpkg, unless a per-state
## {ST}26G/{ST}26P layer is newer or the state is not in the gpkg.
##
##   Rscript scripts/util/make_dashboard_geo.R          # every state with a VoteHub crosswalk
##   Rscript scripts/util/make_dashboard_geo.R GA TX    # these states
##   KEEP=0.05 Rscript scripts/util/make_dashboard_geo.R GA
# ========================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
})
source("scripts/util/globals.R")

IN = fs::path(PATH_DROPBOX, "26_general", "input_data")
VH_DIR = fs::path(IN, "votehub-shapefiles")
GPKG = fs::path(VH_DIR, "26_pct_bdrys.gpkg")
OUT = "data/dashboard/geo"
# share of vertices kept by rmapshaper (Visvalingam); shapes are never dropped
KEEP = as.numeric(Sys.getenv("KEEP", "0.02"))

args = toupper(commandArgs(TRUE))
xw_files = fs::dir_ls(fs::path(IN, "crosswalks", "votehub"), regexp = "/[a-z]{2}\\.csv$")
states = if (length(args)) args else toupper(fs::path_ext_remove(fs::path_file(xw_files)))

model_units = readr::read_csv("scripts/model/lookups/model_precincts.csv", col_types = readr::cols(.default = "c"),
                              col_select = c(state, unit_id)) |>
  filter(!is.na(unit_id))

gpkg_states = st_read(GPKG, query = "SELECT DISTINCT state_abbr FROM precincts", quiet = TRUE)$state_abbr

read_layer = function(st) {
  files = fs::dir_ls(VH_DIR, regexp = glue::glue("/{st}26[GP](\\.zip)?$"))
  files = files[!(st %in% gpkg_states) | fs::file_info(files)$modification_time > fs::file_info(GPKG)$modification_time]
  files = files[order(str_sub(fs::path_file(files), 5, 5), fs::path_ext(files) == "zip")]  # G before P, folder before zip
  if (length(files) == 0) {
    return(st_read(GPKG, query = glue::glue("SELECT unit_id, geom FROM precincts WHERE state_abbr = '{st}'"), quiet = TRUE))
  }
  f = files[1]
  layer = fs::path_ext_remove(fs::path_file(f))
  dsn = if (fs::path_ext(f) == "zip") paste0("/vsizip/", f) else f
  st_read(dsn, query = paste("SELECT UNIQUE_ID AS unit_id FROM", layer), quiet = TRUE)
}

# valid, polygon parts only: make_valid and union leave slivers as lines / points in collections
polys = function(x) suppressWarnings(st_collection_extract(st_make_valid(x), "POLYGON"))

fs::dir_create(OUT)
for (st in states) {
  xw = fs::path(IN, "crosswalks", "votehub", glue::glue("{tolower(st)}.csv"))
  units = c(
    if (fs::file_exists(xw)) readr::read_csv(xw, col_types = readr::cols(.default = "c"))$unit_id,
    model_units$unit_id[model_units$state == st]
  ) |> na.omit() |> unique()
  if (length(units) == 0) {
    cli::cli_alert_warning("{st}: no VoteHub units in the crosswalks, skipped")
    next
  }

  shp = read_layer(st) |>
    filter(unit_id %in% units) |>
    polys() |>
    group_by(unit_id) |>
    summarise(.groups = "drop") |>
    polys()
  if (nrow(shp) == 0) {
    cli::cli_alert_warning("{st}: no VoteHub polygons match the crosswalk units, skipped")
    next
  }

  # equal-area CRS for the point on surface; AK spans the antimeridian, so its own
  crs = if (st == "AK") 3338 else 5070
  shp = st_transform(shp, crs)
  # point on surface from the full-detail polygon, matched back by unit_id (simplifying
  # can leave a sliver unit with no polygon part, which st_collection_extract drops)
  pts = st_coordinates(st_transform(st_point_on_surface(st_geometry(shp)), 4326))
  pts = tibble(unit_id = shp$unit_id, cx = round(pts[, 1], 4), cy = round(pts[, 2], 4))
  shp = shp |>
    rmapshaper::ms_simplify(keep = KEEP, keep_shapes = TRUE) |>
    polys() |>
    group_by(unit_id) |>
    summarise(.groups = "drop") |>
    polys() |>
    st_transform(4326) |>
    left_join(pts, by = "unit_id") |>
    transmute(id = unit_id, cx, cy)

  f = fs::path(OUT, glue::glue("{tolower(st)}.json"))
  # geojsonsf, not st_write: GDAL's GeoJSON writer turns some rounded polygons into collections
  writeLines(geojsonsf::sf_geojson(shp, atomise = FALSE, digits = 4), f)
  cli::cli_alert_success("{st}: {nrow(shp)} of {length(units)} units, {fs::file_size(f)} -> {f}")
}
