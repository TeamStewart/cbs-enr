# ========================================
## County outlines for the dashboard's Models page maps (index.qmd): one simplified GeoJSON
## per state, data/dashboard/geo/{st}_counties.json, from Census cartographic boundaries
## (tigris, 1:500k). Features: one per county (property `id` = 5-digit FIPS, `name`), plus
## one with `id` "state", the dissolved state outline. The maps draw the outlines over the
## precincts, use the state outline as the map extent, and fill the counties we don't
## scrape with CBS county results (read_cbs_counties() in scripts/functions.R).
##
## AK: the 40 state house districts (tigris SLDL, `id` 02001-02040), the units CBS and
## the model inputs use as AK's counties (AK_counties.csv).
##
## Static, like make_dashboard_geo.R: run once, not in the pipeline.
##
##   Rscript scripts/util/make_dashboard_counties.R          # every state with a precinct map ({st}.json)
##   Rscript scripts/util/make_dashboard_counties.R MI TX    # these states
##   KEEP=0.1 Rscript scripts/util/make_dashboard_counties.R TX
# ========================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
})
options(tigris_use_cache = TRUE)

OUT = "data/dashboard/geo"
YEAR = 2024
# share of vertices kept by rmapshaper (Visvalingam, topology kept so the outlines line up)
KEEP = as.numeric(Sys.getenv("KEEP", "0.1"))

args = toupper(commandArgs(TRUE))
states = if (length(args)) args else toupper(str_subset(fs::path_ext_remove(fs::path_file(fs::dir_ls(OUT, glob = "*.json"))), "^[a-z]{2}$"))

for (st in states) {
  shp = if (st == "AK") {
    tigris::state_legislative_districts(st, house = "lower", cb = TRUE, year = YEAR, progress_bar = FALSE) |>
      transmute(id = sprintf("02%03d", as.integer(SLDLST)), name = paste("District", as.integer(SLDLST)))
  } else {
    tigris::counties(st, cb = TRUE, resolution = "500k", year = YEAR, progress_bar = FALSE) |>
      transmute(id = GEOID, name = NAME)
  }
  shp = shp |>
    st_transform(4326) |>
    # AK's coast has far more vertices: a fifth of the share gives a file of similar size
    rmapshaper::ms_simplify(keep = if (st == "AK") KEEP / 5 else KEEP, keep_shapes = TRUE)
  state = rmapshaper::ms_dissolve(shp) |> transmute(id = "state", name = st)
  shp = bind_rows(shp, state)

  f = fs::path(OUT, glue::glue("{tolower(st)}_counties.json"))
  writeLines(geojsonsf::sf_geojson(shp, atomise = FALSE, digits = 4), f)
  cli::cli_alert_success("{st}: {nrow(shp) - 1} counties, {fs::file_size(f)} -> {f}")
}
