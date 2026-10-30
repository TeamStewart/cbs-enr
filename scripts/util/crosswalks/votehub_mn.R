source("scripts/util/crosswalks/helpers.R")

# Minnesota precinct boundaries (input_data/votehub-shapefiles/mn-precincts.json,
# MN SoS schema: Precinct, PrecinctID = "27" + county FIPS + 4-digit precinct
# code, County, ...) -> MN26G.zip in the VoteHub per-state schema (COUNTY, FIPS,
# UNIQUE_ID, PCT), so votehub.R and make_dashboard_geo.R read it like the
# other {ST}26G layers. UNIQUE_ID is "{FIPS}-:-{code}"; the code is the ENR
# precinct id. Rerun if mn-precincts.json changes.
#
#   Rscript scripts/util/crosswalks/votehub_mn.R

VH_DIR = "input_data/votehub-shapefiles"

mn = sf::st_read(election_path(VH_DIR, "mn-precincts.json"), quiet = TRUE) |>
  transmute(
    COUNTY = County,
    FIPS = str_sub(PrecinctID, 1, 5),
    UNIQUE_ID = paste0(FIPS, "-:-", str_sub(PrecinctID, 6)),
    PCT = Precinct
  )
# 102 features are GEOMETRYCOLLECTIONs (polygon + line slivers); keep the polygon parts
sf::st_geometry(mn) = sf::st_geometry(mn) |>
  map(\(g) if (inherits(g, "GEOMETRYCOLLECTION")) sf::st_collection_extract(g, "POLYGON") else g) |>
  map(\(g) sf::st_cast(g, "MULTIPOLYGON")) |>
  sf::st_sfc(crs = sf::st_crs(mn))
stopifnot(all(str_detect(mn$UNIQUE_ID, "^27\\d{3}-:-\\d{4}$")), !anyDuplicated(mn$UNIQUE_ID))

tmp = fs::path(tempdir(), "MN26G")
fs::dir_create(tmp)
sf::st_write(mn, fs::path(tmp, "MN26G.shp"), layer_options = "ENCODING=UTF-8", quiet = TRUE)
out = election_path(VH_DIR, "MN26G.zip")
fs::file_delete(out[fs::file_exists(out)])
zip::zip(out, fs::dir_ls(tmp), mode = "cherry-pick")
cli::cli_alert_success("{nrow(mn)} MN precincts -> {out}")
