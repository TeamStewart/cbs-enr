# VoteHub's current precinct layer for one state (sf, column unit_id): the gpkg
# (26_pct_bdrys.gpkg), unless a per-state {ST}26G / {ST}26P folder or zip is newer or the
# state is not in the gpkg; G before P, folder before zip. Shared by
# scripts/util/make_dashboard_geo.R (the dashboard's precinct maps) and
# scripts/model/reports/redistricting_compare.R (district shares for those same precincts),
# so both use the same unit ids.
votehub_layer = function(st, vh_dir) {
  gpkg = file.path(vh_dir, "26_pct_bdrys.gpkg")
  gpkg_states = sf::st_read(gpkg, query = "SELECT DISTINCT state_abbr FROM precincts", quiet = TRUE)$state_abbr
  files = list.files(vh_dir, pattern = paste0("^", st, "26[GP](\\.zip)?$"), full.names = TRUE)
  files = files[!(st %in% gpkg_states) | file.info(files)$mtime > file.info(gpkg)$mtime]
  files = files[order(substr(basename(files), 5, 5), grepl("\\.zip$", files))]
  if (length(files) == 0) {
    return(sf::st_read(gpkg, query = sprintf("SELECT unit_id, geom FROM precincts WHERE state_abbr = '%s'", st), quiet = TRUE))
  }
  f = files[1]
  layer = sub("\\.zip$", "", basename(f))
  dsn = if (grepl("\\.zip$", f)) paste0("/vsizip/", f) else f
  sf::st_read(dsn, query = paste("SELECT UNIQUE_ID AS unit_id FROM", layer), quiet = TRUE)
}
