source("scripts/util/crosswalks/helpers.R")

# One-time: MN 2024 presidential results estimated onto the 2026 precincts
# (MN26G.zip, built by votehub_mn.R) and appended to
# input_data/votehub-shapefiles/estimated_past_results.csv, which VoteHub
# shipped without MN. Source: MN SoS 2024 results by precinct with boundaries,
# 24_general/MN/General_Election_Results_By_Precinct_2024.geojson.
# Votes are split by area: each 2024 precinct gives each 2026 precinct the
# share of its area they overlap. Only pres24_* is filled; the other columns
# are blank. Refuses to run if MN rows already exist.
#
#   Rscript scripts/util/crosswalks/votehub_mn_pres24.R

out = election_path("input_data/votehub-shapefiles/estimated_past_results.csv")
est = read_csv(out, col_types = cols(.default = "c"))
if ("MN" %in% est$state_abbr) stop("estimated_past_results.csv already has MN rows")

valid = function(x) suppressWarnings(sf::st_collection_extract(sf::st_make_valid(x), "POLYGON"))

p24 = sf::st_read(fs::path(enightdata:::the$path_dropbox, "24_general/MN/General_Election_Results_By_Precinct_2024.geojson"), quiet = TRUE) |>
  transmute(vtd24 = vtdid, dem = usprsdfl, rep = usprsr, total = usprstotal) |>
  sf::st_transform(5070) |>
  valid()
p24$area24 = as.numeric(sf::st_area(p24))
p26 = sf::st_read(paste0("/vsizip/", election_path("input_data/votehub-shapefiles/MN26G.zip")),
                  query = "SELECT UNIQUE_ID FROM MN26G", quiet = TRUE) |>
  sf::st_transform(5070) |>
  valid()

pieces = suppressWarnings(sf::st_intersection(p24, p26))
pieces$w = as.numeric(sf::st_area(pieces)) / pieces$area24
pieces = sf::st_drop_geometry(pieces)

# 2024 precincts left with less than all of their votes (area outside every 2026 precinct)
lost = summarise(pieces, w = sum(w), .by = c(vtd24)) |>
  right_join(sf::st_drop_geometry(p24), join_by(vtd24)) |>
  mutate(w = coalesce(w, 0), lost = total * (1 - pmin(w, 1)))
cli::cli_alert_info("2024 presidential votes not placed: {round(sum(lost$lost))} of {sum(p24$total)}")
print(as_tibble(lost) |> filter(lost >= 1) |> arrange(desc(lost)) |> select(vtd24, total, w, lost), n = 10)

mn = pieces |>
  summarise(across(c(dem, rep, total), \(x) sum(x * w)), .by = UNIQUE_ID) |>
  right_join(sf::st_drop_geometry(p26), join_by(UNIQUE_ID)) |>
  mutate(across(c(dem, rep, total), \(x) round(coalesce(x, 0))))
cli::cli_alert_info("2026 precincts with no 2024 votes: {sum(mn$total == 0)} of {nrow(mn)}")

rows = mn |>
  transmute(unit_id = UNIQUE_ID, state_abbr = "MN", geography = "general",
            pres24_dem = dem, pres24_rep = rep, pres24_oth = pmax(total - dem - rep, 0), pres24_total = total) |>
  mutate(across(everything(), as.character)) |>
  arrange(unit_id)
rows = bind_rows(est[0, ], rows)
cli::cli_alert_info("MN pres24: dem {sum(as.numeric(rows$pres24_dem))}, rep {sum(as.numeric(rows$pres24_rep))}, total {sum(as.numeric(rows$pres24_total))}")

fs::file_copy(out, paste0(out, ".bak"), overwrite = TRUE)
write_csv(rows, out, na = "", append = TRUE, eol = "\r\n")
cli::cli_alert_success("Appended {nrow(rows)} MN rows to {out} (backup: estimated_past_results.csv.bak)")
