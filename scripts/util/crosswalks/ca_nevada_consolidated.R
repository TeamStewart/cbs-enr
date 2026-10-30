source("scripts/util/crosswalks/helpers.R")

# Nevada only: aggregate the YouGov frame up to Nevada County's consolidated
# election precincts so it can be merged with ENR. Same exception to rule 3
# ("merges stay NA") as the San Diego, Solano and Riverside builds; `ca.csv` keeps
# Nevada ENR NA and this script writes separate files instead.
#
# Nevada ENR (Clarity) reports 105 consolidations ("CP001-C3A1S1-B23": CPnnn plus
# a ballot-type suffix) covering 386 frame base precincts ("304.05", "311.").
# The CPnnn numbering has no link to base codes, so a 1:1 crosswalk gives nothing.
#
# Membership is SPATIAL, not an official list (none found on
# nevadacountyca.gov, 2026-10-30). Base precinct geometry: Statewide Database
# 2026 primary MPREC (statewidedatabase.org/d20/p26_geo_conv.html,
# mprec_057_p26_v01.gpkg); its codes drop the frame's dot ("304.05" -> "30405",
# "108.1" -> "1081"), except that "332.1" is "33210" (frame lost a trailing zero),
# so the padded form is the fallback. Consolidation geometry: VoteHub CA26G
# ("CP001"). Rule ("area"): each base goes to the consolidation covering the
# largest share of its area. Voter-weighted mean share is about 0.99.
# Checks:
#   - cd_ok (hard): every member base's dominant frame CD equals the ENR house
#     district. Weak here: the county is all CA-03.
#   - geo_ok (hard): no member base with frame voters is split (best area share
#     < 0.75, e.g. 536: 69% in CP061), and no split base has its runner-up
#     (share >= 0.2) here.
# ENR votes are still 0 (2026-10-30); recheck on real results: ballots per
# consolidation should be <= and roughly proportional to n.
#
# Aggregation as in San Diego: n is summed; every other frame variable is a mean
# weighted by n.

frame_vars = c("turnout20", "turnout22", "turnout24", "turnoutPred", "age", "male",
               "pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth", "pr_harris", "pr_trump")
split_share = 0.75

nv_frame = enightdata::read_local("input_data/yougov_precinct_frame.parquet", parquet = TRUE) |>
  as_tibble() |>
  filter(state == "CA", county_name == "NEVADA")
stopifnot(all(str_detect(nv_frame$precinct26_id, "^\\d+\\.\\d*\\.?$")))

cons = enightdata::read_local("CA/clean/CA_Nevada_latest.parquet", parquet = TRUE) |>
  as_tibble() |>
  filter(contest == "house") |>
  distinct(geo, cd_enr = district) |>
  mutate(precinct_enr = str_remove(geo, "^ca_\\d+_"), cp = str_extract(precinct_enr, "^CP\\d{3}"))
stopifnot(!anyDuplicated(cons$cp), !anyNA(cons$cp))

# ---- spatial overlay: base (SWDB MPREC) x consolidation (VoteHub CA26G) ----
sf::sf_use_s2(FALSE)
base_geo = sf::st_read(election_path("input_data/swdb/mprec_057_p26_v01.gpkg"), quiet = TRUE) |>
  sf::st_transform(3310) |>
  sf::st_make_valid() |>
  mutate(a_base = as.numeric(sf::st_area(geom))) |>
  select(base = PRECINCT, a_base)
cons_geo = sf::st_read(paste0("/vsizip/", election_path("input_data/votehub-shapefiles/CA26G.zip"), "/CA26G.shp"),
                       query = "SELECT UNIQUE_ID FROM CA26G WHERE COUNTY = 'Nevada'", quiet = TRUE) |>
  sf::st_zm() |>
  sf::st_transform(3310) |>
  sf::st_make_valid() |>
  mutate(cp = str_remove(UNIQUE_ID, "^06057-:-")) |> # "06057-:-CP001" -> "CP001"
  inner_join(select(cons, cp, precinct_enr), join_by(cp)) |>
  select(precinct_enr)
stopifnot(nrow(cons_geo) == nrow(cons))

overlap = suppressWarnings(sf::st_intersection(base_geo, cons_geo)) |>
  mutate(share = as.numeric(sf::st_area(geom)) / a_base) |>
  sf::st_drop_geometry() |>
  arrange(base, desc(share)) |>
  summarise(precinct_area = first(precinct_enr), area_share = first(share),
            precinct_2nd = nth(precinct_enr, 2), area_share_2nd = nth(share, 2), .by = base)

# frame "304.05" / "311." / "511.02." -> MPREC "30405" / "311" / "51102"; "332.1" -> "3321", else "33210"
id_parts = str_match(nv_frame$precinct26_id, "^(\\d+)\\.(\\d*)\\.?$")
nv_frame = nv_frame |>
  mutate(
    base_plain = paste0(id_parts[, 2], id_parts[, 3]),
    base_pad = paste0(id_parts[, 2], str_pad(id_parts[, 3], if_else(id_parts[, 3] == "", 0L, 2L), "right", "0")),
    base = if_else(base_plain %in% base_geo$base, base_plain, base_pad)
  )
stopifnot(all(nv_frame$base %in% base_geo$base))

members = nv_frame |>
  left_join(overlap, join_by(base)) |>
  mutate(
    member_rule = if_else(is.na(precinct_area), "none", "area"),
    precinct_enr = precinct_area,
    split = member_rule == "area" & area_share < split_share
  ) |>
  left_join(select(cons, precinct_enr, cd_enr), join_by(precinct_enr)) |>
  mutate(
    cd_match = cd == paste0("CA-", str_pad(cd_enr, 2, pad = "0")),
    base_cd_ok = any(cd_match & n == max(n)), .by = precinct26_id
  )
stopifnot(nrow(members) == nrow(nv_frame), all(members$member_rule != "none"))

split_touched = members |>
  filter(split) |>
  distinct(precinct26_id, precinct_enr, precinct_2nd, area_share_2nd) |>
  (\(s) c(s$precinct_enr, s$precinct_2nd[s$area_share_2nd >= 0.2]))()

consolidated = members |>
  summarise(
    n_base = n_distinct(precinct26_id),
    cd_ok = all(base_cd_ok),
    cd_frame_share = sum(n[cd_match]) / sum(n),
    min_area_share = min(if_else(member_rule == "namesake", 1, area_share)),
    across(all_of(frame_vars), \(x) weighted.mean(x, n, na.rm = TRUE)),
    n = sum(n),
    .by = c(state, county_name, precinct_enr, cd_enr)
  ) |>
  mutate(cd = paste0("CA-", str_pad(cd_enr, 2, pad = "0")), geo_ok = !precinct_enr %in% split_touched, use = cd_ok & geo_ok) |>
  select(-cd_enr) |>
  relocate(state, county_name, cd, precinct_enr, n, n_base, use, cd_ok, geo_ok, cd_frame_share, min_area_share) |>
  arrange(precinct_enr)

cat("consolidations in ENR:", nrow(cons), " with frame members:", nrow(consolidated),
    " use = TRUE:", sum(consolidated$use), " (cd_ok", sum(consolidated$cd_ok), ", geo_ok", sum(consolidated$geo_ok), ")\n")
cat("frame voters:", sum(nv_frame$n), " in use = TRUE consolidations:", sum(consolidated$n[consolidated$use]),
    sprintf("(%.1f%%)", 100 * sum(consolidated$n[consolidated$use]) / sum(nv_frame$n)), "\n")
cat("member rules:\n"); print(count(distinct(members, precinct26_id, member_rule), member_rule))
consolidated |> filter(!use) |> select(precinct_enr, cd, n, n_base, cd_ok, geo_ok, min_area_share) |> print(n = 40)

stopifnot(sum(consolidated$n) == sum(nv_frame$n), !anyDuplicated(consolidated$precinct_enr))

enightdata::write_local(
  select(members, state, county_name, cd, precinct_yougov = precinct26_id, precinct_mprec = base, n, precinct_enr, member_rule,
         area_share, precinct_2nd, area_share_2nd, split, base_cd_ok),
  "input_data/crosswalks/ca_nevada_members.csv", parquet = FALSE
)
enightdata::write_local(consolidated, "input_data/crosswalks/ca_nevada_frame_consolidated.csv", parquet = FALSE)
