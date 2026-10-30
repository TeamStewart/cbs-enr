source("scripts/util/crosswalks/helpers.R")

# Riverside only: aggregate the YouGov frame up to Riverside's consolidated
# election precincts so it can be merged with ENR. Same exception to rule 3
# ("merges stay NA") as ca_sandiego_consolidated.R and ca_solano_consolidated.R;
# `ca.csv` keeps Riverside ENR NA and this script writes separate files instead.
#
# Riverside ENR (LiveVoterTurnout) reports 1,206 consolidations named after one
# base precinct ("101100") covering 3,433 frame base precincts. Evidence they are
# consolidations (2026-10-30): only 969 of the 1,206 ids are frame bases; the
# other 2,464 frame bases (914k of 1.43M frame voters) have no ENR id; frame bases
# that are ENR ids are larger (median 613 voters vs 367); the VoteHub CA26G layer
# has the same 1,206 units ("Precinct 101-100").
#
# Membership is SPATIAL, not an official list. No 2026 consolidation-to-base list
# was found (voteinfo.net, 2026-10-30; Riverside uses vote centers). Base precinct
# geometry: Statewide Database 2026 primary MPREC (registration precincts,
# statewidedatabase.org/d20/p26_geo_conv.html, mprec_065_p26_v01.gpkg), which has
# all 3,433 frame codes. Consolidation geometry: VoteHub CA26G.
# Rule:
#   - "namesake": a base whose code is an ENR consolidation id goes to that
#     consolidation (1,200 of 1,201 namesakes also win on area).
#   - "area": otherwise the consolidation covering the largest share of the base's
#     area. Voter-weighted mean share is 0.992.
# Checks:
#   - cd_ok (hard): every member base's dominant frame CD equals the ENR house district.
#   - geo_ok (hard): no member base with frame voters is split (best area share
#     < 0.75, e.g. 104120: 64% in 104-121, 35% in 104-124), and no split base has
#     its runner-up (share >= 0.2) here. Primary-to-general boundary edits, so
#     both consolidations involved are unreliable.
# ENR has no per-precinct registration and the votes are still 0 (2026-10-30), so
# recheck on real results: ballots per consolidation should be <= and roughly
# proportional to n.
#
# Aggregation as in San Diego: n is summed; every other frame variable is a mean
# weighted by n.

frame_vars = c("turnout20", "turnout22", "turnout24", "turnoutPred", "age", "male",
               "pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth", "pr_harris", "pr_trump")
split_share = 0.75

rv_frame = enightdata::read_local("input_data/yougov_precinct_frame.parquet", parquet = TRUE) |>
  as_tibble() |>
  filter(state == "CA", county_name == "RIVERSIDE")
stopifnot(all(str_detect(rv_frame$precinct26_id, "^\\d{6}$")))

cons = enightdata::read_local("CA/clean/CA_riverside_latest.parquet", parquet = TRUE) |>
  as_tibble() |>
  filter(contest == "house") |>
  distinct(geo, cd_enr = district) |>
  mutate(precinct_enr = str_remove(geo, "^ca_\\d+_"))
stopifnot(!anyDuplicated(cons$precinct_enr), all(str_detect(cons$precinct_enr, "^\\d{6}$")))

# ---- spatial overlay: base (SWDB MPREC) x consolidation (VoteHub CA26G) ----
sf::sf_use_s2(FALSE)
base_geo = sf::st_read(election_path("input_data/swdb/mprec_065_p26_v01.gpkg"), quiet = TRUE) |>
  sf::st_transform(3310) |>
  sf::st_make_valid() |>
  mutate(a_base = as.numeric(sf::st_area(geom))) |>
  select(base = PRECINCT, a_base)
cons_geo = sf::st_read(paste0("/vsizip/", election_path("input_data/votehub-shapefiles/CA26G.zip"), "/CA26G.shp"),
                       query = "SELECT UNIQUE_ID FROM CA26G WHERE COUNTY = 'Riverside'", quiet = TRUE) |>
  sf::st_zm() |>
  sf::st_transform(3310) |>
  sf::st_make_valid() |>
  mutate(precinct_enr = str_remove_all(str_remove(UNIQUE_ID, "^06065-:-"), "-")) |> # "06065-:-101-100" -> "101100"
  select(precinct_enr)
stopifnot(setequal(cons_geo$precinct_enr, cons$precinct_enr))

overlap = suppressWarnings(sf::st_intersection(base_geo, cons_geo)) |>
  mutate(share = as.numeric(sf::st_area(geom)) / a_base) |>
  sf::st_drop_geometry() |>
  arrange(base, desc(share)) |>
  summarise(precinct_area = first(precinct_enr), area_share = first(share),
            precinct_2nd = nth(precinct_enr, 2), area_share_2nd = nth(share, 2), .by = base)

members = rv_frame |>
  left_join(overlap, join_by(precinct26_id == base)) |>
  mutate(
    member_rule = case_when(precinct26_id %in% cons$precinct_enr ~ "namesake", !is.na(precinct_area) ~ "area", .default = "none"),
    precinct_enr = if_else(member_rule == "namesake", precinct26_id, precinct_area),
    split = member_rule == "area" & area_share < split_share
  ) |>
  left_join(select(cons, precinct_enr, cd_enr), join_by(precinct_enr)) |>
  mutate(
    cd_match = cd == paste0("CA-", str_pad(cd_enr, 2, pad = "0")),
    base_cd_ok = any(cd_match & n == max(n)), .by = precinct26_id
  )
stopifnot(nrow(members) == nrow(rv_frame), all(members$member_rule != "none"))

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
cat("frame voters:", sum(rv_frame$n), " in use = TRUE consolidations:", sum(consolidated$n[consolidated$use]),
    sprintf("(%.1f%%)", 100 * sum(consolidated$n[consolidated$use]) / sum(rv_frame$n)), "\n")
cat("member rules:\n"); print(count(distinct(members, precinct26_id, member_rule), member_rule))
consolidated |> filter(!use) |> select(precinct_enr, cd, n, n_base, cd_ok, geo_ok, min_area_share) |> print(n = 40)

stopifnot(sum(consolidated$n) == sum(rv_frame$n), !anyDuplicated(consolidated$precinct_enr))

enightdata::write_local(
  select(members, state, county_name, cd, precinct_yougov = precinct26_id, n, precinct_enr, member_rule,
         area_share, precinct_2nd, area_share_2nd, split, base_cd_ok),
  "input_data/crosswalks/ca_riverside_members.csv", parquet = FALSE
)
enightdata::write_local(consolidated, "input_data/crosswalks/ca_riverside_frame_consolidated.csv", parquet = FALSE)
