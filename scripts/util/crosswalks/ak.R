source("scripts/util/crosswalks/helpers.R")

# Build the AK precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference state: nc.R.
#
# AK ids, as of 2026-10-15:
# - Frame: county = borough/census area (30), precinct_yougov = 3-digit precinct
#   number ("590"). These numbers repeat across house districts (e.g. 02-700 and
#   37-700), so they are unique only within county. `frame_name` is the full
#   official "DD-PPP" id (district-precinct), unique statewide.
# - ENR: `geo` is "ak_0DD_DD-PPP". The FIPS slot is the house district (ED01-ED40
#   in enightmodels::cbs_counties), not a borough, so ENR is joined to the frame
#   on frame_name == "DD-PPP" statewide, not within county. ENR also has
#   district-level "DD-absentee", "DD-early" and "DD-question" units, which have
#   no frame counterpart.
# - L2: county = borough without suffix ("ANCHORAGE"); precinct = "NAME (DD-PPP)".
#   Precincts that straddle a borough line appear once per borough as
#   "NAME (DD-PPP) - BOROUGH". The frame assigns each precinct to one borough.

ak_frame = read_frame("AK")

ak_frame_names = frame_names("AK")
stopifnot(
  !anyDuplicated(ak_frame_names$frame_name),
  str_detect(ak_frame_names$frame_name, "^\\d{2}-\\d{3}$"),
  str_sub(ak_frame_names$frame_name, 4, 6) == ak_frame_names$precinct_yougov
)

# ========================================
## ENR
# ========================================

ak_enr_raw = read_enr("AK/clean/AK_latest.parquet")

# The district in the geo FIPS slot equals the district prefix of the id.
stopifnot(with(
  filter(ak_enr_raw, str_detect(precinct, "^\\d{2}-\\d{3}$")),
  str_sub(county, 3, 4) == str_sub(precinct, 1, 2)
))

ak = ak_enr_raw |>
  filter(str_detect(precinct, "^\\d{2}-\\d{3}$")) |>
  select(precinct_enr) |>
  inner_join(
    select(ak_frame_names, county, precinct_yougov, frame_name),
    join_by(precinct_enr == frame_name), relationship = "one-to-one"
  ) |>
  mutate(state = "AK") |>
  select(state, county, precinct = precinct_yougov, precinct_enr)

# ========================================
## L2
# ========================================

# L2 borough names -> frame county names (frame names are truncated at 28 chars).
l2_county = function(x) {
  x |>
    str_remove("^(MUNICIPALITY OF|CITY AND BOROUGH OF) ") |>
    str_remove(" (BOROUGH|CENSUS AREA|CENSUS A|CENSUS)$")
}
county_map = distinct(ak_frame, county) |> mutate(county_l2 = l2_county(county))

ak_l2 = enightdata::read_local("input_data/l2/ak_20260901_precincts.parquet", parquet = TRUE) |>
  as_tibble() |>
  select(state, county_l2 = county, precinct_l2 = precinct, n_l2 = n) |>
  filter(!is.na(precinct_l2))

stopifnot(
  setequal(county_map$county_l2, ak_l2$county_l2),
  !anyDuplicated(ak_l2$precinct_l2) # raw L2 string identifies an L2 row statewide
)

ak_l2 = ak_l2 |>
  left_join(county_map, join_by(county_l2)) |>
  mutate(
    k_paren = str_match(precinct_l2, "\\((\\d{2}-\\d{3})\\)(?: - .*)?$")[, 2]
  ) |>
  mutate(
    # Share of the whole precinct's L2 voters that sit in this borough piece.
    share = n_l2 / sum(n_l2), .by = k_paren
  )
stopifnot(!is.na(ak_l2$k_paren))

fr_keys = ak_frame_names |> mutate(k_raw = frame_name)

# The "(DD-PPP)" code in the L2 name is the frame's official id, unique
# statewide, so match on it statewide rather than with match_tiers() within
# county. Rules:
# - paren_code: the only L2 row for that code, in the frame's county.
# - paren_code_xcounty: the only L2 row for that code, but L2 puts the whole
#   precinct in a different borough than the frame (e.g. 32-375 FORT WAINWRIGHT:
#   frame NORTH SLOPE, L2 FAIRBANKS NORTH STAR). Same official id, so mapped.
# - paren_code_split: precinct straddles a borough line in L2. The frame keeps
#   the whole precinct in one borough; map the L2 piece in that borough only if
#   it holds >= 90% of the precinct's L2 voters, otherwise leave NA.
split_min_share = 0.9

l2_cand = ak_l2 |>
  select(county_l2_side = county, precinct_l2, n_l2, k_paren, share) |>
  inner_join(fr_keys, join_by(k_paren == k_raw)) |>
  mutate(
    n_pieces = n(), .by = k_paren
  ) |>
  mutate(l2_rule = case_when(
    n_pieces == 1 & county_l2_side == county ~ "paren_code",
    n_pieces == 1 ~ "paren_code_xcounty",
    county_l2_side == county ~ "paren_code_split"
  ))

l2_cand |>
  filter(l2_rule %in% c("paren_code_split", "paren_code_xcounty")) |>
  arrange(l2_rule, share) |>
  select(county, county_l2_side, precinct_l2, precinct_yougov, l2_rule, n_l2, n_frame, share) |>
  print(n = Inf)

l2_matched = l2_cand |>
  filter(!is.na(l2_rule), l2_rule != "paren_code_split" | share >= split_min_share) |>
  select(county, precinct_l2, precinct_yougov, frame_name, n_frame, n_l2, l2_rule)

stopifnot(
  !anyDuplicated(l2_matched$precinct_l2),
  !anyDuplicated(l2_matched[c("county", "precinct_yougov")])
)

review_matches(l2_matched, fr_keys)

ak_l2 = l2_matched |>
  mutate(state = "AK") |>
  select(state, county, precinct = precinct_yougov, precinct_l2, l2_rule)

# ========================================
## Crosswalk
# ========================================

crosswalk = ak_frame |>
  tidylog::left_join(ak, join_by(state, county, precinct_yougov == precinct), na_matches = "never", relationship = "one-to-one") |>
  tidylog::left_join(ak_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never", relationship = "one-to-one")

stopifnot(
  nrow(crosswalk) == nrow(ak_frame),
  !anyDuplicated(na.omit(crosswalk$precinct_enr)),
  !anyDuplicated(na.omit(crosswalk$precinct_l2))
)

write_crosswalk("AK", crosswalk)
