source("scripts/util/crosswalks/helpers.R")

# Build the NM precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md; reference: va.R.
#
# Frame and L2 use the county's precinct number with different zero padding:
# frame "072" (2 digits in some small counties, e.g. "07"), L2 "072". Frame
# names are "DONA ANA 072", so there is nothing name-based to check. Matching is
# on the integer value within county.
#
# ENR (statewide SOS feed): scrape_nm() puts the county precinct number from the
# feed's PrecinctName into geo ("nm_018_067"; since 2026-10-31, before that geo
# was the state-internal StatePrecinctID). ENR spells De Baca "DEBACA" (cbs_counties).

nm_frame = read_frame("NM")
nm_frame_names = frame_names("NM")
frame_counties = unique(nm_frame$county)

num = function(x) if_else(str_detect(x, "^\\d+$"), as.character(as.integer(x)), NA_character_)

fr_keys = nm_frame_names |>
  mutate(k_num = num(precinct_yougov))
stopifnot(!anyDuplicated(fr_keys[c("county", "k_num")]))

# ========================================
## ENR
# ========================================

nm_enr_raw = read_enr("NM/clean/NM_latest.parquet") |>
  mutate(county = recode(county, "DEBACA" = "DE BACA"), k_num = num(precinct))
stopifnot(all(nm_enr_raw$county %in% frame_counties), !anyDuplicated(filter(nm_enr_raw, !is.na(k_num))[c("county", "k_num")]))

nm_enr = nm_enr_raw |>
  select(-precinct) |>
  filter(!is.na(k_num)) |>
  inner_join(select(fr_keys, county, k_num, precinct = precinct_yougov), join_by(county, k_num)) |>
  distinct(state, county, precinct, precinct_enr)

# ENR leftovers (no frame precinct with that number)
anti_join(nm_enr_raw, nm_enr, join_by(county, precinct_enr)) |> select(county, precinct_enr) |> print(n = Inf)

# ========================================
## L2 codes -> frame codes
# ========================================

nm_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/nm_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  as_tibble() |>
  mutate(county = recode(county, "DEBACA" = "DE BACA")) |>
  filter(county %in% frame_counties) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

l2_keys = nm_l2 |>
  filter(!is.na(precinct_l2)) |>
  mutate(k_num = num(precinct_l2))

l2_tiers = tribble(
  ~rule,      ~l2_key, ~fr_key,
  "code_num", "k_num", "k_num"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)
review_matches(l2_matched, fr_keys)

nm_l2 = nm_l2 |>
  select(-precinct) |>
  inner_join(select(l2_matched, county, precinct_l2, precinct = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = nm_frame |>
  tidylog::left_join(nm_enr, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(nm_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(
  nrow(crosswalk) == nrow(nm_frame),
  crosswalk |> filter(!is.na(precinct_enr)) |> distinct(county, precinct_yougov, precinct_enr) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_enr))) |> pull(a) == 0,
  crosswalk |> filter(!is.na(precinct_l2)) |> distinct(county, precinct_yougov, precinct_l2) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_l2))) |> pull(a) == 0
)

crosswalk |> filter(is.na(precinct_enr) | is.na(precinct_l2)) |> left_join(nm_frame_names, join_by(county, precinct_yougov)) |>
  select(county, precinct_yougov, precinct_enr, precinct_l2, n_frame) |> print(n = Inf)
crosswalk |> summarise(n = n(), enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)), both = mean(!is.na(precinct_enr) & !is.na(precinct_l2))) |> print()

write_crosswalk("NM", crosswalk)
