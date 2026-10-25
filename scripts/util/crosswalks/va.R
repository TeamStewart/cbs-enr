source("scripts/util/crosswalks/helpers.R")

# Build the VA precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md; reference: nc.R.
#
# Frame codes are the 3-digit official precinct number zero-padded to 4
# ("0101"), with names "101 - CHINCOTEAGUE". Independent cities are separate
# counties named "X CITY" (FAIRFAX vs FAIRFAX CITY etc.).

va_frame = read_frame("VA")
va_frame_names = frame_names("VA")
frame_counties = unique(va_frame$county)

# ENR county names come from cbs_counties: cities lack the " CITY" suffix
# unless a county of the same name exists ("RICHMOND CITY"), and King and Queen
# is spelled out.
fix_county = function(county) {
  county = recode(county, "KING AND QUEEN" = "KING & QUEEN")
  if_else(!county %in% frame_counties & paste(county, "CITY") %in% frame_counties, paste(county, "CITY"), county)
}

# ENR precincts are "102-RICH CREEK" (two lack the dash: Wythe "603 EVERGREEN",
# Washington "204 WOODLAND HILLS"). The leading number is the official precinct
# number, i.e. the frame code without zero-padding. A code match is kept only if
# the names also agree (spacing/punctuation ignored); see enr_name_check below.
va_enr_raw = read_enr("VA/clean/VA_latest.parquet") |>
  mutate(
    county = fix_county(county),
    code = str_pad(str_match(precinct, "^\\s*(\\d+)\\s*[- ]")[, 2], 4, pad = "0"),
    enr_name = str_remove(precinct, "^\\s*\\d+\\s*-?\\s*")
  )
stopifnot(all(va_enr_raw$county %in% frame_counties), !anyDuplicated(va_enr_raw[c("county", "code")]))

name_key = function(x) str_remove_all(std_name(x), "[^A-Z0-9]")

enr_name_check = va_enr_raw |>
  inner_join(va_frame_names, join_by(county, code == precinct_yougov)) |>
  mutate(same_name = name_key(enr_name) == name_key(str_remove(frame_name, "^\\s*\\d+\\s*-?\\s*")))
enr_name_check |> filter(!same_name) |> select(county, precinct_enr, frame_name, n_frame) |> print(n = Inf)

# Hand-checked code matches whose names differ. Emporia 601/701 are the only
# candidates, and L2 (2026-10) uses the ENR names with L2/frame ratios ~0.97.
enr_name_ok = tribble(
  ~county,        ~precinct_enr,
  "CAMPBELL",     "402-COURTHOUSE",   # 402 - COURT HOUSE
  "EMPORIA CITY", "601-PRECINCT 6-1", # 601 - DISTRICT 6
  "EMPORIA CITY", "701-PRECINCT 7-1"  # 701 - DISTRICT 7
)

# Frame precincts whose area changed since the frame vintage: a neighbouring
# frame precinct was merged into them (ENR and L2 drop the neighbour, and the
# L2/frame ratio jumps by about its size) or part of them was split off into a
# new ENR/L2 precinct. Both ENR and L2 stay NA for these (rule 3).
merged_split = tribble(
  ~county,        ~precinct,    # what happened
  "BUCHANAN",     "0103",       # + 0102 BLACKEY
  "BUCHANAN",     "0403",       # + 0405 GRUNDY
  "HIGHLAND",     "0001",       # + 0003 WILSONS MILL
  "ROCKBRIDGE",   "0101",       # + 0102 BEN SALEM
  "ROCKBRIDGE",   "0302",       # + 0303 FALLING SPRING
  "ROCKBRIDGE",   "0401",       # + 0403 VESUVIUS
  "RUSSELL",      "0101",       # + part of 0102 COPPER CREEK
  "RUSSELL",      "0103",       # + part of 0102 COPPER CREEK
  "AMHERST",      "0201",       # split: new 203 TOWN OF AMHERST
  "NORFOLK CITY", "0405"        # split: new 416 NORFOLK STATE
)

va_enr = enr_name_check |>
  filter(same_name | paste(county, precinct_enr) %in% paste(enr_name_ok$county, enr_name_ok$precinct_enr)) |>
  distinct(state, county, precinct = code, precinct_enr) |>
  anti_join(merged_split, join_by(county, precinct))

# ========================================
## L2 names -> frame codes
# ========================================
# L2 counties carry " COUNTY"/" CITY" suffixes; frame counties keep only " CITY".
# L2 precinct names use the same "101 - CHINCOTEAGUE" form as the frame names.

va_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/va_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  mutate(county = recode(str_remove(county, " COUNTY$"), "KING AND QUEEN" = "KING & QUEEN")) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)
stopifnot(all(frame_counties %in% va_l2$county))

fr_keys = va_frame_names |>
  mutate(
    k_raw = frame_name, k_std = std_name(frame_name), k_key = name_key(frame_name),
    k_code = precinct_yougov
  )

l2_keys = va_l2 |>
  filter(!is.na(precinct_l2)) |>
  mutate(
    k_raw = precinct_l2, k_std = std_name(precinct_l2), k_key = name_key(precinct_l2),
    k_code = str_pad(str_match(precinct_l2, "^\\s*(\\d+)\\s*[- ]")[, 2], 4, pad = "0")
  )

# `code_name` (leading number only) runs last and is checked by the ratio review.
l2_tiers = tribble(
  ~rule,        ~l2_key,  ~fr_key,
  "name_exact", "k_raw",  "k_raw",
  "name_std",   "k_std",  "k_std",
  "name_key",   "k_key",  "k_key",
  "code_lead",  "k_code", "k_code"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)
review_matches(l2_matched, fr_keys)
l2_matched |> filter(l2_rule != "name_exact") |> select(county, precinct_l2, precinct_yougov, frame_name, l2_rule, n_l2, n_frame) |> print(n = Inf)

va_l2 = va_l2 |>
  select(-precinct) |>
  inner_join(select(l2_matched, county, precinct_l2, precinct = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  select(state, county, precinct, precinct_l2, l2_rule) |>
  anti_join(merged_split, join_by(county, precinct))

crosswalk = va_frame |>
  tidylog::left_join(va_enr, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(va_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(nrow(crosswalk) == nrow(va_frame))

# Leftovers by county, for the Results entry.
crosswalk |> filter(is.na(precinct_enr)) |> left_join(va_frame_names, join_by(county, precinct_yougov)) |>
  select(county, precinct_yougov, frame_name, n_frame) |> print(n = Inf)
anti_join(va_enr_raw, va_enr, join_by(county, precinct_enr)) |> select(county, precinct_enr) |> print(n = Inf)
crosswalk |> summarise(n = n(), enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)), both = mean(!is.na(precinct_enr) & !is.na(precinct_l2))) |> print()

write_crosswalk("VA", crosswalk)
