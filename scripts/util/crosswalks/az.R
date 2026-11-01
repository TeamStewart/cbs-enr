source("scripts/util/crosswalks/helpers.R")

# Build the AZ precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference: nc.R.
#
# ENR is one statewide Enhanced Voting file from 2026-11-01, so it lands as
# `AZ_latest.parquet` with no county segment and covers all 9 frame counties.
#
# Frame ids are 4-digit, zero-padded county precinct numbers ("0009"). Frame
# names, L2 and ENR ids all carry that number plus a name, in county-specific
# layouts:
#   frame: "02 STONE BUTTE", "40 - BELLEMONT", "0137 AJ RADIANCE",
#          "005YORK/SHELDON", "PRECINCT 221" (Pima, Yuma), "BE BENSON" and
#          "ACACIA" (Cochise, Maricopa: name only, number is the id)
#   L2:    "0216 DESERT ...", "08 COCHISE ...", "BELLEMONT-40" (Coconino),
#          "097" / "41" (Pima, Yuma: number only)
#   ENR:   "0783 SPUR" (Maricopa), "06 BI WARREN" (Cochise), "201_Wikieup" (Mohave)
# Every source is parsed to (4-digit number, standardized name). A match needs
# both the number and the name to agree (or the name to be absent on a side
# that never has names, e.g. Pima/Yuma L2), so a renumbered or renamed precinct
# stays NA.

az_frame = read_frame("AZ")
az_counties = unique(az_frame$county)

# Pinal frame names abbreviate the place prefix that L2 spells out
# ("0114 STV PECAN CREEK" vs "114 SAN TAN VALLEY PECAN CREEK").
pinal_abbrev = c("STV" = "SAN TAN VALLEY", "CG" = "CASA GRANDE", "FLO" = "FLORENCE",
                 "MAR" = "MARICOPA", "AJ" = "APACHE JUNCTION")

parse_pct = function(county, x) {
  x = str_squish(str_replace_all(str_to_upper(x), "_", " ")) # Mohave ENR "223_Moccasin"
  lead = str_match(x, "^(\\d+)\\s*(?:-\\s*)?(.*)$")   # "0216 NAME", "40 - NAME", "005YORK/SHELDON", "097"
  trail = str_match(x, "^(.*?)\\s*-\\s*(\\d+)$")      # Coconino L2 "BELLEMONT-40"
  pre = str_match(x, "^PRECINCT\\s+(\\d+)$")          # Pima/Yuma frame "PRECINCT 221"
  code = coalesce(lead[, 2], trail[, 3], pre[, 2])
  name = case_when(
    !is.na(lead[, 2]) ~ lead[, 3],
    !is.na(trail[, 3]) ~ trail[, 2],
    !is.na(pre[, 2]) ~ "",
    .default = x
  )
  name = std_name(name)
  # Drop a trailing number equal to the precinct number: Coconino frame
  # "01 - FLAGSTAFF 1" vs L2 "FLAGSTAFF-01" -> both (0001, "FLAGSTAFF").
  tail_n = str_match(name, "^(.*\\S) (\\d+)$")
  same = !is.na(tail_n[, 3]) & !is.na(code) & as.integer(tail_n[, 3]) == as.integer(code)
  name[same] = tail_n[same, 2]
  p = county == "PINAL" & str_detect(name, "^[A-Z]+ ")
  w1 = str_extract(name[p], "^[A-Z]+")
  name[p] = if_else(w1 %in% names(pinal_abbrev), str_replace(name[p], "^[A-Z]+", unname(pinal_abbrev[w1])), name[p])
  tibble(
    code4 = if_else(is.na(code), NA, str_pad(str_remove(code, "^0+(?=\\d)"), 4, pad = "0")),
    pname = name
  )
}

# Frame keys. Cochise/Maricopa names carry no number, so the number is the id.
az_frame_names = frame_names("AZ") |>
  semi_join(az_frame, join_by(county, precinct_yougov))
fr_keys = az_frame_names |>
  bind_cols(parse_pct(az_frame_names$county, az_frame_names$frame_name)) |>
  mutate(code4 = coalesce(code4, precinct_yougov))
# A number parsed from a frame name always equals the frame id.
stopifnot(fr_keys$code4 == fr_keys$precinct_yougov)
fr_keys = fr_keys |>
  mutate(
    k_id = precinct_yougov,
    k_code_name = if_else(pname != "", paste(code4, pname, sep = "|"), NA),
    k_code_bare = if_else(pname == "", code4, NA)
  )

# ========================================
## ENR
# ========================================

# "AZ*_latest" matches the statewide "AZ_latest" and the legacy per-county stems.
enr_files = fs::path_rel(Sys.glob(election_path("AZ/clean/AZ*_latest.parquet")), election_path())
stopifnot(length(enr_files) > 0)

az_enr = read_enr(enr_files) |>
  filter(!is.na(precinct_enr), precinct_enr != "NA") |> # legacy Maricopa TSV NA bucket
  mutate(
    parse_pct(county, precinct_enr),
    k_code_name = if_else(pname != "", paste(code4, pname, sep = "|"), NA),
    k_code_bare = if_else(pname == "", code4, NA)
  )

# Two keys, as on the frame and L2 sides: Pima and Yuma ENR print the number
# with no name ("PRECINCT 001"), so they can only match on the bare code.
enr_matched = c("k_code_name", "k_code_bare") |>
  map(\(k) {
    e = az_enr |> select(state, county, precinct_enr, key = all_of(k)) |> filter(!is.na(key))
    f = fr_keys |> select(county, precinct_yougov, key = all_of(k)) |> filter(!is.na(key))
    stopifnot(!anyDuplicated(e[c("county", "key")]), !anyDuplicated(f[c("county", "key")]))
    inner_join(e, f, join_by(county, key)) |> select(-key)
  }) |>
  list_rbind()

stopifnot(!anyDuplicated(enr_matched[c("county", "precinct_enr")]))

# ENR ids whose number is in the frame but whose name differs (would signal a
# renamed or renumbered precinct, left NA).
az_enr |>
  anti_join(enr_matched, join_by(county, precinct_enr)) |>
  left_join(select(fr_keys, county, code4, frame_name, n_frame), join_by(county, code4)) |>
  select(county, precinct_enr, frame_name, n_frame) |>
  print(n = Inf)
# Unmatched 2026-11-01, 20 of 1,564, none in the frame (presumably no registered
# voters): Maricopa 0564-0577 NVP *, 0773-0777 SOUTH *, 0860 UNO; Yuma 045-047;
# Cochise 55 MONTEZUMA; Navajo 07 PALOMINO, 17 CANYON BUTTE; Pinal 040, 103.

az_enr = enr_matched |>
  select(state, county, precinct = precinct_yougov, precinct_enr)

# ========================================
## L2
# ========================================

az_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/az_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  as_tibble() |>
  filter(county %in% az_counties) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

# Hand-verified one-offs: number agrees, name differs, only candidate in the
# county, voter ratio normal.
l2_manual = tribble(
  ~county, ~precinct_l2,            ~precinct_yougov, # frame name
  "PINAL", "088 IRONWOOD CROSSING", "0088",           # 088 IRONWOOD CROSSING WEST; 4,190 L2 vs 4,116 frame
)

l2_keys = az_l2 |>
  filter(!is.na(precinct_l2)) |>
  mutate(parse_pct(county, precinct_l2)) |>
  left_join(select(l2_manual, county, precinct_l2, k_manual = precinct_yougov), join_by(county, precinct_l2)) |>
  mutate(
    k_code_name = if_else(pname != "", paste(code4, pname, sep = "|"), NA),
    k_code_bare = if_else(pname == "", code4, NA)
  )

l2_tiers = tribble(
  ~rule,       ~l2_key,       ~fr_key,
  "manual",    "k_manual",    "k_id",
  "code_name", "k_code_name", "k_code_name", # number and name agree
  "code_bare", "k_code_bare", "k_code_bare"  # Pima/Yuma: number only on both sides
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)
stopifnot(all(l2_manual$precinct_l2 %in% filter(l2_matched, l2_rule == "manual")$precinct_l2))

# Same-number pairs left unmatched because the names differ (review).
l2_keys |>
  anti_join(l2_matched, join_by(county, precinct_l2)) |>
  left_join(select(fr_keys, county, code4, frame_name, n_frame), join_by(county, code4)) |>
  select(county, precinct_l2, frame_name, n_l2, n_frame) |>
  print(n = Inf)

# Ratio outliers as of 2026-10-23 are all exact number+name matches on tiny
# precincts (Maricopa 0410 JUNE 8/6, 0848 TRES 124/80; Pinal 0089 3/4, 0116
# 29/11) or growth (Pinal 0137 AJ RADIANCE 3,048/2,367); kept.
review_matches(l2_matched, fr_keys)

az_l2 = az_l2 |>
  left_join(select(l2_matched, county, precinct_l2, precinct_new = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  mutate(precinct = precinct_new) |>
  filter(!is.na(precinct)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = az_frame |>
  tidylog::left_join(az_enr, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(az_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(
  nrow(crosswalk) == nrow(az_frame),
  crosswalk |> filter(!is.na(precinct_enr)) |> distinct(county, precinct_yougov, precinct_enr) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_enr))) |> pull(a) == 0,
  crosswalk |> filter(!is.na(precinct_l2)) |> distinct(county, precinct_yougov, precinct_l2) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_l2))) |> pull(a) == 0
)

crosswalk |>
  summarise(n = n(), enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)),
            both = mean(!is.na(precinct_enr) & !is.na(precinct_l2)), .by = county) |>
  print()

write_crosswalk("AZ", crosswalk)
