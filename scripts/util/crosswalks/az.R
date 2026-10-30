source("scripts/util/crosswalks/helpers.R")

# Build the AZ precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference: nc.R.
#
# AZ is partial: 26_general ENR exists only for some frame counties (Cochise and
# Maricopa as of 2026-10-23). L2 matching covers all 9 frame counties. New
# `AZ/clean/AZ_*_latest.parquet` files are picked up by the glob on rerun; check
# the id format of each new county (see `parse_pct()`).
#
# Frame ids are 4-digit, zero-padded county precinct numbers ("0009"). Frame
# names, L2 and ENR ids all carry that number plus a name, in county-specific
# layouts:
#   frame: "02 STONE BUTTE", "40 - BELLEMONT", "0137 AJ RADIANCE",
#          "005YORK/SHELDON", "PRECINCT 221" (Pima, Yuma), "BE BENSON" and
#          "ACACIA" (Cochise, Maricopa: name only, number is the id)
#   L2:    "0216 DESERT ...", "08 COCHISE ...", "BELLEMONT-40" (Coconino),
#          "097" / "41" (Pima, Yuma: number only)
#   ENR:   "0783 SPUR" (Maricopa), "06 BI WARREN" (Cochise)
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

enr_files = fs::path_rel(Sys.glob(election_path("AZ/clean/AZ_*_latest.parquet")), election_path())
stopifnot(length(enr_files) > 0)

az_enr = read_enr(enr_files) |>
  filter(!is.na(precinct_enr), precinct_enr != "NA") |> # Maricopa "az_013_NA" bucket
  mutate(parse_pct(county, precinct_enr), k_code_name = paste(code4, pname, sep = "|"))

enr_dup = az_enr |> filter(n() > 1, .by = c(county, k_code_name))
stopifnot(nrow(enr_dup) == 0)

enr_matched = az_enr |>
  inner_join(select(fr_keys, county, precinct_yougov, k_code_name), join_by(county, k_code_name), na_matches = "never")

# ENR ids whose number is in the frame but whose name differs (none as of
# 2026-10-23; would signal a renamed/renumbered precinct, left NA).
az_enr |>
  anti_join(enr_matched, join_by(county, precinct_enr)) |>
  left_join(select(fr_keys, county, code4, frame_name, n_frame), join_by(county, code4)) |>
  select(county, precinct_enr, frame_name, n_frame) |>
  print(n = Inf)
# Unmatched as of 2026-10-23 (none in the frame, presumably no registered
# voters): Cochise 55 MONTEZUMA; Maricopa 0564/0568/0569/0570/0576/0577 NVP *,
# 0773-0777 SOUTH *, 0860 UNO.

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
