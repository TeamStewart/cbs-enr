source("scripts/util/crosswalks/helpers.R")

# Build the NE precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules for other states: docs/precinct_crosswalks.md

# ========================================
## ENR
# ========================================
# The statewide SOS GetMapData feed uses `StatePrecinctID` as the precinct id,
# which is just a per-county sequence (0001, 0002, ...) and has no relation to
# the frame codes. The precinct *names* (e.g. "01 Wstbl/Hgh", "01-01",
# "Precinct 1") are only in the raw file from the same scrape, so we attach them
# by (county, StatePrecinctID) and match them to the frame names, which come from
# the same SOS naming. `precinct_enr` stays the raw id from clean/.
ne = read_enr("NE/clean/NE_latest.parquet")

# Raw file of the same scrape as NE_latest: the newest timestamped clean file,
# checked to have the same geos as NE_latest.
enr_stamp = Sys.glob(election_path("NE/clean/NE_2*.parquet")) |> fs::path_file() |> fs::path_ext_remove() |> max()
stopifnot(setequal(
  unique(as.character(enightdata::read_local(paste0("NE/clean/", enr_stamp, ".parquet"), parquet = TRUE)$geo)),
  unique(as.character(enightdata::read_local("NE/clean/NE_latest.parquet", parquet = TRUE)$geo))
))
enr_raw_file = election_path("NE/raw", paste0(enr_stamp, ".csv"))
ne_enr_names = readr::read_csv(enr_raw_file, col_types = readr::cols(.default = "c")) |>
  filter(StatePrecinctID != "_TOT") |>
  distinct(county = str_to_upper(CountyName), precinct = StatePrecinctID, enr_name = PrecinctName)
stopifnot(!anyDuplicated(ne_enr_names[c("county", "precinct")]))

ne = ne |> left_join(ne_enr_names, join_by(county, precinct))
stopifnot(!anyNA(ne$enr_name))

ne_frame = read_frame("NE")

# Names + voter counts for the frame codes. Used only to match ENR/L2 names to
# frame codes; `ne_frame` itself is untouched.
ne_frame_names = frame_names("NE")

fr_keys = ne_frame_names |>
  mutate(
    k_raw = str_squish(str_to_upper(frame_name)), k_id = precinct_yougov,
    k_std = std_name(frame_name), k_idstd = std_name(precinct_yougov)
  )

# ENR names -> frame codes. Same tier machinery as L2 (match_tiers wants
# `precinct_l2` / `n_l2` columns, so the ENR id is passed under those names).
enr_keys = ne |>
  transmute(
    county, precinct_l2 = precinct, n_l2 = NA_real_,
    k_raw = str_squish(str_to_upper(enr_name)),
    k_std = std_name(enr_name)
  )
enr_tiers = tribble(
  ~rule,        ~l2_key, ~fr_key,
  "name_exact", "k_raw", "k_raw",
  "name_std",   "k_std", "k_std"
)
enr_matched = match_tiers(enr_keys, fr_keys, enr_tiers)
print(count(enr_matched, l2_rule))

ne = ne |>
  inner_join(select(enr_matched, county, precinct = precinct_l2, precinct_yougov), join_by(county, precinct)) |>
  select(state, county, precinct = precinct_yougov, precinct_enr)

# ========================================
## L2 names -> frame codes
# ========================================
ne_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/ne_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

# NE-specific name key on top of std_name(): punctuation to spaces, ordinals
# and roman numerals to digits, and drop generic words (PRECINCT, TOWNSHIP,
# AREA, CITY OF, VILLAGE OF, ...). With `strip = TRUE` also drop a leading code
# token ("01.NORTH PLATTE", "01 BLF - BLOOMFIELD AREA"). Only used in tiers after
# the exact ones, and match_tiers() still needs the key to be unique within
# county on both sides.
ne_std = function(x, strip = FALSE) {
  words = c("1ST"="1", "2ND"="2", "3RD"="3", "4TH"="4", "5TH"="5",
            "FIRST"="1", "SECOND"="2", "THIRD"="3", "FOURTH"="4", "FIFTH"="5",
            "I"="1", "II"="2", "III"="3", "IV"="4", "COMM"="COM")
  drop = c("PRECINCT", "PRECINCTS", "TOWNSHIP", "TOWNSHIPS", "TWP", "TWPS", "TWNSHPS",
           "AREA", "CITY", "OF", "VILLAGE", "COUNTY", "CNTY", "AT", "LARGE", "AND")
  std_name(x) |>
    str_replace_all("[^A-Z0-9' ]", " ") |>
    str_squish() |>
    str_split(" ") |>
    map_chr(\(t) {
      t = if_else(t %in% names(words), words[t], t)
      t = t[!t %in% drop]
      if (strip && length(t) > 1 && str_detect(t[1], "^\\d+$")) t = t[-1] # leading code token
      paste(t, collapse = " ")
    })
}

# County-specific translations of L2 names into frame names or ids.
county_codes = function(county, x) {
  out = rep(NA_character_, length(x))
  # Douglas: L2 "WARD 01 PCT 01" -> frame name "01-01"
  d = county == "DOUGLAS"
  out[d] = str_replace(x[d], "^WARD (\\d{2}) PCT (\\d{2})$", "\\1-\\2") |> (\(y) if_else(str_detect(y, "^\\d{2}-\\d{2}$"), y, NA))()
  # Lancaster: L2 "01 A-01" -> frame name "01A01"; "01 A" -> "01A00"
  l = county == "LANCASTER"
  lx = str_match(x[l], "^(\\d{2}) ([A-Z])(?:-(\\d{2}))?$")
  out[l] = if_else(is.na(lx[, 1]), NA, paste0(lx[, 2], lx[, 3], coalesce(lx[, 4], "00")))
  out
}
# Counties whose L2 names carry the frame id: Buffalo leading number ("01
# NORTHSIDE COMMUNITY CHURCH" -> 001; the polling place names changed), Hall
# leading number ("21 CAIRO" -> 21), Cass trailing code ("AVOCA 011" -> 011,
# frame "011 - AVOCA"; all 24 names agree). Matched to the frame id.
county_ids = function(county, x) {
  out = rep(NA_character_, length(x))
  lead = str_match(x, "^(\\d{2})(?: |$)")[, 2]
  b = county == "BUFFALO"
  out[b] = str_pad(lead[b], 3, pad = "0")
  h = county == "HALL"
  out[h] = lead[h]
  cs = county == "CASS"
  out[cs] = str_match(x[cs], " (\\d{3})$")[, 2]
  out
}

# Hand-verified one-offs: the only candidate left in the county, names agree,
# and the L2/frame voter ratio is normal.
l2_manual = tribble(
  ~county,      ~precinct_l2,                    ~precinct_yougov, # frame name
  "BUTLER",     "006 PLUM CREEK-RICHARDSON-ULYSSES", "06", # 06 PLUM CREEK-RICHARDSON-ULY
  "CUMING",     "BANC T-V-CLEVELAND-NELIGH",     "01",  # BCN (621 vs 628)
  "CUMING",     "BEEMER T-V-LOGAN TWP",          "02",  # BL (601 vs 601)
  "CUMING",     "SHERMAN-STCHAR-GARFD-CUMING",   "03",  # SSGC (760 vs 773)
  "FILLMORE",   "GENEVA RURAL-104-107-109",      "104", # GENEVA RURAL
  "GARDEN",     "OSHKOSH",                       "02",  # OSHKOSH 1
  "HOLT",       "ATKINSON SURROUNDING TWPS",     "002", # ATKINSON SURRONUNDING TWNSPS
  "HOLT",       "ATKINSON",                      "009", # ATKINSON WARD I (838 vs 840)
  "KNOX",       "LINDY-SANTEE-CENTER AREA-LSC",  "04",  # 04 LSC - LINDY/SANTEE/CTR AREA
  "LINCOLN",    "NORTH PLATTE 07B",              "08",  # 07.NORTH PLATTE PRECINCT 7
  "RICHARDSON", "FALLS CITY 1",                  "03",  # FC WARD 1
  "RICHARDSON", "FALLS CITY 2",                  "04",  # FC WARD 2
  "RICHARDSON", "FALLS CITY 3",                  "05",  # FC WARD 3
  "RICHARDSON", "FALLS CITY 4",                  "06",  # FC WARD 4
  "SEWARD",     "M-BX PCT-BEAVER CROSSING",      "09"   # M-BX PRECINCT - BEAVER CROSSIN
)

# Frame precincts that are a merge of two L2 precincts (rule 3): no L2 match.
l2_exclude = tribble(
  ~county,  ~precinct_yougov, # frame name (n_frame) = L2 pieces (n_l2)
  "CEDAR",  "06",  # PCT 06 8-9 (1337) = "PCT 06 (8-9)" (1208) + "PCT 06 8-9" (144)
  "CUMING", "06",  # BGW (1003) = "BLAINE-GRANT-WISNER" (924) + "BGW" (14)
  "DIXON",  "01"   # LOGAN-WAKEFIELD 3 (257) = "WAKEFIELD 3-LOGAN" (256) + "LOGAN-WAKEFIELD 3" (2)
)

fr_keys_l2 = fr_keys |>
  anti_join(l2_exclude, join_by(county, precinct_yougov)) |>
  mutate(
    k_ne = ne_std(frame_name), k_nebag = bag(k_ne),
    k_nestrip = ne_std(frame_name, strip = TRUE), k_nenospace = str_remove_all(k_nestrip, " "),
    k_nestripbag = bag(k_nestrip),
    k_single = if_else(n() == 1, "SINGLE", NA), .by = county
  )

l2_keys = ne_l2 |>
  filter(!is.na(precinct_l2)) |>
  left_join(select(l2_manual, county, precinct_l2, k_manual = precinct_yougov), join_by(county, precinct_l2)) |>
  mutate(
    k_raw = precinct_l2,
    k_county = county_codes(county, precinct_l2),
    k_cid = county_ids(county, precinct_l2),
    k_std = std_name(precinct_l2),
    k_num = if_else(str_detect(precinct_l2, "^\\d+$"), k_std, NA), # purely numeric L2 name
    k_ne = ne_std(precinct_l2), k_nebag = bag(k_ne),
    k_nestrip = ne_std(precinct_l2, strip = TRUE), k_nenospace = str_remove_all(k_nestrip, " "),
    k_nestripbag = bag(k_nestrip),
    k_single = if_else(n() == 1, "SINGLE", NA), .by = county
  )

l2_tiers = tribble(
  ~rule,           ~l2_key,    ~fr_key,
  "manual",        "k_manual", "k_id",
  "name_exact",    "k_raw",    "k_raw",
  "code_exact",    "k_raw",    "k_id",
  "county_rule",   "k_county", "k_raw",
  "county_id",     "k_cid",    "k_id",
  "name_std",      "k_std",    "k_std",
  "code_std",      "k_num",    "k_idstd",
  "name_ne",       "k_ne",     "k_ne",
  "name_ne_bag",   "k_nebag",  "k_nebag",
  "name_ne_strip", "k_nestrip", "k_nestrip",
  "name_ne_nospace", "k_nenospace", "k_nenospace",
  "name_ne_stripbag", "k_nestripbag", "k_nestripbag",
  "county_single", "k_single", "k_single"
)

l2_matched = match_tiers(l2_keys, fr_keys_l2, l2_tiers)
stopifnot(all(l2_manual$precinct_l2 %in% filter(l2_matched, l2_rule == "manual")$precinct_l2))

review_matches(l2_matched, fr_keys)

# Eyeball every non-exact match
l2_matched |>
  filter(!l2_rule %in% c("name_exact", "code_exact")) |>
  select(l2_rule, county, precinct_l2, precinct_yougov, frame_name, n_l2, n_frame) |>
  arrange(l2_rule, county) |>
  print(n = Inf)

ne_l2 = ne_l2 |>
  left_join(select(l2_matched, county, precinct_l2, precinct_new = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  mutate(precinct = precinct_new) |>
  filter(!is.na(precinct)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = ne_frame |>
  tidylog::left_join(ne, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(ne_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(nrow(crosswalk) == nrow(ne_frame))

write_crosswalk("NE", crosswalk)
