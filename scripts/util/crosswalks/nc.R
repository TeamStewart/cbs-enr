source("scripts/util/crosswalks/helpers.R")

# Build the NC precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules for other states: docs/precinct_crosswalks.md

nc = read_enr("NC/clean/NC_latest.parquet") |>
  # ENR codes match frame codes directly for ~98.5% of frame precincts. The two
  # recodes below have identical official names in the frame and in the NCSBE
  # voter file (2026-10-11) and consistent voter counts. Remaining ENR/frame
  # mismatches are merges (e.g. Halifax ENF = ENF1 + ENF 2; Cabarrus 02-10 =
  # 02-05 + 02-07), which are not 1:1 and are left unmatched on purpose.
  mutate(
    precinct = case_when(
      county == "CHATHAM" & precinct == "WSC112" ~ "85",     # WEST SILER CITY
      county == "CHATHAM" & precinct == "ESC111" ~ "ESC114", # EAST SILER CITY
      .default = precinct
    )
  )

nc_frame = read_frame("NC")

# Names + voter counts for the frame codes. Used only to match L2 names to
# frame codes; `nc_frame` itself is untouched.
nc_frame_names = frame_names("NC")

nc_l2 = enightdata::read_local("input_data/l2/nc_20260901_precincts.parquet", parquet = TRUE) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

# ========================================
## L2 names -> frame codes
# ========================================
# L2 carries precinct *names* ("01 PATTERSON (01)", "NEWTON EAST", "PCT 01-01"),
# the frame carries codes. Match L2 names to the frame's names/codes in ordered
# tiers. A tier only accepts keys that are unique within county on both sides,
# and a frame code claimed by an earlier tier is never reused (strict 1:1).
# Anything not matched stays NA in the crosswalk.

# Counties whose official precinct names are just the codes: translate L2 names
# into those codes.
county_codes = function(county, x) {
  out = rep(NA_character_, length(x))

  g = county == "GUILFORD"
  g_pfx = c(
    "CENTER GROVE NORTH"="NCGR", "CENTER GROVE"="CG", "CLAY NORTH"="NCLAY", "CLAY SOUTH"="SCLAY",
    "DEEP RIVER NORTH"="NDRI", "DEEP RIVER SOUTH"="SDRI", "FENTRESS"="FEN", "FRIENDSHIP"="FR",
    "GIBSONVILLE"="GIB", "GREENE"="GR", "GREENSBORO"="G", "HIGH POINT"="H", "JAMESTOWN"="JAM",
    "JEFFERSON"="JEF", "MADISON NORTH"="NMAD", "MADISON SOUTH"="SMAD", "MONROE"="MON", "OAK RIDGE"="OR",
    "PLEASANT GARDEN"="PG", "ROCK CREEK"="RC", "STOKESDALE"="STOK", "SUMMERFIELD"="SF", "SUMNER"="SUM",
    "WASHINGTON NORTH"="NWASH", "WASHINGTON SOUTH"="SWASH"
  )
  gx = str_match(x, "^(.*?)(?: ([0-9]+[A-Z0-9-]*))?$") # "HIGH POINT 27-A" -> "HIGH POINT", "27-A"
  out[g] = if_else(is.na(g_pfx[gx[g, 2]]), NA, paste0(g_pfx[gx[g, 2]], coalesce(gx[g, 3], "")))

  h = county == "NEW HANOVER"
  h_pfx = c("CAPE FEAR"="CF", "FEDERAL POINT"="FP", "HARNETT"="H", "MASONBORO"="M", "WILMINGTON"="W")
  hx = str_match(x, "^(.*) ([0-9]+)$")
  out[h] = if_else(is.na(h_pfx[hx[h, 2]]), NA, paste0(h_pfx[hx[h, 2]], str_pad(hx[h, 3], 2, pad = "0")))
  out[h & x == "WRIGHTSVILLE BEACH"] = "WB"

  s = county == "SWAIN"
  out[s] = recode(x[s], "BRYSON CITY 1"="BC1", "BRYSON CITY 2"="BC2", "WHITTIER-CHEROKEE"="WHCH", .default = NA_character_)

  v = county == "VANCE"
  out[v] = recode(x[v], "COMMUNITY COLLEGE"="CC", "KITTRELL"="KITT", "MIDDLEBURG"="MIDD",
                  "NORTHERN VANCE"="NV", "SANDY CREEK"="SCRK", "HENDERSON WEST"="WH", .default = NA_character_)

  # "HOPE MILLS 3A-G9" -> frame G9A-3 "HOPE MILLS 3A"
  out[county == "CUMBERLAND" & x == "HOPE MILLS 3A-G9"] = "G9A-3"
  out[county == "CUMBERLAND" & x == "PEARCES MILL 2C-G3"] = "G3A-2C"

  out
}

# Hand-verified one-offs: name differs but the precinct is the only candidate in
# the county and the L2/frame voter ratio is in the normal range (~1.0-1.2).
l2_manual = tribble(
  ~county,      ~precinct_l2,             ~precinct_yougov, # frame name
  "ALAMANCE",   "03 NORTH BOONE 1 (03N1)", "03N",           # NORTH BOONE (03N2 matched separately)
  "GREENE",     "SNOW HILL",              "SH#1",           # SNOW HILL 1
  "ROCKINGHAM", "HOGANS",                 "HO-1",           # HOGANS-1
  "ROCKINGHAM", "HUNTSVILLE",             "HU-1",           # HUNTSVILLE-1
  "ROCKINGHAM", "LINCOLN",                "LI-1",           # LINCOLN-1
  "ROCKINGHAM", "WESTERN",                "WS-1",           # WESTERN-1
  "STANLY",     "ALBEMARLE 07",           "0107",           # ALBEMARLE 7.1
  "STANLY",     "ALBEMARLE 08",           "0108",           # ALBEMARLE 8.1
  "STANLY",     "ALBEMARLE 10",           "0110",           # ALBEMARLE 10.1
  "STOKES",     "WILSONS STORE",          "140"             # WILSON ST.
)

fr_keys = nc_frame_names |>
  mutate(
    k_raw = frame_name, k_id = precinct_yougov,
    k_std = std_name(frame_name), k_idstd = std_name(precinct_yougov),
    k_bag = bag(k_std), k_nospace = str_remove_all(k_std, " "),
    k_idsuffix = str_remove(precinct_yougov, "^[A-Z]+")
  )

l2_keys = nc_l2 |>
  filter(!is.na(precinct_l2)) |>
  left_join(select(l2_manual, county, precinct_l2, k_manual = precinct_yougov), join_by(county, precinct_l2)) |>
  mutate(
    k_raw = precinct_l2,
    k_paren = str_match(precinct_l2, "\\(([^)]+)\\)\\s*$")[, 2],
    k_county = county_codes(county, precinct_l2),
    k_std = std_name(precinct_l2),
    k_strip = std_name(strip_lead(precinct_l2)),
    k_bag = bag(k_strip), k_nospace = str_remove_all(k_strip, " "),
    k_lead = str_match(precinct_l2, "^(\\d+[A-Z]?\\d?)\\s+[A-Z]")[, 2] # Onslow "01 JACKSONVILLE" -> JA01
  )

l2_tiers = tribble(
  ~rule,              ~l2_key,     ~fr_key,
  "manual",           "k_manual",  "k_id",
  "name_exact",       "k_raw",     "k_raw",
  "code_exact",       "k_raw",     "k_id",
  "paren_code",       "k_paren",   "k_id",
  "county_rule",      "k_county",  "k_id",
  "name_std",         "k_std",     "k_std",
  "code_std",         "k_std",     "k_idstd",
  "name_strip",       "k_strip",   "k_std",
  "name_bag",         "k_bag",     "k_bag",
  "name_nospace",     "k_nospace", "k_nospace",
  "lead_code_suffix", "k_lead",    "k_idsuffix"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)
stopifnot(all(l2_manual$precinct_l2 %in% filter(l2_matched, l2_rule == "manual")$precinct_l2))

review_matches(l2_matched, fr_keys)

nc_l2 <- nc_l2 |>
  left_join(select(l2_matched, county, precinct_l2, precinct_new = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  mutate(precinct = precinct_new) |>
  filter(!is.na(precinct)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = nc_frame |>
  tidylog::left_join(nc, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(nc_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(nrow(crosswalk) == nrow(nc_frame))

write_crosswalk("NC", crosswalk)
