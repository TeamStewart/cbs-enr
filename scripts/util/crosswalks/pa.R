source("scripts/util/crosswalks/helpers.R")

# Build the PA precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference: nc.R.
#
# PA is partial: ENR comes as per-county files (`PA_{county}_latest.parquet`);
# as of 2026-10-27 Allegheny, Chester, Dauphin, Delaware, Erie, Philadelphia and
# Washington have 26_general data.
# New county files are picked up by the glob, but each new county's id format
# must be checked and given a rule in `enr_to_frame()` (unknown formats stay NA).
#
# Frame ids are county-specific codes ("005", "0100000E", "0101", "1010001");
# frame names come from the voter file and vary by county ("ASTON TOWNSHIP 1ST
# WARD", "ADAMS(T)#1", "PHILA WD 01 DIV 01"). L2 names follow one convention
# ("ASTON TWP 1", "PHILADELPHIA 01-01", "BETHEL PARK 7-2").

pa_frame = read_frame("PA")
pa_frame_names = frame_names("PA") |>
  semi_join(pa_frame, join_by(county, precinct_yougov))

# ========================================
## Name normalization (PA-specific)
# ========================================
# Reduces a precinct name to "MUNI [TYPE] n [n]" on both sides: abbreviations
# are expanded, ordinals become numbers, the words ward/district/precinct/
# division are dropped, zero padding and all-zero tokens ("ALEPPO 00 01") are
# dropped. `keep_type = FALSE` also drops TWP/BORO/CITY. W and D are dropped only
# in front of a number ("ASHLEY BORO W 01", "17TH W1"); other single letters are
# directions ("W BRADFORD", "KENNETT SQ N").

ordinals = c(
  "FIRST" = "1", "SECOND" = "2", "THIRD" = "3", "FOURTH" = "4", "FIFTH" = "5", "SIXTH" = "6",
  "SEVENTH" = "7", "EIGHTH" = "8", "NINTH" = "9", "TENTH" = "10", "ELEVENTH" = "11", "TWELFTH" = "12"
)
abbrev = c(
  "TOWNSHIP" = "TWP", "TWSP" = "TWP", "TP" = "TWP", "BOROUGH" = "BORO", "BOR" = "BORO", "BR" = "BORO",
  "MARLBOROUGH" = "MARLBORO", "PHILA" = "PHILADELPHIA", "JHTN" = "JOHNSTOWN", # Philadelphia, Cambria
  "CASL" = "CASTLE", "UP" = "UPPER", "SPRINGDAL" = "SPRINGDALE",               # Allegheny
  "HL" = "HILLS", "HILL" = "HILLS", "HT" = "HEIGHTS", "HGTS" = "HEIGHTS",      # Allegheny "PLEASANT HL" / L2 "PLEASANT HILL"
  "BROWNSV" = "BROWNSVILLE", "CONNELLSV" = "CONNELLSVILLE",                    # Fayette
  "WASH" = "WASHINGTON", "STRASBRG" = "STRASBURG", "VAL" = "VALLEY",           # Washington, Lancaster, Crawford
  "LAW" = "LAWRENCE", "LAWARENCE" = "LAWRENCE",                                # Erie (L2 typo)
  "I" = "1", "II" = "2", "III" = "3", "IV" = "4"                               # Lancaster "ELM TREE II"
)
directions = c("E" = "EAST", "W" = "WEST", "N" = "NORTH", "S" = "SOUTH")

pa_norm = function(x, keep_type = TRUE) {
  x = str_to_upper(x) |>
    str_replace_all("\\(T\\)", " TWP ") |>
    str_replace_all("\\(B\\)", " BORO ") |>
    str_replace_all("\\(C\\)", " CITY ") |>
    str_remove("^MUNIC OF ") |>                                       # Westmoreland Murrysville
    str_replace_all("[#,./()&'\"-]", " ") |>
    str_replace_all("([A-Z])(\\d)", "\\1 \\2") |>                     # "TWP1ST", "W1"
    str_replace_all(setNames(ordinals, paste0("\\b", names(ordinals), "\\b"))) |>
    str_replace_all(setNames(abbrev, paste0("\\b", names(abbrev), "\\b"))) |>
    str_replace_all("\\b(\\d+)(ST|ND|RD|TH)\\b", "\\1") |>
    str_replace_all("\\b(\\d+)[WP]\\b", "\\1") |>                     # Lancaster "2W 1P"
    str_remove_all("\\b(WARD|WD|DISTRICT|DIST|DT|PRECINCT|PREC|PRC|PCT|DIVISION|DIV|NO|NUMBER)\\b") |>
    str_remove_all("\\b[WD]\\b(?=\\s*\\d)") |>
    str_remove("(?<=\\d) W$") |>                                     # Washington "EAST BETHLEHEM-1ST W"
    str_replace_all(setNames(directions, paste0("\\b", names(directions), "\\b"))) |>
    str_remove_all("\\b0+\\b") |>
    str_replace_all("\\b0+(?=\\d)", "") |>
    str_replace("^(\\w+) (EAST|WEST) TWP\\b", "\\2 \\1 TWP") |>      # Lancaster "COCALICO EAST TWP"
    str_squish()
  if (!keep_type) x = str_squish(str_remove_all(x, "\\b(TWP|BORO|CITY)\\b"))
  x
}

# ========================================
## ENR
# ========================================
# Per county:
# - CHESTER (LiveVoterTurnout): "005 atglen" -> leading 3-digit code. The rest
#   of the id must equal the frame name (checked below).
# - PHILADELPHIA (city feed): "01-01" -> frame "0101" (ward + division). The
#   feed also has "2nd/3rd/5th congressional" aggregate rows; they stay NA.
# - DELAWARE (Clarity): names like "UPPER DARBY TWP DISTRICT 07 PRECINCT 06" or
#   "ALDAN BOR PRECINCT EASTERN"; frame names say "UPPER DARBY TOWNSHIP 7TH
#   DIST 6TH PREC" / "ALDAN BOROUGH EASTERN PRECINCT". Matched on a
#   canonical "MUNI TYPE W n P n" form of both names, exact and unique.

del_canon = function(x) {
  str_to_upper(x) |>
    str_replace_all("\\bBOROUGH\\b", "BOR") |>
    str_replace_all("\\bTOWNSHIP\\b", "TWP") |>
    str_replace_all("\\b(\\d+)(ST|ND|RD|TH) (WARD|DISTRICT|DIST)\\b", "WD \\1") |>
    str_replace_all("\\b(\\d+)(ST|ND|RD|TH) (PRECINCT|PREC)\\b", "PRECINCT \\1") |>
    str_replace_all("\\b(EASTERN|WESTERN|NORTHERN|SOUTHERN|NORTHEAST|SOUTHWEST) PRECINCT\\b", "PRECINCT \\1") |>
    str_replace_all("\\bDISTRICT (\\d+)", "WD \\1") |>
    str_replace_all("\\b0+(?=\\d)", "") |>
    str_squish()
}

del_frame = pa_frame_names |>
  filter(county == "DELAWARE") |>
  mutate(key = del_canon(frame_name)) |>
  filter(n() == 1, .by = key)

enr_files = fs::path_rel(Sys.glob(election_path("PA/clean/PA_*_latest.parquet")), election_path())
stopifnot(length(enr_files) > 0)

pa = read_enr(enr_files)
print(count(pa, county))

# - ALLEGHENY, DAUPHIN, ERIE, WASHINGTON (Clarity; Dauphin's own site): names
#   only ("Aspinwall Dist 1", "City--1st Ward, 1st Precinct", "ERIE WARD 01
#   DISTRICT 01", "AMWELL 1"). Matched on pa_norm() of both names, exact and
#   unique on both sides, then a pass on the leftovers that also drops type
#   words and spaces ("CRANESVILLE BOR" / "CRANESVILLE", "O'Hara" / "OHARA").
#   Allegheny single-district municipalities have no number in ENR ("Aleppo" /
#   "ALEPPO 00 01"); Dauphin's "City--" is Harrisburg; Washington frame names
#   shorten Monongahela to "MONON.". Added 2026-10-27.
name_counties = c("ALLEGHENY", "DAUPHIN", "ERIE", "WASHINGTON")
enr_name_key = function(county, x, keep_type) {
  k = pa_norm(if_else(county == "DAUPHIN", str_replace(x, "^City--", "HARRISBURG "), x), keep_type)
  if_else(county == "ALLEGHENY" & !str_detect(k, "\\d"), paste(k, "1"), k)
}
frame_name_key = function(county, x, keep_type) {
  pa_norm(if_else(county == "WASHINGTON", str_replace(x, "^MONON\\.", "MONONGAHELA "), x), keep_type)
}
name_match = function(e, f, keep_type) {
  squash = if (keep_type) identity else \(k) str_remove_all(k, " ")
  e = mutate(e, key = squash(enr_name_key(county, precinct_enr, keep_type))) |> filter(n() == 1, .by = c(county, key))
  f = mutate(f, key = squash(frame_name_key(county, frame_name, keep_type))) |> filter(n() == 1, .by = c(county, key))
  inner_join(e, f, join_by(county, key)) |> select(county, precinct_enr, precinct_yougov)
}
name_enr = filter(pa, county %in% name_counties)
name_fr = filter(pa_frame_names, county %in% name_counties)
name_xw = name_match(name_enr, name_fr, TRUE)
name_xw = bind_rows(name_xw, name_match(
  anti_join(name_enr, name_xw, join_by(county, precinct_enr)),
  anti_join(name_fr, name_xw, join_by(county, precinct_yougov)),
  FALSE
))

pa = pa |>
  mutate(
    precinct = case_when(
      county == "CHESTER" & str_detect(precinct_enr, "^\\d{3} ") ~ str_extract(precinct_enr, "^\\d{3}"),
      county == "PHILADELPHIA" & str_detect(precinct_enr, "^\\d{2}-\\d{2}$") ~ str_remove(precinct_enr, "-"),
      county == "DELAWARE" ~ del_frame$precinct_yougov[match(del_canon(precinct_enr), del_frame$key)],
      county %in% name_counties ~ name_xw$precinct_yougov[match(paste(county, precinct_enr), paste(name_xw$county, name_xw$precinct_enr))],
      .default = NA_character_ # unknown county format: check and add a rule
    )
  )

# Chester: the name after the code must agree with the frame name, up to
# spelling ("w caln 1" / "W CALN 1", "kennett square n" / "KENNETT SQ. N",
# "honey brook township 1" / "HONEYBROOK TWP 1"). A code whose names disagree
# is set to NA.
loose = function(x) {
  str_to_upper(x) |>
    str_replace_all("[-.]", " ") |>
    str_replace_all(c("\\bE\\b" = "EAST", "\\bW\\b" = "WEST", "\\bN\\b" = "NORTH", "\\bS\\b" = "SOUTH")) |>
    str_replace_all(c("\\bSQ\\b" = "SQUARE", "\\bTOWNSHIP\\b" = "TWP", "\\bBOROUGH\\b" = "BORO")) |>
    str_remove_all(" ")
}
chk = pa |>
  filter(county == "CHESTER", !is.na(precinct)) |>
  left_join(pa_frame_names, join_by(county, precinct == precinct_yougov)) |>
  filter(is.na(frame_name) | loose(str_remove(precinct_enr, "^\\d{3} ")) != loose(frame_name))
print(chk)
pa = pa |> mutate(precinct = if_else(county == "CHESTER" & precinct %in% chk$precinct, NA, precinct))

stopifnot(
  !anyDuplicated(filter(pa, !is.na(precinct))[c("county", "precinct")]),
  !anyDuplicated(pa[c("county", "precinct_enr")])
)

# ENR leftovers on both sides, in ENR counties
enr_left = pa |> filter(is.na(precinct) | !paste(county, precinct) %in% paste(pa_frame$county, pa_frame$precinct_yougov))
print(enr_left, n = Inf)
pa_frame_names |>
  filter(county %in% pa$county) |>
  anti_join(pa, join_by(county, precinct_yougov == precinct)) |>
  print(n = Inf)

pa = filter(pa, !is.na(precinct))

# ========================================
## L2 names -> frame codes
# ========================================

pa_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/pa_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  as_tibble() |>
  filter(county %in% unique(pa_frame$county)) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

# Hand-verified one-offs: the only candidate left on both sides in the county,
# and the L2/frame voter ratio is in the normal range (0.98-1.06).
l2_manual = tribble(
  ~county,        ~precinct_l2,                                 ~precinct_yougov, # frame name
  "ALLEGHENY",    "MCDONALD",                                   "1670005",        # MCDONALD 00 05 (only McDonald precinct in Allegheny)
  "ALLEGHENY",    "PENNSBURY VLG",                              "2120002",        # PENNSBURY VLG 00 02 (only precinct)
  "ALLEGHENY",    "OSBORNE",                                    "1840001",        # GLEN OSBORNE 00 01
  "CAMBRIA",      "ADAMS TWP GRAMLINGTOWN",                     "001E",           # ADAMS(T)GRAMLIN
  "CAMBRIA",      "CAMBRIA TWP 2 COLVER",                       "007B",           # CAMBRIA(T)COLVER
  "CAMBRIA",      "CAMBRIA TWP 3 REVLOC",                       "007C",           # CAMBRIA(T)REVLOC
  "CAMBRIA",      "JOHNSTOWN 01 CTR TOWN",                      "901A",           # JHTN CENTER TOWN
  "CAMBRIA",      "JOHNSTOWN 05-1 KERNVILLE",                   "905A",           # JHTN KERNVILLE
  "CAMBRIA",      "JOHNSTOWN 07-1",                             "907A",           # JOHNSTOWN 7TH WD (only ward-7 precinct on both sides)
  "CAMBRIA",      "JOHNSTOWN 09 OLD CONEMAUGH",                 "909",            # JHTN OLD CONEMAUGH/WOODVALE
  "CAMBRIA",      "JOHNSTOWN 12-1 PROSPECT",                    "912A",           # JHTN PROSPECT
  "CAMBRIA",      "JOHNSTOWN 15 CAMBRIA CITY",                  "915",            # JHTN CAMBRIA CITY
  "CAMBRIA",      "NANTY GLO BORO 1-1",                         "039A",           # NANTY GLO(B)1ST
  "CAMBRIA",      "NANTY GLO BORO 2-1",                         "039C",           # NANTY GLO(B)2ND
  "CHESTER",      "E NOTTINGHAM",                               "405",            # EAST NOTTINGHAM EAST (E NOTTINGHAM WEST -> 406)
  "CRAWFORD",     "VENANGO BORO",                               "580",            # VENANGOBORO
  "ERIE",         "WAYNE TWP EAST",                             "40001",          # WAYNE TWP. (only Wayne precinct on both sides)
  "FAYETTE",      "PERRY BORO",                                 "56010",          # PERRYOPOLIS BORO (Fayette has no Perry borough)
  "LANCASTER",    "DONEGAL WEST TWP",                           "3904",           # DONEGAL WEST TWP 4 (L2 WEST DONEGAL TWP 1-3 match 3901-3903)
  "LANCASTER",    "EAST HEMPFIELD TWP-LANDISVILLE WEST",        "4911",           # HEMPFIELD EAST TWP LANDISVILLE
  "LANCASTER",    "LANCASTER CITY 7-8-LAMPETER STRASBURG SD",   "0708-3",         # LANCASTER CITY 7W 8P LAMPETER/STRASBRG S
  "LANCASTER",    "MARTIC TWP",                                 "5900",           # MARTIC TWP MARTICVILLE (only precinct)
  "LANCASTER",    "PEQUEA TWP",                                 "6300",           # PEQUEA TWP WEST WILLOW (only precinct)
  "MONTGOMERY",   "UPPER PROVIDENCE MINGO 1",                   "61M100",         # UPPER PROVIDENCE M-1
  "MONTGOMERY",   "UPPER PROVIDENCE MINGO 2",                   "61M200",         # UPPER PROVIDENCE M-2
  "WESTMORELAND", "HEMPFIELD TWP-LINCOLN HGTS EAST",            "19048"           # HEMPFIELD TWP LINCOLN HGTS EST
)
# Left NA (ambiguous): LANCASTER 4906 HEMPFIELD EAST TWP SPRING (1,335) vs L2
# EAST HEMPFIELD TWP-GOLDEN ACRES (1,356): the only leftovers in the township
# and the count fits, but the names share nothing.

# Municipalities with a single precinct: frame "ALEPPO 00 01" vs L2 "ALEPPO".
# Key is the name without a trailing "1", kept only if the municipality has
# exactly one precinct on that side.
single_key = function(county, key) {
  muni = str_remove(key, "( \\d.*)?$")
  out = if_else(key %in% c(muni, paste(muni, "1")), muni, NA)
  n_muni = ave(seq_along(muni), county, str_remove(key, " \\d.*$"), FUN = length)
  if_else(n_muni == 1, out, NA)
}

fr_keys = pa_frame_names |>
  mutate(
    k_raw = frame_name, k_id = precinct_yougov,
    k_norm = pa_norm(frame_name), k_notype = pa_norm(frame_name, keep_type = FALSE),
    k_single = single_key(county, k_norm), k_single_nt = single_key(county, k_notype)
  )

l2_keys = pa_l2 |>
  filter(!is.na(precinct_l2)) |>
  left_join(select(l2_manual, county, precinct_l2, k_manual = precinct_yougov), join_by(county, precinct_l2)) |>
  mutate(
    k_raw = precinct_l2,
    k_norm = pa_norm(precinct_l2), k_notype = pa_norm(precinct_l2, keep_type = FALSE),
    k_single = single_key(county, k_norm), k_single_nt = single_key(county, k_notype)
  )

l2_tiers = tribble(
  ~rule,           ~l2_key,       ~fr_key,
  "manual",        "k_manual",    "k_id",
  "name_exact",    "k_raw",       "k_raw",
  "name_norm",     "k_norm",      "k_norm",
  "name_notype",   "k_notype",    "k_notype",
  "single_pct",    "k_single",    "k_single",
  "single_pct_nt", "k_single_nt", "k_single_nt"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)
stopifnot(all(l2_manual$precinct_l2 %in% filter(l2_matched, l2_rule == "manual")$precinct_l2))
review_matches(l2_matched, fr_keys) |> invisible()

# Remaining frame precincts without L2 (left NA): LANCASTER 4906 (see above);
# LEHIGH 011905S ALLENTOWN 19TH WARD 5TH DT SAL (3 voters, no L2 counterpart).
#
# Ratio outliers above are all same-name matches (university precincts such as
# Radnor 2-2/7-3, Lower Oxford E; city wards in Pittsburgh, Philadelphia, York,
# Lancaster where L2 carries more voters), kept per the rules.

pa_l2 = pa_l2 |>
  left_join(select(l2_matched, county, precinct_l2, precinct_new = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  mutate(precinct = precinct_new) |>
  filter(!is.na(precinct)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = pa_frame |>
  tidylog::left_join(pa, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(pa_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(
  nrow(crosswalk) == nrow(pa_frame),
  # strictly 1:1 at the precinct level (frame rows repeat a precinct across CDs)
  crosswalk |> filter(!is.na(precinct_enr)) |> distinct(county, precinct_yougov, precinct_enr) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_enr))) |> pull(a) == 0,
  crosswalk |> filter(!is.na(precinct_l2)) |> distinct(county, precinct_yougov, precinct_l2) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_l2))) |> pull(a) == 0
)

crosswalk |>
  summarise(rows = n(), enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)),
            both = mean(!is.na(precinct_enr) & !is.na(precinct_l2)), .by = county) |>
  print(n = Inf)
crosswalk |>
  summarise(rows = n(), enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)),
            both = mean(!is.na(precinct_enr) & !is.na(precinct_l2))) |>
  print()

write_crosswalk("PA", crosswalk)
