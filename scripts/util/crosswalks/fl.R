source("scripts/util/crosswalks/helpers.R")

# Build the FL precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference state: nc.R.
#
# FL ENR comes as per-county files (`FL_{County}_latest.parquet`); as of
# 2026-10-20, 19 counties have 26_general data. Rerunning
# picks up new county files automatically, but each new county's id format must
# be checked (see the ENR recodes below).
#
# In FL the frame "names" are the precinct codes themselves (precinct26_name ==
# precinct26_id everywhere), and L2 also carries codes, sometimes with zero
# padding or an attached name. So L2 matching is code-based only.

fl_frame = read_frame("FL")
fl_counties = unique(fl_frame$county)

# ========================================
## ENR
# ========================================

# Polling place -> code; sources in docs/crosswalk_todo.md Results (Gadsden/Levy:
# county lists; Franklin, Gulf, Jackson, Liberty, Union: VoterFocus directories).
place_codes = c(
  "GADSDEN|Centenary Church" = "1", "GADSDEN|Havana Public Library" = "2",
  "GADSDEN|Concordia Baptist Church" = "3", "GADSDEN|Everybody's Church" = "4",
  "GADSDEN|Quincy Recreation Center" = "5", "GADSDEN|Wetumpka Fire Department" = "6",
  "GADSDEN|Union Chapel A.M.E. Church" = "7", "GADSDEN|Sycamore Methodist Ch." = "8",
  "GADSDEN|Chattahoochee REC Center" = "9", "GADSDEN|Mt. Pleasant Fire Dept." = "10",
  "GADSDEN|Gretna City Hall" = "11", "GADSDEN|Greensboro Town Hall" = "12",
  "GADSDEN|FAMU Research Center" = "13", "GADSDEN|Shiloh Church" = "14",
  "GADSDEN|St. James Baptist Church" = "15", "GADSDEN|National Guard Armory" = "16",
  "GADSDEN|New Bethel A.M.E. Church" = "17", "GADSDEN|St. Matthew Church" = "18",
  "GADSDEN|Hopewell Church" = "19", "GADSDEN|Assembly of God" = "20",
  "GADSDEN|Green Shade A.M.E. Church" = "21", "GADSDEN|Second Elizabeth Church" = "22",
  "GADSDEN|The Livestock Pavilion" = "23", "GADSDEN|St. Joseph MBC" = "24",
  "GADSDEN|Elizabeth A.M.E. Church" = "25",
  "LEVY|Levy County Ag Extension Office" = "01", "LEVY|Williston Highlands" = "02",
  "LEVY|Bronson First Baptist" = "03", "LEVY|Cedar Key City Hall" = "04",
  "LEVY|Willow Sink" = "05", "LEVY|Otter Creek Baptist Church" = "06",
  "LEVY|Williston City Hall" = "07", "LEVY|Morriston Baptist Church" = "08",
  "LEVY|Inglis Community Center" = "09", "LEVY|Pine Grove Baptist Church" = "10",
  "LEVY|Miracle Vision Tabernacle" = "11", "LEVY|College of Central Florida" = "12",
  "LEVY|Tommy Usher Center" = "13",
  "FRANKLIN|Eastpoint" = "01", "FRANKLIN|Alligator Point" = "02",
  "FRANKLIN|Holy Family Center" = "03", "FRANKLIN|Historic Holy Family" = "04",
  "FRANKLIN|Carrabelle" = "05", "FRANKLIN|Lanark Village" = "06",
  "FRANKLIN|St. George Island" = "07", "FRANKLIN|American Legion" = "08",
  "GULF|Honeyville Community Center" = "1", "GULF|Wewahitchka Public Library" = "2",
  "GULF|St. Joe Beach Fire Station" = "3", "GULF|Port St Joe Fire Station" = "4",
  "GULF|Centennial Building" = "5", "GULF|White City Fire Station" = "6",
  "GULF|Cape San Blas Fire Station" = "7", "GULF|Howard Creek Fire Station" = "8",
  "GULF|Overstreet Fire Station" = "9", "GULF|Highland View Fire Station" = "10",
  "JACKSON|Malone Joy Club" = "1", "JACKSON|Campbellton Comm Center" = "2",
  "JACKSON|Citizens Lodge" = "3", "JACKSON|Alford Comm Ctr" = "4",
  "JACKSON|Cypress Park" = "5", "JACKSON|Penn Ave Ag Complex" = "6",
  "JACKSON|Eastside Baptist Church" = "7", "JACKSON|Shady Grove Methodist Ch" = "8",
  "JACKSON|Grand Ridge Comm Ctr" = "9", "JACKSON|Welcome Assembly of God Ch" = "10",
  "JACKSON|Sneads City Hall" = "11", "JACKSON|Cottondale First Baptist Ch" = "12",
  "JACKSON|Greenwood Town Hall" = "13", "JACKSON|Graceville Civic Ctr" = "14",
  # Liberty 2 and 4 both normally vote at the Annex (L2 names both LAKE MYSTIC);
  # ENR lists units in precinct order (Rock Bluff, Annex, Bristol, Lake Mystic,
  # ...), so Annex = 2 and Lake Mystic = 4.
  "LIBERTY|Rock Bluff" = "1", "LIBERTY|Annex" = "2", "LIBERTY|Bristol" = "3",
  "LIBERTY|Lake Mystic" = "4", "LIBERTY|Hosford" = "5", "LIBERTY|Telogia" = "6",
  "LIBERTY|Sumatra" = "7", "LIBERTY|Orange" = "8",
  # Union: only single-precinct places. The others are sums of 2 frame precincts
  # and stay NA (strict 1:1): Lake Butler CC 1 = 1A+1B, Raiford CC 2A & = 2A+2B,
  # Worthington Springs = 3A+3B, SREC 4A & 4C, Lake Butler CC 5 = 5A+5C.
  "UNION|SREC Building 3C" = "3C", "UNION|Providence Community Center 4B" = "4B",
  "UNION|Palestine Fire Station 5B" = "5B"
)

enr_files = Sys.glob(election_path("FL/clean/*_latest.parquet")) |> fs::path_rel(election_path())

fl = read_enr(enr_files) |>
  mutate(
    # enightmodels::cbs_counties spells it MIAMIDADE; the frame uses MIAMI-DADE.
    county = recode(county, "MIAMIDADE" = "MIAMI-DADE"),
    # Miami-Dade geo is "{precinct}.{part}" ("436.0", "047.0"); frame is 3-digit
    # "436", "047". Parts > 0 are sub-precincts (e.g. 100.0 + 100.2-100.6 all sit
    # inside frame precinct 100). A frame precinct split into several ENR parts
    # is not 1:1, so all parts of such a precinct stay unmatched (NA by design).
    md_base = if_else(county == "MIAMI-DADE", str_pad(str_remove(precinct, "\\.\\d+$"), 3, pad = "0"), NA)
  ) |>
  mutate(
    precinct = case_when(
      county == "MIAMI-DADE" & n() > 1 ~ NA,
      county == "MIAMI-DADE" ~ md_base,
      .default = precinct
    ),
    .by = c(county, md_base)
  ) |>
  select(-md_base) |>
  # "406 - Redemption Point AG" -> "406". Collier "PCT101" -> "101"; Hendry "1" ->
  # "001". DeSoto "05A The House of God Church" -> "05": ENR 05A/11A/15A pair 1:1
  # with frame and L2 05/11/15 (no B parts), so the letter is a label, not a split.
  mutate(
    precinct = case_when(
      county == "DIXIE" ~ str_pad(str_extract(precinct, "^[0-9]+"), 2, pad = "0"),
      county == "COLLIER" ~ str_remove(precinct, "^PCT"),
      county == "HENDRY" ~ str_pad(precinct, 3, pad = "0"),
      county == "DESOTO" ~ str_extract(precinct, "^[0-9]+"),
      county %in% c("CITRUS", "COLUMBIA", "GLADES", "MONROE") ~ str_extract(precinct, "^[0-9]+"),
      # 2026-10-26: "1-Waldo Branch Library" (Alachua, Hardee: frame pads to 2),
      # "29 West Navarre", Washington "0001 Caryville" (frame "1"), St. Johns "PCT 211".
      county %in% c("ALACHUA", "HARDEE", "MARTIN") ~ str_pad(str_extract(precinct, "^[0-9]+"), 2, pad = "0"),
      county %in% c("NASSAU", "SANTA ROSA", "SUMTER", "WAKULLA") ~ str_extract(precinct, "^[0-9]+"),
      county == "WASHINGTON" ~ as.character(as.integer(str_extract(precinct, "^[0-9]+"))),
      county == "ST JOHNS" ~ str_remove(precinct, "^PCT "),
      county %in% c("GADSDEN", "LEVY", "FRANKLIN", "GULF", "JACKSON", "LIBERTY", "UNION") ~ unname(place_codes[paste(county, precinct, sep = "|")]),
      .default = precinct
    )
  )
stopifnot(
  !anyDuplicated(filter(fl, !is.na(precinct))[c("county", "precinct")]),
  !anyDuplicated(fl[c("county", "precinct_enr")])
)

# Miami-Dade precincts split into sub-precincts in ENR (left NA).
fl_enr_split = fl |>
  filter(is.na(precinct)) |>
  mutate(base = str_remove(precinct_enr, "\\.\\d+$")) |>
  summarise(parts = paste(sort(precinct_enr), collapse = " "), .by = c(county, base))
print(fl_enr_split, n = Inf)

# ========================================
## L2 codes -> frame codes
# ========================================
# L2 formats by county: plain codes matching the frame (most counties), codes
# with different zero padding (Bradford "01" vs frame "1", Hendry/Taylor "01" vs
# "001", Columbia "015" vs "15"), "NAME (code)" (Citrus), and "code-NAME"
# (Baker "4A-BAXTER", Levy "01-BRONSON I").

fl_frame_names = frame_names("FL") |> filter(county %in% fl_counties)
stopifnot(all(fl_frame_names$frame_name == fl_frame_names$precinct_yougov))

fl_l2 = enightdata::read_local(
  fs::path_rel(Sys.glob(election_path("input_data/l2/fl_*_precincts.parquet")), election_path()),
  parquet = TRUE
) |>
  as_tibble() |>
  filter(county %in% fl_counties) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

std_code = function(x) str_remove(str_to_upper(str_squish(x)), "^0+(?=[0-9A-Z])")

fr_keys = fl_frame_names |>
  mutate(k_id = precinct_yougov, k_idstd = std_code(precinct_yougov))

l2_keys = fl_l2 |>
  filter(!is.na(precinct_l2)) |>
  mutate(
    k_raw = precinct_l2,
    k_paren = std_code(str_match(precinct_l2, "\\(([0-9A-Z]+)\\)\\s*$")[, 2]), # Citrus "RED LEVEL (100)"
    k_lead = std_code(str_match(precinct_l2, "^([0-9]+[A-Z]?)-[A-Z]")[, 2]),   # Baker "4A-BAXTER", Levy "01-BRONSON I"
    k_std = std_code(precinct_l2)
  )

l2_tiers = tribble(
  ~rule,        ~l2_key,   ~fr_key,
  "code_exact", "k_raw",   "k_id",
  "paren_code", "k_paren", "k_idstd",
  "lead_code",  "k_lead",  "k_idstd",
  "code_std",   "k_std",   "k_idstd"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)

review_matches(l2_matched, fr_keys)

# Unmatched frame precincts (all tiny, n_frame <= 2, no L2 counterpart; left NA):
# BROWARD D010, K008; PALM BEACH 8741, 8481.

fl_l2 = fl_l2 |>
  left_join(select(l2_matched, county, precinct_l2, precinct_new = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  mutate(precinct = precinct_new) |>
  filter(!is.na(precinct)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = fl_frame |>
  tidylog::left_join(fl, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(fl_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(nrow(crosswalk) == nrow(fl_frame))

write_crosswalk("FL", crosswalk)
