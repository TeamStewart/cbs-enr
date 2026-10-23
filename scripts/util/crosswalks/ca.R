source("scripts/util/crosswalks/helpers.R")

# Build the CA precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference state: nc.R.
#
# CA is partial: as of 2026-10-15 only San Diego has 26_general ENR data. L2 is
# matched for all 17 frame counties, since it does not depend on ENR.
#
# In CA the frame "names" (precinct26_name) are just the codes, so every L2 match
# is code-based. Formats differ by county (see the notes at each tier below).

ca_frame = read_frame("CA")
ca_frame_names = frame_names("CA") |> filter(county %in% ca_frame$county)

# Numeric precinct ids: drop a trailing ".0"/".00" and zero padding, so that
# frame "416800.0" / "0052190" / "02391" line up with L2 "416800" / "52190" / "2391".
norm_num = function(x) x |> str_remove("\\.0+$") |> str_remove("^0+(?=\\d)")

# ========================================
## ENR
# ========================================
# San Diego ENR `geo` precincts are *consolidated* election precincts, e.g.
# "0002-100090-SAN PASQUAL" = consolidation 0002, named after base precinct
# 100090. There are 2,994 of them vs 6,427 frame (base) precincts, and 1,060 of
# the namesake base codes have no registered voters in the frame at all (e.g.
# 100090, 100120, 100150). The frame and L2 are both at the base-precinct level.
# Joining a consolidation to its namesake base precinct would attach a merge of
# several base precincts to one frame row (rule 3), and no membership list for
# the 2026 consolidations is available, so San Diego ENR is left NA on purpose.
# ENR files for other counties are joined on the raw precinct id (none yet;
# check the format when they land and add county-specific recodes here).

enr_files = Sys.glob(election_path("CA/clean/*_latest.parquet")) |>
  str_remove(fixed(paste0(election_path(), "/")))
stopifnot(length(enr_files) > 0)

# Solano (196 ENR vs 608 frame precincts) also reports consolidations named after
# one base precinct (Solano "17025" = frame "17025.00" plus neighbours), so it is
# left NA for the same reason; see ca_solano_consolidated.R.
# Sonoma ENR has two kinds of ids. 331 `07xxxxx` consolidations (97% of voters)
# use their own numbering, with no link to frame base codes; they match nothing
# here and no membership list is available. The other 72 are frame-style base
# codes: the 50 that are frame bases have Clarity totalVoters equal to the frame
# n (11,043 vs 11,040; ratios 0.67-1.11), so they are single precincts and join 1:1.
# San Francisco ENR is "pct 1101" or "pct 1103 mb" (mail-ballot precinct), 1:1
# with frame "1101"; ENR 9901-9904 have no frame row.
ca_consolidated = c("SAN DIEGO", "SOLANO")

ca_enr = read_enr(enr_files) |>
  mutate(precinct = case_when(
    county %in% ca_consolidated ~ NA_character_, # see above
    county == "SAN FRANCISCO" ~ str_extract(precinct, "^pct (\\d+)( mb)?$", group = 1),
    .default = precinct
  )) |>
  filter(!is.na(precinct)) |>
  filter(n() == 1, .by = c(county, precinct))

# ========================================
## L2 -> frame codes
# ========================================

ca_l2 = enightdata::read_local("input_data/l2/ca_20260901_precincts.parquet", parquet = TRUE) |>
  as_tibble() |>
  filter(county %in% ca_frame$county) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

# Counties where the L2 string ends in the frame's numeric code (bare, after a
# place name, or in parentheses), possibly zero-padded differently:
#   BUTTE/MADERA/MARIN/SAN FRANCISCO "10009" (raw); SANTA CLARA "2784" = 0002784;
#   ORANGE "ANAHEIM-02016"; RIVERSIDE "TEMECULA-4 (334008)"; SACRAMENTO
#   "ARCADE (0024500)"; SAN DIEGO "JAMUL 563220" = 563220.0; SOLANO "VALLEJO
#   17195" = 17195.00; SONOMA "PCT 0500173"; IMPERIAL "245230 MAIL" = 245230.00.
num_counties = c("BUTTE", "IMPERIAL", "MADERA", "MARIN", "ORANGE", "RIVERSIDE", "SACRAMENTO",
                 "SAN DIEGO", "SAN FRANCISCO", "SANTA CLARA", "SOLANO", "SONOMA")
l2_code_num = function(county, x) {
  num = str_match(x, "(\\d+)\\)?(?: MAIL)?$")[, 2]
  if_else(county %in% num_counties, norm_num(num), NA_character_)
}

# CONTRA COSTA: L2 "PLEASANT HILL 120" -> frame "PLHL120" (4-letter place
# abbreviation + number). Every L2 place maps to exactly one frame prefix with
# the same precinct count (checked 2026-10-15).
cc_pfx = c(
  "ALAMO"="ALMO", "ALHAMBRA"="ALHA", "ANTIOCH"="ANTI", "AYERS RANCH"="AYER", "BAYPOINT"="BAPO",
  "BETHEL ISLAND"="BETH", "BLACK DIAMOND"="BKDI", "BLACKHAWK"="BKHW", "BRENTWOOD"="BREN",
  "BRIONES HILLS"="BRHL", "BRIONES VALLEY"="BRVL", "BYRON"="BYRN", "CALDECOTT"="CALD", "CANYON"="CANY",
  "CASTLE HILL"="CHIL", "CASTLE ROCK"="CASR", "CLAYTON"="CLAY", "CLYDE"="CLYD", "CONCORD"="CONC",
  "CROCKETT"="CROC", "DANVILLE"="DANV", "DIABLO"="DIAB", "DISCOVERY BAY"="DBAY", "EAST RICHMOND"="ERIC",
  "EL CERRITO"="ELCR", "EL SOBRANTE"="ELSO", "FAIRGROUNDS"="FAIR", "GIANT"="GIAN", "HERCULES"="HERC",
  "KENSINGTON"="KENS", "KNIGHTSEN"="KNGT", "LAFAYETTE"="LFET", "LAS TRAMPAS"="LAST", "MARSH CREEK"="MARC",
  "MARTINEZ"="MART", "MORAGA"="MORG", "MOUNT DIABLO"="MDIA", "MOUNTAINVIEW"="MVIE", "NICHOLS"="NICH",
  "NORRIS CANYON"="NCYN", "NORTH RICHMOND"="NRIC", "NORTONVILLE"="NRTV", "OAKLEY"="OKLY", "OLEUM"="OLEU",
  "ORINDA"="ORDA", "PACHECO"="PACH", "PINOLE"="PINL", "PITTSBURG"="PTTS", "PLEASANT HILL"="PLHL",
  "PORT COSTA"="PRCO", "RICHMOND"="RICH", "RODEO"="RODO", "ROLLINGWOOD"="ROLL", "SAINT MARYS"="STMY",
  "SAN PABLO"="SPAB", "SAN RAMON"="SRAM", "SARANAP"="SARA", "SHELL RIDGE"="SHEL", "TASSAJARA"="TASJ",
  "TESORO"="TESO", "VIERA"="VIER", "VINE HILL"="VINE", "WALDON"="WALD", "WALNUT CREEK"="WLCR",
  "WALNUT KNOLLS"="WLKN"
)

# SAN MATEO: L2 "SAN CARLOS 012" / "BELMONT 022 SP" -> frame "36012" / "21022"
# (2-digit area prefix + 3-digit number). The prefix -> place pairs were found
# by joining on the number and checking L2/frame voter counts; only prefixes
# whose numbers line up with one place set (or a disjoint split of one place,
# e.g. BELMONT = 21 + 31) are used. Prefix 39 is unclear and left out.
# An L2 row gets a code only if exactly one of its place's prefixes has that
# number in the frame. MB/SP/NV/OUT suffixes are dropped, so "BELMONT 035" and
# "BELMONT 035 SP" collide and both stay NA (the tier requires unique keys).
sm_pfx = tribble(
  ~place, ~fp,
  "BURLINGAME", "10", "SAN BRUNO", "12", "SAN BRUNO", "52", "HILLSBOROUGH", "14", "MILLBRAE", "15",
  "SAN MATEO OUT", "17", "BURL HILLS", "17", "CLUB", "17",
  "SO SAN FRANCISCO", "18", "SO SAN FRANCISCO", "58", "BELMONT", "21", "BELMONT", "31",
  "FOSTER CITY", "22", "SAN MATEO", "26", "BELMONT OUT", "27", "ATHERTON", "30", "HALF MOON BAY", "33",
  "PACIFICA", "35", "SAN CARLOS", "36", "WOODSIDE", "38", "EAST PALO ALTO", "40",
  # MENLO PARK 001-019 exist under both 34 and 44 (L2 count ~= sum of the two),
  # so those stay NA via the one-candidate rule; 031-050 are only under 44.
  "MENLO PARK", "34", "MENLO PARK", "44",
  "REDWOOD CITY", "46", "OAKS", "47", "SELBY", "47", "REDWOOD OUT", "47", "MENLO OAKS", "47",
  "BRISBANE", "50", "COLMA", "53", "DALY CITY", "56", "BROADMOOR", "57", "SAN BRUNO OUT", "57",
  # 37: unincorporated coast/hills, each place its own number block
  "LAKE", "37", "PALOMAR", "37", "GRANADA", "37", "FARALLONE", "37", "HMB OUT", "37", "LA HONDA", "37",
  "PESCADERO", "37", "SAN GREGORIO", "37", "SKYLINE", "37", "ALAMEDA", "37", "LADERA", "37",
  "LOS TRANCOS", "37", "STANFORD HILLS", "37", "PACIFICA OUT", "37", "SAN CARLOS OUT", "37",
  "CRYSTAL SPRINGS", "37"
)
sm_codes = ca_l2 |>
  filter(county == "SAN MATEO") |>
  mutate(m = str_match(precinct_l2, "^(.*?) (\\d{3})(?: (?:MB|SP|NV|OUT))?$"), place = m[, 2], num = m[, 3]) |>
  inner_join(sm_pfx, join_by(place), relationship = "many-to-many") |>
  mutate(code = paste0(fp, num)) |>
  semi_join(filter(ca_frame_names, county == "SAN MATEO"), join_by(code == precinct_yougov)) |>
  filter(n() == 1, .by = precinct_l2) |>
  select(county, precinct_l2, code)

county_codes = function(county, x) {
  out = rep(NA_character_, length(x))
  cc = county == "CONTRA COSTA"
  ccx = str_match(x, "^(.*) (\\d{3})$")
  out[cc] = if_else(is.na(cc_pfx[ccx[cc, 2]]), NA, paste0(cc_pfx[ccx[cc, 2]], ccx[cc, 3]))
  out
}

# No hand one-offs yet.
l2_manual = tribble(~county, ~precinct_l2, ~precinct_yougov)

# Not matched on purpose (no reliable name -> code link in the inputs):
#   MERCED, SHASTA: L2 has place names ("BEACHWOOD 1", "REDDING 15"), frame has
#     numeric codes ("17202", "0290630").
#   VENTURA: L2 "OXNARD 5-001" vs frame "0055001"; the numbers do not line up
#     with any prefix mapping.
#   Frame split codes ("10906-B" in MARIN, "0121120-A" in SHASTA, "38003-0" in
#     SAN MATEO) have no L2 counterpart; L2 carries only the parent precinct.

fr_keys = ca_frame_names |>
  mutate(k_raw = frame_name, k_id = precinct_yougov, k_num = norm_num(precinct_yougov))

l2_keys = ca_l2 |>
  filter(!is.na(precinct_l2)) |>
  left_join(select(l2_manual, county, precinct_l2, k_manual = precinct_yougov), join_by(county, precinct_l2)) |>
  left_join(rename(sm_codes, k_sm = code), join_by(county, precinct_l2)) |>
  mutate(
    k_raw = precinct_l2,
    k_num = l2_code_num(county, precinct_l2),
    k_county = coalesce(county_codes(county, precinct_l2), k_sm)
  )

l2_tiers = tribble(
  ~rule,         ~l2_key,    ~fr_key,
  "manual",      "k_manual", "k_id",
  "name_exact",  "k_raw",    "k_raw",
  "code_exact",  "k_raw",    "k_id",
  "code_num",    "k_num",    "k_num",
  "county_rule", "k_county", "k_id"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)

# Same code, different area: drop non-manual matches whose L2/frame voter ratio
# is far off for a precinct of real size. These are boundary changes between the
# frame and L2 vintages, e.g. SONOMA 0500174 (L2 321 vs frame 683; L2 has new
# precincts 0500321-0500328 carved out of the 05001xx block) and SAN DIEGO
# 395050 (417 vs 257).
l2_dropped = l2_matched |>
  filter(l2_rule != "manual", pmax(n_l2, n_frame) >= 100, n_l2 / n_frame < 2/3 | n_l2 / n_frame > 1.5)
print(select(l2_dropped, county, precinct_l2, precinct_yougov, l2_rule, n_l2, n_frame), n = Inf)
l2_matched = anti_join(l2_matched, l2_dropped, join_by(county, precinct_l2))
stopifnot(all(l2_manual$precinct_l2 %in% filter(l2_matched, l2_rule == "manual")$precinct_l2))

review_matches(l2_matched, fr_keys) |> invisible()

ca_l2 = ca_l2 |>
  left_join(select(l2_matched, county, precinct_l2, precinct_new = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  mutate(precinct = precinct_new) |>
  filter(!is.na(precinct)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = as_tibble(ca_frame) |>
  tidylog::left_join(ca_enr, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(ca_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(
  nrow(crosswalk) == nrow(ca_frame),
  !anyDuplicated(drop_na(distinct(crosswalk, county, precinct_yougov, precinct_enr))[c("county", "precinct_enr")]),
  !anyDuplicated(drop_na(distinct(crosswalk, county, precinct_yougov, precinct_l2))[c("county", "precinct_l2")])
)

write_crosswalk("CA", crosswalk)
