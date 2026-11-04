source("scripts/util/crosswalks/helpers.R")

# Build the SC precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md; reference: nc.R.
#
# Frame precinct26_id is the county's 3-digit precinct code ("001") with an
# uppercase name ("ABBEVILLE NO 1"). ENR (SC Clarity) carries names only
# ("Abbeville No. 1", "N. Augusta No. 25", "RACEPATH #1"), so ENR is matched to
# frame names. VoteHub unit_ids embed the same county code ("45003-:-007"), which
# gives a second, name-independent route for ENR names that are misspelled in the
# feed ("ORNAGEBURG WD 7", "BUNRS-DOWNS").

sc_frame = read_frame("SC")
sc_frame_names = frame_names("SC")

# SC-specific name key on top of std_name(): drop apostrophes, "NO" and spaces,
# expand the abbreviations the feed and frame disagree on.
sc_key = function(x) {
  std_name(x) |>
    str_remove_all("'") |>
    str_replace_all("\\bN\\b", "NORTH") |>
    str_replace_all("\\bSPRGS\\b", "SPRINGS") |>
    str_replace_all("\\b(ROADS?|RD)\\b", "RD") |>
    str_remove_all("\\b(NO|PRECINCT)\\b") |>
    str_remove_all("\\bNO(?=\\d)") |>
    str_remove_all(" ")
}

# ========================================
## ENR names -> frame codes
# ========================================

sc_enr_raw = read_enr("SC/clean/SC_latest.parquet")

# Non-geographic ENR buckets, never matched
enr_bucket = "(?i)provisional|failsafe|absentee|emergency|curbside"

# ENR -> VoteHub unit_id (votehub.R); the code after "-:-" is the county code.
vh_code = read_csv(election_path("input_data/crosswalks/votehub/sc.csv"), col_types = cols(.default = "c")) |>
  filter(!is.na(unit_id)) |>
  transmute(county, precinct_enr, k_vh = str_remove(unit_id, "^\\d+-:-"))

# Hand-verified one-offs: the only ENR and frame leftovers in the county, same
# place, no VoteHub code.
enr_manual = tribble(
  ~county,       ~precinct_enr,          ~k_manual, # frame name
  "KERSHAW",     "Rabon'S X Roads",      "143",     # RABON'S CROSSROADS
  "MARLBORO",    "E BENNETTSVILLE",      "012",     # EAST BENNETTSVILLE
  "PICKENS",     "McAllister",           "161",     # MCALISTER
  "SPARTANBURG", "Morningside Baptist",  "007",     # MORNINGSIDE
  "SUMTER",      "GREEN SWAMP",          "118",     # GREEN SWAMP 1 (GREEN SWAMP 2 = 157 matched by name)
  "UNION",       "Monarch",              "013"      # MONARCH BOX 1
)

fr_keys = sc_frame_names |>
  mutate(k_id = precinct_yougov, k_std = std_name(frame_name), k_sc = sc_key(frame_name))

enr_keys = sc_enr_raw |>
  filter(!str_detect(precinct_enr, enr_bucket)) |>
  left_join(vh_code, join_by(county, precinct_enr)) |>
  left_join(enr_manual, join_by(county, precinct_enr)) |>
  transmute(county, precinct_l2 = precinct_enr, n_l2 = NA_integer_,
            k_manual, k_std = std_name(precinct_enr), k_sc = sc_key(precinct_enr), k_vh)

enr_tiers = tribble(
  ~rule,      ~l2_key,    ~fr_key,
  "manual",   "k_manual", "k_id",
  "name_std", "k_std",    "k_std",
  "name_sc",  "k_sc",     "k_sc",
  "votehub",  "k_vh",     "k_id"   # feed typos and truncated frame names
)

# Kershaw "Camden No. 2 & 3" is a merge of frame 108 (CAMDEN NO 2) and Camden 3
# (no frame row): left NA (rule 3).
enr_matched = match_tiers(enr_keys, fr_keys, enr_tiers) |>
  rename(precinct_enr = precinct_l2, enr_rule = l2_rule)
stopifnot(all(enr_manual$precinct_enr %in% filter(enr_matched, enr_rule == "manual")$precinct_enr))
count(enr_matched, enr_rule) |> print()
enr_matched |> filter(!enr_rule %in% c("name_std")) |>
  select(county, precinct_enr, precinct_yougov, frame_name, enr_rule) |> print(n = Inf)

sc_enr = enr_matched |> transmute(state = "SC", county, precinct = precinct_yougov, precinct_enr)

# ========================================
## L2 names -> frame codes
# ========================================
# L2 has names ("AIKEN 01", "CLEARWATER 12", "ST ANDREWS 24"). Frame names
# first, then the matched ENR name for the frame code (L2 follows the current
# official names where the frame label is stale: Aiken 007 BATH, frame "AIKEN NO 7").

sc_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/sc_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

# Broader name key for L2: "&"/AND dropped, directions and street-style
# abbreviations spelled out ("MULLINS NE" = NORTHEAST MULLINS after word bag,
# "ANTIOCH-KINGS CK" = ANTIOCH AND KING'S CREEK).
sc_words = function(x) {
  dirs = c(NE = "NORTHEAST", NW = "NORTHWEST", SE = "SOUTHEAST", SW = "SOUTHWEST",
           N = "NORTH", S = "SOUTH", E = "EAST", W = "WEST")
  abbr = c(CK = "CREEK", HTS = "HEIGHTS", HGTS = "HEIGHTS", SPGS = "SPRINGS", SPRGS = "SPRINGS",
           CR = "CROSS", SAINT = "ST", MT = "MOUNT", ELEM = "ELEMENTARY", REC = "RECREATION",
           "99" = "NINETY NINE")
  w = std_name(str_replace_all(x, "&", " ")) |> str_remove_all("'") |> str_remove_all("\\b(AND|NO|PRECINCT)\\b")
  w = str_replace_all(w, setNames(c(dirs, abbr), paste0("\\b", names(c(dirs, abbr)), "\\b")))
  str_squish(w)
}
nospace = function(x) str_remove_all(x, " ")
first_word = function(x) str_extract(sc_words(x), "^[A-Z]+")
tail_code = function(x) {
  d = str_match(x, "\\b(\\d{1,3})$")[, 2]
  if_else(is.na(d), NA_character_, str_pad(d, 3, pad = "0"))
}

# Counties whose L2 names are not the frame names: translate into frame codes.
county_codes = function(county, x) {
  out = rep(NA_character_, length(x))
  # Richland: L2 "COLUMBIA 07" = frame WARD 7 = code 107 (codes 101-134)
  r = county == "RICHLAND" & str_detect(x, "^COLUMBIA \\d{2}$")
  out[r] = as.character(100L + as.integer(str_sub(x[r], -2)))
  out
}

# Hand-verified one-offs: only candidate in the county, L2/frame ratio ~1.
l2_manual = tribble(
  ~county,      ~precinct_l2,            ~k_manual, # frame name (n_l2 / n_frame)
  "BERKELEY",   "BEVERLY HILLS",         "054", # BEVERLY HILL (1297 / 1225)
  "CHARLESTON", "SEABROOK TOWN",         "181", # SEABROOK (2382 / 2406)
  "CHEROKEE",   "WILKINS -METCALF",      "032", # WILKINSVILLE AND METCALF (610 / 583)
  "HORRY",      "NIXON X ROADS 3",       "225", # NIXONS X ROADS 3 (2209 / 2172)
  "LANCASTER",  "CHESTERFIELD AVE",      "005", # CHESTERFIELD AVENUE (3335 / 3016)
  "LAURENS",    "MARTINS-POPLAR SPRING", "028", # MARTINS-POPLAR SPRINGS (702 / 689)
  "LEE",        "ELLIOT",                "009", # ELLIOTT (378 / 367)
  "NEWBERRY",   "PROSPERITY",            "036", # PROSPERITY CITY (758 / 735); 053 PROSPERITY OUTSIDE matched by name
  "NEWBERRY",   "ST PHILIPS JOLLY ST",   "042", # ST PHILIPS-JOLLY STREET (1095 / 1051)
  "OCONEE",     "MT REST",               "009", # MOUNTAIN REST (1086 / 1081)
  "OCONEE",     "NEWRY CORNITH",         "001", # NEWRY CORINTH (362 / 358)
  "PICKENS",    "RICE CREEK",            "128", # RICES CREEK (1286 / 1270)
  "SALUDA",     "WARD",                  "020"  # WARD (659 / 624); std_name() drops WARD
)

fr_keys = fr_keys |>
  left_join(select(sc_enr, county, precinct, precinct_enr), join_by(county, precinct_yougov == precinct)) |>
  mutate(
    k_enrstd = std_name(precinct_enr), k_enrsc = sc_key(precinct_enr),
    k_w = nospace(sc_words(frame_name)), k_bag = nospace(bag(sc_words(frame_name))),
    k_enrw = nospace(sc_words(precinct_enr)), k_enrbag = nospace(bag(sc_words(precinct_enr))),
    k_tailf = paste(first_word(frame_name), precinct_yougov),
    k_taile = if_else(is.na(precinct_enr), NA, paste(first_word(precinct_enr), precinct_yougov))
  ) |>
  select(-precinct_enr)

l2_keys = sc_l2 |>
  filter(!is.na(precinct_l2), precinct_l2 != "") |>
  left_join(l2_manual, join_by(county, precinct_l2)) |>
  mutate(
    k_raw = precinct_l2, k_std = std_name(precinct_l2), k_sc = sc_key(precinct_l2),
    k_paren = str_match(precinct_l2, "\\((\\d{3})\\)\\s*$")[, 2],  # Spartanburg "HAYNE BAPTIST (041)"
    k_county = county_codes(county, precinct_l2),
    k_w = nospace(sc_words(precinct_l2)), k_bag = nospace(bag(sc_words(precinct_l2))),
    k_w1 = nospace(str_remove(sc_words(precinct_l2), " 1$")),  # Dorchester "ARCHDALE 1" = ARCHDALE
    k_tail = if_else(is.na(tail_code(precinct_l2)), NA, paste(first_word(precinct_l2), tail_code(precinct_l2))) # Aiken "HAMMOND 48"
  )

l2_tiers = tribble(
  ~rule,         ~l2_key,    ~fr_key,
  "manual",      "k_manual", "k_id",
  "name_std",    "k_std",    "k_std",
  "name_sc",     "k_sc",     "k_sc",
  "paren_code",  "k_paren",  "k_id",
  "county_rule", "k_county", "k_id",
  "enr_std",     "k_std",    "k_enrstd",
  "enr_sc",      "k_sc",     "k_enrsc",
  "name_words",  "k_w",      "k_w",
  "enr_words",   "k_w",      "k_enrw",
  "name_bag",    "k_bag",    "k_bag",
  "enr_bag",     "k_bag",    "k_enrbag",
  "name_drop1",  "k_w1",     "k_w",
  "tail_code",   "k_tail",   "k_tailf",
  "tail_code_enr", "k_tail", "k_taile"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)
stopifnot(all(l2_manual$precinct_l2 %in% filter(l2_matched, l2_rule == "manual")$precinct_l2))
# Ratio outliers kept: same official name, growth since the frame vintage near
# campuses (Orangeburg 04 = ORANGEBURG WARD 4 near SC State, 929 / 565; Richland
# COLUMBIA 02 / 08 near USC, 1.33), Beaufort BURTON 1D (exact name, 0.70).
review_matches(l2_matched, fr_keys)
l2_matched |> filter(!l2_rule %in% c("name_std", "name_sc")) |>
  select(county, precinct_l2, precinct_yougov, frame_name, l2_rule, n_l2, n_frame) |> print(n = Inf)
cat("L2 leftovers:\n")
anti_join(l2_keys, l2_matched, join_by(county, precinct_l2)) |> select(county, precinct_l2, n_l2) |> print(n = Inf)

sc_l2 = sc_l2 |>
  select(-precinct) |>
  inner_join(select(l2_matched, county, precinct_l2, precinct = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

# ========================================
## Crosswalk
# ========================================

# Frame rows are county x cd x code; precinct26_id repeats across CDs in 20
# split precincts, and each CD piece gets the same ENR and L2 id.
crosswalk = sc_frame |>
  tidylog::left_join(sc_enr, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(sc_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(nrow(crosswalk) == nrow(sc_frame))

crosswalk |> filter(is.na(precinct_enr) | is.na(precinct_l2)) |> print(n = Inf)
cat("ENR precincts with no frame code:\n")
anti_join(sc_enr_raw, sc_enr, join_by(county, precinct_enr)) |>
  filter(!str_detect(precinct_enr, enr_bucket)) |> select(county, precinct_enr) |> print(n = Inf)
crosswalk |> summarise(n = n(), enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)),
                       both = mean(!is.na(precinct_enr) & !is.na(precinct_l2))) |> print()

write_crosswalk("SC", crosswalk)
