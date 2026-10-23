source("scripts/util/crosswalks/helpers.R")

# Build the OH precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference: nc.R.
#
# OH is partial: 26_general ENR exists only for some of the 13 frame counties
# (Franklin and Stark as of 2026-10-23). L2 matching covers all frame counties.
# Rerun when new `OH/clean/OH_*_latest.parquet` files land; they are picked up
# by the glob, but each new county's ENR id format must be checked (see ENR).
#
# Frame ids are SOS precinct codes ("25ARA"); frame names are county precinct
# names with the code appended ("COLS 74-C (25-ARA)"). L2 and ENR carry names
# only, so everything is matched on names. Several counties abbreviate the
# municipality in the frame names ("COLS" = COLUMBUS, "T-1" = Springfield
# Township 1); `abbr` expands them on every side.

oh_frame = read_frame("OH")
oh_frame_names = frame_names("OH") |>
  semi_join(oh_frame, join_by(county, precinct_yougov))
stopifnot(!anyDuplicated(oh_frame_names[c("county", "precinct_yougov")]))

# ========================================
## Name standardization (OH-specific, on top of std_name)
# ========================================

# Municipality abbreviations, anchored at the start of the standardized name.
# Each was checked against the other source: same precinct suffixes on both
# sides and the same number of precincts per municipality.
abbr = list(
  CLARK = c(
    "^BETH\\b" = "BETHEL", "^GERM\\b" = "GERMAN", "^GREE\\b" = "GREEN", "^HARM\\b" = "HARMONY",
    "^MFLD\\b" = "MOOREFIELD", "^MR\\b" = "MAD RIVER", "^PLES\\b" = "PLEASANT",
    "^CITY\\b" = "SPRINGFIELD CITY", "^T\\b" = "SPRINGFIELD TWP" # L2 SPRINGFIELD CITY 01-27 / TWP 01-08
  ),
  FRANKLIN = c(
    "^COLS\\b" = "COLUMBUS", "^DUB\\b" = "DUBLIN", "^GAH\\b" = "GAHANNA", "^HILL\\b" = "HILLIARD",
    "^REYNS\\b" = "REYNOLDSBURG", "^UA\\b" = "UPPER ARLINGTON", "^WORTH\\b" = "WORTHINGTON",
    "^CANAL WIN\\b" = "CANAL WINCHESTER", "^GRANDVIEW\\b(?! HEIGHTS)" = "GRANDVIEW HEIGHTS"
  ),
  HAMILTON = c("^NO COLL HILL\\b" = "NORTH COLL HILL"),
  SUMMIT = c("^CUY FALLS\\b" = "CUYAHOGA FALLS", "^NFLD CTR\\b" = "NORTHFIELD CENTER")
)

num_words = c(ONE = "1", TWO = "2", THREE = "3", FOUR = "4", FIVE = "5", SIX = "6", SEVEN = "7", EIGHT = "8", NINE = "9", TEN = "10")

oh_std = function(county, x) {
  x = std_name(x) |>
    str_replace_all("\\b(ONE|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN)\\b", \(m) num_words[m]) |> # Fulton "DELTA TWO"
    str_replace_all("\\bTOWNSHIP\\b", "TWP") |>
    str_replace_all("\\b(VILLAGE|VILL)\\b", "VLG") |>
    str_remove_all("\\b0\\b") |> # Cuyahoga "BEACHWOOD-00-A": ward 00 = no ward
    str_squish()
  for (cty in names(abbr)) {
    i = county == cty
    x[i] = str_replace_all(x[i], abbr[[cty]])
  }
  x
}
# Second key: drop the municipality type (TWP/VLG/CITY), for sources that leave
# it out ("BROWN A" vs "BROWN TWP A"). Only used when unique on both sides.
drop_type = function(x) str_squish(str_remove_all(x, "\\b(TWP|VLG|CITY)\\b"))

fr_keys = oh_frame_names |>
  mutate(
    k_id = precinct_yougov,
    k_raw = str_remove(frame_name, "\\s*\\([^)]*\\)$"),
    k_std = oh_std(county, frame_name),
    k_type = drop_type(k_std)
  )

# ========================================
## ENR names -> frame codes
# ========================================
# Franklin (county API): lowercase full names ("columbus 26-b",
#   "blendon township-a"); standardized names match the frame directly.
# Stark: short codes ("cc3a" = CANTON CITY 3-A, "lak17" = LAKE 17, "laka" =
#   LAKE A). `stark_pfx` spells out the prefix; the per-prefix precinct counts
#   and suffixes are identical to the frame's.
# Any other county: stop until its id format has been checked.

enr_files = fs::path_rel(Sys.glob(election_path("OH/clean/OH_*_latest.parquet")), election_path())
stopifnot(length(enr_files) > 0)

oh = read_enr(enr_files)
stopifnot(all(oh$county %in% c("FRANKLIN", "STARK")))

stark_pfx = c(
  alli = "ALLIANCE", bet = "BETHLEHEM", can = "CANTON TWP", cc = "CANTON CITY", cnful = "CANAL FULTON",
  jac = "JACKSON", lak = "LAKE", law = "LAWRENCE", lex = "LEXINGTON", lsvl = "LOUISVILLE", mar = "MARLBORO",
  mass = "MASSILLON", mlake = "MEYERS LAKE", nav = "NAVARRE", nc = "NORTH CANTON", nim = "NIMISHILLEN",
  osn = "OSNABURG", par = "PARIS", per = "PERRY", pik = "PIKE", pla = "PLAIN", san = "SANDY",
  sug = "SUGARCREEK", tus = "TUSCARAWAS", was = "WASHINGTON"
)
stark_name = function(x) {
  m = str_match(x, paste0("^(", paste(names(stark_pfx), collapse = "|"), ")(\\d*)([a-z]?)$"))
  out = str_squish(paste(stark_pfx[m[, 2]], m[, 3], str_to_upper(m[, 4])))
  out[x == "mlake"] = "MEYERS LAKE A" # the village's only precinct
  if_else(is.na(m[, 1]), NA_character_, out)
}

enr_keys = oh |>
  mutate(
    name = if_else(county == "STARK", stark_name(precinct), precinct),
    n_l2 = NA_integer_, precinct_l2 = precinct_enr,
    k_std = oh_std(county, name), k_type = drop_type(k_std)
  )
print(filter(enr_keys, is.na(name)))

enr_tiers = tribble(
  ~rule,       ~l2_key,  ~fr_key,
  "name_std",  "k_std",  "k_std",
  "name_type", "k_type", "k_type"
)
enr_matched = match_tiers(enr_keys, fr_keys, enr_tiers)
print(count(enr_matched, county, l2_rule))

enr_left = anti_join(enr_keys, enr_matched, join_by(county, precinct_l2))
print(select(enr_left, county, precinct_enr, k_std), n = Inf)
fr_keys |>
  filter(county %in% oh$county) |>
  anti_join(enr_matched, join_by(county, precinct_yougov)) |>
  select(county, precinct_yougov, frame_name, n_frame) |>
  print(n = Inf)

oh = enr_matched |>
  transmute(state = "OH", county, precinct = precinct_yougov, precinct_enr = precinct_l2)

# ========================================
## L2 names -> frame codes
# ========================================

oh_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/oh_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  as_tibble() |>
  filter(county %in% unique(oh_frame$county)) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

# Hand-verified one-offs: the only unmatched precinct on each side in its
# municipality, ratio in the normal range.
l2_manual = tribble(
  ~county,      ~precinct_l2,                 ~precinct_yougov, # frame name; n_l2 / n_frame
  "LAKE",       "MENTOR-ON-THE-LAKE CITY DD", "43ACJ",          # MENTOR-ON-THE-LAKE CITY 4A; 1261 / 1134
  "LUCAS",      "RICHFIELD",                  "48ASK",          # RICHFIELD 1; 1200 / 1118
  "MONTGOMERY", "P/NEW LEB A",                "57AWO",          # PER/NEW LEBANON; 737 / 622
  "SUMMIT",     "AKRON 07-1",                 "77AFS"           # AKRON 7-I (1-I and 3-I have no L2 row); 1038 / 920
)

l2_keys = oh_l2 |>
  filter(!is.na(precinct_l2)) |>
  left_join(select(l2_manual, county, precinct_l2, k_manual = precinct_yougov), join_by(county, precinct_l2)) |>
  mutate(
    k_raw = precinct_l2,
    k_std = oh_std(county, precinct_l2),
    k_type = drop_type(k_std)
  )

l2_tiers = tribble(
  ~rule,        ~l2_key,    ~fr_key,
  "manual",     "k_manual", "k_id",
  "name_exact", "k_raw",    "k_raw",  # Hocking "WARD" (std_name drops WARD)
  "name_std",   "k_std",    "k_std",
  "name_type",  "k_type",   "k_type"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)
stopifnot(all(l2_manual$precinct_l2 %in% filter(l2_matched, l2_rule == "manual")$precinct_l2))

review_matches(l2_matched, fr_keys)
anti_join(l2_keys, l2_matched, join_by(county, precinct_l2)) |> select(county, precinct_l2, n_l2) |> print(n = Inf)

oh_l2 = oh_l2 |>
  left_join(select(l2_matched, county, precinct_l2, precinct_new = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  mutate(precinct = precinct_new) |>
  filter(!is.na(precinct)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = oh_frame |>
  tidylog::left_join(oh, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(oh_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(
  nrow(crosswalk) == nrow(oh_frame),
  # strictly 1:1 at the precinct level (frame rows repeat a precinct across CDs)
  crosswalk |> filter(!is.na(precinct_enr)) |> distinct(county, precinct_yougov, precinct_enr) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_enr))) |> pull(a) == 0,
  crosswalk |> filter(!is.na(precinct_l2)) |> distinct(county, precinct_yougov, precinct_l2) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_l2))) |> pull(a) == 0
)

crosswalk |>
  summarise(n = n(), enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)), both = mean(!is.na(precinct_enr) & !is.na(precinct_l2)), .by = county) |>
  print(n = Inf)

write_crosswalk("OH", crosswalk)
