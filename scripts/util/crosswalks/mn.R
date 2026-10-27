source("scripts/util/crosswalks/helpers.R")

# Build the MN precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference: nc.R.
#
# ENR: statewide SOS feed (allracesbyprecinct.txt), `MN/clean/MN_latest.parquet`.
# geo is `mn_{county}_{pppp}`, where the county number is the SOS county id
# (= cbs_counties county_cbs, alphabetical 1-87, not FIPS) and pppp is the SOS
# 4-digit precinct code, the same code as the frame's precinct26_id. The feed
# has no precinct names, so each code join is checked against the SOS precinct
# table for the same election (PrctTbl.txt: county id; code; name; CD; ...),
# which is cached in MN/raw/.
#
# L2 has names only ("MOUND P-1", "ROCHESTER W1 P01"), matched to the frame
# names (which are the SOS names) in tiers.

mn_frame = read_frame("MN")
mn_frame_names = frame_names("MN") |>
  semi_join(mn_frame, join_by(county, precinct_yougov))
stopifnot(
  !anyDuplicated(mn_frame_names[c("county", "precinct_yougov")]),
  nrow(anti_join(distinct(mn_frame, county, precinct_yougov), mn_frame_names, join_by(county, precinct_yougov))) == 0
)

# ========================================
## Name standardization (MN-specific, on top of std_name)
# ========================================

mn_std = function(x) {
  std_name(x) |>
    str_replace_all("\\bTOWNSHIP\\b", "TWP") |>
    str_replace_all("\\bVILLAGE\\b", "VLG") |>
    str_replace_all("\\b(PREC|PRCT)\\b", "PRECINCT") |>
    str_replace_all("\\bUNORGANIZED\\b", "UNORG") |>
    str_replace_all("\\b([PW])0+(?=\\d)", "\\1") |> # Olmsted "ROCHESTER W1 P01" = "W1 P1"
    str_squish()
}
# Second key: drop CITY / CITY OF and a bare trailing PRECINCT, which the
# sources add or leave out ("AITKIN" vs "AITKIN CITY", "CITY OF COURTLAND" vs
# "COURTLAND CITY", "PONEMAH" vs "PONEMAH PCT"). Only used when unique on both
# sides, so "AITKIN" (city) and "AITKIN TWP" stay distinct.
drop_type = function(x) x |> str_remove_all("\\bCITY( OF)?\\b") |> str_remove("\\bPRECINCT$") |> str_squish()

# SOS names (frame and precinct table) get one more rewrite: Crow Wing
# "PRECINCT 3 - UNORG. TERR." = L2 "UNORGANIZED 3".
sos_std = function(x) str_replace(mn_std(x), "^PRECINCT (\\d+) UNORG TERR$", "UNORG \\1")

fr_keys = mn_frame_names |>
  mutate(
    k_raw = frame_name, k_id = precinct_yougov,
    k_std = sos_std(frame_name),
    k_type = drop_type(k_std), k_bag = bag(k_type), k_nospace = str_remove_all(k_type, " ")
  )

# ========================================
## ENR codes -> frame codes
# ========================================

pt_file = election_path("MN/raw/PrctTbl_20261103.txt")
if (!fs::file_exists(pt_file)) {
  download.file("https://electionresultsfiles.sos.mn.gov/20261103/PrctTbl.txt", pt_file, quiet = TRUE)
}
mn_pt = read_delim(pt_file, delim = ";", col_names = FALSE, col_types = cols(.default = "c")) |>
  transmute(county_cbs = as.integer(X1), precinct = X2, pt_name = X3, pt_cd = paste0("MN-0", X4)) |>
  left_join(filter(countyfips, state == "MN"), join_by(county_cbs)) |>
  select(county, precinct, pt_name, pt_cd)
stopifnot(!anyNA(mn_pt$county), !anyDuplicated(mn_pt[c("county", "precinct")]))

mn = read_enr("MN/clean/MN_latest.parquet")
stopifnot(!anyNA(mn$county), !anyDuplicated(mn[c("county", "precinct")]))

# ENR codes and the precinct table must be the same set.
stopifnot(
  nrow(anti_join(mn, mn_pt, join_by(county, precinct))) == 0,
  nrow(anti_join(mn_pt, mn, join_by(county, precinct))) == 0
)

# Keep a code join only if the SOS name for that code is the frame name
# (ignoring CITY / TWP spelling: Rice 0065 MORRISTOWN CITY = MORRISTOWN).
enr_check = mn |>
  left_join(mn_pt, join_by(county, precinct)) |>
  inner_join(select(fr_keys, county, precinct_yougov, frame_name, k_type), join_by(county, precinct == precinct_yougov)) |>
  mutate(name_ok = drop_type(sos_std(pt_name)) == k_type)
print(filter(enr_check, !name_ok))

# ENR precincts with no frame row (25 on 2026-10-23). All have no L2 precinct
# either (checked below), i.e. no registered voters in either voter file, so the
# frame precinct next to them (e.g. Becker DETROIT TWP P-1 next to new P-2) is
# kept as a 1:1 code match. Recheck on real results: ballots there should be ~0.
enr_new = anti_join(mn, fr_keys, join_by(county, precinct == precinct_yougov)) |>
  left_join(mn_pt, join_by(county, precinct))
print(enr_new, n = Inf)

mn = enr_check |>
  filter(name_ok) |>
  transmute(state, county, precinct, precinct_enr)

# ========================================
## L2 names -> frame codes
# ========================================

mn_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/mn_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  as_tibble() |>
  filter(county %in% unique(mn_frame$county)) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

# Hand-verified one-offs: the only unmatched candidate on each side in the
# municipality, ratio in the normal range.
l2_manual = tribble(
  ~county,     ~precinct_l2,          ~precinct_yougov, # frame name; n_l2 / n_frame
  "BECKER",    "DETROIT TWP",         "0050",           # DETROIT TWP P-1 (new P-2 0052 has no voters); 1425 / 1444
  "CASS",      "LIMA TWP",            "0150",           # LIMA UNORG; 69 / 70
  "CROW WING", "CROSBY PCT 1",        "0065",           # CROSBY CITY; 1399 / 1434
  "CROW WING", "CROSSLAKE PCT 1",     "0075",           # CROSSLAKE CITY; 2215 / 2245
  "ITASCA",    "TACONITE P-1",        "0339",           # TACONITE; 460 / 466
  "PIPESTONE", "FOUNTAIN PRAIRIE",    "0035",           # FOUNTAIN PRAIRIE TWP.; 140 / 142
  "ST LOUIS",  "GREAT SCOTT TWP P-1", "0575",           # GREAT SCOTT TWP.; 292 / 301
  "ST LOUIS",  "RICE LAKE TWP",       "0910"            # RICE LAKE (city since 2015); 3116 / 3133
)

l2_keys = mn_l2 |>
  filter(!is.na(precinct_l2)) |>
  left_join(select(l2_manual, county, precinct_l2, k_manual = precinct_yougov), join_by(county, precinct_l2)) |>
  mutate(
    k_raw = precinct_l2,
    k_std = mn_std(precinct_l2),
    k_type = drop_type(k_std), k_bag = bag(k_type), k_nospace = str_remove_all(k_type, " ")
  )

l2_tiers = tribble(
  ~rule,        ~l2_key,    ~fr_key,
  "manual",     "k_manual", "k_id",
  "name_exact", "k_raw",    "k_raw",
  "name_std",   "k_std",    "k_std",
  "name_type",  "k_type",   "k_type",
  "name_bag",   "k_bag",    "k_bag",   # Isanti "CAMBRIDGE CITY P1 WEST" = "CAMBRIDGE WEST - P1"
  "name_nospace", "k_nospace", "k_nospace" # "ST CLOUD W-2 P-1" = "ST CLOUD W2 P1", "LA PORTE" = "LAPORTE"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)
stopifnot(all(l2_manual$precinct_l2 %in% filter(l2_matched, l2_rule == "manual")$precinct_l2))

review_matches(l2_matched, fr_keys)
l2_matched |>
  filter(!l2_rule %in% c("name_exact", "name_std")) |>
  select(county, precinct_l2, precinct_yougov, frame_name, l2_rule, n_l2, n_frame) |>
  print(n = Inf)
anti_join(l2_keys, l2_matched, join_by(county, precinct_l2)) |> select(county, precinct_l2, n_l2) |> print(n = Inf)

# The ENR-only precincts have no L2 precinct of the same name.
stopifnot(nrow(semi_join(l2_keys, mutate(enr_new, k_nospace = str_remove_all(drop_type(sos_std(pt_name)), " ")), join_by(county, k_nospace))) == 0)

mn_l2 = mn_l2 |>
  left_join(select(l2_matched, county, precinct_l2, precinct_new = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  mutate(precinct = precinct_new) |>
  filter(!is.na(precinct)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = mn_frame |>
  tidylog::left_join(mn, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(mn_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(
  nrow(crosswalk) == nrow(mn_frame),
  !anyDuplicated(crosswalk[c("county", "precinct_yougov")]),
  !anyDuplicated(filter(crosswalk, !is.na(precinct_enr))[c("county", "precinct_enr")]),
  !anyDuplicated(filter(crosswalk, !is.na(precinct_l2))[c("county", "precinct_l2")])
)

crosswalk |>
  summarise(n = n(), enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)), both = mean(!is.na(precinct_enr) & !is.na(precinct_l2))) |>
  print()

write_crosswalk("MN", crosswalk)
