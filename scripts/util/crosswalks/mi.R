source("scripts/util/crosswalks/helpers.R")

# Build the MI precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference state: nc.R.
#
# MI specifics:
# - Frame `precinct26_id` is the precinct number *within a municipality*
#   ("001"), so it is not unique within county. Since 2026-10-15 the frame is
#   aggregated on precinct26_code (`MI_KENT_ADA TOWNSHIP_001`), so MI keys on
#   that: `precinct_yougov` in mi.csv is precinct26_code, not precinct26_id.
#   (Before, the frame pooled every municipality's "001" in a county x cd into
#   one row.) Any frame row that still holds more than one voter-file precinct
#   name stays NA by design.
# - All three sources carry names, not codes:
#     frame `KENT - WALKER CITY - WD 01 PCT 001`, `KENT - ADA TOWNSHIP - 001`
#     L2    `WALKER CITY 1`, `ADA TWP 1`, `INKSTER CITY 1-2`, `CHASE TWP (1)`
#     ENR   `Walker City, Ward 1, Precinct 1`, `AUSABLE TWP 1`, `City of Evart`
#   so ENR and L2 are both matched to the frame names with the same tiers on a
#   parsed (municipality, ward, precinct) key.
# - ENR covers frame counties Kent, Osceola, Roscommon only. St. Clair and
#   St. Joseph files exist but are not frame counties.

st = "MI"
mi_frame = read_frame(st, id = "precinct26_code")

root = election_path()
enr_files = fs::path_rel(Sys.glob(fs::path(root, "MI/clean/*_latest.parquet")), root)
mi_enr = read_enr(enr_files)

l2_file = fs::path_rel(Sys.glob(election_path("input_data/l2/mi_*_precincts.parquet")), root)
stopifnot(length(l2_file) == 1)
mi_l2 = enightdata::read_local(l2_file, parquet = TRUE) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  filter(county %in% mi_frame$county) |>
  arrange(county, precinct)

# Voter-file names by frame row. frame_names() drops cd, which MI needs because
# the frame rows are keyed on cd.
mi_vf = open_dataset(election_path(paste0("input_data/vf_frame_current_202609/state=", st))) |>
  count(county = county_name, cd, precinct_yougov = precinct26_code, frame_name = precinct26_name, name = "n_frame") |>
  collect() |>
  filter(!is.na(precinct_yougov), county %in% mi_frame$county)

# ========================================
## Name parsing
# ========================================

muni_std = function(x) {
  x |>
    str_to_upper() |>
    str_replace("^CITY OF (.*)$", "\\1 CITY") |>  # ENR "City of Evart"
    str_replace_all("\\bTWP\\b", "TOWNSHIP") |>
    str_remove_all("\\bCHARTER\\b") |>            # ENR/frame "Caledonia Charter Township" vs "CALEDONIA TOWNSHIP"
    str_replace_all("[.'-]", " ") |>
    str_squish() |>
    str_replace("\\bCITY CITY$", "CITY")        # frame "TRAVERSE CITY CITY" vs L2 "TRAVERSE CITY 08"
}
pct_std = function(x) str_remove_all(str_remove(x, "^0+(?=\\d)"), " ")   # "003N" -> "3N", "2 C" -> "2C"

# "MUNI CITY 12", "MUNI TWP", "MUNI CITY 1-2", "MUNI TWP (7)"
parse_l2 = function(x) {
  m = str_match(x, "^(.*(?:TWP|CITY|TOWNSHIP|VILLAGE))(?: (?:(\\d+)-(\\d+[A-Z]?)|(\\d+[A-Z]?)|\\((\\d+)\\)))?$")
  tibble(muni = muni_std(m[, 2]), ward = pct_std(m[, 3]), pct = pct_std(coalesce(m[, 4], m[, 5], m[, 6])))
}
# "Muni Township, Ward 1, Precinct 2", "Muni Township, Precinct 2 C", "City of Evart",
# or L2-style "AUSABLE TWP 1" (Roscommon)
parse_enr = function(x) {
  m = str_match(x, "^([^,]+)(?:, Ward (\\d+))?(?:, Precinct (\\d+(?: ?[A-Z])?))?$")
  out = tibble(muni = muni_std(m[, 2]), ward = pct_std(m[, 3]), pct = pct_std(m[, 4]))
  l2_style = str_detect(x, "^[A-Z .'-]+ (TWP|CITY) \\d+$")
  out[l2_style, ] = parse_l2(x[l2_style])
  out
}
# "KENT - WALKER CITY - WD 01 PCT 001", "KENT - ADA TOWNSHIP - 001"
parse_frame = function(x) {
  m = str_match(x, "^[^-]+ - (.*) - (?:WD (\\d+) PCT )?(\\d+[A-Z]?)$")
  tibble(muni = muni_std(m[, 2]), ward = pct_std(m[, 3]), pct = pct_std(m[, 4]))
}

# Keys: muni|ward|pct, muni|pct, and muni alone for municipalities with a single
# precinct in the county on that side (L2 "BOWNE TWP" == frame "BOWNE TOWNSHIP -
# 001"; Leelanau L2 "TRAVERSE CITY" == frame "TRAVERSE CITY CITY - 004", the only
# Traverse City precinct in that county).
add_keys = function(d) {
  d |>
    mutate(n_muni = n(), .by = c(county, muni)) |>
    mutate(
      k_wp = if_else(is.na(ward) | is.na(pct), NA, paste(muni, ward, pct, sep = "|")),
      k_p = if_else(is.na(pct), NA, paste(muni, pct, sep = "|")),
      k_m1 = if_else(n_muni == 1, muni, NA),
      across(c(k_wp, k_p, k_m1), \(k) str_remove_all(k, " "), .names = "{.col}_ns")
    )
}

# Frame side: one row per (county, cd, code, name). Rows of a pooled frame unit
# get a placeholder id, so their names still block ambiguous keys and claim
# their L2/ENR counterparts, but never produce a match. A name split across two
# cds appears twice and is dropped by the uniqueness rule.
fr_keys = mi_vf |>
  mutate(k_names = n(), n_row = sum(n_frame), .by = c(county, cd, precinct_yougov)) |>
  mutate(
    fid = if_else(k_names == 1, paste(cd, precinct_yougov, sep = "|"), paste("POOLED", cd, precinct_yougov, frame_name, sep = "|")),
    n_frame = n_row
  ) |>
  select(county, precinct_yougov = fid, frame_name, n_frame) |>
  mutate(parse_frame(frame_name)) |>
  add_keys()
stopifnot(!anyNA(fr_keys$muni), !anyDuplicated(fr_keys[c("county", "precinct_yougov")]))

tiers = tribble(
  ~rule,           ~l2_key,    ~fr_key,
  "ward_pct",      "k_wp",     "k_wp",
  "muni_pct",      "k_p",      "k_p",
  "muni_single",   "k_m1",     "k_m1",
  "ward_pct_ns",   "k_wp_ns",  "k_wp_ns",
  "muni_pct_ns",   "k_p_ns",   "k_p_ns",
  "muni_single_ns","k_m1_ns",  "k_m1_ns"
)

finish = function(matched) {
  matched |>
    filter(!str_starts(precinct_yougov, "POOLED")) |>
    separate_wider_delim(precinct_yougov, "|", names = c("cd", "precinct_yougov"))
}

# ========================================
## ENR names -> frame rows
# ========================================

enr_keys = mi_enr |>
  filter(county %in% mi_frame$county) |>
  transmute(county, precinct_l2 = precinct_enr, n_l2 = NA_real_) |>
  mutate(parse_enr(precinct_l2)) |>
  filter(!is.na(muni)) |>
  add_keys()

enr_matched = match_tiers(enr_keys, fr_keys, tiers)
enr_matched |> count(county, l2_rule) |> print()
enr_matched |> filter(!str_starts(precinct_yougov, "POOLED")) |> select(county, precinct_l2, precinct_yougov, frame_name, l2_rule) |> print(n = Inf)

mi_enr_x = finish(enr_matched) |> select(county, cd, precinct_yougov, precinct_enr = precinct_l2)

# ========================================
## L2 names -> frame rows
# ========================================

l2_keys = mi_l2 |>
  filter(!is.na(precinct_l2)) |>
  select(county, precinct_l2, n_l2) |>
  mutate(parse_l2(precinct_l2)) |>
  add_keys()
l2_keys = filter(l2_keys, !is.na(muni)) # drops only Oakland "FENTON" (16 voters, no type word)

l2_matched = match_tiers(l2_keys, fr_keys, tiers)
review_matches(filter(l2_matched, !str_starts(precinct_yougov, "POOLED")), filter(fr_keys, !str_starts(precinct_yougov, "POOLED")))

# Fuzzy rows for eyeballing
l2_matched |>
  filter(!l2_rule %in% c("ward_pct", "muni_pct"), !str_starts(precinct_yougov, "POOLED")) |>
  select(county, precinct_l2, frame_name, l2_rule, n_l2, n_frame) |>
  print(n = Inf)

mi_l2_x = finish(l2_matched) |> select(county, cd, precinct_yougov, precinct_l2, l2_rule)

# ========================================
## Crosswalk
# ========================================

crosswalk = mi_frame |>
  tidylog::left_join(mi_enr_x, join_by(county, cd, precinct_yougov), na_matches = "never") |>
  tidylog::left_join(mi_l2_x, join_by(county, cd, precinct_yougov), na_matches = "never")

cw = as_tibble(crosswalk)
stopifnot(
  nrow(crosswalk) == nrow(mi_frame),
  !anyDuplicated(cw[c("county", "cd", "precinct_yougov")]),
  !anyDuplicated(filter(cw, !is.na(precinct_enr))[c("county", "precinct_enr")]),
  !anyDuplicated(filter(cw, !is.na(precinct_l2))[c("county", "precinct_l2")])
)

write_crosswalk(st, crosswalk)
