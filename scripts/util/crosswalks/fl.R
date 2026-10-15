source("scripts/util/crosswalks/helpers.R")

# Build the FL precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference state: nc.R.
#
# FL ENR comes as per-county files (`FL_{County}_latest.parquet`); as of
# 2026-10-15 only Hillsborough and Miami-Dade have 26_general data. Rerunning
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
  select(-md_base)
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
