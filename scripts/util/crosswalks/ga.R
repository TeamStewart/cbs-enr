source("scripts/util/crosswalks/helpers.R")

# Build the GA precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md; reference: nc.R.
#
# ENR geo carries the Enhanced Voting precinct `id` (scrape_ga), which is the
# official code in most counties and matches frame codes directly (~91.5%).
# Frame "names" are the codes themselves, so names cannot validate matches;
# the L2 voter counts do instead.

ga_frame = read_frame("GA")
ga_frame_names = frame_names("GA")

# Drop zero-padding at the start and after letters: "0002" -> "2", "MO01" -> "MO1".
num_key = function(x) str_replace_all(str_to_upper(str_squish(x)), "(^|(?<=[A-Z]))0+(?=\\d)", "")
lead_key = function(x) num_key(str_match(x, "^\\s*(\\d+)\\b")[, 2]) # "25 LOCUST GROVE" -> "25"

# ========================================
## ENR ids -> frame codes
# ========================================
# Direct code join first; the leftovers go through ordered tiers (unique keys
# within county, strict 1:1): case (Fulton `04i`, Dodge `Chester`), standardized
# name (Troup `01-Broad St`, Spalding `Precinct 1`), zero-padding (Burke `2` =
# `0002`, Clayton `MO1` = `MO01`), and the frame's leading number (Henry `25` =
# `25 LOCUST GROVE`, White `011` = `011 - BLUE CREEK`).

ga_enr_raw = read_enr("GA/clean/GA_latest.parquet")

fr_codes = ga_frame_names |>
  mutate(k_raw = precinct_yougov, k_upper = str_to_upper(precinct_yougov), k_std = std_name(precinct_yougov),
         k_num = num_key(precinct_yougov), k_lead = lead_key(precinct_yougov))

enr_keys = ga_enr_raw |>
  transmute(county, precinct_l2 = precinct_enr, n_l2 = NA_integer_,
            k_raw = precinct_enr, k_upper = str_to_upper(precinct_enr), k_std = std_name(precinct_enr),
            k_num = num_key(precinct_enr))

enr_tiers = tribble(
  ~rule,       ~l2_key,   ~fr_key,
  "exact",     "k_raw",   "k_raw",
  "upper",     "k_upper", "k_upper",
  "std",       "k_std",   "k_std",
  "num",       "k_num",   "k_num",
  "lead",      "k_num",   "k_lead"
)

# Burke renumbered its precincts for 2026 (ENR adds 1, 16, 17 and drops 9, 14),
# so equal numbers need not be the same precinct. Kept NA.
enr_skip_counties = "BURKE"

enr_matched = match_tiers(filter(enr_keys, !county %in% enr_skip_counties), fr_codes, enr_tiers) |>
  rename(precinct_enr = precinct_l2, enr_rule = l2_rule)
count(enr_matched, enr_rule) |> print()
enr_matched |> filter(enr_rule != "exact") |> select(county, precinct_enr, precinct_yougov, enr_rule) |> print(n = Inf)

ga_enr = enr_matched |> distinct(county, precinct = precinct_yougov, precinct_enr) |> mutate(state = "GA")

# EV precinct labels ("Acworth 1A" for id AC1A) from the raw file of the same
# scrape as GA_latest. They bridge L2 names to frame codes in counties where L2
# has names and the frame has codes (Cobb, Clayton, Catoosa, ...).
enr_stamp = Sys.glob(election_path("GA/clean/GA_2*.parquet")) |> fs::path_file() |> fs::path_ext_remove() |> max()
stopifnot(setequal(
  unique(as.character(enightdata::read_local(paste0("GA/clean/", enr_stamp, ".parquet"), parquet = TRUE)$geo)),
  unique(as.character(enightdata::read_local("GA/clean/GA_latest.parquet", parquet = TRUE)$geo))
))
ga_enr_names = jsonlite::read_json(election_path("GA/raw", paste0(enr_stamp, ".json")))$localResults |>
  map(\(loc) map(loc$ballotItems, \(b) map(b$ballotOptions, \(o) map(o$precinctResults, \(p) c(id = p$id, name = p$name)))) |>
    unlist(recursive = FALSE) |> unlist(recursive = FALSE) |> bind_rows() |>
    mutate(county = str_to_upper(str_remove(loc$name, " County$")))) |>
  list_rbind() |>
  distinct(county, precinct_enr = id, enr_name = name)
stopifnot(!anyDuplicated(ga_enr_names[c("county", "precinct_enr")]))

fr_codes = fr_codes |>
  left_join(ga_enr, join_by(county, precinct_yougov == precinct)) |>
  left_join(ga_enr_names, join_by(county, precinct_enr)) |>
  mutate(
    k_enrname = std_name(enr_name),
    k_sfx = num_key(str_match(precinct_yougov, "^[A-Z]+(\\d+)$")[, 2]),  # Douglas DC01, Grady G01 -> "1"
    k_pnum = str_match(precinct_yougov, "^PRECINCT (\\d+)$")[, 2],      # Spalding
    k_enrnospace = str_remove_all(k_enrname, " "),                         # Clayton LOVE JOY = Lovejoy
    k_enrname1 = str_remove(k_enrname, " 1$"),                             # Cobb ADDISON = Addison 01
    k_numtrim = num_key(str_match(precinct_yougov, "^(\\d+)[A-Z]$")[, 2])  # Paulding 0001B = 01
  ) |>
  select(-state, -precinct_enr, -enr_name)

# ========================================
## L2 names -> frame codes
# ========================================
# L2 mixes codes ("1B"), "NAME (CODE)" (DeKalb `ALLGOOD ELEM (AA)`, Floyd
# `ALTO PARK (001)`, Henry `AUSTIN ROAD (44)`), "CODE-NAME" (Coweta
# `01-SENOIA`, Douglas `01-GOLDEN METHODIST CHURCH`, Houston `ANNX-HOUSTON CNTY
# ANNEX`) and bare names that equal the EV labels (Cobb `ACWORTH 1A`).

ga_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/ga_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

l2_keys = ga_l2 |>
  filter(!is.na(precinct_l2)) |>
  mutate(
    k_raw = precinct_l2, k_upper = str_to_upper(precinct_l2), k_std = std_name(precinct_l2),
    k_num = num_key(precinct_l2),
    k_paren = num_key(str_match(precinct_l2, "\\(([^)]+)\\)\\s*$")[, 2]),
    k_lead = lead_key(precinct_l2),
    k_head = str_to_upper(str_match(precinct_l2, "^([A-Za-z0-9]+)\\s*-")[, 2]),
    k_nospace = str_remove_all(k_std, " ")
  )

l2_tiers = tribble(
  ~rule,        ~l2_key,   ~fr_key,
  "code_exact", "k_raw",   "k_raw",
  "upper",      "k_upper", "k_upper",
  "paren_code", "k_paren", "k_num",
  "std",        "k_std",   "k_std",
  "num",        "k_num",   "k_num",
  "lead_code",  "k_lead",  "k_num",
  "lead_lead",  "k_lead",  "k_lead",
  "paren_lead", "k_paren", "k_lead",
  "head_code",  "k_head",  "k_upper",
  "enr_name",   "k_std",   "k_enrname",
  "lead_sfx",   "k_lead",  "k_sfx",
  "lead_pnum",  "k_lead",  "k_pnum",
  "enr_nospace", "k_nospace", "k_enrnospace",
  "enr_name1",  "k_std",   "k_enrname1",
  "num_trim",   "k_num",   "k_numtrim"
)

l2_matched = match_tiers(l2_keys, fr_codes, l2_tiers)
review_matches(l2_matched, fr_codes)
l2_matched |> filter(!l2_rule %in% c("code_exact", "upper", "paren_code", "std", "num")) |>
  select(county, precinct_l2, precinct_yougov, l2_rule, n_l2, n_frame) |> print(n = Inf)

# L2 (2026-10-06) merged these frame precincts with a neighbour that ENR still
# reports separately; the L2 count is about the sum of both. L2 stays NA (rule 3).
l2_merged = tribble(
  ~county,     ~precinct, # L2 precinct = frame members
  "DOUGHERTY", "02",      # 02 (5,074) = 01 + 02 (3,022 + 2,248); L2 has no 01
  "COBB",      "WG02"     # WADE GREEN 2 (4,457) = WG01 + WG02 (2,464 + 2,005)
)

ga_l2 = ga_l2 |>
  select(-precinct) |>
  inner_join(select(l2_matched, county, precinct_l2, precinct = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  select(state, county, precinct, precinct_l2, l2_rule) |>
  anti_join(l2_merged, join_by(county, precinct))

crosswalk = ga_frame |>
  tidylog::left_join(ga_enr, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(ga_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(nrow(crosswalk) == nrow(ga_frame))

crosswalk |> filter(is.na(precinct_enr)) |> count(county, sort = TRUE) |> print(n = Inf)
anti_join(ga_enr_raw, ga_enr, join_by(county, precinct_enr)) |> select(county, precinct_enr) |> print(n = Inf)
crosswalk |> summarise(n = n(), enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)), both = mean(!is.na(precinct_enr) & !is.na(precinct_l2))) |> print()

write_crosswalk("GA", crosswalk)
