source("scripts/util/crosswalks/helpers.R")

# Build the TX precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules: docs/precinct_crosswalks.md. Reference: nc.R.
#
# TX is partial: 26_general ENR exists only for some frame counties (Kaufman and
# Wood as of 2026-10-15). L2 matching covers all 32 frame counties. Rerun when
# new `TX/clean/*_latest.parquet` files land; they are picked up by the glob.
#
# Frame codes are 4-character, zero-padded county VTD codes ("0001", "1A3B",
# "003C"); frame names are identical to the codes, so name-based tiers do not
# apply. The frame also carries many 1-20 voter "junk" codes (stray voter-file
# assignments); they stay NA unless a source has the same code.

tx_frame = read_frame("TX")
tx_frame_names = frame_names("TX") |>
  semi_join(tx_frame, join_by(county, precinct_yougov))

pad4 = function(x) if_else(str_detect(x, "^[0-9A-Z]{1,4}$"), str_pad(x, 4, pad = "0"), x)

# ========================================
## ENR
# ========================================
# ENR ids are unpadded codes ("1", "37"). Wood also reports sub-precinct units
# for joint/local ballot styles ("11" plus "11 - Joint COQ"; "33", "33 - Hawk",
# "33 - Joint ESD2", "33 - Hawk ESD2"). Every unit appears in the federal and
# state contests, so the frame precinct is the *sum* of the base row and its
# sub-units. That is not 1:1, so the base id and all its sub-units stay NA.
# Galveston labels every unit with its ballot style ("137 - BS1", "142 - BS2"):
# a precinct with a single unit is 1:1 with the frame; one with several units
# (a sum) stays NA, as in Wood.
# Ellis appends a shared polling-place code with no spaces ("1001-501"); the
# base precinct ids are unique, so the suffix is dropped.

enr_files = fs::path_rel(Sys.glob(election_path("TX/clean/*_latest.parquet")), election_path())
stopifnot(length(enr_files) > 0)

tx = read_enr(enr_files) |>
  mutate(precinct = if_else(county == "ELLIS", str_remove(precinct, "-\\d+$"), precinct)) |>
  mutate(base = coalesce(str_match(precinct, "^(\\S+) - .+$")[, 2], precinct)) |>
  mutate(split = n() > 1, .by = c(county, base)) |>
  mutate(precinct = if_else(split, NA_character_, pad4(base))) |>
  select(-base, -split)

stopifnot(!anyDuplicated(filter(tx, !is.na(precinct))[c("county", "precinct")]))

enr_leftover = tx |>
  filter(!is.na(precinct)) |>
  anti_join(tx_frame, join_by(county, precinct == precinct_yougov))
if (nrow(enr_leftover)) print(enr_leftover, n = Inf)

tx = filter(tx, !is.na(precinct))

# ========================================
## L2 codes -> frame codes
# ========================================
# L2 carries the same VTD codes, mostly unpadded ("01", "101", "1C"). Wood L2
# writes them as "commissioner-precinct" ("1-1" -> frame "0011").

tx_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/tx_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  as_tibble() |>
  filter(county %in% unique(tx_frame$county)) |>
  select(state, county, precinct, n_l2 = n) |>
  mutate(precinct_l2 = precinct) |>
  arrange(county, precinct)

county_codes = function(county, x) {
  out = rep(NA_character_, length(x))
  w = county == "WOOD" & str_detect(x, "^\\d-\\d$")
  out[w] = str_pad(str_remove(x[w], "-"), 4, pad = "0")
  out
}

# Hand-verified one-offs: only candidate in the county, ratio in the normal range.
l2_manual = tribble(
  ~county,     ~precinct_l2, ~precinct_yougov, # note
  "GALVESTON", "232",        "232.3",          # frame has no 0232; 418 L2 vs 438 frame voters
)

fr_keys = tx_frame_names |>
  mutate(k_id = precinct_yougov)

l2_keys = tx_l2 |>
  filter(!is.na(precinct_l2)) |>
  left_join(select(l2_manual, county, precinct_l2, k_manual = precinct_yougov), join_by(county, precinct_l2)) |>
  mutate(
    k_raw = precinct_l2,
    k_pad = if_else(str_detect(precinct_l2, "^[0-9A-Z]{1,4}$"), str_pad(precinct_l2, 4, pad = "0"), NA),
    k_county = county_codes(county, precinct_l2)
  )

l2_tiers = tribble(
  ~rule,         ~l2_key,    ~fr_key,
  "manual",      "k_manual", "k_id",
  "code_exact",  "k_raw",    "k_id",
  "code_pad",    "k_pad",    "k_id",
  "county_rule", "k_county", "k_id"
)

l2_matched = match_tiers(l2_keys, fr_keys, l2_tiers)
stopifnot(all(l2_manual$precinct_l2 %in% filter(l2_matched, l2_rule == "manual")$precinct_l2))

# Voter-count guard. Several counties re-precincted between the frame and L2
# vintages but kept some codes (El Paso 0054 split into 0228-0236; Bell 0203 and
# 0210 lost voters to new 0217 and 0322; Victoria 0026+0027 merged into L2 27;
# Travis has ~25 new codes on each side). A same-code pair whose L2/frame ratio
# falls outside 0.75-1.33 (and differs by more than 25 voters) is treated as a
# different area and left NA. Exempt: manual rows, and Lamar, where L2 leaves
# 27% of voters without a precinct (ratios 0.4-1.0) but the code sets match
# exactly on both sides with no leftovers.
l2_dropped = l2_matched |>
  filter(
    l2_rule != "manual", county != "LAMAR",
    !between(n_l2 / n_frame, .75, 1.33), abs(n_l2 - n_frame) > 25
  )
l2_matched = anti_join(l2_matched, l2_dropped, join_by(county, precinct_l2))
print(select(l2_dropped, county, precinct_l2, precinct_yougov, n_l2, n_frame), n = Inf)

review_matches(l2_matched, filter(fr_keys, n_frame >= 20))

tx_l2 = tx_l2 |>
  left_join(select(l2_matched, county, precinct_l2, precinct_new = precinct_yougov, l2_rule), join_by(county, precinct_l2)) |>
  mutate(precinct = precinct_new) |>
  filter(!is.na(precinct)) |>
  select(state, county, precinct, precinct_l2, l2_rule)

crosswalk = tx_frame |>
  tidylog::left_join(tx, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(tx_l2, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(
  nrow(crosswalk) == nrow(tx_frame),
  # strictly 1:1 at the precinct level (frame rows repeat a precinct across CDs)
  crosswalk |> filter(!is.na(precinct_enr)) |> distinct(county, precinct_yougov, precinct_enr) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_enr))) |> pull(a) == 0,
  crosswalk |> filter(!is.na(precinct_l2)) |> distinct(county, precinct_yougov, precinct_l2) |>
    summarise(a = anyDuplicated(pick(county, precinct_yougov)) + anyDuplicated(pick(county, precinct_l2))) |> pull(a) == 0
)

write_crosswalk("TX", crosswalk)
