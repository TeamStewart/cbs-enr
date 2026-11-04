source("scripts/util/crosswalks/helpers.R")

# Indiana ENR -> L2 pairs that l2_direct.R's generic tiers miss. IN is not in the
# YouGov frame, so its only L2 map is input_data/crosswalks/l2_direct/in.csv. This
# script writes scripts/util/crosswalks/l2_manual_in.csv, which l2_direct.R reads
# (with the other l2_manual_*.csv files) before its tiers. Build order:
#
#   Rscript scripts/util/crosswalks/l2_direct.R IN   # generic tiers (first build only)
#   Rscript scripts/util/crosswalks/in_l2.R          # IN rules, on what is left
#   Rscript scripts/util/crosswalks/l2_direct.R IN   # final in.csv
#
# "Left" = ENR ids and L2 precincts without a non-manual row in in.csv. Every rule
# is strict 1:1 within county: a key must be unique on both sides. ENR ids ending
# "Nv" / "(Nv)" (no voters) have no L2 precinct and stay NA.
#
# Rules, tried in order (2026-11-03, L2 in_20261013, ENR IN_latest + IN_Porter_latest):
# - in_bag: normalized name, word order ignored. Directions and ordinals are
#   abbreviated ("Whitley Cleveland Northeast" = "CLEVELAND NE", Jasper "Keener
#   Five" = "KEENER 5", Martin "North Center" = "CENTER N"); PRECINCT, TOWNSHIP and
#   TWP dropped; Howard's leading L2 "(CLAY)" dropped ("1 Clay 1" = "(CLAY) 1 CLAY 1").
# - in_single: a township with one precinct, numbered on one side only (Grant
#   "Green 1" = "GREEN", Franklin "Springfield" = "SPRINGFIELD 1"). Both sides must
#   have exactly one precinct with that stem, counting matched ones.
# - in_ward: "Ward 01 Precinct 02" = "<CITY> 1-02" (Vanderburgh EVANSVILLE,
#   Kosciusko WARSAW).
# - in_ward_dir: city ward + direction with the city left out (Shelby "1 East" =
#   "ADDISON 1-E", Putnam "Second East" = "GREENCASTLE 2-E").
# - in_prefix: abbreviated township prefixes (Marion "Cn001" = "CENTER 001", Lake
#   "Cct 01" = "CEDAR CREEK 1", Hancock "Bc-02 N" = "BUCK CREEK 2-N", LaPorte
#   "Lp 25" = "LAPORTE CITY 25", Jennings "Nv-1a" = "NORTH VERNON-1A"). Prefix ->
#   L2 stem when the first letters agree and the precinct numbers under the two
#   overlap by >= 80% (of the larger group), mutual best only. The map is printed.
# - in_num: Delaware "Precinct 57" = "WASHINGTON 57" (L2 numbers are unique
#   countywide; conditions at the rule).
# Left NA: Porter ids from the statewide feed ("Bne 1"), because the Porter county
# feed's ids ("BOONE 1") already hold those L2 precincts.

st = "IN"
OUT = "scripts/util/crosswalks/l2_manual_in.csv"
ckey = function(x) county_key(st, str_replace_all(x, "&", "AND"))

files = Sys.glob(election_path(st, "clean", "*_latest.parquet")) |> fs::path_rel(election_path())
enr_all = read_enr(files) |> transmute(county_enr = county, county = ckey(county), precinct_enr)
l2_file = max(Sys.glob(election_path("input_data/l2", "in_*_precincts.parquet")))
l2_all = nanoparquet::read_parquet(l2_file, col_select = c("county", "precinct", "n")) |>
  as_tibble() |>
  filter(!is.na(precinct), precinct != "", n > 0) |>
  transmute(county = ckey(county), precinct_l2 = precinct, n)

taken = read_csv(election_path("input_data/crosswalks/l2_direct/in.csv"), col_types = cols(.default = "c")) |>
  filter(!str_starts(l2_rule, "manual_")) |>
  mutate(county = ckey(county))

DIRS = c("NORTHEAST" = "NE", "NORTHWEST" = "NW", "SOUTHEAST" = "SE", "SOUTHWEST" = "SW",
         "NORTH" = "N", "SOUTH" = "S", "EAST" = "E", "WEST" = "W", "CENTRAL" = "C")
NUMS = c("ONE" = "1", "TWO" = "2", "THREE" = "3", "FOUR" = "4", "FIVE" = "5", "SIX" = "6",
         "SEVEN" = "7", "EIGHT" = "8", "NINE" = "9", "TEN" = "10",
         "FIRST" = "1", "SECOND" = "2", "THIRD" = "3", "FOURTH" = "4", "FIFTH" = "5", "SIXTH" = "6")
words = function(x, map) {
  for (k in names(map)) x = str_replace_all(x, paste0("\\b", k, "\\b"), map[[k]])
  x
}
in_norm = function(x) {
  x = str_to_upper(x) |>
    str_remove("^\\([A-Z]+\\)\\s*") |>                                    # Howard "(CLAY) "
    str_replace_all("[-/,.()]", " ") |>
    str_replace_all("(?<=\\d)(?=[A-Z])|(?<=[A-Z])(?=\\d)", " ") |>        # "CN001" -> "CN 001"
    str_replace_all("\\b(PRECINCT|PCT|TOWNSHIP|TWP)\\b", " ") |>
    words(DIRS) |>
    words(NUMS) |>
    str_replace_all("\\b0+(?=\\d)", "") |>
    str_squish()
  na_if(x, "")
}
no_voters = function(x) str_detect(x, "(?i)\\bnv\\)?$")
stem = function(x) str_squish(str_remove(x, "(^| )\\d.*$"))  # alpha words before the first number

prep = function(d, id) {
  d |> mutate(nm = in_norm(.data[[id]]), st_ = stem(nm)) |>
    mutate(n_stem = n(), .by = c(county, st_))
}
enr_p = prep(enr_all, "precinct_enr")
l2_p = prep(l2_all, "precinct_l2")
e = enr_p |> anti_join(taken, join_by(county, precinct_enr)) |> filter(!no_voters(precinct_enr))
l = l2_p |> anti_join(taken, join_by(county, precinct_l2))

# --- keys ---
e = e |> mutate(
  in_bag = bag(nm),
  in_single = if_else(n_stem == 1 & str_detect(nm, "^[A-Z ]+( 1)?$"), str_remove(nm, " 1$"), NA),
  in_ward = if_else(str_detect(nm, "^WARD \\d+ \\d+$"), str_replace(nm, "^WARD ", "W "), NA),
  in_ward_dir = if_else(str_detect(nm, "^\\d+ (N|S|E|W|NE|NW|SE|SW|C)$"), nm, NA)
)
l = l |> mutate(
  in_bag = bag(nm),
  in_single = if_else(n_stem == 1 & str_detect(nm, "^[A-Z ]+( 1)?$"), str_remove(nm, " 1$"), NA),
  in_ward = str_match(nm, "^[A-Z ]+ (\\d+ \\d+)$")[, 2] |> (\(x) if_else(is.na(x), NA, paste("W", x)))(),
  in_ward_dir = str_match(nm, "^[A-Z ]+ (\\d+ (?:N|S|E|W|NE|NW|SE|SW|C))$")[, 2]
)

# in_prefix: ENR "<1-4 letters> <number...>" against L2 "<stem> <number...>" (or a
# bare one-precinct stem, rest "1")
e = e |> mutate(pfx = str_match(nm, "^([A-Z]{1,4}) (\\d.*)$")[, 2], rest = str_match(nm, "^([A-Z]{1,4}) (\\d.*)$")[, 3])
l = l |> mutate(rest = if_else(nm == st_, if_else(n_stem == 1, "1", NA), str_sub(nm, nchar(st_) + 2)))
pm = e |>
  filter(!is.na(pfx)) |>
  distinct(county, pfx, rest) |>
  inner_join(filter(l, !is.na(rest), st_ != "") |> distinct(county, st_, rest), join_by(county, rest), relationship = "many-to-many") |>
  filter(str_sub(st_, 1, 1) == str_sub(pfx, 1, 1)) |>
  count(county, pfx, st_, name = "both") |>
  left_join(count(filter(e, !is.na(pfx)), county, pfx, name = "n_pfx"), join_by(county, pfx)) |>
  left_join(count(filter(l, !is.na(rest)), county, st_, name = "n_st"), join_by(county, st_)) |>
  mutate(share = both / pmax(n_pfx, n_st)) |>
  filter(share >= 0.8) |>
  filter(share == max(share), .by = c(county, pfx)) |> filter(n() == 1, .by = c(county, pfx)) |>
  filter(share == max(share), .by = c(county, st_)) |> filter(n() == 1, .by = c(county, st_))
cli::cli_h2("in_prefix map")
print(arrange(pm, county, pfx), n = Inf)
e = e |> left_join(select(pm, county, pfx, st_map = st_), join_by(county, pfx)) |>
  mutate(in_prefix = if_else(is.na(st_map), NA, paste(st_map, rest)))
l = l |> mutate(in_prefix = if_else(is.na(rest), NA, paste(st_, rest)))

# in_num: counties whose ENR ids are bare numbers ("Precinct 57") while L2 names
# carry the township ("WASHINGTON 57"). Only where >= 90% of the county's ENR ids
# are bare numbers and every L2 trailing number is unique in the county (all L2
# precincts, matched or not): Delaware.
l_num = function(nm) str_match(nm, "^[A-Z ]+ (\\d+)$")[, 2]
num_cty = inner_join(
  enr_p |> filter(!no_voters(precinct_enr)) |> summarise(e_share = mean(str_detect(nm, "^\\d+$")), .by = county),
  l2_p |> mutate(k = l_num(nm)) |> summarise(l_ok = !anyNA(k) & !anyDuplicated(k), .by = county),
  join_by(county)) |>
  filter(e_share >= 0.9, l_ok)
cli::cli_alert_info("in_num counties: {num_cty$county}")
e = e |> mutate(in_num = if_else(county %in% num_cty$county & str_detect(nm, "^\\d+$"), nm, NA))
l = l |> mutate(in_num = if_else(county %in% num_cty$county, l_num(nm), NA))

# --- match, tier by tier, strict 1:1 within county ---
rules = c("in_bag", "in_single", "in_ward", "in_ward_dir", "in_prefix", "in_num")
matched = tibble(county = character(), precinct_enr = character(), precinct_l2 = character(), rule = character())
for (r in rules) {
  a = e |> anti_join(matched, join_by(county, precinct_enr)) |>
    select(county, precinct_enr, key = all_of(r)) |> filter(!is.na(key)) |> filter(n() == 1, .by = c(county, key))
  b = l |> anti_join(matched, join_by(county, precinct_l2)) |>
    select(county, precinct_l2, key = all_of(r)) |> filter(!is.na(key)) |> filter(n() == 1, .by = c(county, key))
  matched = bind_rows(matched, inner_join(a, b, join_by(county, key)) |> transmute(county, precinct_enr, precinct_l2, rule = r))
}
stopifnot(!anyDuplicated(matched[c("county", "precinct_enr")]), !anyDuplicated(matched[c("county", "precinct_l2")]))

out = matched |>
  left_join(distinct(enr_all, county, county_enr), join_by(county)) |>
  left_join(select(l2_all, county, precinct_l2, n), join_by(county, precinct_l2)) |>
  transmute(state = st, county = county_enr, precinct_enr, precinct_l2, rule, n_l2 = n) |>
  arrange(county, precinct_enr)

cli::cli_h2("IN rule pairs")
count(out, rule) |> print()
# review: every pair outside in_bag (and a sample of in_bag)
bind_rows(filter(out, rule != "in_bag"), slice_sample(filter(out, rule == "in_bag"), n = 20)) |>
  print(n = Inf)
left_e = anti_join(e, transmute(out, county = ckey(county), precinct_enr), join_by(county, precinct_enr))
cli::cli_alert_info("{nrow(out)} IN pairs; {nrow(left_e)} ENR ids still unmatched (no-voter ids excluded)")
print(count(left_e, county_enr, sort = TRUE), n = 30)

write_csv(select(out, state, county, precinct_enr, precinct_l2, rule), OUT)
