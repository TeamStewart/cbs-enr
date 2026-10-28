source("scripts/util/crosswalks/helpers.R")

# Build ENR <-> VoteHub precinct crosswalks, one csv per state:
# input_data/crosswalks/votehub/{st}.csv with every ENR precinct in the state's
# clean/*_latest.parquet files and its VoteHub `unit_id` (NA if unmatched).
# VoteHub boundaries: input_data/votehub-shapefiles/26_pct_bdrys.gpkg (all
# states; the per-state {ST}26G/{ST}26P shapefiles are the same polygons).
# States with no VoteHub layer or no clean data are skipped.
#
#   Rscript scripts/util/crosswalks/votehub.R          # states whose clean files are newer than their csv
#   Rscript scripts/util/crosswalks/votehub.R MI TX    # these states
#   Rscript scripts/util/crosswalks/votehub.R --all    # every VoteHub state with clean data
#
# Matching is within county, strict 1:1, through ordered tiers (match_tiers):
# ENR id against the VoteHub code (the `unit_id` part after "-:-") and against
# the VoteHub name (`precinct_sos`), exact first, then standardized keys.
# Per-state recodes go in `state_keys()`.

args = commandArgs(TRUE)
VH_GPKG = "input_data/votehub-shapefiles/26_pct_bdrys.gpkg"
OUT_DIR = "input_data/crosswalks/votehub"

vh_all = sf::st_read(election_path(VH_GPKG), quiet = TRUE) |>
  sf::st_drop_geometry() |>
  as_tibble() |>
  transmute(state = state_abbr, geography, county, unit_id, precinct_vh = precinct_sos)

norm_county = function(x) str_remove_all(str_remove(str_to_upper(x), " COUNTY$"), "[^A-Z]")

num_key = function(x) str_replace_all(str_to_upper(str_squish(x)), "(^|(?<=[A-Z]))0+(?=\\d)", "")
lead_key = function(x) num_key(str_match(x, "^\\s*([A-Za-z]?\\d+[A-Za-z]?)(?=[\\s.-]|$)")[, 2]) # "16-BPK Park" -> "16"
# MI: "Shelby Twp, Pct 18" = "Shelby Charter Township, Precinct 18",
# "Pontiac, Precinct 2" = "City of Pontiac, Precinct 2"; mi_key2 also drops
# TOWNSHIP (Roscommon "Mills 1" = "Mills Township, Precinct 1")
mi_key = function(x, drop = "CITY|OF|CHARTER|PRECINCT") {
  x |>
    std_name() |>
    str_replace_all("\\bTWP\\b", "TOWNSHIP") |>
    str_remove_all(paste0("\\b(", drop, ")\\b")) |>
    str_squish() |>
    bag()
}
mi_key2 = function(x) mi_key(x, "CITY|OF|CHARTER|PRECINCT|TOWNSHIP")
# OH: Summit "031 akron 3-i" = "AKRON 3-I"; Hamilton "2507 cin 25-g" =
# "CINCINNATI 25-G" (first word cut to 3 letters, keep ward/precinct tokens)
abbr_key = function(x) {
  w = str_split(strip_lead(std_name(x)), " ")
  map_chr(w, \(t) if (length(t) < 2 || !str_detect(t[1], "^[A-Z]{3}")) NA_character_ else
    paste(c(str_sub(t[1], 1, 3), t[-1][str_detect(t[-1], "\\d|^[A-Z]$")]), collapse = " "))
}
# OH Summit/Stark short codes: "071 akron 6-k" = "AK07-K", "copley twp k" =
# "COPL-K", Stark "lak12" = "LAKE TWP 12", "cc4f" = "CANTON CITY 4-F".
# `short` cuts the first word to 2 letters; `init` uses the initials of the
# words (or a lone word of up to 3 letters as is). Both keep only ward/precinct
# tokens after the name.
code_tokens = function(x) {
  strip_lead(std_name(x)) |>
    str_replace_all("(?<=[A-Z])(?=\\d)|(?<=\\d)(?=[A-Z])", " ") |>
    str_replace_all("\\b0+(?=\\d)", "") |>
    str_split(" ")
}
is_tail = function(t) str_detect(t, "^\\d+$|^[A-Z]$")
short_key = function(x) map_chr(code_tokens(x), \(t) {
  if (is.na(t[1]) || !str_detect(t[1], "^[A-Z]{2,}$")) return(NA_character_)
  paste(c(str_sub(t[1], 1, 2), t[-1][is_tail(t[-1])]), collapse = " ")
})
init_key = function(x) map_chr(code_tokens(x), \(t) {
  if (anyNA(t)) return(NA_character_)
  words = t[cumsum(is_tail(t)) == 0 & str_detect(t, "^[A-Z]{2,}$") & !t %in% c("TWP", "TOWNSHIP")]
  if (!length(words)) return(NA_character_)
  head = if (length(words) == 1 && nchar(words) <= 3) words else paste(str_sub(words, 1, 1), collapse = "")
  paste(c(head, t[is_tail(t)]), collapse = " ")
})

# keys shared by both sides; `x` is the ENR id or the VoteHub code/name
base_keys = function(x) {
  tibble(
    upper = str_to_upper(str_squish(x)),
    std = std_name(str_remove(x, "\\.0+$")),         # Miami-Dade "592.0"
    num = num_key(str_remove(x, "\\.0+$")),
    lead = lead_key(x),
    paren = num_key(str_match(x, "\\(([^)]+)\\)\\s*$")[, 2]), # NE Cedar "PCT 01  (1)"
    pnum = num_key(str_match(str_to_upper(x), "^(?:PRECINCT|PCT)\\.?\\s*(\\S+)$")[, 2]),
    nolead = std_name(strip_lead(x)),
    bag = bag(std_name(x)),
    mi = mi_key(x),
    mi2 = mi_key2(x),
    abbr = abbr_key(x),
    short = short_key(x),
    init = init_key(x)
  )
}

# County key for matching. AK ENR counties are election districts and
# VoteHub's are house districts, but AK ids ("01-600") are statewide.
county_key = function(st, x) if (st == "AK") rep("AK", length(x)) else norm_county(x)

# State-specific ENR labels. `id_label` (default: the ENR id) feeds the id
# tiers; `enr_name` (optional) is a second label tried after them.
# - NE: ENR ids are a per-county sequence (0001, ...), so the id tiers are off;
#   the SOS precinct names come from the raw file of the same scrape (as in ne.R).
state_keys = function(st, enr) {
  if (st == "NE") {
    stamp = Sys.glob(election_path("NE/clean/NE_2*.parquet")) |> fs::path_file() |> fs::path_ext_remove() |> max()
    stopifnot(setequal(
      unique(as.character(enightdata::read_local(paste0("NE/clean/", stamp, ".parquet"), parquet = TRUE)$geo)),
      unique(as.character(enightdata::read_local("NE/clean/NE_latest.parquet", parquet = TRUE)$geo))
    ))
    ne_names = readr::read_csv(election_path("NE/raw", paste0(stamp, ".csv")), col_types = readr::cols(.default = "c")) |>
      filter(StatePrecinctID != "_TOT") |>
      distinct(county = norm_county(CountyName), precinct_enr = StatePrecinctID, enr_name = PrecinctName)
    enr = left_join(enr, ne_names, join_by(county, precinct_enr)) |> mutate(id_label = NA_character_)
  }
  if (!"enr_name" %in% names(enr)) enr$enr_name = NA_character_
  if (!"id_label" %in% names(enr)) enr$id_label = enr$precinct_enr
  enr
}

tiers = tribble(
  ~rule,         ~l2_key,  ~fr_key,
  "code_exact",  "upper",  "c_upper",
  "name_exact",  "upper",  "n_upper",
  "code_num",    "num",    "c_num",
  "name_std",    "std",    "n_std",
  "name_num",    "num",    "n_num",
  "name_pnum",   "num",    "n_pnum",
  "name_paren",  "num",    "n_paren",
  "code_lead",   "lead",   "c_num",
  "name_lead",   "num",    "n_lead",
  "lead_lead",   "lead",   "n_lead",
  "nolead_name", "nolead", "n_std",
  "name_bag",    "bag",    "n_bag",
  "name_mi",     "mi",     "n_mi",
  "name_mi2",    "mi2",    "n_mi2",
  "name_abbr",   "abbr",   "n_abbr",
  "name_short",  "short",  "n_short",
  "name_init",   "init",   "n_init",
  "alt_exact",   "a_upper", "n_upper",
  "alt_code",    "a_upper", "c_upper",
  "alt_std",     "a_std",  "n_std",
  "alt_num",     "a_num",  "n_num",
  "alt_codenum", "a_num",  "c_num",
  "alt_pnum",    "a_pnum", "n_pnum",
  "alt_lead",    "a_lead", "n_lead",
  "alt_bag",     "a_bag",  "n_bag"
)

build_state = function(st) {
  files = Sys.glob(election_path(st, "clean", "*_latest.parquet")) |> fs::path_rel(election_path())
  vh = filter(vh_all, state == st)
  if (!length(files) || !nrow(vh)) return(NULL)

  enr = read_enr(files) |>
    mutate(county_enr = county, county = county_key(st, county)) |>
    state_keys(st = st)
  vh = mutate(vh, county = county_key(st, county))

  enr_keys = bind_cols(
    transmute(enr, county, precinct_l2 = precinct_enr, n_l2 = NA_integer_),
    base_keys(enr$id_label),
    base_keys(enr$enr_name) |> rename_with(\(x) paste0("a_", x))
  )
  vh_keys = bind_cols(
    transmute(vh, county, precinct_yougov = unit_id, frame_name = precinct_vh, n_frame = NA_integer_),
    base_keys(str_remove(vh$unit_id, "^\\d+-:-")) |> rename_with(\(x) paste0("c_", x)),
    base_keys(vh$precinct_vh) |> rename_with(\(x) paste0("n_", x))
  )

  matched = match_tiers(enr_keys, vh_keys, tiers) |>
    select(county, precinct_enr = precinct_l2, unit_id = precinct_yougov, vh_rule = l2_rule)

  crosswalk = enr |>
    left_join(matched, join_by(county, precinct_enr)) |>
    left_join(select(vh, unit_id, precinct_vh, geography), join_by(unit_id)) |>
    transmute(state = st, county = county_enr, precinct_enr, unit_id, precinct_vh, vh_geography = geography, vh_rule) |>
    arrange(county, precinct_enr)
  stopifnot(nrow(crosswalk) == nrow(enr))

  # review: rules, coverage by county, fuzzy matches to eyeball
  cli::cli_h1(st)
  count(crosswalk, vh_rule) |> print()
  crosswalk |>
    summarise(n_enr = n(), matched = sum(!is.na(unit_id)), .by = county) |>
    mutate(share = round(matched / n_enr, 3)) |>
    filter(share < 1) |>
    arrange(share) |>
    print(n = 40)
  crosswalk |>
    filter(!vh_rule %in% c("code_exact", "name_exact", "code_num", "name_std")) |>
    select(county, precinct_enr, precinct_vh, vh_rule) |>
    slice_sample(n = 15) |>
    print(n = 15)
  cli::cli_alert_info("{st}: {sum(!is.na(crosswalk$unit_id))}/{nrow(crosswalk)} ENR precincts matched ({round(mean(!is.na(crosswalk$unit_id)) * 100, 1)}%) to {nrow(vh)} VoteHub precincts ({paste(unique(vh$geography), collapse = '/')})")

  fs::dir_create(election_path(OUT_DIR))
  enightdata::write_local(crosswalk, paste0(OUT_DIR, "/", str_to_lower(st), ".csv"), parquet = FALSE)
  crosswalk
}

vh_states = unique(vh_all$state)
states = if ("--all" %in% args) vh_states else if (length(args)) toupper(args) else {
  # default: states whose newest clean file is newer than their crosswalk
  keep(vh_states, \(st) {
    clean = Sys.glob(election_path(st, "clean", "*_latest.parquet"))
    out = election_path(OUT_DIR, paste0(str_to_lower(st), ".csv"))
    length(clean) > 0 && (!fs::file_exists(out) || max(fs::file_info(clean)$modification_time) > fs::file_info(out)$modification_time)
  })
}
skip = setdiff(states, vh_states)
if (length(skip)) cli::cli_alert_warning("No VoteHub layer for {skip}; skipped.")
if (!length(intersect(states, vh_states))) cli::cli_alert_success("No VoteHub crosswalk is stale.")
walk(intersect(states, vh_states), build_state)
