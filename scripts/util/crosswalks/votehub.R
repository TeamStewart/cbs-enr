source("scripts/util/crosswalks/helpers.R")

# Build ENR <-> VoteHub precinct crosswalks, one csv per state:
# input_data/crosswalks/votehub/{st}.csv with every ENR precinct in the state's
# clean/*_latest.parquet files and its VoteHub `unit_id` (NA if unmatched).
# Each build also refreshes the `precinct_votehub` column of the state's frame
# crosswalk (input_data/crosswalks/{st}.csv; add_votehub() in helpers.R).
# VoteHub boundaries: input_data/votehub-shapefiles/26_pct_bdrys.gpkg, plus a
# per-state {ST}26G/{ST}26P shapefile (folder or .zip) for any state not in
# the gpkg (the gpkg's states have the same polygons in their per-state files).
# States with no VoteHub layer or no clean data are skipped.
#
#   Rscript scripts/util/crosswalks/votehub.R          # states whose clean files or VoteHub layer are newer than their csv
#   Rscript scripts/util/crosswalks/votehub.R MI TX    # these states
#   Rscript scripts/util/crosswalks/votehub.R --all    # every VoteHub state with clean data
#
# Matching is within county, strict 1:1, through ordered tiers (match_tiers):
# ENR id against the VoteHub code (the `unit_id` part after "-:-") and against
# the VoteHub name (`precinct_sos`), exact first, then standardized keys.
# Per-state recodes go in `state_keys()`.

args = commandArgs(TRUE)
VH_DIR = "input_data/votehub-shapefiles"
OUT_DIR = "input_data/crosswalks/votehub"

# attribute tables only (no geometry); `src` is the file, for staleness
read_vh = function() {
  gpkg = election_path(VH_DIR, "26_pct_bdrys.gpkg")
  vh = sf::st_read(gpkg, query = "SELECT unit_id, state_abbr, geography, county, precinct_sos FROM precincts", quiet = TRUE) |>
    as_tibble() |>
    transmute(state = state_abbr, geography, county, unit_id, precinct_vh = precinct_sos, src = gpkg)
  # per-state layers ("CA26G" folder or "CA26G.zip") for states not in the gpkg,
  # or newer than the gpkg (then they replace its rows: TX26G.zip, Oct 28)
  files = fs::dir_ls(election_path(VH_DIR), regexp = "/[A-Z]{2}26[GP](\\.zip)?$")
  newer = fs::file_info(files)$modification_time > fs::file_info(gpkg)$modification_time
  files = files[!str_sub(fs::path_file(files), 1, 2) %in% vh$state | newer]
  files = files[order(str_sub(fs::path_file(files), 5, 5))]  # G before P
  files = files[!duplicated(fs::path_ext_remove(files))]  # folder wins over its zip
  files = files[!duplicated(str_sub(fs::path_file(files), 1, 2))]  # one layer per state
  vh = filter(vh, !state %in% str_sub(fs::path_file(files), 1, 2))
  extra = map(files, \(f) {
    layer = fs::path_ext_remove(fs::path_file(f))
    dsn = if (fs::path_ext(f) == "zip") paste0("/vsizip/", f) else f
    sf::st_read(dsn, query = paste("SELECT COUNTY, UNIQUE_ID, PCT FROM", layer), quiet = TRUE) |>
      as_tibble() |>
      transmute(state = str_sub(layer, 1, 2), geography = if_else(str_ends(layer, "P"), "primary", "general"),
                county = COUNTY, unit_id = UNIQUE_ID, precinct_vh = PCT, src = f)
  })
  bind_rows(vh, list_rbind(unname(extra)))
}
vh_all = read_vh()

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
    mid = num_key(str_match(x, "^\\d+-(\\w+)-")[, 2]),    # CA San Diego "0001-100000-SAN PASQUAL" -> "100000"
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
  if (st == "CA") {
    enr = mutate(enr, id_label = case_when(
      county == county_key("CA", "Riverside") ~ str_replace(precinct_enr, "^(\\d{3})(\\d{3})$", "\\1-\\2"), # "101100" = VH "101-100"
      county == county_key("CA", "Nevada") ~ str_extract(precinct_enr, "^CP\\d+"),               # "CP001-C3A1S1-B23" = VH "CP001"
      .default = id_label
    ))
  }
  if (st == "OH") {
    # Lucas "toledo 14a" = "City of Toledo 14 Precinct A" (alt_mi)
    # Montgomery short codes, from enr_abbr$MONTGOMERY in oh.R in VoteHub spelling (TWP
    # = TOWNSHIP, "JACKSON TOWNSHIP_NEW LEBANON"): "cv-m" = "CENTERVILLE M"
    # Clark bare "050 mr" / "071 t" (no precinct number) = VoteHub "MR-1" /
    # "T-1": numbered by sequence rank within the group, as in oh.R clark_name()
    oh_montgomery = c(
      "^BRK\\b" = "BROOKVILLE", "^BTLTWP\\b" = "BUTLER TOWNSHIP", "^CLATWP\\b" = "CLAY TOWNSHIP", "^CTN\\b" = "CLAYTON",
      "^CV\\b" = "CENTERVILLE", "^DAY\\b" = "DAYTON", "^ENG\\b" = "ENGLEWOOD", "^FARM\\b" = "FARMERSVILLE",
      "^GERTWP\\b" = "GERMAN TOWNSHIP", "^GTN\\b" = "GERMANTOWN", "^HARTWP\\b" = "HARRISON TOWNSHIP",
      "^HUB\\b" = "HUBER HEIGHTS", "^JAKTWP\\b" = "JACKSON TOWNSHIP", "^JEFTWP\\b" = "JEFFERSON TOWNSHIP",
      "^JNLB\\b" = "JACKSON TOWNSHIP_NEW LEBANON", "^KT\\b" = "KETTERING", "^MBG\\b" = "MIAMISBURG", "^MIATWP\\b" = "MIAMI TOWNSHIP",
      "^MOR\\b" = "MORAINE", "^OAK\\b" = "OAKWOOD", "^PERTWP\\b" = "PERRY TOWNSHIP", "^PHBG\\b" = "PHILLIPSBURG",
      "^PM\\b" = "PAGE MANOR", "^PNLB\\b" = "PERRY TOWNSHIP_NEW LEBANON", "^RV\\b" = "RIVERSIDE", "^TR\\b" = "TROTWOOD",
      "^UN\\b" = "UNION", "^VD\\b" = "VANDALIA", "^WASTWP\\b" = "WASHINGTON TOWNSHIP", "^WCR\\b" = "WEST CARROLLTON"
    )
    clark = enr$county == county_key("OH", "Clark")
    clark_seq = as.integer(str_extract(enr$precinct_enr, "^\\d+"))
    clark_grp = if_else(clark, str_remove(enr$precinct_enr, "^\\d+ "), NA_character_)
    clark_rank = ave(coalesce(clark_seq, 0L), coalesce(clark_grp, ""), FUN = rank)
    enr = mutate(enr, enr_name = case_when(
      county == county_key("OH", "Lucas") ~ str_replace_all(precinct_enr, "(?<=\\d)(?=[a-z])", " "),
      county == county_key("OH", "Montgomery") ~ str_replace_all(std_name(precinct_enr), oh_montgomery),
      clark & clark_grp %in% c("mr", "t") ~ paste(clark_grp, clark_rank),
      .default = enr_name
    ))
  }
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
  "code_mid",    "mid",    "c_num",
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
  "alt_bag",     "a_bag",  "n_bag",
  "alt_mi",      "a_mi",   "n_mi"
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
  # previous build's rate, to spot drops
  out_file = election_path(OUT_DIR, paste0(str_to_lower(st), ".csv"))
  prev = if (fs::file_exists(out_file)) {
    old = read_csv(out_file, col_types = cols(.default = "c"))
    glue::glue("; was {sum(!is.na(old$unit_id))}/{nrow(old)} ({round(mean(!is.na(old$unit_id)) * 100, 1)}%)")
  } else ""
  cli::cli_alert_info("{st}: {sum(!is.na(crosswalk$unit_id))}/{nrow(crosswalk)} ENR precincts matched ({round(mean(!is.na(crosswalk$unit_id)) * 100, 1)}%) to {nrow(vh)} VoteHub precincts ({paste(unique(vh$geography), collapse = '/')}){prev}")

  fs::dir_create(election_path(OUT_DIR))
  enightdata::write_local(crosswalk, paste0(OUT_DIR, "/", str_to_lower(st), ".csv"), parquet = FALSE)

  # refresh precinct_votehub in the frame crosswalk, if the state has one
  frame_xw = election_path("input_data/crosswalks", paste0(str_to_lower(st), ".csv"))
  if (fs::file_exists(frame_xw)) {
    xw = add_votehub(st, read_csv(frame_xw, col_types = cols(.default = "c")))
    write_crosswalk(st, xw)
    cli::cli_alert_info("{st} frame crosswalk: precinct_votehub for {sum(!is.na(xw$precinct_votehub))}/{sum(!is.na(xw$precinct_enr))} rows with an ENR id")
  }
  crosswalk
}

vh_states = unique(vh_all$state)
states = if ("--all" %in% args) vh_states else if (length(args)) toupper(args) else {
  # default: states whose newest clean file or VoteHub layer is newer than their crosswalk
  keep(vh_states, \(st) {
    clean = Sys.glob(election_path(st, "clean", "*_latest.parquet"))
    inputs = c(clean, unique(vh_all$src[vh_all$state == st]))
    out = election_path(OUT_DIR, paste0(str_to_lower(st), ".csv"))
    length(clean) > 0 && (!fs::file_exists(out) || max(fs::file_info(inputs)$modification_time) > fs::file_info(out)$modification_time)
  })
}
skip = setdiff(states, vh_states)
if (length(skip)) cli::cli_alert_warning("No VoteHub layer for {skip}; skipped.")
if (!length(intersect(states, vh_states))) cli::cli_alert_success("No VoteHub crosswalk is stale.")
walk(intersect(states, vh_states), build_state)
