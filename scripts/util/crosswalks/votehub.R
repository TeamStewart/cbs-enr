source("scripts/util/crosswalks/helpers.R")

# Build ENR <-> VoteHub precinct crosswalks, one csv per state:
# input_data/crosswalks/votehub/{st}.csv with every ENR precinct in the state's
# clean/*_latest.parquet files and its VoteHub `unit_id` (NA if unmatched).
# Each build also refreshes the `precinct_votehub` column of the state's frame
# crosswalk (input_data/crosswalks/{st}.csv; add_votehub() in helpers.R).
# VoteHub boundaries: input_data/votehub-shapefiles/26_pct_bdrys.gpkg, plus a
# per-state {ST}26G/{ST}26P shapefile (folder or .zip) for any state not in
# the gpkg (the gpkg's states have the same polygons in their per-state files).
# MN26G.zip is built from the MN SoS mn-precincts.json by votehub_mn.R.
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
# - NE: ENR ids are SOS precinct names ("08-01", "Precinct 54"; scrape_ne()
#   since 2026-10-31), so they go to the name tiers, not the id tiers.
state_keys = function(st, enr) {
  if (st == "NE") {
    # County-specific spellings (others are matched in vh_manual):
    # Dawson "Goth East 008" = "Gothenburg East", "Lex 1st 012" = "Lexington 1st";
    # Dodge "City of Fremont 1A 001" = "Fremont 1A", "Cuming/Everett Twps 023" =
    # "Cuming/Everett Townships"; Hamilton "1 001" = "Precinct 1", "W1 009" = "Ward 1";
    # Box Butte "City of Alliance 001" / "Rural Alliance 11" = "Precinct 1" / "Precinct 11";
    # Cass "001 - Tipton" = "Tipton", "021 - Plattsmouth 1st Ward" = "1st Ward";
    # Adams "11 1-A" = "1A", "02 Juniata/Verona" = "Juniata/Verona"
    seq = str_remove(enr$precinct_enr, "\\s+\\d{3}$")
    enr = mutate(enr, id_label = NA_character_, enr_name = case_when(
      county == county_key("NE", "Dawson") ~ str_replace_all(seq, c("^Goth\\b" = "Gothenburg", "^Lex\\b" = "Lexington")),
      county == county_key("NE", "Dodge") ~ str_replace(seq, "\\bTwps$", "Townships"),
      county == county_key("NE", "Hamilton") ~ str_replace_all(seq, c("^W(\\d+)$" = "Ward \\1", "^(\\d+)$" = "Precinct \\1")),
      county == county_key("NE", "Box Butte") ~ paste("Precinct", as.integer(str_extract(precinct_enr, "\\d+$"))),
      county == county_key("NE", "Cass") ~ str_replace(str_remove(precinct_enr, "^\\d+ - "), "^Plattsmouth (\\d\\w\\w Ward)$", "\\1"),
      county == county_key("NE", "Adams") ~ str_replace(str_remove(precinct_enr, "^\\d+ "), "^(\\d)-([A-Z])$", "\\1\\2"),
      .default = precinct_enr
    ))
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
  if (st == "TX") {
    # Robertson "Calvert 1" = VoteHub "0001". Reeves ENR "102"/"310" (as in the
    # frame) = VoteHub "0002"/"0010": the last two digits (as in tx.R's L2 rows).
    enr = mutate(enr, id_label = case_when(
      county == county_key("TX", "Robertson") ~ str_extract(precinct_enr, "\\d+$"),
      county == county_key("TX", "Reeves") ~ str_sub(precinct_enr, -2),
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
    # Carroll "augusta 001" = "AUGUSTA" (trailing ENR sequence number)
    enr = mutate(enr, enr_name = case_when(
      county == county_key("OH", "Carroll") ~ str_remove(precinct_enr, "\\s+\\d{3}$"),
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

# Hand-matched pairs, ENR id = VoteHub code (the `unit_id` part after "-:-"), for
# counties whose ENR labels are polling places, initials or reordered town lists.
# Each county has as many ENR as VoteHub precincts and every pair is the only
# candidate. Tried before the tiers.
vh_pairs = function(st, county, x) tibble(state = st, county = county_key(st, county), precinct_enr = names(x), vh_code = unname(x))
vh_manual = bind_rows(
  vh_pairs("NE", "Adams", c("01 Wstbl/Hgh" = "West Blue/Highland")),
  vh_pairs("NE", "Dodge", c(
    "Platte East Township 026" = "Platte East", "Platte West Township 027" = "Platte West",
    "UN/CT/RG/PV Twps 032" = "Union/Cott/Ridg/PV Twps")),
  vh_pairs("NE", "Holt", c(
    "Antelope Iowa & Verdigris" = "01 Antelope, Iowa & Verdigris", "Atkinson Surronunding TWNSPS" = "02 Atk & Surrounding Twps",
    "Chambers & Surrounding TWNSHPS" = "03 Chmbrs/Surrounding Twps", "Emmet & Northeast TWNSHPS" = "04 Emmet & Northeast Twps",
    "Ewing, Golden & Deloit TWNSHPS" = "05 Ewing, Golden & Deloit", "Inman Township" = "06 Inman Twp",
    "Stuart & Cleveland Townships" = "07 Stuart & Cleveland Twps", "Grattan & Shields Townships" = "08 Grattan & Shields Twps",
    "Atkinson Ward I" = "09 Atkinson Ward I", "Grattan-O'Neill Ward I" = "10 O'Neill Ward I",
    "Grattan-O'Neill Ward II" = "11 O'Neill Ward II", "Grattan-O'Neill Ward III" = "12 O'Neill Ward III",
    "Grattan-O'Neill Ward IV" = "13 O'Neill Ward IV")),
  # Boelus, Cushing, Farwell vs Loup Fork, Posen, Spring Creek: unknown pairing, NA
  vh_pairs("NE", "Howard", c(
    "St. Paul Ward I" = "St. Paul Ward 1", "St. Paul Ward II" = "St. Paul Ward 2", "Village of Dannebrog & Rural" = "Dannebrog",
    "Village of Elba & Rural" = "Elba", "Village of St. Libory & Rural" = "St. Libory")),
  # the two Red Cloud precincts (Central Co., SW) vs Red Cloud 1st/2nd: unknown pairing, NA
  vh_pairs("NE", "Webster", c(
    "City of BH & Rural NE" = "Blue Hill", "Village of Bladen & Rural NW" = "Bladen", "Village of Guide Rock & Rural" = "Guide Rock")),
  vh_pairs("IA", "Adair", c("1NW" = "1NW ADAIR", "2NE" = "2NE STUART", "3SW" = "3SW FONTANELLE", "4SE" = "4SE ORIENT", "5GF" = "5GF GREENFIELD")),
  vh_pairs("IA", "Allamakee", c(
    "PCT 1 - WL/HV" = "WL/HV", "PCT 2 - FC/JF/LL/MK/UP" = "UP/MK/FC/JF/LL", "Pct 3 - PO/FK/PV CITY" = "PO/FK/PV CITY",
    "Pct 4 - LT/PC/WV CITY" = "PC/LT/WV CITY", "Pct 5 - FV/TL/HF CITY" = "FV/TL/HF CITY", "Pct 6 - CN/LF/LS/LS CITY" = "LF/CN/LS/LS CITY",
    "Pct 7 - IA/UC/NA City" = "UC/IA/NA CITY", "Pct 8 - WAUKON WARD 1" = "WK 1 CITY", "Pct 9 - WAUKON WARD 2" = "WK 2 CITY",
    "Pct 10 - WAUKON WARD 3" = "WK 3 CITY")),
  vh_pairs("IA", "Audubon", c("Precinct #1 Audubon N1/2" = "PRECINCT 1", "Precinct #2 Exira S1/2" = "PRECINCT 2")),
  vh_pairs("IA", "Benton", c(
    "Belle Plaine 1" = "PRECINCT BP1", "Belle Plaine 2" = "PRECINCT BP2", "Belle Plaine 3" = "PRECINCT BP3",
    "Benton-Canton-Shellsburg" = "PRECINCT BN", "Big Grove-Union-Van Horne" = "PRECINCT BG",
    "Bruce-Cedar-Monroe-Mt. Auburn" = "PRECINCT BR", "Eden-Harrison-Taylor-Jackson-Garrison" = "PRECINCT ED",
    "Eldorado-Newhall" = "PRECINCT EL", "Florence-St. Clair-Norway" = "PRECINCT FL", "Fremont-Atkins" = "PRECINCT FR",
    "Homer-Kane-Keystone" = "PRECINCT HO", "Iowa-Leroy-Luzerne-Blairstown" = "PRECINCT IA", "Polk-Urbana" = "PRECINCT PO",
    "Vinton 1" = "PRECINCT VT1", "Vinton 2" = "PRECINCT VT2", "Vinton 3" = "PRECINCT VT3", "Vinton 4" = "PRECINCT VT4",
    "Walford" = "PRECINCT WA")),
  vh_pairs("IA", "Grundy", setNames(paste0("P", 1:7), paste("PRECINCT", 1:7))),
  vh_pairs("IA", "Harrison", c(
    "Precinct #1 Little Sioux" = "LITTLE SIOUX", "Precinct #2 Pisgah" = "JACKSON ALLEN PISGAH",
    "Precinct #3 Dunlap" = "HARRISON DOUGLAS DUNLAP", "Precinct #4 Mondamin" = "MORGAN RAGLAN MONDAMIN",
    "Precinct #5 Magnolia" = "MAGNOLIA CALHOUN", "Precinct #6 Woodbine" = "BOYER LINCOLN WOODBINE",
    "Precinct #7 Modale" = "CLAY TAYLOR CINCINNATI MODALE", "Precinct #8 Logan" = "JEFFERSON LAGRANGE LOGAN",
    "Precinct #9 St John Twp" = "ST JOHN TWP", "Precinct #10 Missouri Valley City" = "MISSOURI VALLEY CITY",
    "Precinct #11 Persia" = "CASS UNION WASHINGTON PERSIA")),
  # precincts 1 and 2 are the two Chariton wards, paired by number
  vh_pairs("IA", "Lucas", c(
    "Precinct #1-Chariton City Hall" = "CHARITON WARD #1", "Precinct #2-Chariton Community Center" = "CHARITON WARD #2",
    "Precinct #3-Lucas County Extension" = "EXTENSION PRECINCT", "Precinct #4-Lucas Community Center" = "LUCAS PRECINCT",
    "Precinct #5-Pin Oak Marsh" = "PIN OAK MARSH", "Precinct #6-Russell Community Center" = "RUSSELL PRECINCT",
    "Precinct #7-Williamson Community Center" = "WILLIAMSON PRECINCT")),
  vh_pairs("IA", "Lyon", c(
    "George Comm Ctr P1" = "PCT 1 GEORGE", "Little Rock Town and Country P2" = "PCT 2 LITTLE ROCK", "Rock Rapids N P3" = "PCT 3 ROCK RAPIDS N",
    "Rock Rapids S P4" = "PCT 4 ROCK RAPIDS S", "Doon City Hall P5" = "PCT 5 DOON", "Inwood P6" = "PCT 6 INWOOD",
    "Logan S P7" = "PCT 7 LOGAN S", "Logan N P8" = "PCT 8 LOGAN N", "Lester P9" = "PCT 9 LESTER", "Larchwood P10" = "PCT 10 LARCHWOOD")),
  vh_pairs("IA", "Monroe", c(
    "No. 1 - Albia First Ward" = "PRECINCT AL1", "No. 2 - Albia Second Ward" = "PRECINCT AL2", "No. 3 - Albia Third Ward" = "PRECINCT AL3",
    "No. 4 - Albia Fourth Ward" = "PRECINCT AL4", "No. 5 - Courthouse(GL,TR,MT,PL,UR,MN,Eddy/Mor Cty)" = "PRECINCT COURTHSE(TR/MT/GL/UR/MN/PL)",
    "No. 6 - Lovilia Comm Center (BC,CE,UN; Lov Cty)" = "PRECINCT LOVILIA(CE/UN/BC/LV)",
    "No. 7 - St. Patrick's Church (FR,JK,WY; Mel Cty)" = "PRECINCT MELROSE(JK/WY/FR/ML)")),
  vh_pairs("IA", "Montgomery", c(
    "Gold Fair Building Ward #1" = "WARD1 PILOT GROVE FRANKFORT RO TWP", "Gold Fair Building Ward #2" = "WARD2 SHERMAN RO TWP",
    "First Christian Church Ward #3" = "WARD3 SHERMAN PILOT GROVE FRANKFORT RO TWP",
    "Elliott Community Building Precinct #4" = "4 GRFLD LNCLN SHER PILOT GROVE TWP ELLIOTT",
    "Elliott Community Building Precinct #5" = "5 PILOT GROVE DOUGLAS TWP GRANT",
    "Stanton Community Building Precinct #6" = "6 WEST GRANT FRANKFORT SCOTT TWP COBURG STANTON",
    "Villisca Community Building Precinct #7" = "7 SCOTT EAST FRANKFORT WASHINGTON TWP VILLISCA")),
  vh_pairs("IA", "Sac", c(
    "Schaller" = "SCHALLER PCT 1", "Nemaha" = "NEMAHA PCT 2", "Early" = "EARLY PCT 3", "Sac City" = "SAC CITY PCT 4", "Lytton" = "LYTTON PCT 5",
    "Odebolt" = "ODEBOLT PCT 6", "Lake View" = "LAKE VIEW PCT 7", "Auburn" = "AUBURN PCT 8", "Wall Lake" = "WALL LAKE PCT 9")),
  vh_pairs("IA", "Taylor", c(
    "Courthouse Precinct 1" = "PRECINCT 1", "Courthouse Precinct 2" = "PRECINCT 2", "Lenox Community Center Precinct 3" = "PRECINCT 3",
    "Gravity Fire Station Precinct 4" = "PRECINCT 4", "New Market Community Center Precinct 5" = "PRECINCT 5")),
  # PCT 9 "DETSUP4/SP" = VoteHub "DETSUP4 & SR" (SP/SR: Springfield, the only leftover)
  vh_pairs("IA", "Winneshiek", c(
    "PCT 1 - FM/BO/OR/BLF/MA" = "FR BO OR BL & MA", "PCT 2 - HE/HI/CN/PL/GL" = "HE HI CN PL & GL", "PCT 3 - DEW1/DETSUP2" = "DEW1 & DETSUP2",
    "PCT 4 - DEW2" = "DEW2", "PCT 5 - DEW3A" = "DEW3A", "PCT 6 - DEW3B" = "DEW3B", "PCT 7 - DEW4" = "DEW4", "PCT 8 - DEW5" = "DEW5",
    "PCT 9 - DETSUP4/SP" = "DETSUP4 & SR", "PCT 10 - FK/BM/MI" = "FR BM & MI", "PCT 11 - CA/WA/JA" = "CA WA & JA", "PCT 12 - LI/SU" = "LI & SU")),
  # ENR polling places; VoteHub "Precinct 1 Eastpoint" (code 001) etc.
  vh_pairs("FL", "Franklin", c(
    "Eastpoint" = "001", "Alligator Point" = "002", "Holy Family Center" = "003", "Historic Holy Family" = "004",
    "Carrabelle" = "005", "Lanark Village" = "006", "St. George Island" = "007", "American Legion" = "008"))
)
# FL26G / NE26G / OH26G (Nov 2) replaced the gpkg layers: same polygons, but OH and part of NE/FL
# renamed their codes, and the new names match the ENR ids less often. These pairs carry the
# 10-31 matches over, through polygon overlap (input_data/votehub-shapefiles/
# unit_id_remap_20261102.csv: old -> new unit_id where IoU > 0.95), for every ENR precinct the
# new layer alone matched differently or not at all. They replace vh_manual pairs for the same
# precinct.
# Every scripts/util/crosswalks/votehub_manual_*.csv is read (state, county, precinct_enr,
# vh_code): the 11-02 carry-over above, and hand-matched batches (votehub_manual_hand_20261103.csv:
# IA polling-place labels, GA Chatham, NE, OH Pickaway, from the 11-03 hand-matching worksheet).
vh_carry = fs::dir_ls("scripts/util/crosswalks", regexp = "votehub_manual_[^/]*\\.csv$") |>
  map(\(f) read_csv(f, col_types = cols(.default = "c"))) |>
  list_rbind() |>
  select(state, county, precinct_enr, vh_code) |>
  mutate(county = map2_chr(state, county, county_key)) |>
  distinct(state, county, precinct_enr, .keep_all = TRUE)
vh_manual = bind_rows(anti_join(vh_manual, vh_carry, join_by(state, county, precinct_enr)), vh_carry)

# A county with one ENR and one VoteHub precinct, both unmatched (NE Hooker, Loup)
match_single = function(enr_keys, vh_keys, matched) {
  e = anti_join(enr_keys, matched, join_by(county, precinct_l2)) |> filter(n() == 1, .by = county) |>
    semi_join(count(enr_keys, county) |> filter(n == 1), join_by(county))
  v = anti_join(vh_keys, matched, join_by(county, precinct_yougov)) |> filter(n() == 1, .by = county) |>
    semi_join(count(vh_keys, county) |> filter(n == 1), join_by(county))
  inner_join(select(e, county, precinct_l2), select(v, county, precinct_yougov), join_by(county)) |>
    anti_join(matched, join_by(county)) |>
    mutate(l2_rule = "single")
}

build_state = function(st) {
  files = Sys.glob(election_path(st, "clean", "*_latest.parquet")) |> fs::path_rel(election_path())
  vh = filter(vh_all, state == st)
  if (!length(files) || !nrow(vh)) return(NULL)

  enr = read_enr(files) |>
    mutate(county_enr = county, county = county_key(st, county)) |>
    state_keys(st = st)
  vh = mutate(vh, county = county_key(st, county))

  man = filter(vh_manual, state == st)
  enr_keys = bind_cols(
    transmute(enr, county, precinct_l2 = precinct_enr, n_l2 = NA_integer_),
    base_keys(enr$id_label),
    base_keys(enr$enr_name) |> rename_with(\(x) paste0("a_", x))
  ) |>
    left_join(transmute(man, county, precinct_l2 = precinct_enr, man = str_to_upper(vh_code)), join_by(county, precinct_l2))
  vh_keys = bind_cols(
    transmute(vh, county, precinct_yougov = unit_id, frame_name = precinct_vh, n_frame = NA_integer_),
    base_keys(str_remove(vh$unit_id, "^\\d+-:-")) |> rename_with(\(x) paste0("c_", x)),
    base_keys(vh$precinct_vh) |> rename_with(\(x) paste0("n_", x))
  )
  # a manual pair whose ENR id or VoteHub code is gone (relabelled feed or new layer)
  stale = anti_join(man, transmute(enr, county, precinct_enr), join_by(county, precinct_enr)) |>
    bind_rows(anti_join(mutate(man, vh_code = str_to_upper(vh_code)), transmute(vh_keys, county, vh_code = c_upper), join_by(county, vh_code)))
  if (nrow(stale)) cli::cli_alert_warning("{st}: {nrow(stale)} vh_manual pair{?s} not found: {paste(stale$county, stale$precinct_enr, sep = ' ', collapse = '; ')}")

  matched = match_tiers(enr_keys, vh_keys, bind_rows(tibble(rule = "manual", l2_key = "man", fr_key = "c_upper"), tiers))
  matched = bind_rows(matched, match_single(enr_keys, vh_keys, matched)) |>
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

# run as a script (l2_direct.R sources this file for its keys and tiers)
if (sys.nframe() == 0) {
  vh_all = read_vh()
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
}
