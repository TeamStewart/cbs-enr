# ========================================
## Prep one state's precinct inputs for the election-night models (2026 general)
##
## Generalizes prep_tx_model_inputs.R (and reproduces its TX output exactly) to every state
## we model: the races in 26_general/input_data/priors.csv. State-specific handling:
##   counties  CBS's county numbers are matched by county NAME (letters only, no type
##             words), not by FIPS order: FIPS order breaks in FL, where Miami-Dade's code
##             changed (025 -> 086) but CBS still files it under D.
##             Alaska has no counties in CBS's feed: its units are the 40 state house
##             election districts (ED01 ... ED40), so an AK precinct's CBS "county" is its
##             house district (the SHnn in the YouGov code) and its county_fips is 02 + the
##             district number (02001 ... 02040, as votehub uses). county_name stays the
##             borough / census area.
##   ids       the crosswalk's precinct_yougov is the YouGov precinct26_id in most states,
##             but the full precinct26_code in MI (ids repeat across municipalities there);
##             the script detects which. MI's yougov geo id is the code without its
##             "MI_<COUNTY>_" prefix. CA ids arrive as "456100.0": the ".0" is dropped.
##   CDs       at-large "AK-AL" is district 1.
## Run from the cbs-enr folder:  Rscript scripts/model/prep_model_inputs.R AK
## Any state with a YouGov frame, a crosswalk and an L2 file (inputs below) works.
##
## Reads only shared files in Dropbox 26_general/input_data, so anyone on the
## team can run it, and writes the prepped files back to Dropbox:
##
##   26_general/input_data/model_inputs/
##     {ST}_precinct.parquet             one row per 2026 precinct (geo):
##                                       spine (data contract §1) + covariates (§2)
##     {ST}_precinct_mode_history.parquet geo x election x method voter counts (§3)
##     {ST}_precinct_cd.parquet          geo x congressional district pieces (§9,
##                                       only needed for House races)
##     {ST}_counties.csv                 every county: FIPS <-> CBS county number
##     {ST}_race_prior.csv               {ST} rows of priors.csv in the §6 layout
##     {ST}_prep_report.txt              everything printed while running
##
## Inputs (26_general/input_data/):
##   yougov_precinct_frame.parquet         precinct x CD pieces: n, turnout rates,
##                                         race, pr_harris / pr_trump
##   yougov_precinct_frame_method.parquet  same by 2024 vote method (cross-check only)
##   crosswalks/{st}.csv                     YouGov id -> L2 id (+ ENR id where known)
##   l2/{st}_YYYYMMDD_precincts.parquet      L2 precinct counts (newest file is used)
##   priors.csv                            expert priors by race
##
## Needs: tidyverse, glue, nanoparquet, enightmodels
# ========================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
})
# bucket_class() / geo_label(): section 6 needs them to exclude county/site reporting geos
# from the live-id join rate, since a bucket is SUPPOSED not to match a precinct.
source("scripts/model/live/R/vote_modes.R")

args = commandArgs(trailingOnly = TRUE)
STATE = toupper(c(args, "TX")[1])
st = tolower(STATE)
STATE_FIPS = c(AK = "02", AZ = "04", CA = "06", FL = "12", GA = "13", IA = "19", MI = "26", MN = "27",
               NC = "37", NE = "31", NJ = "34", NM = "35", NY = "36", OH = "39", PA = "42", TX = "48",
               VA = "51", WA = "53", WI = "55")
if (!STATE %in% names(STATE_FIPS)) stop("Add ", STATE, "'s FIPS code to STATE_FIPS")
BOUNDARY_VINTAGE = "2026_general"

# ---- 0. find the shared 26_general folder ----------------------------------------
# Order: (1) environment variable CBS_26_GENERAL, (2) PATH_DROPBOX if already set
# in this R session (cbs-enr's globals.R), (3) known install locations.
# If none exist, set it once in ~/.Renviron:
#   CBS_26_GENERAL="/path/to/26_general"
find_26_general = function() {
  cands = c(
    Sys.getenv("CBS_26_GENERAL", unset = ""),
    if (exists("PATH_DROPBOX") && nzchar(PATH_DROPBOX)) file.path(PATH_DROPBOX, "26_general"),
    path.expand("~/Dropbox/2024 Election Results/26_general"),
    Sys.glob(path.expand("~/MIT Dropbox/*/CBS-MIT Election Data/26_general")),
    Sys.glob(path.expand("~/Library/CloudStorage/Dropbox*/*/CBS-MIT Election Data/26_general")),
    Sys.glob(path.expand("~/Dropbox*/*/CBS-MIT Election Data/26_general"))
  )
  cands = cands[nzchar(cands) & dir.exists(cands)]
  if (length(cands) == 0) {
    cli::cli_abort(c("Can't find the Dropbox 26_general folder.",
                     "i" = 'Add CBS_26_GENERAL="/full/path/to/26_general" to ~/.Renviron and restart R.'))
  }
  normalizePath(cands[1])
}
DIR_26 = find_26_general()
DIR_IN = file.path(DIR_26, "input_data")
# MODEL_INPUTS_OUT redirects the WRITES only; every read still comes from DIR_26. That
# makes a dry run possible: this script's outputs are shared artifacts in Dropbox that
# other people's work joins against, so "run it and see" is otherwise destructive.
DIR_OUT = Sys.getenv("MODEL_INPUTS_OUT", unset = file.path(DIR_IN, "model_inputs"))
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)
if (DIR_OUT != file.path(DIR_IN, "model_inputs"))
  cli::cli_alert_warning("MODEL_INPUTS_OUT set -- writing to {.path {DIR_OUT}}, \
                          NOT the shared model_inputs folder")

path_frame   = file.path(DIR_IN, "yougov_precinct_frame.parquet")
path_method  = file.path(DIR_IN, "yougov_precinct_frame_method.parquet")
path_xw      = file.path(DIR_IN, "crosswalks", glue("{st}.csv"))
path_priors  = file.path(DIR_IN, "priors.csv")
path_l2      = sort(Sys.glob(file.path(DIR_IN, "l2", glue("{st}_*_precincts.parquet"))), decreasing = TRUE)[1]
dir_live     = file.path(DIR_26, STATE, "clean")   # live/zero files, for the id round-trip check

for (p in c(path_frame, path_method, path_xw, path_priors, path_l2)) {
  if (is.na(p) || !file.exists(p)) cli::cli_abort("Missing input: {p}")
  if (file.size(p) == 0) cli::cli_abort("{p} is 0 bytes: in Dropbox, right-click input_data > Make available offline")
}
L2_AS_OF = as.Date(str_extract(basename(path_l2), "\\d{8}"), "%Y%m%d")

# everything printed below is also saved to TX_prep_report.txt
path_report = file.path(DIR_OUT, glue("{STATE}_prep_report.txt"))
report_con = file(path_report, open = "wt")
sink(report_con, split = TRUE)                       # printed tables: console + report
# cli messages: written once through the sink (so they reach console AND report), then muffled
log_handler = function(m) {
  msg = conditionMessage(m)
  cat(msg, if (!endsWith(msg, "\n")) "\n")
  invokeRestart("muffleMessage")
}
globalCallingHandlers(message = log_handler)
cat(glue("Prep run {format(Sys.time())} by {Sys.info()[['user']]}\n26_general: {DIR_26}\nL2 file: {basename(path_l2)} (as of {L2_AS_OF})\n\n"))

read_pq = function(p) as_tibble(nanoparquet::read_parquet(p))

# ---- precinct id normalization ------------------------------------------------------
# TWO rules, because the two sources are not the same kind of thing.
#
# norm_prec_id() is for ids we have to RECONCILE -- YouGov / raw state ids, where the
# padding is arbitrary and dropping it is what makes them join: trim, uppercase, drop
# leading zeros ("0007" -> "7", "001A" -> "1A"). Validated on Harris, Kaufman and Wood,
# all three TEXAS counties, where a precinct id is a zero-padded pure number.
norm_prec_id = function(x) {
  x = str_squish(toupper(as.character(x)))
  x = str_remove(x, "\\.0$")              # ids stored as numbers upstream ("860.0", FL ENR ids)
  x = str_remove(x, "^0+(?=\\d)")
  if_else(x == "", NA_character_, x)
}
# norm_enr_id() is for the LIVE FEED's own ids (`precinct_enr`). It does NOT drop leading
# zeros, and that is the whole point.
#
# WHY. An ENR id is the key the feed will send on the night. There is nothing to
# reconcile -- we already have it exactly right -- so normalizing it REWRITES the join
# key. Applying the TX-validated rule state-blind corrupted every compound id:
#     AK  09-115 -> 9-115    the 09 is HOUSE DISTRICT 9, not padding
#     NC  02-04  -> 2-04     the 02 is a ward number
#     NC  001    -> 1        the feed sends 001
# Measured 2026-10-26 on {ST}_latest.csv, exact join as cell_counts() does it: AK 312 of
# 402 precinct geos joined (77.6%), NC 2,005 of 2,625 (76.4%). Using the ENR id verbatim
# takes AK to 402 of 402 and NC to 2,616 of 2,625, with ZERO collisions and zero orphan
# cells. In votes that is 32,051 of AK's 164,303 in the governor contest -- 19.5% -- that
# production was silently dropping, on geos that look like precincts because they ARE
# precincts (ak_001_01-600, nc_001_035).
#
# Section 6's live-id round trip did not catch it because it applied norm_geo() to the
# LIVE side too (see the note there), so it compared the mangled id against the mangled
# id and reported 100.0% for AK while production joined 77.6%.
#
# Scope: this changes `geo` only where `precinct_enr` is populated -- AK 100%, NC 99.4%,
# FL 50.2%, MI 43.9%, TX 0.7%, CA 0%.
#
# VERIFIED ON ALL SIX STATES against their feed files, exact join, no transform either
# side. AK and NC file statewide ({ST}_latest.csv); FL/MI/TX/CA file PER COUNTY
# ({ST}_{County}_latest.csv), scored over the counties that have filed:
#     AK  77.6% -> 100.0%    (90 geos changed)
#     NC  76.4% ->  99.7%    (611)
#     FL  38.8% ->  39.9%    (67)
#     MI   0.9% ->   0.9%    (0 -- MI's ENR ids carry no leading zeros)
#     TX  92.2% ->  92.2%    (0 -- 0.7% populated, so it never took this path)
#     CA   0.0% ->   0.0%    (0 -- no ENR ids at all)
# ZERO regressions: no geo that joined before fails to join after, in any state.
#
# Read FL/MI/CA's rates as the warning they are: they are low for reasons that have
# NOTHING to do with this rule, and that is the larger problem. Do not read 39.9% as this
# fix underperforming.
#
# The `.0$` strip and the squish/uppercase are KEPT: those fix how a value was stored, not
# what the id is.
norm_enr_id = function(x) {
  x = str_squish(toupper(as.character(x)))
  x = str_remove(x, "\\.0$")              # "001.0" -> "001": a storage artifact, not padding
  if_else(x == "", NA_character_, x)
}
norm_geo = function(geo) {
  parts = str_match(as.character(geo), "^([a-z]{2}_\\d{3})_(.*)$")
  if_else(is.na(parts[, 1]), as.character(geo), paste0(parts[, 2], "_", norm_prec_id(parts[, 3])))
}

# county names differ across sources ("MIAMI-DADE" / "MiamiDade", "ALEUTIANS EAST BOROUGH" /
# "ALEUTIANS EAST", "ST. LUCIE" / "Saint Lucie"): match on a key of letters only, without
# type words
county_key = function(x) {
  x = str_replace(toupper(as.character(x)), "^SAINT ", "ST ")
  str_remove_all(x, "\\b(COUNTY|BOROUGH|CENSUS AREA|CITY AND|MUNICIPALITY|OF|PARISH)\\b|[^A-Z]")
}
cty = enightmodels::cbs_counties |>
  filter(state == STATE) |>
  transmute(ckey = county_key(county), county_cbs = str_pad(county_cbs, 3, pad = "0"), geostratum)
if (STATE != "AK" && anyDuplicated(cty$ckey)) cli::cli_abort("Two CBS counties share a name key in {STATE}")

# ========================================
## 1. YouGov frame: precinct x CD pieces -> one row per precinct
# ========================================
rate_cols = c("turnout20", "turnout22", "turnout24", "turnoutPred", "male",
              "pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")

raw = read_pq(path_frame) |>
  filter(state == STATE) |>
  mutate(county_name = str_squish(toupper(county_name)),
         cd = if_else(str_detect(cd, "-AL$"), 1L, as.integer(str_extract(cd, "\\d+$"))))
cli::cli_h1("YouGov frame: {nrow(raw)} pieces, {n_distinct(raw$county_name)} counties")

stopifnot(
  "rate columns must be in [0, 1]" = all(between(unlist(raw[rate_cols]), 0, 1), na.rm = TRUE),
  "n must be positive" = all(raw$n > 0)
)
na_id = filter(raw, is.na(precinct26_id))
cli::cli_alert_warning("Dropping {nrow(na_id)} pieces with no precinct id ({format(sum(na_id$n), big.mark=',')} registrants)")
raw = filter(raw, !is.na(precinct26_id))
dup = count(raw, precinct26_code, cd) |> filter(n > 1)
if (nrow(dup) > 0) cli::cli_abort("{nrow(dup)} duplicate precinct x CD rows in the frame")

pieces = raw |>
  mutate(n_prec = sum(n), piece_share = n / n_prec, n_cd = n_distinct(cd), .by = precinct26_code)
cli::cli_inform(c(
  "Precincts split across CDs: {n_distinct(pieces$precinct26_code[pieces$n_cd > 1])} of {n_distinct(pieces$precinct26_code)}",
  "Pieces under 1% of their precinct: {sum(pieces$piece_share < 0.01)} ({format(sum(pieces$n[pieces$piece_share < 0.01]), big.mark=',')} registrants)"
))

# 2024 precinct RETURNS (voter file prec_*), if create_frame.R included them
prec_cols = intersect(c("prec_harris2p", "prec_harris", "prec_trump", "prec_other"), names(pieces))
wmean_na = function(x, w) { ok = !is.na(x) & w > 0; if (any(ok)) sum(x[ok] * w[ok]) / sum(w[ok]) else NA_real_ }

prec = pieces |>
  mutate(w_vote = n * coalesce(turnout24, turnoutPred) * !is.na(pr_harris),
         w_vote_all = n * coalesce(turnout24, turnoutPred)) |>
  summarize(
    county_name = first(county_name),
    precinct26_id = first(precinct26_id),
    across(all_of(rate_cols), \(x) weighted.mean(x, n, na.rm = TRUE)),
    # 2024 vote shares: weight pieces by their 2024 voters (pr_* is NA on non-voters)
    pr_harris = if (sum(w_vote) > 0) sum(w_vote * coalesce(pr_harris, 0)) / sum(w_vote) else NA_real_,
    pr_trump  = if (sum(w_vote) > 0) sum(w_vote * coalesce(pr_trump, 0)) / sum(w_vote) else NA_real_,
    across(all_of(prec_cols), \(x) wmean_na(x, w_vote_all)),
    cd_main = cd[which.max(n)],
    cd_main_share = max(n) / sum(n),
    n_cd = n_distinct(cd),
    n = sum(n),
    .by = precinct26_code
  ) |>
  # keep YouGov's turnout rates apart from L2's (same names otherwise)
  rename(yg_turnout20 = turnout20, yg_turnout22 = turnout22, yg_turnout24 = turnout24)

# ========================================
## 2. crosswalk: YouGov id -> L2 id and ENR (live) id
# ========================================
xw = read_csv(path_xw, col_types = cols(.default = "c")) |>
  filter(state == STATE, !is.na(precinct_yougov)) |>
  transmute(county_name = str_squish(toupper(county)), precinct26_id = str_remove(precinct_yougov, "\\.0$"),
            precinct_l2 = str_squish(precinct_l2), precinct_enr = str_squish(precinct_enr), l2_rule) |>
  distinct()
# MI's crosswalk holds the full YouGov code (ids repeat across municipalities); everywhere
# else the id. Join on whichever it is, and build the yougov geo id from the same thing.
XW_BY_CODE = mean(xw$precinct26_id %in% prec$precinct26_code) > 0.5
prec = prec |>
  mutate(precinct26_id = if (XW_BY_CODE) precinct26_code else str_remove(precinct26_id, "\\.0$"),
         precinct_local = if (XW_BY_CODE) str_remove(precinct26_code, paste0("^", STATE, "_[^_]+_")) else precinct26_id)
cli::cli_alert_info("Crosswalk joined on the YouGov precinct {if (XW_BY_CODE) 'code' else 'id'}")
if (anyDuplicated(xw[c("county_name", "precinct26_id")])) cli::cli_abort("Crosswalk maps a YouGov precinct to more than one id")
no_xw = anti_join(prec, xw, by = c("county_name", "precinct26_id"))
if (nrow(no_xw) > 0) cli::cli_abort("{nrow(no_xw)} frame precincts are not in the crosswalk")

prec = prec |> left_join(xw, by = c("county_name", "precinct26_id"))
if (STATE == "AK") {
  # CBS reports Alaska by state house election district: the SHnn in the YouGov code
  prec = prec |>
    mutate(county_cbs = str_pad(str_match(precinct26_code, "^AK_SH(\\d+)_")[, 2], 3, pad = "0")) |>
    left_join(select(cty, county_cbs, geostratum), by = "county_cbs") |>
    mutate(county_fips = paste0("02", county_cbs))
} else {
  prec = prec |> mutate(ckey = county_key(county_name)) |> left_join(cty, by = "ckey") |> select(-ckey)
}
if (anyNA(prec$county_cbs) || anyNA(prec$geostratum))
  cli::cli_abort("Counties not in cbs_counties: {unique(prec$county_name[is.na(prec$county_cbs) | is.na(prec$geostratum)])}")

# geo: the live id when the crosswalk has it, else the normalized YouGov id
prec = prec |>
  mutate(
    geo_yougov = paste(st, county_cbs, norm_prec_id(precinct_local), sep = "_"),
    # VERBATIM for the live id (norm_enr_id), reconciled for the YouGov one. See the
    # note at norm_enr_id: normalizing an ENR id rewrote the key the feed sends.
    geo = if_else(!is.na(precinct_enr), paste(st, county_cbs, norm_enr_id(precinct_enr), sep = "_"), geo_yougov)
  )
if (anyDuplicated(prec$geo)) cli::cli_abort("Duplicate geo ids: {head(prec$geo[duplicated(prec$geo)])}")
cli::cli_h2("Crosswalk")
cli::cli_inform(c(
  "Precincts with an L2 id: {sum(!is.na(prec$precinct_l2))} of {nrow(prec)}",
  "Precincts with a live (ENR) id: {sum(!is.na(prec$precinct_enr))} -- counties: {sort(unique(prec$county_name[!is.na(prec$precinct_enr)]))}",
  "ENR id differs from normalized YouGov id: {sum(prec$geo != prec$geo_yougov)}"
))
print(count(prec, l2_rule))

# ========================================
## 3. L2 covariates (joined through the crosswalk's L2 id, never directly)
# ========================================
l2 = read_pq(path_l2) |>
  filter(state == STATE) |>
  mutate(county_name = str_squish(toupper(county)),
         precinct_l2 = str_squish(coalesce(precinct, str_extract(precinct_cbs, "(?<=_).+$"))),
         county_fips = paste0(STATE_FIPS[[STATE]], str_pad(FIPS, 3, pad = "0")))
# L2's county names can differ from the frame's (AK: "ALEUTIANS EAST" vs "ALEUTIANS EAST
# BOROUGH"): use the frame's name for each L2 county, matched by name key
l2 = l2 |>
  mutate(ckey = county_key(county_name)) |>
  left_join(distinct(prec, frame_county = county_name) |> mutate(ckey = county_key(frame_county)), by = "ckey") |>
  mutate(county_name = coalesce(frame_county, county_name)) |>
  select(-ckey, -frame_county)
# AK: a few precincts sit in a different borough in L2 (crosswalk l2_rule
# paren_code_xcounty). Their L2 names carry the district code, so they are unique
# statewide: move them to the frame's borough so the county + precinct join finds them.
if (STATE == "AK") {
  uniq = l2 |> filter(!is.na(precinct_l2), precinct_l2 != "") |> add_count(precinct_l2, name = "k") |>
    filter(k == 1) |> select(precinct_l2, l2_county = county_name)
  moved = prec |> filter(!is.na(precinct_l2)) |> distinct(county_name, precinct_l2) |>
    inner_join(uniq, by = "precinct_l2") |> filter(l2_county != county_name)
  if (nrow(moved)) {
    cli::cli_alert_info("AK: {nrow(moved)} precincts are in a different borough in L2; matched on the precinct name alone")
    l2 = l2 |> left_join(select(moved, precinct_l2, new_county = county_name), by = "precinct_l2") |>
      mutate(county_name = coalesce(new_county, county_name)) |> select(-new_county)
  }
}
l2_nocty = filter(l2, is.na(precinct_l2) | precinct_l2 == "")
cli::cli_h2("L2 ({basename(path_l2)}): {nrow(l2)} rows, {n_distinct(l2$county_name)} counties")
cli::cli_alert_info("{nrow(l2_nocty)} county-level rows with no precinct ({format(sum(l2_nocty$n), big.mark=',')} registrants) are not used")
l2 = filter(l2, !is.na(precinct_l2), precinct_l2 != "")
if (anyDuplicated(l2[c("county_name", "precinct_l2")])) cli::cli_abort("Duplicate county x precinct ids in L2")

fips = distinct(l2, county_name, county_fips)
if (STATE == "AK") fips = fips[0, ]    # AK's county_fips is the election district, set above
miss_l2 = anti_join(filter(prec, !is.na(precinct_l2)), l2, by = c("county_name", "precinct_l2"))
if (nrow(miss_l2) > 0) cli::cli_alert_warning("{nrow(miss_l2)} crosswalk L2 ids are not in this L2 file (different vintage?): {head(paste(miss_l2$county_name, miss_l2$precinct_l2))}")

l2_cov = l2 |>
  transmute(
    county_name, precinct_l2,
    l2_n = n, active, new,
    p_male = male / n,
    # L2 age1-age4 = 18-29 / 30-44 / 45-64 / 65+ (enightmodels::rename_demo_cbs)
    p_age_18_29 = age1 / n, p_age_30_44 = age2 / n, p_age_45_64 = age3 / n, p_age_65_up = age4 / n,
    # race1-race4 = white / black / hispanic / other; race_unk is left out, so these sum to < 1
    p_white = race1 / n, p_black = race2 / n, p_hisp = race3 / n, p_other = race4 / n,
    # college share among voters with KNOWN education (L2 noncol includes unknowns)
    p_col = if_else(col_known + noncol_known > 0, col_known / (col_known + noncol_known), NA_real_),
    p_white_col = race1_col / n, p_white_noncol = race1_noncol / n,
    p_black_col = race2_col / n, p_black_noncol = race2_noncol / n,
    p_hisp_col = race3_col / n,  p_hisp_noncol = race3_noncol / n,
    # past general-election turnout of current registrants: RATES in [0,1]
    turnout18 = vote18 / n, turnout20 = vote20 / n, turnout22 = vote22 / n, turnout24 = vote24 / n
  )

if (!"county_fips" %in% names(prec)) prec = left_join(prec, fips, by = "county_name")
prec = prec |>
  left_join(l2_cov, by = c("county_name", "precinct_l2")) |>
  mutate(l2_matched = !is.na(l2_n))
cli::cli_inform(c(
  "Frame precincts with L2 covariates: {sum(prec$l2_matched)} of {nrow(prec)} ({round(100*mean(prec$l2_matched), 1)}%)",
  "Registrants in precincts without L2: {format(sum(prec$n[!prec$l2_matched]), big.mark=',')}"
))
prec |>
  summarize(precincts = n(), l2_match = round(mean(l2_matched), 3), registrants_no_l2 = sum(n[!l2_matched]), .by = county_name) |>
  filter(l2_match < 0.99) |>
  arrange(l2_match) |>
  print(n = 40)
# Fill covariates for precincts with no L2 match: the county's registrant-weighted
# mean (statewide if the county has none). The model can't take blank covariates
# (lm_bayes misaligns rows with NAs), so every row gets a value and l2_imputed flags
# which ones are filled. Counts (active, new) are filled as county rate x n.
l2_share_cols = c("p_male", "p_age_18_29", "p_age_30_44", "p_age_45_64", "p_age_65_up",
                  "p_white", "p_black", "p_hisp", "p_other", "p_col",
                  "p_white_col", "p_white_noncol", "p_black_col", "p_black_noncol", "p_hisp_col", "p_hisp_noncol",
                  "turnout18", "turnout20", "turnout22", "turnout24")
fill_mean = function(x, w) coalesce(x, wmean_na(x, w))
prec = prec |>
  mutate(l2_imputed = !l2_matched | if_any(all_of(l2_share_cols), is.na),
         active_rate = active / l2_n, new_rate = new / l2_n) |>
  mutate(across(c(all_of(l2_share_cols), active_rate, new_rate), \(x) fill_mean(x, n)), .by = county_name) |>
  mutate(across(c(all_of(l2_share_cols), active_rate, new_rate), \(x) fill_mean(x, n))) |>
  mutate(active = coalesce(active, round(active_rate * n)), new = coalesce(new, round(new_rate * n))) |>
  select(-active_rate, -new_rate)
cli::cli_alert_info("L2 covariates filled from county averages for {sum(prec$l2_imputed)} precincts ({format(sum(prec$n[prec$l2_imputed]), big.mark=',')} registrants); flagged l2_imputed")

chk = filter(prec, l2_matched)
cli::cli_inform(c(
  "Registrants, matched precincts: YouGov {format(sum(chk$n), big.mark=',')} vs L2 {format(sum(chk$l2_n), big.mark=',')}; correlation {round(cor(chk$n, chk$l2_n), 3)}",
  # a matched pair should be the same place: registrant counts within 25% of each other
  "Matched precincts whose L2 and YouGov registrant counts differ by more than 25%: {sum(abs(chk$l2_n / chk$n - 1) > 0.25)} (check these in the crosswalk if the number grows)"
))

# ========================================
## 4. baselines: est_turnout, dem2, oth_base, strata
# ========================================
# dem2 / oth_base come from the YouGov 2024 vote model (pr_harris / pr_trump, averaged
# over 2024 voters). pr_harris + pr_trump + pr_other = 1 for each voter, so
# 1 - pr_harris - pr_trump is the precinct's mean pr_other.
# If create_frame.R includes the precinct RETURNS (prec_harris2p / prec_harris /
# prec_trump / prec_other: each voter's 2024 precinct result, averaged over the 2026
# precinct's 2024 voters), those are used instead -- data contract §11.1. The model
# version is kept as dem2_model either way, and the two are compared below.
use_returns = all(c("prec_harris", "prec_trump", "prec_other") %in% prec_cols)
DEM2_SOURCE = if (use_returns) "2024 precinct returns (voter file prec_*)" else "yougov pr_harris/pr_trump (2024 vote model)"

prec = prec |>
  mutate(
    vote_imputed = is.na(pr_harris) | (pr_harris + pr_trump) <= 0,
    w = n * yg_turnout24
  ) |>
  mutate(
    # precincts with no modeled 2024 voters: county average, weighted by 2024 voters
    pr_harris = if_else(vote_imputed, weighted.mean(pr_harris[!vote_imputed], w[!vote_imputed]), pr_harris),
    pr_trump  = if_else(vote_imputed, weighted.mean(pr_trump[!vote_imputed], w[!vote_imputed]), pr_trump),
    .by = county_name
  ) |>
  mutate(dem2_model = pr_harris / (pr_harris + pr_trump), oth_model = 1 - pr_harris - pr_trump) |>
  select(-w)
if (use_returns) {
  prec = prec |>
    mutate(
      ret_two = if ("prec_harris2p" %in% names(prec)) prec_harris2p else prec_harris / (prec_harris + prec_trump),
      returns_imputed = is.na(ret_two) | is.na(prec_other),
      w = n * yg_turnout24
    ) |>
    # precincts the voter file couldn't place in a 2024 precinct: county average
    mutate(ret_two = fill_mean(ret_two, w), prec_other = fill_mean(prec_other, w), .by = county_name) |>
    mutate(dem2_raw = ret_two, oth_raw = prec_other) |>
    select(-w, -ret_two)
  ok = !prec$returns_imputed & !prec$vote_imputed
  cli::cli_inform(c(
    "Precincts with returns filled from county average: {sum(prec$returns_imputed)}",
    "Check (contract 14.4): corr(logit dem2 returns, logit dem2 model) = {round(cor(qlogis(pmin(pmax(prec$dem2_raw[ok], .001), .999)), qlogis(pmin(pmax(prec$dem2_model[ok], .001), .999))), 3)}"
  ))
} else {
  prec = mutate(prec, dem2_raw = dem2_model, oth_raw = oth_model, returns_imputed = NA)
}
prec = prec |>
  mutate(
    # the contract requires both strictly inside (0, 1)
    dem2 = pmin(pmax(dem2_raw, 0.005), 0.995),
    oth_base = pmin(pmax(oth_raw, 0.001), 0.5),
    # expected 2026 votes: vendor midterm turnout score x registrants, capped at n
    est_turnout = pmin(turnoutPred * n, n),
    stratum_demo = case_when(
      pred.whi > 0.5 ~ "white",
      pred.his > 0.5 ~ "hisp",
      pred.bla > 0.5 ~ "black",
      pred.asi > 0.5 ~ "asian",
      .default = "mixed"
    )
  )

cli::cli_h2("Baselines")
cli::cli_inform(c(
  "dem2 source: {DEM2_SOURCE}",
  "Precincts with dem2 imputed from county average: {sum(prec$vote_imputed)} ({format(sum(prec$n[prec$vote_imputed]), big.mark=',')} registrants)",
  "dem2 clamped to [0.005, 0.995]: {sum(prec$dem2 != prec$dem2_raw)}; oth_base clamped to [0.001, 0.5]: {sum(prec$oth_base != prec$oth_raw)}",
  "Expected turnout: {format(round(sum(prec$est_turnout)), big.mark=',')} votes; Dem two-party baseline {round(weighted.mean(prec$dem2, prec$est_turnout), 3)}; third-party baseline {round(weighted.mean(prec$oth_base, prec$est_turnout), 4)}",
  "Cross-check: L2 2022 voters among current registrants, matched precincts: {format(sum(prec$turnout22 * prec$l2_n, na.rm = TRUE), big.mark=',')}"
))
print(count(prec, stratum_demo, wt = n, name = "registrants") |> mutate(share = round(registrants / sum(registrants), 3)))

# ---- validation (data contract §2) ----
p_fams = list(age = c("p_age_18_29", "p_age_30_44", "p_age_45_64", "p_age_65_up"),
              race = c("p_white", "p_black", "p_hisp", "p_other"),
              race_edu = c("p_white_col", "p_white_noncol", "p_black_col", "p_black_noncol", "p_hisp_col", "p_hisp_noncol"))
for (nm in names(p_fams)) {
  s = rowSums(prec[p_fams[[nm]]])
  if (any(s > 1 + 1e-6, na.rm = TRUE)) cli::cli_abort("p_{nm} shares sum above 1 in {sum(s > 1 + 1e-6, na.rm = TRUE)} precincts")
}
tcols = c("turnout18", "turnout20", "turnout22", "turnout24")
stopifnot(
  "turnout* must be in [0,1]" = all(between(unlist(prec[tcols]), 0, 1), na.rm = TRUE),
  "est_turnout must be > 0" = all(prec$est_turnout > 0),
  "dem2 must be inside (0,1)" = all(prec$dem2 > 0 & prec$dem2 < 1),
  "oth_base must be inside [0,1)" = all(prec$oth_base >= 0 & prec$oth_base < 1)
)
cli::cli_alert_success("Validation passed (share families <= 1, rates in [0,1], est_turnout > 0, dem2 and oth_base interior)")

# ========================================
## 5. mode turnout history (data contract §3): L2 voteYY_{mail,early,eday}
# ========================================
elections = tibble(yy = c("16", "18", "20", "22", "24")) |>
  mutate(election = paste0("20", yy, "_general"), is_midterm = yy %in% c("18", "22"))
mode_cols = as.vector(outer(paste0("vote", elections$yy), c("mail", "early", "eday"), paste, sep = "_"))

mode_hist = prec |>
  filter(l2_matched) |>
  select(geo, county_name, precinct_l2) |>
  inner_join(select(l2, county_name, precinct_l2, all_of(mode_cols)), by = c("county_name", "precinct_l2")) |>
  select(-county_name, -precinct_l2) |>
  pivot_longer(-geo, names_to = c("yy", "method"), names_pattern = "^vote(\\d{2})_(mail|early|eday)$", values_to = "voters") |>
  left_join(elections, by = "yy") |>
  transmute(geo, election, method, voters = as.integer(voters), is_midterm)

# ---- OFFICIAL county x mode composition, where L2's split is known WRONG ------------
# L2's mode columns are not uniformly trustworthy. For NC they are wrong in three of five
# elections, including 2022 -- the one the model uses, because 2026 is a midterm:
#
#   NC 2022      mail    early    eday
#   L2           0.579   0.000    0.421
#   OFFICIAL     0.049   0.531    0.416
#
# Note WHAT is wrong: L2's election-day share is almost exactly right and its per-precinct
# TOTAL is sound (3,437,858 across 2,619 precincts against an actual ~3.78M). The whole
# error is that early's 53% was coded into mail. So this replaces the SPLIT ONLY and keeps
# every precinct's L2 total -- it is a reallocation, not a new vote count, and the state's
# total voters are unchanged by construction (asserted below).
#
# Applied per state from lookups/mode_composition.csv where `replaces_l2` is TRUE, which
# today is NC alone: AK, CA and MI were checked against the same official returns and
# their L2 splits agree to within ~0.01 (see make_mode_composition.R's header for the
# numbers). Do not widen this without that measurement for the state in question.
#
# County grain, applied to every precinct in the county, because the official source joins
# at county level for all 100 NC counties but only 17% at precinct level (CONTRACT.md's
# priority-states table). That is a smaller concession than it sounds: the real variation
# is mostly BETWEEN counties (NC eday 0.269-0.721, median 0.428), and both
# cells_from_inputs() and derive_mode_priors() already shrink a precinct toward its
# county, so a county split is what they would mostly end up using anyway.
P_MODE_COMP = "scripts/model/lookups/mode_composition.csv"
comp = if (!file.exists(P_MODE_COMP)) NULL else
  readr::read_csv(P_MODE_COMP, show_col_types = FALSE, progress = FALSE) |>
    filter(state == STATE, replaces_l2)
if (!is.null(comp) && nrow(comp)) {
  el_comp = paste0(unique(comp$year), "_general")
  if (length(el_comp) != 1) cli::cli_abort("mode_composition.csv has >1 year for {STATE}")
  if (!el_comp %in% mode_hist$election)
    cli::cli_abort("mode_composition.csv covers {el_comp}, which is not in the L2 history")
  sh = comp |> transmute(ckey = county_key(county_name), method, share)
  before_tot = sum(mode_hist$voters[mode_hist$election == el_comp], na.rm = TRUE)
  keep = filter(mode_hist, election != el_comp)
  old  = filter(mode_hist, election == el_comp)
  im   = old$is_midterm[1]
  # each precinct's L2 TOTAL for that election, re-split by its county's official shares
  pt = old |> summarize(tot = sum(voters, na.rm = TRUE), .by = geo) |>
    left_join(select(prec, geo, county_name), by = "geo") |>
    mutate(ckey = county_key(county_name))
  miss = pt |> filter(!ckey %in% sh$ckey)
  if (nrow(miss))
    cli::cli_alert_warning("{nrow(miss)} {STATE} precinct(s) in {nrow(distinct(miss, ckey))} \
                            county(ies) are absent from mode_composition.csv -- they KEEP \
                            their L2 split: {.val {head(unique(miss$county_name), 4)}}")
  redone = pt |>
    inner_join(sh, by = "ckey", relationship = "many-to-many") |>
    transmute(geo, election = el_comp, method,
              voters = as.integer(round(tot * share)), is_midterm = im)
  mode_hist = bind_rows(keep, filter(old, !geo %in% redone$geo), redone)
  after_tot = sum(mode_hist$voters[mode_hist$election == el_comp], na.rm = TRUE)
  # a reallocation must not move the total. Integer rounding per cell is the only slack.
  if (abs(after_tot - before_tot) > nrow(redone))
    cli::cli_abort("re-splitting {el_comp} moved {STATE}'s total from \
                    {before_tot} to {after_tot} -- that is a reallocation bug, not rounding")
  newsh = redone |> summarize(v = sum(voters), .by = method) |> mutate(sh = v / sum(v))
  cli::cli_alert_success("{STATE} {el_comp} mode split replaced from official returns on \
                          {n_distinct(redone$geo)} precinct(s): \
                          {paste(sprintf('%s %.3f', newsh$method, newsh$sh), collapse = '  ')} \
                          (total {format(after_tot, big.mark = ',')}, was \
                          {format(before_tot, big.mark = ',')})")
  # The model reads ONE election (MODE_ELECTION / mode_election, default 2022_general).
  # The other elections still carry L2's numbers, and for NC 2020 and 2024 those are also
  # wrong -- so say so here rather than let a future backtest discover it.
  cli::cli_alert_warning("only {el_comp} was replaced; {STATE}'s other elections still \
                          carry L2's split. Do not point mode_election at them without \
                          checking it first.")
}

cli::cli_h2("Mode history ({n_distinct(mode_hist$geo)} precincts)")
mode_tot = l2 |>
  semi_join(filter(prec, l2_matched), by = c("county_name", "precinct_l2")) |>
  summarize(across(paste0("vote", elections$yy), sum)) |>
  pivot_longer(everything(), names_to = "yy", values_to = "all_voters") |>
  mutate(yy = str_remove(yy, "vote"), election = paste0("20", yy, "_general"))
mode_hist |>
  summarize(voters = sum(voters), .by = c(election, method)) |>
  mutate(share = round(voters / sum(voters), 4), .by = election) |>
  select(-voters) |>
  pivot_wider(names_from = method, values_from = share) |>
  left_join(
    mode_hist |> summarize(mode_voters = sum(voters), .by = election) |>
      left_join(mode_tot, by = "election") |>
      transmute(election, pct_with_mode = round(100 * mode_voters / all_voters, 1)),
    by = "election") |>
  print()
cli::cli_inform("These count CURRENT registrants who voted, so they are survivorship-biased (people who died or moved are missing).")

# cross-check 2024 against the YouGov method frame (same idea, different voter file)
yg24 = read_pq(path_method) |>
  filter(state == STATE, method %in% c("mail", "early", "election_day")) |>
  summarize(n = sum(n), .by = method) |>
  mutate(share = round(n / sum(n), 4))
cli::cli_inform("2024 mode shares in the YouGov method frame (same counties): {paste(yg24$method, yg24$share, collapse = ', ')}")

# ========================================
## 6. live-id round trip (data contract §14.1): do live/zero-file ids join to geo?
# ========================================
# BOTH filename shapes. AK and NC file STATEWIDE as {ST}_latest.parquet; CA/FL/MI/TX file
# PER COUNTY as {ST}_{County}_latest.parquet. The glob used to cover only the per-county
# shape, so this entire check was silently SKIPPED for the two statewide states -- which
# are the two with county/site reporting, i.e. the ones it most needed to run on.
live_files = unique(c(
  Sys.glob(file.path(dir_live, glue("{STATE}_latest.parquet"))),
  Sys.glob(file.path(dir_live, glue("{STATE}_*_latest.parquet")))))
live_files = live_files[file.size(live_files) > 0]
cli::cli_h2("Live id check ({length(live_files)} files in {STATE}/clean)")
if (length(live_files) > 0) {
  # COMPARE THE IDS AS PRODUCTION DOES -- raw, exact. cell_counts() joins on `geo`
  # verbatim with no crosswalk, so any transform applied here that production does not
  # apply makes this check LIE. It used to call norm_geo() on the live side, which is the
  # same transform the frame's own geo had already been through, so it compared a mangled
  # id against a mangled id: it reported 100.0% for AK while production joined 77.6% and
  # dropped 19.5% of the state's votes. A check that normalizes both sides cannot fail.
  # `geo_norm` is kept alongside purely to SHOW how much of any gap normalization would
  # close -- it is a diagnostic, never the pass criterion.
  live = map(live_files, \(f) read_pq(f) |> distinct(geo) |> mutate(file = basename(f))) |>
    bind_rows() |>
    mutate(geo = as.character(geo), geo_norm = norm_geo(geo),
           county_cbs = str_extract(geo, "(?<=^[a-z]{2}_)\\d{3}"))
  live |>
    summarize(live_ids = n(),
              live_in_frame = round(mean(geo %in% prec$geo), 3),          # what production gets
              if_normalized = round(mean(geo_norm %in% prec$geo), 3),     # diagnostic only
              .by = c(file, county_cbs)) |>
    left_join(prec |> summarize(frame_ids = n(), frame_in_live = round(mean(geo %in% live$geo), 3), .by = county_cbs), by = "county_cbs") |>
    print()
  # Buckets are SUPPOSED not to join (they are county/site reports, not precincts), so
  # they must not be counted against the rate -- see live/R/bucket_counts().
  lp = filter(live, is.na(bucket_class(str_replace(geo, "^[a-z]{2}_[0-9]+_", ""))))
  rate = if (nrow(lp)) mean(lp$geo %in% prec$geo) else NA_real_
  cli::cli_alert_info("live PRECINCT ids joining geo exactly: {round(100 * rate, 1)}% \\
                       ({sum(lp$geo %in% prec$geo)} of {nrow(lp)}); \\
                       {sum(!is.na(bucket_class(str_replace(live$geo, '^[a-z]{2}_[0-9]+_', ''))))} \\
                       bucket geo(s) excluded")
  if (!is.na(rate) && rate < 0.99)
    cli::cli_alert_danger("Only {round(100 * rate, 1)}% of live precinct ids join. \\
                           cell_counts() will DROP the rest with no counter. Fix the id \\
                           rule or add crosswalk entries before the night.")
} else {
  cli::cli_alert_info("No readable *_latest.parquet files (make TX/clean available offline to run this check)")
}

# ========================================
## 7. write
# ========================================
out = prec |>
  transmute(
    # §1 spine
    geo, state = STATE, county_name, county_fips, county_cbs,
    precinct_raw = precinct26_id, boundary_vintage = BOUNDARY_VINTAGE,
    geostratum = as.character(geostratum), stratum_demo,
    # ids for joins
    precinct26_code, precinct_l2, precinct_enr, l2_rule,
    # §2 covariates
    as_of = L2_AS_OF,
    n = as.integer(n), active = as.integer(active), new = as.integer(new),
    p_male, p_age_18_29, p_age_30_44, p_age_45_64, p_age_65_up,
    p_white, p_black, p_hisp, p_other, p_col,
    p_white_col, p_white_noncol, p_black_col, p_black_noncol, p_hisp_col, p_hisp_noncol,
    turnout18, turnout20, turnout22, turnout24,
    est_turnout, dem2, oth_base,
    # extras: YouGov values (complete for every precinct) and flags
    yg_turnoutPred = turnoutPred, yg_turnout20, yg_turnout22, yg_turnout24,
    yg_pred_whi = pred.whi, yg_pred_bla = pred.bla, yg_pred_his = pred.his, yg_pred_asi = pred.asi,
    cd_main, cd_main_share, n_cd,
    dem2_model, oth_model,
    l2_n = as.integer(l2_n), l2_matched, l2_imputed, vote_imputed, returns_imputed, dem2_source = DEM2_SOURCE
  )
stopifnot(!anyDuplicated(out$geo))
nanoparquet::write_parquet(out, file.path(DIR_OUT, glue("{STATE}_precinct.parquet")))
nanoparquet::write_parquet(mode_hist, file.path(DIR_OUT, glue("{STATE}_precinct_mode_history.parquet")))

out_cd = pieces |>
  inner_join(select(prec, precinct26_code, geo), by = "precinct26_code") |>
  transmute(geo, cd, n_piece = n, share = piece_share)
stopifnot(all(abs(summarize(out_cd, s = sum(share), .by = geo)$s - 1) < 1e-9))
nanoparquet::write_parquet(out_cd, file.path(DIR_OUT, glue("{STATE}_precinct_cd.parquet")))

# every county in the state (not just the scraped ones): FIPS <-> CBS county number, by
# name. cbs_county_pull.R uses this to place CBS's county rows; FIPS order is not safe
# (FL's Miami-Dade). AK: the 40 election districts, FIPS 02 + district number.
counties = if (STATE == "AK") {
  enightmodels::cbs_counties |> filter(state == "AK") |>
    transmute(county_fips = paste0("02", str_pad(county_cbs, 3, pad = "0")),
              county_cbs = str_pad(county_cbs, 3, pad = "0"), county_name = county)
} else {
  read_pq(path_l2) |> filter(state == STATE) |>
    distinct(county_fips = paste0(STATE_FIPS[[STATE]], str_pad(FIPS, 3, pad = "0")), county_name = str_squish(toupper(county))) |>
    filter(!is.na(county_name), county_name != "") |>
    mutate(ckey = county_key(county_name)) |>
    left_join(select(cty, ckey, county_cbs), by = "ckey") |>
    select(county_fips, county_cbs, county_name)
}
n_cbs = sum(enightmodels::cbs_counties$state == STATE)
if (anyNA(counties$county_cbs) || anyDuplicated(counties$county_cbs) || anyDuplicated(counties$county_fips) || nrow(counties) != n_cbs)
  cli::cli_abort(c("County lookup for {STATE} is not one-to-one with cbs_counties ({nrow(counties)} vs {n_cbs})",
                   "i" = "Unmatched: {head(counties$county_name[is.na(counties$county_cbs)], 10)}"))
chk = distinct(prec, county_fips, county_cbs) |> anti_join(counties, by = c("county_fips", "county_cbs"))
if (nrow(chk)) cli::cli_abort("{nrow(chk)} precinct counties disagree with the county lookup")
write_csv(counties, file.path(DIR_OUT, glue("{STATE}_counties.csv")))

priors = read_csv(path_priors, col_types = cols(.default = "c")) |>
  filter(state == STATE) |>
  transmute(race_id, prior_dem = as.numeric(prior_dem), prior_rep = as.numeric(prior_rep),
            prior_oth = NA_real_, prior_turnout = NA_real_,
            as_of = as.Date(file.mtime(path_priors)), source = "26_general/input_data/priors.csv")
write_csv(priors, file.path(DIR_OUT, glue("{STATE}_race_prior.csv")), na = "")

cli::cli_h2("Written to {DIR_OUT}")
cli::cli_inform(c(
  "{STATE}_precinct.parquet: {nrow(out)} precincts x {ncol(out)} columns",
  "{STATE}_precinct_mode_history.parquet: {nrow(mode_hist)} rows",
  "{STATE}_precinct_cd.parquet: {nrow(out_cd)} rows",
  "{STATE}_race_prior.csv: {nrow(priors)} races",
  "{STATE}_counties.csv: {nrow(counties)} counties (FIPS <-> CBS county number)",
  "{basename(path_report)}: this log"
))

sink()
close(report_con)
# remove the message handler so later messages in this R session behave normally
local({
  h = globalCallingHandlers()
  globalCallingHandlers(NULL)
  keep = !vapply(h, identical, logical(1), log_handler)
  if (any(keep)) do.call(globalCallingHandlers, h[keep])
})
