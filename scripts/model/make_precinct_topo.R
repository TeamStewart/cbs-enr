# ========================================
## Precinct geometry bridge: `geo` <-> votehub `unit_id` crosswalk, then topojson
##
## Workstream 4 of the 2026-10-26 plan. Two artifacts, built ONCE, OFFLINE:
##
##   1. scripts/model/lookups/precinct_geo_crosswalk.csv
##        (state, year, geo, unit_id, rule, county_fips, county_name)  -- reviewable,
##        keyed on (state, year) like `vote_mode_map.csv`, with the matching rule
##        recorded per row so a bad join is traceable to the rule that made it.
##   2. scripts/model/topo/{st}_precincts.topojson
##        simplified geometry, one file per state, with `geo` attached as a feature
##        property for the dashboard to join on.
##
## GEOMETRY IS STATIC FOR THE WHOLE NIGHT. Only predictions change. So this script is
## NOT part of the refresh loop -- run it once before the election and never again.
## Re-emitting geometry per refresh is the thing this design exists to prevent.
##
## SCOPE IS AK / FL / NC / TX, and that is a data limit, not a choice:
##   - CA has NO precinct geometry in Dropbox at any depth, and it is our largest state
##     (23,685 precincts). A CA precinct choropleth needs county-by-county acquisition.
##   - MI's geometry exists but joins at 0% on every column we carry. votehub's
##     `precinct_sos` is "Township of Addison, Precinct 1" against our
##     MI_{COUNTY}_{MUNI}_W{nn}_P{nnn}; that needs a name-construction crosswalk.
##   - NY is statewide-only on our side, so there is nothing to map.
## GA/IA/NE/OH/SC are in the GeoPackage but we model none of them.
##
## NO R SPATIAL DEPENDENCY. GDAL (`ogr2ogr`) reads the GeoPackage and mapshaper does the
## simplification, so this script never loads `sf`. `arrow` is NOT installed despite
## being in renv.lock -- the precinct frames are read with `nanoparquet`.
##
## Prerequisites:  ogr2ogr (GDAL) on PATH;  mapshaper on PATH (`npm i -g mapshaper`).
##
## Run:  Rscript scripts/model/make_precinct_topo.R
##       STATES=FL Rscript scripts/model/make_precinct_topo.R    # one state
##       SKIP_TOPO=1 Rscript scripts/model/make_precinct_topo.R  # crosswalk only
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
})

# ---- paths ----------------------------------------------------------------------
# PATH_DROPBOX env var if set, else this machine's Dropbox (live/R/paths.R)
PATH_DROPBOX <- local({ source("scripts/model/live/R/paths.R", local = TRUE); find_dropbox() })
P_IN   <- path.expand(file.path(PATH_DROPBOX, "26_general/input_data/model_inputs"))
P_GPKG <- path.expand(file.path(PATH_DROPBOX,
            "26_general/input_data/votehub-shapefiles/26_pct_bdrys.gpkg"))
GPKG_LAYER <- "precincts"

DIR_LOOKUP <- "scripts/model/lookups"
DIR_TOPO   <- "scripts/model/topo"
P_XWALK    <- file.path(DIR_LOOKUP, "precinct_geo_crosswalk.csv")
P_RATES    <- file.path(DIR_LOOKUP, "precinct_geo_match_rates.csv")

YEAR   <- 2026
STATES <- strsplit(Sys.getenv("STATES", unset = "AK,FL,NC,TX"), ",")[[1]]
SKIP_TOPO <- nzchar(Sys.getenv("SKIP_TOPO", unset = ""))

# ---- match-rate gates -----------------------------------------------------------
## TWO denominators, because they answer different questions and only one of them is
## a key-quality measure.
##
##   pct_ours       matched / our precincts.  What the map actually covers.
##   pct_attainable matched / min(our precincts, votehub features in that county).
##
## The second exists because several counties have FEWER votehub polygons than we have
## precincts, and no normalization can conjure a polygon that is not in the file. TX is
## the whole story here: `TX26P` is MARCH-PRIMARY vintage, and primary-consolidated
## counties are renumbered wholesale -- Bell has vh 101..120/201.. against our 0003..,
## El Paso has a sequential vh 001..236 against our real precinct numbers, Wood has 14
## polygons for our 68 precincts. Those are different precinct universes, not different
## spellings.
##
## So the GATE is on pct_attainable, which is the part we control, and pct_ours is
## reported loudly beside it. A county that fails the gate has a KEY problem worth
## fixing; a county with low pct_ours and high pct_attainable has a geometry-vintage
## problem that this script cannot fix and must not pretend to.
MIN_STATE_ATTAINABLE  <- 0.95
MIN_COUNTY_ATTAINABLE <- 0.80
## Below this, the state is reported but its topojson is NOT written: a map covering
## half a state's precincts is worse than no map, because the holes read as "no votes".
MIN_STATE_OURS        <- 0.90

## Simplification is expressed as a RESOLUTION IN METRES, not a percentage. A percentage
## is a quota on removable vertices, so the same number means a different ground
## resolution in every state and AK (pathological coastline: 402 features, 105 MB of
## full-precision GeoJSON) would come out far coarser than TX at the same setting.
## 100 m is generous for a choropleth read at state scale.
SIMPLIFY_INTERVAL <- Sys.getenv("SIMPLIFY_INTERVAL", unset = "100")
## Coordinate quantization, tried in order until the written file has ZERO null
## geometries. Quantization is per-file over the layer's bounding box, so a coarse
## setting COLLAPSES SMALL POLYGONS TO NOTHING -- and small-area precincts are the dense
## urban ones, so the holes land exactly where the votes are. Measured: 1e4 nulls 75 of
## 5,919 TX features and 15 of 402 AK features, for ~10% off the file size. AK is the
## worst case because its bounds straddle the antimeridian (-179.1 to 179.8), so the
## quantization grid is stretched over 360 degrees rather than the state's real width.
## Escalation is normally a single pass: 1e5 is clean for all four states.
QUANT_CANDIDATES <- strsplit(
  Sys.getenv("QUANTIZATION", unset = "1e5,1e6,1e7,no-quantization"), ",")[[1]]

dir.create(DIR_LOOKUP, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_TOPO,   showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(P_GPKG))
if (nchar(Sys.which("ogr2ogr")) == 0) cli_abort("ogr2ogr not on PATH (GDAL required).")
if (!SKIP_TOPO && nchar(Sys.which("mapshaper")) == 0)
  cli_abort("mapshaper not on PATH -- `npm i -g mapshaper`, or set SKIP_TOPO=1.")

# ---- key normalization ----------------------------------------------------------
## Three normalizers, each with a reason.
##
## `code4` is the verified recipe from `tx_backtest_panel.R` / `backtest_panel.R`
## (98.9% on TX 2022 L2<->votehub): upcase, take a TRAILING PARENTHETICAL if present,
## strip punctuation, zero-pad to 4. Padding both sides to a common width is what makes
## "01" and "1" meet; stripping leading zeros instead would be equivalent for numeric
## keys but breaks alphanumerics like NC's "03C".
##   - the parenthetical rule is for our `precinct_l2`, which stores several states as
##     "NAME (ABBREV)" while votehub stores the bare abbreviation (AK "WALES (39-950)",
##     NC "01 PATTERSON (01)"). votehub never uses parentheses, so applying it to both
##     sides is safe.
##   - the TRAILING ".0" strip is for FL Levy, whose votehub `unit_id` carries
##     spreadsheet decimal artifacts ("1.0", "10.0"). WITHOUT it the normalizer is
##     actively DANGEROUS, not merely weak: "1.0" strips to "10" and silently matches
##     OUR precinct 10. That is a wrong join, not a missing one, and it is the reason
##     Levy previously read 7.7% rather than 0%.
##
## `name_norm` is for the FL counties votehub keys by precinct NAME. No padding -- these
## are facility names, and padding would corrupt them.
##
## `lead_code` / `tail_tok` pull a code out of a mixed "code + name" string. votehub is
## inconsistent about which end the code sits on: AK Bethel has "38-824 Kipnuk" (code
## first), FL Baker has `precinct_sos` "Baxter 4A" (code last).
code4 <- function(x) {
  x <- toupper(trimws(as.character(x)))
  inner <- str_match(x, "\\(([^()]*)\\)\\s*$")[, 2]
  x <- ifelse(!is.na(inner) & nzchar(trimws(inner)), inner, x)
  x <- str_remove(x, "\\.0+$")
  out <- str_pad(str_remove_all(x, "[^0-9A-Z]"), 4, pad = "0")
  ifelse(is.na(x) | !nzchar(str_remove_all(x, "[^0-9A-Z]")), NA_character_, out)
}
name_norm <- function(x) {
  out <- str_remove_all(toupper(trimws(as.character(x))), "[^0-9A-Z]")
  ifelse(is.na(out) | !nzchar(out), NA_character_, out)
}
lead_code <- function(x) str_extract(trimws(as.character(x)), "^\\S+")
tail_tok  <- function(x) str_extract(trimws(as.character(x)), "\\S+$")
## `exact` exists because `code4` is LOSSY and two genuinely different precincts can
## collide under it. NC Buncombe carries both "68.1" and "681", and votehub carries both
## as separate polygons -- `code4` maps both to "0681" on both sides, so the uniqueness
## guard (correctly) drops all four and Buncombe loses two precincts it could have had.
## Literal equality resolves them, so it runs FIRST and lossy normalization only ever
## sees what literal matching could not place.
exact <- function(x) {
  out <- toupper(trimws(as.character(x)))
  ifelse(is.na(out) | !nzchar(out), NA_character_, out)
}
## name suffix of our `precinct_l2`: "4A-BAXTER" -> "BAXTER", "1A-SANDERSON" -> "SANDERSON"
l2_suffix <- function(x) {
  x <- as.character(x)
  ifelse(str_detect(x, "-"), str_remove(x, "^[^-]*-"), NA_character_)
}

# ---- the rule cascade -----------------------------------------------------------
## Ordered, strongest first. Each rule is tried only on the rows still unmatched, and
## only within a county (`county_fips` <-> votehub `fips`), so a rule can never pull a
## precinct across a county line. A rule contributes a pair ONLY if its key is unique on
## BOTH sides within that county -- an ambiguous key is dropped, never guessed, because
## a wrong polygon on air is worse than a grey one.
##
## One cascade serves all four states; the per-state winner simply fires first. That is
## deliberate: a per-state hardcoded key would hide the fact that AK/NC key on
## `precinct_enr` and FL/TX on `precinct_raw`, and it would have to be edited rather
## than merely re-run when a state's feed changes shape.
RULES <- list(
  # literal equality first, so `code4`'s lossiness never gets to collide two precincts
  # that are distinct in both feeds (NC Buncombe "68.1" vs "681").
  list(id = "enr_exact",     ours = function(d) exact(d$precinct_enr),
                             vh   = function(v) exact(v$vprec)),
  list(id = "raw_exact",     ours = function(d) exact(d$precinct_raw),
                             vh   = function(v) exact(v$vprec)),
  # AK, NC: our ENR precinct code against votehub's unit_id tail.
  list(id = "enr_code",      ours = function(d) code4(d$precinct_enr),
                             vh   = function(v) code4(v$vprec)),
  # FL, TX: our RAW precinct code. NC falls here for the 16 rows with precinct_enr = NA.
  list(id = "raw_code",      ours = function(d) code4(d$precinct_raw),
                             vh   = function(v) code4(v$vprec)),
  list(id = "l2_code",       ours = function(d) code4(d$precinct_l2),
                             vh   = function(v) code4(v$vprec)),
  # AK Bethel: votehub appends the place name to the code ("38-824 Kipnuk").
  list(id = "enr_code_lead", ours = function(d) code4(d$precinct_enr),
                             vh   = function(v) code4(lead_code(v$vprec))),
  # FL Walton: unit_id is "Walton 210" but `precinct_sos` is the bare "210".
  # FL Baker:  unit_id is the bare name but `precinct_sos` is "Baxter 4A".
  list(id = "raw_code_sos",  ours = function(d) code4(d$precinct_raw),
                             vh   = function(v) code4(v$precinct_sos)),
  list(id = "raw_code_sostail", ours = function(d) code4(d$precinct_raw),
                             vh   = function(v) code4(tail_tok(v$precinct_sos))),
  # FL Gadsden, Levy: votehub keys by polling-place NAME, and our `precinct_enr` for
  # those counties IS the polling-place name.
  list(id = "enr_name",      ours = function(d) name_norm(d$precinct_enr),
                             vh   = function(v) name_norm(v$precinct_sos)),
  list(id = "enr_name_uid",  ours = function(d) name_norm(d$precinct_enr),
                             vh   = function(v) name_norm(v$vprec)),
  # FL Baker again: `precinct_enr` is NA there, but `precinct_l2` carries the name as a
  # suffix ("4A-BAXTER") and votehub's unit_id tail is the bare name ("Baxter").
  list(id = "l2_name",       ours = function(d) name_norm(l2_suffix(d$precinct_l2)),
                             vh   = function(v) name_norm(v$vprec)),
  list(id = "l2_name_sos",   ours = function(d) name_norm(l2_suffix(d$precinct_l2)),
                             vh   = function(v) name_norm(v$precinct_sos)),
  # NC Cherokee: our `precinct_l2` is the WHOLE name with no code prefix
  # ("ANDREWS NORTH WARD"), votehub's unit_id tail is an abbreviation ("ANW") and only
  # `precinct_sos` spells it out. The `l2_suffix` rules above cannot see this because
  # they require a "-" to split on.
  list(id = "l2_name_full",  ours = function(d) name_norm(d$precinct_l2),
                             vh   = function(v) name_norm(v$precinct_sos)),
  list(id = "l2_name_full_uid", ours = function(d) name_norm(d$precinct_l2),
                             vh   = function(v) name_norm(v$vprec))
)

# ---- read the votehub attribute table (no geometry) -----------------------------
cli_h1("Precinct geometry crosswalk  ({paste(STATES, collapse='/')}, {YEAR})")

tmpd <- file.path(tempdir(), "precinct_topo")
dir.create(tmpd, showWarnings = FALSE, recursive = TRUE)

read_vh_attrs <- function(states) {
  f <- file.path(tmpd, "vh_attrs.csv")
  if (file.exists(f)) unlink(f)
  sql <- sprintf(
    "SELECT unit_id, state_abbr, county, fips, precinct_sos FROM %s WHERE state_abbr IN (%s)",
    GPKG_LAYER, paste0("'", states, "'", collapse = ","))
  st <- system2("ogr2ogr", c("-f", "CSV", shQuote(f), shQuote(P_GPKG),
                             "-sql", shQuote(sql), "-nlt", "NONE"),
                stdout = TRUE, stderr = TRUE)
  if (!file.exists(f)) cli_abort("ogr2ogr failed reading {GPKG_LAYER}: {paste(st, collapse=' ')}")
  read_csv(f, col_types = cols(.default = "c")) |>
    mutate(fips5 = str_extract(unit_id, "^[^-]+"),
           vprec = str_remove(unit_id, "^[^-]+-:-"))
}

vh_all <- read_vh_attrs(STATES)
cli_inform("votehub: {format(nrow(vh_all), big.mark=',')} features across \\
            {n_distinct(vh_all$state_abbr)} states")
## `fips` is carried for provenance but the join keys on the unit_id prefix: the prefix
## is what defines the key and is guaranteed consistent with it.
bad_fips <- sum(vh_all$fips5 != str_pad(vh_all$fips, 5, pad = "0"), na.rm = TRUE)
if (bad_fips > 0)
  cli_alert_warning("{bad_fips} votehub rows where the unit_id FIPS prefix \\
                     disagrees with the `fips` column -- keying on the prefix")

# ---- crosswalk for one state ----------------------------------------------------
build_one <- function(st) {
  p_prec <- file.path(P_IN, paste0(st, "_precinct.parquet"))
  if (!file.exists(p_prec)) { cli_alert_warning("no {st}_precinct.parquet -- skipped"); return(NULL) }

  d <- read_parquet(p_prec) |> as_tibble() |>
    select(geo, county_fips, county_name, precinct_raw, precinct26_code,
           precinct_l2, precinct_enr) |>
    mutate(county_fips = str_pad(as.character(county_fips), 5, pad = "0"))
  if (anyDuplicated(d$geo))
    cli_abort("{st}: `geo` is not unique in {st}_precinct.parquet \\
               ({sum(duplicated(d$geo))} dups) -- the crosswalk key is broken")

  v <- vh_all |> filter(state_abbr == st)
  ## restrict geometry to the counties we actually model. FL is 31 of 67 counties and TX
  ## 32 of 254, so this is most of the file.
  v <- v |> filter(fips5 %in% unique(d$county_fips))

  cli_h2("{st}: {nrow(d)} precincts, {nrow(v)} votehub features in \\
          {n_distinct(d$county_fips)} modelled counties")

  matched <- tibble(geo = character(), unit_id = character(), rule = character())
  for (r in RULES) {
    todo <- d |> filter(!geo %in% matched$geo)
    free <- v |> filter(!unit_id %in% matched$unit_id)
    if (nrow(todo) == 0 || nrow(free) == 0) break

    lhs <- todo |> transmute(geo, cf = county_fips, k = r$ours(todo)) |> filter(!is.na(k))
    rhs <- free |> transmute(unit_id, cf = fips5, k = r$vh(free)) |> filter(!is.na(k))
    ## uniqueness on BOTH sides within the county, or the pair is dropped
    lhs <- lhs |> add_count(cf, k, name = "nl") |> filter(nl == 1) |> select(-nl)
    rhs <- rhs |> add_count(cf, k, name = "nr") |> filter(nr == 1) |> select(-nr)
    hit <- inner_join(lhs, rhs, by = c("cf", "k"))
    if (nrow(hit) == 0) next
    matched <- bind_rows(matched, hit |> transmute(geo, unit_id, rule = r$id))
    cli_inform("  rule {.field {r$id}}: +{nrow(hit)} \\
                (cumulative {nrow(matched)}/{nrow(d)} = {round(100*nrow(matched)/nrow(d),1)}%)")
  }
  stopifnot(!anyDuplicated(matched$geo), !anyDuplicated(matched$unit_id))

  xw <- d |> left_join(matched, by = "geo") |>
    transmute(state = st, year = YEAR, geo, unit_id, rule,
              county_fips, county_name, precinct_raw)

  ## per-county report. `vh_n` is the polygon count that unit actually has, which is
  ## what makes `pct_attainable` meaningful.
  ##
  ## GROUPED ON `county_fips` ALONE, not on (county_fips, county_name). In AK our
  ## `county_fips` is a HOUSE DISTRICT, not a county -- 02001 covers Ketchikan Gateway,
  ## Prince of Wales-Hyder AND Wrangell, and one HD reaches 7 borough names. Grouping on
  ## the name too would split one join unit into several rows, each carrying the SAME
  ## `vh_n`, which double-counts the attainable denominator. `vh_county` is votehub's own
  ## label for the unit ("HD-01" in AK, the county name elsewhere) and is the honest
  ## caption for a row.
  vh_n <- v |> summarize(vh_n = n(), vh_county = first(county), .by = fips5)
  cty <- xw |>
    summarize(our_n = n(), matched = sum(!is.na(unit_id)),
              county_name = first(sort(county_name)),
              n_names = n_distinct(county_name), .by = county_fips) |>
    left_join(vh_n, by = c(county_fips = "fips5")) |>
    mutate(vh_n = coalesce(vh_n, 0L),
           attainable = pmin(our_n, vh_n),
           pct_ours = round(100 * matched / our_n, 1),
           pct_attainable = ifelse(attainable > 0, round(100 * matched / attainable, 1), NA_real_),
           state = st, year = YEAR) |>
    arrange(pct_attainable, pct_ours)

  list(state = st, xwalk = xw, county = cty, vh = v,
       n_ours = nrow(d), n_matched = sum(!is.na(xw$unit_id)),
       attainable = sum(cty$attainable))
}

res <- map(STATES, build_one) |> compact()
if (length(res) == 0) cli_abort("nothing built")

xwalk <- bind_rows(map(res, "xwalk"))
cty   <- bind_rows(map(res, "county"))

# ---- report ---------------------------------------------------------------------
cli_h1("Match rates")

state_tbl <- tibble(
  state = map_chr(res, "state"),
  our_prec = map_int(res, "n_ours"),
  matched  = map_int(res, "n_matched"),
  attainable = map_int(res, ~ as.integer(.x$attainable))) |>
  mutate(pct_ours = round(100 * matched / our_prec, 1),
         pct_attainable = round(100 * matched / attainable, 1),
         counties = map_int(res, ~ n_distinct(.x$county$county_fips)))
print(as.data.frame(state_tbl), row.names = FALSE)

cli_h2("counties below 100% (ranked by pct_attainable)")
show <- cty |> filter(pct_ours < 100) |>
  select(state, vh_county, county_name, county_fips, our_n, vh_n, attainable, matched,
         pct_ours, pct_attainable)
if (nrow(show) == 0) cli_alert_success("every county at 100%") else
  print(as.data.frame(show), row.names = FALSE)

cli_h2("rule usage")
print(as.data.frame(
  xwalk |> count(state, rule) |> pivot_wider(names_from = rule, values_from = n, values_fill = 0L)),
  row.names = FALSE)

# ---- gates: fail loudly, do not emit a map with silent holes --------------------
## Collect every violation, then abort once with all of them. Aborting on the first
## means a second run to learn about the second problem.
fails <- character()

bad_cty <- cty |> filter(!is.na(pct_attainable), pct_attainable < 100 * MIN_COUNTY_ATTAINABLE)
if (nrow(bad_cty) > 0)
  fails <- c(fails, sprintf(
    "county key failure: %s (%s) matched %d of %d attainable = %.1f%% < %.0f%%",
    bad_cty$county_name, bad_cty$state, bad_cty$matched, bad_cty$attainable,
    bad_cty$pct_attainable, 100 * MIN_COUNTY_ATTAINABLE))

bad_st <- state_tbl |> filter(pct_attainable < 100 * MIN_STATE_ATTAINABLE)
if (nrow(bad_st) > 0)
  fails <- c(fails, sprintf("state key failure: %s at %.1f%% of attainable < %.0f%%",
                            bad_st$state, bad_st$pct_attainable, 100 * MIN_STATE_ATTAINABLE))

## pct_ours is a COVERAGE warning, not a key failure -- it is capped by the geometry
## vintage. It blocks that state's topojson but does not fail the crosswalk, because the
## crosswalk is still correct and still useful for the counties that do have polygons.
thin <- state_tbl |> filter(pct_ours < 100 * MIN_STATE_OURS)
if (nrow(thin) > 0)
  cli_alert_danger("COVERAGE: {paste(thin$state, collapse=', ')} below \\
                    {100*MIN_STATE_OURS}% of OUR precincts -- geometry vintage, \\
                    not a key problem. Topojson NOT written for these states.")

# WRITE THE CROSSWALK FIRST, THEN GATE. The gate protects the MAP -- a topojson with
# holes is worse than none -- but the crosswalk is a different artifact: a reviewable,
# per-row-attributed record of which `geo` matched which `unit_id` and by what rule. It is
# exactly what someone needs in order to FIX a gate failure, so withholding it on a gate
# failure is backwards, and it left model_precincts.csv joined against a stale crosswalk
# (AK read 312 of 402 after the ENR id fix changed its geos, purely because this file had
# not been rewritten).
write_csv(xwalk |> arrange(state, county_fips, geo), P_XWALK, na = "")
write_csv(cty |> select(state, year, county_fips, vh_county, county_name, n_names,
                        our_n, vh_n, attainable, matched, pct_ours, pct_attainable) |>
            arrange(state, county_fips), P_RATES, na = "")
cli_alert_success("wrote {.file {P_XWALK}} ({nrow(xwalk)} rows, \\
                   {sum(!is.na(xwalk$unit_id))} matched)")
cli_alert_success("wrote {.file {P_RATES}} ({nrow(cty)} counties)")

if (SKIP_TOPO) { cli_alert_info("SKIP_TOPO set -- stopping before geometry"); quit(save = "no") }

# ---- emit one simplified topojson per state -------------------------------------
## Unmatched polygons are KEPT, with `geo` null. A dropped polygon is a hole in the
## state outline that reads as "no votes here"; a null-`geo` polygon renders as no-data
## and is honest about what it is.

if (length(fails) > 0) {
  cli_h1("GATE FAILED")
  cli_bullets(set_names(fails, rep("x", length(fails))))
  cli_abort("{length(fails)} match-rate gate failure(s) -- refusing to emit a map with holes.")
}

# ---- persist the lookup ---------------------------------------------------------
## Keyed on (state, year) like `vote_mode_map.csv`, with `rule` as the provenance column
## so any single row's join can be traced to the rule that made it.
cli_h1("topojson")

emit_one <- function(r) {
  st <- r$state
  if (st %in% thin$state) { cli_alert_warning("{st}: skipped (coverage gate)"); return(NULL) }

  counties <- unique(r$county$county_fips)
  f_json <- file.path(tmpd, paste0(st, "_raw.geojson"))
  f_join <- file.path(tmpd, paste0(st, "_xwalk.csv"))
  f_out  <- file.path(DIR_TOPO, paste0(tolower(st), "_precincts.topojson"))

  ## one row per unit_id for mapshaper's -join; only the matched ones, so unmatched
  ## features come through with geo unset
  r$xwalk |> filter(!is.na(unit_id)) |>
    transmute(unit_id, geo, county_fips, county_name, rule) |>
    write_csv(f_join, na = "")

  if (file.exists(f_json)) unlink(f_json)
  ## `geom` MUST be named explicitly in the SELECT list. With an explicit column list and
  ## no geometry column, OGR SQL on a GeoPackage returns `"geometry": null` for every
  ## feature and ogr2ogr still EXITS 0 -- producing a valid GeoJSON that mapshaper
  ## happily turns into a valid, geometry-free topojson. Nothing downstream complains.
  ## The assertion below is the guard; this comment is why it exists.
  sql <- sprintf(
    "SELECT unit_id, geom FROM %s WHERE state_abbr = '%s' AND fips IN (%s)",
    GPKG_LAYER, st, paste0("'", counties, "'", collapse = ","))
  ok <- system2("ogr2ogr", c("-f", "GeoJSON", shQuote(f_json), shQuote(P_GPKG),
                             "-sql", shQuote(sql), "-t_srs", "EPSG:4326"),
                stdout = TRUE, stderr = TRUE)
  if (!file.exists(f_json)) cli_abort("{st}: ogr2ogr GeoJSON failed: {paste(ok, collapse=' ')}")
  mb_in <- round(file.size(f_json) / 1e6, 1)
  if (any(grepl('"geometry":null', readLines(f_json, n = 20, warn = FALSE), fixed = TRUE)))
    cli_abort("{st}: ogr2ogr returned NULL geometry -- is `geom` in the SELECT list?")

  ## STEP ORDER IS LOAD-BEARING: `-clean` must come BEFORE `-simplify`.
  ## `-clean` rebuilds topology from the full-precision arcs, so running it AFTER
  ## `-simplify` silently DISCARDS the simplification -- measured on AK, 0.25 MB with
  ## this order against 12.62 MB with the two swapped, and mapshaper reports success
  ## either way. Cleaning first also means the sliver removal operates on real geometry
  ## rather than on simplification artifacts, which is the right order anyway.
  ##
  ## `string-fields=` is equally load-bearing: without it mapshaper type-infers the
  ## joined CSV and `county_fips` "02001" becomes the NUMBER 2001.
  ## read the written file back and count what is actually in it. `-info` is the only
  ## thing that reports null geometries, and nothing else in the chain does.
  inspect <- function(f) {
    chk <- system2("mapshaper", c(shQuote(f), "-info"), stdout = TRUE, stderr = TRUE)
    num <- function(pat) {
      ln <- grep(pat, chk, value = TRUE)[1]
      if (is.na(ln)) return(0L)
      as.integer(str_match(str_remove_all(ln, ","), "([0-9]+)")[, 2])
    }
    bb <- grep("Bounds:", chk, value = TRUE)[1]
    bb <- if (is.na(bb)) rep(NA_real_, 4) else
      as.numeric(strsplit(trimws(str_remove(bb, ".*Bounds:")), ",")[[1]])
    list(polygon = any(grepl("Type:\\s+polygon", chk)),
         records = num("Records:"), nulls = num("Nulls:"), bounds = bb, raw = chk)
  }

  quant <- NA_character_; info <- NULL
  for (q in QUANT_CANDIDATES) {
    ms <- system2("mapshaper", c(
      "-i", shQuote(f_json),
      "-join", shQuote(f_join), "keys=unit_id,unit_id",
        "fields=geo,county_fips,county_name", "unjoined", "unmatched",
        "string-fields=unit_id,geo,county_fips,county_name,rule",
      "-filter-fields", "unit_id,geo,county_fips,county_name",
      "-clean",
      "-simplify", paste0("interval=", SIMPLIFY_INTERVAL), "keep-shapes",
      "-o", shQuote(f_out), "format=topojson",
        if (q == "no-quantization") "no-quantization" else paste0("quantization=", q),
        "force"),
      stdout = TRUE, stderr = TRUE)
    if (!file.exists(f_out)) cli_abort("{st}: mapshaper failed: {paste(ms, collapse='\n')}")
    if (q == QUANT_CANDIDATES[1]) {
      jm <- grep("unmatched|unjoined|Removed|Retained", ms, value = TRUE)
      for (m in jm) cli_inform("  {m}")
    }
    ## verify the ARTIFACT, not the exit code.
    info <- inspect(f_out)
    if (!info$polygon)
      cli_abort("{st}: topojson is not a polygon layer -- geometry was lost. \\
                 mapshaper -info said: {paste(info$raw, collapse=' ')}")
    if (info$records != nrow(r$vh))
      cli_abort("{st}: topojson has {info$records} features, expected {nrow(r$vh)}")
    quant <- q
    if (info$nulls == 0L) break
    cli_alert_warning("{st}: quantization={q} nulled {info$nulls} geometries -- escalating")
  }
  if (info$nulls != 0L)
    cli_abort("{st}: {info$nulls} null geometries remain at \\
               quantization={quant} -- small precincts would render as holes.")

  ## ANTIMERIDIAN. AK's Aleutian tail crosses +180, so its bounding box reads
  ## -179.1..179.8 -- 360 degrees wide for a state ~55 degrees wide. The GEOMETRY IS
  ## CORRECT; what breaks is any renderer that fits a viewport to the bbox, which draws
  ## AK as a sliver. The fix belongs at render time (an Albers projection, e.g. Observable
  ## Plot's `projection: "albers-usa"`), not in the data, so this warns rather than
  ## rewriting coordinates.
  if (!anyNA(info$bounds) && (info$bounds[3] - info$bounds[1]) > 180)
    cli_alert_warning("{st}: bounds span {round(info$bounds[3]-info$bounds[1])} deg \\
                       -- crosses the antimeridian. Geometry is correct, but the \\
                       renderer MUST use a projection (e.g. albers-usa); fitting a \\
                       viewport to the bbox will draw this state as a sliver.")

  mb_out <- round(file.size(f_out) / 1e6, 2)
  cli_alert_success("{st}: {info$records} features, 0 nulls, quant {quant}, \\
                     geojson {mb_in} MB -> topojson {mb_out} MB  {.file {f_out}}")
  tibble(state = st, features = info$records, mb_geojson = mb_in, mb_topojson = mb_out,
         interval_m = SIMPLIFY_INTERVAL, quantization = quant, file = f_out)
}

sizes <- map(res, emit_one) |> compact() |> bind_rows()
cli_h2("artifact sizes")
if (nrow(sizes)) print(as.data.frame(sizes |> select(-file)), row.names = FALSE)
cli_alert_info("index.html is already ~17.8 MB with embed-resources -- check the delta \\
                before wiring these in, and raise SIMPLIFY_INTERVAL if they cost too much.")
cli_alert_success("done. geometry is static: do NOT re-run this per refresh.")
