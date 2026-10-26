# ========================================
## make_model_reference.R -- the two authoritative identity tables FOR THE MODEL.
##
## WHY. County identity currently lives in four places (geocodes.xls -> cbs_counties ->
## {ST}_counties.csv -> L2's own FIPS/county) and precinct identity in three
## (crosswalks/{st}.csv, lookups/precinct_geo_crosswalk.csv, lookups/reporting_levels.csv),
## each with its own name normalisation. Nothing says which to consult for which join.
## This script collapses both into one table per entity, scoped to the states we model.
##
## STRICTLY READ-ONLY UPSTREAM. It consumes Dropbox inputs and writes only into this repo.
## It does not touch create_frame.R, crosswalks/, cbs_counties or anyone else's joins, and
## nothing upstream depends on its output. Phase 1 (now) is reference + diagnostic; pointing
## the model's own joins at it is a later, separate change.
##
## DERIVED, NEVER HAND-MAINTAINED. Rebuilt on demand, like the {ST}_prep_report.txt files
## and lookups/precinct_geo_match_rates.csv. A hand-kept doc would rot exactly the way TX's
## prep report did -- it says "32 counties", which was true on 2026-10-19 and misleading
## once the YouGov frame was refreshed on 2026-10-23.
##
## Writes (committed, CSV so they diff in review):
##   scripts/model/lookups/model_counties.csv    one row per state x county
##   scripts/model/lookups/model_precincts.csv   one row per state x geo
##
## ALASKA, stated once. AK's `county_fips` is a HOUSE DISTRICT, not a county: 02001 spans
## Ketchikan Gateway, Prince of Wales-Hyder and Wrangell, and one district reaches seven
## borough names. RUCC and the L2 demographics key on real borough FIPS -- 30 boroughs
## against 40 districts. Per YK (2026-10-26) this table is DISTRICT-KEYED THROUGHOUT and
## AK's demographic and RUCC columns are left NA rather than faked through a many-to-many.
## `demog_source` records which, so a consumer can never mistake NA for zero.
##
## Run from the cbs-enr folder:  Rscript scripts/model/make_model_reference.R
##   STATES=TX,NC  limit the states
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readr)
})
source("scripts/model/live/R/paths.R")

STATE_FIPS <- c(AK = "02", CA = "06", FL = "12", MI = "26", NC = "37", TX = "48")
STATES <- { s <- Sys.getenv("STATES"); if (nzchar(s)) trimws(strsplit(s, ",")[[1]]) else names(STATE_FIPS) }
AK_DISTRICT_KEYED <- TRUE   # see header

d26     <- find_26_general()
dir_in  <- file.path(d26, "input_data")
dir_mi  <- file.path(dir_in, "model_inputs")
dir_out <- "scripts/model/lookups"
stopifnot("run from the cbs-enr folder" = dir.exists(dir_out))

say <- function(...) cat(sprintf(...), "\n", sep = "")

# county-name comparison key: upper, drop everything but letters and digits. "ST. JOHNS",
# "St Johns" and "St.Johns" all collapse to STJOHNS.
ckey <- function(x) str_replace_all(toupper(coalesce(as.character(x), "")), "[^A-Z0-9]", "")

# ---- shared reference reads --------------------------------------------------------
rucc <- read_csv(file.path(dir_in, "Ruralurbancontinuumcodes2023.csv"),
                 col_types = cols(.default = "c"), progress = FALSE) |>
  filter(Attribute == "RUCC_2023") |>
  transmute(county_fips = sprintf("%05d", as.integer(FIPS)),
            rucc = as.integer(Value),
            rucc_county_name = str_squish(County_Name)) |>
  mutate(rurality = case_when(rucc <= 3 ~ "urban", rucc == 4 ~ "suburban", rucc >= 5 ~ "rural"))

cbs <- enightmodels::cbs_counties |>
  transmute(state, county_cbs = str_pad(county_cbs, 3, pad = "0"),
            cbs_county_name = str_squish(county), geostratum_cbs = geostratum)

geo_xw <- {
  f <- file.path(dir_out, "precinct_geo_crosswalk.csv")
  if (!file.exists(f)) { say("! %s absent -- unit_id will be NA", basename(f)); NULL }
  else read_csv(f, col_types = cols(.default = "c"), progress = FALSE) |>
         transmute(state, geo, unit_id, geo_rule = rule)
}

rep_lv <- {
  f <- file.path(dir_out, "reporting_levels.csv")
  if (!file.exists(f)) { say("! %s absent -- reporting levels will be NA", basename(f)); NULL }
  else read_csv(f, col_types = cols(.default = "c"), progress = FALSE)
}

# ---- per-state build --------------------------------------------------------------
cty_rows <- list(); prec_rows <- list()

for (st in STATES) {
  fips2 <- STATE_FIPS[[st]]
  p_prec <- file.path(dir_mi, paste0(st, "_precinct.parquet"))
  if (!file.exists(p_prec)) { say("! %s: no %s -- skipped", st, basename(p_prec)); next }
  prec <- nanoparquet::read_parquet(p_prec)

  ## ---- county demographics from L2 (ALL counties, not just modelled) -------------
  ## Aggregate COUNTS then divide. Never average precinct proportions: that weights a
  ## 50-registrant precinct the same as a 5,000-registrant one.
  l2f <- sort(Sys.glob(file.path(dir_in, "l2", paste0(tolower(st), "_*_precincts.parquet"))))
  ak_skip <- st == "AK" && AK_DISTRICT_KEYED
  demog <- NULL
  if (length(l2f) && !ak_skip) {
    l2 <- nanoparquet::read_parquet(l2f[length(l2f)])
    l2 <- l2[l2$state == st, ]
    demog <- l2 |>
      mutate(county_fips = paste0(fips2, str_pad(FIPS, 3, pad = "0"))) |>
      group_by(county_fips) |>
      summarise(across(any_of(c("n", "male", "age1", "age2", "age3", "age4",
                                "race1", "race2", "race3", "race4",
                                "col_known", "noncol_known",
                                "vote18", "vote20", "vote22", "vote24")),
                       \(x) sum(as.numeric(x), na.rm = TRUE)),
                n_precincts_l2 = n(), .groups = "drop") |>
      transmute(county_fips, l2_n = n, n_precincts_l2,
                p_male       = male  / pmax(n, 1),
                p_age_18_29  = age1  / pmax(n, 1),
                p_age_30_44  = age2  / pmax(n, 1),
                p_age_45_64  = age3  / pmax(n, 1),
                p_age_65_up  = age4  / pmax(n, 1),
                p_white      = race1 / pmax(n, 1),
                p_black      = race2 / pmax(n, 1),
                p_hisp       = race3 / pmax(n, 1),
                p_other      = race4 / pmax(n, 1),
                p_col        = col_known / pmax(col_known + noncol_known, 1),
                turnout18 = vote18 / pmax(n, 1), turnout20 = vote20 / pmax(n, 1),
                turnout22 = vote22 / pmax(n, 1), turnout24 = vote24 / pmax(n, 1))
  }

  ## ---- the county spine ---------------------------------------------------------
  ## {ST}_counties.csv is the FIPS <-> CBS-number bridge that cbs_counties lacks (it has
  ## no FIPS column at all), so it is the spine. Every county in the state, not only the
  ## modelled ones -- project_rest() needs all of them, and it is the one place a
  ## county-level covariate is not absorbed by the model's county random effect.
  p_cty <- file.path(dir_mi, paste0(st, "_counties.csv"))
  spine <- if (file.exists(p_cty)) {
    read_csv(p_cty, col_types = cols(.default = "c"), progress = FALSE) |>
      transmute(state = st, county_fips = str_pad(county_fips, 5, pad = "0"),
                county_cbs = str_pad(county_cbs, 3, pad = "0"),
                county_name = str_squish(county_name))
  } else {
    say("! %s: no %s -- county spine from L2 only", st, basename(p_cty))
    tibble(state = st, county_fips = unique(demog$county_fips),
           county_cbs = NA_character_, county_name = NA_character_)
  }

  modelled <- prec |>
    mutate(county_fips = str_pad(as.character(county_fips), 5, pad = "0")) |>
    count(county_fips, name = "n_precincts_modelled")

  rl <- if (is.null(rep_lv)) NULL else rep_lv |>
    filter(state == st, method != "*") |>
    mutate(county_fips = str_pad(county_fips, 5, pad = "0")) |>
    select(county_fips, method, level) |>
    distinct(county_fips, method, .keep_all = TRUE) |>
    pivot_wider(names_from = method, values_from = level, names_prefix = "level_")

  ct <- spine |>
    left_join(cbs, by = c("state", "county_cbs")) |>
    left_join(rucc, by = "county_fips") |>
    { \(x) if (is.null(demog)) mutate(x, l2_n = NA_real_, n_precincts_l2 = NA_integer_)
           else left_join(x, demog, by = "county_fips") }() |>
    left_join(modelled, by = "county_fips") |>
    { \(x) if (is.null(rl)) x else left_join(x, rl, by = "county_fips") }() |>
    mutate(
      n_precincts_modelled = coalesce(n_precincts_modelled, 0L),
      in_frame = n_precincts_modelled > 0,
      # NA means "not derived", never zero. AK is district-keyed so its demographics and
      # RUCC are deliberately absent -- see the header.
      demog_source = if (ak_skip) "none (AK is district-keyed; see make_model_reference.R)"
                     else if (is.null(demog)) "none (no L2 file)" else basename(l2f[length(l2f)]),
      # Compare on a NORMALISED key, not the raw string. The raw comparison flags five
      # purely cosmetic pairs (MIAMI-DADE/MiamiDade, ST. JOHNS/St Johns, ST. LUCIE,
      # ST. CLAIR, ST. JOSEPH) -- punctuation and case only, no data problem. A mismatch
      # flag that fires on punctuation trains people to ignore it, so strip it out and
      # keep the flag for differences that actually matter.
      name_match_cbs  = !is.na(cbs_county_name)  & ckey(cbs_county_name)  == ckey(county_name),
      name_match_rucc = !is.na(rucc_county_name) &
        ckey(str_remove(rucc_county_name,
                        " (County|Parish|Borough|Census Area|Municipality|city)$")) == ckey(county_name)
    )
  cty_rows[[st]] <- ct

  ## ---- the precinct identity table ---------------------------------------------
  ## {ST}_precinct.parquet already carries 18 identity columns; the only things missing
  ## are the votehub polygon id and the county's reporting level. So this is a join, not
  ## a rebuild.
  keep <- intersect(names(prec), c("geo", "state", "county_name", "county_fips", "county_cbs",
    "precinct_raw", "precinct26_code", "precinct_l2", "precinct_enr", "l2_rule",
    "geostratum", "stratum_demo", "boundary_vintage", "as_of",
    "l2_matched", "l2_imputed", "vote_imputed", "dem2_source"))
  pr <- prec |>
    select(all_of(keep)) |>
    mutate(across(everything(), as.character),
           state = st,
           county_fips = str_pad(county_fips, 5, pad = "0")) |>
    { \(x) if (is.null(geo_xw)) mutate(x, unit_id = NA_character_, geo_rule = NA_character_)
           else left_join(x, geo_xw, by = c("state", "geo")) }() |>
    { \(x) if (is.null(rl)) x
           else left_join(x, mutate(rl, across(everything(), as.character)), by = "county_fips") }() |>
    mutate(in_geometry = !is.na(unit_id) & nzchar(unit_id))
  prec_rows[[st]] <- pr

  say("%-3s counties %3d (modelled %3d) | precincts %6d | in geometry %5.1f%% | demog %s",
      st, nrow(ct), sum(ct$in_frame), nrow(pr), 100 * mean(pr$in_geometry),
      if (ak_skip || is.null(demog)) "NA" else "L2")
}

counties  <- bind_rows(cty_rows)
precincts <- bind_rows(prec_rows)

write_csv(counties,  file.path(dir_out, "model_counties.csv"),  na = "")
write_csv(precincts, file.path(dir_out, "model_precincts.csv"), na = "")

# ---- the diagnostic half: identity mismatches ARE the status signal ----------------
say("")
say("== model_counties.csv: %d rows, %d cols ==", nrow(counties), ncol(counties))
print(as.data.frame(counties |>
  summarise(counties = n(), modelled = sum(in_frame),
            rucc = sum(!is.na(rucc)), demog = sum(!is.na(l2_n)),
            name_ne_cbs = sum(!name_match_cbs, na.rm = TRUE),
            name_ne_rucc = sum(!name_match_rucc, na.rm = TRUE), .by = state)))
say("")
say("== model_precincts.csv: %d rows, %d cols ==", nrow(precincts), ncol(precincts))
print(as.data.frame(precincts |>
  summarise(precincts = n(),
            has_l2 = sum(l2_matched == "TRUE", na.rm = TRUE),
            has_enr = sum(!is.na(precinct_enr) & nzchar(precinct_enr)),
            in_geometry = sum(in_geometry), .by = state)))
say("")
say("wrote %s and %s", file.path(dir_out, "model_counties.csv"),
    file.path(dir_out, "model_precincts.csv"))
