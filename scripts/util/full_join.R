# ========================================
## Fully joined ENR files: every contest in a state's clean *_latest.parquet files,
## joined to L2 precinct covariates through input_data/crosswalks/{st}.csv
## (ENR id -> frame precinct -> L2 precinct; docs/precinct_crosswalks.md) and the
## newest input_data/l2/{st}_YYYYMMDD_precincts.parquet.
## Written to 26_general/full/{ST}_latest_full.parquet by the full_{ST} targets
## (_targets.R) whenever that state's scrapes, crosswalk, or L2 file change.
##
## Columns: the clean file's columns, then
##   county          county name (enightmodels::cbs_counties, from the geo's county number)
##   precinct_l2     L2 precinct id; ";"-separated for a CA consolidation; NA when the ENR
##                   precinct has no L2 match
##   l2_match        "crosswalk" ({st}.csv), or "consolidated" for the CA counties that
##                   report consolidated precincts (L2 summed over the member precincts in
##                   ca_{county}_members.csv); NA when unmatched
##   l2_n_precincts  L2 precincts combined into the row
##   l2_n ... turnout24
##                   the L2 covariates the models use (l2_cov in
##                   scripts/model/prep/prep_model_inputs.R): l2_n, active, new, p_male,
##                   p_age_*, p_white/black/hisp/other, p_col, p_{race}_{col,noncol},
##                   turnout18-24 (past general turnout of current registrants). Shares
##                   are computed from the summed counts.
## The L2 columns repeat on every candidate x method row of a precinct, so weight or
## sum them over distinct geos, not over rows.
##
## Matching keys are those of scripts/util/crosswalks/match_rates.R (the watcher's
## crosswalk match-rate check).
# ========================================

source("scripts/util/crosswalks/match_rates.R") # xw_county(), xw_prec()

# L2 counts the covariates are built from (summed over a consolidation's members)
L2_COUNTS = c("n", "active", "new", "male", "age1", "age2", "age3", "age4", "race1", "race2", "race3", "race4",
              "col_known", "noncol_known", "race1_col", "race1_noncol", "race2_col", "race2_noncol",
              "race3_col", "race3_noncol", "vote18", "vote20", "vote22", "vote24")

# CA counties whose ENR reports consolidated precincts (scripts/util/crosswalks/ca_*_consolidated.R)
CA_CONSOLIDATED = c("sandiego", "riverside", "solano", "nevada")

input_path = function(...) fs::path(PATH_DROPBOX, ELECTION_FOLDER, "input_data", ...)

# Newest L2 precinct file for a state, or character(0)
l2_file = function(state) {
  fs::dir_ls(input_path("l2"), regexp = glue("/{tolower(state)}_\\d{{8}}_precincts\\.parquet$"), fail = FALSE) |>
    sort() |>
    tail(1) |>
    as.character()
}

# Every file a state's ENR -> L2 map reads (for the targets' file cue)
l2_map_files = function(state) {
  f = c(input_path("crosswalks", glue("{tolower(state)}.csv")), l2_file(state))
  if (state == "CA") {
    f = c(f, input_path("crosswalks", glue("ca_{CA_CONSOLIDATED}_members.csv")),
          input_path("crosswalks", glue("ca_{CA_CONSOLIDATED}_frame_consolidated.csv")))
  }
  as.character(f[fs::file_exists(f)])
}

# ENR geo ("mi_041_Ada Township, Precinct 1") -> "{county key} {precinct key}", the key
# l2_precinct_map() is on. NA for geos without a known county number.
geo_key = function(state, geo) {
  m = stringr::str_match(as.character(geo), "^[a-z]{2}_(\\d+)_(.*)$")
  cf = enightmodels::cbs_counties[enightmodels::cbs_counties$state == state, ]
  county = toupper(cf$county)[match(as.integer(m[, 2]), as.integer(cf$county_cbs))]
  ifelse(is.na(county), NA_character_, paste(xw_county(state, county), xw_prec(m[, 3])))
}

# County key on the L2 side, where spellings differ from the frame's ("ALEUTIANS EAST
# BOROUGH", "MIAMI-DADE")
l2_county = function(x) {
  x = sub(" (COUNTY|BOROUGH|PARISH|CENSUS AREA|MUNICIPALITY|CITY AND BOROUGH)$", "", str_squish(toupper(x)))
  gsub("[^A-Z]", "", gsub("&", "AND", x))
}

# l2_cov of scripts/model/prep/prep_model_inputs.R, from (summed) L2 counts
l2_covariates = function(x) {
  x |>
    mutate(
      l2_n = n,
      p_male = male / n,
      p_age_18_29 = age1 / n,
      p_age_30_44 = age2 / n,
      p_age_45_64 = age3 / n,
      p_age_65_up = age4 / n,
      p_white = race1 / n,
      p_black = race2 / n,
      p_hisp = race3 / n,
      p_other = race4 / n,
      p_col = if_else(col_known + noncol_known > 0, col_known / (col_known + noncol_known), NA_real_),
      p_white_col = race1_col / n,
      p_white_noncol = race1_noncol / n,
      p_black_col = race2_col / n,
      p_black_noncol = race2_noncol / n,
      p_hisp_col = race3_col / n,
      p_hisp_noncol = race3_noncol / n,
      turnout18 = vote18 / n,
      turnout20 = vote20 / n,
      turnout22 = vote22 / n,
      turnout24 = vote24 / n,
      across(c(starts_with("p_"), starts_with("turnout")), \(v) if_else(is.finite(v), v, NA_real_))
    ) |>
    select(-any_of(setdiff(L2_COUNTS, c("active", "new"))))
}

#' ENR precinct -> L2 covariates for one state
#'
#' @param state Two-letter state.
#' @return Tibble, one row per crosswalked ENR precinct found in L2: gkey (geo_key() of
#'   the ENR id, plus the "<county>" spelling of VA "<county> CITY" ids), precinct_l2,
#'   l2_match, l2_n_precincts, and the covariates. NULL when the state has no crosswalk
#'   or no L2 file.
l2_precinct_map = function(state) {
  xw_path = input_path("crosswalks", glue("{tolower(state)}.csv"))
  l2_path = l2_file(state)
  if (!fs::file_exists(xw_path) || length(l2_path) == 0) return(NULL)
  rd = \(p) readr::read_csv(p, col_types = readr::cols(.default = "c"), progress = FALSE)
  pk = \(x) str_squish(toupper(x)) # L2 precinct id

  l2 = nanoparquet::read_parquet(l2_path) |>
    as_tibble() |>
    transmute(lck = l2_county(county), precinct_l2 = pk(coalesce(precinct, str_extract(precinct_cbs, "(?<=_).+$"))),
              across(all_of(L2_COUNTS), as.numeric)) |>
    filter(!is.na(precinct_l2), precinct_l2 != "")
  stopifnot(!anyDuplicated(l2[c("lck", "precinct_l2")]))
  # an L2 id unique statewide is matched without its county (AK precincts that sit in a
  # different borough in L2; prep_model_inputs.R does the same)
  uniq = !(duplicated(l2$precinct_l2) | duplicated(l2$precinct_l2, fromLast = TRUE))
  locate = \(lck, p) coalesce(match(paste(lck, p), paste(l2$lck, l2$precinct_l2)), which(uniq)[match(p, l2$precinct_l2[uniq])])

  xw = rd(xw_path)
  out = xw |>
    filter(!is.na(precinct_enr), precinct_enr != "", !is.na(precinct_l2), precinct_l2 != "") |>
    distinct(county, precinct_enr, precinct_l2) |> # frame rows repeat per cd
    transmute(gkey = paste(xw_county(.env$state, county), xw_prec(precinct_enr)),
              .row = locate(l2_county(county), pk(precinct_l2))) |>
    filter(!is.na(.row))
  stopifnot(!anyDuplicated(out$gkey))
  out = bind_cols(select(out, gkey), l2[out$.row, c("precinct_l2", L2_COUNTS)]) |>
    mutate(l2_match = "crosswalk", l2_n_precincts = 1L)

  if (state == "CA") {
    # consolidations: member frame precincts (ca_{name}_members.csv, usable ones flagged
    # `use` in ca_{name}_frame_consolidated.csv) -> L2 through ca.csv's frame rows, which
    # keep these counties' ENR ids NA; counts summed over the distinct L2 precincts
    fr = xw |>
      filter(!is.na(precinct_yougov), !is.na(precinct_l2), precinct_l2 != "") |>
      distinct(county, precinct_yougov, precinct_l2)
    cons = map(CA_CONSOLIDATED, function(nm) {
      mem = input_path("crosswalks", glue("ca_{nm}_members.csv"))
      use = input_path("crosswalks", glue("ca_{nm}_frame_consolidated.csv"))
      if (!fs::file_exists(mem) || !fs::file_exists(use)) return(NULL)
      ok = rd(use) |> filter(toupper(use) == "TRUE") |> distinct(county_name, precinct_enr)
      rd(mem) |>
        semi_join(ok, by = c("county_name", "precinct_enr")) |>
        distinct(county = county_name, precinct_enr, precinct_yougov) |>
        inner_join(fr, by = c("county", "precinct_yougov")) |>
        distinct(county, precinct_enr, precinct_l2)
    }) |>
      bind_rows() |>
      mutate(gkey = paste(xw_county(.env$state, county), xw_prec(precinct_enr)),
             .row = locate(l2_county(county), pk(precinct_l2))) |>
      filter(!is.na(.row)) |>
      distinct(gkey, .row)
    cons = bind_cols(select(cons, gkey), l2[cons$.row, c("precinct_l2", L2_COUNTS)]) |>
      summarise(across(all_of(L2_COUNTS), sum), precinct_l2 = paste(sort(precinct_l2), collapse = ";"),
                l2_n_precincts = n(), .by = gkey) |>
      mutate(l2_match = "consolidated")
    out = bind_rows(out, anti_join(cons, out, by = "gkey"))
  }
  if (state == "VA") {
    # feed counties spelled without " CITY" (as match_rates.R handles them)
    city = filter(out, str_detect(gkey, "^[A-Z]+CITY ")) |>
      mutate(gkey = str_replace(gkey, "^([A-Z]+)CITY ", "\\1 "))
    out = bind_rows(out, anti_join(city, out, by = "gkey"))
  }
  out |>
    filter(n > 0) |>
    l2_covariates() |>
    relocate(gkey, precinct_l2, l2_match, l2_n_precincts, l2_n)
}

# the clean *_latest.parquet files of a state's metadata rows (build & updated)
full_inputs = function(state, metadata) {
  # metadata is a data.table, where `state` inside [ ] would be the column
  m = as.data.frame(metadata)
  m = m[m$state == state, ]
  f = fs::path(PATH_DROPBOX, ELECTION_FOLDER, state, "clean",
               paste0(ifelse(is.na(m$county), state, paste0(state, "_", m$county)), "_latest.parquet"))
  f = unique(f[fs::file_exists(f)])
  as.character(f[fs::file_size(f) > 0])
}

#' Write one state's fully joined file
#'
#' @param state Two-letter state.
#' @param files The state's clean *_latest.parquet files (full_inputs()).
#' @param ... Upstream targets, only so the target reruns when they change.
#' @return Path to 26_general/full/{state}_latest_full.parquet, or character(0) when the
#'   state has no data yet.
build_full = function(state, files, ...) {
  d = map(files, \(f) nanoparquet::read_parquet(f) |> mutate(across(where(is.factor), as.character))) |>
    bind_rows()
  if (nrow(d) == 0) return(character(0))
  cf = enightmodels::cbs_counties[enightmodels::cbs_counties$state == state, ]
  num = as.integer(str_match(as.character(d$geo), "^[a-z]{2}_(\\d+)_")[, 2])
  d$county = toupper(cf$county)[match(num, as.integer(cf$county_cbs))]

  lm = l2_precinct_map(state)
  if (is.null(lm)) {
    # same columns as a state with a crosswalk, all NA
    lm = as_tibble(setNames(rep(list(numeric()), length(L2_COUNTS)), L2_COUNTS)) |>
      mutate(gkey = character(), precinct_l2 = character(), l2_match = character(), l2_n_precincts = integer()) |>
      l2_covariates() |>
      relocate(gkey, precinct_l2, l2_match, l2_n_precincts, l2_n)
  }
  g = distinct(d, geo) |> mutate(gkey = geo_key(.env$state, geo))
  d = d |>
    left_join(g, by = "geo") |>
    left_join(lm, by = "gkey", na_matches = "never") |>
    select(-gkey)

  out = fs::path(PATH_DROPBOX, ELECTION_FOLDER, "full", glue("{state}_latest_full.parquet"))
  fs::dir_create(fs::path_dir(out))
  tmp = paste0(out, ".tmp")
  nanoparquet::write_parquet(d, tmp)
  fs::file_move(tmp, out) # readers never see a half-written file
  as.character(out)
}
