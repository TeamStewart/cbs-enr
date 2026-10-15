# ========================================
## Build TX precinct covariates + baseline for the precinct model
##
## Input:  yougov_precinct_frame.csv (26_general/input_data/). Rows are
##         precinct x congressional-district PIECES: a precinct touching k
##         districts appears k times, each piece with its own n / rates.
## Output: 1. {STATE}_prec_covs.parquet  one row per precinct x party
##            (method = "all"; the frame has no early/mail split)
##         2. {STATE}_prec_cd.parquet    one row per precinct x district,
##            with each piece's share of the precinct's registered voters
##
## `geo` uses the live-scraper format: tx_{county_cbs 3-digit}_{precinct_id}
##
## Run from the cbs-enr folder:  Rscript scripts/model/prep_tx_covs.R
# ========================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
})

STATE = "TX"
# ---- EDIT THESE PATHS ----------------------------------------------------------
path_in   = "~/Downloads/yougov_precinct_frame.csv"
path_out  = glue("~/Downloads/{STATE}_prec_covs.parquet")
path_cd   = glue("~/Downloads/{STATE}_prec_cd.parquet")
path_snap = "~/Downloads/TX_harris_latest.parquet"  # optional, else NA
path_l2   = "~/Downloads/tx_20260930_precincts.parquet" # the state's L2 precinct parquet (Dropbox l2/ folder), else NA
# --------------------------------------------------------------------------------

rate_cols = c("turnout20", "turnout22", "turnout24", "turnoutPred", "age", "male",
              "pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth")

# ---- 1. read ------------------------------------------------------------------
raw = read_csv(path_in, col_types = cols(.default = "c")) |>
  filter(state == STATE) |>
  mutate(
    across(c(n, all_of(rate_cols), pr_harris, pr_trump), as.numeric),
    cd = as.integer(str_extract(cd, "\\d+$"))  # "TX-27" -> 27
  )
cli::cli_h1("{STATE}: {nrow(raw)} rows (precinct x district pieces), {n_distinct(raw$county_name)} counties")

# ---- 2. checks on what the columns mean -----------------------------------------
stopifnot(
  "turnout columns should be rates in [0, 1]" =
    all(between(c(raw$turnout20, raw$turnout22, raw$turnout24, raw$turnoutPred), 0, 1), na.rm = TRUE),
  "cd could not be parsed for some rows" = !anyNA(raw$cd)
)
cli::cli_inform("pr_harris + pr_trump: median {round(median(raw$pr_harris + raw$pr_trump, na.rm = TRUE), 3)}")

na_id = filter(raw, is.na(precinct26_id))
cli::cli_alert_warning("Dropping {nrow(na_id)} rows with no precinct id ({format(sum(na_id$n), big.mark=',')} of {format(sum(raw$n), big.mark=',')} voters)")
raw = filter(raw, !is.na(precinct26_id))

dup_pieces = raw |> count(county_name, precinct26_id, cd) |> filter(n > 1)
if (nrow(dup_pieces) > 0) cli::cli_abort("{nrow(dup_pieces)} duplicate county x precinct x district rows")

# ---- 3. precinct x district table (for House races) -----------------------------
pieces = raw |>
  mutate(
    n_prec = sum(n),
    piece_share = n / n_prec,
    n_cd = n(),
    .by = c(county_name, precinct26_id)
  )

cli::cli_h2("District splits")
pieces |>
  distinct(county_name, precinct26_id, n_cd) |>
  count(n_cd, name = "precincts") |>
  print()
cli::cli_inform("Pieces under 1% of their precinct's voters: {sum(pieces$piece_share < 0.01)} of {nrow(pieces)} (holding {format(sum(pieces$n[pieces$piece_share < 0.01]), big.mark=',')} voters)")
many_cd = pieces |> filter(n_cd >= 6) |> distinct(county_name, precinct26_id, n_cd, n_prec) |> arrange(desc(n_cd))
if (nrow(many_cd) > 0) {
  cli::cli_alert_warning("{nrow(many_cd)} precincts touch 6+ districts (implausible; check with the file's owner):")
  print(many_cd, n = 30)
}

# ---- 4. collapse pieces to one row per precinct ---------------------------------
# rates: weighted by registered voters; Harris/Trump: weighted by expected 2024
# voters among pieces that have an estimate.
prec = pieces |>
  mutate(w_vote = n * coalesce(turnout24, turnoutPred), has_vote = !is.na(pr_harris)) |>
  summarize(
    across(all_of(rate_cols), ~ weighted.mean(.x, n, na.rm = TRUE)),
    pr_harris = if (any(has_vote)) weighted.mean(pr_harris[has_vote], w_vote[has_vote]) else NA_real_,
    pr_trump  = if (any(has_vote)) weighted.mean(pr_trump[has_vote],  w_vote[has_vote]) else NA_real_,
    cd_main = cd[which.max(n)],
    cd_main_share = max(n) / sum(n),
    n_cd = n(),
    n = sum(n),
    .by = c(county_name, precinct26_id)
  ) |>
  mutate(across(where(is.double), ~ if_else(is.nan(.x), NA_real_, .x)))

cli::cli_h2("Precinct level")
cli::cli_inform(c(
  "{nrow(prec)} precincts after combining pieces",
  "Precincts still missing pr_harris: {sum(is.na(prec$pr_harris))} ({format(sum(prec$n[is.na(prec$pr_harris)]), big.mark=',')} voters) -> filled with county average",
  "Precincts missing race shares: {sum(is.na(prec$pred.whi))}"
))

# fill remaining vote-share gaps with the county average (weighted by expected 2024 voters)
prec = prec |>
  mutate(
    w_cty = n * coalesce(turnout24, turnoutPred),
    vote_imputed = is.na(pr_harris),
    pr_harris = coalesce(pr_harris, weighted.mean(pr_harris, w_cty, na.rm = TRUE)),
    pr_trump  = coalesce(pr_trump,  weighted.mean(pr_trump,  w_cty, na.rm = TRUE)),
    .by = county_name
  ) |>
  select(-w_cty)

# ---- 5. geo in the live-scraper format -------------------------------------------
# Precinct ids are written differently by different sources ("0007" vs "7").
# norm_prec_id() is the ONE rule used here and (later) when reading live data:
# trimmed, uppercased, and leading zeros removed from the start of the id
# ("0007" -> "7", "001A" -> "1A"); empty ids become NA.
norm_prec_id = function(x) {
  x = str_squish(toupper(as.character(x)))
  x = str_remove(x, "^0+(?=\\d)")
  if_else(x == "", NA_character_, x)
}
# same rule applied to a full geo id: tx_101_0007 -> tx_101_7
norm_geo = function(geo) {
  parts = str_match(geo, "^([a-z]{2}_\\d{3})_(.*)$")
  if_else(is.na(parts[, 1]), geo, paste0(parts[, 2], "_", norm_prec_id(parts[, 3])))
}

cty = enightmodels::cbs_counties |>
  filter(state == STATE) |>
  transmute(county_key = toupper(county), county_cbs = str_pad(county_cbs, 3, pad = "0"), geostratum)

add_geo = function(d) {
  d |>
    mutate(county_key = county_name |> str_remove(regex("\\s+county$", TRUE)) |> str_squish() |> toupper()) |>
    left_join(cty, by = "county_key") |>
    mutate(geo = paste(tolower(STATE), county_cbs, norm_prec_id(precinct26_id), sep = "_"))
}
prec = add_geo(prec)
bad_cty = prec |> filter(is.na(county_cbs)) |> distinct(county_name)
if (nrow(bad_cty) > 0) cli::cli_abort("County names not found in cbs_counties: {bad_cty$county_name}")
stopifnot(!anyDuplicated(prec$geo))
cli::cli_inform("Example geo ids: {head(prec$geo, 5)}")

# ---- 5b. join L2 precinct covariates (education, vote history, party) -------------
# L2 rows are counts of registered voters per precinct; we convert to shares of n.
l2_cols = character()
if (!is.na(path_l2)) {
  l2 = nanoparquet::read_parquet(path_l2) |>
    as_tibble() |>
    filter(state == STATE) |>
    mutate(
      prec_raw = coalesce(precinct, str_extract(precinct_cbs, "(?<=_).+$")),
      county_key = str_squish(toupper(county)),
      # County-specific id fixes, each verified against live results:
      #  WOOD: L2 writes "1-1"; live results (26_primaries TX_wood_latest) write "11"
      prec_raw = if_else(county_key == "WOOD", str_remove_all(prec_raw, "-"), prec_raw)
    ) |>
    left_join(cty, by = "county_key") |>
    filter(county_cbs %in% prec$county_cbs)      # only counties in the frame

  # voters L2 could not place in a precinct (precinct_cbs like "48001_"): drop + report
  l2_noprec = filter(l2, is.na(norm_prec_id(prec_raw)))
  cli::cli_alert_warning("L2: dropping {nrow(l2_noprec)} county-level rows with no precinct ({format(sum(l2_noprec$n), big.mark=',')} voters)")
  l2 = l2 |>
    filter(!is.na(norm_prec_id(prec_raw))) |>
    mutate(geo = paste(tolower(STATE), county_cbs, norm_prec_id(prec_raw), sep = "_"))

  if (anyDuplicated(l2$geo)) {
    cli::cli_abort("L2 has {sum(duplicated(l2$geo))} duplicate geo ids after normalizing; check precinct ids")
  }

  l2_sh = l2 |>
    transmute(
      geo,
      l2_n = n,
      l2_active = active / n,
      l2_new = new / n,
      # education: college share among voters with KNOWN education (noncol includes unknowns)
      l2_edu_known = (col_known + noncol_known) / n,
      l2_col = col_known / pmax(col_known + noncol_known, 1),
      # race x education, as shares of registered voters (L2-modeled)
      l2_white_col = race1_col / n, l2_white_noncol = race1_noncol / n,
      l2_black_col = race2_col / n, l2_black_noncol = race2_noncol / n,
      l2_hisp_col = race3_col / n,  l2_hisp_noncol = race3_noncol / n,
      # party (TX has no party registration: L2-modeled, use with care)
      l2_dem = dem / n, l2_rep = rep / n, l2_ind = ind / n,
      # turnout history of current registrants
      l2_vote16 = vote16 / n, l2_vote18 = vote18 / n, l2_vote20 = vote20 / n,
      l2_vote22 = vote22 / n, l2_vote24 = vote24 / n,
      l2_prim22 = prim22 / n, l2_prim24 = prim24 / n, l2_prim26 = prim26 / n
    ) |>
    mutate(across(-c(geo, l2_n), \(x) if_else(is.finite(x), x, NA_real_)))
  l2_cols = setdiff(names(l2_sh), "geo")

  # match report, both directions, by county
  cli::cli_h2("L2 join")
  m_frame = mean(prec$geo %in% l2_sh$geo)
  m_l2 = mean(l2_sh$geo %in% prec$geo)
  cli::cli_inform(c(
    "Frame precincts found in L2: {round(100 * m_frame, 1)}% ({sum(!prec$geo %in% l2_sh$geo)} missing, {format(sum(prec$n[!prec$geo %in% l2_sh$geo]), big.mark=',')} voters)",
    "L2 precincts (same counties) found in frame: {round(100 * m_l2, 1)}% ({sum(!l2_sh$geo %in% prec$geo)} missing, {format(sum(l2_sh$l2_n[!l2_sh$geo %in% prec$geo]), big.mark=',')} voters)"
  ))
  prec |>
    mutate(in_l2 = geo %in% l2_sh$geo) |>
    summarize(match_rate = mean(in_l2), n_prec = n(), voters_unmatched = sum(n[!in_l2]), .by = county_key) |>
    filter(match_rate < 0.99) |>
    arrange(match_rate) |>
    print(n = 40)
  cli::cli_inform("Examples missing from L2: {head(setdiff(prec$geo, l2_sh$geo), 6)}")
  cli::cli_inform("Examples missing from frame: {head(setdiff(l2_sh$geo, prec$geo), 6)}")

  prec = left_join(prec, l2_sh, by = "geo")

  # cross-check registered voters: YouGov n vs L2 n (matched precincts)
  chk = filter(prec, !is.na(l2_n))
  cli::cli_inform(c(
    "Registered voters, matched precincts: YouGov {format(sum(chk$n), big.mark=',')} vs L2 {format(sum(chk$l2_n), big.mark=',')}",
    "Correlation of n across precincts: {round(cor(chk$n, chk$l2_n), 3)}; precincts where they differ by >25%: {sum(abs(chk$l2_n / chk$n - 1) > 0.25)}",
    "Turnout check: YouGov turnout22 (weighted) {round(weighted.mean(chk$turnout22, chk$n, na.rm = TRUE), 3)} vs L2 vote22/n {round(sum(chk$l2_vote22 * chk$l2_n, na.rm = TRUE) / sum(chk$l2_n), 3)}"
  ))
}

# ---- 6. baseline expected votes ---------------------------------------------------
prec = prec |>
  mutate(
    est_tot = pmin(turnoutPred * n, n),
    dem2 = (pr_harris + 1e-6) / (pr_harris + pr_trump + 2e-6),  # two-party Dem baseline
    est_dem = est_tot * dem2,
    est_rep = est_tot * (1 - dem2),
    stratum_demo = case_when(
      pred.whi > 0.5 ~ "white",
      pred.his > 0.5 ~ "hisp",
      pred.bla > 0.5 ~ "black",
      pred.asi > 0.5 ~ "asian",
      .default = "mixed"
    ),
    across(c(geostratum, stratum_demo), as.factor)
  )
cli::cli_inform(c(
  "Baseline turnout: {format(round(sum(prec$est_tot)), big.mark = ',')} votes in these {n_distinct(prec$county_key)} counties",
  "Baseline Dem two-party share: {round(weighted.mean(prec$dem2, prec$est_tot), 3)}"
))

# ---- 7. write -----------------------------------------------------------------------
out = prec |>
  select(geo, county = county_key, geostratum, stratum_demo, cd_main, cd_main_share, n_cd, n,
         turnout20, turnout22, turnout24, turnoutPred, age, male, starts_with("pred."),
         pr_harris, pr_trump, vote_imputed, all_of(l2_cols), dem2, est_dem, est_rep) |>
  pivot_longer(c(est_dem, est_rep), names_to = "party", values_to = "est", names_prefix = "est_") |>
  mutate(method = "all")
stopifnot(!anyNA(out$est), all(out$est >= 0), !anyDuplicated(out[c("geo", "party", "method")]))
nanoparquet::write_parquet(out, path_out)
cli::cli_alert_success("Wrote {nrow(out)} rows to {path_out}")

out_cd = pieces |>
  add_geo() |>
  select(geo, county = county_key, cd, n_piece = n, n_prec, piece_share)
stopifnot(all(abs(summarize(out_cd, s = sum(piece_share), .by = geo)$s - 1) < 1e-9))
nanoparquet::write_parquet(out_cd, path_cd)
cli::cli_alert_success("Wrote {nrow(out_cd)} rows to {path_cd}")

# ---- 8. match check vs. live precinct ids ------------------------------------------
if (!is.na(path_snap)) {
  live_geo = nanoparquet::read_parquet(path_snap) |>
    distinct(geo) |>
    mutate(geo = norm_geo(as.character(geo)), county_cbs = str_extract(geo, "(?<=^tx_)\\d{3}"))
  cli::cli_h2("Match vs. live ids")
  cli::cli_inform("Example live geo ids: {head(live_geo$geo, 5)}")
  prec |>
    filter(county_cbs %in% live_geo$county_cbs) |>
    mutate(in_live = geo %in% live_geo$geo) |>
    summarize(match_rate = mean(in_live), n_prec = n(), voters_unmatched = sum(n[!in_live]), .by = county_key) |>
    arrange(match_rate) |>
    print(n = 50)

  # which ids fail, on each side?
  cty_checked = unique(live_geo$county_cbs)
  ours = prec$geo[prec$county_cbs %in% cty_checked]
  cli::cli_inform("Live ids with no match in the frame: {sum(!live_geo$geo %in% ours)} of {nrow(live_geo)}; examples: {head(setdiff(live_geo$geo, ours), 8)}")
  cli::cli_inform("Frame ids with no match in live data: {sum(!ours %in% live_geo$geo)} of {length(ours)}; examples: {head(setdiff(ours, live_geo$geo), 8)}")
}
