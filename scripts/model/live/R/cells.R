# ========================================
## cells.R -- the model's unit of analysis: one row per precinct x vote method.
##
## build_cells("TX") returns, for every precinct in TX_precinct.parquet and every method:
##   geo, county, geostratum, method   identifiers
##   dem2       baseline two-party Dem share (2024; see prep_tx_model_inputs.R for source)
##   oth        baseline third-party share, clamped to [0, oth_cap]
##   exp_votes  expected 2026 votes in the cell: est_turnout x the precinct's method share
##
## The method share is the precinct's share in the mode-history election (2022 general by
## default), shrunk toward its county's share with strength k = 40 votes:
##     share_pm = (v_pm + k * s_cm) / (sum_m' v_pm' + k),  then renormalized over methods
## Precincts with no mode history get their county's split (1 / #methods if that is
## missing too). exp_votes is floored at 0.5.
##
## Moved verbatim from expected_cells() in scripts/model/tests/test_sim_night.R.
## Note, kept as in the original: v and n_p count EVERY method in the mode-history file,
## including any not listed in `methods`, before the shares are renormalized.
# ========================================

build_cells = function(state, dir_26 = find_26_general(), mode_election = "2022_general",
                       methods = c("early", "eday", "mail"), k = 40, oth_cap = 0.25) {
  dir_in = file.path(dir_26, "input_data", "model_inputs")
  inp = read_pq(file.path(dir_in, paste0(state, "_precinct.parquet")))
  mh = read_pq(file.path(dir_in, paste0(state, "_precinct_mode_history.parquet"))) |>
    dplyr::filter(election == mode_election)
  cells_from_inputs(inp, mh, methods = methods, k = k, oth_cap = oth_cap)
}

# inp: one row per precinct (geo, county_name, geostratum, dem2, oth_base, est_turnout)
# mh:  one row per precinct x method for ONE past election (geo, method, voters)
cells_from_inputs = function(inp, mh, methods = c("early", "eday", "mail"), k = 40, oth_cap = 0.25) {
  need = c("geo", "county_name", "geostratum", "dem2", "oth_base", "est_turnout")
  miss = setdiff(need, names(inp))
  if (length(miss)) stop("precinct inputs are missing: ", paste(miss, collapse = ", "))

  sh = mh |> inner_join(select(inp, geo, county = county_name), by = "geo") |> rename(v = voters)
  cty_sh = sh |> summarize(v = sum(v), .by = c(county, method)) |> mutate(cs = v / sum(v), .by = county) |>
    select(county, method, cs)
  inp |>
    transmute(geo, county = county_name, geostratum, dem2, oth = pmin(pmax(oth_base, 0), oth_cap), est_turnout) |>
    cross_join(tibble(method = methods)) |>
    left_join(sh |> mutate(n_p = sum(v), .by = geo) |> select(geo, method, v, n_p), by = c("geo", "method")) |>
    left_join(cty_sh, by = c("county", "method")) |>
    mutate(v = coalesce(v, 0), n_p = coalesce(n_p, 0), cs = coalesce(cs, 1 / length(methods)),
           share = (v + k * cs) / (n_p + k)) |>
    mutate(share = share / sum(share), .by = geo) |>
    mutate(exp_votes = pmax(est_turnout * share, 0.5)) |>
    select(geo, county, geostratum, method, dem2, oth, exp_votes)
}
