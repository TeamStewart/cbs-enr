# ========================================
## cells.R -- the model's unit of analysis: one row per precinct x vote method.
##
## build_cells("TX") returns, for every precinct in TX_precinct.parquet and every method:
##   geo, county, geostratum, method   identifiers
##   dem2       baseline two-party Dem share (2024; prep/prep_model_inputs.R)
##   oth        baseline third-party share, clamped to [0, oth_cap]
##   exp_votes  expected 2026 votes in the cell: est_turnout x the precinct's method share
##
## The method share is the precinct's share in the mode-history election (see mode_history():
## the actual 2024 results where {ST}_precinct_mode_results.parquet exists, e.g. NC; else 2022 general by
## default), shrunk toward its county's share with strength k = 40 votes:
##     share_pm = (v_pm + k * s_cm) / (sum_m' v_pm' + k),  then renormalized over methods
## Precincts with no mode history get their county's split (the state's split if the county
## has none; 1 / #methods if that is
## missing too). exp_votes is floored at 0.5.
##
## Note: v and n_p count EVERY method in the mode-history file, including any not listed in
## `methods`, before the shares are renormalized.
# ========================================

#' Build a state's precinct x vote-method cells
#'
#' The models' unit of analysis: one row per precinct in `{ST}_precinct.parquet` x method.
#' Each precinct's method share is its share in the mode-history election
#' (`mode_history()`), shrunk toward its county's share (`cells_from_inputs()`).
#'
#' No method split is assumed: if the history has no votes for one of `methods` (MI: no
#' early in-person in any L2 year), the state is modeled without vote method -- one cell per
#' precinct, method "all", `exp_votes = est_turnout` -- and `run_race()` fits on precinct
#' totals.
#'
#' @param state Two-letter state code.
#' @param dir_26 Path to 26_general.
#' @param mode_election Mode-history election to use (e.g. "2022_general"). NULL uses
#'   `{ST}_precinct_mode_results.parquet` if it exists, else 2022 general.
#' @param methods Vote methods to build cells for.
#' @param k Shrinkage strength toward the county split, in votes.
#' @param oth_cap Upper clamp on the third-party baseline share.
#' @return Tibble: geo, county, geostratum, method, dem2 (baseline two-party Dem share),
#'   oth (baseline third-party share), exp_votes (expected 2026 votes in the cell).
build_cells = function(
  state,
  dir_26 = find_26_general(),
  mode_election = NULL,
  methods = c("early", "eday", "mail"),
  k = 40,
  oth_cap = 0.25
) {
  dir_in = file.path(dir_26, "input_data", "model_inputs")
  inp = read_pq(file.path(dir_in, paste0(state, "_precinct.parquet")))
  mh = mode_history(state, dir_26, mode_election)
  # No assumed splits: if the history has no votes at all for one of the methods (MI: no
  # early in-person in any L2 year), the state is modeled WITHOUT vote method -- one cell
  # per precinct, method "all", exp_votes = est_turnout -- rather than guessing that
  # method's share. run_race() then fits on precinct totals (see shape_for_method()).
  have = unique(mh$method[mh$voters > 0])
  if (!all(methods %in% have)) {
    message(
      state,
      ": vote-method history has no ",
      paste(setdiff(methods, have), collapse = ", "),
      " votes, so ",
      state,
      " is modeled without vote method"
    )
    methods = "all"
  }
  fold_cells(cells_from_inputs(inp, mh, methods = methods, k = k, oth_cap = oth_cap), state)
}

#' Drop cells for methods a county's feed never reports, keeping its expected vote
#'
#' `fold_mode_history()` folds the history, but that is not enough on its own:
#' `cells_from_inputs()` builds a cell for every method in `methods` and fills a
#' (county, method) with no history from the STATE split, which puts back the exact mode the
#' fold just removed. Hamilton came out of the history with no `mail` and went back into the
#' cells with a state-share `mail` cell.
#'
#' So the same hard-coded table masks the cell frame. Each dropped cell's expected votes are
#' NOT discarded: the precinct's remaining cells are rescaled to its original total, so
#' turnout is unchanged and only the attribution moves. Deleting absent-mode cells outright
#' is the mistake that cost 31% of NC's turnout -- those cells are the gross-up mechanism.
#'
#' This is sound only where the folded mode's votes really do arrive under the method they
#' fold onto (Hamilton files its absentee votes in its `early` rows). It is NOT a way to
#' drop a mode that reports at county/site level instead of precinct level -- that is
#' `reporting_levels.csv` and `virtual.R`'s job, and NC is the cautionary case.
#'
#' @param cells Cells from `cells_from_inputs()`.
#' @param state Two-letter state code.
#' @param fold The fold table.
#' @return `cells` without the folded-away county x method rows.
fold_cells = function(cells, state, fold = read_mode_fold()) {
  f = dplyr::filter(fold, state == !!state)
  if (!nrow(f)) {
    return(cells)
  }
  drop = dplyr::distinct(dplyr::transmute(f, county_cbs, method = cbs_method))
  cols = names(cells)
  cells |>
    dplyr::mutate(county_cbs = geo_county(geo)) |>
    dplyr::mutate(.tot = sum(exp_votes), .by = geo) |>
    dplyr::anti_join(drop, by = c("county_cbs", "method")) |>
    dplyr::mutate(
      exp_votes = if (sum(exp_votes) > 0) exp_votes * .tot / sum(exp_votes) else exp_votes,
      .by = geo
    ) |>
    dplyr::select(dplyr::all_of(cols))
}

#' One past election's votes by precinct x method
#'
#' Used for each precinct's method split. Reads `{ST}_precinct_mode_results.parquet` when it
#' exists and `mode_election` is NULL: the split from actual results
#' (prep/make_mode_results.R), for states whose L2 history mislabels methods (NC). Otherwise
#' `{ST}_precinct_mode_history.parquet` (prep/prep_model_inputs.R, from L2).
#'
#' Either source is then folded onto the methods each county's feed actually reports
#' (`fold_mode_history()`), so the prior never puts weight on a mode a county never files.
#'
#' @param state Two-letter state code.
#' @param dir_26 Path to 26_general.
#' @param mode_election Election to keep from the L2 history; NULL = "2022_general".
#' @return Tibble with geo, method, voters (and election).
mode_history = function(state, dir_26 = find_26_general(), mode_election = NULL) {
  dir_in = file.path(dir_26, "input_data", "model_inputs")
  f_res = file.path(dir_in, paste0(state, "_precinct_mode_results.parquet"))
  mh = if (file.exists(f_res) && is.null(mode_election)) {
    read_pq(f_res)
  } else {
    read_pq(file.path(dir_in, paste0(state, "_precinct_mode_history.parquet"))) |>
      dplyr::filter(election == if (is.null(mode_election)) "2022_general" else mode_election)
  }
  fold_mode_history(mh, state)
}

#' Fold a mode history onto the methods each county's feed reports
#'
#' Applied to EVERY source `mode_history()` can return, which is why it lives here rather
#' than in the scripts that build those files: `{ST}_precinct_mode_results.parquet` and the
#' L2 history both pass through, so all three models see one split (the property
#' make_mode_results.R's header claims) and a new source cannot skip the fold by being
#' added later.
#'
#' The remap is per precinct, so each precinct's own pattern is kept and the folded method
#' collapses onto the one it lands on: OH Hamilton's `mail` votes become its `early` votes,
#' precinct by precinct, leaving the county's total untouched.
#'
#' A state with no rows in `lookups/mode_fold.csv` returns **unchanged** -- not recomputed,
#' returned as it came in -- so TX, GA, FL and NC are bit-identical and only OH moves.
#'
#' @param mh A mode history: geo, method, voters (and election).
#' @param state Two-letter state code.
#' @param fold The fold table (`read_mode_fold()`).
#' @return `mh` with its methods folded, same columns.
fold_mode_history = function(mh, state, fold = read_mode_fold()) {
  if (!nrow(dplyr::filter(fold, state == !!state))) {
    return(mh)
  }
  cols = names(mh)
  mh |>
    dplyr::mutate(county_cbs = geo_county(geo), voters = as.numeric(voters)) |>
    fold_cbs_methods(state, values = "voters", fold = fold) |>
    dplyr::select(dplyr::all_of(cols))
}

#' Cells from precinct inputs and a mode history
#'
#' Method share per precinct: `share_pm = (v_pm + k * s_cm) / (sum_m' v_pm' + k)`, then
#' renormalized over `methods`, where `s_cm` is the county's share. A precinct with no
#' history gets its county's split; a county with none gets the state's split; then
#' `1 / length(methods)`. `exp_votes = est_turnout * share`, floored at 0.5. `v` and the
#' precinct total count every method in `mh`, including any not in `methods`.
#'
#' @param inp One row per precinct: geo, county_name, geostratum, dem2, oth_base, est_turnout.
#' @param mh One row per precinct x method for one past election: geo, method, voters.
#' @param methods Vote methods to build cells for.
#' @param k Shrinkage strength toward the county split, in votes.
#' @param oth_cap Upper clamp on the third-party baseline share.
#' @return Tibble as `build_cells()`.
cells_from_inputs = function(
  inp,
  mh,
  methods = c("early", "eday", "mail"),
  k = 40,
  oth_cap = 0.25
) {
  need = c("geo", "county_name", "geostratum", "dem2", "oth_base", "est_turnout")
  miss = setdiff(need, names(inp))
  if (length(miss)) {
    stop("precinct inputs are missing: ", paste(miss, collapse = ", "))
  }

  sh = mh |> inner_join(select(inp, geo, county = county_name), by = "geo") |> rename(v = voters)
  cty_sh = sh |>
    summarize(v = sum(v), .by = c(county, method)) |>
    mutate(cs = v / sum(v), .by = county) |>
    select(county, method, cs)
  # a county with no history at all gets the state's split (not an even split)
  sw_t = sh |> filter(method %in% methods) |> summarize(v = sum(v), .by = method)
  sw = setNames(sw_t$v / sum(sw_t$v), sw_t$method)
  inp |>
    transmute(
      geo,
      county = county_name,
      geostratum,
      dem2,
      oth = pmin(pmax(oth_base, 0), oth_cap),
      est_turnout
    ) |>
    cross_join(tibble(method = methods)) |>
    left_join(
      sh |> mutate(n_p = sum(v), .by = geo) |> select(geo, method, v, n_p),
      by = c("geo", "method")
    ) |>
    left_join(cty_sh, by = c("county", "method")) |>
    mutate(
      v = coalesce(v, 0),
      n_p = coalesce(n_p, 0),
      cs = coalesce(cs, sw[method], 1 / length(methods)),
      share = (v + k * cs) / (n_p + k)
    ) |>
    mutate(share = share / sum(share), .by = geo) |>
    mutate(exp_votes = pmax(est_turnout * share, 0.5)) |>
    select(geo, county, geostratum, method, dem2, oth, exp_votes)
}
