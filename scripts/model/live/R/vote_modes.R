# ========================================
## vote_modes.R -- the bucket-label vocabulary, in ONE place.
##
## A reporting BUCKET is a geo whose precinct part is not a precinct: an early-voting site,
## a county mail/absentee pile, a provisional pile, a transfer pile. 31% of AK's and NC's
## vote arrives on one (AK 153,478 of 493,117 in the 2026 primary file; NC 1,743,117 of
## 5,591,547 in 2024 Governor), and `cell_counts()` drops every such row because it joins
## FROM `cells`.
##
## This file exists because the classifier is now needed at TWO times:
##   build time  make_reporting_levels.R, to decide which (state, county, method) report at
##               county/site level and write lookups/reporting_levels.csv
##   run time    live/R/returns.R::bucket_counts(), to turn tonight's dropped bucket rows
##               into county x mode observations
## Two copies of a regex that decides where 31% of two states' votes go is not a risk worth
## taking, so both source this.
# ========================================

# Matching order is the one lookups/README.md fixes for vote_mode_map.csv -- mail -> prov ->
# early -> eday -- so `EV MAIL` reaches mail and `EARLY VOTING PROVISIONAL` reaches prov.
#
# VALIDATED, not guessed: scored against `virtual_precinct` on all 2,908 distinct
# (county, precinct_id) labels in NC 2024 Governor it gets 249 of 250 virtual labels
# (missing only Hoke's `ROCKFISH`, an EV site named after a place) with 2 false positives
# (Caswell `PROVI` and Henderson `EV`, both of which look like buckets and are flagged
# virtual = FALSE upstream). Re-score with make_reporting_levels.R::score_classifier()
# after any edit.
#
# NC is the ONLY state this can be scored on -- it is the only one with both a statewide
# 2024 clean file and site-level reporting. AK's three label forms (`NN-early`,
# `NN-absentee`, `NN-question`) all classify correctly but have no ground truth to score
# against, so bucket_counts() asserts AK's vocabulary explicitly instead.
BUCKET_RX = c(
  mail = "(^|[^A-Z])(ABS|ABSEN[A-Z]*|MAIL|BY[ -]?MAIL)([^A-Z]|$)",
  prov = "(^|[^A-Z])(PROV[A-Z]*|PROVSIONAL|QUESTION[A-Z]*|CHALLENGE[A-Z]*)([^A-Z]|$)",
  early = "(^|[^A-Z])(EV|EARLY[A-Z]*|ONE ?STOP|BOE|CURBSIDE)([^A-Z]|$)|^EV[A-Z]{2,4}$",
  transfer = "(^|[^A-Z])(TRANS[A-Z]*)([^A-Z]|$)"
)

#' Classify a geo label as a reporting bucket
#'
#' Applies `BUCKET_RX` in its fixed order (mail, prov, early, transfer).
#'
#' @param label Character vector of precinct labels (the part of a geo after the county code).
#' @return Character vector: "mail", "prov", "early" or "transfer"; NA for anything that
#'   looks like a precinct.
bucket_class = function(label) {
  u = toupper(label)
  dplyr::case_when(
    stringr::str_detect(u, BUCKET_RX[["mail"]]) ~ "mail",
    stringr::str_detect(u, BUCKET_RX[["prov"]]) ~ "prov",
    stringr::str_detect(u, BUCKET_RX[["early"]]) ~ "early",
    stringr::str_detect(u, BUCKET_RX[["transfer"]]) ~ "transfer",
    TRUE ~ NA_character_
  )
}

# bucket class -> the MODEL's mode. `prov` and `transfer` are not in
# enightmodels::VOTE_METHODS and are not modes of their own:
#   prov      arrives after the canvass and is folded into eday everywhere (the per-state
#             notes in lookups/mode_scope.csv say so for AK, FL, NC and TX)
#   transfer  a county's transfer pile. Its votes ARE eday votes -- 254 statewide in NC
#             2024, so the fold is immaterial in magnitude but has to go somewhere
# Kept as a named vector rather than inlined so the fold is greppable and testable.
BUCKET_FOLD = c(mail = "mail", early = "early", prov = "eday", transfer = "eday")

#' County code of a canonical geo
#'
#' `{st}_{county_cbs}_{precinct_id}` (CONTRACT.md section 0) -> the 3-digit `county_cbs`.
#' Used on both sides of the county bound so the model never joins a county name to a
#' county number. In AK the code is a house district, not a borough.
#'
#' @param geo Character (or factor) vector of geo ids.
#' @return Character vector of 3-digit codes; NA where the geo has none.
geo_county = function(geo) stringr::str_extract(as.character(geo), "(?<=_)[0-9]{3}(?=_)")

#' Precinct part of a canonical geo
#'
#' @param geo Character (or factor) vector of geo ids.
#' @return Character vector: the geo with its `{st}_{county}_` prefix removed.
geo_label = function(geo) stringr::str_replace(as.character(geo), "^[a-z]{2}_[0-9]+_", "")

# ---- CBS method vocabulary -> the methods a county's feed actually reports --------------
# CBS's county expected-vote feed carries its own method vocabulary, and a county's feed
# does not always have a geo for every method CBS expects. In OH's 2026 zero files, of the
# 9 counties that report a mode split at all: 6 report early+eday+mail, Franklin (025) and
# Fulton (026) report eday+mail with NO early geo, and Hamilton (031) reports early+eday
# with NO mail geo. (The other 5 OH counties report precinct totals only; method_counties()
# in run.R already keeps those out of the by-method fit.)
#
# An expectation on a mode a county never reports is not a rounding error, it is two
# defects at once:
#   - the cell can never be counted out, so it stands as pure gross-up for the whole night
#     (Hamilton: 30.3% of the county's prior, 92,149 votes)
#   - the mode the county DOES report is left with a near-zero prior, and the completeness
#     gate fires at COMPLETE_FRAC * prior/true, so that cell pins almost immediately
#     (Hamilton early 0.26% of prior, Cuyahoga early 0.33%)
#
# The fold is a HARD-CODED table, lookups/mode_fold.csv -- not an eday/non-eday aggregation
# worked out at run time. Which method lands on which is a per-county determination, so it
# belongs in a file a person reads and git diffs, exactly like reporting_levels.csv. At run
# time this is a plain join: no inference, no renormalizing over whatever happens to be
# present in this cycle's feed.
#
# The columns are `from_method` -> `to_method`, deliberately NOT named for a source. The
# fold is a property of the COUNTY's feed, not of whoever supplied the prior: it is applied
# to the L2 mode history today and will be applied to CBS's county expected vote when that
# lands, with the same rows. An earlier name of `cbs_method` read as though the table
# compared L2 against CBS, which it never did.
#
# Anything absent from the table is the IDENTITY (eday -> eday, early -> early, ...), which
# is every county in TX, GA, FL and NC. check_mode_fold() re-derives each county's
# vocabulary from the zero files and fails when a county needs a fold the table lacks, so a
# new exception cannot land silently.
P_MODE_FOLD = "scripts/model/lookups/mode_fold.csv"

#' The hard-coded prior-method -> feed-method fold
#'
#' One row per county whose feed vocabulary cannot carry the method the prior names.
#' Absent = identity. Source-agnostic: `from_method` is whatever the prior calls it.
#'
#' @param path Path to `lookups/mode_fold.csv`.
#' @return Tibble: state, county_cbs (3-digit, zero-padded), from_method, to_method.
read_mode_fold = function(path = P_MODE_FOLD) {
  if (!file.exists(path)) {
    stop("mode fold table not found: ", path)
  }
  f = readr::read_csv(path, col_types = readr::cols(.default = "c")) |>
    dplyr::mutate(county_cbs = stringr::str_pad(county_cbs, 3, pad = "0"))
  need = c("state", "county_cbs", "from_method", "to_method")
  miss = setdiff(need, names(f))
  if (length(miss)) {
    stop(basename(path), " is missing: ", paste(miss, collapse = ", "))
  }
  dup = f |> dplyr::count(state, county_cbs, from_method) |> dplyr::filter(n > 1)
  if (nrow(dup)) {
    stop(
      basename(path),
      " maps the same method twice: ",
      paste(dup$state, dup$county_cbs, dup$from_method, collapse = "; ")
    )
  }
  # a row that changes nothing is a mistake, not a no-op: it means someone meant to fold
  # and wrote the same value on both sides
  same = dplyr::filter(f, from_method == to_method)
  if (nrow(same)) {
    stop(
      basename(path),
      " has rows whose from_method equals to_method (fold does nothing): ",
      paste(same$state, same$county_cbs, same$from_method, collapse = "; ")
    )
  }
  dplyr::select(f, state, county_cbs, from_method, to_method)
}

#' Apply the fold to a county x method table
#'
#' Remaps `method` per `lookups/mode_fold.csv` and sums `values` within the result, so a
#' folded method collapses onto the method it lands on (Hamilton's mail onto its early).
#' Rows whose county x method is not in the table are untouched. `x$method` is whatever
#' vocabulary the caller's prior uses -- the L2 history, or CBS once it is wired in.
#'
#' @param x Tibble with `county_cbs` and `method` plus `values`.
#' @param state Two-letter state code.
#' @param values Numeric columns to sum after the remap.
#' @param fold The fold table.
#' @return `x` with `method` folded and `values` summed; one row per county x method.
fold_methods = function(x, state, values, fold = read_mode_fold()) {
  need = c("county_cbs", "method", values)
  miss = setdiff(need, names(x))
  if (length(miss)) {
    stop("fold_methods(): input is missing ", paste(miss, collapse = ", "))
  }
  keys = setdiff(names(x), values)
  f = dplyr::filter(fold, state == !!state)
  x |>
    dplyr::mutate(county_cbs = stringr::str_pad(as.character(county_cbs), 3, pad = "0")) |>
    dplyr::left_join(
      dplyr::select(f, county_cbs, from_method, .to = to_method),
      by = c("county_cbs", method = "from_method")
    ) |>
    dplyr::mutate(method = dplyr::coalesce(.to, method), .to = NULL) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(values),
        \(v) if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
      ),
      .by = dplyr::all_of(keys)
    )
}

#' Which methods each county's feed reports, from the zero files
#'
#' The vocabulary comes from the ZERO FILE -- every precinct x method the county will
#' report -- not from live returns, which early in the night simply have not shown every
#' mode yet. Counties whose only label is `all` report precinct totals and are excluded:
#' `method_counties()` (run.R) already keeps them out of the by-method fit.
#'
#' @param state Two-letter state code.
#' @param dir_26 Path to 26_general.
#' @return Tibble: county_cbs, method (one row per county x method the feed reports).
feed_methods = function(state, dir_26 = find_26_general()) {
  fs = Sys.glob(file.path(dir_26, state, "clean", "*_latest.parquet"))
  if (!length(fs)) {
    stop("no zero/clean files for ", state, " in ", file.path(dir_26, state, "clean"))
  }
  lapply(fs, function(f) {
    x = tryCatch(nanoparquet::read_parquet(f), error = function(e) NULL)
    if (is.null(x)) {
      return(NULL)
    }
    dplyr::tibble(
      county_cbs = geo_county(x$geo),
      method = unname(METHOD_MAP[as.character(x$method)])
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(county_cbs), !is.na(method)) |>
    dplyr::distinct(county_cbs, method)
}

#' Check the fold table against the feed vocabularies
#'
#' For every county that reports a mode split, each of `prior_methods` must either be a method
#' that county reports or have a row in `lookups/mode_fold.csv` pointing it at one that it
#' does. This is what stops a new exception county from landing silently: the table is
#' hard-coded, so something has to notice when the world no longer matches it.
#'
#' @param state Two-letter state code.
#' @param dir_26 Path to 26_general.
#' @param prior_methods The methods the prior names, i.e. what has to land somewhere.
#' @param fold The fold table.
#' @param abort Stop on a problem; FALSE returns the rows instead.
#' @return Tibble of problems (invisibly when empty): county_cbs, from_method, issue.
check_mode_fold = function(
  state,
  dir_26 = find_26_general(),
  prior_methods = c("early", "eday", "mail"),
  fold = read_mode_fold(),
  abort = TRUE
) {
  fm = feed_methods(state, dir_26)
  f = dplyr::filter(fold, state == !!state)
  have = split(fm$method, fm$county_cbs)
  bad = list()
  for (cc in names(have)) {
    vocab = have[[cc]]
    for (m in prior_methods) {
      if (m %in% vocab) {
        next # CBS's method is one this county reports: identity, nothing to do
      }
      to = f$to_method[f$county_cbs == cc & f$from_method == m]
      if (!length(to)) {
        bad[[length(bad) + 1]] = dplyr::tibble(
          county_cbs = cc,
          from_method = m,
          issue = paste0(
            "county reports ",
            paste(sort(vocab), collapse = "+"),
            " -- needs a mode_fold.csv row for ", m
          )
        )
      } else if (!to %in% vocab) {
        bad[[length(bad) + 1]] = dplyr::tibble(
          county_cbs = cc,
          from_method = m,
          issue = paste0(
            "mode_fold.csv folds ", m, " -> ", to,
            ", but the county reports only ", paste(sort(vocab), collapse = "+")
          )
        )
      }
    }
  }
  # a fold row for a county that no longer needs one is stale, and says the table and the
  # feed have drifted apart even though nothing is currently mis-allocated
  stale = dplyr::inner_join(
    dplyr::select(f, county_cbs, from_method),
    dplyr::distinct(fm, county_cbs, method),
    by = c("county_cbs", from_method = "method")
  )
  if (nrow(stale)) {
    bad[[length(bad) + 1]] = dplyr::tibble(
      county_cbs = stale$county_cbs,
      from_method = stale$from_method,
      issue = "stale fold row: the county now reports this method itself"
    )
  }
  out = dplyr::bind_rows(bad)
  if (nrow(out) && abort) {
    stop(
      state,
      ": mode_fold.csv does not match the feed vocabulary\n",
      paste0("  ", out$county_cbs, " ", out$from_method, ": ", out$issue, collapse = "\n")
    )
  }
  if (nrow(out)) out else invisible(out)
}

#' Cache tag for a state's fold rows
#'
#' Empty for a state with no rows, so every existing `medsl_key()` is unchanged and only a
#' folded state gets a new cache entry. Spelled out rather than hashed: a cache key that
#' says `031mail>early` is one a person can read when a cached fit looks wrong, and the
#' table is small enough that there is no reason to compress it.
#'
#' @param state Two-letter state code.
#' @param fold The fold table.
#' @return "" or "|fold<rows>".
mode_fold_tag = function(state, fold = read_mode_fold()) {
  f = dplyr::filter(fold, state == !!state)
  if (!nrow(f)) {
    return("")
  }
  f = dplyr::arrange(f, county_cbs, from_method)
  paste0("|fold", paste0(f$county_cbs, f$from_method, ">", f$to_method, collapse = ","))
}

# ---- imposing a county composition target on a precinct pattern ------------------------
#' Rescale a precinct x method pattern to county method shares
#'
#' Keeps WHICH precincts vote by which method (the pattern) and replaces HOW MUCH of each
#' county does (the level). Each county's resulting method share equals `target`'s, and the
#' county's total is unchanged -- only the split across methods moves.
#'
#' This is the shape `prep/make_mode_results.R` already uses to put a 2024 precinct pattern
#' at a 2022 county split (`v * ts * cv / cmv`), lifted out so the target can come from
#' anywhere. The three cases are its three cases:
#'   county absent from the target   keep the pattern untouched
#'   method present in the pattern   rescale it to the target share
#'   method absent from the pattern  spread the target share by precinct size
#'
#' Blending rather than imposing would not work. `cells_from_inputs()` already shrinks a
#' precinct toward its county share with `share = (v + k * cs) / (n_p + k)`, k = 40, which
#' gives `cs` about 7% weight on a precinct with a few hundred history votes -- roughly
#' 0.35pp of movement against a measured 4-8pp gap. The target has to be imposed.
#'
#' `pattern` and `target` must already speak the same vocabulary: fold BOTH through
#' `fold_methods()` first, or a county whose feed cannot report a method will be handed a
#' share for it.
#'
#' @param pattern One row per precinct x method: geo, method, voters, plus any columns to
#'   carry (e.g. election), which are taken from the precinct's first row.
#' @param target One row per county x method: county_cbs, method, share. NULL or empty
#'   returns `pattern` unchanged.
#' @return `pattern`'s columns, with `voters` rescaled.
rescale_modes = function(pattern, target) {
  if (is.null(target) || !nrow(target)) {
    return(pattern)
  }
  stopifnot(
    "pattern needs geo, method, voters" = all(c("geo", "method", "voters") %in% names(pattern)),
    "target needs county_cbs, method, share" =
      all(c("county_cbs", "method", "share") %in% names(target))
  )
  cols = names(pattern)
  carry = setdiff(cols, c("geo", "method", "voters"))

  # defensive: a target that does not sum to 1 within county would move the county's total
  tg = target |>
    dplyr::mutate(county_cbs = stringr::str_pad(as.character(county_cbs), 3, pad = "0")) |>
    dplyr::filter(!is.na(county_cbs), !is.na(method), is.finite(share), share >= 0) |>
    dplyr::mutate(share = share / sum(share), .by = county_cbs) |>
    dplyr::filter(is.finite(share))

  p = dplyr::mutate(
    pattern,
    county_cbs = geo_county(geo),
    voters = as.numeric(voters)
  )
  hit = intersect(unique(p$county_cbs), unique(tg$county_cbs))
  if (!length(hit)) {
    return(pattern)
  }
  keep = dplyr::filter(p, !county_cbs %in% hit)
  work = dplyr::filter(p, county_cbs %in% hit)

  # a method the target names but the pattern has no row for still needs a row to land on,
  # so complete the grid over the TARGET's methods for each precinct. The target is folded,
  # so this cannot invent a method the county's feed does not report.
  spine = work |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(c("geo", "county_cbs", carry)))) |>
    dplyr::slice_head(n = 1, by = geo)
  grid = spine |>
    dplyr::inner_join(
      dplyr::distinct(tg, county_cbs, method),
      by = "county_cbs",
      relationship = "many-to-many"
    )
  work = grid |>
    dplyr::left_join(
      dplyr::select(work, geo, method, voters),
      by = c("geo", "method")
    ) |>
    dplyr::mutate(voters = dplyr::coalesce(voters, 0))

  out = work |>
    dplyr::left_join(
      dplyr::select(tg, county_cbs, method, .ts = share),
      by = c("county_cbs", "method")
    ) |>
    dplyr::mutate(
      .pt = sum(voters), # the precinct's own total, for the no-pattern case
      .by = geo
    ) |>
    dplyr::mutate(
      .cv = sum(voters), # county total: preserved
      .by = county_cbs
    ) |>
    dplyr::mutate(
      .cmv = sum(voters), # county total for this method
      .by = c(county_cbs, method)
    ) |>
    dplyr::mutate(
      voters = dplyr::case_when(
        is.na(.ts) ~ voters, # target silent on this method: leave it
        .cmv > 0 ~ voters * .ts * .cv / .cmv, # the pattern, at the target's share
        TRUE ~ .ts * .pt # method absent from the pattern: by precinct size
      )
    ) |>
    dplyr::select(dplyr::all_of(cols))

  dplyr::bind_rows(dplyr::select(keep, dplyr::all_of(cols)), out)
}
