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
