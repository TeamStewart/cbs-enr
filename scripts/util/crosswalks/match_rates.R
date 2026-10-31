# ========================================
## Crosswalk match rates against the feed, for every state with clean files. Two maps,
## measured separately:
##   l2       input_data/crosswalks/{st}.csv: ENR id -> frame precinct -> L2 precinct
##   votehub  input_data/crosswalks/votehub/{st}.csv: ENR id -> VoteHub unit
## Per state x map, over the feed counties the map covers:
##   rate     share of the feed's distinct precinct geos that match
##   present  share of the map's matched ENR ids found in the feed (100% right after a build)
## plus the feed counties the map doesn't cover at all.
##
## The ENR cycle (phase 3, after the crosswalk and votehub.R rebuilds) writes the baseline:
##   Rscript scripts/util/crosswalks/match_rates.R            # print vs the current baseline
##   Rscript scripts/util/crosswalks/match_rates.R --write    # and write it
## to docs/enr_cycle/crosswalk_match_baseline.csv. The election-night watcher
## (check_crosswalk_match() in scripts/watch/checks.R) sources this file and compares
## each cycle's rates with that baseline. Uncovered counties are listed in the baseline,
## so the watcher reports only new ones; fixing them is ENR-cycle work.
# ========================================

suppressPackageStartupMessages(library(data.table))

XW_BASELINE = "docs/enr_cycle/crosswalk_match_baseline.csv"
XW_SOURCES = list(
  l2 = list(file = function(st) file.path("crosswalks", paste0(tolower(st), ".csv")), col = "precinct_l2", label = "L2"),
  votehub = list(file = function(st) file.path("crosswalks", "votehub", paste0(tolower(st), ".csv")), col = "unit_id", label = "VoteHub")
)

# county_key() of scripts/util/crosswalks/helpers.R, plus & -> AND (VA "KING & QUEEN")
xw_county = function(st, x) {
  if (st == "AK") return(rep("AK", length(x)))
  gsub("[^A-Z]", "", gsub("&", "AND", sub(" COUNTY$", "", toupper(x))))
}
# the precinct part as norm_live_geo() treats it
xw_prec = function(x) sub("^0+(?=\\d)", "", sub("\\.0$", "", gsub("\\s+", " ", trimws(toupper(x)))), perl = TRUE)

# the states' non-empty clean files, named by state
clean_files = function(d26) {
  f = Sys.glob(file.path(d26, "*", "clean", "*_latest.parquet"))
  f = f[grepl("^[A-Z]{2}$", basename(dirname(dirname(f)))) & file.size(f) > 0]
  setNames(f, basename(dirname(dirname(f))))
}

#' Match rates for one state
#'
#' @param st Two-letter state.
#' @param g The state's distinct feed geos (`{st}_{ddd}_{precinct}`).
#' @param d26 Path to 26_general.
#' @return Named list, one element per map the state has a file for: n (feed precincts in
#'   covered counties), k (of those, matched), rate, xw_n (map's matched ids in feed
#'   counties), present, n_uncovered, uncovered (county names), missing (data.table ck, N:
#'   the map's ids not in the feed, by county), missing_ids (examples), file, mtime.
xw_match_state = function(st, g, d26) {
  cf = as.data.table(enightmodels::cbs_counties)[state == st, .(county = toupper(county), cbs = as.integer(county_cbs))]
  m = stringr::str_match(g, "^[a-z]{2}_(\\d+)_(.*)$")
  fd = data.table(cbs = as.integer(m[, 2]), p = m[, 3])[!is.na(cbs)]
  fd = merge(fd, cf, by = "cbs", all.x = TRUE)
  fd[is.na(county), county := paste0("cbs ", cbs)]
  fd[, ck := xw_county(st, county)]
  out = list()
  for (nm in names(XW_SOURCES)) {
    s = XW_SOURCES[[nm]]
    path = file.path(d26, "input_data", s$file(st))
    if (!file.exists(path)) next
    x = fread(path, colClasses = "character", select = c("county", "precinct_enr", s$col))
    setnames(x, s$col, "id")
    x = unique(x[!is.na(precinct_enr) & nzchar(precinct_enr) & !is.na(id) & nzchar(id),
                 .(ck = xw_county(st, county), p = xw_prec(precinct_enr))])
    # a feed county the map spells "<name> CITY" (VA independent cities)
    fck = ifelse(!fd$ck %in% x$ck & paste0(fd$ck, "CITY") %in% x$ck, paste0(fd$ck, "CITY"), fd$ck)
    key = paste(fck, xw_prec(fd$p))
    covered = fck %in% x$ck
    xs = x[ck %in% fck]
    xs[, in_feed := paste(ck, p) %in% key]
    n = sum(covered)
    k = sum(covered & key %in% paste(x$ck, x$p))
    out[[nm]] = list(
      n = n, k = k, rate = if (n) k / n else NA_real_,
      xw_n = nrow(xs), present = if (nrow(xs)) mean(xs$in_feed) else NA_real_,
      n_uncovered = sum(!covered), uncovered = sort(unique(fd$county[!covered])),
      missing = xs[in_feed == FALSE, .N, by = ck][order(-N)],
      missing_ids = head(xs[in_feed == FALSE, paste(ck, p)], 10),
      file = s$file(st), mtime = file.mtime(path)
    )
  }
  out
}

#' Match rates for every state, as one row per state x map
#'
#' @param d26 Path to 26_general.
#' @param geos Named list of feed geos per state; NULL reads every state's clean files.
xw_match_table = function(d26, geos = NULL) {
  if (is.null(geos)) {
    f = clean_files(d26)
    geos = lapply(split(f, names(f)), \(ff) unique(unlist(lapply(ff, \(x) as.character(nanoparquet::read_parquet(x, col_select = "geo")$geo)))))
  }
  rbindlist(lapply(sort(names(geos)), function(st) {
    r = xw_match_state(st, geos[[st]], d26)
    rbindlist(lapply(names(r), \(nm) with(r[[nm]], data.table(
      state = st, map = nm, n_feed = n, n_match = k, rate = round(rate, 4), n_xw = xw_n, present = round(present, 4),
      n_uncovered = n_uncovered, uncovered = paste(uncovered, collapse = "; "),
      xw_file = file, xw_mtime = format(mtime, "%Y-%m-%d %H:%M")
    ))))
  }))
}

read_xw_baseline = function(path = XW_BASELINE) {
  if (!file.exists(path)) return(NULL)
  fread(path, colClasses = list(character = c("uncovered", "xw_mtime", "built")), na.strings = "")
}

if (sys.nframe() == 0) {
  source("scripts/model/live/R/paths.R")
  d26 = find_26_general()
  tab = xw_match_table(d26)
  old = read_xw_baseline()
  show = copy(tab)
  if (!is.null(old)) {
    show = merge(show, old[, .(state, map, was_rate = rate, was_present = present, was_uncovered = n_uncovered)],
                 by = c("state", "map"), all.x = TRUE)
  }
  options(width = 200)
  print(show[, !c("uncovered", "xw_file")], nrows = Inf)
  if (!is.null(old)) {
    down = show[!is.na(was_rate) & (rate < was_rate - 0.005 | present < was_present - 0.005)]
    if (nrow(down)) { cat("\nDROPPED since the last baseline (inspect before --write):\n"); print(down[, .(state, map, rate, was_rate, present, was_present)]) }
  }
  noxw = setdiff(names(clean_files(d26)), tab$state)
  cat("\nNo crosswalk (not checked):", paste(sort(unique(noxw)), collapse = ", "), "\n")
  cat("Uncovered feed counties (crosswalk to-do):\n")
  for (i in which(tab$n_uncovered > 0)) cat(sprintf("  %s %-7s %5d precincts: %s\n", tab$state[i], tab$map[i], tab$n_uncovered[i], tab$uncovered[i]))
  if ("--write" %in% commandArgs(TRUE)) {
    tab[, built := format(Sys.time(), "%Y-%m-%d %H:%M")]
    fwrite(tab, XW_BASELINE)
    cat("\nWrote", XW_BASELINE, "\n")
  }
}
