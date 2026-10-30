# ========================================
## measure_complete_frac.R -- how wrong is prior_tau, and what does that imply for
## COMPLETE_FRAC (model_turnout.R:76, the fallback completeness test
## `tau >= COMPLETE_FRAC * prior_tau`)?
##
##   Rscript scripts/model/prep/measure_complete_frac.R [--pairs=2018:2022,2022:2024]
##
## THE DECOMPOSITION this rests on. For a counted-out cell,
##     log(c_im / prior_tau_im) = log(T_i / est_tot_i) + log(q_im / p_im)
##                              =        L_i           +       e_im
## L_i is the precinct LEVEL error (no mode variation); e_im is the COMPOSITION error
## (sums to ~0 across modes within a precinct). A single COMPLETE_FRAC can only work if
## the per-mode means of e_im are near zero -- otherwise one threshold is simultaneously
## too strict for one mode and too loose for another, which is what TX 2024 showed.
##
## WHY THIS IS COVERAGE-ROBUST. The L2 mode history counts CURRENT registrants who voted
## in a past election, so it misses voters since purged (NC 2022: 91.5% of official). That
## shortfall is a precinct-level factor, so it cancels out of q_im and p_im -- both are
## within-precinct shares. e_im is therefore clean. L_i is measured as the precinct's
## deviation from the statewide turnout change, which also cancels a state-level coverage
## factor. Neither statistic uses k_c, so neither mixes the official and L2 scales.
##
## WHAT IT CANNOT MEASURE. The MEAN of L_i needs official county totals, which exist only
## for CA and NC (gov22_total) and for no state in 2018. So the selection-bias sweep below
## assumes est_tot is unbiased at the state level -- which is what k_c targets, but is not
## verified here. The sweep's SHAPE is robust; its position on the f axis is not.
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(nanoparquet); library(stringr); library(cli)
})

args <- commandArgs(trailingOnly = TRUE)
opt <- function(k, d) {
  m <- grep(paste0("^--", k, "="), args, value = TRUE)
  if (length(m)) sub(paste0("^--", k, "="), "", m[1]) else d
}
DIR_IN <- opt("dir", file.path(
  "/Users/young-kyungkim/Library/CloudStorage/Dropbox-MIT",
  "Young-Kyung Kim/Contracting/2024 Election Results",
  "26_general/input_data/model_inputs"))
PAIRS  <- strsplit(opt("pairs", "2018:2022,2020:2024,2022:2024"), ",")[[1]]
STATES <- strsplit(opt("states", "TX,NC,MI,FL,GA,OH"), ",")[[1]]
K_SHRINK <- as.numeric(opt("k", "40"))   # cells_from_inputs()'s county shrinkage, in votes
MODES  <- c("mail", "early", "eday")

#' Mode shares per precinct, shrunk toward the county (mirrors cells_from_inputs())
#'
#' @param d One election's rows: geo, county, method, voters.
#' @param k Shrinkage toward the county split, in votes.
#' @return Tibble geo, method, share (sums to 1 per geo).
shrunk_shares <- function(d, k = K_SHRINK) {
  cty <- d |> summarize(v = sum(voters), .by = c(county, method)) |>
    mutate(cs = v / sum(v), .by = county) |> select(county, method, cs)
  sw <- d |> summarize(v = sum(voters), .by = method) |> mutate(sw = v / sum(v)) |> select(method, sw)
  d |>
    mutate(n_p = sum(voters), .by = geo) |>
    left_join(cty, by = c("county", "method")) |>
    left_join(sw, by = "method") |>
    mutate(cs = coalesce(cs, sw, 1 / length(MODES)),
           share = (voters + k * cs) / (n_p + k)) |>
    mutate(share = share / sum(share), .by = geo) |>
    select(geo, method, share)
}

#' One (source -> target) pair for one state
#'
#' @param x That state's mode history.
#' @param src,tgt Election labels, e.g. "2018_general".
#' @return Tibble of per-cell errors, or NULL when a year is absent.
one_pair <- function(x, src, tgt) {
  if (!all(c(src, tgt) %in% x$election)) return(NULL)
  s <- filter(x, election == src, method %in% MODES)
  t <- filter(x, election == tgt, method %in% MODES)
  if (!nrow(s) || !nrow(t)) return(NULL)
  p <- shrunk_shares(s) |> rename(p = share)
  q <- t |> mutate(T_tgt = sum(voters), .by = geo) |>
    filter(T_tgt > 0) |> transmute(geo, method, q = voters / T_tgt, c = voters, T_tgt)
  Ts <- s |> summarize(T_src = sum(voters), .by = geo) |> filter(T_src > 0)
  inner_join(q, p, by = c("geo", "method")) |>
    inner_join(Ts, by = "geo") |>
    filter(q > 0, p > 0) |>
    mutate(e = log(q / p), L_raw = log(T_tgt / T_src))
}

#' Selection and partial-admission bias at a threshold
#'
#' A finished cell is admitted iff lambda >= f; a cell counted to fraction g iff
#' g*lambda >= f, contributing log(g). With no cadence file the partial term is reported
#' separately by `g`, so the net can be read off for whatever cadence applies.
#'
#' @param ll Vector of log lambda (centred: est_tot assumed unbiased statewide).
#' @param w Expected-vote weights.
#' @param f Threshold.
#' @return Named numeric: admitted share and selection bias in log points.
sel_bias <- function(ll, w, f) {
  keep <- ll >= log(f)
  if (!any(keep)) return(c(admit = 0, bias = NA_real_))
  c(admit = sum(w[keep]) / sum(w),
    bias  = weighted.mean(ll[keep], w[keep]) - weighted.mean(ll, w))
}

#' Flag (state, election) rows whose mode split is a labelling defect
#'
#' The "early mislabelled into mail" defect is NOT NC-specific: GA 2016/2018 report early
#' at 0.1% against 58% in 2020-2024, and OH 2018 reports mail at 4% against 19-34%
#' elsewhere. Reading those as composition error measures the defect, not the model.
#' Rule: a method whose share is under `ratio` of its median in that state's OTHER
#' elections, where that median clears `floor`, is a defect rather than a real shift. The
#' `floor` is what keeps genuinely absent modes (MI early, 0 in every year) from tripping it.
#'
#' @param x One state's mode history.
#' @param ratio,floor Thresholds described above.
#' @return Character vector of suspect election labels.
suspect_elections <- function(x, ratio = 0.25, floor = 0.05) {
  sh <- x |>
    filter(method %in% MODES) |>
    summarize(v = sum(voters, na.rm = TRUE), .by = c(election, method)) |>
    mutate(share = v / sum(v), .by = election)
  bad <- sh |>
    group_by(method) |>
    mutate(med_other = vapply(seq_len(n()), \(i) median(share[-i]), 0)) |>
    ungroup() |>
    filter(med_other > floor, share < ratio * med_other)
  unique(bad$election)
}

res <- list()
dropped <- list()
for (st in STATES) {
  f <- file.path(DIR_IN, paste0(st, "_precinct_mode_history.parquet"))
  if (!file.exists(f)) { cli_alert_warning("{st}: no mode history"); next }
  x <- as_tibble(read_parquet(f)) |>
    mutate(geo = as.character(geo), county = str_extract(geo, "(?<=_)[0-9]{3}(?=_)"))
  if (anyNA(x$county)) x$county <- coalesce(x$county, "000")
  bad <- suspect_elections(x)
  if (length(bad)) {
    dropped[[st]] <- bad
    cli_alert_warning("{st}: dropping {bad} -- mode split looks mislabelled, not shifted")
    x <- filter(x, !election %in% bad)
  }
  for (pr in PAIRS) {
    yy <- strsplit(pr, ":")[[1]]
    d <- one_pair(x, paste0(yy[1], "_general"), paste0(yy[2], "_general"))
    if (is.null(d) || !nrow(d)) next
    res[[paste(st, pr)]] <- mutate(d, state = st, pair = pr)
  }
}
if (!length(res)) stop("no (state, pair) combinations produced rows")
A <- bind_rows(res)
# MI's `early` is 0 in every L2 year, so it is a two-mode state here by construction
# (build_cells() already routes it to the mode-free path). Keep it, but say so.
cli_h2("Input screening")
if (length(dropped)) {
  for (st in names(dropped)) cli_alert_info("{st}: excluded {dropped[[st]]}")
} else cli_alert_info("no elections excluded")
cli_alert_info("usable (state, pair): {paste(sort(unique(paste(A$state, A$pair))), collapse = '; ')}")

# ---- 1. THE HEADLINE: per-mode composition error --------------------------------
cli_h1("Composition error e_im = log(q_target / p_source), vote-weighted by target votes")
cli_alert_info("Near-zero in every mode => one COMPLETE_FRAC can serve all modes.")
# MEDIAN, not mean. A per-cell log ratio blows up where the source mode is sparse: the
# k=40 shrinkage floors p at ~k*cs/(n_p+k), so a mode at a few percent statewide (OH early)
# produces enormous log(q/p) in precincts that reported none of it in the source year. The
# mean reads that tail; the median reads the typical cell, which is what a per-cell
# threshold actually meets. Both are shown.
hd <- A |>
  summarize(med = median(e), mean = weighted.mean(e, c),
            agg = log(sum(c) / sum(c / exp(e))), .by = c(pair, state, method)) |>
  pivot_wider(names_from = method, values_from = c(med, mean, agg), names_sort = TRUE) |>
  arrange(pair, state)
print(as.data.frame(select(hd, pair, state, starts_with("med_"))), digits = 2, row.names = FALSE)
cli_h2("same, vote-weighted MEAN (tail-sensitive; large gaps vs the median mean a sparse mode)")
print(as.data.frame(select(hd, pair, state, starts_with("mean_"))), digits = 2, row.names = FALSE)

cli_h2("Spread across modes within a pair (max - min of the means above)")
sp <- hd |>
  rowwise() |>
  mutate(spread = {v <- c_across(starts_with("med_")); diff(range(v[!is.na(v)]))}) |>
  ungroup() |>
  summarize(mean_spread = mean(spread), max_spread = max(spread),
            worst = state[which.max(spread)], .by = pair)
print(as.data.frame(sp), digits = 3, row.names = FALSE)

# ---- 2. variance decomposition --------------------------------------------------
cli_h1("var(log lambda) = var(L_i) + var(e_im): is the error common-mode?")
cli_alert_info("Common-mode means var(e) is small beside var(L).")
vd <- A |>
  summarize(
    n_cells = n(),
    var_L = {Li <- L_raw - weighted.mean(L_raw, T_tgt); sum(T_tgt * Li^2) / sum(T_tgt)},
    var_e = sum(c * (e - weighted.mean(e, c))^2) / sum(c),
    .by = c(pair, state)) |>
  mutate(pct_mode = 100 * var_e / (var_L + var_e)) |>
  arrange(pair, state)
print(as.data.frame(vd), digits = 3, row.names = FALSE)

# ---- 3. the threshold sweep -----------------------------------------------------
cli_h1("Selection bias by COMPLETE_FRAC")
cli_alert_info(
  "log lambda = L_i + e_im, centred (est_tot assumed statewide-unbiased -- NOT verified \\
   here, see the header). `admit` is the expected-vote share of cells a FINISHED cell \\
   would pass; `bias` is the resulting level error in log points.")
FS <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2)
sw <- A |>
  group_by(pair, state) |>
  group_modify(function(g, k) {
    ll <- (g$L_raw - weighted.mean(g$L_raw, g$T_tgt)) + g$e
    w <- g$c
    as_tibble(do.call(rbind, lapply(FS, function(f) {
      b <- sel_bias(ll, w, f); tibble(f = f, admit = b[["admit"]], bias = b[["bias"]])
    })))
  }) |> ungroup()
pooled <- sw |> summarize(admit = mean(admit), bias = mean(bias, na.rm = TRUE), .by = c(pair, f))
cli_h2("Pooled over states, per pair")
print(as.data.frame(pivot_wider(pooled, names_from = f,
      values_from = c(admit, bias), names_sort = TRUE)), digits = 2, row.names = FALSE)

cli_h2("Per state, midterm->midterm pair only")
mm <- filter(sw, pair == PAIRS[1]) |>
  select(state, f, bias) |> pivot_wider(names_from = f, values_from = bias, names_sort = TRUE)
print(as.data.frame(mm), digits = 2, row.names = FALSE)

# ---- 4. partial-admission, as a function of counted fraction --------------------
cli_h1("Partial-admission bias: which counted fractions a threshold lets in")
cli_alert_info(
  "No real cadence panel exists (TX 2024 is 718 raw zips), so this is reported as a \\
   function of g rather than of the clock: the minimum counted fraction a cell needs to \\
   pass f, at the median and 10th percentile of lambda.")
pa <- A |>
  group_by(pair) |>
  group_modify(function(g, k) {
    ll <- (g$L_raw - weighted.mean(g$L_raw, g$T_tgt)) + g$e
    lam <- exp(quantile(ll, c(0.5, 0.1), names = FALSE))
    as_tibble(do.call(rbind, lapply(FS, function(f) tibble(
      f = f, g_min_median_lambda = f / lam[1], g_min_p10_lambda = f / lam[2]))))
  }) |> ungroup()
print(as.data.frame(pa), digits = 2, row.names = FALSE)

cli_h1("Reminder")
cli_alert_warning(
  "This sizes the error structure only. In production `complete = stage >= 6` is never \\
   TRUE (CONTRACT.md 8), so COMPLETE_FRAC is reached only when the feed carries no \\
   `complete` column at all. Fixing the threshold does not by itself revive obs_turn.")
