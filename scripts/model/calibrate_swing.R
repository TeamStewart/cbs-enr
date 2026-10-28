# ========================================
## Statewide swing: how wrong is the model's PRIOR LEVEL, across states and cycles?
##
## This sizes the statewide level prior (component 1). The estimand is the sd of the
## logit-scale error in the prior's statewide two-party Dem share, where the prior comes
## from a previous contest -- which is exactly what the model does (the TX 2024 backtest
## primes dem2 from the 2020 Senate race and targets the 2024 Senate race).
##
## Needs ONLY the votehub results frames from backtest_panel.R: precinct results plus a
## county FIPS, at full votehub precinct coverage. No L2 join, so this is not limited by
## the precinct-id reconciliation that caps the covariate panels at 17% in NC and 20% in
## OH.
##
## COVERAGE CAVEAT: votehub's precinct universe is not every state's full electorate, and
## for a few states L2-era county coverage is partial. A swing computed over a partial,
## urban-skewed precinct set is not the statewide swing. Reported per state so the spread
## across states can be read with that in mind; no single pooled number is presented
## without it.
##
## Cross-office pairs are deliberately INCLUDED, because the model's prior routinely
## comes from a different office than its target. sen20 -> pres24 is the direct analogue
## of what the TX backtest does.
##
## Run:  Rscript scripts/model/calibrate_swing.R
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
})

PANELS <- "scripts/model/panels"
lg <- function(p) qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))

# Ordered (prior contest -> target contest) pairs. Each is a realization of "the prior
# level was off by this much on the logit scale".
PAIRS <- tibble::tribble(
  ~from,     ~to,
  "sen20",   "gov22",
  "gov22",   "pres24",
  "sen20",   "pres24",
  "sen20s",  "pres24",
  "sen20",   "cong24",
  "gov22",   "cong24"
)

fs <- list.files(PANELS, pattern = "_results\\.parquet$", full.names = TRUE)
if (!length(fs)) cli_abort("no *_results.parquet in {PANELS} -- run backtest_panel.R first")
cli_h1("Statewide swing calibration -- {length(fs)} state(s)")

res <- map(fs, function(f) {
  d <- read_parquet(f) |> as_tibble()
  st <- d$state[1]
  two <- function(c0) {
    dd <- paste0(c0, "_dem"); rr <- paste0(c0, "_rep")
    if (!all(c(dd, rr) %in% names(d))) return(NULL)
    D <- sum(d[[dd]], na.rm = TRUE); R <- sum(d[[rr]], na.rm = TRUE)
    if (D + R < 1000) return(NULL)
    list(share = D / (D + R), votes = D + R, prec = sum(d[[dd]] + d[[rr]] > 0, na.rm = TRUE))
  }
  map_dfr(seq_len(nrow(PAIRS)), function(i) {
    a <- two(PAIRS$from[i]); b <- two(PAIRS$to[i])
    if (is.null(a) || is.null(b)) return(NULL)
    tibble(state = st, from = PAIRS$from[i], to = PAIRS$to[i],
           share_from = a$share, share_to = b$share,
           swing_pp = 100 * (b$share - a$share),
           swing_logit = lg(b$share) - lg(a$share),
           votes_to = b$votes, prec_to = b$prec)
  })
}) |> bind_rows()

if (!nrow(res)) cli_abort("no usable contest pairs")

cli_h2("per-state, per-pair")
print(as.data.frame(res |> mutate(across(c(share_from, share_to), ~ round(100 * .x, 2)),
                                  swing_pp = round(swing_pp, 2),
                                  swing_logit = round(swing_logit, 4))),
      row.names = FALSE)

cli_h2("the number component 1 needs")
by_pair <- res |> summarize(n = n(), mean_logit = round(mean(swing_logit), 4),
                            sd_logit = round(sd(swing_logit), 4),
                            mean_abs_pp = round(mean(abs(swing_pp)), 2), .by = c(from, to))
print(as.data.frame(by_pair), row.names = FALSE)

sp <- filter(res, from == "sen20", to == "pres24")
cli_inform(c(
  "ALL pairs pooled: n = {nrow(res)}, \\
   sd(swing_logit) = {round(sd(res$swing_logit), 4)}, \\
   mean |swing| = {round(mean(abs(res$swing_pp)), 2)}pp",
  "sen20->pres24 only (the 4-year cross-office analogue the TX backtest runs): \\
   n = {nrow(sp)}, sd = {if (nrow(sp) > 1) round(sd(sp$swing_logit), 4) else NA}"
))
cli_inform(c(
  "For reference: the g-prior currently IMPLIES a statewide level prior sd of about",
  "sigma/sqrt(prior_n) = 0.17/sqrt(3) ~= 0.098 logit units, and the TX 2024 backtest's",
  "own single realization was 0.085. If the sd here lands near 0.09 the existing coupling",
  "is accidentally right and component 1 is a correctness-only change; if it lands near",
  "0.2 the current prior is ~2x too tight and early intervals should widen."
))

write_csv(res, "quality_reports/swing_calibration.csv")
cli_alert_success("wrote quality_reports/swing_calibration.csv")
