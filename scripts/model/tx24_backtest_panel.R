# ========================================
## TX 2024 US Senate backtest -- covariate panel, truth, and mode history
##
## Out-of-sample by construction: every predictor is knowable BEFORE Nov 2024.
##   dem2      votehub sen20 two-party Dem share          (2020)
##   tp_raw    votehub sen20_oth / sen20_total            (2020)
##   est_tot   L2 vote20                                  (2020 presidential turnout --
##             the right analogue for a presidential-year target; the 2022 backtest used
##             vote18 for the same reason)
##   covars    L2 demographics + turnout18 / turnout22    (2018 / 2022)
## DELIBERATELY EXCLUDED as look-ahead: vote24 and vote24_*, turnout24, pr_harris,
## pr_trump, prec_harris/prec_trump (the vf-frame codebook states these are CALIBRATED to
## 2024 precinct returns), and anything from the 2024 returns themselves.
##
## Outputs
##   scripts/model/tx24_backtest_panel.parquet   covariates, in model_turnout.R's
##                                               load_covs() schema (party-long)
##   scripts/model/tx24_truth.parquet            per-precinct x mode FINAL votes
##   scripts/model/tx24_mode_history_2020.parquet   BACKTEST mode history (2020)
##   scripts/model/tx24_mode_history_2022.parquet   BACKTEST mode history (2022)
##   scripts/model/tx24_mode_history_2024.parquet   PRODUCTION mode history, from the
##                                               2024 final canvass. NOT copied into the
##                                               Dropbox pipeline path -- see OUT_H24.
##
## On the three mode-history files:
##   2020 / 2022 (backtest) -- carry the TURNOUT SPLIT only, from L2
##     vote{20,22}_{mail,early,eday}. Their dem/rep columns split each mode's total at the
##     precinct's own 2020 lean, so the derived delta_m is ~0 by construction. That is
##     deliberate: L2's TX party is MODELLED, and its mode differential is directionally
##     wrong (eday reads more Dem than early in both 2022 and 2024 and the ordering flips
##     between cycles), so using it would be worse than zero. Arms set delta_m via
##     MODE_OFFSET_SOURCE instead. 2020 is the right analogue for a presidential target;
##     2022 is kept to price what a mis-chosen turnout-split prior costs.
##   2024 (production) -- real precinct x mode x party, the input CONTRACT.md §7 records
##     as blocking MODE_SHARE_OFFSET and the TX cross-mode Sigma blocks. Legitimate for
##     2026. Look-ahead for THIS backtest, so it is used only by the `oracle` arm.
##
##
## GEO CONVENTION — NOT the production one. This backtest keys on
##   "{fips5} {precinct zero-padded to 4}"  e.g. "48453 0100"
## because it joins votehub `unit_id` to the L2 frame directly. Production uses
##   "{st}_{county_cbs}_{precinct_id}" with LEADING ZEROS DROPPED, e.g. "tx_257_1"
## (CONTRACT.md §0, built by prep_tx_model_inputs.R). The two do NOT interoperate.
## Anything moving between this backtest and the live path must go through the
## crosswalk, not through string surgery -- see §0 on why normalizing ids to close
## crosswalk gaps produces wrong matches.
## Run:  Rscript scripts/model/tx24_backtest_panel.R
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
})

# PATH_DROPBOX env var if set, else this machine's Dropbox (live/R/paths.R)
PATH_DROPBOX <- local({ source("scripts/model/live/R/paths.R", local = TRUE); find_dropbox() })
P_L2  <- path.expand(file.path(PATH_DROPBOX, "26_general/input_data/l2_precinct_frame_method.parquet"))
P_VH  <- path.expand(file.path(PATH_DROPBOX, "26_general/input_data/votehub-shapefiles/estimated_past_results.csv"))
P_RET <- "scripts/model/tx24_returns_panel.parquet"
OUT_COVS  <- "scripts/model/tx24_backtest_panel.parquet"
OUT_TRUTH <- "scripts/model/tx24_truth.parquet"
OUT_H20   <- "scripts/model/tx24_mode_history_2020.parquet"
OUT_H22   <- "scripts/model/tx24_mode_history_2022.parquet"
# The PRODUCTION mode history is written into the repo, NOT straight to
#   {PATH_DROPBOX}/26_general/input_data/TX_mode_history.parquet
# which is where model_turnout.R looks by default. Dropping it there would silently switch
# the live 2026 model off delta_m = 0 and onto a 2024-derived offset. That is a decision
# to take deliberately, not a side effect of running a backtest, so activating it is a
# one-line copy the user makes:
#   cp scripts/model/tx24_mode_history_2024.parquet \
#      "{PATH_DROPBOX}/26_general/input_data/TX_mode_history.parquet"
OUT_H24   <- "scripts/model/tx24_mode_history_2024.parquet"

MODES <- c("mail", "early", "eday")
lg <- function(p) qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4))

cli_h1("TX 2024 Senate backtest panel")

# ---- 1. returns: night counties, final canvass, truth ----------------------------
ret <- read_parquet(P_RET) |> as_tibble() |>
  mutate(cty = str_sub(geo, 1, 5), ts = as.POSIXct(timestamp, tz = "UTC"))

# Counties with at least one snapshot in the election-night window. Three counties in the
# scrape (48167 Galveston, 48257 Kaufman, 48499 Wood) have ONLY 2026-02 scrapes and those
# carry zero votes, so they contribute nothing and would otherwise sit in the panel as
# permanently-unreported precincts with no observable truth.
N0 <- as.POSIXct("2024-11-05 18:00:00", tz = "UTC")
N1 <- as.POSIXct("2024-11-06 08:00:00", tz = "UTC")
night_cty <- ret |> filter(ts >= N0, ts <= N1) |> distinct(cty) |> pull(cty)
cli_inform("counties in scrape: {n_distinct(ret$cty)}   \\
            with election-night snapshots: {length(night_cty)}")

# `prov` is 1,565 votes = 0.04% of the panel. Dropped from returns AND truth so the two
# stay consistent, and so the model runs at the documented MODES = mail/early/eday.
ret <- ret |> filter(cty %in% night_cty, method %in% MODES)

# FINAL = each county's last snapshot (snapshots are cumulative within a county)
final <- ret |> filter(ts == max(ts), .by = cty)
cli_inform("night counties: {length(night_cty)}   \\
            precincts in final: {format(n_distinct(final$geo), big.mark=',')}   \\
            night snapshots: {ret |> filter(ts >= N0, ts <= N1) |> distinct(cty, ts) |> nrow()}")

# ---- 2. L2 covariates ------------------------------------------------------------
l2 <- read_parquet(P_L2) |> as_tibble() |> filter(state == "TX") |>
  mutate(fips5 = paste0("48", str_pad(FIPS, 3, pad = "0")),
         geo = paste(fips5, str_pad(str_remove_all(toupper(trimws(precinct)), "[^0-9A-Z]"),
                                    4, pad = "0")))
cli_inform("L2 TX precincts: {format(nrow(l2), big.mark=',')} in {n_distinct(l2$fips5)} counties")

# ---- 3. votehub 2020 Senate (the pre-2024 partisan baseline) ---------------------
vh <- read_csv(P_VH, col_types = cols(.default = "c"), progress = FALSE) |>
  filter(state_abbr == "TX") |>
  mutate(fips5 = str_extract(unit_id, "^[0-9]+"),
         prec  = str_remove(unit_id, "^[0-9]+-:-"),
         geo = paste(fips5, str_pad(str_remove_all(toupper(trimws(prec)), "[^0-9A-Z]"),
                                    4, pad = "0"))) |>
  mutate(across(c(sen20_dem, sen20_rep, sen20_oth, sen20_total), as.numeric)) |>
  select(geo, sen20_dem, sen20_rep, sen20_oth, sen20_total) |>
  distinct(geo, .keep_all = TRUE)

# ---- 4. the universe -------------------------------------------------------------
# Precincts present in the returns AND in L2 AND in votehub. A returns precinct that
# does not join cannot be modeled; an L2 precinct absent from the returns has no
# observable truth. Report the loss per county -- it is NOT random within a county.
ret_geo <- final |> summarize(votes = sum(dem + rep + oth), .by = c(cty, geo))
j <- ret_geo |>
  mutate(in_l2 = geo %in% l2$geo, in_vh = geo %in% vh$geo)
by_cty <- j |>
  summarize(prec = n(), joined = sum(in_l2 & in_vh),
            v_all = sum(votes), v_join = sum(votes[in_l2 & in_vh]), .by = cty) |>
  mutate(pct_prec = round(100 * joined / prec, 1),
         pct_votes = round(100 * v_join / v_all, 1)) |>
  arrange(pct_prec)
cli_h2("precinct join, returns -> L2 + votehub")
print(as.data.frame(by_cty), row.names = FALSE)
cli_inform("overall: {sum(by_cty$joined)}/{sum(by_cty$prec)} precincts \\
            = {round(100*sum(by_cty$joined)/sum(by_cty$prec), 1)}%   \\
            {round(100*sum(by_cty$v_join)/sum(by_cty$v_all), 1)}% of votes")

universe <- j |> filter(in_l2, in_vh) |> pull(geo)

# ---- 5. covariate panel ----------------------------------------------------------
d <- l2 |> filter(geo %in% universe) |> left_join(vh, by = "geo") |>
  mutate(
    county    = county_name,
    dem2      = (sen20_dem + 1e-6) / (sen20_dem + sen20_rep + 2e-6),
    tp_raw    = pmin(pmax(sen20_oth / pmax(sen20_total, 1), 1e-4), 0.15),
    est_tot   = pmax(vote20, 1),                 # 2020 presidential turnout
    turnout18 = vote18 / pmax(n, 1),
    turnout22 = vote22 / pmax(n, 1),
    p_male    = male / pmax(male + fem, 1),
    p_age65   = age4 / pmax(n, 1),
    p_black   = race2 / pmax(n, 1),
    p_hisp    = race3 / pmax(n, 1),
    p_col     = col_known / pmax(col_known + noncol_known, 1),
    # model_turnout.R's RE_GROUPING offers county / geostratum / cory(stratum_demo +
    # geostratum). The L2 frame has no geostratum, so build an honest stand-in: county
    # for geostratum, and a coarse demographic bucket for stratum_demo.
    geostratum   = county_name,
    stratum_demo = case_when(
      p_hisp  >= 0.50 ~ "hisp",
      p_black >= 0.30 ~ "black",
      p_col   >= 0.45 ~ "white_col",
      TRUE            ~ "white_noncol")
  ) |>
  filter(sen20_total > 20, n > 50, is.finite(dem2), dem2 > 0, dem2 < 1,
         if_all(c(turnout18, turnout22, p_male, p_age65, p_black, p_hisp, p_col),
                is.finite))
cli_inform("covariate panel: {format(nrow(d), big.mark=',')} precincts \\
            in {n_distinct(d$county)} counties")

# party-long, as load_covs() expects
covs <- d |>
  transmute(geo, county, geostratum, stratum_demo, n,
            turnout18, turnout22, p_male, p_age65, p_black, p_hisp, p_col,
            dem2, tp_raw,
            est_dem = est_tot * dem2, est_rep = est_tot * (1 - dem2)) |>
  pivot_longer(c(est_dem, est_rep), names_to = "party", values_to = "est",
               names_prefix = "est_") |>
  mutate(method = "all")

# ---- 6. truth --------------------------------------------------------------------
truth <- final |> filter(geo %in% d$geo) |> select(geo, method, dem, rep, oth)
TT <- truth |> summarize(dem = sum(dem), rep = sum(rep), oth = sum(oth))
tot <- TT$dem + TT$rep + TT$oth
cli_h2("truth on the modeled universe")
cli_inform(c(
  "dem {format(TT$dem, big.mark=',')} / rep {format(TT$rep, big.mark=',')} / \\
   oth {format(TT$oth, big.mark=',')}  total {format(tot, big.mark=',')}",
  "dem share of all {round(100*TT$dem/tot, 3)}%   \\
   two-party {round(100*TT$dem/(TT$dem+TT$rep), 3)}%   \\
   third-party {round(100*TT$oth/tot, 3)}%"
))
# how good the pre-2024 partisan baseline is, before the model does anything
pv <- truth |> summarize(v = sum(dem + rep), .by = geo)
dd <- d |> select(geo, dem2) |> left_join(pv, by = "geo") |> mutate(v = coalesce(v, 0))
act <- truth |> summarize(d = sum(dem), r = sum(rep), .by = geo) |>
  mutate(s = d / pmax(d + r, 1)) |> select(geo, s)
cmp <- dd |> inner_join(act, by = "geo") |> filter(v > 20)
cli_inform(c(
  "dem2 (2020 sen), vote-weighted: {round(100*weighted.mean(cmp$dem2, cmp$v), 2)}%   \\
   vs actual two-party {round(100*TT$dem/(TT$dem+TT$rep), 2)}%",
  "corr(logit dem2, logit actual) at precinct level: \\
   {round(cor(lg(cmp$dem2), lg(cmp$s)), 3)}"
))
mode_split <- truth |> summarize(v = sum(dem + rep + oth), .by = method) |>
  mutate(pct = round(100 * v / sum(v), 2)) |> arrange(desc(v))
print(as.data.frame(mode_split), row.names = FALSE)

# ---- 7. mode history: pre-2024 turnout split (backtest) --------------------------
# Turnout split from L2 vote{YY}_{mail,early,eday}. dem/rep split each mode's TOTAL at
# the precinct's own 2020 lean, so dem+rep reproduces the mode total exactly (what the
# turnout split needs) while carrying no mode partisan differential (so a `history`
# offset derivation returns ~0 rather than something misleading).
#
# TWO files, because the choice of analogue year is itself a modeling decision and the
# model's own config warns about getting it wrong in the other direction:
#   2020 -- the right analogue for a PRESIDENTIAL target. TX mode composition tracks
#           turnout type, not calendar: midterms run 27-32% election-day, presidentials
#           13-20%. 2024 actual is 19.1% eday; 2020 is 13.3%, 2022 is 30.7%.
#   2022 -- the WRONG analogue here (it is the right one for the 2026 midterm, which is
#           what model_turnout.R ships with). Kept so the backtest can measure what a
#           mis-chosen turnout-split prior costs.
mk_hist <- function(cols, election, contest) {
  x <- d |> select(geo, dem2, all_of(cols))
  names(x)[3:5] <- c("mail", "early", "eday")          # cols are given in MODES order
  x |>
    pivot_longer(c(mail, early, eday), names_to = "method", values_to = "v") |>
    mutate(v = pmax(coalesce(v, 0), 0),
           dem = round(v * dem2), rep = v - round(v * dem2)) |>
    select(geo, method, dem, rep) |>
    pivot_longer(c(dem, rep), names_to = "party", values_to = "votes") |>
    mutate(election = election, contest = contest) |>
    select(geo, election, contest, method, party, votes)
}
h20 <- mk_hist(c("vote20_mail", "vote20_early", "vote20_eday"), "2020_general", "president")
h22 <- mk_hist(c("vote22_mail", "vote22_early", "vote22_eday"), "2022_general", "governor")
cli_h2("pre-2024 mode turnout splits (backtest inputs)")
for (nm in c("h20", "h22")) {
  sw <- get(nm) |> summarize(v = sum(votes), .by = method) |>
    mutate(pct = round(100 * v / sum(v), 2))
  cli_inform("{nm}: {paste(sprintf('%s %.2f%%', sw$method, sw$pct), collapse='  ')}")
}
cli_inform("2024 ACTUAL on this universe: \\
            {paste(sprintf('%s %.2f%%', mode_split$method, mode_split$pct), collapse='  ')}")

# ---- 8. mode history: 2024 (production) ------------------------------------------
# Real TX precinct x mode x party. Unblocks MODE_SHARE_OFFSET and the TX cross-mode Sigma
# blocks for the 2026 model. LOOK-AHEAD for this backtest -- `oracle` arm only.
h24 <- truth |>
  select(geo, method, dem, rep) |>
  pivot_longer(c(dem, rep), names_to = "party", values_to = "votes") |>
  mutate(election = "2024_general", contest = "ussenate") |>
  select(geo, election, contest, method, party, votes)

# The measured TX delta_m, reported here so it can replace the NC-borrowed NC_DELTA in
# model_turnout.R. Same specification derive_mode_priors() uses:
#   logit(s_im) - logit(dem2_i) = a_m + b_m * logit(dem2_i), weighted by the cell's
#   two-party votes.
cli_h2("MEASURED TX 2024 delta_m (same spec as derive_mode_priors)")
fits <- lapply(MODES, function(m) {
  hj <- truth |> filter(method == m) |> inner_join(select(d, geo, dem2), by = "geo") |>
    mutate(n2 = dem + rep, tau = dem + rep + oth, s_m = dem / pmax(n2, 1)) |>
    filter(n2 >= 25, dem2 > 0, dem2 < 1, s_m > 0, s_m < 1)
  x <- lg(hj$dem2); y <- lg(hj$s_m) - x
  f <- stats::lm.wfit(cbind(1, x), y, hj$n2)
  list(method = m, n_prec = nrow(hj),
       a = unname(f$coefficients[1]), b = unname(f$coefficients[2]),
       mode_twop_dem = sum(hj$dem) / sum(hj$n2),
       # residual on the logit share scale -- this, NOT the raw share, is what Sigma
       # holds, so it is what the NC figures below are comparable to
       res = tibble(geo = hj$geo, r_s = unname(f$residuals), tau = hj$tau))
})
names(fits) <- MODES
delta_tx <- bind_rows(lapply(fits, \(f) tibble(method = f$method, n_prec = f$n_prec,
  a = f$a, b = f$b, mode_twop_dem = f$mode_twop_dem)))
print(as.data.frame(delta_tx |> mutate(across(c(a, b, mode_twop_dem), ~ round(.x, 4)))),
      row.names = FALSE)
cli_inform("for comparison, NC 2024: mail a +0.660 b -0.297 | \\
            early a -0.029 b +0.041 | eday a -0.025 b -0.089")

# Cross-mode SHARE-RESIDUAL correlations -- the other quantity CONTRACT.md §7 lists as
# NC-borrowed. Sigma is a residual covariance, so these must be measured on residuals
# after the lean/delta_m fit. Raw share correlations are dominated by the precinct's own
# lean (they come out 0.81-0.94 here) and are NOT comparable to the NC numbers.
wide_r <- bind_rows(lapply(fits, \(f) mutate(f$res, method = f$method))) |>
  select(geo, method, r_s) |> pivot_wider(names_from = method, values_from = r_s)
cli_h2("MEASURED TX 2024 cross-mode share-RESIDUAL correlations (n2 >= 25)")
for (p in list(c("mail","early"), c("mail","eday"), c("early","eday"))) {
  ok <- is.finite(wide_r[[p[1]]]) & is.finite(wide_r[[p[2]]])
  if (sum(ok) >= 30) {
    ct <- suppressWarnings(stats::cor.test(wide_r[[p[1]]][ok], wide_r[[p[2]]][ok]))
    cli_inform("  {p[1]} <-> {p[2]}: rho {round(ct$estimate, 3)}  \\
                (n = {sum(ok)}, t = {round(ct$statistic, 1)})")
  }
}
cli_inform("for comparison, NC 2024: mail<->early +0.232 | mail<->eday -0.050 | \\
            early<->eday +0.039")

# Within-mode rho(tau, s) on residuals -- the third NC-borrowed constant (RHO_TAU_S)
cli_h2("MEASURED TX 2024 within-mode rho(tau, s) on residuals")
for (m in MODES) {
  r <- fits[[m]]$res |> filter(tau > 0)
  # residualize log(tau) on the same lean term so both sides are residuals
  lt <- lm(log(r$tau) ~ lg(d$dem2[match(r$geo, d$geo)]))$residuals
  ct <- suppressWarnings(stats::cor.test(lt, r$r_s))
  cli_inform("  {m}: rho {round(ct$estimate, 3)}  (n = {nrow(r)}, t = {round(ct$statistic, 1)})")
}
cli_inform("for comparison, NC 2024: mail +0.204 | early +0.052 | eday -0.140")

write_parquet(covs,  OUT_COVS);  cli_alert_success("wrote {OUT_COVS}")
write_parquet(truth, OUT_TRUTH); cli_alert_success("wrote {OUT_TRUTH}")
write_parquet(h20,   OUT_H20);   cli_alert_success("wrote {OUT_H20}")
write_parquet(h22,   OUT_H22);   cli_alert_success("wrote {OUT_H22}")
write_parquet(h24,   OUT_H24);   cli_alert_success("wrote {OUT_H24}")
