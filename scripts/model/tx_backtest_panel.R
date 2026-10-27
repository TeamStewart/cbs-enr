# ========================================
## TX 2022 governor backtest -- assemble the panel
##
## Out-of-sample by construction: every predictor is knowable BEFORE Nov 2022.
##   outcome   votehub gov22_dem / gov22_rep / gov22_oth   (precinct level)
##   dem2      votehub sen20 two-party Dem share            (2020 -> pre-2022)
##   oth_base  votehub sen20_oth / sen20_total              (2020)
##   est_turn  L2 vote18 (the prior MIDTERM)                (2018)
##   covars    L2 demographics + vote18/vote20 turnout rates
## DELIBERATELY EXCLUDED as look-ahead: vote22, vote24, pr_harris, pr_trump, pres24, cong24.
##
## Join: L2 (48 + zero-padded FIPS, precinct) <-> votehub unit_id "{fips5}-:-{precinct}".
## Verified 98.9% across all 32 TX counties in the frame.
##
## Run:  Rscript scripts/model/tx_backtest_panel.R
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
})
DB <- local({ source("scripts/model/live/R/paths.R", local = TRUE); find_dropbox() })
p_l2 <- file.path(DB, "26_general/input_data/l2_precinct_frame_method.parquet")
p_vh <- file.path(DB, "26_general/input_data/votehub-shapefiles/estimated_past_results.csv")
out  <- "scripts/model/tx_backtest_panel.parquet"

l2 <- read_parquet(path.expand(p_l2)) |> as_tibble() |> filter(state == "TX") |>
  mutate(fips5 = paste0("48", str_pad(FIPS, 3, pad = "0")),
         key = paste(fips5, str_pad(str_remove_all(toupper(trimws(precinct)), "[^0-9A-Z]"), 4, pad = "0")))

vh <- read_csv(path.expand(p_vh), col_types = cols(.default = "c"), progress = FALSE) |>
  filter(state_abbr == "TX") |>
  mutate(fips5 = str_extract(unit_id, "^[0-9]+"),
         prec  = str_remove(unit_id, "^[0-9]+-:-"),
         key = paste(fips5, str_pad(str_remove_all(toupper(trimws(prec)), "[^0-9A-Z]"), 4, pad = "0"))) |>
  mutate(across(c(gov22_dem, gov22_rep, gov22_oth, gov22_total,
                  sen20_dem, sen20_rep, sen20_oth, sen20_total,
                  sen20s_dem, sen20s_rep, sen20s_oth, sen20s_total), as.numeric)) |>
  select(key, starts_with("gov22_"), starts_with("sen20_"), starts_with("sen20s_"))

cli_h1("TX 2022 governor backtest panel")
cli_inform("L2 TX precincts: {nrow(l2)}   votehub TX rows: {nrow(vh)}")

d <- inner_join(l2, distinct(vh, key, .keep_all = TRUE), by = "key")
cli_inform("joined: {nrow(d)} ({round(100*nrow(d)/nrow(l2),1)}% of L2)")

lg <- function(p) qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4))
d <- d |>
  mutate(
    geo = key, county = county_name,
    # --- OUTCOME (2022) ---
    y_votes = gov22_total, y_dem = gov22_dem, y_rep = gov22_rep, y_oth = gov22_oth,
    # --- PRE-2022 PRIORS ---
    dem2     = (sen20_dem + 1e-6) / (sen20_dem + sen20_rep + 2e-6),
    oth_base = pmin(pmax(sen20_oth / pmax(sen20_total, 1), 1e-4), 0.15),
    est_turn = pmax(vote18, 1),                      # prior midterm turnout
    # --- PRE-2022 COVARIATES ---
    log_n     = log(pmax(n, 1)),
    turnout18 = vote18 / pmax(n, 1),
    turnout20 = vote20 / pmax(n, 1),
    p_male    = male / pmax(male + fem, 1),
    p_age_65  = age4 / pmax(n, 1),
    p_black   = race2 / pmax(n, 1),
    p_hisp    = race3 / pmax(n, 1),
    p_col     = col_known / pmax(col_known + noncol_known, 1)
  ) |>
  filter(y_votes > 20, sen20_total > 20, n > 50, is.finite(dem2), dem2 > 0, dem2 < 1) |>
  select(geo, county, y_votes, y_dem, y_rep, y_oth, dem2, oth_base, est_turn,
         log_n, turnout18, turnout20, p_male, p_age_65, p_black, p_hisp, p_col,
         sen20_dem, sen20_rep, sen20_total)   # retained ONLY to measure the ICC pre-2022

cli_inform("usable: {nrow(d)} precincts in {n_distinct(d$county)} counties")
cli_h2("sanity checks")
tp <- sum(d$y_dem) / (sum(d$y_dem) + sum(d$y_rep))
cli_inform(c(
  "2022 gov two-party Dem (panel): {round(100*tp, 2)}%   [official statewide 44.48%]",
  "2022 gov third-party share    : {round(100*sum(d$y_oth)/sum(d$y_votes), 2)}%   [official ~1.42%]",
  "turnout: panel {format(sum(d$y_votes), big.mark=',')} of 8.1M statewide"
))
cli_inform("dem2 (2020 sen) n-weighted: {round(100*weighted.mean(d$dem2, d$y_votes), 2)}%")
cli_inform("correlation dem2 (2020) vs actual 2022 gov share: {round(cor(lg(d$dem2), lg(d$y_dem/pmax(d$y_dem+d$y_rep,1))), 3)}")
write_parquet(d, out)
cli_alert_success("wrote {out}")
