# ========================================
## TX 2022 governor backtest -- the real thing
##
## Fits the model to a REAL election using only pre-2022 information, reveals precincts in
## a plausible reporting order, and checks the estimate and interval against the known
## result at every stage.
##
## Single-mode (M=1): Texas has no precinct x mode x party history, so this cannot test
## delta_m or the mode machinery. It CAN test the county random effect, the g-prior, the
## prior->data transition and interval calibration -- on real data rather than synthetic.
##
## Target: the PANEL's own aggregate (the 32 L2-frame counties, 5.33M of 8.1M votes),
## not the official statewide number. Those counties are large and urban, hence 52.23%
## two-party Dem against 44.48% statewide.
##
## Run:  Rscript scripts/model/tx_backtest.R
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
  library(enightmodels)
})
source(Sys.getenv("MODEL_CORE", unset = "scripts/model/model_core.R"))
set.seed(as.integer(Sys.getenv("SEED", unset = "2022")))

DRAWS      <- 500
LEVEL      <- 0.95
PRIOR_N_T  <- 3.0
PRIOR_N_S  <- 3.0
# ICC measured from the 2020 Senate race on L2 demographics -- fully PRE-2022, so this is
# an honest out-of-sample test of a borrowed ICC, which is the 2026 situation exactly.
ICC_T      <- as.numeric(Sys.getenv("ICC_T", unset = "0.302"))
ICC_S      <- as.numeric(Sys.getenv("ICC_S", unset = "0.302"))
USE_RE     <- Sys.getenv("USE_RE", unset = "1") == "1"
ORDER      <- Sys.getenv("ORDER", unset = "county_size")   # county_size | random

d <- read_parquet("scripts/model/tx_backtest_panel.parquet") |> as_tibble()
COVS <- c("log_n","turnout18","turnout20","p_male","p_age_65","p_black","p_hisp","p_col")
d <- d |> mutate(across(all_of(setdiff(COVS,"log_n")), ~ as.numeric(scale(.x)))) |>
  filter(if_all(all_of(COVS), is.finite))
N <- nrow(d)

# ---- truth ----
TRUTH <- list(
  turn = sum(d$y_votes),
  dem  = sum(d$y_dem) / sum(d$y_votes),
  twop = sum(d$y_dem) / (sum(d$y_dem) + sum(d$y_rep))
)
cli_h1("TX 2022 governor backtest -- {N} precincts, {n_distinct(d$county)} counties")
cli_inform(c("truth (panel): Dem {round(100*TRUTH$dem,2)}% of all votes, \\
              two-party {round(100*TRUTH$twop,2)}%, turnout {format(TRUTH$turn, big.mark=',')}",
             "ICC used: {ICC_T} (turnout) / {ICC_S} (share), measured from the 2020 Senate race",
             "county random effect: {ifelse(USE_RE,'ON','OFF')}   reporting order: {ORDER}"))

# ---- reporting order -------------------------------------------------------------
# Small rural counties finish counting before large urban ones, and urban counties are
# more Democratic -- so the early sample is systematically Republican-leaning. That is a
# real selection problem even without mode.
cty <- d |> summarize(votes = sum(y_votes), .by = county) |> arrange(votes)
d <- d |> left_join(mutate(cty, cty_rank = row_number()), by = "county")
d$pos <- if (ORDER == "random") runif(N) else
  (d$cty_rank + runif(N, 0, 0.9)) / (nrow(cty) + 1)

# ---- design ----------------------------------------------------------------------
X   <- cbind(1, as.matrix(d[, COVS]))
CTY <- if (USE_RE) { f <- factor(d$county); m <- model.matrix(~ f - 1); colnames(m) <- levels(f); m
        } else matrix(0, N, 0)
lam <- c(tau = (1-ICC_T)/ICC_T, s = (1-ICC_S)/ICC_S)

# priors (all pre-2022)
y_t_prior <- log(d$est_turn + 0.1)
y_s_prior <- logit(d$dem2)
y_o_prior <- logit(d$oth_base)

run_stage <- function(frac) {
  obs <- d$pos <= frac
  if (sum(obs) < 40) return(NULL)
  tot2 <- d$y_dem + d$y_rep
  y_t <- log(d$y_votes + 0.1)
  y_s <- logit((d$y_dem + 0.5) / (tot2 + 1))
  y_o <- logit((d$y_oth + 0.5) / (d$y_votes + 1))
  wt  <- { w <- pmax(d$y_votes, 1); w[!obs] <- 1; w / mean(w[obs]) }

  fits <- list(
    tau = fit_conj_re(y_t, X, CTY, obs, y_t_prior, PRIOR_N_T/N, lam[["tau"]]),
    tp  = fit_conj_re(y_o, X, CTY, obs, y_o_prior, PRIOR_N_S/N, lam[["s"]],   wts = wt),
    s   = fit_conj_re(y_s, X, CTY, obs, y_s_prior, PRIOR_N_S/N, lam[["s"]],   wts = wt)
  )
  if (any(vapply(fits, is.null, TRUE))) return(NULL)
  A <- cbind(X, CTY)
  mu <- lapply(fits, \(f) draw_beta_re(f, DRAWS) %*% t(A))
  sg <- vapply(fits, sig_of, 0)
  eps <- lapply(sg, \(s) matrix(rnorm(DRAWS * N, 0, s), DRAWS, N))

  tau <- exp(mu$tau + eps$tau) - 0.1; tau[tau < 0] <- 0
  tp  <- invlogit(mu$tp + eps$tp)
  s   <- invlogit(mu$s  + eps$s)
  if (any(obs)) {                                    # substitute counted precincts
    nb <- sum(obs)
    tau[, obs] <- matrix(d$y_votes[obs],                       DRAWS, nb, byrow = TRUE)
    tp[,  obs] <- matrix(d$y_oth[obs]/pmax(d$y_votes[obs],1),  DRAWS, nb, byrow = TRUE)
    s[,   obs] <- matrix(d$y_dem[obs]/pmax(tot2[obs],1),       DRAWS, nb, byrow = TRUE)
  }
  two <- tau * (1 - tp)
  S   <- rowSums(two * s) / pmax(rowSums(tau), 1e-9)
  ci  <- cred_int(S, level = LEVEL)
  tibble(frac = frac, n_obs = sum(obs), pct_votes = sum(d$y_votes[obs]) / TRUTH$turn,
         est = mean(S), sd = sd(S), lo = ci[["lower"]], hi = ci[["upper"]],
         err = mean(S) - TRUTH$dem,
         covers = TRUTH$dem >= ci[["lower"]] && TRUTH$dem <= ci[["upper"]],
         turn = mean(rowSums(tau)))
}

res <- map_dfr(c(0.05, 0.15, 0.30, 0.50, 0.70, 0.85, 0.95, 1.00), run_stage)
cli_h2("Dem share of all votes -- truth {round(100*TRUTH$dem, 2)}%")
res |>
  mutate(est = 100*est, lo = 100*lo, hi = 100*hi, err = 100*err,
         pct_votes = 100*pct_votes, width = hi - lo,
         across(c(est, lo, hi, err, sd, pct_votes, width, turn), ~ signif(.x, 4))) |>
  as.data.frame() |> print(row.names = FALSE)
cli_inform("coverage across stages: {sum(res$covers)}/{nrow(res)}")
