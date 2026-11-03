# ========================================
## ablate.R -- run a set of env-switch ARMS in one command and compare them.
##
## Why this exists. Every switch ablation here has been hand-driven, and hand-driving it
## produced three wrong answers in a row on 2026-11-03, each of which looked like a finding:
##
##   1. "94 precincts predict above their registrants" -- measured against `l2_n` when the
##      bound is `KAPPA * n`. On the right column it is 2, and nothing breaches the cap.
##   2. "all four switches are inert" -- the shell variable set was `MODEL_CBS_ENV`, but
##      globals.R reads `CBS_MODEL_CBS_ENV`, so the feed silently stayed on live.
##   3. "all four switches are inert, again" -- `env ... $VARS Rscript` in zsh, which does
##      NOT word-split an unquoted expansion, so all four assignments arrived as ONE
##      variable and every arm ran the same configuration.
##
## Each failure had the same tell -- arms that came out identical -- and each cost a round
## trip. So this script encodes the tells as assertions:
##
##   * arms are DATA, never a shell string: the env goes through system2(env=), which takes
##     name=value pairs directly and never sees a shell.
##   * every arm runs in its OWN PROCESS, because `.medsl_env` caches the model environment
##     per state and in-process arms after the first are silently inert.
##   * every arm REPORTS ITS OWN env back, read inside the child, and that is what the table
##     prints -- never what the caller believes it set.
##   * every arm records whether the switch actually BIT, by matching the lever's own log
##     lines, so "no effect" and "never fired" cannot be confused.
##   * IDENTICAL ARMS ARE A FAILURE, not a result. Two arms with the same predictions abort
##     the comparison and name the pair.
##   * metrics use `n`, the column `cap = KAPPA * pmax(n, 1)` is built from (model_turnout.R).
##
## Usage, from the cbs-enr folder:
##   Rscript scripts/model/live/tests/ablate.R
##   Rscript scripts/model/live/tests/ablate.R --scenario=tx_base --at="2026-11-03 22:00"
##   Rscript scripts/model/live/tests/ablate.R --models=medsl_sur --cores=4 --cbs=test
##   Rscript scripts/model/live/tests/ablate.R --arms=all_off,pair1      # a subset
##   Rscript scripts/model/live/tests/ablate.R --state=TX --out=/tmp/ablate
##
## The switches only exist on the branch that defines them, so to compare against a tree
## that lacks them, run the script on each tree and diff the CSVs -- it records `head` for
## exactly that. It never switches branches itself.
##
## Writes <out>/ablate_<scenario>_<stamp>.csv plus one log per arm, and prints the table.
# ========================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})
source("scripts/model/live/R/paths.R")

opt <- function(k, d = NA_character_) {
  a <- commandArgs(trailingOnly = TRUE)
  v <- grep(paste0("^--", k, "="), a, value = TRUE)
  if (length(v)) sub(paste0("^--", k, "="), "", v[1]) else d
}

SCENARIO <- opt("scenario", "tx_base")
AT       <- opt("at", "2026-11-03 22:00")
MODELS   <- opt("models", "medsl_sur")
STATE    <- toupper(opt("state", "TX"))
CORES    <- opt("cores", "4") # 4, not all-but-one: 9 workers on a 16 GB box swaps and runs 4x slower
CBS_ENV  <- opt("cbs", "test") # the live feed carries no county expected vote until CBS files it
OUT      <- opt("out", file.path(tempdir(), "ablate"))

# ---- the arms, as data ---------------------------------------------------------------
# Defaults in the branch code are MODE_TARGET=1, SCRAPED_TURNOUT=1, LEVEL_PRIOR=0,
# SHARE_PRIOR=0, so `all_off` is NOT the code's default state -- it is the main-equivalent
# baseline, and `default` is what a merge would actually ship.
ARMS <- tribble(
  ~arm,       ~CBS_MODE_TARGET, ~CBS_SCRAPED_TURNOUT, ~CBS_LEVEL_PRIOR, ~CBS_SHARE_PRIOR,
  "all_off",  "0",              "0",                  "0",              "0",
  "pair1",    "1",              "1",                  "0",              "0",
  "pair2",    "0",              "0",                  "1",              "1",
  "default",  "1",              "1",                  "0",              "0",
  "all_on",   "1",              "1",                  "1",              "1"
)
SWITCHES <- setdiff(names(ARMS), "arm")

want <- opt("arms", NA_character_)
if (!is.na(want)) {
  keep <- trimws(strsplit(want, ",")[[1]])
  bad <- setdiff(keep, ARMS$arm)
  if (length(bad)) {
    stop("no such arm: ", paste(bad, collapse = ", "), ". Have: ", paste(ARMS$arm, collapse = ", "))
  }
  ARMS <- filter(ARMS, arm %in% keep)
}

D26 <- find_26_general()
SIM_DIR <- file.path(D26, "model_tests", "run_night_sim", SCENARIO)
FRAME <- file.path(D26, "input_data", "model_inputs", paste0(STATE, "_precinct.parquet"))
stopifnot("no such frame" = file.exists(FRAME))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
HEAD <- tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE), error = function(e) NA_character_)

cli::cli_h1("ablate: {nrow(ARMS)} arm(s) on {SCENARIO} @ {AT}")
cli::cli_alert_info("tree {HEAD} | models {MODELS} | state {STATE} | cores {CORES} | cbs {CBS_ENV}")

# ---- run each arm, in its own process ------------------------------------------------
# The child reports its own env; `switch state` in the table is the CHILD's reading, so a
# mis-set variable shows up as a mismatched arm rather than a silent null result.
run_arm <- function(row) {
  env <- c(
    paste0("CBS_MODEL_CBS_ENV=", CBS_ENV),
    paste0(SWITCHES, "=", unlist(row[SWITCHES]))
  )
  log <- file.path(OUT, paste0("arm_", row$arm, ".log"))

  # what the CHILD sees, not what we think we set
  seen <- system2("Rscript", c("-e", shQuote(sprintf(
    'cat(paste(Sys.getenv(c(%s, "CBS_MODEL_CBS_ENV")), collapse="|"))',
    paste0('"', c(SWITCHES), '"', collapse = ", ")
  ))), env = env, stdout = TRUE, stderr = FALSE)
  seen <- tail(seen[nzchar(seen)], 1)

  system2(
    "Rscript",
    c(
      "scripts/model/live/run_night.R",
      paste0("--sim=", SCENARIO),
      paste0("--at=", shQuote(AT)),
      paste0("--models=", MODELS),
      paste0("--states=", STATE),
      paste0("--cores=", CORES),
      "--no-redistricting"
    ),
    env = env, stdout = log, stderr = log
  )

  txt <- paste(readLines(log, warn = FALSE), collapse = "\n")
  # --sim ignores --out, so snapshot before the next arm overwrites it
  dest <- file.path(OUT, row$arm)
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  for (f in c("precincts_latest.parquet", "turnout_latest.csv", "status_latest.csv")) {
    src <- file.path(SIM_DIR, f)
    if (file.exists(src)) file.copy(src, file.path(dest, f), overwrite = TRUE)
  }

  tibble(
    arm = row$arm,
    env_seen = seen,
    # did the lever fire? matched on the levers' OWN log lines
    bit_turnout = grepl("turnout constraint target", txt),
    bit_level = grepl("level prior from CBS expected vote", txt),
    bit_share = grepl("share prior from", txt),
    inert_warn = grepl("stays on 2022", txt),
    ran = grepl("races x", txt)
  )
}

fired <- lapply(seq_len(nrow(ARMS)), function(i) {
  cli::cli_alert("arm {ARMS$arm[i]}")
  run_arm(ARMS[i, ])
}) |> bind_rows()

if (any(!fired$ran)) {
  cli::cli_abort("arm(s) {paste(fired$arm[!fired$ran], collapse = ', ')} did not complete -- see {.path {OUT}}")
}

# ---- metrics ------------------------------------------------------------------------
frame <- nanoparquet::read_parquet(FRAME, col_select = c("geo", "n")) |>
  as_tibble() |>
  transmute(geo = as.character(geo), n = as.numeric(n))

metrics <- function(arm) {
  d <- file.path(OUT, arm)
  tu <- read_csv(file.path(d, "turnout_latest.csv"), show_col_types = FALSE, progress = FALSE)
  pr <- as_tibble(nanoparquet::read_parquet(file.path(d, "precincts_latest.parquet")))
  tot <- if ("method" %in% names(tu)) filter(tu, method == "total") else tu
  j <- inner_join(transmute(pr, geo = as.character(geo), turn_est), frame, by = "geo") |> filter(n > 0)
  races <- tot |>
    filter(geo == "all") |>
    summarize(pred = sum(pred), .by = race_id) |>
    arrange(race_id)
  tibble(
    arm = arm,
    pred_all = paste(sprintf("%s=%s", races$race_id, format(round(races$pred), big.mark = ",")), collapse = " "),
    scraped = round(sum(tot$pred[tot$geo == "scraped"])),
    above_n = sum(j$turn_est > j$n),          # `n`, the KAPPA base -- not l2_n
    over_kappa = sum(j$turn_est > 1.25 * j$n), # must be 0: the bound is by construction
    max_ratio = round(max(j$turn_est / j$n), 3),
    pred_md5 = substr(digest::digest(round(pr$turn_est, 3), algo = "md5"), 1, 8)
  )
}

res <- lapply(ARMS$arm, metrics) |> bind_rows() |> left_join(fired, by = "arm")
res$head <- HEAD
res$scenario <- SCENARIO
res$at <- AT

cli::cli_h2("arms")
print(as.data.frame(select(res, arm, env_seen, bit_turnout, bit_level, bit_share, inert_warn)), row.names = FALSE)
cli::cli_h2("estimates")
print(as.data.frame(select(res, arm, pred_all, scraped, above_n, over_kappa, max_ratio, pred_md5)), row.names = FALSE)

# ---- the assertions that would have caught all three mistakes ------------------------
cli::cli_h2("checks")
fails <- character()
ck <- function(ok, msg) {
  if (isTRUE(ok)) cli::cli_alert_success(msg) else {
    fails <<- c(fails, msg)
    cli::cli_alert_danger(msg)
  }
}

# the env the CHILD saw must be the env we asked for
asked <- apply(ARMS[SWITCHES], 1, paste, collapse = "|")
ck(all(res$env_seen == paste(asked, CBS_ENV, sep = "|")),
   "every arm's child reported the env it was given")

# a switch set to 1 with CBS data present must leave a trace
on_t <- ARMS$CBS_SCRAPED_TURNOUT == "1"
ck(all(res$bit_turnout[on_t]), "CBS_SCRAPED_TURNOUT=1 arms show the turnout constraint firing")
on_l <- ARMS$CBS_LEVEL_PRIOR == "1"
ck(all(res$bit_level[on_l]), "CBS_LEVEL_PRIOR=1 arms show the level prior firing")
on_s <- ARMS$CBS_SHARE_PRIOR == "1"
ck(all(res$bit_share[on_s]), "CBS_SHARE_PRIOR=1 arms show the share prior firing")
ck(!any(res$inert_warn), "no arm degraded to the 2022 level for want of CBS data")

# KAPPA is a construction bound, so a breach means the metric is wrong, not the model
ck(all(res$over_kappa == 0), "no arm breaches the KAPPA cap (a breach means the wrong base column)")

# IDENTICAL ARMS ARE A BROKEN HARNESS
dup <- res |> summarize(arms = paste(arm, collapse = " == "), k = n(), .by = pred_md5) |> filter(k > 1)
# `default` is pair1 by construction, so that one collision is expected
expected_dup <- c("pair1 == default", "default == pair1")
unexpected <- filter(dup, !arms %in% expected_dup)
ck(nrow(unexpected) == 0,
   if (nrow(unexpected)) paste0("DISTINCT ARMS PRODUCED IDENTICAL PREDICTIONS: ",
                                paste(unexpected$arms, collapse = "; "),
                                " -- probe the lever before reading any number as a finding")
   else "every distinct arm produced distinct predictions")

f <- file.path(OUT, sprintf("ablate_%s_%s.csv", SCENARIO, format(Sys.time(), "%Y%m%d_%H%M%S")))
write_csv(res, f)
cli::cli_alert_info("wrote {.path {f}}")

if (length(fails)) {
  cli::cli_abort("{length(fails)} check(s) failed -- the comparison above is NOT safe to read")
}
cli::cli_alert_success("all checks passed")
