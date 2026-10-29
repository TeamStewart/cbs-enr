# ========================================
## Interval coverage from tests/sim_night.R runs: pools live_sim_<scenario>.csv files and
## reports, by model x interval x share of votes counted, the mean absolute error of the
## two-party share, how often the interval held the truth, and its mean width.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/coverage_summary.R <dir or csv> [<dir or csv> ...] [--out=<prefix>]
## A directory means every live_sim_*.csv in it. Prints markdown tables; --out= also writes
## <prefix>_by_share.csv and <prefix>_by_scenario.csv.
# ========================================
source("scripts/model/live/load.R")
args = commandArgs(trailingOnly = TRUE)
OUT = sub("^--out=", "", c(grep("^--out=", args, value = TRUE), "--out=")[1])
paths = grep("^--", args, value = TRUE, invert = TRUE)
files = unlist(lapply(paths, \(p) {
  if (dir.exists(p)) list.files(p, "^live_sim_.*\\.csv$", full.names = TRUE) else p
}))
if (!length(files)) {
  stop("no live_sim_*.csv found in ", paste(paths, collapse = ", "))
}

res = bind_rows(lapply(files, \(f) {
  readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(time = "c"))
}))
res = res |>
  mutate(
    base = model_base(model),
    interval = vapply(
      model,
      \(m) model_parts(m)$interval %||% MODEL_INTERVALS[[model_base(m)]][1],
      ""
    ),
    share = cut(
      pct_votes,
      c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 0.99, Inf),
      c("<10%", "10-25%", "25-50%", "50-75%", "75-90%", "90-99%", "99%+"),
      right = FALSE
    )
  )
bad = filter(res, !grepl("^ok", status))
if (nrow(bad)) {
  cat("Fits not ok (left out):\n")
  print(count(bad, scenario, model, status = substr(status, 1, 80)), n = Inf)
}
ok = res |> filter(grepl("^ok", status), counted_votes > 0)

#' Coverage summary of a set of fits
#'
#' @param x Fits with est_twop, lo_twop, hi_twop, truth_twop, covers (grouped as wanted).
#' @return Tibble: fits, mae_pts (mean absolute two-party error, points), coverage,
#'   width_pts (mean interval width, points).
summ = function(x) {
  summarize(
    x,
    fits = n(),
    mae_pts = round(100 * mean(abs(est_twop - truth_twop)), 2),
    coverage = round(mean(covers), 3),
    width_pts = round(100 * mean(hi_twop - lo_twop), 2),
    .groups = "drop"
  )
}

#' Print a table as markdown
#'
#' @param x Data frame.
#' @return Called for its side effect.
md = function(x) {
  cat(knitr::kable(x, format = "pipe"), sep = "\n")
  cat("\n")
}

cat(sprintf(
  "\n%d fits ok from %d scenario(s): %s\n\n",
  nrow(ok),
  n_distinct(ok$scenario),
  paste(sort(unique(ok$scenario)), collapse = ", ")
))
cat("## All scenarios, by model x interval\n\n")
md(ok |> group_by(base, interval) |> summ())
by_share = ok |> group_by(base, interval, share) |> summ() |> arrange(base, interval, share)
cat("## By share of votes counted\n\n")
md(by_share)
by_sc = ok |> group_by(scenario, base, interval) |> summ() |> arrange(scenario, base, interval)
cat("## By scenario\n\n")
md(by_sc)
if (nzchar(OUT)) {
  readr::write_csv(by_share, paste0(OUT, "_by_share.csv"))
  readr::write_csv(by_sc, paste0(OUT, "_by_scenario.csv"))
}
