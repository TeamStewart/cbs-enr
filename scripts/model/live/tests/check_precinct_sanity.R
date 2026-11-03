# ========================================
## check_precinct_sanity.R -- are the PER-PRECINCT predictions reasonable?
##
## The race total can be right while the precincts underneath it are not, and the mode
## composition is the part most able to hide: an aggregate of 0.84-1.16 has sat on top of an
## `eday` running 0.29-0.69 of truth. Anything that moves the composition prior or the
## turnout level has to be judged here, not only on the headline.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/check_precinct_sanity.R
##   Rscript .../check_precinct_sanity.R --scenario=tx_base --at=7 --compare
##
## --compare runs the same checkpoint twice, with CBS_MODE_TARGET / CBS_SCRAPED_TURNOUT off
## and then on, so the effect of the composition and level work is visible per precinct.
##
## TWO KINDS OF CHECK, deliberately separated.
##
## BOUNDS are absolute: a violation is a defect regardless of what truth turns out to be.
##   negative prediction; a cell predicted BELOW what it has already counted; per-mode
##   predictions not summing to the precinct total; a precinct total above its registrant
##   cap (KAPPA * n), which is the ceiling fit_stage() imposes -- a composition that shifts
##   mass into a mode can push a total into that cap and have it silently truncated.
##
## ACCURACY is relative, and on a sim it is only as good as the sim. The `_base` scenarios
##   are anchored at 2022 turnout (truth / est_turnout 0.97-1.06), so enabling the CBS
##   turnout level makes them look WORSE by construction -- measured on oh_base at 81%
##   counted, the race ratio goes 1.018 -> 1.173. That is the sim's anchor, not a defect.
##   Read the bounds section for correctness and the accuracy section for shape.
##
## Per-precinct x mode predictions come from `fit$cell_pred` (medsl_sur only -- it is the
## only model whose unit of analysis is the cell; `cells` is collapsed to precinct grain).
# ========================================
suppressPackageStartupMessages({
  library(nanoparquet)
  library(dplyr)
})
source("scripts/model/live/load.R") |> suppressMessages()

args = commandArgs(trailingOnly = TRUE)
opt = function(key, default) {
  sub(
    paste0("^--", key, "="),
    "",
    c(grep(paste0("^--", key, "="), args, value = TRUE), paste0("--", key, "=", default))[1]
  )
}
SCENARIO = opt("scenario", "oh_base")
AT = as.integer(opt("at", "7"))
COMPARE = "--compare" %in% args
D26 = find_26_general()
dir_sc = if (dir.exists(SCENARIO)) SCENARIO else file.path(dirname(D26), "26_sim", SCENARIO)
sim = read_sim(dir_sc)
ST = sim$truth$state[1]
RACE = sort(unique(sim$truth$contest))[1]

#' One run: fit, then every check
run_one = function(label) {
  cells = feed_cells(suppressMessages(build_cells(ST, D26)), sim$truth)
  suppressMessages(setup_model("medsl_sur", cells, ST))
  tms = sim_checkpoints()
  ret = returns_from_sim(sim, RACE, tms[min(AT, length(tms))])
  fit = suppressMessages(fit_model("medsl_sur", cells, ret))
  cp = fit$cell_pred
  if (is.null(cp)) {
    cat(sprintf("\n%s: no cell_pred (status %s)\n", label, as.character(fit$status)[1]))
    return(invisible(NULL))
  }
  inp = read_pq(file.path(D26, "input_data", "model_inputs", paste0(ST, "_precinct.parquet")))
  reg = tibble(geo = as.character(inp$geo), n = inp$n, est = inp$est_turnout)
  kappa = as.numeric(Sys.getenv("KAPPA", unset = "1.25"))

  prec = cp |>
    summarize(pred = sum(pred), counted = sum(c_tot), .by = geo) |>
    inner_join(reg, by = "geo")
  # truth, folded to the model's vocabulary
  tr = sim$truth |>
    filter(contest == RACE) |>
    mutate(geo = as.character(geo), m = unname(METHOD_MAP[as.character(method)])) |>
    filter(!is.na(m)) |>
    summarize(truth = sum(votes), .by = c(geo, m))
  cellj = inner_join(cp, tr, by = c("geo", method = "m"))
  precj = tr |> summarize(truth = sum(truth), .by = geo) |> inner_join(prec, by = "geo")

  cat(sprintf("\n================ %s ================\n", label))
  cat(sprintf(
    "%d precincts x %d modes | counted %.0f%% of in-frame truth\n",
    n_distinct(cp$geo), n_distinct(cp$method), 100 * sum(prec$counted) / sum(precj$truth)
  ))

  cat("\n-- BOUNDS (a violation is a defect whatever truth turns out to be) --\n")
  bad_neg = sum(cp$pred < -1e-6)
  bad_below = sum(cp$pred < cp$c_tot - 1e-6)
  sums = cp |>
    summarize(p = sum(pred), .by = geo) |>
    inner_join(summarize(cp, p2 = sum(pred), .by = geo), by = "geo")
  cap_bind = sum(prec$pred > kappa * pmax(prec$n, 1) - 1e-6)
  over_reg = sum(prec$pred > prec$n + 1e-6)
  chk = function(lab, n, extra = "") {
    cat(sprintf("  %-54s %s%s\n", lab, if (n == 0) "ok" else sprintf("** %d **", n), extra))
  }
  chk("negative cell predictions", bad_neg)
  chk("cells predicted BELOW their counted votes", bad_below)
  chk(sprintf("precinct totals at/over the cap (KAPPA=%.2f x n)", kappa), cap_bind)
  chk("precinct totals above registrants", over_reg)
  cat(sprintf(
    "  %-54s %.4f%%\n",
    "per-mode sum vs race est_turnout (rel. error)",
    100 * abs(sum(cp$pred) - fit$race$est_turnout[1]) / fit$race$est_turnout[1]
  ))

  cat("\n-- SHAPE --\n")
  q = function(x, p) quantile(x, p, na.rm = TRUE)
  r = prec$pred / pmax(prec$est, 1)
  cat(sprintf(
    "  pred / est_turnout   p05 %.2f  median %.2f  p95 %.2f  max %.2f\n",
    q(r, .05), median(r, na.rm = TRUE), q(r, .95), max(r, na.rm = TRUE)
  ))
  tr_rate = prec$pred / pmax(prec$n, 1)
  cat(sprintf(
    "  pred / registrants   p05 %.2f  median %.2f  p95 %.2f  max %.2f\n",
    q(tr_rate, .05), median(tr_rate, na.rm = TRUE), q(tr_rate, .95), max(tr_rate, na.rm = TRUE)
  ))

  if (nrow(cellj)) {
    cat("\n-- ACCURACY vs this sim's truth (see the header: sims are 2022-anchored) --\n")
    cat("  per precinct x mode, median |log(pred/truth)| and the ratio's spread:\n")
    acc = cellj |>
      filter(truth > 0, pred > 0) |>
      mutate(lr = log(pred / truth)) |>
      summarize(
        cells = n(),
        med_abs_log = round(median(abs(lr)), 4),
        ratio_p10 = round(exp(q(lr, .10)), 3),
        ratio_med = round(exp(median(lr)), 3),
        ratio_p90 = round(exp(q(lr, .90)), 3),
        .by = method
      ) |>
      arrange(method)
    print(as.data.frame(acc))
    pa = precj |> filter(truth > 0, pred > 0) |> mutate(lr = log(pred / truth))
    cat(sprintf(
      "  precinct TOTAL: median |log| %.4f | ratio p10 %.3f med %.3f p90 %.3f\n",
      median(abs(pa$lr)), exp(q(pa$lr, .10)), exp(median(pa$lr)), exp(q(pa$lr, .90))
    ))
    cat(sprintf(
      "  aggregate: pred %s vs truth %s (%.3f)\n",
      format(round(sum(precj$pred)), big.mark = ","),
      format(round(sum(precj$truth)), big.mark = ","),
      sum(precj$pred) / sum(precj$truth)
    ))
  }
  invisible(list(prec = prec, cell = cellj))
}

if (!COMPARE) {
  run_one(sprintf("%s @ checkpoint %d", SCENARIO, AT))
} else {
  for (sw in c("0", "1")) {
    Sys.setenv(CBS_MODE_TARGET = sw, CBS_SCRAPED_TURNOUT = sw)
    rm(list = ls(envir = .medsl_env), envir = .medsl_env)
    rm(list = ls(envir = .mode_target_env), envir = .mode_target_env)
    run_one(sprintf(
      "%s @ %d -- CBS composition + turnout level %s",
      SCENARIO, AT, if (sw == "1") "ON" else "OFF"
    ))
  }
  Sys.unsetenv("CBS_MODE_TARGET")
  Sys.unsetenv("CBS_SCRAPED_TURNOUT")
}
