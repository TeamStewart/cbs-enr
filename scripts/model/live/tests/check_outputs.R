# ========================================
## check_outputs.R -- CONTRACT.md section 15's acceptance tests, as something you can RUN.
##
## Until now they existed only as prose in the contract, which means they were never run:
## the one that mattered most (`check_pred_data()` passes) could not pass at all, because
## that function cannot return for any input (see check_pred() in live/R/output.R). A
## criterion nobody executes is a comment.
##
## ABORTS on a violation -- this is the gate, not a report. write_predictions() deliberately
## only WARNS on the night, so that one malformed race cannot stop the other eleven from
## being written; this is where a schema break is supposed to stop the world, before air.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/check_outputs.R                     # model_results
##   Rscript scripts/model/live/tests/check_outputs.R <dir>               # any output dir
##   Rscript scripts/model/live/tests/check_outputs.R --sim=tx_base       # a sim run
# ========================================
source("scripts/model/live/load.R")
suppressPackageStartupMessages(library(dplyr))

args = commandArgs(trailingOnly = TRUE)
sim  = sub("^--sim=", "", grep("^--sim=", args, value = TRUE))
pos  = grep("^--", args, value = TRUE, invert = TRUE)
d26  = find_26_general()
DIR  = if (length(sim)) file.path(d26, "model_tests", "run_night_sim", sim[1]) else
       if (length(pos)) pos[1] else file.path(d26, "model_results")
if (!dir.exists(DIR)) stop("no such directory: ", DIR)
cli::cli_h1("CONTRACT section 15 acceptance tests")
cli::cli_alert_info("{.path {DIR}}")

fails = character()
# `msg` is passed as a VALUE, not as a cli template: several of these messages contain
# literal braces (set notation like {vote, share}) and cli would try to evaluate them.
ck = function(ok, msg) {
  if (isTRUE(ok)) cli::cli_alert_success("{msg}")
  else { fails <<- c(fails, msg); cli::cli_alert_danger("{msg}") }
}
# a check that needs to report WHY, not just that it failed
ck_try = function(expr, msg) {
  r = tryCatch({ expr; TRUE }, error = function(e) conditionMessage(e))
  if (isTRUE(r)) cli::cli_alert_success("{msg}")
  else { fails <<- c(fails, msg); cli::cli_alert_danger("{msg} -- {r}") }
}

# ---- 15.1 predictions -------------------------------------------------------------
f_pred = file.path(DIR, "predictions_latest.csv")
if (!file.exists(f_pred)) stop("missing ", f_pred)
# `district` is read as logical when every row is NA, which is every statewide race; that
# is a CSV typing artifact and check_pred() handles it. Everything else is read as written.
pred = readr::read_csv(f_pred, show_col_types = FALSE, progress = FALSE)
cli::cli_h2("15.1 predictions ({nrow(pred)} rows, {ncol(pred)} cols)")
ck_try(check_pred(pred), "check_pred() passes (schema + COLS_PRED order)")
EXTRA_OK = c("p_win", "geo", "cbs_as_of", "race_id")
extra = setdiff(names(pred), c(enightmodels::COLS_PRED, EXTRA_OK))
ck(!length(extra), paste0("no columns beyond COLS_PRED + ", paste(EXTRA_OK, collapse = "/"),
                          if (length(extra)) paste0(" -- found: ", paste(extra, collapse = ", "))))
ck(all(pred$pred_type %in% c("vote", "share")), "pred_type is only vote/share")
if ("geo" %in% names(pred)) {
  # section 15: `geo` here is RESERVED for the literals, it is not a precinct key
  ck(all(pred$geo %in% c("scraped", "all", NA)), "geo is only scraped/all")
  a = filter(pred, !is.na(geo), geo == "all")
  if (nrow(a)) {
    ck(all(!is.na(a$cbs_as_of)), "every geo=='all' row has a non-NA cbs_as_of")
    ck(all(is.na(a$model_conf) | grepl("_plus_rest$", a$model_conf)),
       "every geo=='all' row's model_conf ends _plus_rest or is NA")
  }
}

# ---- 15.2 status: a race x model is never SILENTLY absent --------------------------
f_stat = file.path(DIR, "status_latest.csv")
if (!file.exists(f_stat)) stop("missing ", f_stat)
stat = readr::read_csv(f_stat, show_col_types = FALSE, progress = FALSE)
cli::cli_h2("15.2 status ({nrow(stat)} rows)")
# The 7 documented columns, IN ORDER, then anything else. run_night.R also emits `cbs`
# (the provenance of the county snapshot, e.g. "not pulled; using the snapshot saved ..."),
# which section 15.2 did not list -- documented there now rather than asserted away here.
STAT_CORE = c("race_id", "model", "time", "status", "statewide", "seconds", "settings")
ck(identical(names(stat)[seq_along(STAT_CORE)], STAT_CORE),
   paste0("status's first 7 columns are the documented ones, in order -- got: ",
          paste(utils::head(names(stat), 7), collapse = ", ")))
ck(!anyNA(stat$status), "no NA status")
# every race x model that produced predictions must have a status row. The converse is
# allowed: a model that failed appears in status with no prediction rows, which is the
# whole point of 15.2.
# JOIN ON race_id, which predictions now carry as a permitted extra. Do NOT reconstruct it
# from state/contest: that decomposition is not invertible (see output.R::to_pred_data) and
# the wrong rule fails silently -- this anti-join would report every row missing.
ck("race_id" %in% names(pred), "predictions carry race_id, so the two tables join directly")
pm = pred |> distinct(race_id, model)
miss = anti_join(pm, distinct(stat, race_id, model), by = c("race_id", "model"))
ck(!nrow(miss), paste0("every predicted race x model has a status row",
   if (nrow(miss)) paste0(" -- missing: ", nrow(miss))))
bad = filter(stat, !grepl("^ok", status), race_id %in% pm$race_id & model %in% pm$model)
if (nrow(bad)) cli::cli_alert_info("{nrow(bad)} race x model(s) have a non-ok status: \\
                                    {paste(unique(bad$status), collapse = '; ')}")
# PRIOR-ONLY IS THE LOUD ONE. Those rows have predictions that look exactly like a real
# fit's and contain nothing from tonight, so they must not pass quietly. Not a FAILURE --
# a state whose feed has not joined is a data problem, not a schema one -- but it has to be
# impossible to miss, which is the whole point of the status carrying it.
po = filter(stat, grepl("^prior_only", status))
if (nrow(po))
  cli::cli_alert_danger(paste0("{nrow(po)} race x model(s) are PRIOR ONLY -- their ",
    "predictions carry no information from tonight: ",
    "{paste(sprintf('%s/%s', po$race_id, po$model), collapse = ', ')}"))
pp = filter(stat, grepl("^ok \\(partial prior", status))
if (nrow(pp)) cli::cli_alert_warning("{nrow(pp)} race x model(s) are a PARTIAL prior fit")
# THE CONVERSE OF THE 15.2 INVARIANT, and the one that was missing. 15.2 guarantees a
# race x model with no prediction still appears in `status`; this asserts the other
# direction -- a race x model whose status says it SUCCEEDED must have produced rows.
# Without it a model can report "ok" and emit nothing, which is exactly what happened when
# `status` gained detail and three call sites were still testing identical(status, "ok").
okm = stat |> filter(grepl("^ok", status)) |> distinct(race_id, model)
silent = anti_join(okm, pm, by = c("race_id", "model"))
ck(!nrow(silent), paste0("every race x model with an ok status produced prediction rows",
   if (nrow(silent)) paste0(" -- SILENT: ",
     paste(sprintf("%s/%s", silent$race_id, silent$model), collapse = ", "))))

# ---- 15.3 precincts ---------------------------------------------------------------
f_prec = file.path(DIR, "precincts_latest.parquet")
if (!file.exists(f_prec)) {
  cli::cli_alert_warning("no precincts_latest.parquet -- 15.3 skipped")
} else {
  pr = read_pq(f_prec)
  cli::cli_h2("15.3 precincts ({nrow(pr)} rows, {ncol(pr)} cols)")
  ck(identical(names(pr), PREC_COLS), "columns are exactly PREC_COLS, in order")
  ck(!anyNA(pr$geo) && !anyNA(pr$model) && !anyNA(pr$race_id) && !anyNA(pr$time) &&
       !anyNA(pr$turn_est),
     "no NA in geo / model / race_id / time / turn_est")
  d = pr |> count(geo, race_id, model) |> filter(n > 1)
  ck(!nrow(d), paste0("one row per geo x race_id x model",
     if (nrow(d)) paste0(" -- ", nrow(d), " duplicated")))
  ck(all(pr$est_dem2 >= 0 & pr$est_dem2 <= 1, na.rm = TRUE), "est_dem2 is within [0, 1]")
  # n_obs_modes needs a DENOMINATOR, and it is only meaningful against one. `n_modes` was
  # added and then reverted with the structural-absence mask (cfad2984), so guard on its
  # presence instead of silently comparing against a column of NAs -- which is what an
  # unguarded all(..., na.rm = TRUE) does: it returns TRUE and the check becomes decorative.
  if ("n_modes" %in% names(pr)) {
    ck(all(pr$n_obs_modes <= pr$n_modes, na.rm = TRUE), "n_obs_modes <= n_modes")
  } else {
    m = suppressWarnings(max(pr$n_obs_modes, na.rm = TRUE))
    cli::cli_alert_warning(paste0("no n_modes column, so n_obs_modes has no denominator; ",
      "max observed is ", m, ". A mode reporting at county or one-stop-site level has no ",
      "precinct cell, so the denominator is NOT constant per state (AK precincts have 1, ",
      "NC's have 1-3) and n_obs_modes/M would be wrong for a third of what we model."))
  }
  # Reported, never asserted: a precinct counting past its own prediction is a signal, not
  # a rounding artifact, and the count is the thing to watch across refreshes.
  over = sum(pr$cnt_prec > pr$turn_est + 0.5, na.rm = TRUE)
  cli::cli_alert_info("{over} of {nrow(pr)} row(s) have cnt_prec > turn_est")
  iv = pr |> filter(!is.na(turn_lo)) |> summarize(n = n(), bad = sum(turn_hi < turn_lo))
  ck(iv$bad == 0, paste0("turn_hi >= turn_lo on the ", iv$n, " rows with intervals"))
}

# ---- verdict ----------------------------------------------------------------------
if (length(fails)) {
  cli::cli_h1("FAILED")
  stop(length(fails), " acceptance check(s) failed:\n  - ",
       paste(fails, collapse = "\n  - "), call. = FALSE)
}
cli::cli_h1("all acceptance checks passed")
