# ========================================
## Check that the moved models reproduce the originals in scripts/model/tests/test_sim_night.R.
##
## Runs the ORIGINAL harness (--model=swing / --model=cory, statewide step off), then the
## moved code on the same simulated night, in the same order and with the same
## random-number stream, and compares every race x checkpoint.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/check_against_original.R [scenario] [--models=swing,cory]
## scenario: a name in 26_sim (default tx_base) or a path to a sim folder.
## Stops with an error if anything differs by more than 1e-9.
# ========================================
source("scripts/model/live/load.R")
args = commandArgs(trailingOnly = TRUE)
SCENARIO = c(grep("^--", args, value = TRUE, invert = TRUE), "tx_base")[1]
CHECK = strsplit(sub("^--models=", "", c(grep("^--models=", args, value = TRUE), "--models=swing,cory")[1]), ",")[[1]]
TOL = 1e-9

DIR_26 = find_26_general()
dir_sc = if (dir.exists(SCENARIO)) normalizePath(SCENARIO) else file.path(dirname(DIR_26), "26_sim", SCENARIO)
sim = read_sim(dir_sc)
cells = build_cells("TX", DIR_26)
races = sort(unique(sim$truth$contest))
tmp = tempfile(fileext = ".csv")

run_original = function(model) {
  cat("Running the original harness (--model=", model, ") ...\n", sep = "")
  # quoted: the Dropbox path has spaces. stderr shows, so a failure says why.
  st = system2("Rscript", shQuote(c("scripts/model/tests/test_sim_night.R", paste0("--sim_dir=", dir_sc),
                                    paste0("--model=", model), "--no_statewide", paste0("--out=", tmp))),
               stdout = FALSE, stderr = "")
  if (st != 0) stop("The original harness failed (exit ", st, "). Run it by hand to see why.")
  read_csv(tmp, col_types = cols(time = col_character(), .default = col_guess()))
}

compare = function(old, new, cols, label) {
  j = full_join(old |> select(race, time, all_of(cols)), new |> select(race, time, all_of(cols)),
                by = c("race", "time"), suffix = c("_old", "_new"))
  cat(sprintf("\n%s: %d rows in the original, %d in the moved code, %d matched\n",
              label, nrow(old), nrow(new), sum(complete.cases(select(j, ends_with("_old"), ends_with("_new"))))))
  diffs = vapply(cols, function(cl) {
    a = j[[paste0(cl, "_old")]]; b = j[[paste0(cl, "_new")]]
    if (any(is.na(a) != is.na(b))) return(Inf)
    ok = !is.na(a); if (!any(ok)) 0 else max(abs(a[ok] - b[ok]))
  }, 0)
  print(tibble(column = cols, max_abs_diff = diffs))
  if (nrow(old) != nrow(new) || any(diffs > TOL)) stop(label, ": the moved code does NOT match the original")
  cat(label, ": matches the original\n")
}

# ---- swing ------------------------------------------------------------------------
if ("swing" %in% CHECK) {
  old = run_original("swing")
  # the original sets the seed once and then fits races in order, checkpoints in order
  set.seed(2026)
  new = map_dfr(races, function(r) map_dfr(sim_checkpoints(), function(tm) {
    ret = returns_from_sim(sim, r, tm, geos = cells$geo, round_by = "party")
    f = fit_swing(cells, cell_counts(cells, ret), seed = NULL)
    if (f$status == "no_votes") return(NULL)
    mutate(f$race, race = r, time = format(tm, "%Y-%m-%d %H:%M"))
  }))
  compare(old, new, c("est_twop", "lo_twop", "hi_twop", "p_dem_win", "est_turnout", "lo_turnout",
                      "hi_turnout", "raw_twop", "obs_cells", "complete_cells"), "swing")
}

# ---- cory -------------------------------------------------------------------------
if ("cory" %in% CHECK) {
  if (!requireNamespace("enightmodels", quietly = TRUE)) {
    cat("\ncory: skipped -- enightmodels is not installed in this R\n")
  } else {
    old = run_original("cory")
    new = map_dfr(races, function(r) map_dfr(sim_checkpoints(), function(tm) {
      ret = returns_from_sim(sim, r, tm, geos = cells$geo, round_by = "candidate")
      mutate(fit_cory(cells, ret)$race, race = r, time = format(tm, "%Y-%m-%d %H:%M"))
    }))
    compare(old, new, c("est_twop", "est_turnout", "raw_twop", "obs_cells"), "cory")
  }
}
unlink(tmp)
