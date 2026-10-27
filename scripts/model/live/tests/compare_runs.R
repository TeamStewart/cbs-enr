# ========================================
## compare_runs.R -- check that a refactor changes no estimates.
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/tests/compare_runs.R save <out.rds> [models] [scenario]
##       predict_all() on the scenario's gov race at 21:15 and 23:00 (default models:
##       RUN, scenario tx_base), saved with its status and precinct rows
##   Rscript scripts/model/live/tests/compare_runs.R compare <before.rds> <after.rds> [old=new,...]
##       joins the two runs by model x candidate x pred_type x geo and prints the largest
##       difference in pred, pred_min, pred_max, p_win and precinct turn_est per model.
##       old=new renames models in <before> (e.g. model_cory=ratio_strata) when labels changed.
##
## Typical use: save on the commit before the refactor (git stash or a worktree), save after,
## compare. Every difference should be 0 unless the change is meant to move estimates.
# ========================================
source("scripts/model/live/load.R")
a = commandArgs(trailingOnly = TRUE)

if (a[1] == "save") {
  ms = if (length(a) >= 3) strsplit(a[3], ",")[[1]] else RUN
  sc = if (length(a) >= 4) a[4] else "tx_base"
  sim = read_sim(file.path(dirname(find_26_general()), "26_sim", sc))
  cells = feed_cells(build_cells(sim$truth$state[1]), sim$truth)
  out = list()
  for (tm in sim_checkpoints()[c(6, 13)]) {
    ret = returns_from_sim(sim, "gov", tm)
    out[[format(tm)]] = suppressMessages(predict_all(cells, ret, models = ms, cores = 5))
  }
  saveRDS(out, a[2])
  cat("saved", a[2], "\n")
} else if (a[1] == "compare") {
  o = readRDS(a[2]); n = readRDS(a[3])
  map = if (length(a) >= 4) {
    kv = strsplit(strsplit(a[4], ",")[[1]], "=")
    setNames(vapply(kv, `[`, "", 2), vapply(kv, `[`, "", 1))
  } else character()
  ren = function(m) ifelse(m %in% names(map), map[m], m)
  mx = function(x) if (all(is.na(x))) NA_real_ else max(abs(x), na.rm = TRUE)
  for (k in names(o)) {
    cat("==", k, "\n")
    po = mutate(o[[k]]$pred, model = ren(model)); pn = n[[k]]$pred
    j = full_join(po, pn, by = c("model", "candidate", "pred_type", "geo"), suffix = c(".o", ".n"))
    cat("prediction rows before / after:", nrow(po), nrow(pn), "; unmatched:", sum(is.na(j$pred.o) | is.na(j$pred.n)), "\n")
    print(j |> summarize(d_pred = mx(pred.o - pred.n), d_min = mx(pred_min.o - pred_min.n),
                         d_max = mx(pred_max.o - pred_max.n), d_p_win = mx(p_win.o - p_win.n),
                         conf = paste(unique(model_conf.o), "->", unique(model_conf.n)), .by = model))
    pr_o = mutate(o[[k]]$prec, model = ren(model)); pr_n = n[[k]]$prec
    jp = inner_join(pr_o, pr_n, by = c("model", "geo"), suffix = c(".o", ".n"))
    cat("precinct rows before / after / joined:", nrow(pr_o), nrow(pr_n), nrow(jp),
        "; max |turn_est diff|:", mx(jp$turn_est.o - jp$turn_est.n), "\n")
  }
} else stop("first argument: save or compare")
