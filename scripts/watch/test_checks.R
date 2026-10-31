# ========================================
## Watcher L1 on real data: sync the live pipeline-state and dashboard-live branches into a
## throwaway WATCH_HOME, run every check against them and the Dropbox, print the findings
## and the time per check. Then runs the checks a second time with the first run's metrics
## as `prev`, which exercises the cached paths (unchanged clean files, cached cells).
##   Rscript scripts/watch/test_checks.R [watch_home]
## Read-only on everything except the temp WATCH_HOME.
# ========================================

args = commandArgs(trailingOnly = TRUE)
home = if (length(args)) args[1] else tempfile("cbs-watch-")
Sys.setenv(CBS_WATCH_HOME = home)
source("scripts/watch/lib.R")
source("scripts/watch/sync.R")
source("scripts/watch/checks.R")
watch_init()
cat("WATCH_HOME:", WATCH_HOME, "\n")

t0 = Sys.time()
s = sync_pipeline_state()
cat(sprintf("sync: ok=%s run_time=%s commit_time=%s changed=%s dashboard_ok=%s (%.1fs)%s\n",
            s$ok, s$run_time, format(s$commit_time), s$changed, s$dashboard_ok,
            as.numeric(difftime(Sys.time(), t0, units = "secs")),
            if (is.null(s$error)) "" else paste0(" error: ", s$error)))
if (!s$ok) stop("sync failed")

show = function(res) {
  f = res$findings
  tab = data.frame(
    check = vapply(f, \(x) x$check, ""),
    severity = vapply(f, \(x) x$severity, ""),
    transient = vapply(f, \(x) isTRUE(x$transient), TRUE),
    key = vapply(f, \(x) x$key, ""),
    message = vapply(f, \(x) substr(x$message, 1, 90), "")
  )
  tab = tab[order(tab$severity, tab$check, tab$key), ]
  options(width = 250)
  print(tab, right = FALSE, row.names = FALSE)
  cat("\nfindings by check x severity:\n")
  print(table(tab$check, tab$severity))
  cat("\nseconds per check:\n")
  print(unlist(res$metrics$seconds))
  cat("\nmethods_match seconds by state:\n")
  print(unlist(res$metrics$methods_match$seconds_by_state))
  cat("\nsummary:\n")
  str(res$metrics$summary[c("n_targets_errored", "n_ok", "n_findings", "by_severity")])
  invisible(tab)
}

cat("\n==== run 1 (no prev) ====\n")
r1 = run_checks()
show(r1)
write_json_file(r1$metrics, state_path("metrics_prev.json"))

cat("\n==== run 2 (prev = run 1's metrics, read back from JSON) ====\n")
r2 = run_checks()
tab2 = show(r2)
cat("\nscraper files re-read in run 2:", r2$metrics$scraper_output$n_read, "of", r2$metrics$scraper_output$n_files, "\n")
cat("metrics_prev.json size:", file.size(state_path("metrics_prev.json")), "bytes\n")
