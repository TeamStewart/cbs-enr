# ========================================
## Watcher L1 fault injection for the regression checks: the HIST_N-cycle history, the
## enightdata jump, votes down, warning grouping and the crosswalk match (L2 and VoteHub).
## Fake contexts and a fake 26_general in a temp dir; no network, no Dropbox writes.
##   Rscript scripts/watch/test_regress.R
# ========================================
Sys.setenv(CBS_WATCH_HOME = tempfile("watch_regress_"))
source("scripts/watch/lib.R")
source("scripts/watch/checks.R")
watch_init()
n_fail = 0
ok = function(cond, what) {
  if (!isTRUE(cond)) { n_fail <<- n_fail + 1; cat("FAIL:", what, "\n") } else cat("ok:", what, "\n")
}
keys = function(r) vapply(r$findings, `[[`, "", "key")
mk_ctx = function(prev = list(), hist = list(), ...) {
  ctx = new.env()
  ctx$prev = prev; ctx$hist = hist; ctx$now = as.POSIXct("2026-11-03 23:00", tz = WATCH_TZ)
  for (nm in names(list(...))) assign(nm, list(...)[[nm]], envir = ctx)
  ctx
}

# ---- read_hist: newest first, current cycle skipped, prev as fallback ----
ok(identical(read_hist(list(a = 1)), list(list(a = 1))), "no cycle files: hist is prev")
for (i in 1:5) write_json_file(list(metrics = list(i = i)), state_path("cycles", sprintf("2026_11_03_20_0%d_00.json", i)))
h = read_hist(list(), "2026_11_03_20_05_00")
ok(identical(vapply(h, \(m) m$i, 0L), 4:2), "hist = last 3 before the current cycle, newest first")

# ---- B4: enightdata jump against 3 cycles back, and it stays open ----
dir.create(watch_path("pipeline-state"), showWarnings = FALSE)
writeLines("bbbbbbbbbbbb", watch_path("pipeline-state", "enightdata_sha"))
prog = function(n) function() data.frame(name = paste0("t", 1:40), progress = c(rep("errored", n), rep("completed", 40 - n)))
old = list(enightdata = list(sha = "aaaaaaaaaaaa", n_errored = 2))
new = function(n) list(enightdata = list(sha = "bbbbbbbbbbbb", n_errored = n))
# sha changed 2 cycles ago; errors crept 2 -> 4 -> 6 -> 12: the last-cycle diff (6 -> 12, same sha) would miss it
r = check_enightdata(mk_ctx(prev = new(6), hist = list(new(6), new(4), old), progress = prog(12)))
ok(any(grepl("^enightdata_jump:", keys(r))) && r$metrics$jump$before == 2, "jump vs the pre-change cycle 3 back")
# next cycle: the change has left the 3-cycle window, but the carried baseline keeps the P0 open
r2 = check_enightdata(mk_ctx(prev = list(enightdata = r$metrics), hist = list(new(12), new(12), new(6)), progress = prog(12)))
ok(length(r2$findings) == 1, "jump stays open while the same sha is installed")
r3 = check_enightdata(mk_ctx(prev = list(enightdata = r2$metrics), hist = list(new(12), new(12), new(12)), progress = prog(3)))
ok(!length(r3$findings) && is.null(r3$metrics$jump), "jump clears once errors fall back")
r4 = check_enightdata(mk_ctx(prev = new(2), hist = list(new(2), new(2), new(2)), progress = prog(12)))
ok(!length(r4$findings), "no sha change: no jump")

# ---- B3: warning shapes group duplicates ----
s = warn_shape(c("Failed to open 'https://x/ENR/a/22/P_22.csv': error: 404", "Failed to open 'https://x/ENR/b/7/P_7.csv': error: 404",
                 "county bound: 40 (county, mode) pairs", "county bound: 12 (county, mode) pairs", "No party for 'wi'"))
ok(s[1] == s[2] && s[3] == s[4] && length(unique(s)) == 3, "warn_shape masks quotes and numbers")

# ---- B6: votes down against the most of the last 3 cycles ----
d26 = file.path(tempdir(), "regress_26"); dir.create(file.path(d26, "TX", "clean"), recursive = TRUE, showWarnings = FALSE)
pq = file.path(d26, "TX", "clean", "TX_ellis_latest.parquet")
nanoparquet::write_parquet(data.frame(contest = "sen", candidate = c("a", "b"), votes = c(480, 480), geo = c("tx_070_1001-501", "tx_070_1002-502")), pq)
fm = function(v, mt = 1) list(scraper_output = list(files = list(TX_ellis = list(state = "TX", mtime = mt, size = 1, rows = 2, votes = v, cands = "x"))))
# 1000 -> 990 -> 970 -> 960: the last-cycle drop is 1%, the drop from 3 cycles back is 4%
r = check_scraper_output(mk_ctx(prev = fm(970), hist = list(fm(970), fm(990), fm(1000)), d26 = d26))
f = Filter(\(x) grepl("votes_down", x$key), r$findings)
ok(length(f) == 1 && f[[1]]$evidence$before == 1000, "votes_down vs the most of the last 3 cycles")

# ---- B7b: crosswalk match, L2 and VoteHub ----
dir.create(file.path(d26, "input_data", "crosswalks", "votehub"), recursive = TRUE, showWarnings = FALSE)
ids = sprintf("%d-5%02d", 1001:1040, 1:40)
data.table::fwrite(data.frame(state = "TX", county = "ELLIS", cd = "TX-06", precinct_yougov = 1001:1040, precinct_enr = ids,
                              precinct_l2 = c(1001:1038, NA, NA), l2_rule = "code_exact", precinct_votehub = NA),
                   file.path(d26, "input_data", "crosswalks", "tx.csv"))
data.table::fwrite(data.frame(state = "TX", county = "ELLIS", precinct_enr = ids, unit_id = paste0("48139-:-", 1001:1040)),
                   file.path(d26, "input_data", "crosswalks", "votehub", "tx.csv"))
feed = function(g) {
  nanoparquet::write_parquet(data.frame(contest = "sen", candidate = "a", votes = 0, geo = g), pq)
  Sys.setFileTime(pq, Sys.time() + stats::runif(1, 1, 1e4))  # a new mtime, so feed_geos re-reads
}
feed(c(paste0("tx_070_", ids), "tx_057_1004-01"))
bl = file.path(tempdir(), "regress_baseline.csv")
# no baseline yet: every uncovered county is reported
r = check_crosswalk_match(mk_ctx(d26 = d26, xw_baseline = bl))
m = r$metrics$states$TX
ok(m$l2$present == 1 && m$votehub$present == 1 && abs(m$l2$rate - 38 / 40) < 1e-9 && m$votehub$rate == 1, "rates: 100% present, L2 38/40, VoteHub 40/40")
ok(setequal(keys(r), c("crosswalk_match:TX_l2_uncovered", "crosswalk_match:TX_votehub_uncovered")), "no baseline: Dallas reported for both maps")
# the ENR cycle writes the baseline (match_rates.R --write); Dallas is then known cycle work
tab = xw_match_table(d26)
tab[, built := format(Sys.time(), "%Y-%m-%d %H:%M")]
data.table::fwrite(tab, bl)
ok(nrow(tab) == 2 && all(tab$uncovered == "DALLAS"), "xw_match_table: one row per map, Dallas uncovered")
r1 = check_crosswalk_match(mk_ctx(d26 = d26, xw_baseline = bl))
ok(!length(r1$findings), "with the baseline: known uncovered county is quiet")
# the county drops its polling-place suffix overnight: ids no longer match either map
feed(c(paste0("tx_070_", 1001:1040), "tx_057_1004-01"))
r2 = check_crosswalk_match(mk_ctx(d26 = d26, xw_baseline = bl, hist = list(list(crosswalk_match = r1$metrics))))
ok(setequal(keys(r2), c("crosswalk_match:TX_l2", "crosswalk_match:TX_votehub")), "renamed feed ids trip both L2 and VoteHub")
f = Filter(\(x) x$key == "crosswalk_match:TX_l2", r2$findings)[[1]]
ok(f$severity == "P1" && grepl("ENR-cycle baseline", f$message) && grepl("best of the last", f$message) && f$evidence$present == 0,
   "P1 after close: below the baseline and the 3-cycle best, present = 0")
# a new uncovered county, and a crosswalk rebuilt after the baseline
feed(c(paste0("tx_070_", ids), "tx_057_1004-01", "tx_220_1005"))
Sys.setFileTime(file.path(d26, "input_data", "crosswalks", "tx.csv"), Sys.time() + 3600)
r3 = check_crosswalk_match(mk_ctx(d26 = d26, xw_baseline = bl))
f = Filter(\(x) x$key == "crosswalk_match:TX_l2_uncovered", r3$findings)
ok(length(f) == 1 && identical(unlist(f[[1]]$evidence$counties), "TARRANT"), "only the county missing from the baseline is reported")
ok("crosswalk_match:baseline_stale" %in% keys(r3), "crosswalk newer than its baseline: baseline_stale")

cat(if (n_fail) sprintf("\n%d FAILED\n", n_fail) else "\nall passed\n")
quit(status = as.integer(n_fail > 0))
