# ========================================
## check_mode_folds.R -- does lookups/mode_fold.csv still match what the feeds will report?
##
## The fold table is HAND-WRITTEN (lookups/README.md says why: which method lands on which
## is a per-county determination, not something to infer at run time). Nothing generates it,
## so nothing notices when the world moves away from it. This is that something.
##
## Run it before the night, and after any refresh of the clean files:
##   Rscript scripts/model/live/tests/check_mode_folds.R
##   Rscript scripts/model/live/tests/check_mode_folds.R --states=OH,TX --priors
##
## Exits 1 if any state's table and feed disagree, so it can gate a command chain:
##   Rscript .../check_mode_folds.R && Rscript scripts/model/prep/make_reporting_levels.R
##
## THREE questions, because table-vs-feed agreement is necessary and not sufficient:
##   1. FOLD    does every method the prior names land somewhere the county reports?
##              (check_mode_fold(): a county needing a fold the table lacks, a row pointing
##              at a method the county also does not report, or a row gone stale)
##   2. BASIS   is each state's clean file still all zeros? The vocabulary is meant to come
##              from the ZERO file. `{ST}*_latest.parquet` is overwritten by every scrape, so
##              after a poll close it carries partial returns -- and a county that has not
##              reported mail YET looks exactly like a county with no mail geo. AK is a known
##              exception: its only file is the Aug-2026 primary and has votes by design.
##   3. PRIORS  (--priors) the two defects the fold exists to prevent, measured on the real
##              prior: share on a mode the feed never reports, and a reported mode whose
##              prior is ~0. A county can agree with the table and still fail these -- OH
##              Cuyahoga reports all three methods at PRECINCT level, so the fold rightly
##              leaves it alone, and its 0.33% `early` prior was a LEVEL problem.
##
##              Modes reported at county/site level are EXCLUDED from the near-zero test.
##              feed_methods() reads the vocabulary from every geo including buckets, so a
##              mode a county reports only on a county-level pile looks "in the feed" while
##              its PRECINCT prior is correctly ~0 -- those votes reach the model through
##              bucket_counts()/virtual.R, never through the precinct cell. Without this,
##              AK 001 `early` (32 expected votes) and NC 089 `mail` (4) are flagged as
##              defects when they are the design working. lookups/reporting_levels.csv is
##              the authority; note its county_cbs is NOT zero-padded.
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
D26 = find_26_general()
DO_PRIORS = "--priors" %in% args
THRESH = as.numeric(opt("thresh", "0.02"))

# lookups/mode_scope.csv is the authoritative list of states modelled BY VOTE METHOD, and
# of each one's MODES. Both matter here. Checking a state we do not model by method is
# noise, and checking a method the state does not model is wrong: CA's MODES are mail|eday
# because its feed carries no `early` at all, so flagging every CA county for a missing
# `early` fold would be demanding a fold for a mode the model never asks about.
MODE_SCOPE_PATH = Sys.getenv("MODE_SCOPE_PATH", unset = "scripts/model/lookups/mode_scope.csv")
MODE_SCOPE_YEAR = Sys.getenv("MODE_SCOPE_YEAR", unset = "2026")
scope = utils::read.csv(MODE_SCOPE_PATH, colClasses = "character")
scope = scope[scope$year == MODE_SCOPE_YEAR, , drop = FALSE]
state_modes = function(st) {
  m = scope$modes[scope$state == st]
  if (!length(m)) return(character())
  setdiff(trimws(strsplit(m[1], "|", fixed = TRUE)[[1]]), "all")
}
# a fold row for a state outside the scope is itself a finding, so union it in
found = sort(unique(c(scope$state, read_mode_fold()$state)))
STATES = strsplit(opt("states", paste(found, collapse = ",")), ",")[[1]]
STATES = sort(unique(toupper(STATES[nchar(STATES) == 2])))

fold = read_mode_fold()
cat(sprintf(
  "\nmode_fold.csv: %d row(s) across %s\n",
  nrow(fold),
  if (nrow(fold)) paste(sort(unique(fold$state)), collapse = ", ") else "no states"
))
if (nrow(fold)) {
  print(as.data.frame(arrange(fold, state, county_cbs)))
}
cat(sprintf("\nchecking %s\n", paste(STATES, collapse = ", ")))

rows = list()
problems = 0L
for (st in STATES) {
  fs = Sys.glob(file.path(D26, st, "clean", "*_latest.parquet"))
  if (!length(fs)) {
    rows[[st]] = tibble(
      state = st,
      counties = NA_integer_,
      fold_rows = nrow(filter(fold, state == st)),
      all_zero = NA,
      verdict = "no clean files -- skipped"
    )
    next
  }
  # BASIS: is the evidence still a zero file?
  votes = sum(vapply(fs, function(f) {
    x = tryCatch(read_parquet(f), error = function(e) NULL)
    if (is.null(x) || !"votes" %in% names(x)) 0 else sum(x$votes, na.rm = TRUE)
  }, numeric(1)))
  vocab = tryCatch(feed_methods(st, D26), error = function(e) NULL)
  n_cty = if (is.null(vocab)) 0L else n_distinct(vocab$county_cbs)
  # FOLD
  pm = state_modes(st)
  bad = if (!length(pm)) {
    # not modelled by vote method (MN), or not in scope at all: a fold row for it is stale
    f_st = filter(fold, state == st)
    if (nrow(f_st)) {
      transmute(f_st, county_cbs, from_method, issue = "state is not modelled by vote method")
    } else {
      tibble(county_cbs = character(), from_method = character(), issue = character())
    }
  } else {
    tryCatch(check_mode_fold(st, D26, prior_methods = pm, abort = FALSE), error = function(e) {
      tibble(county_cbs = NA_character_, from_method = NA_character_, issue = conditionMessage(e))
    })
  }
  if (nrow(bad)) {
    problems = problems + nrow(bad)
    cat(sprintf("\n!! %s: mode_fold.csv does not match the feed\n", st))
    for (i in seq_len(nrow(bad))) {
      cat(sprintf("     %s %-5s %s\n", bad$county_cbs[i], bad$from_method[i], bad$issue[i]))
    }
  }
  rows[[st]] = tibble(
    state = st,
    counties = n_cty,
    fold_rows = nrow(filter(fold, state == st)),
    all_zero = votes == 0,
    modes = if (length(pm)) paste(pm, collapse = "+") else "(not by method)",
    verdict = if (nrow(bad)) {
      sprintf("%d MISMATCH", nrow(bad))
    } else if (!length(pm) || n_cty == 0) {
      "not by method"
    } else {
      "ok"
    }
  )
}

cat("\n==== fold vs feed ====\n")
summ = bind_rows(rows)
print(as.data.frame(summ))
nz = filter(summ, !is.na(all_zero), !all_zero)
if (nrow(nz)) {
  cat(sprintf("\n! clean files already carry votes in: %s\n", paste(nz$state, collapse = ", ")))
  cat("  The vocabulary is meant to come from the ZERO file. Where a state has started\n")
  cat("  reporting, a mode that has not arrived YET is indistinguishable from a mode the\n")
  cat("  county never files -- so treat those rows as provisional. (AK is expected: its\n")
  cat("  only file is the Aug-2026 primary and carries votes by design.)\n")
}

if (DO_PRIORS) {
  cat("\n==== priors: the two defects the fold exists to prevent ====\n")
  for (st in STATES) {
    if (!length(state_modes(st))) next
    vocab = tryCatch(feed_methods(st, D26), error = function(e) NULL)
    if (is.null(vocab) || !nrow(vocab)) next
    ce = tryCatch(suppressMessages(build_cells(st, D26)), error = function(e) NULL)
    if (is.null(ce) || all(ce$method == "all")) next
    pri = ce |>
      mutate(cc = geo_county(geo)) |>
      summarize(e = sum(exp_votes), .by = c(cc, method)) |>
      mutate(p = e / sum(e), .by = cc)
    j = pri |>
      full_join(mutate(vocab, in_feed = TRUE), by = c(cc = "county_cbs", method = "method")) |>
      mutate(p = coalesce(p, 0), e = coalesce(e, 0), in_feed = coalesce(in_feed, FALSE)) |>
      filter(cc %in% unique(pri$cc), cc %in% unique(vocab$county_cbs))
    # a mode reported at county/site level has no precinct-level prior to get wrong
    lv = tryCatch(
      readr::read_csv(
        "scripts/model/lookups/reporting_levels.csv",
        col_types = readr::cols(.default = "c")
      ) |>
        filter(state == st, level == "county_site") |>
        transmute(cc = stringr::str_pad(county_cbs, 3, pad = "0"), method, bucket = TRUE),
      error = function(e) tibble(cc = character(), method = character(), bucket = logical())
    )
    j = j |>
      left_join(lv, by = c("cc", "method")) |>
      mutate(bucket = coalesce(bucket, FALSE))
    A = filter(j, !in_feed, p >= THRESH)
    B = filter(j, in_feed, !bucket, p < 0.01)
    cat(sprintf(
      "\n%s  prior-on-unreported-mode: %d   precinct-reported-mode-with-~0-prior: %d%s\n",
      st, nrow(A), nrow(B),
      if (sum(j$bucket & j$in_feed & j$p < 0.01)) {
        sprintf("   (%d county-site mode(s) excluded)", sum(j$bucket & j$in_feed & j$p < 0.01))
      } else {
        ""
      }
    ))
    if (nrow(A)) {
      cat(sprintf("   A %s %-5s %5.1f%% of prior, %s expected votes\n",
                  A$cc, A$method, 100 * A$p, format(round(A$e), big.mark = ",")))
    }
    if (nrow(B)) {
      cat(sprintf(
        "   B %s %-5s %.2f%% of prior (gate fires at COMPLETE_FRAC x prior/true)\n",
        B$cc, B$method, 100 * B$p
      ))
    }
  }
}

cat(sprintf("\n%s\n", if (problems) {
  sprintf("FAILED: %d mismatch(es)", problems)
} else {
  "PASSED: every fold row matches its feed"
}))
if (problems) quit(status = 1)
