# ========================================
## Watcher L2's read-only data tool (Read can't open parquet or the targets store):
##   Rscript scripts/watch/inspect.R parquet <file> [n]        columns, rows, counts, head
##   Rscript scripts/watch/inspect.R errors [pattern]          errored targets (pushed meta)
##   Rscript scripts/watch/inspect.R dropped <ST>              returns_dropped() by method/geo
## Never writes anything.
# ========================================
args = commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("usage: inspect.R parquet <file> [n] | errors [pattern] | dropped <ST>")
suppressPackageStartupMessages(library(dplyr))
options(width = 200)
home = path.expand(Sys.getenv("CBS_WATCH_HOME", "~/.cache/cbs-enr-watch"))

switch(args[1],
  parquet = {
    d = nanoparquet::read_parquet(args[2])
    n = if (length(args) >= 3) as.integer(args[3]) else 10
    cat("rows:", nrow(d), "\n")
    utils::str(d, give.attr = FALSE)
    v = if ("votes" %in% names(d)) d$votes else rep(NA_real_, nrow(d))
    for (col in intersect(c("contest", "method", "party", "stage", "jurisdiction"), names(d))) {
      cat("\n", col, ":\n", sep = "")
      print(as.data.frame(summarise(mutate(d, .v = v), rows = n(), votes = sum(.v, na.rm = TRUE), .by = all_of(col))), row.names = FALSE)
    }
    cat("\nhead:\n")
    print(as.data.frame(utils::head(d, n)), row.names = FALSE)
  },
  errors = {
    m = targets::tar_meta(store = file.path(home, "pipeline-state", "_targets"), fields = c(error, time))
    m = filter(m, !is.na(error))
    if (length(args) >= 2) m = filter(m, grepl(args[2], name))
    print(as.data.frame(transmute(m, name, time, error = substr(gsub("[[:space:]]+", " ", error), 1, 160))), row.names = FALSE, right = FALSE)
  },
  dropped = {
    st = toupper(args[2])
    source("scripts/util/globals.R")
    source("scripts/model/live/load.R")
    d26 = find_26_general()
    cells = build_cells(st, d26)
    races = filter(races_from_priors(d26), state == st)
    ret = returns_from_clean(races, d26, geos = unique(cells$geo))
    dr = returns_dropped(cells, ret)
    cat("total votes:", attr(dr, "total_votes"), "\n")
    print(as.data.frame(dr), row.names = FALSE)
    cat("\ntop dropped geos:\n")
    print(as.data.frame(utils::head(attr(dr, "geos"), 25)), row.names = FALSE)
  },
  stop("unknown subcommand ", args[1])
)
