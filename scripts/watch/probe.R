# ========================================
## Watcher L2's only Bash tool: run one jurisdiction's timestamp + scraper against a TEMP
## Dropbox, so nothing touches the live election folder, and print what came back.
##   Rscript scripts/watch/probe.R <ST> <county|NA> <path> [path_timestamp]
## Uses the installed enightdata, or the L2 clone with PROBE_ENIGHTDATA=<dir> (load_all), so a
## patch can be tried before anyone pushes it.
# ========================================
args = commandArgs(trailingOnly = TRUE)
if (length(args) < 3) stop("usage: probe.R <ST> <county|NA> <path> [path_timestamp]")
state = toupper(args[1])
county = if (args[2] %in% c("NA", "")) NA_character_ else args[2]
path = args[3]
path_ts = if (length(args) >= 4) args[4] else path

suppressPackageStartupMessages(library(dplyr))
src = Sys.getenv("PROBE_ENIGHTDATA")
if (nzchar(src)) suppressMessages(devtools::load_all(src, quiet = TRUE)) else library(enightdata)

tmp = tempfile("probe_")
dir.create(file.path(tmp, "26_probe", state, "raw"), recursive = TRUE)
dir.create(file.path(tmp, "26_probe", state, "clean"), recursive = TRUE)
# set the paths in memory only: set_all() would also save them to enightdata's config file
the = get("the", envir = asNamespace("enightdata"))
assign("path_dropbox", tmp, envir = the)
assign("election_folder", "26_probe", envir = the)
assign("election_date", "2026-11-03", envir = the)
# the scrape writes its raw and clean files under the temp folder; uploads are not called here
ts = tryCatch(get(paste0(tolower(state), "_timestamp"))(state = state, county = county, path = path_ts),
              error = function(e) { cat("timestamp error:", conditionMessage(e), "\n"); NA })
cat("timestamp:", format(ts), "\n")
# as get_data() calls it (scripts/functions.R)
d = rlang::exec(get(paste0("scrape_", tolower(state))), state = state, county = county, path = path, timestamp = ts)
cat("rows:", nrow(d), " votes:", sum(d$votes, na.rm = TRUE), "\n")
print(count(d, contest, district, method, name = "rows") |> as.data.frame(), right = FALSE)
print(summarise(d, votes = sum(votes, na.rm = TRUE), .by = c(contest, candidate, party)) |> head(30) |> as.data.frame(), right = FALSE)
