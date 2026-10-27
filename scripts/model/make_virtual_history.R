# ========================================
## make_virtual_history.R -- {ST}_virtual_history.parquet from a past election in the
## scrapers' clean format (state, election, contest, district, candidate, party, method,
## geo, votes), for states that report some vote methods in county- or district-level
## batches instead of by precinct.
##
## A row is a batch when its geo is not a precinct in {ST}_precinct.parquet. For each
## county code (the 3 digits in the geo: a county in NC, a house district in AK) x method
## (prov -> eday, abs -> mail, as in live/R/run.R): the batch votes, how they split by
## party, and the share of that county's method reported in batches (virt_share).
## live/R/virtual.R uses it on election night: a county x method with virt_share >= 0.5 is
## taken out of the precinct fit for model_simpleswing and model_cory and handled as one
## unit. Without the file those cells stay in the precinct fit, which never sees their
## votes (they arrive in the batch), so the batch would be counted twice.
##
## NC's file comes from make_mode_results.R (2024 general). AK has no 2024 results in our
## Dropbox, so AK's comes from the August 2026 primary scrape, which already shows the
## structure: every district reports ALL its early and absentee votes, and its questioned
## (provisional) ballots, as district batches (ak_004_04-early, ak_001_01-absentee, ...).
##
## The batches' party split is written only with --lean: a primary's (AK's) says little
## about the general, so by default dem / rep / oth are left blank and virtual.R projects
## an unreported batch at the precinct baseline of the cells it replaces, moved by tonight's
## swing.
##
## From the cbs-enr folder:
##   Rscript scripts/model/make_virtual_history.R AK                       # 26_general/AK/clean/AK_latest.parquet
##   Rscript scripts/model/make_virtual_history.R AK --file=<parquet> --contest=gov
# ========================================
suppressPackageStartupMessages({ library(dplyr); library(stringr) })
source("scripts/model/live/R/paths.R")
args = commandArgs(trailingOnly = TRUE)
opt = function(k, d = NA) { a = grep(paste0("^--", k, "="), args, value = TRUE); if (length(a)) sub("^[^=]*=", "", a[1]) else d }
st = toupper(grep("^--", args, value = TRUE, invert = TRUE)[1])
if (is.na(st)) stop("Give the state, e.g. Rscript scripts/model/make_virtual_history.R AK")
d26 = find_26_general()
f = opt("file", file.path(d26, st, "clean", paste0(st, "_latest.parquet")))
if (!file.exists(f) || file.size(f) == 0) stop("Missing or online-only: ", f)
x = nanoparquet::read_parquet(f) |> mutate(across(where(is.factor), as.character))
contest = opt("contest", if ("gov" %in% x$contest) "gov" else x$contest[1])
x = filter(x, contest == !!contest)
if (!sum(x$votes)) stop(basename(f), " has no votes for contest ", contest, ": use an election that has been counted")

METHOD_MAP = c(early = "early", eday = "eday", mail = "mail", abs = "mail", prov = "eday")
dir_in = file.path(d26, "input_data", "model_inputs")
geos = nanoparquet::read_parquet(file.path(dir_in, paste0(st, "_precinct.parquet")), col_select = "geo")$geo
x = x |>
  filter(as.character(method) %in% names(METHOD_MAP)) |>
  mutate(method = unname(METHOD_MAP[as.character(method)]),
         county_cbs = str_extract(geo, "(?<=_)[0-9]{3}(?=_)"),
         batch = !geo %in% geos,
         party = case_when(grepl("^dem", party, ignore.case = TRUE) ~ "dem",
                           grepl("^rep", party, ignore.case = TRUE) ~ "rep", TRUE ~ "oth"))
vh = x |>
  # method_votes first: summarize() sees each new column, so `votes` below would be the batch sum
  summarize(method_votes = sum(votes), dem = sum(votes[batch & party == "dem"]),
            rep = sum(votes[batch & party == "rep"]), votes = sum(votes[batch]),
            .by = c(county_cbs, method)) |>
  filter(votes > 0, !is.na(county_cbs)) |>
  mutate(county = county_cbs, oth = votes - dem - rep, virt_share = votes / method_votes,
         source = paste0(basename(f), " (", contest, ")")) |>
  mutate(across(c(dem, rep, oth), \(v) if ("--lean" %in% args) v else NA_real_)) |>
  select(county_cbs, county, method, votes, dem, rep, oth, method_votes, virt_share, source) |>
  arrange(county_cbs, method)

f_out = file.path(dir_in, paste0(st, "_virtual_history.parquet"))
nanoparquet::write_parquet(vh, f_out)
s = vh |> summarize(units = sum(virt_share >= 0.5), votes = sum(votes), share = round(median(virt_share), 3), .by = method)
cat(sprintf("%s (%s): %s batch votes. Mostly batched (>= 50%%), by method: %s\n", st, contest,
            format(sum(vh$votes), big.mark = ","),
            paste(sprintf("%s %d county codes (median batch share %.0f%%)", s$method, s$units, 100 * s$share), collapse = "; ")))
cat("Wrote", f_out, "\n")
