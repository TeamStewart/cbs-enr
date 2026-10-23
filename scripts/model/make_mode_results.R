# ========================================
## make_mode_results.R -- each precinct's 2024 vote-method split from the ACTUAL 2024
## results, for states whose L2-based history ({ST}_precinct_mode_history.parquet) labels
## methods wrongly (NC: early in-person filed as mail in 2022, as Election Day in 2024).
##
## Writes 26_general/input_data/model_inputs/{ST}_precinct_mode_results.parquet:
##   geo, election ("2024_general"), method (early / eday / mail), voters
## live/R/cells.R and live/R/model_medsl.R use this file instead of the L2 history when it
## exists (mode_history() in cells.R). prep_model_inputs.R does not touch it.
##
## Source: our 2024 scrape, 2024 Election Results/24_general/{ST}/clean/{ST}*_latest.parquet
## (President race). Methods: Election Day and Provisional -> eday, Early Voting -> early,
## Absentee/Mail -> mail (over/undervote rows dropped).
## Only precincts whose results match ours (county name + precinct_enr) get a row; no split
## is assumed for anything else. A county counts as matched when most of its precinct-level
## (non-virtual) votes match our precincts.
## The one allocation: votes a county reports in county-level "virtual" precincts (EARLY
## VOTING, EV-<site>, ABSENTEE, ...; in NC 33 of 100 counties report all their early vote
## this way) are spread over that county's matched precincts in proportion to their own
## 2024 votes, so the county's method split is exact and each precinct's is an estimate.
## Not used for MI: our 2024 MI results cover 3 counties whose precinct names don't match
## ours, and MI's L2 history has no early vote, so MI is modeled without vote method
## (build_cells() in live/R/cells.R).
##
## From the cbs-enr folder:
##   Rscript scripts/model/make_mode_results.R NC
# ========================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(stringr) })
source("scripts/model/live/R/paths.R")
args = commandArgs(trailingOnly = TRUE)
st = toupper(grep("^--", args, value = TRUE, invert = TRUE)[1])
if (is.na(st)) stop("Give the state, e.g. Rscript scripts/model/make_mode_results.R NC")
f_arg = sub("^--file=", "", grep("^--file=", args, value = TRUE))
d26 = find_26_general()
dir_src = file.path(dirname(d26), "24_general", st, "clean")
srcs = if (length(f_arg)) path.expand(f_arg) else list.files(dir_src, pattern = paste0("^", st, "(_.*)?_latest\\.parquet$"), full.names = TRUE)
if (!length(srcs)) stop("No ", st, "*_latest.parquet in ", dir_src)
if (any(file.size(srcs) == 0)) stop("0-byte (online-only) files: ", paste(basename(srcs[file.size(srcs) == 0]), collapse = ", "), ". Make them available offline in Dropbox.")
x = bind_rows(lapply(srcs, function(f) nanoparquet::read_parquet(f) |>
                       mutate(across(where(is.factor), as.character), race_id = as.character(race_id))))
x = filter(x, grepl("^(US )?President", race_name, ignore.case = TRUE), !grepl("Overvote|Undervote", vote_mode))
if (!nrow(x)) stop("No President race in ", paste(basename(srcs), collapse = ", "))
x = x |>
  mutate(method = case_when(vote_mode %in% c("Election Day", "Provisional") ~ "eday",
                            vote_mode == "Early Voting" ~ "early",
                            vote_mode == "Absentee/Mail" ~ "mail", TRUE ~ NA_character_),
         county = toupper(jurisdiction))
if (any(is.na(x$method))) stop("Unmapped vote_mode values: ", paste(unique(x$vote_mode[is.na(x$method)]), collapse = ", "))

dir_in = file.path(d26, "input_data", "model_inputs")
inp = nanoparquet::read_parquet(file.path(dir_in, paste0(st, "_precinct.parquet"))) |>
  transmute(geo = as.character(geo), county = toupper(county_name), precinct_id = precinct_enr)

real = filter(x, !virtual_precinct) |> summarize(v = sum(precinct_total), .by = c(county, precinct_id, method))
virt = filter(x, virtual_precinct) |> summarize(virt = sum(precinct_total), .by = c(county, method))
m = inner_join(inp, real, by = c("county", "precinct_id"))
# matched counties: most of their precinct-level (non-virtual) votes match our precincts
cty_votes = x |> filter(!virtual_precinct) |> summarize(v = sum(precinct_total), .by = county)
cty_match = m |> summarize(vm = sum(v), .by = county)
ok_cty = cty_votes |> left_join(cty_match, by = "county") |> filter(coalesce(vm, 0) >= 0.5 * v) |> pull(county)
cat(sprintf("%d of %d counties in our inputs matched; the rest get no row (no split assumed)\n",
            sum(unique(inp$county) %in% ok_cty), n_distinct(inp$county)))

# each matched precinct's own split, plus its county's virtual-precinct votes spread by precinct size
mp = filter(m, county %in% ok_cty)
w = mp |> summarize(v = sum(v), .by = c(geo, county)) |> mutate(w = v / sum(v), .by = county)
alloc = w |> select(geo, county, w) |> inner_join(virt, by = "county", relationship = "many-to-many") |>
  transmute(geo, method, v = w * virt)
out_p = bind_rows(select(mp, geo, method, v), alloc)

out = out_p |>
  summarize(voters = round(sum(v)), .by = c(geo, method)) |>
  tidyr::complete(geo, method = c("early", "eday", "mail"), fill = list(voters = 0)) |>
  mutate(election = "2024_general", .after = geo) |>
  arrange(geo, method)
m = mp
cat(sprintf("%s: %d of %d precincts in our inputs matched by name; %d precincts in the output; %s from virtual precincts spread over their counties\n",
            st, n_distinct(m$geo), nrow(inp), n_distinct(out$geo), format(round(sum(alloc$v)), big.mark = ",")))
sh = out |> summarize(v = sum(voters), .by = method) |> mutate(share = round(v / sum(v), 3))
cat("method split:", paste(sprintf("%s %.1f%%", sh$method, 100 * sh$share), collapse = ", "), "\n")
f = file.path(d26, "input_data", "model_inputs", paste0(st, "_precinct_mode_results.parquet"))
nanoparquet::write_parquet(out, f)
cat("Wrote", f, "\n")
