# ========================================
## make_mode_results.R -- each precinct's vote-method split, combining
##   COUNTY totals by method  from the 2022 general in {ST}_precinct_mode_history.parquet
##                            (for NC, prep_model_inputs.R replaces 2022 with the official
##                            county returns -- MEDSL, county level only)
##   PRECINCT pattern within  from the ACTUAL 2024 precinct results (our scrape), which the
##   each county              2022 county-level history cannot give
## i.e. each county's 2024 precinct x method votes are rescaled, method by method, so the
## county's split equals its 2022 split: which precincts vote early / on the day / by mail
## comes from 2024, how much of the county does comes from 2022 (a midterm, unlike 2024).
## Precincts with no usable 2024 match get their 2022 rows as they are (the county split).
##
## Writes 26_general/input_data/model_inputs/{ST}_precinct_mode_results.parquet:
##   geo, election ("2022_split_2024_pattern"), method (early / eday / mail), voters
## live/R/cells.R and live/R/model_medsl.R use this file instead of the mode history when it
## exists (mode_history() in cells.R), so all three models get the same split.
## prep_model_inputs.R does not touch it: rerun this after rerunning prep for {ST}.
##
## Source: our 2024 scrape, 2024 Election Results/24_general/{ST}/clean/{ST}*_latest.parquet
## (President race). Methods: Election Day and Provisional -> eday, Early Voting -> early,
## Absentee/Mail -> mail (over/undervote rows dropped).
## No crosswalk: 2024 results join our precincts directly on county name + precinct_enr.
## Only counties where most of the 2024 precinct-level (non-virtual) votes match our
## precincts use the 2024 pattern; other precincts keep the 2022 county split.
## Votes a county reports in county-level "virtual" precincts (EARLY VOTING, EV-<site>,
## ABSENTEE, ...; in NC 33 of 100 counties report all their early vote this way) are spread
## over that county's matched precincts in proportion to their own 2024 votes. That only
## sizes the county's expected vote by method: on election night a method a county reports
## mostly virtually is taken out of the precinct model and handled at county level (live/R/
## virtual.R), so no precinct-level split of those votes is used.
## Also writes {ST}_virtual_history.parquet: 2024 virtual votes by county x method (votes,
## dem, rep, oth, and the share of that county's method reported virtually).
## Not used for MI: our 2024 MI results cover 3 counties whose precinct names don't match
## ours, and MI's L2 history has no early vote, so MI is modeled without vote method
## (build_cells() in live/R/cells.R).
##
## From the cbs-enr folder:
##   Rscript scripts/model/prep/make_mode_results.R NC
# ========================================
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})
source("scripts/model/live/R/paths.R")
args = commandArgs(trailingOnly = TRUE)
st = toupper(grep("^--", args, value = TRUE, invert = TRUE)[1])
if (is.na(st)) {
  stop("Give the state, e.g. Rscript scripts/model/prep/make_mode_results.R NC")
}
f_arg = sub("^--file=", "", grep("^--file=", args, value = TRUE))
d26 = find_26_general()
dir_src = file.path(dirname(d26), "24_general", st, "clean")
srcs = if (length(f_arg)) {
  path.expand(f_arg)
} else {
  list.files(dir_src, pattern = paste0("^", st, "(_.*)?_latest\\.parquet$"), full.names = TRUE)
}
if (!length(srcs)) {
  stop("No ", st, "*_latest.parquet in ", dir_src)
}
if (any(file.size(srcs) == 0)) {
  stop(
    "0-byte (online-only) files: ",
    paste(basename(srcs[file.size(srcs) == 0]), collapse = ", "),
    ". Make them available offline in Dropbox."
  )
}
x = bind_rows(lapply(srcs, function(f) {
  nanoparquet::read_parquet(f) |>
    mutate(across(where(is.factor), as.character), race_id = as.character(race_id))
}))
x = filter(
  x,
  grepl("^(US )?President", race_name, ignore.case = TRUE),
  !grepl("Overvote|Undervote", vote_mode)
)
if (!nrow(x)) {
  stop("No President race in ", paste(basename(srcs), collapse = ", "))
}
x = x |>
  mutate(
    method = case_when(
      vote_mode %in% c("Election Day", "Provisional") ~ "eday",
      vote_mode == "Early Voting" ~ "early",
      vote_mode == "Absentee/Mail" ~ "mail",
      TRUE ~ NA_character_
    ),
    county = toupper(jurisdiction)
  )
if (any(is.na(x$method))) {
  stop("Unmapped vote_mode values: ", paste(unique(x$vote_mode[is.na(x$method)]), collapse = ", "))
}

dir_in = file.path(d26, "input_data", "model_inputs")
inp = nanoparquet::read_parquet(file.path(dir_in, paste0(st, "_precinct.parquet"))) |>
  transmute(geo = as.character(geo), county = toupper(county_name), precinct_id = precinct_enr)

real = filter(x, !virtual_precinct) |>
  summarize(v = sum(precinct_total), .by = c(county, precinct_id, method))
virt = filter(x, virtual_precinct) |> summarize(virt = sum(precinct_total), .by = c(county, method))
m = inner_join(inp, real, by = c("county", "precinct_id"))
# matched counties: most of their precinct-level (non-virtual) votes match our precincts
cty_votes = x |> filter(!virtual_precinct) |> summarize(v = sum(precinct_total), .by = county)
cty_match = m |> summarize(vm = sum(v), .by = county)
ok_cty = cty_votes |>
  left_join(cty_match, by = "county") |>
  filter(coalesce(vm, 0) >= 0.5 * v) |>
  pull(county)
cat(sprintf(
  "%d of %d counties in our inputs matched to 2024 precinct results; the rest keep the 2022 county split\n",
  sum(unique(inp$county) %in% ok_cty),
  n_distinct(inp$county)
))

# each matched precinct's own split, plus its county's virtual-precinct votes spread by precinct size
mp = filter(m, county %in% ok_cty)
w = mp |> summarize(v = sum(v), .by = c(geo, county)) |> mutate(w = v / sum(v), .by = county)
alloc = w |>
  select(geo, county, w) |>
  inner_join(virt, by = "county", relationship = "many-to-many") |>
  transmute(geo, method, v = w * virt)
out_p = bind_rows(select(mp, geo, method, v), alloc)

p24 = out_p |>
  summarize(v = sum(v), .by = c(geo, method)) |>
  tidyr::complete(geo, method = c("early", "eday", "mail"), fill = list(v = 0)) |>
  left_join(select(inp, geo, county), by = "geo")
m = mp
cat(sprintf(
  "%s: %d of %d precincts in our inputs matched by name; %s from virtual precincts spread over their counties\n",
  st,
  n_distinct(m$geo),
  nrow(inp),
  format(round(sum(alloc$v)), big.mark = ",")
))

# ---- rescale each matched county to its 2022 split -------------------------------------
SPLIT_ELECTION = "2022_general"
h22 = nanoparquet::read_parquet(file.path(dir_in, paste0(st, "_precinct_mode_history.parquet"))) |>
  filter(election == SPLIT_ELECTION) |>
  transmute(geo = as.character(geo), method, voters = as.numeric(voters)) |>
  inner_join(select(inp, geo, county), by = "geo")
if (!nrow(h22)) {
  stop(st, "_precinct_mode_history.parquet has no ", SPLIT_ELECTION, " rows")
}
tgt = h22 |>
  summarize(v = sum(voters), .by = c(county, method)) |>
  mutate(ts = v / sum(v), .by = county) |>
  select(county, method, ts)
scaled = p24 |>
  mutate(pt = sum(v), .by = geo) |> # precinct's 2024 total
  mutate(cv = sum(v), .by = county) |> # county's 2024 total
  mutate(cmv = sum(v), .by = c(county, method)) |> # county's 2024 total for the method
  left_join(tgt, by = c("county", "method")) |>
  mutate(
    voters = case_when(
      is.na(ts) ~ v, # no 2022 split for the county: keep 2024
      cmv > 0 ~ v * ts * cv / cmv, # the method's 2024 precinct pattern, at the 2022 county share
      TRUE ~ ts * pt
    )
  ) # method absent in 2024: spread by precinct size
fill = h22 |> filter(!geo %in% scaled$geo)
out = bind_rows(select(scaled, geo, method, voters), select(fill, geo, method, voters)) |>
  mutate(voters = round(voters), election = "2022_split_2024_pattern") |>
  select(geo, election, method, voters) |>
  arrange(geo, method)
chk = out |>
  inner_join(select(inp, geo, county), by = "geo") |>
  summarize(v = sum(voters), .by = c(county, method)) |>
  mutate(s = v / sum(v), .by = county) |>
  inner_join(tgt, by = c("county", "method"))
cat(sprintf(
  "%d precincts with the 2024 pattern at the %s county split, %d with the %s county split only (no 2024 match); largest county share gap vs %s: %.4f\n",
  n_distinct(scaled$geo),
  SPLIT_ELECTION,
  n_distinct(fill$geo),
  SPLIT_ELECTION,
  SPLIT_ELECTION,
  max(abs(chk$s - chk$ts))
))
sh = out |> summarize(v = sum(voters), .by = method) |> mutate(share = round(v / sum(v), 3))
cat("method split:", paste(sprintf("%s %.1f%%", sh$method, 100 * sh$share), collapse = ", "), "\n")
f = file.path(d26, "input_data", "model_inputs", paste0(st, "_precinct_mode_results.parquet"))
nanoparquet::write_parquet(out, f)
cat("Wrote", f, "\n")

# ---- virtual-precinct history: how much of each county x method was reported in virtual
# precincts in 2024, and how it voted. live/R/virtual.R uses it on election night: where a
# county reported most of a method virtually, that method is taken out of the precinct
# model and handled as one county-level unit (counted once the batch is in, projected
# from 2024 until then).
lk = read_csv(file.path(dir_in, paste0(st, "_counties.csv")), col_types = cols(.default = "c"))

#' Party label to dem / rep / oth
#'
#' @param p Party labels.
#' @return Character vector.
pty = function(p) {
  case_when(
    grepl("^dem", p, ignore.case = TRUE) ~ "dem",
    grepl("^rep", p, ignore.case = TRUE) ~ "rep",
    TRUE ~ "oth"
  )
}
vh = x |>
  mutate(party = pty(candidate_party)) |>
  summarize(
    votes = sum(precinct_total[virtual_precinct]),
    dem = sum(precinct_total[virtual_precinct & party == "dem"]),
    rep = sum(precinct_total[virtual_precinct & party == "rep"]),
    method_votes = sum(precinct_total),
    .by = c(county, method)
  ) |>
  filter(votes > 0) |>
  mutate(oth = votes - dem - rep, virt_share = votes / method_votes) |>
  left_join(transmute(lk, county = toupper(county_name), county_cbs), by = "county") |>
  relocate(county_cbs, county)
if (any(is.na(vh$county_cbs))) {
  cat(
    "! counties not in ",
    st,
    "_counties.csv: ",
    paste(unique(vh$county[is.na(vh$county_cbs)]), collapse = ", "),
    "\n",
    sep = ""
  )
}
f2 = file.path(d26, "input_data", "model_inputs", paste0(st, "_virtual_history.parquet"))
nanoparquet::write_parquet(filter(vh, !is.na(county_cbs)), f2)
cat(sprintf(
  "Virtual precincts in 2024: %s votes; county x method reported mostly virtually (>= 50%%): %s\n",
  format(sum(vh$votes), big.mark = ","),
  paste(
    sprintf(
      "%s %d counties",
      names(table(vh$method[vh$virt_share >= 0.5])),
      table(vh$method[vh$virt_share >= 0.5])
    ),
    collapse = ", "
  )
))
cat("Wrote", f2, "\n")
