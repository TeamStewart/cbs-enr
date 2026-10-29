# ========================================
## make_county_history.R -- county history for states votehub doesn't cover (CA, NY), so
## the statewide rows (R/counties.R) can project the counties CBS reports.
##
## Writes 26_general/input_data/county_history/{ST}_county_history.csv, one row per county:
##   county_fips, county_name, pres24_dem, pres24_rep, pres24_total, gov22_total
##
## Source (default): our 2024 county presidential results in the shared Dropbox,
##   2024 Election Results/county_level/data/county24_president.parquet
## (election-night snapshots in the enightmodels format, through certification; the last
## snapshot of method "all" is the final count -- CA's matches the certified totals). It
## has Harris and Trump only, so pres24_total is their sum and the 2024 third-party share
## is taken as 0.
##
## gov22_total -- each county's 2022 midterm vote, used by prep_model_inputs.R to calibrate
## expected turnout and by the statewide step as each county's baseline size:
##   - KEPT from the existing file when this script rebuilds it, so a rerun never blanks it
##   - or read with --v22=<MEDSL 2022 precinct file> --office=<office>, from MEDSL's
##     github.com/MEDSL/2022-elections-official (individual_states/2022-<st>-local-precinct-
##     general.zip; the zip can be passed as is). Use the 2022 statewide race: GOVERNOR (CA),
##     US SENATE (NC, which had no governor race). Its total votes by county, all modes.
##   - blank if neither: the statewide step then uses 2024 presidential votes instead.
## As of 2026-10-29: CA = 2022 governor, NC = 2022 US Senate (both from MEDSL's files).
##
## From the cbs-enr folder:
##   Rscript scripts/model/prep/make_county_history.R CA
##   Rscript scripts/model/prep/make_county_history.R CA NY
##   Rscript scripts/model/prep/make_county_history.R NC --v22=<2022-nc-local-precinct-general.zip> --office="US SENATE"
## --file=<path> reads another file instead: the same enightmodels format, or MEDSL's
## countypres_2000-2024 (csv / tab).
# ========================================
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})
source("scripts/model/live/R/paths.R")
args = commandArgs(trailingOnly = TRUE)
states = toupper(grep("^--", args, value = TRUE, invert = TRUE))
if (!length(states)) {
  stop("Give the state(s), e.g. Rscript scripts/model/prep/make_county_history.R CA")
}
f_arg = sub("^--file=", "", grep("^--file=", args, value = TRUE))
v22_arg = sub("^--v22=", "", grep("^--v22=", args, value = TRUE))
office_arg = toupper(sub("^--office=", "", grep("^--office=", args, value = TRUE)))
if (length(v22_arg) && (length(states) != 1 || !length(office_arg))) {
  stop("--v22= reads one state's MEDSL 2022 file: give one state and --office= (e.g. GOVERNOR)")
}

d26 = find_26_general()
dir_out = file.path(d26, "input_data", "county_history")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# ---- read the source into one row per state x county x party (dem / rep / all) ------------
src = if (length(f_arg)) {
  path.expand(f_arg[1])
} else {
  file.path(dirname(d26), "county_level", "data", "county24_president.parquet")
}
if (!file.exists(src)) {
  stop(
    "Can't find ",
    src,
    ". Make the county_level folder available offline in Dropbox, or use --file=."
  )
}
if (file.size(src) == 0) {
  stop(
    src,
    " is 0 bytes: it is online-only in Dropbox. Right-click it in Finder > Make available offline."
  )
}
x = if (grepl("\\.parquet$", src)) {
  nanoparquet::read_parquet(src)
} else if (grepl("\\.tab$", src)) {
  read_tsv(src, show_col_types = FALSE, guess_max = 1e5)
} else {
  read_csv(src, show_col_types = FALSE, guess_max = 1e5)
}
x = mutate(x, across(where(is.factor), as.character))
names(x) = tolower(names(x))
if (all(c("geo", "method", "votes", "time", "party", "state") %in% names(x))) {
  # enightmodels format: latest snapshot of each county x candidate, method "all"
  if ("contest" %in% names(x)) {
    x = filter(x, contest == "pres")
  }
  if (any(x$method == "all")) {
    x = filter(x, method == "all")
  }
  cty = x |>
    slice_max(time, n = 1, with_ties = FALSE, by = c(state, geo, candidate, method)) |>
    summarize(votes = sum(votes), .by = c(state, geo, party)) |>
    transmute(
      state_po = state,
      county_fips = str_pad(as.character(geo), 5, pad = "0"),
      county_name = NA_character_,
      party = tolower(party),
      votes
    )
  has_total = FALSE
} else if (
  all(c("year", "state_po", "county_fips", "party", "mode", "candidatevotes") %in% names(x))
) {
  # MEDSL countypres: TOTAL rows where a county has them, otherwise the modes added up
  cty = x |>
    filter(year == 2024, !is.na(county_fips)) |>
    mutate(
      county_fips = str_pad(as.character(as.integer(county_fips)), 5, pad = "0"),
      mode = toupper(mode)
    ) |>
    group_by(county_fips) |>
    filter(if (any(mode == "TOTAL")) mode == "TOTAL" else TRUE) |>
    ungroup() |>
    mutate(
      party = case_when(
        toupper(party) == "DEMOCRAT" ~ "dem",
        toupper(party) == "REPUBLICAN" ~ "rep",
        TRUE ~ "oth"
      )
    ) |>
    summarize(
      votes = sum(candidatevotes, na.rm = TRUE),
      county_name = first(county_name),
      .by = c(state_po, county_fips, party)
    )
  has_total = TRUE
} else {
  stop(basename(src), " is neither the enightmodels format nor MEDSL's countypres format")
}

# 2022 midterm votes by county from a MEDSL 2022 precinct file (all modes, the race's
# regular election; special elections and other offices dropped)
read_v22 = function(path, office) {
  m = read_csv(path.expand(path), col_types = cols(.default = "c"), progress = FALSE)
  names(m) = tolower(names(m))
  m = filter(m, toupper(office) == !!office, is.na(special) | toupper(special) == "FALSE")
  if (!nrow(m)) {
    stop("No ", office, " rows in ", basename(path))
  }
  # a precinct reporting both a TOTAL row and its modes: the modes only, so nothing counts
  # twice (precincts with only a TOTAL row keep it)
  m |>
    mutate(county_fips = str_pad(county_fips, 5, pad = "0"), mode = toupper(mode)) |>
    filter(!(mode == "TOTAL" & any(mode != "TOTAL")), .by = c(county_fips, precinct)) |>
    summarize(gov22_total = sum(as.numeric(votes), na.rm = TRUE), .by = county_fips)
}

# ---- one state at a time -------------------------------------------------------------------
for (st in states) {
  y = filter(cty, state_po == st)
  if (!nrow(y)) {
    cat(st, ": no 2024 county rows in", basename(src), "; skipped\n")
    next
  }
  out = y |>
    summarize(
      county_name = first(county_name),
      pres24_dem = sum(votes[party == "dem"]),
      pres24_rep = sum(votes[party == "rep"]),
      pres24_total = sum(votes),
      .by = county_fips
    ) |>
    arrange(county_fips)
  # 2022 midterm votes: from --v22 if given, else whatever the existing file already has
  f = file.path(dir_out, paste0(st, "_county_history.csv"))
  old22 = if (file.exists(f)) {
    read_csv(f, col_types = cols(.default = "c"), progress = FALSE) |>
      transmute(county_fips = str_pad(county_fips, 5, pad = "0"), gov22_total = as.numeric(gov22_total))
  } else {
    tibble(county_fips = character(), gov22_total = numeric())
  }
  v22 = if (length(v22_arg)) read_v22(v22_arg[1], office_arg[1]) else filter(old22, !is.na(gov22_total))
  out = left_join(out, v22, by = "county_fips")
  cat(sprintf(
    "%s: 2022 midterm votes for %d of %d counties (%s)%s\n",
    st,
    sum(!is.na(out$gov22_total)),
    nrow(out),
    if (length(v22_arg)) paste(office_arg[1], "from", basename(v22_arg[1])) else "kept from the existing file",
    if (!any(!is.na(out$gov22_total))) " -- blank: the statewide step uses 2024 presidential votes" else ""
  ))
  if (!has_total) {
    cat(
      "i",
      st,
      ": the source has Harris and Trump only; pres24_total = their sum (third-party share taken as 0)\n"
    )
  }

  # check against the CBS county lookup from prep_model_inputs.R
  p_cty = file.path(d26, "input_data", "model_inputs", paste0(st, "_counties.csv"))
  if (file.exists(p_cty)) {
    lk = read_csv(p_cty, col_types = cols(.default = "c"))
    no_hist = setdiff(lk$county_fips, out$county_fips)
    if (length(no_hist)) {
      cat(
        "!",
        st,
        ":",
        length(no_hist),
        "CBS counties have no 2024 row:",
        paste(head(no_hist, 10), collapse = ", "),
        "\n"
      )
    }
    out = out |>
      left_join(select(lk, county_fips, cbs_name = county_name), by = "county_fips") |>
      mutate(county_name = coalesce(county_name, cbs_name)) |>
      select(-cbs_name)
  }
  write_csv(out, f, na = "")
  cat(sprintf(
    "%s: %d counties; Harris %.1f%% of the two-party vote, %s votes in 2024 -> %s\n",
    st,
    nrow(out),
    100 * sum(out$pres24_dem) / sum(out$pres24_dem + out$pres24_rep),
    format(sum(out$pres24_total), big.mark = ","),
    f
  ))
}
