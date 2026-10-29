# ========================================
## make_county_cd.R -- how each county's registrants divide among the 2026 congressional
## districts, for EVERY county in the state (not just the ones we scrape).
##
## live/R/counties.R uses it to project the counties we don't scrape by congressional
## district: an uncounted county's turnout follows what CBS has counted so far in the other
## counties of its district(s), not one statewide ratio. Ansolabehere et al. (2024),
## "Forecasting Turnout" (HDSR 6.4): turnout forecasts are much more accurate by district
## than by county, because counties differ wildly in size and districts don't.
##
## Source: the voter-file frame, 26_general/input_data/vf_frame_current_<month>/state=<ST>/
## (newest month). Its `cd` is the district the 2026 election runs in (codebook), i.e. the
## new maps in NC, TX, FL and CA.
##
## Writes 26_general/input_data/model_inputs/{ST}_county_cd.csv:
##   county_fips, cd (district number; at-large = 1), registrants, share (of the county's
##   registrants)
##
## From the cbs-enr folder (reads only two columns, so even TX takes a minute or so):
##   Rscript scripts/model/prep/make_county_cd.R NC FL TX MI CA AK
# ========================================
suppressPackageStartupMessages(library(dplyr))
source("scripts/model/live/R/paths.R")

STATES = toupper(commandArgs(trailingOnly = TRUE))
if (!length(STATES)) {
  stop("Give the states, e.g. Rscript scripts/model/prep/make_county_cd.R NC FL TX")
}
d26 = find_26_general()
dir_vf = sort(Sys.glob(file.path(d26, "input_data", "vf_frame_current_*")), decreasing = TRUE)[1]
if (is.na(dir_vf)) {
  stop("No vf_frame_current_* folder in ", file.path(d26, "input_data"))
}
dir_out = file.path(d26, "input_data", "model_inputs")
# state FIPS, for files that give only the 3-digit county (AK's are its 40 house districts,
# 02001-02040 in our inputs)
STATE_FIPS = c(AL = "01", AK = "02", AZ = "04", AR = "05", CA = "06", CO = "08", CT = "09",
  DE = "10", DC = "11", FL = "12", GA = "13", HI = "15", ID = "16", IL = "17", IN = "18",
  IA = "19", KS = "20", KY = "21", LA = "22", ME = "23", MD = "24", MA = "25", MI = "26",
  MN = "27", MS = "28", MO = "29", MT = "30", NE = "31", NV = "32", NH = "33", NJ = "34",
  NM = "35", NY = "36", NC = "37", ND = "38", OH = "39", OK = "40", OR = "41", PA = "42",
  RI = "44", SC = "45", SD = "46", TN = "47", TX = "48", UT = "49", VT = "50", VA = "51",
  WA = "53", WV = "54", WI = "55", WY = "56")

for (st in STATES) {
  f = file.path(dir_vf, paste0("state=", st), "part-0.parquet")
  if (!file.exists(f)) {
    cli::cli_alert_warning("{st}: no {f} -- skipped")
    next
  }
  if (file.size(f) == 0) {
    stop(f, " is 0 bytes: make the folder available offline in Dropbox")
  }
  t0 = Sys.time()
  x = nanoparquet::read_parquet(f, col_select = c("county_fips", "cd"))
  out = x |>
    filter(!is.na(cd), !is.na(county_fips)) |>
    mutate(
      county_fips = as.character(county_fips),
      county_fips = if_else(
        nchar(county_fips) <= 3,
        paste0(STATE_FIPS[[st]], stringr::str_pad(county_fips, 3, pad = "0")),
        stringr::str_pad(county_fips, 5, pad = "0")
      ),
      # "NC-01" -> 1; at-large ("WY-AL", or no number) -> 1
      cd = coalesce(as.integer(stringr::str_extract(as.character(cd), "[0-9]+$")), 1L)
    ) |>
    count(county_fips, cd, name = "registrants") |>
    mutate(share = registrants / sum(registrants), .by = county_fips) |>
    arrange(county_fips, desc(share))
  readr::write_csv(out, file.path(dir_out, paste0(st, "_county_cd.csv")))
  s = out |> summarize(k = n(), main = max(share), .by = county_fips)
  cli::cli_alert_success(paste0(
    "{st}: {nrow(s)} counties, {n_distinct(out$cd)} districts; {sum(s$k > 1)} counties ",
    "split across districts ({sum(s$main < 0.95)} with under 95% in one); ",
    "{format(nrow(x) - sum(out$registrants), big.mark = ',')} registrants with no district or county left out ",
    "({round(as.numeric(difftime(Sys.time(), t0, units = 'secs')))} s) -> {st}_county_cd.csv"
  ))
}
