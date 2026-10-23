# ========================================
## Multi-state precinct backtest panels
##
## One reusable panel per state: L2 precinct covariates joined to votehub precinct
## results for every contest that state has. Built so that swing, county ICC, prior
## quality and mode turnout splits can all be computed from the same artifact rather
## than measured ad hoc and remembered.
##
## This is the GENERAL builder. `tx24_backtest_panel.R` stays as the TX-2024
## cadence-specific one (it also needs the returns panel, the truth cells and the mode
## histories, none of which generalize).
##
## LEAKAGE IS THE CONSUMER'S JOB. The panel carries every contest and every L2 history
## column, including vote24*. A backtest of contest X must choose predictors that
## predate X -- see tx24_backtest_panel.R for a worked example of that discipline.
##
## COVERAGE CAVEAT, which must travel with any number computed from these panels:
## L2's county coverage is wildly uneven. GA/NC/IA/NE are effectively full-state;
## TX is 32 of 254 counties, OH 13 of 88, MI 33, FL 32, AK 30. A "statewide" quantity
## computed from a partial, urban-skewed precinct set is NOT the statewide quantity.
## The coverage report below exists so that is a column, not a footnote.
##
## Join: L2 ({2-digit state FIPS} + zero-padded county FIPS, punctuation-stripped
## precinct zero-padded to 4) <-> votehub unit_id "{fips5}-:-{precinct}". Verified at
## 98.9% for TX 2022 and used by both existing TX builders.
##
## Output: scripts/model/panels/{STATE}_results.parquet  votehub results + county FIPS,
##             FULL precinct coverage, no L2 join -- use for swing and county ICC
##         scripts/model/panels/{STATE}_panel.parquet    results + L2 covariates, limited
##             by the precinct-id join -- use only for covariate-adjusted quantities
##         scripts/model/panels/coverage.csv
##
## Run:  Rscript scripts/model/backtest_panel.R          # all overlapping states
##       STATE=GA Rscript scripts/model/backtest_panel.R # one state
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
})

PATH_DROPBOX <- Sys.getenv(
  "PATH_DROPBOX",
  unset = "~/Library/CloudStorage/Dropbox-MIT/Young-Kyung Kim/Contracting/2024 Election Results")
P_L2  <- path.expand(file.path(PATH_DROPBOX, "26_general/input_data/l2_precinct_frame_method.parquet"))
P_VH  <- path.expand(file.path(PATH_DROPBOX, "26_general/input_data/votehub-shapefiles/estimated_past_results.csv"))
OUTDIR <- "scripts/model/panels"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

CONTESTS <- c("pres24", "cong24", "gov22", "sen20", "sen20s")
# state FIPS prefixes, for building the 5-digit county code from L2's 3-digit FIPS
STATE_FIPS <- c(AK = "02", AZ = "04", CA = "06", FL = "12", GA = "13", IA = "19",
                MI = "26", MN = "27", NC = "37", NE = "31", NJ = "34", NM = "35",
                NY = "36", OH = "39", PA = "42", SC = "45", TX = "48", VA = "51",
                WA = "53", WI = "55")

# Precinct-key normalization. The TRAILING PARENTHETICAL rule matters because L2 stores
# several states' precincts as "NAME (ABBREV)" while votehub stores the bare abbreviation
# -- NC is the clearest case ("01 PATTERSON (01)" vs "01"). votehub never uses
# parentheses, so applying the rule to both sides is safe and only bites on L2.
# This does NOT rescue every state: OH is a name-vs-code mismatch (L2 "AUGUSTA" vs
# votehub "AAA") and AK disagrees even on county FIPS. Those need real crosswalks and are
# left alone -- the join-rate column is where that shows up.
norm_prec <- function(x) {
  x <- toupper(trimws(x))
  inner <- str_match(x, "\\(([^()]*)\\)\\s*$")[, 2]
  x <- ifelse(!is.na(inner) & nzchar(trimws(inner)), inner, x)
  str_pad(str_remove_all(x, "[^0-9A-Z]"), 4, pad = "0")
}

cli_h1("Multi-state precinct backtest panels")

# ---- load both sources once, loop states in memory -------------------------------
l2_all <- read_parquet(P_L2) |> as_tibble()
cli_inform("L2 frame: {format(nrow(l2_all), big.mark=',')} rows, \\
            {n_distinct(l2_all$state)} states")

vh_cols <- c("unit_id", "state_abbr",
             as.vector(outer(CONTESTS, c("dem", "rep", "oth", "total"), paste, sep = "_")))
vh_all <- read_csv(P_VH, col_types = cols(.default = "c"), progress = FALSE) |>
  select(any_of(vh_cols))
cli_inform("votehub: {format(nrow(vh_all), big.mark=',')} rows, \\
            {n_distinct(vh_all$state_abbr)} states")

want <- Sys.getenv("STATE", unset = "")
states <- if (nzchar(want)) want else
  sort(intersect(unique(l2_all$state), unique(vh_all$state_abbr)))
cli_inform("states with BOTH L2 and votehub: {paste(states, collapse=' ')}")

# ---- per-state build -------------------------------------------------------------
# the votehub result columns actually present in a frame
ends_with_any <- function(nms) nms[grepl("_(dem|rep|oth|total)$", nms)]

build_one <- function(st) {
  pre <- STATE_FIPS[[st]]
  if (is.null(pre)) { cli_alert_warning("no state FIPS for {st} -- skipped"); return(NULL) }

  l2 <- l2_all |> filter(state == st) |>
    mutate(fips5 = paste0(pre, str_pad(FIPS, 3, pad = "0")),
           geo   = paste(fips5, norm_prec(precinct)))

  vh <- vh_all |> filter(state_abbr == st) |>
    mutate(fips5 = str_extract(unit_id, "^[0-9]+"),
           prec  = str_remove(unit_id, "^[0-9]+-:-"),
           geo   = paste(fips5, norm_prec(prec))) |>
    mutate(across(any_of(setdiff(vh_cols, c("unit_id", "state_abbr"))), as.numeric))
  # a normalized key can collapse sub-precincts; sum rather than drop them
  vh <- vh |>
    summarize(across(any_of(ends_with_any(names(vh))), ~ sum(.x, na.rm = TRUE)), .by = geo)

  # RESULTS-ONLY frame: full votehub precinct coverage, NO L2 join required. Swing and
  # plain county ICC should be computed from this -- they need results and a county, both
  # of which votehub has, and nothing from L2. Keeping them off the join means the weakest
  # link in the pipeline does not set the sample for the quantities that matter most.
  vh_res <- vh |> mutate(state = st, fips5 = str_sub(geo, 1, 5))

  d <- inner_join(l2, vh, by = "geo")

  # which contests this state actually has
  has <- CONTESTS[vapply(CONTESTS, function(c0) {
    cl <- paste0(c0, "_total"); cl %in% names(d) && sum(d[[cl]] > 0, na.rm = TRUE) > 0
  }, TRUE)]

  keep_l2 <- c("n", "active", "male", "fem", "age1", "age2", "age3", "age4",
               "race1", "race2", "race3", "race4", "col_known", "noncol_known",
               "vote16", "vote18", "vote20", "vote22", "vote24",
               as.vector(outer(c("vote16", "vote18", "vote20", "vote22", "vote24"),
                               c("mail", "early", "eday"), paste, sep = "_")))
  panel <- d |>
    transmute(
      state = st, geo, fips5, county = county_name, precinct,
      across(any_of(keep_l2)),
      across(any_of(as.vector(outer(has, c("dem", "rep", "oth", "total"), paste, sep = "_")))),
      # convenience derived columns every consumer wants
      p_male  = male / pmax(male + fem, 1),
      p_age65 = age4 / pmax(n, 1),
      p_black = race2 / pmax(n, 1),
      p_hisp  = race3 / pmax(n, 1),
      p_col   = col_known / pmax(col_known + noncol_known, 1),
      turnout18 = vote18 / pmax(n, 1),
      turnout20 = vote20 / pmax(n, 1),
      turnout22 = vote22 / pmax(n, 1)
    )

  # coverage, against the FULL votehub state (not just the joined rows), which is the
  # most complete precinct universe we have for these states
  vh_state_votes <- vh_all |> filter(state_abbr == st) |>
    mutate(across(any_of(paste0(has, "_total")), as.numeric)) |>
    summarize(across(any_of(paste0(has, "_total")), ~ sum(.x, na.rm = TRUE)))
  joined_votes <- panel |> summarize(across(any_of(paste0(has, "_total")), ~ sum(.x, na.rm = TRUE)))
  ref <- if ("pres24_total" %in% names(vh_state_votes)) "pres24_total" else paste0(has[1], "_total")

  cov <- tibble(
    state = st, contests = paste(has, collapse = "/"),
    l2_prec = nrow(l2), vh_prec = nrow(vh), joined = nrow(panel),
    l2_counties = n_distinct(l2$fips5), joined_counties = n_distinct(panel$fips5),
    pct_l2_joined = round(100 * nrow(panel) / nrow(l2), 1),
    ref_contest = ref,
    pct_votes_of_votehub = round(100 * joined_votes[[ref]] / vh_state_votes[[ref]], 1)
  )
  list(panel = panel, cov = cov, has = has, vh_res = vh_res)
}

res <- map(states, build_one) |> compact()
cov_tbl <- bind_rows(map(res, "cov"))

cli_h2("coverage")
print(as.data.frame(cov_tbl), row.names = FALSE)
cli_inform(c(
  "`pct_votes_of_votehub` is the joined share of the votes in votehub's OWN precinct",
  "universe for that state -- not of the official statewide total. Treat states with low",
  "`joined_counties` as partial and urban-skewed; see the header note."
))

for (r in res) {
  st <- r$cov$state
  write_parquet(r$vh_res, file.path(OUTDIR, paste0(st, "_results.parquet")))
  if (nrow(r$panel) > 0) {
    write_parquet(r$panel, file.path(OUTDIR, paste0(st, "_panel.parquet")))
  } else {
    cli_alert_warning("{st}: 0 precincts joined to L2 -- results frame written, covariate panel skipped")
  }
}
write_csv(cov_tbl, file.path(OUTDIR, "coverage.csv"))
cli_alert_success("wrote {length(res)} panel(s) + coverage.csv to {OUTDIR}")
