# ========================================
## make_mode_composition.R -- county x mode vote composition from OFFICIAL returns.
##
## WHY. The model's mode composition comes from the L2 vote history
## ({ST}_precinct_mode_history.parquet, built by prep_model_inputs.R from
## vote{yy}_{mail,early,eday}). For NC that history is WRONG, in three of its five
## elections, and 2022 -- the one production uses, because 2026 is a midterm -- is one of
## them. Measured 2026-10-26 against official returns:
##
##   NC 2022      mail    early    eday
##   L2           0.579   0.000    0.421     <- what the model consumes today
##   OFFICIAL     0.049   0.531    0.416
##
## L2's ELECTION-DAY share is almost exactly right (0.421 vs 0.416). The entire error is
## that early's 53% was coded into mail -- lookups/mode_scope.csv's "early mislabelled into
## mail", now quantified. L2's per-precinct TOTAL is sound (3,437,858 across 2,619
## precincts against an actual ~3.78M), so only the SPLIT needs replacing, which is what
## prep_model_inputs.R does with this file.
##
## WHY NOT YOUGOV. yougov_precinct_frame_method.parquet DOES fix NC's split and validates
## well -- voter-weighted it reads mail 0.055 / early 0.728 / eday 0.209 against the real
## 2024 result of 0.053 / 0.740 / 0.207. But its `method` is 2024-ONLY (create_frame.R
## changed from coalesce(g2024,g2022,g2020) to coalesce(g2024) on 2026-10-18), so it is a
## PRESIDENTIAL composition. 2026 is a midterm, and NC's real midterm election-day share is
## 0.416 against 2024's 0.207. Using YouGov for 2026 would understate election day by
## 20.7pp -- the red-mirage direction, which is the worst way to be wrong on air.
## Deriving a midterm adjustment instead was also tried and is also wrong: the L2
## midterm/presidential eday ratio in states where L2 is sound (AK 1.32, MI 1.68, FL 1.76,
## TX 1.78) applied to 2024's 0.207 gives 0.27-0.37 against a real 0.416.
##
## WHY COUNTY GRAIN, AND WHY THAT DODGES THE KNOWN BLOCKER. CONTRACT.md's priority-states
## table lists NC's blocker as "the 17% join -- needs a precinct name/abbreviation
## crosswalk". That is a PRECINCT-level problem. At county grain there is no such problem:
## all 100 NC counties are present and all 100 join lookups/model_counties.csv on a
## letters-only key. County grain is also not a big concession -- the real variation is
## mostly between counties (NC eday share 0.269-0.721, median 0.428; early 0.245-0.654) --
## and both cells_from_inputs() and derive_mode_priors() already shrink a precinct toward
## its county, so a county split drops straight into the existing machinery.
##
## WHAT IS AND IS NOT AVAILABLE. The 2022 MEDSL archive's mode vocabulary is per state and
## two of our six have none at all:
##   NC   ABSENTEE BY MAIL / ONE STOP / ELECTION DAY / PROVISIONAL     4 modes
##   AK   ABSENTEE / EARLY VOTING / ELECTION DAY / QUESTION / FEDERAL OVERSEAS
##   CA   ABSENTEE / NOT ABSENTEE / TOTAL                              mail vs not
##   MI   ABSENTEE / ELECTION DAY / TOTAL                              mail vs not
##   FL   TOTAL only                                                   NOTHING
##   TX   TOTAL only                                                   NOTHING
## `TOTAL` coexists with the per-mode rows in CA and MI and must NEVER be summed beside
## them -- the same aggregate trap as `all` and `adv` in the live feed.
##
## MEASURED: ONLY NC IS BROKEN. Everything else validates against its L2 2022:
##   AK  official eday .654 / mail .204 / early .141   vs L2 .666 / .193 / .142   SOUND
##   MI  official eday .588 / mail .412                vs L2 .578 / .422          SOUND
##   CA  official mail .875 / eday .125                vs L2 .897 / .088          SOUND
##   NC  official eday .419 / early .532 / mail .049   vs L2 .421 / .000 / .579   BROKEN
## So this file is written for all four states that have a split -- the other three as
## documentation and as a regression check on L2 -- but prep_model_inputs.R applies it
## only where L2 is known wrong. FL and TX cannot be checked this way at all; TX's L2 is
## corroborated instead by its 2024 split agreeing with YouGov 2024 (early .764 vs .750).
##
## WHY A COMMITTED LOOKUP RATHER THAN READING THE ARCHIVE AT PREP TIME. The source is a
## 51-state set of zips in ~/Work/gh/2022-elections-official, outside both this repo and
## the Dropbox data tree, and NC's CSV alone is 128 MB. A small reviewable CSV in lookups/
## is versioned, diffable in a PR, and does not make prep depend on a path that may not
## exist on another machine -- the same reason vote_mode_map.csv, mode_scope.csv and
## reporting_levels.csv live there.
##
## Usage (from the cbs-enr folder):
##   Rscript scripts/model/make_mode_composition.R
## Writes scripts/model/lookups/mode_composition.csv. Re-run only when the archive or the
## state list changes; the output is committed.
# ========================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
})

DIR_MEDSL = Sys.getenv("MEDSL_2022_DIR",
  unset = file.path(Sys.getenv("HOME"), "Work/gh/2022-elections-official/individual_states"))
OUT  = "scripts/model/lookups/mode_composition.csv"
YEAR = 2022

# The office whose returns define each state's composition. NOT derived: it has to be a
# SINGLE statewide contest, and `magnitude == 1` does not identify one -- NC's "COURT OF
# APPEALS JUDGE" is magnitude 1 and sums to 14.9M across several separate seats, which
# would still give the right SHARES but makes any vote-total sanity check meaningless.
# US Senate / Governor are one seat, statewide, and every voter is eligible.
OFFICE = c(NC = "US SENATE", AK = "GOVERNOR", CA = "GOVERNOR", MI = "ATTORNEY GENERAL")

# MEDSL mode vocabulary -> the model's modes, per state. `prov`, `QUESTION` (AK's word for
# provisional) and `FEDERAL OVERSEAS` are not modes of their own: provisionals arrive after
# the canvass and fold into eday, overseas ballots are mail. Matches BUCKET_FOLD in
# live/R/vote_modes.R. `TOTAL` is deliberately absent so it can never be summed in.
MODE_MAP = list(
  NC = c("ABSENTEE BY MAIL" = "mail", "ONE STOP" = "early",
         "ELECTION DAY" = "eday", "PROVISIONAL" = "eday"),
  AK = c("ABSENTEE" = "mail", "FEDERAL OVERSEAS" = "mail", "EARLY VOTING" = "early",
         "ELECTION DAY" = "eday", "QUESTION" = "eday"),
  CA = c("ABSENTEE" = "mail", "NOT ABSENTEE" = "eday"),
  MI = c("ABSENTEE" = "mail", "ELECTION DAY" = "eday")
)

# States whose L2 mode split is KNOWN WRONG and must be replaced. Everything else is
# written for documentation only. Kept here, beside the evidence, rather than in
# prep_model_inputs.R, so the claim and its measurement sit together.
L2_BROKEN = "NC"

unzip_state = function(st) {
  z = file.path(DIR_MEDSL, sprintf("2022-%s-local-precinct-general.zip", tolower(st)))
  if (!file.exists(z)) {
    cli::cli_alert_warning("no archive for {st} at {.path {z}} -- skipped")
    return(NULL)
  }
  d = file.path(tempdir(), paste0("medsl_", tolower(st)))
  utils::unzip(z, exdir = d, overwrite = TRUE)
  f = list.files(d, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (length(f) != 1) {
    cli::cli_alert_warning("{st}: expected one CSV in the archive, found {length(f)} -- skipped")
    return(NULL)
  }
  f
}

one_state = function(st) {
  f = unzip_state(st); if (is.null(f)) return(NULL)
  cli::cli_h2("{st}  ({round(file.size(f) / 1048576)} MB)")
  d = readr::read_csv(f, show_col_types = FALSE, progress = FALSE,
                      col_select = c(office, mode, votes, county_name, county_fips, precinct))
  off = OFFICE[[st]]
  if (!off %in% d$office)
    cli::cli_abort("{st}: office {.val {off}} not in the archive. Present (top): \\
                    {paste(head(names(sort(table(d$office), decreasing = TRUE)), 5), collapse = ' | ')}")
  mm = MODE_MAP[[st]]
  bad = setdiff(unique(d$mode[d$office == off]), c(names(mm), "TOTAL"))
  if (length(bad))
    cli::cli_abort("{st}: unmapped mode value(s) {.val {bad}} -- add them to MODE_MAP or \\
                    decide explicitly that they are dropped.")
  z = d |>
    filter(office == off, mode %in% names(mm)) |>          # TOTAL excluded by construction
    mutate(method = unname(mm[mode])) |>
    summarize(voters = sum(votes, na.rm = TRUE),
              n_precincts = n_distinct(precinct),
              .by = c(county_name, county_fips, method)) |>
    mutate(share = voters / sum(voters), .by = c(county_name, county_fips)) |>
    mutate(state = st, year = YEAR, source = "medsl_2022_official", office = off,
           replaces_l2 = st %in% L2_BROKEN)
  stopifnot("shares must sum to 1 per county" =
              all(abs(tapply(z$share, z$county_name, sum) - 1) < 1e-9))
  sw = z |> summarize(v = sum(voters), .by = method) |> mutate(sh = v / sum(v))
  cli::cli_alert_success("{st}: {n_distinct(z$county_name)} county(ies), \\
                          {format(sum(z$voters), big.mark = ',')} votes -- \\
                          {paste(sprintf('%s %.3f', sw$method, sw$sh), collapse = '  ')}")
  z
}

res = map(names(MODE_MAP), one_state) |> compact() |> bind_rows()
if (!nrow(res)) cli::cli_abort("no state produced a composition -- check {.path {DIR_MEDSL}}")

res = res |>
  select(state, year, county_fips, county_name, method, voters, share, n_precincts,
         source, office, replaces_l2) |>
  arrange(state, county_name, method)
readr::write_csv(res, OUT)
cli::cli_alert_success("wrote {.path {OUT}}: {nrow(res)} rows, \\
                        {n_distinct(paste(res$state, res$county_name))} state x county")
cli::cli_alert_info("applied by prep_model_inputs.R only where replaces_l2 is TRUE \\
                     ({paste(L2_BROKEN, collapse = ', ')})")
