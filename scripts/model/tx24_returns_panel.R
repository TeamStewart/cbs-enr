# ========================================
## TX 2024 US Senate -- election-night returns panel, rebuilt from raw
##
## Produces one row per (geo, method, timestamp) with dem / rep / oth votes, from the
## county election-night scrapes. This is the REAL release cadence: snapshots are
## cumulative WITHIN a county, and counties are scraped INDEPENDENTLY, so a timestamp is
## only comparable to other timestamps of the same county. Any statewide aggregation must
## do an as-of join (latest snapshot per county at or before t) -- see tx24_backtest.R.
##
## Input:  {PATH_DROPBOX}/24_general/TX/raw/*.csv   (249 real CSVs)
##   race_id, race_name, vote_mode, precinct_id, precinct_total,
##   candidate_name, candidate_party, jurisdiction, timestamp
##   NOTE the 469 .zip files in the same folder are 2,616-byte HTML error pages. Ignored.
##   NOTE 24_general/TX/clean/ is EMPTY (NC's has 80 files) -- the TX 2024 cleaning step
##   was never run, which is why this works from raw.
##
## Output: scripts/model/tx24_returns_panel.parquet  (UNFILTERED: every county and
##   precinct with Senate data. The join to the L2 covariate universe happens once, and
##   visibly, in tx24_backtest_panel.R -- not here.)
##
## Lookups (both keyed on state + year):
##   scripts/model/lookups/vote_mode_map.csv
##   scripts/model/lookups/party_map.csv
##
##
## GEO CONVENTION — NOT the production one. This backtest keys on
##   "{fips5} {precinct zero-padded to 4}"  e.g. "48453 0100"
## because it joins votehub `unit_id` to the L2 frame directly. Production uses
##   "{st}_{county_cbs}_{precinct_id}" with LEADING ZEROS DROPPED, e.g. "tx_257_1"
## (CONTRACT.md §0, built by prep_tx_model_inputs.R). The two do NOT interoperate.
## Anything moving between this backtest and the live path must go through the
## crosswalk, not through string surgery -- see §0 on why normalizing ids to close
## crosswalk gaps produces wrong matches.
## Run:  Rscript scripts/model/tx24_returns_panel.R
## Env:  PANEL_REF  -- path to the 2026-10-18 reference CSV; when set, the script asserts
##                     byte-equality against it restricted to the reference's geo set.
# ========================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr); library(cli)
  library(nanoparquet, lib.loc = "renv/library/macos/R-4.5/aarch64-apple-darwin20")
})

PATH_DROPBOX <- Sys.getenv(
  "PATH_DROPBOX",
  unset = "~/Library/CloudStorage/Dropbox-MIT/Young-Kyung Kim/Contracting/2024 Election Results")
RAW   <- path.expand(file.path(PATH_DROPBOX, "24_general/TX/raw"))
P_L2  <- path.expand(file.path(PATH_DROPBOX, "26_general/input_data/l2_precinct_frame_method.parquet"))
OUT   <- "scripts/model/tx24_returns_panel.parquet"
STATE <- "TX"; YEAR <- 2024

mode_map  <- read_csv("scripts/model/lookups/vote_mode_map.csv", col_types = cols(.default = "c")) |>
  filter(state == STATE, year == as.character(YEAR))
party_map <- read_csv("scripts/model/lookups/party_map.csv", col_types = cols(.default = "c")) |>
  filter(state == STATE, year == as.character(YEAR))

cli_h1("TX 2024 US Senate returns panel")

# ---- 1. county name -> FIPS, from the L2 frame -----------------------------------
fips_tbl <- read_parquet(P_L2) |> as_tibble() |> filter(state == STATE) |>
  distinct(county_name, FIPS) |>
  mutate(cty_key = toupper(trimws(county_name)),
         fips5   = paste0("48", str_pad(FIPS, 3, pad = "0"))) |>
  distinct(cty_key, fips5)
cli_inform("county->FIPS from L2: {nrow(fips_tbl)} TX counties")

# ---- 2. race filter --------------------------------------------------------------
# Six observed race_name spellings for the one contest: "United States Senator",
# "US Senator", "U.S. Senator", "U.S. SENATOR", and Kaufman's party-prefixed
# "DEM US Senator" / "REP US Senator". Must NOT catch US Representative.
RE_SEN <- regex("(u\\.?s\\.?|united states)\\s*senator", ignore_case = TRUE)
RE_REP <- regex("represent", ignore_case = TRUE)

files <- list.files(RAW, pattern = "\\.csv$", full.names = TRUE)
cli_inform("raw CSVs: {length(files)}")

read_one <- function(f) {
  d <- suppressWarnings(read_csv(f, col_types = cols(.default = "c"), progress = FALSE))
  if (!all(c("race_name", "vote_mode", "precinct_id", "candidate_name",
             "candidate_party", "jurisdiction", "timestamp") %in% names(d))) return(NULL)
  d |>
    filter(!is.na(race_name), str_detect(race_name, RE_SEN), !str_detect(race_name, RE_REP)) |>
    select(race_name, vote_mode, precinct_id, precinct_total, candidate_name,
           candidate_party, jurisdiction, timestamp)
}
raw <- map(files, read_one) |> compact() |> bind_rows()
cli_inform("Senate rows: {format(nrow(raw), big.mark=',')} \\
            across {n_distinct(raw$jurisdiction)} counties")

# ---- 3. drop non-vote rows -------------------------------------------------------
# A blank candidate_name is a metadata / county-total row (Tarrant alone: 75,712 votes
# worth). A blank precinct_id is a county-level aggregate (Bell has 13 such rows).
n0 <- nrow(raw)
raw <- raw |>
  mutate(across(everything(), ~ replace_na(.x, ""))) |>
  filter(trimws(candidate_name) != "", trimws(precinct_id) != "")
cli_inform("dropped {format(n0-nrow(raw), big.mark=',')} rows with blank \\
            candidate_name or precinct_id")

# ---- 4. vote mode ----------------------------------------------------------------
# Exact-label join, so there is no rule-order hazard. The ORDER the README documents
# (mail -> prov -> early -> eday) matters only for regex matching; an exact lookup makes
# `EV Mail` vs `Early Voting Provisional` unambiguous by construction. Any label not in
# the lookup is a hard error -- silently dropping an unknown mode would short the
# turnout denominator.
raw <- raw |>
  mutate(raw_label = trimws(vote_mode)) |>
  left_join(select(mode_map, raw_label, method), by = "raw_label")
unmapped <- raw |> filter(is.na(method)) |> count(raw_label, sort = TRUE)
if (nrow(unmapped)) {
  print(as.data.frame(unmapped))
  cli_abort("{nrow(unmapped)} unmapped vote_mode label(s) -- add them to \\
             lookups/vote_mode_map.csv before proceeding")
}
cli_alert_success("vote modes: 0 unmapped labels")
n0 <- nrow(raw)
raw <- raw |> filter(method != "DROP")
cli_inform("dropped {format(n0-nrow(raw), big.mark=',')} rows on DROP labels \\
            (PROV/EV/ED composite, regVotersCounty, Undervotes, Overvotes)")

# ---- 5. party --------------------------------------------------------------------
# Fallback chain, because the counties disagree about where the party lives:
#   1. candidate_party, exact        (DEM / Dem / D / (D) / ...)
#   2. leading parenthetical in candidate_name   -- Travis: "(D) Colin Allred"
#   3. leading party token in candidate_name     -- Bell/Montague: "DEM Colin Allred"
#   4. DEM/REP prefix on race_name               -- Kaufman: "DEM US Senator"
#   5. otherwise `oth`                           -- write-ins with no party recorded
# Step 5 is where Analisa Roche / Tracy Andrus / "Write-in" land: real write-in Senate
# candidates whose party field the county left blank. They STAY as oth rather than being
# dropped -- dropping them would short the turnout denominator and bias every share up.
# Guarded below so a major-party candidate can never fall through unnoticed.
pm <- setNames(party_map$party, party_map$raw_label)
map_party <- function(x) unname(pm[trimws(x)])

raw <- raw |>
  mutate(
    p1 = map_party(candidate_party),
    p2 = map_party(str_extract(candidate_name, "^\\s*\\([A-Za-z]\\)")),
    p3 = map_party(str_extract(candidate_name, "^\\s*[A-Za-z]{3}(?=\\s)")),
    p4 = map_party(str_extract(race_name, "^\\s*(DEM|REP)(?=\\s)")),
    party = coalesce(p1, p2, p3, p4)
  )
ft_names <- raw |> filter(is.na(party)) |> distinct(candidate_name) |> pull(candidate_name)
raw <- raw |> mutate(party = coalesce(party, "oth")) |> select(-p1, -p2, -p3, -p4)
if (length(ft_names))
  cli_alert_info("party fell through to `oth` for: {paste(ft_names, collapse=' | ')}")

# ---- 6. votes --------------------------------------------------------------------
# `precinct_total` is the vote count on the candidate x mode x precinct row.
raw <- raw |> mutate(votes = suppressWarnings(as.numeric(precinct_total))) |>
  mutate(votes = coalesce(votes, 0))

# ---- 7. precinct key -------------------------------------------------------------
# geo = "{fips5} {pppp}", the SAME key as tx_backtest_panel.parquet. Four rules, in order:
#   strip a Precinct/PCT prefix (Bastrop "Precinct 1001")
#   split on dash, keep the lead (Dallas sub-precincts "1004-01" -> 1004, summed)
#   strip punctuation
#   zero-pad to 4            (Travis "100" -> "0100"; Montague "10" -> "0010")
norm_prec <- function(x) {
  x |> toupper() |> trimws() |>
    str_remove("^(PRECINCT|PCT)\\.?\\s*") |>
    str_remove("-.*$") |>
    str_remove_all("[^0-9A-Z]") |>
    str_pad(4, pad = "0")
}
raw <- raw |>
  mutate(cty_key = toupper(trimws(jurisdiction))) |>
  left_join(fips_tbl, by = "cty_key")
nofips <- raw |> filter(is.na(fips5)) |> distinct(cty_key)
if (nrow(nofips)) cli_abort("no FIPS for county: {nofips$cty_key}")
raw <- raw |> mutate(geo = paste(fips5, norm_prec(precinct_id)))

# ---- 8. timestamp ----------------------------------------------------------------
# Raw carries a -05:00 offset (CDT). Stored as naive LOCAL time, which is what the
# reference panel holds and what the 19:00 poll-close reasoning assumes.
raw <- raw |> mutate(ts = str_sub(trimws(timestamp), 1, 19))
bad_ts <- sum(is.na(as.POSIXct(raw$ts, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")))
if (bad_ts) cli_abort("{bad_ts} unparseable timestamps")

# ---- 9. aggregate ----------------------------------------------------------------
panel <- raw |>
  summarize(votes = sum(votes), .by = c(geo, method, ts, party)) |>
  pivot_wider(names_from = party, values_from = votes, values_fill = 0) |>
  { \(x) { for (p in c("dem", "rep", "oth")) if (!p %in% names(x)) x[[p]] <- 0; x } }() |>
  transmute(geo, method, timestamp = ts, dem, rep, oth) |>
  arrange(geo, method, timestamp)

cli_h2("panel")
cli_inform(c(
  "rows: {format(nrow(panel), big.mark=',')}",
  "precincts: {format(n_distinct(panel$geo), big.mark=',')}   \\
   counties: {n_distinct(str_sub(panel$geo, 1, 5))}   \\
   modes: {paste(sort(unique(panel$method)), collapse='/')}   \\
   timestamps: {n_distinct(panel$timestamp)}"
))

# final snapshot per county, to sanity-check against the official figures
fin <- panel |> mutate(cty = str_sub(geo, 1, 5)) |>
  filter(timestamp == max(timestamp), .by = cty)
tot <- fin |> summarize(dem = sum(dem), rep = sum(rep), oth = sum(oth))
cli_inform(c(
  "final (all counties): dem {format(tot$dem, big.mark=',')} / \\
   rep {format(tot$rep, big.mark=',')} / oth {format(tot$oth, big.mark=',')}",
  "  dem share of all {round(100*tot$dem/(tot$dem+tot$rep+tot$oth), 3)}%   \\
   two-party {round(100*tot$dem/(tot$dem+tot$rep), 3)}%   \\
   third-party {round(100*tot$oth/(tot$dem+tot$rep+tot$oth), 3)}%"
))
mode_split <- fin |> summarize(v = sum(dem + rep + oth), .by = method) |>
  mutate(pct = round(100 * v / sum(v), 2)) |> arrange(desc(v))
print(as.data.frame(mode_split), row.names = FALSE)

# guard on the party fall-through: a major-party candidate must never reach step 5.
# Write-ins are a rounding error; anything above 1% of the panel means the chain is wrong.
panel_votes <- tot$dem + tot$rep + tot$oth
ft_votes <- raw |> mutate(cty = str_sub(geo, 1, 5)) |>
  filter(ts == max(ts), .by = cty) |>                 # final snapshot only, not cumulative
  filter(candidate_name %in% ft_names) |>
  summarize(v = sum(votes), .by = candidate_name) |> arrange(desc(v))
if (nrow(ft_votes)) {
  cli_inform("votes assigned to `oth` by fall-through:")
  print(as.data.frame(mutate(ft_votes, pct_of_panel = round(100*v/panel_votes, 4))), row.names = FALSE)
  if (max(ft_votes$v) / panel_votes > 0.01)
    cli_abort("a fall-through candidate holds >1% of the panel -- the party chain is \\
               mis-assigning a major-party candidate")
}

# ---- 10. verification gate against the 2026-10-18 reference --------------------
# The reference panel was built in a prior session whose builder was never saved; this
# script replaces it. The gate is equality AFTER restricting to the reference's geo set,
# because the reference was filtered to the L2 covariate universe and this output is not.
ref_path <- Sys.getenv("PANEL_REF", unset = "")
if (nzchar(ref_path) && file.exists(ref_path)) {
  cli_h2("verification against reference panel")
  ref <- read_csv(ref_path, col_types = cols(geo = "c", method = "c", timestamp = "c",
                                             dem = "d", rep = "d", oth = "d")) |>
    arrange(geo, method, timestamp)
  mine <- panel |> semi_join(distinct(ref, geo), by = "geo") |> arrange(geo, method, timestamp)
  cli_inform("reference rows {format(nrow(ref), big.mark=',')}   \\
              mine restricted to reference geos {format(nrow(mine), big.mark=',')}")
  cmp <- full_join(ref,  mine, by = c("geo", "method", "timestamp"),
                   suffix = c("_ref", "_new"))
  only_ref <- cmp |> filter(is.na(dem_new)); only_new <- cmp |> filter(is.na(dem_ref))
  diffs <- cmp |> filter(!is.na(dem_new), !is.na(dem_ref)) |>
    filter(dem_ref != dem_new | rep_ref != rep_new | oth_ref != oth_new)
  cli_inform(c("rows only in reference: {nrow(only_ref)}",
               "rows only in new:       {nrow(only_new)}",
               "key-matched rows differing on votes: {nrow(diffs)}"))
  if (nrow(only_ref) || nrow(only_new) || nrow(diffs)) {
    if (nrow(only_ref)) print(head(as.data.frame(only_ref), 10), row.names = FALSE)
    if (nrow(only_new)) print(head(as.data.frame(only_new), 10), row.names = FALSE)
    if (nrow(diffs))    print(head(as.data.frame(diffs), 10), row.names = FALSE)
    cli_alert_danger("REFERENCE MISMATCH -- isolate before using this panel")
  } else {
    cli_alert_success("reference reproduced exactly: {format(nrow(ref), big.mark=',')} rows")
  }
}

write_parquet(panel, OUT)
cli_alert_success("wrote {OUT}")
