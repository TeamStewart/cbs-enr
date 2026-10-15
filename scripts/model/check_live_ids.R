# ========================================
## Check precinct ids against LIVE results, county by county
##
## Compares the precinct file from prep_tx_covs.R with every live-results
## file in a folder (e.g. the *_latest.parquet files from
## 26_primaries/TX/clean/ in Dropbox). Uses the same id normalization as the
## prep script ("0007" -> "7", "001A" -> "1A").
##
## Run from the cbs-enr folder:  Rscript scripts/model/check_live_ids.R
# ========================================
suppressPackageStartupMessages(library(tidyverse))

# ---- EDIT THESE PATHS ----------------------------------------------------------
path_covs = "~/Downloads/TX_prec_covs.parquet"
live_dir  = "~/Downloads/tx_live"   # folder holding TX_*_latest.parquet files
# --------------------------------------------------------------------------------

norm_prec_id = function(x) {
  x = str_squish(toupper(as.character(x)))
  x = str_remove(x, "^0+(?=\\d)")
  if_else(x == "", NA_character_, x)
}
norm_geo = function(geo) {
  parts = str_match(geo, "^([a-z]{2}_\\d{3})_(.*)$")
  if_else(is.na(parts[, 1]), geo, paste0(parts[, 2], "_", norm_prec_id(parts[, 3])))
}

covs = nanoparquet::read_parquet(path_covs) |>
  as_tibble() |>
  distinct(geo, county, n) |>
  mutate(county_cbs = str_extract(geo, "(?<=^[a-z]{2}_)\\d{3}"))

files = fs::dir_ls(path.expand(live_dir), regexp = "\\.parquet$")
if (length(files) == 0) cli::cli_abort("No .parquet files in {live_dir}")
cli::cli_h1("Checking {length(files)} live files")

live = map(files, \(f) {
  d = nanoparquet::read_parquet(f) |> as_tibble()
  d |>
    mutate(geo = norm_geo(as.character(geo))) |>
    summarize(votes = sum(votes, na.rm = TRUE), .by = geo) |>
    mutate(file = fs::path_file(f))
}) |>
  bind_rows() |>
  mutate(county_cbs = str_extract(geo, "(?<=^[a-z]{2}_)\\d{3}")) |>
  summarize(votes = max(votes), file = first(file), .by = c(geo, county_cbs))  # same geo in several files

res = covs |>
  filter(county_cbs %in% live$county_cbs) |>
  summarize(
    county = first(county),
    frame_prec = n(),
    frame_voters = sum(n),
    frame_in_live = mean(geo %in% live$geo),
    frame_voters_unmatched = sum(n[!geo %in% live$geo]),
    .by = county_cbs
  ) |>
  left_join(
    live |>
      summarize(
        live_prec = n(),
        live_votes = sum(votes),
        live_in_frame = mean(geo %in% covs$geo),
        live_votes_unmatched = sum(votes[!geo %in% covs$geo]),
        .by = county_cbs
      ),
    by = "county_cbs"
  ) |>
  mutate(across(c(frame_in_live, live_in_frame), \(x) round(x, 3))) |>
  arrange(pmin(frame_in_live, live_in_frame))

print(res, n = Inf, width = Inf)

no_live = setdiff(unique(covs$county_cbs), live$county_cbs)
if (length(no_live) > 0) {
  cli::cli_alert_warning("Counties in the frame with no live file checked: {sort(unique(covs$county[covs$county_cbs %in% no_live]))}")
}
bad = filter(res, frame_in_live < 0.95 | live_in_frame < 0.95)
for (cc in bad$county_cbs) {
  cat("\n---", bad$county[bad$county_cbs == cc], "---\n  frame-only:", head(setdiff(covs$geo[covs$county_cbs == cc], live$geo), 8),
      "\n  live-only: ", head(setdiff(live$geo[live$county_cbs == cc], covs$geo), 8), "\n")
}
