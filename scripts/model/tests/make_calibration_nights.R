# ========================================
## Synthetic election nights for calibration: how often is each model's interval wrong?
##
## Writes N nights to 26_general/model_tests/calibration/nights/night_NNN/ (TX_truth.parquet,
## TX_schedule.parquet, manifest.json), each built from tx_base's turnout, method split and
## reporting schedule, with a NEW two-party result drawn per night. Each precinct's
## Democratic two-party share is
##
##   Harris 2024 share
##   + race shift      the 32-county result lands near tx_base's, +/- RACE_SD (one per race)
##   + county shift    one per county, sd = the real between-county spread of swing
##   + demographic tilt within each county: Hispanic, college and Black shares relative to
##                     the county average, times tilts drawn per night (Hispanic from
##                     -15 to +20 points per 100% Hispanic: the 2024 shift toward Republicans
##                     through the 2026 reversal; college and Black ~ N(0, 5 points))
##   + leftover pattern a REAL precinct swing pattern (sen20->gov22, gov22->pres24 or
##                     sen20->pres24, one per night) with its county averages and
##                     demographic part removed, scaled by 0.75-1.25
##
## so about three-quarters of the variation is within counties, as in real TX swings, and the
## part a county fixed effect cannot absorb varies from night to night. Each method's lean
## (mail, early, Election Day) relative to its precinct is kept from tx_base. The two races
## share the county shifts, tilts and leftover pattern, with their own race shift.
##
## Real swings come from votehub's precinct results, joined to TX_precinct by county and
## precinct number. Demographics are TX_precinct's L2 shares.
##
## Run from the cbs-enr folder (run_calibration.R calls this for you):
##   Rscript scripts/model/tests/make_calibration_nights.R 100        # nights 1-100
##   Rscript scripts/model/tests/make_calibration_nights.R 100 --from=tx_base --seed=2026
## Existing nights are kept, so raising N only adds nights.
# ========================================
source("scripts/model/tests/model_test_common.R")
args = commandArgs(trailingOnly = TRUE)
N = as.integer(c(grep("^[0-9]+$", args, value = TRUE), "100")[1])
opt = function(k, d) { a = grep(paste0("^--", k, "="), args, value = TRUE); if (length(a)) sub("^[^=]*=", "", a[1]) else d }
FROM = opt("from", "tx_base"); SEED = as.integer(opt("seed", "2026"))
RACE_SD = 0.04                                   # spread of the 32-county result around tx_base's
DIR_CAL = file.path(DIR_OUT, "calibration", "nights")
dir.create(DIR_CAL, showWarnings = FALSE, recursive = TRUE)

truth0 = read_pq(file.path(DIR_SIM, FROM, "TX_truth.parquet"))
sched0 = read_pq(file.path(DIR_SIM, FROM, "TX_schedule.parquet"))
inp = read_pq(file.path(DIR_26, "input_data", "model_inputs", "TX_precinct.parquet")) |>
  transmute(geo = as.character(geo), county = county_fips, precinct_raw, dem2,
            hisp = p_hisp, black = p_black, col = coalesce(p_col, 0)) |>
  mutate(across(c(hisp, black, col), \(x) coalesce(x, 0))) |>
  mutate(across(c(hisp, black, col), \(x) x - mean(x), .names = "{.col}_c"), .by = county)

# ---- real swing patterns, on TX_precinct's precincts ---------------------------------
key_of = function(fips5, prec) paste(fips5, str_pad(str_remove_all(toupper(trimws(prec)), "[^0-9A-Z]"), 4, pad = "0"))
vh = read_csv(file.path(DIR_26, "input_data", "votehub-shapefiles", "estimated_past_results.csv"),
              col_types = cols(.default = "c"), progress = FALSE) |>
  filter(state_abbr == "TX") |>
  mutate(key = key_of(str_extract(unit_id, "^[0-9]+"), str_remove(unit_id, "^[0-9]+-:-")),
         across(matches("^(pres24|gov22|sen20)_(dem|rep)$"), as.numeric)) |>
  distinct(key, .keep_all = TRUE)
inp = inp |> mutate(key = key_of(county, precinct_raw)) |> left_join(vh, by = "key")
PAIRS = list(c("sen20", "gov22"), c("gov22", "pres24"), c("sen20", "pres24"))
real = lapply(PAIRS, function(pr) {
  b = inp[[paste0(pr[1], "_dem")]] / (inp[[paste0(pr[1], "_dem")]] + inp[[paste0(pr[1], "_rep")]])
  t = inp[[paste0(pr[2], "_dem")]] / (inp[[paste0(pr[2], "_dem")]] + inp[[paste0(pr[2], "_rep")]])
  w = inp[[paste0(pr[2], "_dem")]] + inp[[paste0(pr[2], "_rep")]]
  ok = is.finite(b) & is.finite(t) & is.finite(w) & w > 50
  x = tibble(geo = inp$geo, county = inp$county, y = if_else(ok, t - b, NA_real_), w = if_else(ok, w, 0),
             hisp_c = inp$hisp_c, black_c = inp$black_c, col_c = inp$col_c)
  cm = x |> filter(w > 0) |> summarize(cm = weighted.mean(y, w), cw = sum(w), .by = county)
  x = x |> left_join(cm, by = "county") |> mutate(r = y - cm)
  f = x |> filter(w > 0)
  beta = stats::lm.wfit(cbind(f$hisp_c, f$black_c, f$col_c), f$r, f$w)$coefficients
  x = x |> mutate(resid = r - as.vector(cbind(hisp_c, black_c, col_c) %*% beta))
  list(pair = paste(pr, collapse = " to "), resid = x |> select(geo, county, resid, w),
       sd_between = sqrt(stats::weighted.mean((cm$cm - stats::weighted.mean(cm$cm, cm$cw))^2, cm$cw)),
       sd_within = sqrt(stats::weighted.mean(f$r^2, f$w)))
})
SD_COUNTY = mean(vapply(real, \(r) r$sd_between, 0))
cat(sprintf("Real TX swings (32 counties): between-county sd %.2f pts; within-county sd %s pts\n", 100 * SD_COUNTY,
            paste(sprintf("%.2f", 100 * vapply(real, \(r) r$sd_within, 0)), collapse = " / ")))

# ---- tx_base cells: two-party total and each method's lean vs its precinct -------------
cells = truth0 |>
  mutate(geo = as.character(geo)) |>
  summarize(d = sum(votes[party == "dem"]), r = sum(votes[party == "rep"]), .by = c(contest, geo, method)) |>
  mutate(two = d + r, s = d / pmax(two, 1)) |>
  mutate(s_p = sum(d) / pmax(sum(two), 1), .by = c(contest, geo)) |>
  mutate(dev = if_else(two > 0, s - s_p, 0)) |>
  left_join(select(inp, geo, county, dem2, hisp_c, black_c, col_c), by = "geo")
base_level = cells |> summarize(lvl = sum(d) / sum(two), base = weighted.mean(dem2, two, na.rm = TRUE), .by = contest)

for (i in seq_len(N)) {
  # each night has its own seed, so night i is the same whatever N is or which nights exist
  set.seed(SEED * 1000L + i)
  ctys = sort(unique(inp$county))
  p = list(
    race_shift = setNames(rnorm(nrow(base_level), 0, RACE_SD), base_level$contest),
    county_shift = setNames(rnorm(length(ctys), 0, SD_COUNTY), ctys),
    tilt_hisp = runif(1, -0.15, 0.20), tilt_col = rnorm(1, 0, 0.05), tilt_black = rnorm(1, 0, 0.05),
    pair = sample(seq_along(real), 1), scale = runif(1, 0.75, 1.25), resid_seed = sample.int(1e6, 1))
  dir_n = file.path(DIR_CAL, sprintf("night_%03d", i))
  if (file.exists(file.path(dir_n, "manifest.json"))) next
  dir.create(dir_n, showWarnings = FALSE)
  # leftover pattern: real residuals; precincts without one borrow a random one from the same county
  set.seed(p$resid_seed)
  rr = real[[p$pair]]$resid |>
    mutate(resid = if (any(!is.na(resid))) {
      pool = resid[!is.na(resid)]
      if_else(is.na(resid), if (length(pool)) sample(pool, n(), replace = TRUE) else 0, resid)
    } else 0, .by = county) |>
    mutate(resid = coalesce(resid, 0))
  new = cells |>
    left_join(select(rr, geo, resid), by = "geo") |>
    left_join(base_level, by = "contest") |>
    mutate(s_prec = dem2 + (lvl - base) + p$race_shift[contest] + p$county_shift[county] +
                    p$tilt_hisp * hisp_c + p$tilt_col * col_c + p$tilt_black * black_c +
                    p$scale * coalesce(resid, 0),
           s_new = pmin(pmax(coalesce(s_prec, s_p) + dev, 0.005), 0.995),
           d_new = if_else(two > 0, round(s_new * two), d), r_new = two - d_new) |>
    select(contest, geo, method, d_new, r_new)
  tr = truth0 |>
    mutate(geo_c = as.character(geo)) |>
    left_join(new, by = c("contest", "geo_c" = "geo", "method")) |>
    mutate(votes = case_when(party == "dem" ~ d_new, party == "rep" ~ r_new, TRUE ~ votes)) |>
    select(-geo_c, -d_new, -r_new)
  write_pq(tr, file.path(dir_n, "TX_truth.parquet"))
  write_pq(sched0, file.path(dir_n, "TX_schedule.parquet"))
  tot = tr |> summarize(dem2 = sum(votes[party == "dem"]) / sum(votes[party %in% c("dem", "rep")]), .by = contest)
  writeLines(jsonlite::toJSON(list(
    night = i, from = FROM, seed = SEED, made_by = "scripts/model/tests/make_calibration_nights.R",
    race_shift = as.list(p$race_shift), county_shift_sd = SD_COUNTY,
    tilt_hisp = p$tilt_hisp, tilt_col = p$tilt_col, tilt_black = p$tilt_black,
    leftover_pattern = real[[p$pair]]$pair, leftover_scale = p$scale,
    result_32_counties = setNames(as.list(tot$dem2), tot$contest)), auto_unbox = TRUE, pretty = TRUE, digits = NA),
    file.path(dir_n, "manifest.json"))
  cat(sprintf("night %03d: Hispanic tilt %+.0f pts, pattern %s; 32-county Dem two-party %s\n", i, 100 * p$tilt_hisp,
              real[[p$pair]]$pair, paste(sprintf("%s %.1f%%", tot$contest, 100 * tot$dem2), collapse = ", ")))
}
cat("Nights in", DIR_CAL, "\n")
