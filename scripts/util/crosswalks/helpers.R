library(tidyverse)
library(arrow)

# Shared pieces for the per-state precinct crosswalks (YouGov frame <-> ENR <-> L2).
# Process and rules: docs/precinct_crosswalks.md. Reference state: nc.R.
# Per-state scripts source this file and should not edit it; define new generic
# helpers locally and list them under "Follow-ups" in docs/crosswalk_todo.md.

election_path = function(...) fs::path(enightdata:::the$path_dropbox, enightdata:::the$election_folder, ...)

countyfips = enightmodels::cbs_counties |>
  select(state, county, county_cbs) |>
  mutate(county = toupper(county))

# ========================================
## Inputs
# ========================================

# Frame rows for one state. Never modify this object downstream. `id` picks the
# frame column used as precinct_yougov: precinct26_id (default; unique within
# county x cd except MI) or precinct26_code (unique nationally).
read_frame = function(st, id = "precinct26_id") {
  enightdata::read_local("input_data/yougov_precinct_frame.csv") |>
    as_tibble() |>
    filter(state == st) |>
    select(state, county = county_name, cd, precinct_yougov = all_of(id)) |>
    arrange(county, precinct_yougov)
}

# Precinct names + voter counts for the frame codes, from the voter file the
# frame was built from (see input_data/create_frame.R).
frame_names = function(st) {
  open_dataset(election_path(paste0("input_data/vf_frame_current_202609/state=", st))) |>
    count(county = county_name, precinct_yougov = precinct26_id, frame_name = precinct26_name, name = "n_frame") |>
    collect() |>
    filter(!is.na(precinct_yougov))
}

# Distinct ENR precincts from `{ST}/clean/` files (relative to the election
# folder). `geo` is `st_countyfips_precinct`; `precinct_enr` keeps the raw id and
# `precinct` is the copy that state scripts may recode.
read_enr = function(files) {
  map(files, \(f) enightdata::read_local(f, parquet = TRUE) |> distinct(state, geo)) |>
    list_rbind() |>
    distinct() |>
    separate_wider_regex(geo, c("^[a-z]{2}_", county_fips = "\\d+", "_", precinct = ".*$")) |>
    mutate(county_fips = as.integer(county_fips)) |>
    left_join(countyfips, join_by(state, county_fips == county_cbs)) |>
    distinct(state, county, precinct) |>
    mutate(precinct_enr = precinct)
}

# ========================================
## Name standardization
# ========================================

std_name = function(x) {
  x |>
    str_to_upper() |>
    str_remove("\\s*\\([^)]*\\)\\s*$") |>       # trailing "(code)"
    str_replace_all("[#,./-]", " ") |>
    str_replace_all("\\bPCT\\b", "PRECINCT") |>
    str_remove_all("\\b(NUMBER|WARD)\\b") |>
    str_replace_all("\\b0+(?=\\d)", "") |>      # "01" -> "1", "04A" -> "4A"
    str_squish()
}
strip_lead = function(x) str_remove(x, "^\\d+[A-Z]?\\d?\\s+(?=[A-Z])") # "03 CRESTON" -> "CRESTON"
bag = function(x) map_chr(str_split(x, " "), \(t) paste(sort(t), collapse = " "))

# ========================================
## Tiered L2 -> frame matching
# ========================================
# `l2_keys` needs county, precinct_l2, n_l2 plus key columns; `fr_keys` needs
# county, precinct_yougov, frame_name, n_frame plus key columns; `tiers` has
# rule, l2_key, fr_key. A tier only accepts keys unique within county on both
# sides, and a frame code claimed by an earlier tier is never reused (strict 1:1).

match_tiers = function(l2_keys, fr_keys, tiers) {
  matched = tibble(county = character(), precinct_l2 = character(), precinct_yougov = character())
  for (i in seq_len(nrow(tiers))) {
    tier = tiers[i, ]
    l = l2_keys |>
      anti_join(matched, join_by(county, precinct_l2)) |>
      select(county, precinct_l2, n_l2, key = all_of(tier$l2_key)) |>
      filter(!is.na(key), key != "") |>
      filter(n() == 1, .by = c(county, key))
    f = fr_keys |>
      select(county, precinct_yougov, frame_name, n_frame, key = all_of(tier$fr_key)) |>
      filter(!is.na(key), key != "") |>
      filter(n() == 1, .by = c(county, key)) |>
      anti_join(matched, join_by(county, precinct_yougov))
    matched = bind_rows(matched, inner_join(l, f, join_by(county, key)) |> mutate(l2_rule = tier$rule, .keep = "unused"))
  }
  stopifnot(
    !anyDuplicated(matched[c("county", "precinct_l2")]),
    !anyDuplicated(matched[c("county", "precinct_yougov")])
  )
  matched
}

# Review: match counts by rule, L2/frame voter ratios outside the normal range,
# and unmatched frame precincts. Exact name/code matches with odd ratios are kept
# (same official ID, different voter-file vintage); inspect fuzzy-rule rows.
review_matches = function(l2_matched, fr_keys) {
  l2_matched |>
    mutate(ratio = n_l2 / n_frame) |>
    summarise(n = n(), q01 = quantile(ratio, .01), med = median(ratio), q99 = quantile(ratio, .99), .by = l2_rule) |>
    print()
  l2_matched |>
    filter(n_l2 / n_frame < .8 | n_l2 / n_frame > 1.3) |>
    select(county, precinct_l2, precinct_yougov, frame_name, l2_rule, n_l2, n_frame) |>
    print(n = Inf)
  anti_join(fr_keys, l2_matched, join_by(county, precinct_yougov)) |>
    select(county, precinct_yougov, frame_name, n_frame) |>
    print(n = Inf)
  invisible(l2_matched)
}

# ========================================
## Output
# ========================================

write_crosswalk = function(st, crosswalk) {
  fs::dir_create(election_path("input_data/crosswalks"))
  enightdata::write_local(crosswalk, paste0("input_data/crosswalks/", str_to_lower(st), ".csv"), parquet = FALSE)
}
