source("scripts/util/crosswalks/votehub.R") # helpers, base_keys(), state_keys(), tiers, match_single()

# Direct ENR -> L2 precinct crosswalk: the fallback for ENR precincts the frame
# crosswalk (input_data/crosswalks/{st}.csv, ENR -> frame -> L2) leaves without an
# L2 id, and for states with no frame crosswalk (NJ, NY, WI, IA).
# Writes input_data/crosswalks/l2_direct/{st}.csv: one row per matched ENR precinct
# (state, county, precinct_enr, precinct_l2, l2_rule, n_l2). Only ENR ids without
# an L2 id in {st}.csv are matched, and only to L2 precincts that {st}.csv does not
# already pair with an ENR id, so the two files together stay 1:1. Read by full_join.R (l2_precinct_map()) and
# match_rates.R. Rebuild after the frame crosswalks and votehub.R:
#
#   Rscript scripts/util/crosswalks/l2_direct.R          # every state with clean files and an L2 file
#   Rscript scripts/util/crosswalks/l2_direct.R NJ IA    # these states
#
# Matching is votehub.R's: within county, strict 1:1, ordered tiers on the ENR id
# (and state_keys() labels) against the L2 precinct name. ENR has no voter counts,
# so there is no ratio check; instead the tiers are those that agreed with the frame
# route on >= 99.4% of the 2026-10-31 ENR ids both routes matched. Dropped:
# name_short (95.3%: OH Butler "fair1wd2" = FAIRFIELD CITY 1-02, Orange NC "CH" =
# CHEEKS), name_init (98.6%: Allegheny "Whitehall Dist 3" = WEST DEER 3) and
# lead_lead (no overlap to check).

L2_DIR = "input_data/crosswalks/l2_direct"
l2_tiers = filter(tiers, !rule %in% c("name_short", "name_init", "lead_lead"))

# Counties never matched directly:
# - GA Burke renumbered its precincts for 2026; L2 (2026-10-06) has the old
#   numbers, so equal numbers are not the same precinct (ga.R).
# - CA counties whose ENR reports consolidated precincts: an ENR id there is
#   a consolidation, not the base precinct with that code (ca_*_consolidated.R).
l2_skip = tribble(
  ~state, ~county,
  "GA",   "BURKE",
  "CA",   "SANDIEGO",
  "CA",   "RIVERSIDE",
  "CA",   "SOLANO",
  "CA",   "NEVADA"
)
# Single precincts never matched directly: L2 merged them with a neighbour that ENR
# still reports separately (ga.R l2_merged).
l2_skip_precinct = tribble(
  ~state, ~county,      ~precinct_enr,
  "GA",   "DOUGHERTY",  "2"            # L2 02 (5,074) = 01 + 02
)
# Size check: where the ENR id has frame rows, the L2 precinct's voters over the
# frame's (summed over its frame precincts) must be within these bounds. Frame and
# L2 are a month apart, so a larger gap means different units (TX Ellis 1038:
# L2 4,983 vs frame 2,234).
RATIO_OK = c(0.5, 2)

l2_path = function(st) {
  f = Sys.glob(election_path("input_data/l2", paste0(str_to_lower(st), "_*_precincts.parquet")))
  if (length(f)) max(f) else character(0)
}

# county key with "&" = "AND" (VA "KING & QUEEN" in the frame, "KING AND QUEEN" in ENR)
ckey = function(st, x) county_key(st, str_replace_all(x, "&", "AND"))

build_l2_direct = function(st) {
  files = Sys.glob(election_path(st, "clean", "*_latest.parquet")) |> fs::path_rel(election_path())
  if (!length(files) || !length(l2_path(st))) return(NULL)

  # what the frame crosswalk already maps
  frame_xw = election_path("input_data/crosswalks", paste0(str_to_lower(st), ".csv"))
  fr_all = if (fs::file_exists(frame_xw)) {
    read_csv(frame_xw, col_types = cols(.default = "c")) |>
      transmute(county = ckey(st, county), precinct_yougov, precinct_enr, precinct_l2)
  } else tibble(county = character(), precinct_yougov = character(), precinct_enr = character(), precinct_l2 = character())
  enr = read_enr(files) |>
    mutate(county_enr = county, county = ckey(st, county))
  # frame counties spelled "<name> CITY" where ENR has "<name>" (VA independent cities)
  city = str_remove(fr_all$county, "CITY$")
  fr_all$county = if_else(!fr_all$county %in% enr$county & city %in% enr$county, city, fr_all$county)
  fx = filter(fr_all, !is.na(precinct_l2), precinct_l2 != "") |> select(-precinct_yougov)

  enr = enr |>
    state_keys(st = st) |>
    anti_join(filter(l2_skip, state == st), join_by(county)) |>
    anti_join(filter(l2_skip_precinct, state == st), join_by(county, precinct_enr)) |>
    anti_join(filter(fx, !is.na(precinct_enr)), join_by(county, precinct_enr))
  l2 = nanoparquet::read_parquet(l2_path(st), col_select = c("county", "precinct", "n")) |>
    as_tibble() |>
    filter(!is.na(precinct), precinct != "", n > 0) |>
    mutate(county = ckey(st, str_remove(str_to_upper(county), " (BOROUGH|PARISH|CENSUS AREA|MUNICIPALITY|CITY AND BOROUGH)$"))) |>
    anti_join(filter(fx, !is.na(precinct_enr)), join_by(county, precinct == precinct_l2))

  enr_keys = bind_cols(
    transmute(enr, county, precinct_l2 = precinct_enr, n_l2 = NA_integer_),
    base_keys(enr$id_label),
    base_keys(enr$enr_name) |> rename_with(\(x) paste0("a_", x))
  )
  l2_keys = bind_cols(
    transmute(l2, county, precinct_yougov = precinct, frame_name = precinct, n_frame = n),
    base_keys(l2$precinct) |> rename_with(\(x) paste0("c_", x)),
    base_keys(l2$precinct) |> rename_with(\(x) paste0("n_", x))
  )
  matched = match_tiers(enr_keys, l2_keys, l2_tiers)
  # one-precinct counties need the whole county unmatched on both sides
  single = if (nrow(fx)) match_single(filter(enr_keys, !county %in% fx$county), filter(l2_keys, !county %in% fx$county), matched)
           else match_single(enr_keys, l2_keys, matched)
  matched = bind_rows(matched, single)

  out = matched |>
    left_join(distinct(enr, county, precinct_enr, county_enr), join_by(county, precinct_l2 == precinct_enr)) |>
    left_join(select(l2, county, precinct, n), join_by(county, precinct_yougov == precinct)) |>
    transmute(state = st, county = county_enr, precinct_enr = precinct_l2, precinct_l2 = precinct_yougov, l2_rule, n_l2 = n) |>
    arrange(county, precinct_enr)
  stopifnot(!anyDuplicated(out[c("county", "precinct_enr")]), !anyDuplicated(out[c("county", "precinct_l2")]))

  # size check against the frame, where the ENR id has frame rows
  if (nrow(fr_all) && nrow(out)) {
    nf = frame_names(st) |>
      summarise(n_frame = sum(n_frame), .by = c(county, precinct_yougov)) |>
      mutate(county = ckey(st, county))
    nf$county = if_else(!nf$county %in% enr$county & str_remove(nf$county, "CITY$") %in% enr$county, str_remove(nf$county, "CITY$"), nf$county)
    nf = fr_all |>
      filter(!is.na(precinct_enr)) |>
      distinct(county, precinct_enr, precinct_yougov) |>
      inner_join(nf, join_by(county, precinct_yougov)) |>
      summarise(n_frame = sum(n_frame), .by = c(county, precinct_enr))
    out = out |>
      mutate(ck = ckey(st, county)) |>
      left_join(nf, join_by(ck == county, precinct_enr)) |>
      mutate(ratio = as.numeric(n_l2) / n_frame) |>
      select(-ck)
    bad = filter(out, !is.na(ratio), ratio < RATIO_OK[1] | ratio > RATIO_OK[2])
    if (nrow(bad)) {
      cli::cli_alert_warning("{st}: {nrow(bad)} direct match{?es} dropped, L2/frame voters outside {RATIO_OK[1]}-{RATIO_OK[2]}")
      print(select(bad, county, precinct_enr, precinct_l2, l2_rule, n_l2, n_frame, ratio), n = Inf)
    }
    out = anti_join(out, bad, join_by(county, precinct_enr)) |> select(-n_frame, -ratio)
  }

  cli::cli_h1(st)
  count(out, l2_rule, sort = TRUE) |> print()
  out |>
    filter(!l2_rule %in% c("code_exact", "name_exact", "code_num", "name_std")) |>
    select(county, precinct_enr, precinct_l2, l2_rule) |>
    slice_sample(n = 15) |>
    print(n = 15)
  all_enr = read_enr(files) |> transmute(k = paste(ckey(st, county), precinct_enr))
  n_fx = sum(all_enr$k %in% paste(fx$county, fx$precinct_enr))
  cli::cli_alert_info("{st}: {nrow(out)} ENR precincts matched directly, on top of {n_fx} from the frame crosswalk ({round((nrow(out) + n_fx) / nrow(all_enr) * 100, 1)}% of {nrow(all_enr)} ENR precincts)")

  fs::dir_create(election_path(L2_DIR))
  enightdata::write_local(out, paste0(L2_DIR, "/", str_to_lower(st), ".csv"), parquet = FALSE)
  out
}

if (sys.nframe() == 0) {
  args = commandArgs(TRUE)
  states = if (length(args)) toupper(args) else {
    f = Sys.glob(election_path("*", "clean", "*_latest.parquet"))
    f = f[file.size(f) > 0]
    sort(unique(basename(dirname(dirname(f)))))
  }
  states = keep(states, \(st) str_detect(st, "^[A-Z]{2}$") && length(l2_path(st)) > 0)
  walk(states, build_l2_direct)
}
