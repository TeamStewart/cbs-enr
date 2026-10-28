source("scripts/util/crosswalks/helpers.R")

# Solano only: aggregate the YouGov frame up to Solano's consolidated election
# precincts so it can be merged with ENR. Same exception to rule 3 ("merges stay
# NA") as ca_sandiego_consolidated.R; `ca.csv` keeps Solano ENR NA and this script
# writes separate files instead.
#
# Solano ENR (LiveVoterTurnout) reports 196 consolidations named after one base
# precinct ("17025", shown as "17025 - BT-46" with its ballot type) covering 608
# frame base precincts ("17005.00"). 185 of the 196 names are frame base codes.
#
# Membership is INFERRED, not official. No 2026 consolidation-to-base list was
# found (solanocounty.gov, 2026-10-23: polling place list and maps only). Rule
# ("namesake_floor", as in San Diego): each frame base precinct goes to the
# consolidation with the largest namesake code <= its own code. Bases below the
# first namesake (17005-17020) get no consolidation.
# Checks:
#   - cd_ok: every member base's dominant frame CD equals the ENR house district.
#     Weak here: Solano is 99.8% CA-08 in the frame.
#   - city_ok (hard): a consolidation shares one ballot, and the cities have their
#     own contests and measures, so all members must be in the same city, or all
#     unincorporated. The place is the L2 name prefix from ca.csv precinct_l2
#     ("FAIRFIELD 53053" -> FAIRFIELD); members without L2 are not checked.
#   - place_share (soft): share of member voters whose L2 place equals the
#     namesake's L2 place. Under this rule it is 97.7% of voters; the
#     alternative "smallest namesake >= base" rule gives 94.8%.
# ENR has no per-precinct registration (LVT gives a county total only) and the
# votes are still 0 (2026-10-23), so recheck on real results: ballots per
# consolidation should be <= and roughly proportional to n.
#
# Aggregation as in San Diego: n is summed; every other frame variable is a mean
# weighted by n. Requires input_data/crosswalks/ca.csv (run ca.R first).

frame_vars = c("turnout20", "turnout22", "turnout24", "turnoutPred", "age", "male",
               "pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth", "pr_harris", "pr_trump")
cities = c("BENICIA", "DIXON", "FAIRFIELD", "RIO VISTA", "SUISUN", "VACAVILLE", "VALLEJO")

so_frame = enightdata::read_local("input_data/yougov_precinct_frame.parquet", parquet = TRUE) |>
  as_tibble() |>
  filter(state == "CA", county_name == "SOLANO")
stopifnot(all(str_detect(so_frame$precinct26_id, "^\\d{5}\\.00$")))

cons = enightdata::read_local("CA/clean/CA_solano_latest.parquet", parquet = TRUE) |>
  as_tibble() |>
  filter(contest == "house") |>
  distinct(geo, cd_enr = district) |>
  mutate(precinct_enr = str_remove(geo, "^ca_\\d+_"), namesake = as.integer(precinct_enr)) |>
  arrange(namesake)
stopifnot(!anyDuplicated(cons$precinct_enr), all(str_detect(cons$precinct_enr, "^\\d{5}$")))

l2_place = read_csv(election_path("input_data/crosswalks/ca.csv"), col_types = cols(.default = "c")) |>
  filter(county == "SOLANO", !is.na(precinct_l2)) |>
  distinct(precinct_yougov, l2_place = str_remove(precinct_l2, " \\d+$")) # "FAIRFIELD 53053" -> "FAIRFIELD"

members = so_frame |>
  mutate(base = as.integer(str_remove(precinct26_id, "\\.00$"))) |>
  mutate(namesake = cons$namesake[na_if(findInterval(base, cons$namesake), 0)]) |>
  left_join(select(cons, namesake, precinct_enr, cd_enr), join_by(namesake)) |>
  left_join(l2_place, join_by(precinct26_id == precinct_yougov)) |>
  # the namesake's own L2 place (NA for the 11 namesakes that are not frame bases)
  left_join(transmute(l2_place, namesake = as.integer(str_remove(precinct_yougov, "\\.00$")), place = l2_place), join_by(namesake)) |>
  mutate(
    cd_match = cd == paste0("CA-", str_pad(cd_enr, 2, pad = "0")),
    base_cd_ok = any(cd_match & n == max(n)), .by = precinct26_id
  ) |>
  mutate(
    city = if_else(l2_place %in% cities, l2_place, if_else(is.na(l2_place), NA, "UNINCORPORATED")),
    place_match = l2_place == place,
    member_rule = if_else(is.na(precinct_enr), "none", "namesake_floor")
  )
stopifnot(nrow(members) == nrow(so_frame))

consolidated = members |>
  filter(!is.na(precinct_enr)) |>
  summarise(
    n_base = n_distinct(precinct26_id),
    cd_ok = all(base_cd_ok),
    city_ok = n_distinct(city, na.rm = TRUE) <= 1,
    cd_frame_share = sum(n[cd_match]) / sum(n),
    place_share = sum(n[place_match %in% TRUE]) / sum(n[!is.na(place_match)]),
    across(all_of(frame_vars), \(x) weighted.mean(x, n, na.rm = TRUE)),
    n = sum(n),
    .by = c(state, county_name, precinct_enr, cd_enr)
  ) |>
  mutate(cd = paste0("CA-", str_pad(cd_enr, 2, pad = "0")), use = cd_ok & city_ok) |>
  select(-cd_enr) |>
  relocate(state, county_name, cd, precinct_enr, n, n_base, use, cd_ok, city_ok, cd_frame_share, place_share) |>
  arrange(precinct_enr)

unassigned = filter(members, is.na(precinct_enr))
cat("consolidations in ENR:", nrow(cons), " with frame members:", nrow(consolidated),
    " use = TRUE:", sum(consolidated$use), " (cd_ok", sum(consolidated$cd_ok), ", city_ok", sum(consolidated$city_ok), ")\n")
cat("frame voters:", sum(so_frame$n), " in use = TRUE consolidations:", sum(consolidated$n[consolidated$use]),
    sprintf("(%.1f%%)", 100 * sum(consolidated$n[consolidated$use]) / sum(so_frame$n)),
    " unassigned bases:", n_distinct(unassigned$precinct26_id), "voters", sum(unassigned$n), "\n")
cat("voter-weighted place agreement (bases with L2):",
    with(filter(members, !is.na(place_match)), weighted.mean(place_match, n)), "\n")
consolidated |> filter(!use) |> select(precinct_enr, cd, n, n_base, cd_ok, city_ok, place_share) |> print(n = 30)

stopifnot(
  sum(consolidated$n) + sum(unassigned$n) == sum(so_frame$n),
  !anyDuplicated(consolidated$precinct_enr)
)

enightdata::write_local(
  select(members, state, county_name, cd, precinct_yougov = precinct26_id, n, precinct_enr, member_rule, base_cd_ok, l2_place, city, place),
  "input_data/crosswalks/ca_solano_members.csv", parquet = FALSE
)
enightdata::write_local(consolidated, "input_data/crosswalks/ca_solano_frame_consolidated.csv", parquet = FALSE)
