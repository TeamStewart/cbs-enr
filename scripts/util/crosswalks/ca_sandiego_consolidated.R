source("scripts/util/crosswalks/helpers.R")

# San Diego only: aggregate the YouGov frame up to San Diego's consolidated
# election precincts so it can be merged with ENR.
#
# SPECIFIC CHOICE (logged in docs/precinct_crosswalks.md and docs/crosswalk_todo.md):
# this is a deliberate exception to rule 3 ("merges stay NA"), made for San Diego
# only. San Diego ENR reports 2,994 consolidations (e.g. "0002-100090-SAN PASQUAL"
# = consolidation 0002, named after base precinct 100090) covering 6,427 frame base
# precincts, so a 1:1 crosswalk gives no ENR at all. `ca.csv` keeps San Diego ENR NA
# and stays strictly 1:1. This script writes separate files instead.
#
# Membership is INFERRED, not official. No 2026 consolidation-to-base list was
# found (sdvote.com, 2026-10-15). Rule ("namesake_range"): consolidation numbers
# and namesake base codes are both ascending, so each frame base precinct is
# assigned to the consolidation with the largest namesake code <= its own code.
# Checks:
#   - cd_ok: every member base's dominant frame CD (ties: any tied CD) equals the consolidation's
#     ENR house district. A consolidation shares one ballot, so it cannot cross a
#     CD. This is a hard check; failing consolidations get use = FALSE.
#   - place_share: share of member voters whose L2 community name (from
#     ca.csv precinct_l2) equals the consolidation's place name. Soft check only:
#     rural consolidations legitimately span communities (PALA -> RAINBOW).
# As of 2026-10-15 the ENR votes are placeholders (uncorrelated with frame size),
# so the mapping cannot yet be checked against ballot totals. Recheck on real
# results: ballots per consolidation should be <= and roughly proportional to n.
#
# Aggregation: n is summed; every other frame variable is a mean weighted by n
# (over members with non-NA values). `age` is a precinct median in the frame, so
# its aggregate is an n-weighted mean of medians, not a true median.
# Frame rows with any cd are pooled into the consolidation (the frame cd is a
# per-voter assignment; a base precinct is in one CD). The output cd is the ENR
# district; cd_frame_share is the share of n whose frame cd agrees.
#
# Requires input_data/crosswalks/ca.csv (run ca.R first).

frame_vars = c("turnout20", "turnout22", "turnout24", "turnoutPred", "age", "male",
               "pred.whi", "pred.bla", "pred.his", "pred.asi", "pred.oth", "pr_harris", "pr_trump")

sd_frame = enightdata::read_local("input_data/yougov_precinct_frame.csv") |>
  as_tibble() |>
  filter(state == "CA", county_name == "SAN DIEGO")
stopifnot(all(str_detect(sd_frame$precinct26_id, "^\\d{6}\\.0$")))

cons = enightdata::read_local("CA/clean/CA_san diego_latest.parquet", parquet = TRUE) |>
  as_tibble() |>
  filter(contest == "house") |>
  distinct(geo, cd_enr = district) |>
  mutate(precinct_enr = str_remove(geo, "^ca_\\d+_")) |>
  separate_wider_regex(precinct_enr, c(cons_no = "\\d{4}", "-", namesake = "\\d{6}", "-", place = ".*"), cols_remove = FALSE) |>
  mutate(across(c(cons_no, namesake), as.integer)) |>
  arrange(cons_no)
stopifnot(
  !anyDuplicated(cons$precinct_enr), !anyDuplicated(cons$namesake), # one house district per consolidation
  !is.unsorted(cons$namesake)                                        # premise of the range rule
)

l2_place = read_csv(election_path("input_data/crosswalks/ca.csv"), col_types = cols(.default = "c")) |>
  filter(county == "SAN DIEGO", !is.na(precinct_l2)) |>
  distinct(precinct_yougov, precinct_l2) |>
  mutate(l2_place = precinct_l2 |> str_remove(" \\d+$") |> str_remove("^.*-")) |> # "SAN DIEGO-SAN PASQUAL 100000" -> "SAN PASQUAL"
  distinct(precinct_yougov, l2_place)

members = sd_frame |>
  mutate(base = as.integer(str_remove(precinct26_id, "\\.0$"))) |>
  mutate(namesake = cons$namesake[findInterval(base, cons$namesake)]) |>
  left_join(select(cons, namesake, precinct_enr, cd_enr, place), join_by(namesake)) |>
  left_join(l2_place, join_by(precinct26_id == precinct_yougov)) |>
  mutate(
    cd_match = cd == paste0("CA-", cd_enr),
    # dominant frame cd; on a tie (6 bases of 1-2 voters) any tied cd passes
    base_cd_ok = any(cd_match & n == max(n)), .by = precinct26_id
  ) |>
  mutate(
    place_match = l2_place == place,
    member_rule = "namesake_range"
  )
stopifnot(!anyNA(members$precinct_enr), nrow(members) == nrow(sd_frame))

consolidated = members |>
  summarise(
    n_base = n_distinct(precinct26_id),
    cd_ok = all(base_cd_ok),
    cd_frame_share = sum(n[cd_match]) / sum(n),
    place_share = sum(n[place_match %in% TRUE]) / sum(n[!is.na(place_match)]),
    across(all_of(frame_vars), \(x) weighted.mean(x, n, na.rm = TRUE)),
    n = sum(n),
    .by = c(state, county_name, precinct_enr, cd_enr)
  ) |>
  mutate(cd = paste0("CA-", cd_enr), use = cd_ok) |>
  select(-cd_enr) |>
  relocate(state, county_name, cd, precinct_enr, n, n_base, use, cd_ok, cd_frame_share, place_share) |>
  arrange(precinct_enr)

# Consolidations with no frame base precinct (namesake and range hold no frame
# voters): present in ENR, absent here.
cat("consolidations in ENR:", nrow(cons), " with frame members:", nrow(consolidated),
    " use = TRUE:", sum(consolidated$use), "\n")
cat("frame voters:", sum(sd_frame$n), " in use = TRUE consolidations:", sum(consolidated$n[consolidated$use]),
    sprintf("(%.1f%%)", 100 * sum(consolidated$n[consolidated$use]) / sum(sd_frame$n)), "\n")
cat("voter-weighted place agreement (bases with L2):",
    with(filter(members, !is.na(place_match)), weighted.mean(place_match, n)), "\n")
consolidated |> filter(!use) |> select(precinct_enr, cd, n, n_base, cd_frame_share, place_share) |> print(n = 20)

stopifnot(
  sum(consolidated$n) == sum(sd_frame$n),
  !anyDuplicated(consolidated$precinct_enr)
)

enightdata::write_local(
  select(members, state, county_name, cd, precinct_yougov = precinct26_id, n, precinct_enr, member_rule, base_cd_ok, l2_place, place),
  "input_data/crosswalks/ca_sandiego_members.csv", parquet = FALSE
)
enightdata::write_local(consolidated, "input_data/crosswalks/ca_sandiego_frame_consolidated.csv", parquet = FALSE)
