# Crosswalk TODO: one script per frame state

Goal: for every state in `input_data/yougov_precinct_frame.csv`, write
`scripts/util/crosswalks/{st}.R`, which produces `input_data/crosswalks/{st}.csv`.
Follow `docs/precinct_crosswalks.md` (rules and tiers) and use
`scripts/util/crosswalks/nc.R` as the reference implementation.

**Only build a state once it has 26_general ENR data.** The ENR source is
`26_general/{ST}/clean/` and nothing else: do not use `26_primaries/`,
`26_dashtest/` or older folders. States without that data stay in `waiting`.
This list is meant to be rerun over the coming weeks as ENR data arrives.

Status as of 2026-10-15 (second check): NC is done. New `clean/` data has
landed for AK (statewide), CA (San Diego), FL (Hillsborough, Miami-Dade), MI
(Kent, Osceola, Roscommon in the frame; St. Clair and St. Joseph are not frame
counties) and TX (Kaufman, Wood). NY also has Columbia but is not in the frame.

## Rerun procedure (each time new ENR data has landed)

1. Check which states are ready. From the project root, run:
   ```r
   root = fs::path(enightdata:::the$path_dropbox, enightdata:::the$election_folder)
   frame_states = unique(enightdata::read_local("input_data/yougov_precinct_frame.csv")$state)
   # Needs actual parquet files: state folders and empty clean/ dirs can exist before data lands
   n_files = sapply(frame_states, \(s) length(Sys.glob(fs::path(root, s, "clean", "*.parquet"))))
   ready = frame_states[n_files > 0]
   done = toupper(fs::path_ext_remove(fs::path_file(fs::dir_ls(fs::path(root, "input_data/crosswalks"), glob = "*.csv"))))
   setdiff(ready, done)   # states to build now
   ```
   A state counts as ready only if `clean/` has files covering the frame
   counties. Partial-county states have per-county files
   (`{ST}_{county}_*`), and an agent should check which frame counties have
   data. A partially ready state can be built for the counties that have data.
   Record which counties are covered, and rerun the state later when the rest
   arrive.
2. If step 0 below is not done yet, do it first.
3. Set each newly ready state's status in the table to `ready`, then start one
   agent per state using the prompt template.
4. **Rebuild states that are already done** when their ENR data changes in a
   way that affects precinct ids, e.g. the first file for a county that was
   missing before, or a new results vintage. Rerun `Rscript
   scripts/util/crosswalks/{st}.R` and compare the coverage numbers with the
   Results section. If ENR coverage drops, inspect the new ids and update that
   state's ENR recodes.

## Step 0: shared helpers (do once, before the first fan-out)

- [x] Move the generic pieces of `nc.R` into `scripts/util/crosswalks/helpers.R`:
      `std_name`, `strip_lead`, `bag`, the tier loop (as
      `match_tiers(l2_keys, fr_keys, tiers)`), the review prints (ratio summary
      by rule, ratio outliers, unmatched frame rows by county), a
      `frame_names(st)` loader for `input_data/vf_frame_current_202609/state={ST}`,
      and a `write_crosswalk(st, crosswalk)` that creates the directory and
      writes the file. Also `read_frame(st)` and `read_enr(files)` (distinct
      ENR precincts from one or more clean files). Done 2026-10-15; `nc.csv`
      unchanged byte-for-byte.
- [x] Refactor `nc.R` to `source("scripts/util/crosswalks/helpers.R")`, keeping
      only the NC-specific parts (ENR recodes, `county_codes`, `l2_manual`,
      tier list).
- [x] Check the refactor: rerun `nc.R` and confirm that `nc.csv` is identical
      to the current file (back it up first and diff it).
- After this, per-state agents only add `{st}.R` files and never edit
      `helpers.R`. If a state needs a new generic helper, define it locally in
      `{st}.R` and note it under "Follow-ups" below.

## Per-state steps (one agent per ready state)

1. **Inputs.**
   - Frame: filter the frame CSV to `{ST}`. Never modify that object.
   - Frame names: `frame_names("{ST}")`.
   - L2: `input_data/l2/{st}_*_precincts.parquet`.
   - ENR: `26_general/{ST}/clean/`, using `{ST}_latest.parquet` or the
     latest per-county files. `geo` has the form `st_countyfips_precinct`.
     Map county FIPS to names with `enightmodels::cbs_counties`, as in
     `nc.R`.
2. **Look before matching.** Print samples of frame ids and names, L2 names,
   and ENR ids. Note the id formats per county, since partial-county states
   often differ by county.
3. **ENR vs frame.** Join directly, list leftovers by county, recode only
   identical-name cases, and leave merges and splits NA.
4. **L2 vs frame.** Run the standard tiers, then add `county_codes` rules and
   `manual` rows for the leftovers, based on the voter-count ratio and an
   eyeball check of every non-exact match.
5. **Write and verify.** The script must
   `stopifnot(nrow(crosswalk) == nrow(frame))` and be strictly 1:1. Report
   the share of frame rows with ENR, with L2, and with both; the ENR counties
   covered; and NA-by-design and ambiguous cases. Update the state's row and
   Results entry below.

| state | frame counties | frame precincts | L2 file | sample frame ids | status |
|---|---|---|---|---|---|
| NC | 100 (all) | 2,657 (31 blank) | nc_20260901 | `01`, `PR24`, `01-01` | done 2026-10-15 |
| AK | 30 (all) | 402 | ak_20260901 | `748`, `002` (frame_name `DD-PPP`) | done 2026-10-15 |
| AZ | 9 | 1,545 | az_20260901 | `0009`, `0133` | waiting |
| CA | 17 | 23,685 (26,700 rows) | ca_20260901 | `47252`, `1204` | partial 2026-10-15 (L2 all counties; ENR San Diego only, 0 matched: consolidated precincts) |
| FL | 31 | 3,764 (4,342 rows) | fl_20260923 | `8735`, `127` | partial 2026-10-15 (ENR: Hillsborough, Miami-Dade) |
| GA | 159 (all) | 2,581 | ga_20261002 | `CP04A`, `029` | waiting |
| IA | 99 (all) | 1,658 | ia_20260901 | `DOWS`, `FC2` (name-like) | waiting |
| MI | 31 | 2,737 rows (keyed on `precinct26_code`) | mi_20260929 | `MI_KENT_ADA TOWNSHIP_001` | partial 2026-10-15 (ENR: Kent, Osceola, Roscommon) |
| NE | 93 (all) | 1,321 | ne_20261002 | `03`, `16` | waiting |
| NM | 33 (all) | 2,203 | nm_20260914 | `034`, `012` | waiting |
| OH | 13 | 4,153 | oh_20260929 | `83AHA`, `31BLU` | waiting |
| PA | 19 | 6,217 | pa_20260911 | `2403`, `1680100`, `011905S` | waiting |
| TX | 32 | 6,153 (45 blank; 11,563 rows) | tx_20260930 | `2305`, `4608` | partial 2026-10-15 (ENR: Kaufman, Wood) |
| VA | 133 (all) | 2,534 | va_20260901 | `0206`, `0412` | waiting |
| WA | 4 | 167 | wa_20260917 | `0001` | waiting |

Statuses: `waiting` means no 26_general ENR data yet. `ready` means data has
landed and an agent can be started. `partial` means the crosswalk is built for
some counties only; list them in Results. `done` means built.

## Agent prompt template

> Build the precinct crosswalk for **{ST}**. Read
> `docs/precinct_crosswalks.md`, `docs/crosswalk_todo.md` and
> `scripts/util/crosswalks/nc.R` first. Use ENR data only from
> `26_general/{ST}/clean/`. If it is missing, stop and report back. Write
> `scripts/util/crosswalks/{st}.R`, which sources `helpers.R` and writes
> `input_data/crosswalks/{st}.csv`. Do not edit `helpers.R`, `nc.R` or other
> states' scripts. Put accuracy over coverage: leave NA unless the match is
> 1:1 and verified. Run the script, then update only your state's row and
> Results entry in `docs/crosswalk_todo.md`.

## Results

- NC (2026-10-15, ENR `NC_latest` as of 2026-10-05): 3,498 coded frame rows.
  99.4% have ENR, 99.5% have L2, 99.4% have both. See
  `docs/precinct_crosswalks.md`.
- AK (2026-10-15, ENR `AK_latest` as of 2026-08-31, statewide): 402 frame rows,
  all coded. 100% have ENR, 99.0% have L2, 99.0% have both.
  - The ENR "county" FIPS is the house district (ED01-ED40), not the borough, so
    ENR joins statewide on frame `precinct26_name` (`DD-PPP`), with no recodes.
  - L2 names are `NAME (DD-PPP)`, matched on the code statewide: `paren_code`
    (378, median ratio 0.96); `paren_code_xcounty` (5, the L2 precinct is in a
    different borough from the frame: 32-375, 40-040, 02-730, 02-735, 02-760);
    `paren_code_split` (15, the precinct crosses a borough line in L2, and the
    piece in the frame's borough holds at least 90% of the L2 voters).
  - NA by design: 4 splits under 90% (37-728 77%, 29-525 89.1%, 02-740 89.6%,
    03-340 89.9%).
  - **Decision needed:** keep the 90% threshold and the `xcounty` rule (exact
    statewide id, only the county label differs), or NA them to follow the
    within-county rule strictly.
- CA (2026-10-15, partial; ENR `CA_san diego_latest` 2026-10-13): 26,700 frame
  rows. ENR 0%, L2 88.9%, both 0%. In San Diego (7,221 rows), L2 is 99.8%.
  - San Diego ENR is NA by design. Its `geo` holds 2,994 consolidated precincts
    (e.g. `0002-100090-SAN PASQUAL`) covering 6,427 base precincts, and no 2026
    consolidation-to-base list was found. Rerun when the SD ROV list, or real
    ballot totals, are available.
  - L2 tiers: `name_exact` 1,308; `code_num` (trailing numeric code, 12
    counties) 20,518; `county_rule` 1,911 (Contra Costa place->prefix map; San
    Mateo place->prefix map inferred from voter counts, least certain). A ratio
    filter (<2/3 or >1.5 with max n >= 100) drops 6 matches.
  - L2 NA by design: Ventura (1,150), Merced (597) and Shasta (341), whose L2
    names can't be linked to frame codes; Marin's 594 frame split codes
    (`10906-B`); Sacramento's 19 Vineyard merges; San Mateo's 71 (mostly the
    Menlo Park 34/44 split).
  - Too large to eyeball every match. The agent sampled ~30 non-exact matches
    and read all 410 ratio outliers.
  - **San Diego exception (logged):** `ca_sandiego_consolidated.R` aggregates
    the frame to ENR consolidations, with inferred membership and n-weighted
    means. Outputs are `ca_sandiego_frame_consolidated.csv` and
    `ca_sandiego_members.csv`. 1,893 consolidations (88.9% of SD frame voters)
    pass the CD check. Rerun it after `ca.R`, and recheck it on real ballot
    totals. See `docs/precinct_crosswalks.md`.
- FL (2026-10-15, partial; ENR Hillsborough 2026-10-08, Miami-Dade 2026-09-28):
  4,342 frame rows. Overall 29.4% ENR, 99.9% L2, 29.4% both. In the two ENR
  counties (1,294 rows): 98.5% ENR, 100% L2, 98.5% both.
  - `cbs_counties` spells `MIAMIDADE`; the script recodes it to `MIAMI-DADE`.
  - Miami-Dade ENR ids are `{precinct}.{part}`. Precincts with more than one
    part are NA by design: 100, 116, 281, 283, 285, 462, 489, 708, 710, 730,
    745, 765.
  - Frame names equal the codes, so L2 matching is code-based (`code_exact`,
    `paren_code` Citrus, `lead_code` Baker/Levy, `code_std` zero-padding).
  - Only Miami-Dade gets an ENR recode. Check each new county's id format on
    rerun.
- MI (2026-10-15, partial; ENR Kent, Osceola, Roscommon). The frame was rebuilt
  on `precinct26_code` the same day; see Follow-ups. In `mi.csv`,
  `precinct_yougov` is `precinct26_code` (e.g. `MI_KENT_ADA TOWNSHIP_001`), not
  `precinct26_id`.
  - 2,737 frame rows: 8.5% have ENR (232), 98.9% have L2 (2,708; 99.2% of
    voters), 8.5% have both.
  - In the ENR counties: Kent 201 of 202 rows; Osceola 18 of 18; Roscommon 13
    of 13. The miss is Kent NELSON TOWNSHIP 002 (1,877 voters); its ENR name
    doesn't parse to the frame name.
  - Names are parsed to (municipality, ward, precinct) keys on all three
    sources. L2 tiers: `muni_pct` 2,322, `muni_single` 364, `ward_pct` 21,
    `muni_single_ns` 1. Median ratios are 0.94-0.96, and q01 is at least 0.89.
  - 29 rows have no L2, led by Berrien (8), Ottawa (7) and Branch (4).
  - Check: Washtenaw ANN ARBOR CITY 06, 38 and 43 (ratios 0.76-0.79). These
    are exact name matches, probably boundary changes.
  - The Roscommon ENR file includes precincts from other counties coded with
    Roscommon's FIPS.
- TX (2026-10-15, partial; ENR Kaufman 2026-10-13, Wood 2026-10-05; votes still
  0): 11,563 frame rows (11,518 coded). Overall 0.8% ENR, 93.5% L2, 0.8% both.
  Real precincts: Kaufman 37/37 ENR and L2; Wood 14/14 L2, 6/14 ENR.
  - NA by design: Wood 11, 21, 31, 32, 33, 41, 42, 44, which report sub-units
    (`11 - Joint COQ`, `33 - Hawk`, ...), so each is a sum of ENR rows.
  - The frame has ~600 junk codes with 1-20 voters; they stay NA.
  - An L2 voter-count guard (ratio outside 0.75-1.33 and difference > 25) drops
    92 same-code pairs from redrawn precincts (El Paso 0054, Bell, Victoria,
    Denton 1004, 47 Travis codes). Lamar is exempt (27% of its L2 voters have no
    precinct). About 15 Dallas/Harris/Tarrant drops at ratios 1.3-1.6 may just
    be growth and could be restored by hand.
  - Check: Galveston frame 0439 is matched to L2 439, but frame `439.1` (23
    voters) may be part of it.

## Follow-ups

- (agents: list any generic helper worth promoting into `helpers.R`)
- Done 2026-10-15: `input_data/create_frame.R` now groups by
  `precinct26_code` and keeps `precinct26_id`. Outside MI the frame rows and
  values are unchanged. The exception is `age`: Arrow's `median()` is
  approximate and moves by up to 2 years between runs, so making `age`
  reproducible would need an exact median. MI went from 1,026 to 2,737 rows.
  The other crosswalks were rerun and have the same contents (only the row
  order changed). Call `read_frame(st, id = "precinct26_code")` for any future
  state where `precinct26_id` repeats within county x cd.
- Helper candidates from the 2026-10-15 batch, defined locally for now:
  - `pad_code(x, width)` / `norm_num(x)` / `std_code(x)`: code-only padding
    and `.0` normalization (TX, CA, FL).
  - `drop_ratio_outliers(matched, lo, hi, min_n, exempt)`: drop same-code pairs
    whose voter ratio signals re-precincting (TX, CA).
  - ENR sub-unit detector: NA a base id and all its parts when ENR reports
    `X - suffix` (TX Wood) or `X.1` (FL Miami-Dade) parts.
  - Run ENR names through `match_tiers()` for states whose ENR has names (MI).
  - `frame_names(st, by_cd = TRUE)` and a guard against `precinct_yougov`
    repeating within county (MI).
  - Fix county spellings in `read_enr()` (`MIAMIDADE`), or fail loudly when an
    ENR county isn't in the frame.
  - `read_frame()`/`read_enr()` return data.tables; return tibbles instead.
  - Statewide joins on an official full id when the ENR "county" isn't a county
    (AK house districts).
  - CSV read-back drops leading zeros (`042` -> `42`); readers should force
    character columns.
