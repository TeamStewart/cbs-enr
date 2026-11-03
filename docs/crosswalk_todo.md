# Crosswalk TODO: one script per frame state

Goal: for every state in `input_data/yougov_precinct_frame.parquet`, write
`scripts/util/crosswalks/{st}.R`, which produces `input_data/crosswalks/{st}.csv`.
Follow `docs/precinct_crosswalks.md` (rules and tiers) and use
`scripts/util/crosswalks/nc.R` as the reference implementation.

The crosswalk queue (frame counties whose ENR precincts changed since the
state's last build) comes from `cycle.R status`; see `docs/enr_cycle.md`.
After rebuilding a state, run
`Rscript scripts/util/enr_cycle/cycle.R xwalk-done {ST}`.

**Only build a state once it has 26_general ENR data.** The ENR source is
`26_general/{ST}/clean/` and nothing else: do not use `26_primaries/`,
`26_dashtest/` or older folders. States without that data stay in `waiting`.
This list is meant to be rerun over the coming weeks as ENR data arrives.

## Rerun procedure (each time new ENR data has landed)

1. Check which states are ready. From the project root, run:
   ```r
   root = fs::path(enightdata:::the$path_dropbox, enightdata:::the$election_folder)
   frame_states = unique(enightdata::read_local("input_data/yougov_precinct_frame.parquet", parquet = TRUE)$state)
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
2. Set each newly ready state's status in the table to `ready`, then start one
   agent per state using the prompt template.
3. **Rebuild states that are already done** when their ENR data changes in a
   way that affects precinct ids, e.g. the first file for a county that was
   missing before, or a new results vintage. Rerun `Rscript
   scripts/util/crosswalks/{st}.R` and compare the coverage numbers with the
   state's table row and notes below. If ENR coverage drops, inspect the new ids and update that
   state's ENR recodes.

## Shared helpers

`scripts/util/crosswalks/helpers.R` holds the generic pieces (`std_name`,
`match_tiers`, review prints, `read_frame`, `frame_names`, `read_enr`,
`write_crosswalk`). Per-state scripts never edit it; a state that needs a new
generic helper defines it locally and lists it under "Follow-ups". MI keys on
`precinct26_code` (`read_frame(st, id = "precinct26_code")`); use that for any
state where `precinct26_id` repeats within county x cd.

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
   covered; and NA-by-design and ambiguous cases. Update the state's table row
   and its entry under "State notes".

| state | frame counties | frame rows | L2 file | ENR rows | L2 | status (last build) |
|---|---|---|---|---|---|---|
| NC | 100 (all) | 3,539 | nc_20261006 | 3,505 (99.0%) | 99.5% | done 10-27 |
| AK | 30 (all) | 402 | ak_20261008 | 402 (100%) | 99.0% | done 10-15 |
| AZ | 9 | 1,545 | az_20260901 | 1,545 (100%) | 100% | done 11-01 (statewide ENR, all 9 frame counties; was 1,172 / 5 counties) |
| CA | 17 | 29,961 | ca_20260901 | 4,288 (14.3%) | 88.9% (10-30 measure; L2 rows 23,726 -> 25,353 on 10-31 with San Bernardino) | partial 10-31 (ENR: Sacramento, San Bernardino, San Francisco, Sonoma singletons, Ventura; SD, Solano, Riverside, Nevada via consolidated builds) |
| FL | 65 | 6,252 | fl_20261011 | 3,376 (54.0%) | 99.9% | partial 10-31 (41 ENR counties; Pinellas, St Johns added; L2 St Johns/St Lucie fixed) |
| GA | 159 (all) | 2,616 | ga_20261006 | 2,600 (99.4%) | 99.4% | done 10-31 |
| IA | 99 (all) | 1,658 | ia_20261002 | 1,632 (98.4%) | 100% | done 10-31 (frame codes have no names: matched by voter profile + code/township evidence, see `ia.R`) |
| MI | 73 | 3,722 (keyed on `precinct26_code`) | mi_20260929 | 2,549 (68.5%) | 99.4% | partial 10-31 (28 ENR counties; Leelanau added; L2 St Clair/St Joseph fixed) |
| MN | 87 (all) | 4,080 | mn_20261009 | 4,080 (100%) | 99.9% | done 10-27 |
| NE | 93 (all) | 1,321 | ne_20261002 | 1,321 (100%) | 99.8% | done 10-23 |
| NM | 33 (all) | 2,204 | nm_20260914 | 2,203 (99.95%) | 99.95% | done 10-31 |
| OH | 15 | 5,052 | oh_20260929 | 4,626 (91.6%) | 99.96% | partial 10-30 (no ENR: Lake, Warren) |
| PA | 20 | 6,492 | pa_20260911 | 4,930 (75.9%) | 99.9% | partial 10-31 (ENR: Allegheny, Chester, Dauphin, Delaware, Erie, Lancaster, Montgomery, Philadelphia, Washington, York) |
| SC | 46 (all) | 2,332 | sc_20261014 | 2,331 (99.96%) | 100% | done 11-03 |
| TX | 72 | 10,851 | tx_20261011 | 6,712 (61.9%) | 94.8% | partial 11-01 (23 ENR counties; Collin added on its Enhanced Voting switch; Tarrant via consolidated sub-units) |
| VA | 133 (all) | 2,538 | va_20261006 | 2,526 (99.5%) | 99.5% | done 10-31 |
| WA | 4 | 167 | wa_20260917 | - | - | waiting |

Statuses: `waiting` means no 26_general ENR data yet. `ready` means data has
landed and an agent can be started. `partial` means the crosswalk is built for
some counties only; list them in Results. `done` means built.

## State notes

Current coverage is in the table above. Keep only what a rerun or election
night needs: NA-by-design cases, inferred calls, and checks. Rule details live
in each `{st}.R`; dated rerun history is in git (`git log -p docs/crosswalk_todo.md`).

- **AK**: ENR joins statewide on `precinct26_name` (`DD-PPP`; the ENR "county"
  is the house district). L2 `paren_code_xcounty` (5) and `paren_code_split`
  (15, >=90% of L2 voters in the frame borough). NA: 4 splits under 90%.
  **Decision (Mason):** keep the 90% threshold and `xcounty`, or NA them.
- **AZ**: ENR is one statewide Enhanced Voting file, covering all 9 frame
  counties (was 5). Pima and Yuma ENR print the number with no name
  ("PRECINCT 001") and match only on `k_code_bare`, as their L2 already did, so
  the ENR join runs both key tiers. All 9 counties are 100% ENR. Pinal 0040
  needed two local rules: `parse_pct()` rewrites JCT to JUNCTION (the frame
  abbreviates it there, its siblings 0056-0059 do not), and a third ENR tier
  pairs still-unmatched rows on the number and accepts a prefix match when the
  feed's label is at its 30-character cap ("040 APACHE JUNCTION SUPERSTITI").
- **CA**: San Diego, Solano, Riverside and Nevada ENR are consolidations, NA
  in `ca.csv`, with separate builds run after `ca.R`:
  `ca_sandiego_consolidated.R` (1,893 consolidations, 88.9% of SD frame
  voters), `ca_solano_consolidated.R` (176/196, 94.4%),
  `ca_riverside_consolidated.R` (951/970, 97.8%) and
  `ca_nevada_consolidated.R` (86/90, 92.5%; both spatial via SWDB 2026 MPREC x
  VoteHub CA26G). San Diego is the logged exception to the merges-stay-NA rule
  (see `docs/precinct_crosswalks.md`). **Check on real results**: ballots <=
  n per consolidation. Sonoma `07xxxxx` consolidations (331) stay NA until the
  ROV's membership list is found. Ventura L2 joins through the county's precinct map
  (`Ventura_PrecinctMap.csv`, NUMBER_ -> ELECTID; see `ca.R`). L2 NA by design: Merced, Shasta,
  Marin split codes, Sacramento Vineyard merges, San Mateo 71.
- **FL**: `place_codes` in `fl.R` maps polling-place ENR ids to codes for
  Gadsden (votegadsdenfl.gov list of precincts), Levy, Franklin, Gulf, Jackson,
  Liberty and Union (voterfocus.com/PrecinctFinder/precinctDirectory?county=FL-LEV,
  -FRA, -GUL, -JAC, -LIB, -UNI). Liberty 2/4 (Annex, Lake Mystic) assigned by
  ENR order: confirm with the SOE. Union: only 3C, 4B, 5B are single-precinct
  places; the rest are sums (NA). Miami-Dade multi-part precincts NA (100, 116,
  281, 283, 285, 462, 489, 708, 710, 730, 745, 765). Washington ENR `0010 Ebro`
  has no frame row. See the Gadsden/Levy follow-up.
- **GA**: needs the uncommitted `scrape_ga()` change (geo keyed on EV id).
  ENR ids also match frame names through their EV labels (`label` tier:
  Quitman G = GEORGETOWN, Dodge PONDT = RHINE; Morgan 46026 by hand = REC).
  L2 `PCT 1` tiers (Bleckley, Glascock, Webster), Jackson `1000` = L2 `1`, and
  one-precinct counties (Candler) were added 10-31.
  `ga.R` reads EV labels from `GA/raw/`. NA by design: Burke (renumbered),
  L2 for Dougherty 01/02 and Cobb WG01/WG02. Unresolved NA: Fayette 20A, Newton 23FV/25HB, Forsyth 21, Gordon 849-B.
- **IA**: frame `precinct26_id` is a county code with no name ("GCP",
  "AS001", "032 DSM"), so `ia.R` assigns codes to L2 names within county
  (`lpSolve::lp.assign`) on voter profile (n, dem, rep, voted 24/22/20) plus
  code-abbreviation, digit and voter-file `town` evidence, then ENR names to
  frame rows on name similarity. L2 rules: `profile` 1,335, `code` 296,
  `joint` 23 (college precincts: Ames, Iowa City, Grinnell, Decorah, Cedar
  Falls, where frame vote history is low), `rest` 4 (Emmet 10, Poweshiek 06,
  Story 29/32). NA by design (ENR): Mahaska (polling-place acronyms), Muscatine
  except city wards (ENR splits cities from townships), Warren GR2/GR3/GR4/LC3/
  NW4 and Johnson CV.02 (merges), Plymouth AKRON/MERRILL (polling-place names).
  **Check on real results**: Clarity `totalVoters` per precinct vs frame `n`.
- **MI**: crosswalk keyed on `precinct26_code`. NA by design: Ottawa 22
  school-district splits, Dearborn Heights 011, Berrien Lincoln 003 and
  Royalton 001 (CD splits), Van Buren South Haven W3 P2 (Allegan piece),
  Macomb Grosse Pointe Shores 002, Oakland Independence 7, Kent Nelson 002.
  Check: Washtenaw Ann Arbor 06/38/43 L2 ratios 0.76-0.79.
- **MN**: 25 ENR precincts have no frame row or voters. **Check on real
  results**: ballots there should be about 0.
- **NE**: since 10-31 `scrape_ne()` puts the SOS `PrecinctName` into geo
  (StatePrecinctID was a per-county sequence), and `ne.R` matches it to frame
  names directly. Check Lancaster `03D00` (L2 ratio 1.52).
- **NM**: the SOS feed's StatePrecinctID is not the county precinct number
  (they differ in Chaves, Rio Arriba, Lea, San Juan and others); since 10-31
  `scrape_nm()` puts the number from `PrecinctName` into geo ("067"). NA: one
  Sandoval ENR precinct with no frame row, Torrance frame 085 (1 voter).
- **OH**: no ENR for Lake, Warren. Greene Sugarcreek frame rows numbered
  157/158 from L2 size match (review). L2 NA: Summit AKRON 1-I, 3-I. Check:
  516 exact-name L2 matches with ratio > 1.3 (big cities, likely vintage).
  `oh.R` stops on any new ENR county until its id format is checked.
- **PA**: new ENR counties get NA until a rule is added in `pa.R`. NA:
  Allegheny BRADFORDWOOD, MCDONALD 00 05, ROSSLYN FARM, SEWICKLEY HT,
  PENNSBURY VLG 00 02; Washington 8th ward; Lehigh 011905S. Ambiguous, NA:
  Lancaster 4906. Dauphin and Erie carry test votes before election day.
- **SC**: ENR (Clarity) has names only, matched to frame names; a `votehub`
  tier uses the county code inside the VoteHub unit_id (`45003-:-007`) for
  feed typos (`ORNAGEBURG WD 7`) and renames the frame label missed (Aiken 007
  Bath, 012 Clearwater; L2 agrees). L2 rules: Aiken `NAME code`, Spartanburg
  `(code)`, Richland `COLUMBIA nn` = WARD nn = 1nn, Dorchester trailing ` 1`,
  13 manual one-offs. NA by design: Kershaw ENR `Camden No. 2 & 3` (merge; frame
  108 keeps L2 only). Provisional/Failsafe ENR buckets never match. Check:
  L2 ratios Orangeburg 04 (1.64), Richland COLUMBIA 02/08 (1.33), Beaufort
  BURTON 1D (0.70). L2 vote mode: blank BallotType = eday (`BLANK_EDAY`);
  "Early but Method Unknown" stays unknown (no mail/early split).
- **TX**: sub-unit reporters (Wood `11 - Joint COQ`, Hood `101 - GCTY_GISD`,
  multi-split Dallas `1063-01`) are sums, NA. Tarrant reports every precinct as
  `1005` + `1005 - F99`/`L01`; since 10-31 `scrape_tx()` sums the units
  (enightdata `consolidate_units()`), so its geo is the precinct; Galveston single-unit ballot styles match. ~600 junk frame
  codes stay NA. L2 voter-count guard drops redrawn same-code pairs; ~15
  Dallas/Harris/Tarrant drops at 1.3-1.6 may be growth. Check: Galveston
  `439.1`. Collin moved to Enhanced Voting on 11-01 and went 0 -> 366/367
  (99.7%), the whole statewide gain; `scrape_tx()` strips its "PCT " prefix so
  the ids pad to frame codes, and its 26 "LB" limited-ballot units are NA by
  design (no frame row). Collin's one NA is a frame row with no precinct code.
  Reeves 9/11: `002B` and `0136` are absent from its feed, not a join failure.
  Possibly open: L2 rules for low-coverage counties found on 10-26
  (Van Zandt, Grayson, Ellis, Wood, Burnet); recheck per-county L2 rates.
- **VA**: NA by design: Buchanan, Highland, Rockbridge, Russell merges;
  Amherst 0201, Norfolk 0405 splits. **Check on real results**: Halifax 0603
  missing from ENR; Charlottesville 0202 ratio 1.32.

## Follow-ups

- **Truncated ENR labels.** `az.R` carries a local third ENR tier for feeds that
  cap a precinct label (Enhanced Voting cuts Arizona's at 30 characters): pair
  the leftovers on the precinct number, accept a prefix match only at the cap,
  and require it to be unique within the county. Worth promoting to
  `helpers.R` if a second state needs it.
- **Recheck on 2026-11-02 and 2026-11-03: FL Gadsden and Levy polling
  places.** If a county moved a polling place, that precinct silently drops to
  NA. Compare ENR names with the county lists above, rerun `fl.R`, and confirm
  both are still 100% ENR.
- Helper candidates (defined locally for now): `pad_code`/`std_code` (TX, CA,
  FL), `drop_ratio_outliers` (TX, CA), an ENR sub-unit detector (TX Wood, FL
  Miami-Dade), `frame_names(st, by_cd = TRUE)`, a county-spelling check in
  `read_enr()` (`MIAMIDADE`), tibble returns from `read_frame()`/`read_enr()`,
  and forcing character columns on CSV read-back (leading zeros).

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
