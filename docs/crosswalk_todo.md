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

Status as of 2026-10-19: new `clean/` data landed for MI (Berrien, Branch,
Clare, Isabella, Newaygo, Saginaw, Van Buren) and TX (Harris, Lamar), and both
states were rebuilt. All built states were also rerun on the parquet frame and
the new L2 vintages (see Results). AZ (Pima), VA and WA (Cowlitz, Pacific,
Skamania, Wahkiakum) are in the pipeline but have no precinct rows yet: Pima and
VA have not posted contests, and the WA statewide feed fills `precinctResults`
only once results post. WI Wood has data but is not a frame state.

Status as of 2026-10-15 (second check): NC is done. New `clean/` data has
landed for AK (statewide), CA (San Diego), FL (Hillsborough, Miami-Dade), MI
(Kent, Osceola, Roscommon in the frame; St. Clair and St. Joseph are not frame
counties) and TX (Kaufman, Wood). NY also has Columbia but is not in the frame.

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
| NC | 100 (all) | 2,657 (31 blank) | nc_20261006 | `01`, `PR24`, `01-01` | done 2026-10-15 |
| AK | 30 (all) | 402 | ak_20261008 | `748`, `002` (frame_name `DD-PPP`) | done 2026-10-15 |
| AZ | 9 | 1,545 | az_20260901 | `0009`, `0133` | partial 2026-10-23 (ENR: Cochise, Maricopa; L2 all 9 counties) |
| CA | 17 | 23,685 (26,700 rows) | ca_20260901 | `47252`, `1204` | partial 2026-10-23 (L2 all counties; ENR San Francisco, Sonoma singletons; San Diego and Solano consolidated builds; Sonoma `07` consolidations NA) |
| FL | 31 | 3,764 (4,342 rows) | fl_20261011 | `8735`, `127` | partial 2026-10-23 (ENR: 21 frame counties; see Results) |
| GA | 159 (all) | 2,581 (2,618 rows) | ga_20261006 | `CP04A`, `029` | done 2026-10-25 |
| IA | 99 (all) | 1,658 | ia_20260901 | `DOWS`, `FC2` (name-like) | waiting |
| MI | 31 | 2,737 rows (keyed on `precinct26_code`) | mi_20260929 | `MI_KENT_ADA TOWNSHIP_001` | partial 2026-10-23 (ENR: Berrien, Branch, Clare, Isabella, Kent, Newaygo, Osceola, Roscommon, Saginaw, Van Buren, Wayne) |
| NE | 93 (all) | 1,321 | ne_20261002 | `03`, `16` | done 2026-10-23 |
| NM | 33 (all) | 2,203 | nm_20260914 | `034`, `012` | waiting |
| OH | 13 | 4,153 (4,524 rows) | oh_20260929 | `83AHA`, `31BLU` | partial 2026-10-23 (ENR: Franklin, Stark; L2 all counties) |
| PA | 19 | 6,217 | pa_20260911 | `2403`, `1680100`, `011905S` | partial 2026-10-23 (ENR: Chester, Delaware, Philadelphia; L2 all 19 counties) |
| TX | 32 | 6,153 (45 blank; 11,563 rows) | tx_20261011 | `2305`, `4608` | partial 2026-10-23 (ENR: Bexar, Harris, Kaufman, Lamar, Wood) |
| VA | 133 (all) | 2,534 (2,541 rows) | va_20261006 | `0206`, `0412` | done 2026-10-25 |
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

Run of 2026-10-25 (ENR cycle): new state scripts `va.R` and `ga.R`.

- GA (2026-10-25, ENR `GA_latest` as of 2026-10-23, statewide SOS Enhanced
  Voting export, votes still 0; L2 ga_20261006): 2,618 frame rows (2,581
  precincts). 99.1% have ENR, 99.1% have L2, 98.3% have both.
  - `scrape_ga()` now keys geo on the EV precinct `id` (the official code)
    instead of the `name` label ("Allgood Elem" for DeKalb AA, "001 Harbins A"
    for Gwinnett 001), enightdata change uncommitted. With ids, 2,362 frame
    precincts join directly; ENR tiers add `upper` 19 (Fulton `04i`), `std` 121
    (zero-padding, Troup `01-Broad St`), `num` 9 (Clayton `MO1`), `lead` 47
    (Henry `25` = `25 LOCUST GROVE`, White `011`).
  - `ga.R` reads the EV labels from the raw JSON of the same scrape
    (`GA/raw/GA_{stamp}.json`), as `ne.R` does, to bridge L2 names to codes
    (Cobb `ACWORTH 1A`, Clayton `LOVE JOY 1`). L2 tiers: `code_exact` 1,338,
    `paren_code` 552, `std` 254, `enr_name` 205, `paren_lead` 48, `enr_name1` 46,
    `lead_sfx` 34, `lead_code` 24, `num_trim` 20, `head_code` 17, `enr_nospace`
    10, `lead_pnum` 8, `lead_lead` 4. Median ratios 0.99-1.02 per tier.
  - NA by design: Burke ENR (renumbered for 2026: ENR has 1, 16, 17 and lacks
    9, 14, while frame and L2 agree on 02-15). L2 merges that ENR still splits:
    Dougherty 02 (L2 5,074 = frame 01 + 02) and Cobb WG02 (WADE GREEN 2 4,457 =
    WG01 + WG02), so L2 is NA there (and for the absorbed 01/WG01).
  - Left NA, unresolved: Morgan (ENR `46026`, `02`, `03`, `05` vs frame
    BUCKHEAD, CENTENNIAL, GIBBS, REC), Dodge ENR `PLAIN`/`PONDT` vs frame
    PLAINFIELD/RHINE, Fayette frame 20A vs ENR 20, Newton 23FV/25HB, Forsyth
    21, Gordon 849-B, Lanier 300 (1 voter); L2 misses Jackson 1000-4000 (L2
    `1`-`4`), Cobb Mount Bethel/CA01/SF01 and a few singletons.
  - ENR precincts not in the frame: Fulton 08F2, SC08A, UC01C, UC033; McIntosh
    "South Darien" has an empty EV id (geo ends in `_`).


- VA (2026-10-25, ENR `VA_latest` as of 2026-10-23, statewide Enhanced Voting
  export, votes still 0; L2 va_20261006): 2,541 frame rows (2,534 precincts; 7
  split across two CDs). 99.3% have ENR, 99.3% have L2, 99.3% have both.
  - County names: ENR (via `cbs_counties`) drops " CITY" from independent cities
    unless a county of the same name exists, and spells out KING AND QUEEN; L2
    adds " COUNTY". `va.R` maps both to the frame names.
  - ENR ids are `102-RICH CREEK`; the leading number zero-padded to 4 is the
    frame code. A code match is kept only if the names also agree (ignoring
    spacing and punctuation), plus 3 checked renames: Campbell 402 COURTHOUSE,
    Emporia 601/701 (frame DISTRICT 6/7, ENR and L2 PRECINCT 6-1/7-1, ratios
    0.96-0.99).
  - L2 tiers: `name_exact` 2,424, `name_std` 81, `code_lead` 18 (leading number;
    renames such as Bristol FIRST PRECINCT -> FIRST WARD, Spotsylvania 102
    BERKELEY -> BLAYDES CORNER, all ratios 0.95-1.05), `name_key` 4.
  - NA by design (merges and splits since the frame vintage, ENR and L2 both
    NA): Buchanan 0103 (+0102 BLACKEY) and 0403 (+0405 GRUNDY); Highland 0001
    (+0003); Rockbridge 0101 (+0102), 0302 (+0303), 0401 (+0403); Russell 0101
    and 0103 (+ parts of 0102); Amherst 0201 (split into new 203 TOWN OF
    AMHERST) and Norfolk 0405 (split into new 416 NORFOLK STATE). The absorbed
    frame precincts have no ENR or L2 row.
  - Check on real results: Halifax 0603 MT CARMEL is in the frame and L2 (1,629
    vs 1,659) but missing from the ENR export. Charlottesville 0202 TRAILBLAZER
    is an exact match with ratio 1.32 (4,355 vs 3,305) and no unmatched
    neighbour, probably a boundary shift; kept.

Rerun of 2026-10-23 (ENR cycle: new clean files from `cycle.R verify`; L2 and
frame unchanged). New state scripts `az.R`, `ne.R`, `oh.R`, `pa.R`; entries below.

- TX: ENR 4,336 -> 5,514 rows (47.7%). New county Bexar: 744 of 842 frame
  precincts (1,178 of 1,296 rows). All 98 misses also lack L2 (junk codes).
  62 Bexar ENR precincts are not in the frame. The glob also reads non-frame
  Hardin (harmless). Harris, Kaufman, Lamar and Wood are unchanged.
- FL: ENR 2,141 -> 2,249 rows (51.8%). New counties Collier 78/78, DeSoto 16/16,
  Hendry 14/14. New ENR recodes: Collier `PCT101` -> `101`; Hendry `1` -> `001`;
  DeSoto keeps the leading code (`05A The House of God Church` -> `05`). DeSoto
  ENR 05A/11A/15A pair 1:1 with frame and L2 05/11/15 and have no B parts.
  Manatee (frame) found but its Precincts/Reports pages are not up yet.
- MI: ENR 480 -> 1,198 rows (43.8%). New county Wayne 718 of 720. `muni_std`
  maps `(Charter) Township of X` to `X TOWNSHIP`. `parse_enr` reads Inkster
  `District N Precinct M` as ward/precinct, and maps `The Village of Grosse
  Pointe Shores, a Michigan City` to the city. NA by design: Dearborn Heights 011
  (frame splits it across MI-12/MI-13). Detroit absentee counting boards
  (`CB 1`-`CB 48`) and Detroit Precinct 59 have no frame row.
- CA: ENR 0 -> 600 rows (2.2%). New county San Francisco 543 of 545:
  `pct 1101`/`pct 1103 mb` (mail-ballot precinct) -> `1101`. ENR 9901-9904 have
  no frame row; frame 1152 and 9744 are not in ENR. Sonoma 57 of 1,393 rows
  (50 base precincts) are unchanged, but are now verified as single precincts.
  - Sonoma ENR has 331 `07xxxxx` consolidations (308k of 319k voters) numbered
    separately from the base codes, plus 72 base-coded precincts. The 50 that
    are frame bases have Clarity `totalVoters` equal to frame n (11,043 vs
    11,040), so they join 1:1. The `07` consolidations stay NA: there is no
    namesake to infer membership from, and no list was found (sonomacounty.gov,
    2026-10-23). They need the ROV's consolidation list.
  - Solano (196 ENR consolidations vs 608 frame bases) stays NA in `ca.csv` and
    gets a San Diego-style build, `ca_solano_consolidated.R` (see
    `docs/precinct_crosswalks.md`): 176 of 196 consolidations `use = TRUE`,
    94.4% of 276,037 frame voters.
  - `ca_sandiego_consolidated.R` was rerun and is unchanged (1,893
    consolidations, 88.9% of SD frame voters).

- AZ (2026-10-23, partial; ENR `AZ_Cochise_latest` 2026-10-21, `AZ_Maricopa_latest`
  2026-09-21; L2 az_20260901): 1,545 frame rows. 64.6% have ENR (998), 100% have
  L2, 64.6% have both. ENR: Cochise 54/54, Maricopa 944/944.
  - Every source is parsed to (4-digit number, standardized name), and a match
    needs both to agree. Frame names: `02 STONE BUTTE`, `40 - BELLEMONT`,
    `PRECINCT 221`, and name-only in Cochise/Maricopa (the number is the id).
    L2: `0216 NAME`, `BELLEMONT-40` (Coconino), bare numbers (Pima, Yuma). ENR:
    `0783 SPUR`, `06 BI WARREN`. All ENR names equal the frame names.
  - L2 tiers: `code_name` 1,235 (median ratio 1.01), `code_bare` 309 (Pima/Yuma,
    1.00), `manual` 1 (Pinal 088 IRONWOOD CROSSING = frame 088 IRONWOOD CROSSING
    WEST, 4,190/4,116). Pinal frame abbreviations (STV, CG, FLO, MAR, AJ) are
    expanded; a trailing number equal to the precinct number is dropped (Coconino
    `FLAGSTAFF-01` = `01 - FLAGSTAFF 1`).
  - ENR precincts not in the frame (presumably no voters): Cochise 55 MONTEZUMA;
    Maricopa 0564/0568/0569/0570/0576/0577 NVP, 0773-0777 SOUTH MTN PARK, 0860 UNO.
  - No merges, splits or ambiguous cases. Ratio outliers are exact matches on
    tiny precincts (Maricopa 0410, 0848; Pinal 0089, 0116) and Pinal 0137 AJ
    RADIANCE at 1.29 (growth); all kept.
  - Rerun when the other 7 frame counties get clean files, and check each new
    county's id format against `parse_pct()`.
- OH (2026-10-23, partial; ENR `OH_franklin_latest` 2026-10-23 from the county API,
  `OH_stark_latest` 2026-10-22; votes still 0): 4,524 frame rows (4,153 precincts).
  Overall 27.5% ENR, 99.96% L2, 27.5% both. In the ENR counties: Franklin 889/889
  precincts (935 rows), Stark 274/274 (308 rows), no ENR leftovers on either side.
  - Frame ids are SOS codes (`25ARA`); frame names carry the code (`COLS 74-C
    (25-ARA)`). L2 and ENR have names only, so all matching is name-based.
  - `oh_std()` (local): number words to digits (Fulton `DELTA TWO`), TOWNSHIP->TWP,
    VILLAGE/VILL->VLG, drop Cuyahoga's `00` ward, plus a per-county `abbr` map for
    frame abbreviations (Clark BETH/GERM/GREE/HARM/MFLD/MR/PLES, `CITY n` =
    Springfield City, `T-n` = Springfield Twp; Franklin COLS/DUB/GAH/HILL/REYNS/UA/
    WORTH/CANAL WIN/GRANDVIEW; Hamilton NO COLL HILL; Summit CUY FALLS/NFLD CTR).
    Precinct counts per municipality agree on both sides for every entry.
  - Tiers: `manual` 4, `name_exact` 1,906, `name_std` 2,447, `name_type` 165 (drop
    TWP/VLG/CITY when unique on both sides: Carroll `BROWN TWP A` = `BROWN A`,
    Montgomery `MIAMI A` = `MIAMI TWP A`). Median L2/frame ratio 1.09-1.14 per tier.
    No match is below 0.85.
  - ENR: Franklin ids are lowercase full names (`blendon township-a`); Stark ids are
    short codes (`cc3a`, `laka`, `mlake`) decoded by `stark_pfx`. The script stops on
    any other ENR county until its id format is checked.
  - Manual L2 (only candidate on each side, ratio 1.07-1.18): Lake MENTOR-ON-THE-LAKE
    CITY DD = frame 4A; Lucas RICHFIELD = RICHFIELD 1; Montgomery P/NEW LEB A =
    PER/NEW LEBANON; Summit AKRON 07-1 = AKRON 7-I.
  - NA: Summit AKRON 1-I and AKRON 3-I (890 and 590 frame voters), which have no L2
    precinct.
  - Check: 516 exact-name matches with ratio > 1.3, mostly Cincinnati, Cleveland,
    Akron, Dayton and Columbus, probably a voter-file vintage difference. The largest
    are Summit AKRON 1-L (4.5) and 3-L (2.5), Hamilton CINCINNATI 12-B and 13-H (3.5),
    and Cuyahoga CLEVELAND 08-A (3.3).

- NE (2026-10-23, ENR `NE_latest` as of 2026-10-06, statewide SOS GetMapData
  feed, votes still 0): 1,321 frame rows, all coded. 100% have ENR, 99.8% have
  L2, 99.8% have both.
  - The ENR id is `StatePrecinctID`, a per-county sequence (0001, ...) unrelated
    to the frame codes. Names come from the raw CSV of the same scrape
    (`NE/raw/NE_{stamp}.csv`, keyed on county x `StatePrecinctID`); the script
    stops if the matching timestamped clean parquet differs from `NE_latest`.
    All 1,321 match frame names exactly, with no recodes. Douglas `0187` (07-33)
    is in ENR but in neither the frame nor L2, probably a new precinct.
    **Decision:** keep reading `raw/`, or have `scrape_ne()` carry `PrecinctName`
    into `clean/`.
  - L2 tiers: `name_exact` 477, `county_rule` 408 (Douglas `WARD 01 PCT 01` ->
    `01-01`, Lancaster `01 A-01` -> `01A01`), `name_ne` 119 (ordinals and roman
    numerals to digits, drop PRECINCT/TWP/AREA/CITY OF/VILLAGE OF), `name_std`
    106, `code_exact` 101, `county_id` 38 (Buffalo and Hall leading number, Cass
    trailing code), `code_std` 20, `manual` 15, `name_ne_strip` 13,
    `name_ne_bag` 11, `name_ne_stripbag` 6, `name_ne_nospace` 4. Median ratios
    are 1.01-1.05 per tier. The generic `name_nospace` tier is dropped because it
    matched Hamilton `11` to `1 001`.
  - NA by design for L2 (the frame precinct is two L2 precincts): Cedar `06` PCT
    06 8-9 (1,337 = 1,208 + 144), Cuming `06` BGW (1,003 = 924 + 14), Dixon `01`
    LOGAN-WAKEFIELD 3 (257 = 256 + 2).
  - Check: Lancaster `03D00` <-> L2 `03 D`, ratio 1.52 (481 vs 317). It is the
    only candidate on both sides, so it is kept as a code match.
  - Manual one-offs: Cuming BCN/BL/SSGC (abbreviations of the L2 township lists),
    Richardson `FC WARD n` = `FALLS CITY n`, Lincoln `07.NORTH PLATTE PRECINCT 7`
    = `NORTH PLATTE 07B`, Holt ATKINSON WARD I and the SURRONUNDING typo, Garden
    `OSHKOSH 1`, Fillmore GENEVA RURAL, Knox LSC, Seward M-BX, Butler 06. Ratios
    are 0.99-1.04.

- PA (2026-10-23, partial; ENR `PA_chester_latest` 2026-10-15 (LiveVoterTurnout),
  `PA_delaware_latest` 2026-10-22 (Clarity), `PA_philadelphia_latest` 2026-10-17
  (city feed); clean files written 2026-10-23, Chester and Philadelphia carry
  nonzero test votes): 6,223 frame rows (6,217 precincts). Overall 37.2% ENR,
  99.97% L2, 37.2% both. In the three ENR counties (2,316 rows): 100% ENR, 100%
  L2, 100% both.
  - ENR id formats differ by county: Chester `005 atglen` (leading code; name
    checked loosely against the frame name), Philadelphia `01-01` -> `0101`,
    Delaware names only (`UPPER DARBY TWP DISTRICT 07 PRECINCT 06`), matched to
    frame names (`UPPER DARBY TOWNSHIP 7TH DIST 6TH PREC`) on a canonical
    form. Philadelphia's three "Nth congressional" aggregate rows are dropped.
    New county files get NA ENR until a rule is added in `pa.R`.
  - L2 names follow one convention (`ASTON TWP 1`, `BETHEL PARK 7-2`); frame
    names vary by county. A PA normalizer (`pa_norm`: abbreviations, ordinals,
    ward/district words, Allegheny `00` tokens, Lancaster `X EAST TWP`) gives
    name_exact 1,106, name_norm 4,978, name_notype 82 (Fayette/York names
    without TWP/BORO/CITY), single_pct 23 (Allegheny `ALEPPO 00 01` vs
    `ALEPPO`), manual 26 (Cambria Johnstown/Nanty Glo, Allegheny McDonald/
    Pennsbury Vlg/Glen Osborne, Fayette PERRYOPOLIS = L2 `PERRY BORO`, etc.).
    Median ratios 1.01-1.04; manual 0.96-1.07.
  - NA by design: Lehigh 011905S (3 voters, no L2).
  - Ambiguous, left NA: Lancaster 4906 HEMPFIELD EAST TWP SPRING (1,335) vs L2
    EAST HEMPFIELD TWP-GOLDEN ACRES (1,356): the only leftovers in the township
    with consistent counts, and the code's alphabetical position fits Golden
    Acres, but the names share nothing.
  - Ratio outliers kept (same-name matches): Radnor 2-2 and 7-3 (3.5x, 5.2x),
    Lower Oxford E (1.7x), plus wards in Pittsburgh, Philadelphia and
    Lancaster where L2 carries more voters; York City ward matches also run
    1.09-1.19.

Rerun of 2026-10-19 (frame now `input_data/yougov_precinct_frame.parquet`, rebuilt
2026-10-18; `nc.R` and `ak.R` now pick the latest L2 file, like the others).
Changes against the 2026-10-15 files:

- NC: identical. CA and the San Diego files: same coverage; L2 gains Imperial
  `285001 MAIL` and loses San Diego `ENCINITAS 414270` (L2 vintage unchanged,
  frame rebuild).
- AK (L2 ak_20261008): L2 398 -> 400 rows. 29-525 and 02-740 now clear the 90%
  `paren_code_split` threshold.
- FL (L2 fl_20261011): L2 4,338 -> 4,334. Broward A016, R044, R047, T024 are no
  longer in L2.
- MI: ENR 232 -> 480 rows, L2 2,708 -> 2,722. New counties: Berrien 53 of 55
  precincts, Branch 21/21, Clare 19/19, Isabella 24/24, Newaygo 29/29, Saginaw
  71/71, Van Buren 31/31. Two rule changes in `mi.R`:
  - `muni_std` maps `MT` to `MOUNT` (ENR "City of Mt. Pleasant").
  - New last tier `ward_as_pct`: cities with one precinct per ward, where the
    frame has `WD 02 PCT 001` but L2/ENR number precincts by ward (Coldwater,
    Benton Harbor, South Haven, Hudsonville). 14 L2 matches, ratios 0.91-0.98.
  - NA by design: Berrien Lincoln Charter Twp 003 and Royalton 001 (frame
    splits them across MI-04/MI-05); Van Buren ENR "City of South Haven, Ward
    3, Precinct 2" (the Allegan County piece; 10 frame voters under ALLEGAN).
- TX (L2 tx_20261011): ENR 92 -> 4,336 rows. Harris 1,151 of 1,157 frame
  precincts; the 6 misses are 1-voter junk codes and the blank bucket. 73 Harris
  ENR precincts are in neither the frame nor L2 (presumably no registered
  voters). Lamar 25 of 28 (misses: `002F`, 231 voters, not in ENR; `0408`, 1
  voter; blank). L2: the voter-count guard now drops El Paso 0086 and keeps
  Travis 0439.

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
- FL (2026-10-20 rebuild; clean files written for the 19 `updated` FL counties
  from the 2026 general sites, votes still 0): ENR 1,275 -> 2,141 of 4,342
  frame rows (49.3%). L2 unchanged. All earlier matches kept.
  - 100%: Bradford, Citrus, Columbia, Dixie, Gadsden, Glades, Hernando, Hillsborough,
    Indian River, Lake, Levy, Marion, Monroe, Okeechobee, Sarasota, Walton. Polk
    197/198, Miami-Dade 752/771 (sub-precinct splits, as before).
  - New ENR recode: Citrus, Columbia, Dixie, Glades and Monroe append the
    polling place to the code (`406 - Redemption Point AG`, `15.0 Deep
    Creek`); keep the leading code, Dixie padded to 2.
  - Gadsden and Levy ENR ids are polling-place names; `place_codes` maps them using
    the county precinct lists (one polling place per precinct). Levy's L2 names are
    precinct areas, not polling places (06 GULF HAMMOCK votes at Otter Creek Baptist).
  - St. Johns has ENR but is not a frame county.
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

- **Recheck on 2026-11-02 and 2026-11-03: FL Gadsden and Levy polling places.**
  Their ENR precinct ids are polling-place names, mapped to precinct codes by
  `place_codes` in `scripts/util/crosswalks/fl.R`. If a county moved a polling
  place, that precinct silently drops to NA. Compare the ENR names with:
  - Gadsden: https://votegadsdenfl.gov/election-info/precinct-information/list-of-precincts/
  - Levy: https://www.voterfocus.com/PrecinctFinder/precinctDirectory?county=FL-LEV
  Then rerun `fl.R` and confirm both counties are still 100% ENR.
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
