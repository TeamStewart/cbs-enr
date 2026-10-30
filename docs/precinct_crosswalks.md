# Precinct crosswalks (YouGov frame <-> ENR <-> L2)

Goal: one file per state, `input_data/crosswalks/{st}.csv`, keyed on the YouGov
frame (one row per `state x county x cd x precinct_yougov`), with the *raw*
ENR and L2 precinct identifiers alongside so other code can merge the three
sources without repeating any cleaning. Accuracy over coverage: a wrong match is
worse than an NA.

Reference implementation: `scripts/util/crosswalks/nc.R` (NC); shared code in `scripts/util/crosswalks/helpers.R`.

## Output columns

| column | meaning |
|---|---|
| `state`, `county`, `cd`, `precinct_yougov` | frame keys, unmodified |
| `precinct_enr` | raw ENR precinct id (as in `geo`), NA if no 1:1 match |
| `precinct_l2` | raw L2 `precinct` string, NA if no 1:1 match |
| `l2_rule` | which matching tier produced the L2 match (for auditing) |
| `precinct_votehub` | VoteHub shapefile `unit_id`, joined through `precinct_enr` from `crosswalks/votehub/{st}.csv` by `write_crosswalk()`; NA if no ENR id, no VoteHub layer for the state, or no match (see `docs/enr_cycle.md`, "VoteHub crosswalks") |

## Rules

1. **Never modify the frame object.** All standardization happens on the ENR and
   L2 side, and the raw id is kept in `precinct_enr` / `precinct_l2` before any
   changes.
2. **Strict 1:1 within county.** A key is usable only if it is unique within
   county on both sides, and a frame code already claimed is never reused.
3. **Merges and splits stay NA.** If the frame has precincts A and B and another
   source has the merged A+B (or the reverse), do not map. The frame source
   flags many of these with `"- OLD"` in `precinct26_name`.
4. **Join with `na_matches = "never"`.** Every source has an NA/blank precinct
   bucket (unassigned voters), and those must not join to each other.
5. **Every rule lives in the script.** No hand edits to the output. One-offs go in
   a `manual` tribble with a comment giving the frame name.

## Useful sources

- **Frame names**: `input_data/vf_frame_current_202609/state={ST}` has
  `precinct26_id`, `precinct26_name` and one row per voter. The frame was built
  from it, so its names have the same vintage as the frame codes. Count rows
  per precinct to get `n_frame`. This parquet is large; open it with
  `arrow::open_dataset()` and aggregate before `collect()`.
- **L2 precinct file**: `input_data/l2/{st}_YYYYMMDD_precincts.parquet` has the
  precinct name plus `n` (registered voters), which is useful for validation.
- **State voter file** (when the frame names are insufficient): NC publishes
  `https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvoter_Statewide.zip`
  (`precinct_abbrv`, `precinct_desc`). This gives the official current
  code/name pairs. In NC it showed some ENR codes are newer than frame codes.

## Workflow

1. **ENR vs frame.** These are usually both codes, so join directly and list the
   leftovers by county on both sides. Most leftovers are merges (rule 3). Recode
   only when both sides have identical official names (see the Chatham entries
   in the NC script).
2. **L2 vs frame.** L2 usually has names, not codes. Match L2 names to the frame
   names/codes in ordered tiers, taking the first unique hit:
   - `manual`: hand-verified one-offs
   - `name_exact`: L2 name == frame name
   - `code_exact`: L2 name == frame code (e.g. Durham, Forsyth)
   - `paren_code`: trailing `(code)` in the L2 name == frame code
   - `county_rule`: county-specific translation of names into codes, for
     counties whose official names are just the codes (e.g. Guilford
     `GREENSBORO 01` -> `G01`)
   - `name_std`: standardized name. Uppercase; drop trailing `(...)`; turn
     `# , . / -` into spaces; `PCT` -> `PRECINCT`; drop `NUMBER`/`WARD`;
     strip zero-padding (`01` -> `1`).
   - `code_std`: standardized L2 name == standardized frame code
   - `name_strip`: also strip a leading code token (`03 CRESTON` -> `CRESTON`)
   - `name_bag`: word order ignored (`NEWTON EAST` == `EAST NEWTON`). Run it
     after the in-order tiers so `01-10` vs `10-01` cannot collide.
   - `name_nospace`: spaces ignored (`ALDERMAN 1` == `ALDERMAN1`)
   - `lead_code_suffix`: leading L2 number == frame code minus its alpha
     prefix (Onslow `01 JACKSONVILLE` -> `JA01`)
3. **Validate with voter counts.** Compute `n_l2 / n_frame` by tier. On exact
   matches in NC it was about 1.0-1.22 (median 1.08). Fuzzy-tier matches
   outside that range need a look. Exact matches with odd ratios are fine
   (voter-file vintage differs). The same ratio check backs every `manual`
   entry.
4. **Eyeball every non-exact match.** Print county / L2 name / frame code /
   frame name for every tier except `name_exact` and `code_exact`. There are a
   few hundred rows and it takes minutes.
5. **List what is left on both sides by county**, with voter counts, and
   classify each leftover as one of: merge/split (leave NA), a one-off rename
   (add to `manual` if it is the only candidate and the ratio is normal), or
   ambiguous (leave NA).

## NC results (2026-10-15)

- 3,529 frame rows; 3,498 have a precinct code. Of those, 99.4% matched ENR,
  99.5% matched L2, and 99.4% matched both.
- Left NA on purpose (frame side): Cabarrus 02-05/02-07 (now 02-10); Halifax
  ENF1/ENF 2 (ENFIELD) and RR1-2/RR3-4 (RR1-4); Johnston PR24/PR25 (SELMA) and
  PR27A/PR27B (SMITHFIELD); Lenoir PH1/PH2 (PINK HILL). These are all merges.
  Also Buncombe 681, which has no L2 counterpart.
- Ambiguous, also left NA, worth a hand check: Vance frame `NH` vs ENR `TWN` /
  L2 `TOWNSVILLE`. These are the only leftovers in the county and the counts
  are consistent, but the code-to-name link is unconfirmed. Cherokee frame
  `ANNW` and `TOPT` have no ENR match because ENR's `ANW` appears to be
  ANNW+TOPT merged.

## Logged exception: San Diego consolidated precincts (2026-10-15)

San Diego, Solano, Riverside and Nevada (below) are the **only** places where the frame is aggregated to match ENR. This
breaks rule 3 on purpose, and only in the separate files listed below.
`ca.csv` itself stays strictly 1:1, and San Diego ENR stays NA there.

- **Why:** San Diego ENR reports 2,994 consolidated precincts
  (`0002-100090-SAN PASQUAL`) covering 6,427 frame base precincts, so a 1:1
  crosswalk gives no ENR at all.
- **Script:** `scripts/util/crosswalks/ca_sandiego_consolidated.R`. Run it after
  `ca.R`.
- **Outputs** (in `input_data/crosswalks/`):
  - `ca_sandiego_members.csv`: frame row -> `precinct_enr`.
  - `ca_sandiego_frame_consolidated.csv`: one row per consolidation, keyed on
    `precinct_enr`.
- **Membership is inferred, not official.** No 2026 list was found.
  - Rule `namesake_range`: consolidation numbers and namesake codes both
    ascend, so each base goes to the consolidation with the largest namesake
    code <= its own code.
  - Hard check `cd_ok`: every member's dominant frame CD equals the
    consolidation's ENR house district. If not, `use = FALSE`.
  - Soft check `place_share`: L2 community name vs consolidation place.
- **Aggregation:** `n` is summed. All other frame variables are n-weighted
  means. `age` becomes an n-weighted mean of precinct medians. Frame rows with
  any `cd` are pooled, and the output `cd` is the ENR district
  (`cd_frame_share` gives the agreement).
- **Results:** 2,090 consolidations have frame members, and 1,893 have
  `use = TRUE` (88.9% of 2,007,894 frame voters). Voter-weighted place
  agreement is 98.9%. 904 ENR consolidations get no frame member under the
  rule.
- **Recheck on real results.** The ENR votes are placeholders as of
  2026-10-15. Ballots per consolidation should be <= `n` and roughly
  proportional to it. Replace the rule if the ROV publishes a membership list.

## Logged exception: Solano consolidated precincts (2026-10-23)

Same exception as San Diego, built the same way. `ca.csv` keeps Solano ENR NA.

- **Why:** Solano ENR (LiveVoterTurnout) reports 196 consolidations named after
  one base precinct (`17025`, labelled `17025 - BT-46` with its ballot type).
  They cover 608 frame base precincts (`17005.00`). 185 of the 196 names are
  frame base codes.
- **Script:** `scripts/util/crosswalks/ca_solano_consolidated.R`. Run it after
  `ca.R`.
- **Outputs** (in `input_data/crosswalks/`): `ca_solano_members.csv` and
  `ca_solano_frame_consolidated.csv`, laid out like the San Diego files, plus
  `city_ok`.
- **Membership is inferred, not official.** No list was found: solanocounty.gov
  posts only polling places and maps, and LVT gives no per-precinct
  registration.
  - Rule `namesake_floor`: the San Diego range rule. Members share the
    namesake's L2 place for 97.7% of voters; the ceiling alternative gives
    94.8%. Bases below the first namesake (17005-17020, 2,715 voters) are
    unassigned.
  - Hard check `cd_ok`: weak, since Solano is 99.8% CA-08.
  - Hard check `city_ok`: all members in one city (Benicia, Dixon, Fairfield,
    Rio Vista, Suisun, Vacaville, Vallejo; L2 name prefix), or all
    unincorporated. Each city has its own contests, so a consolidation can't
    cross a city line. `use = cd_ok & city_ok`.
- **Results:** 191 consolidations have frame members, and 176 have
  `use = TRUE` (94.4% of 276,037 frame voters). The 15 that fail mix a city
  with unincorporated areas or another city under the rule.
- **Recheck on real results**, as for San Diego.
- **Sonoma is not built this way.** Its 331 `07xxxxx` consolidations are
  numbered separately from base precincts, so there is no namesake to range
  over. It needs the ROV's membership list (see `crosswalk_todo.md`).

## Logged exception: Riverside and Nevada consolidated precincts (2026-10-30)

Same exception as San Diego and Solano. `ca.csv` keeps both counties' ENR NA.

- **Why:** Riverside ENR (LVT) reports 1,206 consolidations over 3,433 frame
  base precincts. Only 969 ENR ids are frame base codes, and the 2,464 bases
  that aren't hold 914k of 1.43M frame voters, so the earlier 1:1 match joined
  each consolidation to its namesake base. Nevada ENR (Clarity) reports 105
  consolidations (`CP001-C3A1S1-B23`) over 386 base precincts.
- **Scripts:** `scripts/util/crosswalks/ca_riverside_consolidated.R` and
  `ca_nevada_consolidated.R`. Run them after `ca.R`.
- **Outputs** (in `input_data/crosswalks/`): `ca_{riverside,nevada}_members.csv`
  and `ca_{riverside,nevada}_frame_consolidated.csv`, laid out like the San
  Diego files.
- **Membership is inferred, not official.** Both counties use vote centers and
  publish no consolidation list.
  - Rule: spatial. Each base precinct (Statewide Database 2026 primary base
    polygons, `input_data/swdb/`) goes to the VoteHub `CA26G` consolidation
    covering most of its area. A Riverside base whose code is an ENR id goes
    to its own consolidation (1,200 of 1,201 would anyway). Voter-weighted
    area share is 0.99 in both counties.
  - `use` requires the house district to agree and no member base split below
    75% area. Bases redrawn after the primary (e.g. Riverside 104120, 104124)
    fail this.
- **Results:** Riverside 951/970 consolidations with members usable (97.8% of
  frame voters); Nevada 86/90 (92.5%).
- **Recheck on real results**, as for San Diego.

