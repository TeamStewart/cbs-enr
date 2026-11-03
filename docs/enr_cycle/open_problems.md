# ENR cycle: open problems

Open problems for agents to pick up. Each brief stands on its own. Updated
2026-10-31 at the end of an `/enr-cycle` session; see the last `SESSION` line
in `log.md` for that session's context. When an item is done, delete it here
and add a line to `log.md` with `cycle.R log`.

## Rules for every task

- Read `docs/enr_cycle.md` (runbook) first. For crosswalk work, also read
  `docs/crosswalk_todo.md` and `docs/precinct_crosswalks.md`.
- Never edit `metadata.csv` by hand. Make tracker changes with
  `cycle.R set ...` and sheet changes with `cycle.R sheet-add ...`, then run
  `cycle.R sheet` (dry run) and stop: the person running the task applies
  sheet edits.
- Clarity (`results.enr.clarityelections.com`): one request at a time, at
  least 2 s apart, no EID scans, and stop on any HTTP 405 (its WAF blocks the
  IP for hours).
- enightdata (`~/Documents/github/enightdata`, branch `main`; `nov26-scrapers` was merged 10-30):
  follow its `CLAUDE.md`. Every change needs a test, plus a clean
  `devtools::document()` and `devtools::test()`. Never push.
- Do not commit. Report what changed.
- Data root: `~/MIT Dropbox/Mason Reece/Research/CBS-MIT Election Data/26_general/`
  (crosswalks in `input_data/crosswalks/`, ENR in `{ST}/clean/`).

## Discovery and scrapers

### 3. MI Ogemaw: get the PDF id from the Drive folder on election night

The steps are in the row's `disc_note`. List the folder with
`curl -sL "https://drive.google.com/embeddedfolderview?id=1r_JWFQ2XntQCXIrVlHDgBi53Pg0Ehm97" | grep -oE 'entry-[A-Za-z0-9_-]+|flip-entry-title">[^<]+'`,
check the file starts with `%PDF`, then
`set MI Ogemaw disc_status=found "disc_path=https://drive.usercontent.google.com/download?id=<ID>&export=download&confirm=t"`.
`pdf_download()` already handles this URL. If the county uploads a new file
per update, the id changes.

## Crosswalks

### 4. Feed counties with no crosswalk (from `match_rates.R`, 2026-10-31)

`Rscript scripts/util/crosswalks/match_rates.R` lists feed counties that a
map doesn't cover. `l2` is `crosswalks/{st}.csv` (needs both an ENR and an L2
id) plus the direct fallback `crosswalks/l2_direct/{st}.csv`; `votehub` is
`crosswalks/votehub/{st}.csv`. The 10-31 pass fixed every VoteHub gap (NE,
IA, OH Carroll, FL Franklin) and GA L2 except Burke; 11-01 fixed CA Ventura. Left:

| state | map | counties | feed precincts | note |
|---|---|---|---|---|
| CA | l2 | Nevada, Riverside, San Diego, Solano | 4,501 | L2 comes through the consolidated members files (full files and model prep read them; `match_rates.R` doesn't). Ventura fixed 11-01 from the county's `Ventura_PrecinctMap.csv` (1,252 of 1,254 frame rows). |
| GA | l2 | Burke | 12 | renumbered for 2026; L2 (10-06) has the old numbers. Needs a newer Burke L2 pull. |
| NY | l2 (direct) | Chemung, Clinton, Madison, Ontario, Orange, Putnam, Sullivan, Ulster | 967 | not yet inspected |
| WI | l2 (direct) | Barron, Dane, Oneida | 340 | not yet inspected |

Also NA by choice in VoteHub: NE Howard (Boelus, Cushing, Farwell vs Loup
Fork, Posen, Spring Creek) and NE Webster's two Red Cloud precincts. The
pairing is unknown.
IA L2 is 1,638 of 1,666 ENR precincts since `ia.R` (10-31); the other 28 are
NA by design (Mahaska polling-place acronyms, Muscatine city/township pieces,
Warren and Johnson merges, Plymouth polling-place names; see `ia.R`).

For L2 gaps in states with no frame crosswalk, add county rules or hand pairs to
`l2_direct.R`. For frame counties, add a rule in `scripts/util/crosswalks/{st}.R`
(that also feeds the model). After a frame crosswalk changes:

1. Rerun model prep for the state: `MODEL_INPUTS_OUT=<tmp> Rscript scripts/model/prep/prep_model_inputs.R {ST}`,
   diff with `26_general/input_data/model_inputs/{ST}_*` (teammates prep with
   newer code; see the prep report header), and copy only if the diff is the
   new crosswalk rows. A stale prep is what broke Ellis: the crosswalk had the
   ids, the model inputs didn't.
2. `l2_direct.R {ST}`, then `match_rates.R --write`, and update this table.

### 5. CA live ids: 70.7% join (10-31)

Since 10-31 `prep_model_inputs.R` reads the `ca_{name}_members.csv` /
`ca_{name}_frame_consolidated.csv` files: each usable consolidation (`use`) is
one model precinct (frame pieces merged, L2 summed over members' L2 precincts,
geo = the ENR id); members of unusable consolidations whose id equals a
consolidation's ENR id get a `~frame` geo so live votes can't land on them.
All 3,106 usable consolidations receive live rows. Remaining unjoined CA live
ids (production `match_live_geo()` against `CA_precinct.parquet`):

| county | joined | why not |
|---|---|---|
| Sonoma | 50 / 403 | 331 `07xxxxx` consolidations with their own numbering, no membership list |
| San Diego | 1,893 / 2,994 | 904 consolidations have no frame members (no voters); 197 fail `cd_ok` |
| San Bernardino | 1,334 / 2,066 | 732 ENR ids have no frame row |
| Ventura | 1,147 / 1,433 | |
| Riverside | 951 / 1,206 | 19 fail `cd_ok`/`geo_ok`; rest no frame members |
| Nevada | 86 / 105 | 4 fail `geo_ok` |
| Solano | 176 / 196 | 15 fail `city_ok` |

The prep's own "live PRECINCT ids joining geo exactly" check compares verbatim
while production falls back to a normalized match, so it understates (NE 46.7%
exact vs 99.9% production; MI 0.5% vs 94.2%). CA has no races in
`input_data/priors.csv` yet.

### 7. `votehub.R` picks per-state shapefile FOLDERS by the folder's own date (11-03, urgent)

`read_vh()` uses a per-state layer (`votehub-shapefiles/{ST}26G` folder or `.zip`) instead of
`26_pct_bdrys.gpkg` when `fs::file_info()` says it is newer than the gpkg, and a folder beats its
own zip. For a folder that date is the folder's, which changes whenever it is unzipped or
touched by Dropbox, not its files'. On 11-02/03 the `AK26G` and `GA26G` folders (files dated
Sept 21/24, older than the gpkg) counted as newer, and their unit_ids differ from the gpkg's
(AK `02001-:-600 Ketchikan No. 1` vs `02001-:-01-600`): a rebuild took AK VoteHub to 0/522 and
GA to 83.7%. Both were restored by hand from the 10-29 files (`crosswalks/votehub/{ak,ga}.csv`
and the `precinct_votehub` column of `crosswalks/{ak,ga}.csv`).

Until fixed: never run `votehub.R` with no states named, or on AK or GA.

Proposed fix, in `read_vh()`: judge a folder by its newest file.

```r
mtime = \(f) if (fs::is_dir(f)) max(fs::file_info(fs::dir_ls(f))$modification_time) else fs::file_info(f)$modification_time
newer = map_vec(files, mtime) > fs::file_info(gpkg)$modification_time
```

Not applied on 11-03 because it also changes OH: the `OH26G` folder is currently winning over
`OH26G.zip` (Nov 2), which is the layer `votehub_manual_20261102.csv` was built for (its 326
Cuyahoga pairs print "not found" on every OH run, a sign the wrong layer is in use). With the fix,
OH (and NE, FL) would switch to their Nov 2 zips as intended. To apply: make the change, run
`Rscript scripts/util/crosswalks/votehub.R AK GA OH NE FL`, check that each state's "was ..."
rate does not drop and the Cuyahoga warning is gone, then `match_rates_county.R`.

## Model reference tables

### 6. `model_precincts.csv` / `model_counties.csv` are stale against the model inputs (11-01)

On 11-01 GA, MN and OH were added to `STATE_FIPS` in
`scripts/model/prep/make_model_reference.R` (the Demographics page's Urbanicity
blocks read RUCC from `model_counties.csv`). The committed CSVs keep the old rows for
the other states and append only the GA/MN/OH rows, because a full rerun of the
script changes the existing states: the `model_inputs/{ST}_precinct.parquet` files
have been re-prepped since the CSVs were last built (30e1ccbd).

| state | `model_precincts.csv` rows now | full rerun | rows that differ (now / rerun) |
|---|---|---|---|
| CA | 23,685 | 20,062 | 11,028 / 7,405 |
| TX | 8,771 | 7,542 | 3,319 / 2,090 |
| FL | 5,459 | 5,455 | 604 / 600 |
| MI | 3,712 | 3,712 | 699 / 699 |
| NC | 2,633 | 2,626 | 24 / 17 |
| AK | 402 | 402 | 2 / 2 |

`model_counties.csv` differs in 58 rows (TX 43, CA 8, NC 5, FL 2). `model_precincts.csv`
feeds the Models page maps (`read_geo_units()` in `scripts/functions.R`) and
`scripts/util/make_dashboard_geo.R`, so before rerunning the script in full, check
that the changed rows are the re-prep's and not a regression, then rebuild the
dashboard geo files. GA/MN/OH have no `precinct_geo_crosswalk.csv` rows, so their
`unit_id` is NA and they add nothing to the maps.

## Not for agents (Mason)
