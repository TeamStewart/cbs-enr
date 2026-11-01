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

### 1. SC statewide: path points at the wrong election

The SC row's path `https://www.enr-scvotes.org/SC/127179` is the Aug 25 2026
election, not the general, and scvotes.gov has no November link yet (rechecked 10-31: newest is
127227, the Sep 1 State Senate 15 special primary). Recheck
scvotes.gov/election-results/ daily. When the general's EID appears, confirm
`electiondate` 11/3/2026 (two Clarity requests at most), then
`set SC - disc_status=found disc_path=...` and `verify SC -`.

### 2. Rerun verify on election night (pre-election files missing)

These rows point at the right Nov 3 site, but `verify` failed on 10-31 because
the precinct file isn't published before results post:

- LiveVoterTurnout `Precincts_{id}.csv` 404: FL Broward (240), AZ Pima (7),
  CA San Joaquin (22), KS Leavenworth (3), MO Buchanan (27), Ray (23), Stone
  (15), NJ Bergen (20). San Bernardino's file already exists, so the format is fine.
- Clarity `reports/detailxml.zip` 404 or not a zip (summary only): NJ Morris,
  Monmouth (new 10-31, EID 127586), Cumberland, Gloucester; CA Contra Costa;
  WV statewide passes with only 10 geos for the same reason.
- WA Cowlitz, Pacific, Skamania, Wahkiakum: statewide EV export has no precinct rows.
- NY NYC: enr.boenyc.gov is an offline stub (no AD/ED table).

Once results start, run `verify {ST} {county}` once per row. If a Clarity zip
is still missing, the scraper needs a summary-JSON fallback.

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

## Not for agents (Mason)

- **Push enightdata before the next automation run** (uncommitted on local
  main): `consolidate_units()` for Tarrant, `scrape_nm()` and `scrape_ne()`
  geo from `PrecinctName`, with tests. The crosswalks and model inputs built
  10-31 already use the new geos (`tx_220_1005`, `nm_018_067`, `ne_028_08-01`);
  automation pulls enightdata from origin, so until this is pushed its runs
  rewrite those clean files with the old geos and the three crosswalks stop
  matching. Rebuild `tx.R`/`nm.R`/`ne.R` if that happens first.
  `devtools::document()` needs roxygen2 8.1.1 (8.0.0 installed).
- Commit the 10-31 cbs-enr changes: `tx.R`, `pa.R`, `ca.R`, `fl.R`, `mi.R`,
  `ne.R`, `votehub.R`, new `nm.R`, `prep_model_inputs.R` (VA cities, `&`,
  CA consolidations).
- VA model inputs exist now but `VA_race_prior.csv` has 0 races (no VA rows
  in `input_data/priors.csv`) and there is no `VA_county_history.csv`.
- IA (`ia.R`, 10-31) matches frame codes to L2 names by voter profile plus
  code/name/township evidence, not by shared ids. Election-night check: the
  Clarity detail XML's `totalVoters` per precinct (0 before results) against
  frame `n` for each matched ENR precinct; a ratio far from ~1 means a wrong
  pair. IA has `IA_race_prior.csv` (sen) but no `IA_county_history.csv`.
  `l2_direct.R` pairs Muscatine ENR "FUL TWP" with L2 "FULTON TWP/MONTPELLIER
  TWP", a merge (ENR reports "MONT TWP" separately).
- `build` / `trust_stage` for the 27 rows made live on 10-30.
- `ICX` → `eday` in `general_method()` (Orange NY ImageCast X ballots; the
  source doesn't say whether they were cast early or on election day).
- RI drops the "Federal" pseudo-jurisdiction (federal-only ballots, no town).
- SD statewide row (`custom`, electionresults.sd.gov) appeared in the sheet
  on 10-30 (not added by the cycle): decide `build` and run discovery/verify.
- Review two inferred crosswalk calls: FL Liberty precincts 2 and 4 (Annex vs
  Lake Mystic, assigned by ENR order; confirm with the SOE), and OH Greene
  Sugarcreek rows numbered 157/158 from L2 size match (ENR now depends on L2
  evidence).
