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
IA, OH Carroll, FL Franklin) and GA L2 except Burke. Left:

| state | map | counties | feed precincts | note |
|---|---|---|---|---|
| CA | l2 | Nevada, Riverside, San Diego, Solano, Ventura | 5,934 | first four: L2 comes through the consolidated members files (full files and model prep read them; `match_rates.R` doesn't). Ventura: L2's `precinct` is another scheme ("CALLEGUAS 5-001", 1,148 units) with no link to ENR's 7-digit codes, and no newer L2 exists. Accepted. |
| GA | l2 | Burke | 12 | renumbered for 2026; L2 (10-06) has the old numbers. Needs a newer Burke L2 pull. |
| IA | l2 (direct) | 27 counties (Adair, Allamakee, Audubon, Harrison, Johnson, Lee, Scott, ...) | 383 | direct names don't match; many need `vh_manual`-style pairs (see `votehub.R`). Overlaps problem 5. |
| NY | l2 (direct) | Chemung, Clinton, Madison, Ontario, Orange, Putnam, Sullivan, Ulster | 967 | not yet inspected |
| WI | l2 (direct) | Barron, Dane, Oneida | 340 | not yet inspected |

Also NA by choice in VoteHub: NE Howard (Boelus, Cushing, Farwell vs Loup
Fork, Posen, Spring Creek) and NE Webster's two Red Cloud precincts. The
pairing is unknown.

For L2 gaps in states with no frame crosswalk, add county rules or hand pairs to
`l2_direct.R`. For frame counties, add a rule in `scripts/util/crosswalks/{st}.R`
(that also feeds the model). After a frame crosswalk changes:

1. Rerun model prep for the state: `MODEL_INPUTS_OUT=<tmp> Rscript scripts/model/prep/prep_model_inputs.R {ST}`,
   diff with `26_general/input_data/model_inputs/{ST}_*` (teammates prep with
   newer code; see the prep report header), and copy only if the diff is the
   new crosswalk rows. A stale prep is what broke Ellis: the crosswalk had the
   ids, the model inputs didn't.
2. `l2_direct.R {ST}`, then `match_rates.R --write`, and update this table.

### 5. IA: frame ids have no names

For the full files this is partly moot: `l2_direct.R` gives 940 of 1,666 IA
ENR precincts an L2 id (10-31) without the frame. The model still needs it.

IA ENR (statewide Clarity, `IA/clean/IA_latest.parquet`, 1,666 precincts) is
clean, but no frame crosswalk exists. Why: every other state's frame has
either precinct names in the voter file (`precinct26_name`) or codes that the
ENR/L2 ids share. IA has neither. The frame `precinct26_id` is a county-made
short code and `precinct26_name` repeats it, while ENR and L2 use names:

| county | frame | ENR | L2 |
|---|---|---|---|
| Guthrie | BP, CP, GCP, PP, SP, YP | BAYARD, CASEY, GUTHRIE CENTER, PANORA, ... | BAYARD, CASEY, GUTHRIE CENTER, ... |
| Monona | AS001, CP002, ..., ON105 | AS-BV-CS-CN-KB-TU, ..., Onawa Ward 1 | 01 ASHTON/BELVIDERE/..., 05 ONAWA WARD 1 |
| Carroll | 01 ... 08 | Arcadia Precinct, Carroll Ward Four, ... | ARCADIA, CARROLL 1/GRANT TWP, ... |
| Audubon | 01, 02 | Precinct #1 Audubon N1/2, Precinct #2 Exira S1/2 | 1, 2 |

Counting codes that appear verbatim (punctuation ignored): 203 of 1,658 frame
codes are in ENR, 112 in L2, 148 in VoteHub IA26G; only 4 counties match fully
and 72 counties match nowhere. ENR <-> L2 is name to name and doable with the
usual tiers; the missing link is frame code -> name.

Possible routes, per county: (a) a leading or trailing sequence number shared
with L2 (Monona `ON105` = L2 `05 ONAWA WARD 1`; Audubon `01` = L2 `1`);
(b) initials (Guthrie `GCP` = GUTHRIE CENTER PRECINCT); (c) where neither
works, assign frame codes to L2 names by voter count within county (frame `n`
vs L2 `n`) and accept only unambiguous, close pairs; (d) county auditor
precinct lists. Every pair needs the voter-count check. Write
`scripts/util/crosswalks/ia.R` following `docs/crosswalk_todo.md` "Per-state
steps", then `votehub.R IA`, `xwalk-done IA`, `match_rates.R --write`, and
prep IA model inputs.

### 6. CA live ids: 70.7% join (10-31)

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
- `build` / `trust_stage` for the 27 rows made live on 10-30.
- `ICX` → `eday` in `general_method()` (Orange NY ImageCast X ballots; the
  source doesn't say whether they were cast early or on election day).
- RI drops the "Federal" pseudo-jurisdiction (federal-only ballots, no town).
- SD statewide row (`custom`, electionresults.sd.gov) appeared in the sheet
  on 10-30 (not added by the cycle): decide `build` and run discovery/verify.
- AR: a statewide scraper is feasible (see AR `disc_note`: public
  `FullDataFile.json` with county and precinct results, no vote-method split).
  Decide whether to write it; AR is not a frame state.
- Review two inferred crosswalk calls: FL Liberty precincts 2 and 4 (Annex vs
  Lake Mystic, assigned by ENR order; confirm with the SOE), and OH Greene
  Sugarcreek rows numbered 157/158 from L2 size match (ENR now depends on L2
  evidence).
