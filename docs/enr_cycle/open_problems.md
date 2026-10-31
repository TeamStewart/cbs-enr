# ENR cycle: open problems

Open problems for agents to pick up. Each brief stands on its own. Written
2026-10-30 at the end of an `/enr-cycle` session; see the last `SESSION` line
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
- enightdata (`~/Documents/github/enightdata`, branch `nov26-scrapers`):
  follow its `CLAUDE.md`. Every change needs a test, plus a clean
  `devtools::document()` and `devtools::test()`. Never push.
- Do not commit. Report what changed.
- Data root: `~/MIT Dropbox/Mason Reece/Research/CBS-MIT Election Data/26_general/`
  (crosswalks in `input_data/crosswalks/`, ENR in `{ST}/clean/`).

## Discovery and scrapers

### 1. SC statewide: path points at the wrong election

The SC row's path `https://www.enr-scvotes.org/SC/127179` is the Aug 25 2026
election, not the general, and scvotes.gov has no November link yet (rechecked 10-30: newest is
127227, the Sep 1 State Senate 15 special primary). Recheck
scvotes.gov/election-results/ daily. When the general's EID appears, confirm
`electiondate` 11/3/2026 (two Clarity requests at most), then
`set SC - disc_status=found disc_path=...` and `verify SC -`.

### 2. NJ Morris: rerun verify on election night

Morris is `found` at `https://results.enr.clarityelections.com/NJ/Morris/127576/`
(electiondate 11/3/2026). `verify` failed on 10-30 because
`reports/detailxml.zip` returns 404 before any results post (summary only).
On election night, once results start, run `verify NJ morris` once. If the
zip is still missing, the scraper needs a summary-JSON fallback.

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
map doesn't cover (`l2`: `crosswalks/{st}.csv`; `votehub`:
`crosswalks/votehub/{st}.csv`). On 2026-10-31:

| state | map | counties | feed precincts |
|---|---|---|---|
| TX | l2, votehub | Dallas, Fort Bend, Reeves, Robertson, Tarrant | 4,122 |
| CA | l2 | Nevada, Riverside, San Diego, Solano (consolidated crosswalks, which `match_rates.R` doesn't read), Ventura | 5,934 |
| PA | l2 | Montgomery, York | 590 |
| FL | l2 | Pinellas, St Johns | 343 |
| FL | votehub | Franklin, Pinellas | 296 |
| NE | votehub | Adams, Box Butte, Cass, Dawes, Dawson, Dodge, Hamilton, Holt, Hooker, Howard, Loup, Webster | 156 |
| MI | l2 | St Clair, St Joseph | 85 |
| GA | l2 | Bleckley, Burke, Candler, Glascock, Jackson, Morgan, Quitman, Webster | 26 |
| OH | votehub | Carroll | 23 |

For each frame county: rebuild `scripts/util/crosswalks/{st}.R` (and
`votehub.R`). Check first whether the county has a clean file but isn't in the
frame (then L2 can't apply), or whether the ids need a county rule. TX notes:
Dallas ids carry a `-NN` suffix (`1004-01`) like Ellis (`1001-501`, which
`tx.R` strips); Tarrant reports sub-units (`1005 - F99`), which are sums, as
in Wood. Then:

1. Rerun model prep for the state: `MODEL_INPUTS_OUT=<tmp> Rscript scripts/model/prep/prep_model_inputs.R {ST}`,
   diff with `26_general/input_data/model_inputs/{ST}_*` (teammates prep with
   newer code; see the prep report header), and copy only if the diff is the
   new crosswalk rows. A stale prep is what broke Ellis: the crosswalk had the
   ids, the model inputs didn't.
2. `match_rates.R --write`, and delete the fixed rows from this table.

## Not for agents (Mason)

- `build` / `trust_stage` for the 27 rows made live on 10-30.
- `ICX` → `eday` in `general_method()` (Orange NY ImageCast X ballots; the
  source doesn't say whether they were cast early or on election day).
- RI drops the "Federal" pseudo-jurisdiction (federal-only ballots, no town).
- Commit enightdata and the cbs-enr crosswalk scripts and docs.
- SD statewide row (`custom`, electionresults.sd.gov) appeared in the sheet
  on 10-30 (not added by the cycle): decide `build` and run discovery/verify.
- AR: a statewide scraper is feasible (see AR `disc_note`: public
  `FullDataFile.json` with county and precinct results, no vote-method split).
  Decide whether to write it; AR is not a frame state.
- Review two inferred crosswalk calls: FL Liberty precincts 2 and 4 (Annex vs
  Lake Mystic, assigned by ENR order; confirm with the SOE), and OH Greene
  Sugarcreek rows numbered 157/158 from L2 size match (ENR now depends on L2
  evidence).
- Commit enightdata `R/scrape-id.R`, `R/scrape-pa.R` (Montgomery CSV) and
  tests, and the new `ca_riverside_consolidated.R` / `ca_nevada_consolidated.R`.
