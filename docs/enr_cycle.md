# ENR cycle: find live sites -> check scrapers -> build crosswalks

A repeatable loop for the weeks before 2026-11-03. Each pass looks for
jurisdictions that have posted their 2026 general results page, checks the
enightdata scraper against it, and rebuilds precinct crosswalks for states
whose ENR precincts changed. All state lives in files, so a session can stop
at any point and a fresh session can pick up from `status`.

## Kicking off a run (Claude Code)

From the project root, in a fresh Claude Code session:

```
/enr-cycle                 # all states, all phases
/enr-cycle MI TX           # only these states
/enr-cycle phases 1-2 only # discovery + scraper checks, no crosswalks/sheet
/enr-cycle FL; recheck not_posted PDF rows   # free-text instructions also work
```

The command lives in `.claude/commands/enr-cycle.md`. It runs the steps below
(sync, status, log, phases 1-3, then the sheet dry run and `--apply` if there
are no conflicts). It never commits or pushes, and it ends with a `SESSION`
line in `log.md` plus a summary. To stop partway, tell it to stop and log;
the next `/enr-cycle` resumes from that line.

## Files

| file | what | who writes it |
|---|---|---|
| `scripts/util/enr_cycle/cycle.R` | CLI for every step below | — |
| `docs/enr_cycle/tracker.csv` | one row per `metadata.csv` row: discovery, scraper and crosswalk status | `cycle.R` only (use `set` for hand edits) |
| `docs/enr_cycle/sheet_changes.csv` | pending Google Sheet cell edits (`state, county, field, old, new, note`): verified URLs plus the manual queue | `cycle.R` (regenerated on every write) |
| `docs/enr_cycle/sheet_manual.csv` | hand-queued sheet edits (`sheet-add`) | `cycle.R` |
| `docs/enr_cycle/log.md` | append-only log: automatic probe/verify/xwalk lines plus a `SESSION` line per session | `cycle.R` |
| `docs/crosswalk_todo.md`, `docs/precinct_crosswalks.md` | crosswalk method, per-state results | crosswalk work |
| enightdata `tasks/todo.md`, `CLAUDE.md` | scraper rules, known scraper blockers | scraper work |

**Never edit `metadata.csv` by hand.** `scripts/automation.R` overwrites it
from the Google Sheet. Change the sheet with `cycle.R sheet` (see "Google
Sheet" below), which then refreshes `metadata.csv` from the sheet.

## Start of every session

```sh
Rscript scripts/util/enr_cycle/cycle.R sync      # pull in metadata.csv, frame, clean/ files
Rscript scripts/util/enr_cycle/cycle.R status    # queues + last log lines
```

Read the last `SESSION` line in `log.md` first: it says what was left half
done. Work the queues in order (1, 2, 3), frame counties first (`frame=TRUE`
are counties in the YouGov frame, the only ones that get crosswalks). Stop
whenever; everything is saved after each command.

## End of every session

```sh
Rscript scripts/util/enr_cycle/cycle.R log "SESSION end: <done>; <half-done>; <next>"
```

Then ask Mason before committing `docs/enr_cycle/`, the crosswalk scripts, or
enightdata changes. Never push enightdata: `automation.R` pulls it on election
night.

## Status values

| column | values |
|---|---|
| `disc_status` | `pending` (not checked yet), `not_posted` (checked, no 2026 general page yet), `found` (2026 URL in `disc_path`, not in sheet), `live` (sheet has `updated=TRUE`), `blocked` (no usable source; see `disc_note`) |
| `scr_status` | `untested`, `pass`, `fail`, `blocked` (no scraper possible). Resets to `untested` when the URL changes. |
| `xw_status` | `n/a` (not a frame county), `waiting` (no clean data), `stale` (precinct set changed since the last crosswalk build), `current` |

`platform` is the enightdata dispatcher arm for the row (`clarity`, `ev`,
`lvt`, `pdf_*`, a county name for bespoke scrapers, `single` for one-scraper
states, `none` for no scraper).

## Phase 1: discovery

**Automated** (Clarity, Enhanced Voting, LiveVoterTurnout):

```sh
Rscript scripts/util/enr_cycle/cycle.R probe            # all pending/not_posted rows
Rscript scripts/util/enr_cycle/cycle.R probe MI         # one state
Rscript scripts/util/enr_cycle/cycle.R probe TX bexar   # one row
```

- Clarity: `/{ST}/{County}/elections.json`, looking for an election dated
  2026-11-03. Many large counties return `[]`; then it scans
  `/{ST}/{County}/{EID}/current_ver.txt` over the EID window around the 2026
  general EIDs already known and confirms `electiondate` in
  `electionsettings.json` (~7 s per county). A county not on the standard
  Clarity domain or slug needs a hand check.
- EV: `{host}/results/public/api/jurisdictions/{slug}`, the election with
  `electionDate` 2026-11-03, giving `export-{publicElectionId}.json`. The slug
  comes from the current path or `{county}-county-{st}`.
- LVT: the current site id and the next five, looking for "November 3, 2026"
  on the page.
- Found rows go to `found`; it never overwrites `live` rows. To recheck a
  `found` row, `set ... disc_status=pending`.

**By hand** (`pdf_*`, bespoke platforms, `none`, statewide customs): open the
`path` landing page (curl with a browser UA, or Playwright if JS/Cloudflare)
and look for a Nov 3 2026 link. Record the result either way:

```sh
Rscript scripts/util/enr_cycle/cycle.R set MI Bay disc_status=found disc_path=https://... "disc_note=Nov 3 PDF link on results page"
Rscript scripts/util/enr_cycle/cycle.R set MI Bay disc_status=not_posted "disc_note=page lists only Aug 2026"
```

- PDF rows: the metadata `path` must be the PDF itself (see enightdata
  `CLAUDE.md`). Most counties post the PDF only on election night, so
  `not_posted` is the normal answer before then. Don't recheck a `not_posted`
  row checked in the last 3 days, except daily from Nov 1.
- Bespoke rows whose scraper finds the page itself (e.g. Rock, Jefferson,
  Polk, Barron, Vernon defaults; Hardin is already election 13): check the
  site has the general and record `found` with the URL the scraper should
  use.

## Phase 2: scraper check

```sh
Rscript scripts/util/enr_cycle/cycle.R verify TX bexar            # uses disc_path or the sheet path
Rscript scripts/util/enr_cycle/cycle.R verify TX bexar https://...  # try another URL
```

`verify` loads enightdata from source (`devtools::load_all`, so local fixes
count), runs `{st}_timestamp()` and `scrape_{st}()`, then `verify_scrape()`
(schema, `_NA_` geos, duplicates, `check_elec_data`, `coerce_elec_data`) and
prints the candidate and method tables. On a pass it writes
`{ST}/clean/{stem}_latest.*` and a timestamped parquet, the same files
`get_data()` writes, so phase 3 can run without `tar_make()`.

Check the printed tables by eye as well: unmapped candidate names, wrong
party, missing target contests, `method` labels. Before Nov 3, zero votes and
`stage 0` everywhere are expected, but every precinct must be there.

On `fail`, or a pass with bad tables:

1. Fix it in `~/Documents/github/enightdata` (branch `nov26-scrapers`),
   following its `CLAUDE.md`. Add a test for the fix.
2. `devtools::document()` and `devtools::test()` must be clean.
3. Rerun `verify`. Note the fix in `scr_note` if useful, e.g.
   `set ... "scr_note=fixed general_method 'Absentee Walk-in' (uncommitted)"`.

A pass with a `found` URL queues `path` and `updated=TRUE` for the Google
Sheet automatically; push them with `cycle.R sheet` (below).

## Phase 3: crosswalks

The queue lists frame counties whose precinct set (`geo` hash of the latest
clean file) differs from the one the state's crosswalk was last built on.

1. Follow the "Rerun procedure" in `docs/crosswalk_todo.md`. For a state with
   no script yet, use its per-state steps and agent prompt template.
2. `Rscript scripts/util/crosswalks/{st}.R`; check coverage against that
   state's Results entry; if ENR coverage dropped, inspect the new ids.
3. Update the state's row and Results entry in `docs/crosswalk_todo.md`.
4. `Rscript scripts/util/enr_cycle/cycle.R xwalk-done {ST}` marks every
   clean row of the state as current.

## Google Sheet

The sheet (`SHEET_ID` in `cycle.R`, the same one `automation.R` reads) is the
source of `metadata.csv`. `cycle.R` may write only `path`, `path_timestamp`,
`type` and `updated`. `build`, `upload`, `trust_stage` and `for_cbs` stay
Mason's decision: mention them in the `SESSION` line instead.

```sh
# queue a hand edit (anything other than a verified URL, which queues itself)
Rscript scripts/util/enr_cycle/cycle.R sheet-add MI Bay type=pdf "note=landing page links Hart SOVC PDF"
Rscript scripts/util/enr_cycle/cycle.R sheet            # dry run: always read this first
Rscript scripts/util/enr_cycle/cycle.R sheet --apply    # write
```

- The dry run reads the live sheet and lists every queued cell with its live
  value and an action: `write`, `skip: already set`, `skip: no unique sheet
  row`, or `CONFLICT` (the live value no longer equals the `metadata.csv`
  value the edit was based on: someone edited the sheet). Resolve a conflict
  by hand; `--apply` never writes it.
- Push only URLs that passed `verify` (the automatic ones) or a hand edit you
  checked against the site. Apply in batches, e.g. once per phase-2 pass.
- `--apply` writes single cells, reads them back, rewrites `metadata.csv` and
  `metadata.parquet` from the sheet (as `automation.R` does), prints any other
  rows that changed in the sheet meanwhile, logs the write, and runs `sync`.
- Auth: needs a cached googlesheets4 token (`~/Library/Caches/gargle`). If
  `sheet` says "No cached Google token", stop and ask Mason to run
  `R -e 'options(rlang_interactive = TRUE); googlesheets4::gs4_auth(cache = TRUE)'`.
