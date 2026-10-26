---
description: Run the ENR cycle (find live sites, verify scrapers, rebuild crosswalks, update the Google Sheet)
argument-hint: "[states and/or instructions, e.g. MI TX, or phases 1-2 only]"
---

Run the ENR cycle as described in `docs/enr_cycle.md`. Read that file first.

Scope: $ARGUMENTS
(If the scope is empty, cover all states and all phases. If it names states, pass
them to `probe`, `verify` and `status`. If it names phases, run only those.)

1. Run `Rscript scripts/util/enr_cycle/cycle.R sync`, then `status`. Read the last
   `SESSION` line in `docs/enr_cycle/log.md` and pick up anything left half done.
2. Discovery: run `probe` (never `probe --deep` unless the scope asks for a "deep"
   Clarity search). Then hand-check the pending PDF and custom rows, frame
   counties first, and web-search the Clarity rows noted "web search by hand" (see
   the runbook). Record every result with `set`.
3. Scrapers: `verify` every row in the scraper queue. Fix failing enightdata scrapers
   per its `CLAUDE.md`, with a test, `devtools::document()` and `devtools::test()`.
4. Crosswalks: rebuild each stale state per `docs/crosswalk_todo.md`, update its
   Results entry, then run `xwalk-done`.
5. Google Sheet: run `cycle.R sheet` as a dry run. If there are no conflicts, run
   `sheet --apply`. Report any conflicts instead of resolving them.

Do not commit or push. Finish with
`cycle.R log "SESSION end: <done>; <half-done>; <next>"`, then summarize: sheet edits
applied, scraper fixes (uncommitted), crosswalks rebuilt, new blockers, and anything
that needs Mason (build flags, conflicts, auth).
