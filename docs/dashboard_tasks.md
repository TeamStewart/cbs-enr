# Dashboard rewrite: task list

Branch: `worktree-dashboard-contests`. Goal: config-driven State x Contest dashboard, all OJS, no modeling section.

Decisions (agreed 2026-09-23):
- Config tibble `DASHBOARD_CONTESTS` in `scripts/util/globals.R` (state, contest, district, label, hist_contest)
- One HTML; dropdown in navbar; all boxes + charts drawn client-side (Observable Plot)
- Boxes: Last Updated, Precincts (complete stage 6 + partial stage 1-5 / total), Total Votes, EDay Votes, leading Dem, leading Rep. "% of 'YY" only if history exists
- Charts: time plots (total / count / share, Total + per-method facets for methods present), margin histogram, swing histogram + scatter vs history (history only), precinct explorer
- Counties pooled per state; state time series carries each jurisdiction's last summary forward
- History spec: `{ELECTION_FOLDER}/history/{STATE}_history.parquet`, long: geo, method (NA ok), contest, district (NA ok), party, year, votes, county (optional)
- `tar_quarto` in pipeline, no S3 upload. `26_general` starts fresh (no summary backfill there)
- General-only party logic (dem vs rep). Test on fake Dem-vs-Rep data built from `26_primaries`

## Tasks

- [x] 1. `globals.R`: `DASHBOARD_CONTESTS`, `DASHBOARD_TITLE`, env override `CBS_ELECTION_FOLDER`
- [x] 2. `functions.R`: `make_summary()` (writes per-run summary parquet, returns precinct table), `build_dashboard_data()` (pool, carry-forward time series, precinct + history join, boxes)
- [x] 3. `_targets.R`: summary target in both tar_maps, `tar_combine` -> `dashboard_data`, `tar_quarto(dashboard)`
- [x] 4. Test data script `scripts/util/make_dashboard_test_data.R`: fake general folder from 26_primaries + backfilled summaries + one fake history file
- [x] 5. `index.qmd` rewrite (OJS dropdown, boxes, charts, explorer); `custom.scss`
- [x] 6. Render against test folder; check in browser (switching, no-history state, no-eday state, NE all-only)
- [x] 7. README: config + history spec; commit

## Notes / log

- `arrow` not in renv; parquet IO uses `nanoparquet` (as enightdata does). Added `quarto` R pkg to renv (needed by `tar_quarto`).
- NC scraper tags statewide `sen` with `district = 1`. Config rows with `district = NA` fall back to the single district present in the data.
- County-level scrapes (TX, PA) report `jurisdiction = "tx"`, so pooling/county fallback uses the source key (`TX_bexar`), not `jurisdiction`.
- `make_summary()` summary files: `{STATE}/summaries/summary_{key}_{time}.parquet` (time = max data `time`, UTC).
- Test folder: `CBS-MIT Election Data/26_dashtest` (GA history w/o methods, NC history w/ methods; NE all-only; PA 3 counties; TX-house-33). TX-30 not in primary data so test config uses 33.
- `tar_manifest()` on the real `_targets.R` fails here at `create_dirs()` (S3 403, stale AWS key in this shell). Not from this change; validated graph with those lines stripped.
- Quarto dashboard OJS gotchas hit while building (all handled in `index.qmd`):
  - OJS cells get re-printed from their AST (`autosizeOJSPlot`); a top-level object literal cell `x = ({...})` loses its parens and kills the whole chunk. Use `x = { return {...} }`.
  - Autosize only injects card width/height into a literal `Plot.plot({...})` with no spread properties, and only for single-output chunks. Helpers return specs; cells call `Plot.plot({ key: s.key, ... })`.
  - Charts in hidden tabs render at 0 size; a `shown.bs.tab` listener fires `resize` so they redraw. Hidden tabs still log harmless negative `<rect>` size errors on load.
  - Plot legends add height above the card-sized svg; CSS in `custom.scss` lets the svg shrink.
- Browser-checked (Playwright) on the test folder: GA (history w/o methods), NC (history w/ methods, EDay % of '22), NE (all-only, no EDay, no history), PA (17 pooled sources), TX-house-33; dropdown switching, `?c=` deep link, tab switching after a contest change.
- Page still loads Observable stdlib (d3/Plot) from jsdelivr at view time, same as the old dashboard.

## Open

- [ ] Many-to-many join warning in `make_dashboard_test_data.R` (seen 2026-10-30, 10-contest dashtest rebuild). It comes from the `inner_join()` on the VoteHub crosswalk `{ELECTION_FOLDER}/crosswalks/votehub/{st}.csv` in `scripts/functions.R` (around line 277): "Row 91 of `x` matches multiple rows in `y`. Row 1 of `y` matches multiple rows in `x`." The state is not yet identified. Either the crosswalk has duplicate keys or the join is missing a key, and both would double-count votes in `mres` (Results / Shift maps). Find the state, check the crosswalk for duplicate keys, and fix the data or the join. Log: `logs/sim/dashtest_data.log`.
