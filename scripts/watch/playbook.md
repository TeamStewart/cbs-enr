# Watcher playbook

Known patterns and standing decisions, read by L2 on every run. Add to it after each night:
promote rulings from `$WATCH_HOME/state/decisions.json` that held up.

- Before a state's polls close, scraper errors such as "No target races (zero rows)", HTTP 404,
  "Clarity returned no rows" and "found no precinct results" usually mean the 2026 general
  page isn't posted yet. Mark them `expected`.
- Clarity sites return HTTP 405 after bursts of requests (a WAF). That's transient: don't
  propose a patch for it.
- `tbl_cbs_*` errors whose `data_*` sibling errored are knock-ons. The checks already skip them.
- A wrong vote-method or candidate mapping is always `human`, with a link to the vendor page
  as evidence.
- Sheet edits (`updated`, `path`) are `human`. The Sheet is read at the end of a cycle and
  used by the next one.
