# ENR cycle log

`cycle.R` appends probe/verify/xwalk/sheet lines automatically. End each
session with `cycle.R log "SESSION end: ..."` (done; half-done; next; carry
forward anything still open from the previous SESSION line), then
`cycle.R compact`, which drops every entry before that line. Older entries are
in git history.

- 2026-10-31 15:05 SESSION end: done - 10-31: prep_model_inputs.R reads CA consolidated crosswalks (3,106 consolidations, all get live rows; CA live join 48.5% -> 70.7%; namesake geo guard for 18 Riverside precincts); CA inputs re-prepped and copied; earlier today AL Madison updated=FALSE, PA NM VA OH TX NE inputs shared, Tarrant/NM/NE geo fixes in enightdata (uncommitted). half-done - enightdata must be pushed before next automation run or Tarrant/NM/NE clean geos revert. next - push enightdata (Mason); IA ia.R (#5); remaining CA gaps (#6); daily rechecks from Nov 1; e-night verify reruns (#2).
- 2026-10-31 15:28 open problem 4: VoteHub gaps fixed (vh_manual + county keys in votehub.R; NE 97.4%, IA 81.5%, OH Carroll, FL Franklin); ga.R EV-label/PCT/Jackson/single rules (GA L2 uncovered 26 -> 12, Burke only); new l2_direct.R fallback read by full_join.R and match_rates.R (NJ 93.9%, NY 74.5%, IA 73.3%, WI 55.8%, TX +83); baseline written; GA model prep not copied (diff includes frame vintage + new columns)
- 2026-10-31 16:19 GA and MN model inputs re-prepped with current code and frame (vf_frame_current_202610, reg22 est_turnout) and copied, matching the other states. GA adds the 10-31 crosswalk fixes (L2 2558 -> 2563, live ids 2558 -> 2564); MN has no 2022 county totals so k_c = 1, as NM/PA/VA. 10-27 files backed up in ~/model_inputs_backup_20261027
- 2026-10-31 16:22 xwalk-done IA: 1 rows ()
- 2026-10-31 17:02 open problem 5: new scripts/util/crosswalks/ia.R. Frame codes matched to L2 names within county by voter profile + code/digit/town evidence (lp.assign), ENR names to frame rows by name similarity. IA frame crosswalk 1658 rows: L2 100%, ENR 1632 (98.4%); NA by design Mahaska, Muscatine non-ward, Warren/Johnson merges, Plymouth. match_rates IA l2 98.3% (was 73.3%). IA model inputs prepped and copied (new; live join 98.0% via match_live_geo). Check on e-night: Clarity totalVoters vs frame n.
- 2026-10-31 19:36 xwalk-done IA: 1 rows ()
- 2026-11-01 07:00 CA Ventura L2 crosswalk: ca.R county rule from Ventura_PrecinctMap.csv (NUMBER_ -> ELECTID, per place-district offset); 1,252/1,254 frame rows get L2; CA prep rerun and copied (L2 coverage 80.4% -> 86.1%); match_rates CA l2 68.9% -> 72.0%
- 2026-11-01 07:04 AR: tally-enr statewide scraper written (enightdata ar-tally worktree, uncommitted); verify pass 2768 precincts; sheet path+type=custom queued, not applied
- 2026-11-01 07:07 sheet --apply: wrote 3 cells (AR  path; AR  type; AR  updated); metadata.csv refreshed
- 2026-11-01 07:10 AR: scraper pushed to enightdata main (de82cec); sheet AR path/type=custom/updated=TRUE applied
- 2026-11-01 12:55 probe all : 62 rows, 0 found, 1 unchanged
- 2026-11-01 12:58 discovery 11-01: probe 61 rows all not_posted (no WAF 405s); verified the 4 stale Clarity EIDs in updated=FALSE paths are all non-general (AL Madison 127283=9/22, CO 126592=6/30, PA Susquehanna 123829=5/20/2025, SC 127179=8/25) and FL Pinellas 127357 confirmed 11/3/2026 but already updated=TRUE. BLOCKER: cycle.R sync fails because 26_general/input_data/create_frame.R hardcodes ../CBS-MIT Election Data paths (Mason layout), so the tracker is stale: FL pinellas shows not_posted though live, AZ greenlee/yuma rows obsolete since AZ went statewide. Hand-check queue not worked pending sync.
- 2026-11-03 18:07 xwalk-done SC: 0 rows ()
