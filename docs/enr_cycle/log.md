# ENR cycle log

`cycle.R` appends probe/verify/xwalk/sheet lines automatically. End each
session with `cycle.R log "SESSION end: ..."` (done; half-done; next; carry
forward anything still open from the previous SESSION line), then
`cycle.R compact`, which drops every entry before that line. Older entries are
in git history.

- 2026-10-31 15:05 SESSION end: done - 10-31: prep_model_inputs.R reads CA consolidated crosswalks (3,106 consolidations, all get live rows; CA live join 48.5% -> 70.7%; namesake geo guard for 18 Riverside precincts); CA inputs re-prepped and copied; earlier today AL Madison updated=FALSE, PA NM VA OH TX NE inputs shared, Tarrant/NM/NE geo fixes in enightdata (uncommitted). half-done - enightdata must be pushed before next automation run or Tarrant/NM/NE clean geos revert. next - push enightdata (Mason); IA ia.R (#5); remaining CA gaps (#6); daily rechecks from Nov 1; e-night verify reruns (#2).
- 2026-10-31 15:28 open problem 4: VoteHub gaps fixed (vh_manual + county keys in votehub.R; NE 97.4%, IA 81.5%, OH Carroll, FL Franklin); ga.R EV-label/PCT/Jackson/single rules (GA L2 uncovered 26 -> 12, Burke only); new l2_direct.R fallback read by full_join.R and match_rates.R (NJ 93.9%, NY 74.5%, IA 73.3%, WI 55.8%, TX +83); baseline written; GA model prep not copied (diff includes frame vintage + new columns)
- 2026-10-31 16:19 GA and MN model inputs re-prepped with current code and frame (vf_frame_current_202610, reg22 est_turnout) and copied, matching the other states. GA adds the 10-31 crosswalk fixes (L2 2558 -> 2563, live ids 2558 -> 2564); MN has no 2022 county totals so k_c = 1, as NM/PA/VA. 10-27 files backed up in ~/model_inputs_backup_20261027
