# ENR cycle log

`cycle.R` appends probe/verify/xwalk/sheet lines automatically. End each
session with `cycle.R log "SESSION end: ..."` (done; half-done; next; carry
forward anything still open from the previous SESSION line), then
`cycle.R compact`, which drops every entry before that line. Older entries are
in git history.

- 2026-10-31 15:05 SESSION end: done - 10-31: prep_model_inputs.R reads CA consolidated crosswalks (3,106 consolidations, all get live rows; CA live join 48.5% -> 70.7%; namesake geo guard for 18 Riverside precincts); CA inputs re-prepped and copied; earlier today AL Madison updated=FALSE, PA NM VA OH TX NE inputs shared, Tarrant/NM/NE geo fixes in enightdata (uncommitted). half-done - enightdata must be pushed before next automation run or Tarrant/NM/NE clean geos revert. next - push enightdata (Mason); IA ia.R (#5); remaining CA gaps (#6); daily rechecks from Nov 1; e-night verify reruns (#2).
