# ENR cycle log

`cycle.R` appends probe/verify/xwalk/sheet lines automatically. End each
session with `cycle.R log "SESSION end: ..."` (done; half-done; next; carry
forward anything still open from the previous SESSION line), then
`cycle.R compact`, which drops every entry before that line. Older entries are
in git history.

- 2026-10-31 14:51 SESSION end: done - 10-31 follow-up: AL Madison updated=FALSE in sheet; OH SOS PDF removal confirmed intentional; PA NM VA OH TX NE model inputs in shared folder (VA prep fixed for independent cities and &; OH was stale 10-27, 50% -> 100% live join); Tarrant sub-units summed in scrape_tx (consolidate_units), NM/NE geo from PrecinctName (enightdata, uncommitted, tests pass); tx.R nm.R ne.R votehub.R rebuilt on new geos, baseline written. half-done - enightdata must be pushed before next automation run or Tarrant/NM/NE clean geos revert. next - push enightdata (Mason); IA ia.R (open_problems #5); CA consolidated counties in prep (#6); daily rechecks from Nov 1; e-night verify reruns (#2).
