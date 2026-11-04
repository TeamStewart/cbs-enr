# ENR cycle log

`cycle.R` appends probe/verify/xwalk/sheet lines automatically. End each
session with `cycle.R log "SESSION end: ..."` (done; half-done; next; carry
forward anything still open from the previous SESSION line), then
`cycle.R compact`, which drops every entry before that line. Older entries are
in git history.

- 2026-11-03 20:44 SESSION end: done - 11-03 FL-only crosswalk pass: fl.R live-label recodes for Baker, Gilchrist, Seminole, Jefferson, Holmes, Lafayette (place_codes), Calhoun (C parts merged: 101/201/401/501 NA), Charlotte (.1 parts: base+part NA); FL frame ENR 4,149 -> 4,314/5,339; votehub.R vh_pairs Baker/Calhoun/Holmes/Lafayette/DeSoto, FL VoteHub 98.4% -> 99.3%; l2_direct FL 12 (Miami-Dade); baseline FL rows only rewritten (MI l2 DROPPED 94.6 -> 93.5% left for MI owner); discovery/verify/sheet skipped (scope). half-done - 12 FL frame counties have no clean file yet (Bay, Hamilton, Highlands, Manatee, Okaloosa, Orange, Pasco, Putnam, St Lucie, Suwannee, Taylor, Volusia): rerun fl.R + votehub.R FL + l2_direct.R FL + match_rates when they land; FL model inputs not re-prepped. next - MI l2 drop and OH/TX uncovered changes vs baseline; votehub.R folder-date fix (#7); CA gaps (#5); e-night verify reruns.
