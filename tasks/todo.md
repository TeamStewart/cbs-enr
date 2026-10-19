# Election-night sim: TX governor + senate (branch `ethan-sim`)

Goal: a test input for the 2026 models (`scripts/model/CONTRACT.md` on
`2026-modeling-initial`) without uploading hundreds of snapshot files.

Design: two files plus a function.

- `TX_truth.parquet`: final canvass, one row per `geo x method x contest x candidate`,
  with a `censored` flag.
- `TX_schedule.parquet`: one row per `geo x method x batch`. At time `t`, that cell's
  cumulative counted share becomes `cum_frac`. `t = NA` means it never reports during the sim.
- `sim_snapshot(truth, schedule, time)`: clean-schema rows as of `time` (what
  `{ST}/clean/*_latest.parquet` would hold). `sim_write_clean()` writes per-county clean
  files for testing the file-reading path.

Output: `{Dropbox}/26_sim/{scenario}/` (approved 2026-10-19; nothing else in Dropbox is written).

## Steps

- [x] `scripts/sim/make_sim_tx.R`: params, load `model_inputs/TX_*`
- [x] Truth: turnout (`est_turnout`, capped at 1.25 x `n`), method split (2022 general mode
      history, county average where missing), Dem two-party share (`dem2` + swing to the
      prior + county/precinct/race noise), mail lean `a_m + b_m * logit(dem2)` (NC values),
      third party from `oth_base` split minor/write-in
- [x] Censoring: per-county Beta drop rate (mean 10%); 1 county never reports
- [x] Schedule: county speed, county posting cycle of 15 or 30 min, early + first-mail dump
      after close, eday per precinct rounded up to the county's posting grid, late mail days
      later. El Paso closes at 21:00 ET, everyone else 20:00 ET.
- [x] `scripts/sim/sim_snapshot.R`: `sim_snapshot()`, `sim_write_clean()`; stage 0/4/6
- [x] Write truth, schedule, `manifest.json` to `26_sim/tx_base/`
- [x] Checks: `check_elec_data()` + `coerce_elec_data()` on snapshots, zero file at 19:00,
      monotone votes and stage, every geo in `TX_precinct.parquet`, final shares vs target,
      turnout vs `est_turnout`, no mode replication, reporting curve by county

## Review

Built 2026-10-19; `26_sim/tx_base/` written with seed 2026 (truth 147,216 rows, schedule
24,536 rows, 225 KB + 62 KB). `check_sim.R tx_base` passes everything: schema and
`check_elec_data()` at 19:00 / 22:00 / Nov 10, every geo in `TX_precinct.parquet`, all-zero
stage-0 file at 19:00, votes/stage/posting time never decrease, final snapshot equals truth,
0.02% mode replication.

- 32-county two-party Dem share: gov 0.516 (statewide target 0.488), sen 0.575 (target
  0.514; drew a +0.10 logit miss). The 32 counties run bluer than the state, so 32-county
  totals sit above statewide targets by design (uniform swing from 2024 pres on `dem2`).
- Mail lean is on: gov mail 0.68 two-party Dem vs early 0.51 / eday 0.51; mail is 4.6% of votes.
- Turnout 5.27M vs `est_turnout` 5.33M; max votes/n 1.02.
- Timing: early votes 61% counted at 20:30, 99% at 21:30; eday 34% counted at 21:30, 71% at 23:30, 92% at
  02:30; stage 6 only once late mail lands (Nov 5-9).
- Censoring: 502 of 6,134 precincts; county rates 1%-35% (Kaufman 35%). PARKER dark.
- `sim_write_clean()` tested to scratch: 503 per-county files for 19:00-03:00, same column
  types as the real clean files. Not uploaded.
- `manifest.json` records the git commit, which is still main's HEAD because the scripts
  aren't committed yet.
