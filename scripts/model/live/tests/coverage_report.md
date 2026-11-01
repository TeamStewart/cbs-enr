# Interval coverage on the simulated nights

Written 2026-10-27, updated 2026-11-01. The 2026-11-01 section below covers the models that
run on the night, on the current code and sims; the sections after it are the original
2026-10-27 run (`coverage/main`), kept for the spec and interval comparisons.

## 2026-11-01: the run = TRUE models on the current code

The `run_config` runs behind the test dashboard predated `b3dbd600` (AK bucket votes' partisan
split) and `4a31f2ec` (medsl_sur without a vote-method split), so they were rerun on all eight
`_base` sims with only the `run = TRUE` models in `model_config.csv`:

```
CFG=<26_general>/model_config.csv
MODELS=$(awk -F, 'NR>1 && $6=="TRUE" {print $1}' "$CFG" | paste -sd, -)
for sc in ak_base fl_base ga_base mi_base mn_base nc_base oh_base tx_base; do
  Rscript scripts/model/live/tests/sim_night.R $sc --models=$MODELS --cores=5 \
    --out=<26_general>/model_tests/coverage/run_config
done
```

The previous CSVs are in `model_tests/archive/run_config_20261030`. The sims were regenerated
after 2026-10-27 (e.g. tx_base is now Senate only), and the model set differs, so these numbers
are not directly comparable with the tables further down.

- 2,760 fits, 2,748 ok. Not ok: medsl_sur `prior_only` at the first 3 AK checkpoints and the
  first MN checkpoint (each x 3 medsl_sur variants).
- medsl_sur now fits MI and MN (one pseudo-mode `all`). medsl_sur/tpsob_g/conf_seq has no
  interval on 10 fits (8 under 25% counted), and pois_bayes/conf_seq on 2.
- Coverage counts only fits with an interval. Columns as in the original run below.

| model | fits | no interval | MAE (pts) | coverage | width (pts) |
|---|--:|--:|--:|--:|--:|
| swing | 345 | 0 | 0.55 | 0.580 | 0.98 |
| swing/conformal | 345 | 0 | 0.55 | 0.968 | 3.22 |
| ratio_strata/strat_dg/conformal | 345 | 0 | 0.55 | 0.991 | 4.22 |
| pois_bayes | 345 | 0 | 0.53 | 0.383 | 1.02 |
| pois_bayes/conf_seq | 345 | 2 | 0.53 | 0.732 | 2.50 |
| medsl_sur/conformal | 341 | 0 | 0.48 | 0.933 | 3.50 |
| medsl_sur/tpsob_g/conformal | 341 | 0 | 0.51 | 0.944 | 3.74 |
| medsl_sur/tpsob_g/conf_seq | 341 | 10 | 0.51 | 0.909 | 3.65 |

By scenario, coverage (MAE, width):

| scenario | swing | swing conf | ratio_strata conf | pois_bayes | pois conf_seq | medsl_sur conf | medsl tpsob_g conf | medsl tpsob_g conf_seq |
|---|---|---|---|---|---|---|---|---|
| ak_base | 0.70 (0.74, 1.8) | 1.00 (4.5) | 1.00 (0.89, 5.0) | 0.60 (0.86, 1.7) | 1.00 (6.2) | 1.00 (0.48, 7.9) | 1.00 (0.64, 7.4) | 1.00 (10.2) |
| fl_base | 0.35 (0.47, 0.9) | 1.00 (2.3) | 1.00 (0.13, 3.0) | 0.00 (0.42, 0.2) | 0.16 (1.0) | 0.95 (0.37, 1.8) | 1.00 (0.36, 1.9) | 0.51 (1.1) |
| ga_base | 1.00 (0.06, 0.7) | 1.00 (1.8) | 1.00 (0.07, 2.7) | 0.76 (0.06, 0.2) | 1.00 (0.7) | 1.00 (0.15, 1.8) | 1.00 (0.18, 1.9) | 1.00 (0.8) |
| mi_base | 0.22 (1.81, 1.6) | 0.97 (7.1) | 0.94 (1.95, 9.6) | 0.86 (1.49, 5.4) | 0.92 (8.6) | 0.97 (1.31, 7.2) | 1.00 (1.27, 8.0) | 0.97 (8.8) |
| mn_base | 1.00 (0.12, 0.9) | 1.00 (3.4) | 1.00 (0.30, 5.2) | 0.22 (0.24, 0.7) | 0.31 (1.2) | 1.00 (0.10, 4.0) | 1.00 (0.09, 4.7) | 1.00 (4.0) |
| nc_base | 0.11 (0.34, 0.4) | 1.00 (2.2) | 1.00 (0.40, 3.5) | 0.11 (0.39, 0.5) | 1.00 (2.9) | 1.00 (0.10, 4.2) | 1.00 (0.21, 5.4) | 1.00 (3.5) |
| oh_base | 0.11 (1.42, 1.6) | 0.73 (4.3) | 1.00 (1.31, 4.2) | 0.00 (1.45, 0.3) | 0.03 (0.6) | 0.46 (1.54, 3.9) | 0.49 (1.51, 3.5) | 0.68 (5.5) |
| tx_base | 0.71 (0.27, 0.8) | 1.00 (2.7) | 0.99 (0.26, 3.4) | 0.31 (0.24, 0.7) | 1.00 (2.5) | 1.00 (0.25, 2.5) | 1.00 (0.33, 2.4) | 1.00 (3.1) |

What changed since 2026-10-27:

- **AK medsl_sur is fixed** (item 1 below, by `b3dbd600`): MAE 0.5-0.6 points and every
  interval holds, against 4.37 and 0 of 17 before.
- **MI and MN medsl_sur run** (item 2 below, by `4a31f2ec`). In MI medsl_sur has the lowest
  error of any model (1.3 points) but is 3-7 points too Democratic before about 40% counted.
- **OH is the new problem scenario.** Every model is off by 1.3-1.5 points and only
  ratio_strata/strat_dg/conformal holds the truth every time; medsl_sur's conformal intervals
  cover about half the fits and swing/conformal 73%. Not diagnosed.
- All three medsl_sur variants and the conformal intervals for swing and ratio_strata stay
  above 90% overall; pois_bayes credible (38%) and conf_seq (73%) under-cover, worst in FL,
  MN and OH.

## Original run (2026-10-27)

All 10 scenarios in `26_sim/` as of that date. **lm_bayes not run** (dropped on time, see
"Not covered").

### How it was run

```
for s in tx_base tx_hispanic_swing tx_turnout_demo tx_turnout_partisan_D0.1 nc_base mi_base ga_base oh_base mn_base ak_base; do
  Rscript scripts/model/live/tests/sim_night.R $s \
    --models=swing,swing/conformal,ratio_strata/conformal,pois_bayes,pois_bayes/conf_seq,pois_bayes/conformal,medsl_sur \
    --cores=4 --out=<26_general>/model_tests/coverage/main
done
Rscript scripts/model/live/tests/coverage_summary.R <26_general>/model_tests/coverage/main
```

- Models as of commit `fcb11f7f` (renames, specs and intervals). Every checkpoint with votes
  (every 15 minutes 8pm-4am ET, then Nov 4, 5, 7 and 10), every race in the scenario (TX: gov
  and sen; the others: sen), on the counties the sim covers (no statewide rows). Plain names
  run the model's default interval (swing bootstrap, pois_bayes credible, medsl_sur credible).
- **Error**: |estimated - true| Dem two-party share, in points. **Coverage**: share of fits
  whose 95% two-party interval holds the true final share. **Width**: hi - lo, in points.
- **Share counted** = counted votes / final votes in the sim's counties. It rarely passes 90%
  (5 fits per model), because each sim has censored precincts that never report (tx_base:
  502 precincts, plus Parker County, whose schedule has no times). So this says little about
  the intervals once a race is nearly fully counted.
- Fits within one scenario are refits of the same night every 15 minutes and are strongly
  correlated: 487 fits are closer to 10 independent nights than to 487 trials. Read 1.000 as
  "never missed on these nights".
- 3,338 fits ok. Not ok: medsl_sur in MI and MN (skipped: no vote-method split, by design
  at the time; medsl_sur fits them since `4a31f2ec`, see the 2026-11-01 section) and 3 early
  AK medsl_sur fits (`prior_only`).

### By model x interval, all scenarios

| model | interval | fits | MAE (pts) | coverage | width (pts) |
|---|---|--:|--:|--:|--:|
| swing | bootstrap (default) | 487 | 0.46 | 0.396 | 0.63 |
| swing | conformal | 487 | 0.46 | 0.996 | 3.47 |
| ratio_strata | conformal | 487 | 0.46 | 0.996 | 4.37 |
| pois_bayes | credible (default) | 487 | 0.40 | 0.448 | 0.95 |
| pois_bayes | conf_seq | 487 | 0.40 | 0.979 | 3.20 |
| pois_bayes | conformal | 487 | 0.40 | 0.996 | 2.96 |
| medsl_sur | credible (default) | 416 | 0.36 | 0.947 | 1.33 |

(medsl_sur has no MI or MN fits; on the scenarios it ran, the other models' MAE is about
0.3.)

### By share of votes counted

| model | interval | share counted | fits | MAE | coverage | width |
|---|---|---|--:|--:|--:|--:|
| swing | bootstrap | <10% | 17 | 1.92 | 0.529 | 2.59 |
| swing | bootstrap | 10-25% | 8 | 2.36 | 0.750 | 1.91 |
| swing | bootstrap | 25-50% | 47 | 0.90 | 0.383 | 0.97 |
| swing | bootstrap | 50-75% | 153 | 0.47 | 0.386 | 0.64 |
| swing | bootstrap | 75-90% | 257 | 0.23 | 0.374 | 0.39 |
| swing | bootstrap | 90-99% | 5 | 0.04 | 1.000 | 0.18 |
| swing | conformal | <10% | 17 | 1.93 | 0.882 | 10.86 |
| swing | conformal | 10-25% | 8 | 2.36 | 1.000 | 9.58 |
| swing | conformal | 25-50% | 47 | 0.90 | 1.000 | 6.41 |
| swing | conformal | 50-75% | 153 | 0.47 | 1.000 | 3.63 |
| swing | conformal | 75-90% | 257 | 0.23 | 1.000 | 2.21 |
| swing | conformal | 90-99% | 5 | 0.04 | 1.000 | 0.77 |
| ratio_strata | conformal | <10% | 17 | 2.09 | 0.882 | 11.62 |
| ratio_strata | conformal | 10-25% | 8 | 2.62 | 1.000 | 10.87 |
| ratio_strata | conformal | 25-50% | 47 | 0.91 | 1.000 | 7.27 |
| ratio_strata | conformal | 50-75% | 153 | 0.45 | 1.000 | 4.46 |
| ratio_strata | conformal | 75-90% | 257 | 0.22 | 1.000 | 3.16 |
| ratio_strata | conformal | 90-99% | 5 | 0.04 | 1.000 | 0.97 |
| pois_bayes | credible | <10% | 17 | 1.70 | 0.765 | 4.96 |
| pois_bayes | credible | 10-25% | 8 | 1.98 | 0.500 | 2.54 |
| pois_bayes | credible | 25-50% | 47 | 0.67 | 0.660 | 1.58 |
| pois_bayes | credible | 50-75% | 153 | 0.37 | 0.471 | 0.92 |
| pois_bayes | credible | 75-90% | 257 | 0.24 | 0.377 | 0.56 |
| pois_bayes | credible | 90-99% | 5 | 0.04 | 0.200 | 0.05 |
| pois_bayes | conf_seq | <10% | 17 | 1.69 | 0.941 | 20.81 |
| pois_bayes | conf_seq | 10-25% | 8 | 1.97 | 0.750 | 5.16 |
| pois_bayes | conf_seq | 25-50% | 47 | 0.67 | 0.915 | 6.03 |
| pois_bayes | conf_seq | 50-75% | 153 | 0.37 | 0.980 | 2.55 |
| pois_bayes | conf_seq | 75-90% | 257 | 0.24 | 1.000 | 1.91 |
| pois_bayes | conf_seq | 90-99% | 5 | 0.04 | 1.000 | 0.12 |
| pois_bayes | conformal | <10% | 17 | 1.70 | 0.882 | 9.18 |
| pois_bayes | conformal | 10-25% | 8 | 1.99 | 1.000 | 8.68 |
| pois_bayes | conformal | 25-50% | 47 | 0.68 | 1.000 | 5.45 |
| pois_bayes | conformal | 50-75% | 153 | 0.38 | 1.000 | 3.03 |
| pois_bayes | conformal | 75-90% | 257 | 0.24 | 1.000 | 1.92 |
| pois_bayes | conformal | 90-99% | 5 | 0.04 | 1.000 | 0.72 |
| medsl_sur | credible | <10% | 10 | 3.09 | 0.400 | 5.34 |
| medsl_sur | credible | 10-25% | 4 | 2.58 | 0.500 | 5.26 |
| medsl_sur | credible | 25-50% | 36 | 1.24 | 0.722 | 3.19 |
| medsl_sur | credible | 50-75% | 133 | 0.31 | 0.970 | 1.57 |
| medsl_sur | credible | 75-90% | 232 | 0.09 | 1.000 | 0.67 |
| medsl_sur | credible | 90-99% | 1 | 0.01 | 1.000 | 0.27 |

### By scenario (coverage, with MAE and width in points)

| scenario | swing boot | swing conf | ratio conf | pois cred | pois conf_seq | pois conf | medsl cred |
|---|---|---|---|---|---|---|---|
| tx_base | 0.24 (0.28, 0.5) | 1.00 (2.8) | 1.00 (0.26, 3.6) | 0.14 (0.27, 0.7) | 1.00 (3.4) | 1.00 (2.6) | 0.99 (0.13, 1.1) |
| tx_hispanic_swing | 0.94 (0.21, 1.0) | 1.00 (4.9) | 1.00 (0.29, 6.0) | 0.97 (0.26, 0.7) | 1.00 (3.5) | 1.00 (3.4) | 0.97 (0.23, 1.1) |
| tx_turnout_demo | 0.35 (0.24, 0.5) | 1.00 (2.9) | 1.00 (0.16, 4.0) | 0.15 (0.30, 0.8) | 1.00 (3.5) | 1.00 (2.6) | 1.00 (0.15, 1.1) |
| tx_turnout_partisan_D0.1 | 0.03 (0.51, 0.5) | 1.00 (2.8) | 1.00 (0.43, 3.7) | 0.14 (0.29, 0.7) | 1.00 (3.4) | 1.00 (2.5) | 0.99 (0.16, 1.1) |
| nc_base | 0.05 (0.27, 0.3) | 1.00 (1.8) | 1.00 (0.39, 3.1) | 0.11 (0.27, 0.5) | 1.00 (2.5) | 1.00 (1.4) | 0.97 (0.10, 1.5) |
| mi_base | 0.14 (2.13, 0.7) | 0.97 (6.4) | 0.97 (2.57, 7.5) | 0.69 (1.62, 2.7) | 0.75 (4.8) | 0.97 (6.5) | skipped |
| ga_base | 0.81 (0.07, 0.2) | 1.00 (1.7) | 1.00 (0.17, 2.7) | 1.00 (0.03, 0.5) | 1.00 (0.9) | 1.00 (1.4) | 1.00 (0.03, 0.4) |
| oh_base | 0.05 (0.69, 0.7) | 1.00 (5.0) | 1.00 (0.18, 4.5) | 1.00 (0.67, 2.1) | 1.00 (4.1) | 1.00 (3.5) | 1.00 (0.59, 2.6) |
| mn_base | 1.00 (0.10, 1.1) | 1.00 (3.3) | 1.00 (0.10, 4.4) | 0.28 (0.11, 0.6) | 1.00 (1.1) | 1.00 (3.3) | skipped |
| ak_base | 0.50 (0.88, 1.1) | 0.95 (3.8) | 0.95 (1.06, 4.7) | 0.25 (0.89, 1.5) | 0.95 (4.2) | 0.95 (3.6) | 0.00 (4.37, 4.2) |

Cells: coverage (MAE, width); for the second and later intervals of a model, coverage
(width), since the MAE is the same point estimate. Per-row numbers are in
`coverage_summary.R`'s `--out=` CSVs.

## Reading

- **swing bootstrap and pois_bayes credible are badly under-covered**: 40% and 45% over all
  scenarios, and under 40% once half the vote is in, when they are 0.4-0.9 points wide
  against a 0.2-0.5 point error. Neither has a term for the uncounted vote differing
  systematically from the counted vote; the miss is in every share band, not just early.
- **conformal holds almost everywhere.** 99.6% for swing, ratio_strata and pois_bayes. Its
  only misses are under 10% counted (the first checkpoint in MI and AK). It is conservative:
  2-3 points wide at 75-90% counted, about 10x the error.
- **conf_seq is weaker than conformal for pois_bayes.** 97.9% overall, and it is the only
  "wide" interval with a real failure: MI, 75% (9 misses in 36), where the model error is
  1.6 points. It is narrower than conformal at 50-75% counted (2.55 vs 3.03), about equal
  at 75-90% (1.91 vs 1.92), and much wider very early (21 points under 10% counted).
- **medsl_sur credible is the best narrow interval:** 97-100% coverage from half the vote
  on, at 0.7-1.6 points wide, and the lowest error there (0.09 at 75-90%). It under-covers
  early (40-72% under half counted) and fails completely on AK (below).
- MI is the hardest scenario: every model is off by 1.6-2.6 points. It is modeled without
  vote method; a likely but untested cause is that differences between the methods counted
  first and last go unmodeled.

## Recommended default interval per model (MODEL_INTERVALS not changed; Mason decides)

| model | today's default | recommended | why |
|---|---|---|---|
| swing | bootstrap | **conformal** | bootstrap covers 40%; conformal 99.6% at 2.2 points from 75% counted |
| ratio_strata | none | **conformal** | the only interval it has; 99.6% |
| pois_bayes | credible | **conformal** | credible covers 45%; conformal 99.6% vs conf_seq 97.9% (conf_seq missed 25% of MI fits), at similar width late and much less width early |
| lm_bayes | credible | no data | not run (see below); README's single tx_base night: credible 1.4 points and missed, conf_seq and conformal held |
| medsl_sur | credible | **credible**, keep | 94.7% overall, 97-100% from half counted; the only interval it has. Fix AK first (below) |

If the conformal widths are too wide for the dashboard, the next step would be a
calibrated in-between (e.g. a conformal quantile below 95%, or inflating the credible
interval by a measured factor), not the current bootstrap / credible intervals.

## Model issues found on 2026-10-27

Both are fixed; see the 2026-11-01 section for the reruns.

1. **(Fixed by `b3dbd600`.) medsl_sur in AK stays at its prior all night.** Estimates 0.43-0.46 against a true
   0.505, interval never holds the truth (0 of 17), status "ok (partial prior: 7 of 9
   equations ...)" from 1am to the last checkpoint, and `prior_only` before that, while
   pois_bayes converges to 0.503. Not diagnosed: the 2 equations left at the prior are the
   first place to look. Relevant to `R/model_medsl.R` and `medsl/model_turnout.R`.
2. **(Fixed by `4a31f2ec`: medsl_sur now fits such states on one pseudo-mode `all`.)
   medsl_sur's set-up aborts for a state with no vote-method split.** On MN,
   `fit_medsl_setup_for()` stops at `scripts/model/live/medsl/model_turnout.R:783` with
   `MODES names all with no MODE_TURN_SHARE fallback value.` (MI did not hit it). The fits
   themselves are skipped correctly ("no vote-method split"), but anything that calls the
   set-up first (`sim_night.R`, `time_models.R`, `run_night.R`) dies for the whole run on such
   a state. `sim_night.R` now catches it; `run_night.R` should be checked for MN.

## Harness changes in this commit (tests/sim_night.R)

- A failed medsl_sur set-up no longer stops the run (item 2 above); its jobs report why.
- A skipped fit (no race row) keeps its status ("skipped: ...") instead of NA.
- A run where every fit fails still writes its CSV.

## Item 10 check (sim_statewide.R, medsl_sur statewide rows)

`sim_statewide.R tx_base --checkpoints=calib --races=gov --models=swing,pois_bayes,medsl_sur`
now runs medsl_sur at all 7 checkpoints with statewide rows (before: `CBS county snapshot is
missing method` every time). Dem share of all votes, mean absolute error over the 7
checkpoints: statewide (geo = all) swing 0.57, pois_bayes 0.53, medsl_sur 0.60 points; scraped
counties 0.26, 0.27, 0.25. At the last checkpoint 31 of 32 scraped counties pass
`county_targets()`'s completeness gate (Parker never reports in tx_base), so the county arm
can fire there.

## Not covered

- **lm_bayes** (credible, conf_seq, conformal): dropped on time (2-3 hours for tx_base alone).
  To run it later:
  ```
  Rscript scripts/model/live/tests/sim_night.R tx_base --models=lm_bayes,lm_bayes/conf_seq,lm_bayes/conformal --cores=4 --out=<26_general>/model_tests/coverage/lm_bayes
  Rscript scripts/model/live/tests/coverage_summary.R <26_general>/model_tests/coverage/main <26_general>/model_tests/coverage/lm_bayes
  ```
- Statewide (geo = all) intervals: `sim_night.R` scores the sim's counties only; conf_seq and
  conformal have no statewide interval until item 5.
- Nearly complete counts (over 90%): see the share-counted note above.
