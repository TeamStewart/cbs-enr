# Interval coverage on the simulated nights (todo item 6)

Written 2026-10-27. **Interim: 2 of 10 scenarios, no lm_bayes yet** (see "Status" at the end).

## How it was run

```
Rscript scripts/model/live/tests/sim_night.R <scenario> \
  --models=swing,swing/conformal,ratio_strata/conformal,pois_bayes,pois_bayes/conf_seq,pois_bayes/conformal,medsl_sur \
  --cores=5 --out=26_general/model_tests/coverage/main
Rscript scripts/model/live/tests/coverage_summary.R 26_general/model_tests/coverage/main
```

- Commit `01461449` (renames, specs and intervals; item 10's feed change does not touch `sim_night.R`).
- Every checkpoint with votes (36: every 15 minutes 8pm-4am ET, then Nov 4, 5, 7 and 10), both
  races (gov, sen), on the scraped counties only (no statewide rows). One fit per checkpoint x
  race x model; plain names run the model's default interval.
- Error: |estimated - true| Dem two-party share, in points. Coverage: the share of fits whose
  95% two-party interval holds the true final share. Width: hi - lo, in points.
- "Share counted" is counted votes / final votes in the scraped counties. It never goes above
  about 88%: in tx_base 502 precincts are censored (never reported by the scrapers) and Parker
  County never reports (every schedule row has `t = NA`). So nothing here says how the
  intervals behave once a race is nearly fully counted.
- Fits within a scenario are strongly correlated (same night, refit every 15 minutes), so 144
  fits is closer to 2 independent nights than to 144 trials. A coverage of 1.000 means "never
  missed on these two nights", not "well calibrated".

## Results: tx_base + tx_hispanic_swing (1,008 fits, all ok)

By model x interval:

| model | interval | fits | MAE (pts) | coverage | width (pts) |
|---|---|--:|--:|--:|--:|
| swing | bootstrap (default) | 144 | 0.25 | 0.590 | 0.73 |
| swing | conformal | 144 | 0.25 | 1.000 | 3.86 |
| ratio_strata | conformal | 144 | 0.27 | 1.000 | 4.79 |
| pois_bayes | credible (default) | 144 | 0.26 | 0.556 | 0.71 |
| pois_bayes | conf_seq | 144 | 0.26 | 1.000 | 3.46 |
| pois_bayes | conformal | 144 | 0.26 | 1.000 | 2.97 |
| medsl_sur | credible (default) | 144 | 0.18 | 0.979 | 1.08 |

By share of votes counted (no checkpoint fell in 10-25%; the count jumps from under 10% to
over 25% between 8:15 and 8:30pm):

| model | interval | share counted | fits | MAE | coverage | width |
|---|---|---|--:|--:|--:|--:|
| swing | bootstrap | <10% | 4 | 1.10 | 0.750 | 3.32 |
| swing | bootstrap | 25-50% | 8 | 0.66 | 0.500 | 0.91 |
| swing | bootstrap | 50-75% | 32 | 0.24 | 0.875 | 0.97 |
| swing | bootstrap | 75-90% | 100 | 0.18 | 0.500 | 0.54 |
| swing | conformal | <10% | 4 | 1.11 | 1.000 | 16.01 |
| swing | conformal | 25-50% | 8 | 0.66 | 1.000 | 9.34 |
| swing | conformal | 50-75% | 32 | 0.24 | 1.000 | 4.73 |
| swing | conformal | 75-90% | 100 | 0.18 | 1.000 | 2.65 |
| ratio_strata | conformal | <10% | 4 | 1.10 | 1.000 | 15.82 |
| ratio_strata | conformal | 25-50% | 8 | 0.42 | 1.000 | 9.86 |
| ratio_strata | conformal | 50-75% | 32 | 0.25 | 1.000 | 5.88 |
| ratio_strata | conformal | 75-90% | 100 | 0.23 | 1.000 | 3.60 |
| pois_bayes | credible | <10% | 4 | 1.18 | 1.000 | 5.37 |
| pois_bayes | credible | 25-50% | 8 | 0.37 | 1.000 | 2.27 |
| pois_bayes | credible | 50-75% | 32 | 0.22 | 0.625 | 0.54 |
| pois_bayes | credible | 75-90% | 100 | 0.23 | 0.480 | 0.45 |
| pois_bayes | conf_seq | <10% | 4 | 1.17 | 1.000 | 32.86 |
| pois_bayes | conf_seq | 25-50% | 8 | 0.37 | 1.000 | 11.20 |
| pois_bayes | conf_seq | 50-75% | 32 | 0.22 | 1.000 | 2.42 |
| pois_bayes | conf_seq | 75-90% | 100 | 0.23 | 1.000 | 1.99 |
| pois_bayes | conformal | <10% | 4 | 1.17 | 1.000 | 12.24 |
| pois_bayes | conformal | 25-50% | 8 | 0.37 | 1.000 | 6.91 |
| pois_bayes | conformal | 50-75% | 32 | 0.22 | 1.000 | 3.66 |
| pois_bayes | conformal | 75-90% | 100 | 0.23 | 1.000 | 2.07 |
| medsl_sur | credible | <10% | 4 | 2.97 | 0.250 | 5.11 |
| medsl_sur | credible | 25-50% | 8 | 0.07 | 1.000 | 2.95 |
| medsl_sur | credible | 50-75% | 32 | 0.10 | 1.000 | 1.32 |
| medsl_sur | credible | 75-90% | 100 | 0.10 | 1.000 | 0.69 |

By scenario:

| scenario | model | interval | MAE | coverage | width |
|---|---|---|--:|--:|--:|
| tx_base | swing | bootstrap | 0.28 | 0.236 | 0.49 |
| tx_base | swing | conformal | 0.28 | 1.000 | 2.84 |
| tx_base | ratio_strata | conformal | 0.26 | 1.000 | 3.63 |
| tx_base | pois_bayes | credible | 0.27 | 0.139 | 0.69 |
| tx_base | pois_bayes | conf_seq | 0.27 | 1.000 | 3.38 |
| tx_base | pois_bayes | conformal | 0.27 | 1.000 | 2.57 |
| tx_base | medsl_sur | credible | 0.13 | 0.986 | 1.11 |
| tx_hispanic_swing | swing | bootstrap | 0.21 | 0.944 | 0.97 |
| tx_hispanic_swing | swing | conformal | 0.21 | 1.000 | 4.87 |
| tx_hispanic_swing | ratio_strata | conformal | 0.29 | 1.000 | 5.96 |
| tx_hispanic_swing | pois_bayes | credible | 0.26 | 0.972 | 0.73 |
| tx_hispanic_swing | pois_bayes | conf_seq | 0.26 | 1.000 | 3.53 |
| tx_hispanic_swing | pois_bayes | conformal | 0.26 | 1.000 | 3.38 |
| tx_hispanic_swing | medsl_sur | credible | 0.23 | 0.979 | 1.05 |

## Reading

- **swing bootstrap and pois_bayes credible are too narrow.** Their error after half the vote
  is counted is about 0.2 points, but their intervals are about 0.5 points wide and miss the
  truth on half the fits. On tx_base they miss for most of the night (24% and 14% coverage).
  Both treat the uncounted vote as if it behaves like the counted vote within strata, so they
  have no term for a systematic gap between the two.
- **conformal and conf_seq never missed, but are wide.** At 75-90% counted they are 2-3.6
  points wide against an error of about 0.2: roughly 10x the error, so they are conservative.
  For pois_bayes, conformal is narrower under 50% counted (conf_seq is 33 points wide under
  10%); from 50% on, conf_seq is narrower (2.4 vs 3.7 points at 50-75%, 2.0 vs 2.1 at 75-90%).
- **medsl_sur credible is the best calibrated of the narrow intervals:** 0.69-1.3 points wide
  after a quarter of the vote is in, and it held the truth on every fit past 25% counted. Its
  only misses are the first checkpoints (<10% counted), where its error is also largest
  (3 points).

## Recommended default interval per model (MODEL_INTERVALS not changed)

| model | today's default | recommended | why |
|---|---|---|---|
| swing | bootstrap | **conformal** | bootstrap covers 59% (24% on tx_base); conformal 100% at about 2.7 points late |
| ratio_strata | none | **conformal** | only interval it has; covers 100% |
| pois_bayes | credible | **conformal** | credible covers 56% (14% on tx_base); conformal avoids conf_seq's very wide early intervals (conf_seq is the alternative if the late-night width matters more) |
| lm_bayes | credible | pending | not run yet |
| medsl_sur | credible | **credible** (keep) | 98% coverage at about 1 point; only interval it has |

This is from two TX nights only. The other eight scenarios (other states, turnout shocks)
are where the conformal intervals could start missing, so treat the recommendation as
provisional until they are in.

## Item 10 check (sim_statewide.R, medsl_sur statewide rows)

`sim_statewide.R tx_base --checkpoints=calib --races=gov --models=swing,pois_bayes,medsl_sur`
now runs medsl_sur at all 7 checkpoints with statewide rows (before: `CBS county snapshot is
missing method` on every checkpoint). Dem share of all votes, mean absolute error over the 7
checkpoints: statewide (geo = all) swing 0.57, pois_bayes 0.53, medsl_sur 0.60 points; scraped
counties 0.26, 0.27, 0.25. At the last checkpoint 31 of 32 scraped counties pass
`county_targets()`'s completeness gate (Parker never reports in tx_base), so the county arm
can fire there.

## Status

Not run yet, because both runs were stopped by Claude Code when the machine ran low on memory
(agent A's `compare_runs.R` jobs, lm_bayes included, were running on the same machine):

- lm_bayes, lm_bayes/conf_seq, lm_bayes/conformal on tx_base (216 fits, about 1.5-2.5 hours
  on 4 cores);
- the 7 main variants on tx_turnout_demo, tx_turnout_partisan_D0.1, nc_base, mi_base,
  ga_base, oh_base, mn_base, ak_base.

To finish, from the cbs-enr folder (one run at a time on a shared machine):
```
for s in tx_turnout_demo tx_turnout_partisan_D0.1 nc_base mi_base ga_base oh_base mn_base ak_base; do
  Rscript scripts/model/live/tests/sim_night.R $s --models=swing,swing/conformal,ratio_strata/conformal,pois_bayes,pois_bayes/conf_seq,pois_bayes/conformal,medsl_sur --cores=4 --out=<26_general>/model_tests/coverage/main
done
Rscript scripts/model/live/tests/sim_night.R tx_base --models=lm_bayes,lm_bayes/conf_seq,lm_bayes/conformal --cores=4 --out=<26_general>/model_tests/coverage/lm_bayes
Rscript scripts/model/live/tests/coverage_summary.R <26_general>/model_tests/coverage/main <26_general>/model_tests/coverage/lm_bayes
```
