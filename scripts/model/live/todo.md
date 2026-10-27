# Election-night models: simplification to-do

Written 2026-10-27, after the model renames (swing, ratio_strata, pois_bayes, lm_bayes,
medsl_sur), the demographic specs (`R/demos.R`) and the interval options (`R/results.R`).
Read `README.md` in this folder first: it describes the model names
(`<model>[/<spec>][/<interval>]`), the specs and the intervals.

**Goal.** Bring the live models back towards Cory's tx-prim-26 and NYC code: few files, one
way of doing each thing, readable at a glance. Most items below should change **no
estimates**; the exceptions say so.

## Before starting

- The renames, specs and intervals are committed; start from that commit.
- **Proof of "no change":** `tests/compare_runs.R`. Run `save` before your change and again
  after, then `compare`. Every difference must be 0 unless the item says estimates move.
  ```
  Rscript scripts/model/live/tests/compare_runs.R save /tmp/before.rds swing,ratio_strata,pois_bayes,lm_bayes,medsl_sur
  # ... make the change ...
  Rscript scripts/model/live/tests/compare_runs.R save /tmp/after.rds swing,ratio_strata,pois_bayes,lm_bayes,medsl_sur
  Rscript scripts/model/live/tests/compare_runs.R compare /tmp/before.rds /tmp/after.rds
  ```
  pois_bayes and lm_bayes draw random numbers: keep the order of random calls the same, or
  the comparison shows seed-sized differences (about 2e-5 in the two-party share).
- **Whole-path check:** `Rscript scripts/model/live/run_night.R --sim=tx_base --at="2026-11-03 22:00" --cores=6`
  (writes to `26_general/model_tests/run_night_sim/`, never `model_results/`). Expect every
  model `ok` except medsl_sur (see item 10).
- Run everything from the `cbs-enr` folder. R packages come from renv; `bases` (CRAN) is in
  the lockfile.
- `git commit` needs Mason's OK; `git push` always does.

## Items

### 1. medsl_sur is still the outlier
`R/model_medsl.R` builds its own result list instead of `model_result()` (`R/results.R`),
uses its own covariates (`yg_*`), takes no demographic spec and only the `credible`
interval, and needs its own set-up and caching (`medsl_setup()`, `fit_medsl_setup_for()`,
plus special cases in `run_jobs()`, `run_night.R`, `tests/sim_night.R` and
`tests/time_models.R` so the set-up happens before forking). It is the main reason
`output.R` and the runners still check model names.
- Return `model_result(...)` (keep `prior_only`), and set `conf` there.
- Bring it in line with the others where it fits: demographic specs, other intervals,
  simpler set-up. **Mason has cleared editing Young-Kyung's files**
  (`scripts/model/model_turnout.R`, `scripts/model/model_hybrid.R`) as needed. Keep
  `tests/compare_runs.R` at zero for the default `medsl_sur` unless a change is meant to
  move it, and say in the commit what moved and why.

### 2. One input signature
`fit_swing()` takes `cell_counts()`; every other model takes returns. Make it
`fit_swing(cells, returns, demo, interval, ...)` like the rest, and drop the special case
(and the `state =` / `returns =` arguments) in `run_race()` (`R/run.R`). No estimate change.

### 3. One precinct schema
Each model's `cells` slot has a different shape: swing gives one row per cell with dem /
rep / tot, ratio_strata one row per precinct, pois_bayes and lm_bayes `draws_precincts()`'s
columns, medsl_sur its own. `to_prec_data()` (`R/output.R`) has one branch per shape. Have
every model return `PREC_COLS`-shaped precinct rows (one helper in `R/results.R`), and
delete the branches. No estimate change; the precinct file's columns must stay as they are
(CONTRACT.md section 15.3).

### 4. One rule for splitting votes among candidates
ratio_strata's `past_share()` (`R/model_ratio_strata.R`) splits the third-party baseline
85 / 15 between named minor candidates and the write-in line, per cell. The other models
split their third-party total by counted votes, and that logic is written twice:
`to_pred_data()`'s draws branch (`R/output.R`) and `cand_from_party()` (`R/results.R`). Keep
one function and use it in both places.

**Decided (Mason, 2026-10-27): drop the 85 / 15 split and use Cory's rule.** In tx-prim-26,
`run_model_cty()` gives each candidate the party's expected votes divided by the number of
candidates (`vctrs::vec_unique_count(pick(candidate, party))`), and `model_prec_pois()`
does the same (`est / n_cand`): an equal split. For a general election, `past_share()` should
give each party's baseline share equally to that party's candidates, and the third-party
share equally to all third-party candidates, the write-in line included. This **changes
ratio_strata's third-party (and so its total) estimates**; expect `compare_runs.R` to show
differences for ratio_strata only, and report them in the commit. Update README's "Cory's
third-party split" note.

### 5. Statewide intervals for conf_seq and conformal
Fits with bounds instead of draws (`fit$pred`) get a point and NA `model_conf` in the
statewide rows (`state_pred_data()`, `R/counties.R`), and `add_virtual()` (`R/virtual.R`)
moves their bounds only roughly. Carry the bounds through `project_rest()` so every interval
type gets a statewide interval (`model_conf` ending `_plus_rest`). Changes statewide output
for those variants only; defaults unchanged. Do this after items 2-4 (same files).

### 6. Interval calibration (correctness, not refactoring)
On tx_base (TX governor, 61-80% counted) swing's bootstrap and pois_bayes's credible
intervals were 0.4-0.7 points wide and missed the truth from 22:00 on; every conf_seq and
conformal interval held it (widths in `README.md`). Run `tests/sim_night.R` over several
scenarios in `26_sim/` with each model x interval, e.g.
`--models=swing,swing/conformal,pois_bayes,pois_bayes/conf_seq,pois_bayes/conformal,ratio_strata/conformal,lm_bayes,lm_bayes/conformal`,
and report coverage and width by model, interval and share counted. Recommend each model's
default interval to Mason; **don't change `MODEL_INTERVALS` defaults without his OK** (it
changes what the dashboard shows). lm_bayes takes 2-4 minutes per fit with 7 in parallel;
budget for it or leave it out.

### 7. One place for draws and seeds
Today: `POIS_BAYES_DRAWS`, `LM_BAYES_DRAWS` (+ `LM_BAYES_MAX_CELL_DRAWS`), `MEDSL_DRAWS`
(+ `MEDSL_MAX_PRECINCT_DRAWS`), swing's `boot`, and a `seed` argument on each model. Make
one `DRAWS` and one `SEED` (in `R/results.R` or `R/run.R`), keeping the memory caps. No
estimate change if the values stay the same.

### 8. Cory's style of run list
Replace `MODELS` / `MODELS_DEFAULT` (`R/run.R`) with one named list of what runs on the
night, as in Cory's tx-prim-26 `specs`, e.g.
`RUN = c(swing = "swing", ratio = "ratio_strata", pois = "pois_bayes", sur = "medsl_sur")`,
plus the list of implemented models that `check_models()` validates against. `--models=`
keeps working. No estimate change.

### 9. Remove retired code
Once Mason has seen pois_bayes/conformal on the sims (item 6): delete `R/model_cpois.R`,
`DEMO_DEFAULT$cpois` (`R/demos.R`) and its README row, and the "LOCAL CHANGE" block in
`scripts/model/modeling-cory/tx-prim-26/R/prec_model.R` (it only fed model_cpois.R). Also
whether `fit_medsl()` on its own (formerly model_medsl) should go: ask Mason and
Young-Kyung. Ask before deleting.

### 10. Simulated CBS feed with a vote-method split
`tests/sim_statewide.R` writes the simulated CBS county feed without a `method` column
(the real `cbs_county_pull.R` writes schema_version 2, with it). medsl_sur's county step
(`county_targets()` in `scripts/model/model_hybrid.R`) needs it, so medsl_sur errors on every
simulated night (`CBS county snapshot is missing method`) and its statewide rows have never
been tested. Make the simulated feed match schema_version 2: the columns in
`county_targets()`'s `need` list, split by method from the sim truth.

## Suggested split across agents

Three agents, each owning separate files, so none waits on or overwrites another.

| agent | items | files it edits | when |
|---|---|---|---|
| **A: refactor** | 2, 3, 4 (the shared function only), 7, 8, then 5 | `R/run.R`, `R/output.R`, `R/results.R`, `R/counties.R`, `R/virtual.R`, the `R/model_*.R` files except `model_medsl.R`, `README.md` | now; one agent, because these items all touch `run.R` / `output.R` / `results.R` |
| **B: evaluation** | 10, then 6 | `tests/sim_statewide.R`, `tests/sim_night.R` only; writes results to `26_general/model_tests/` | now, in its own git worktree off the commit above, so A's edits can't break a long sim run halfway |
| **C: medsl_sur** | 1 | `R/model_medsl.R`, `scripts/model/model_turnout.R`, `scripts/model/model_hybrid.R` | after A finishes item 3 (both touch the precinct output and `run_jobs()`) |

Item 9 is a few minutes of work for whoever finishes last, after Mason has read B's
coverage report. Still needing Mason's decision: 6 (default intervals) and 9 (deletions). Decided: 4 (Cory's
equal split) and 1 (Young-Kyung's files may be edited).
