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

### 1. medsl_sur is still the outlier -- DONE (2026-10-27)
Done:
- `fit_medsl()` returns `model_result()` (via `draws_result()`), sets `conf` there, keeps
  `prior_only` on the result, and returns `prec_rows()` precinct rows.
- Pre-fork set-up is one hook, `MODEL_SETUP` / `setup_model()` (`R/run.R`), called by
  `run_jobs()` and the test runners for every model; no runner or `output.R` checks for
  "medsl_sur" to set it up. Still model-specific, on purpose: `predict_one()` passes
  `cbs_county` only to medsl_sur (no other fit takes it) and `METHOD_ONLY_MODELS`.
- Intervals: `medsl_sur/conformal` (Cory's approximate conformal, from `fit_stage()`'s new
  `cell_votes` slot in `scripts/model/model_turnout.R`: per-cell final votes and fitted
  shares). Default `medsl_sur` unchanged (`compare_runs.R`: 0 differences).

Not done, and why:
- **conf_seq.** `conf_seq_bounds()` needs prior draws of each party's votes and turnout.
  `model_turnout.R`'s `prior_draws()` returns only the Dem share of all votes, its spreads
  (`sd_sys_*`, `sd_idio_*`) were tuned for that one quantity, and it draws from the
  unshifted priors, not model_hybrid.R's re-centred ones. Adding it means writing a new
  prior predictive in Young-Kyung's file and choosing its spreads, which is a modelling
  decision, not a refactor.
- **Demographic specs.** The model's covariates are `COVS_TURN` / `COVS_SHARE` / `COVS_TP`
  in `model_turnout.R`: global constants read from the environment once per state set-up,
  naming numeric columns of its own `covs_long` frame, which it standardizes. A spec would
  have to (a) become part of the set-up cache key, so each spec costs its own set-up and
  memory; (b) turn strata into dummy columns, since the model has no grouping terms but its
  county random effect, so `(1 | geostratum)` cannot be written as Cory specifies it; and
  (c) decide whether the spec replaces Young-Kyung's covariates (turnout22/24, age, male,
  YouGov race predictions) or adds to them. Replacing them makes a different model; adding
  43 tpsob columns to each per-mode equation of a SUR with `MIN_FIT_OBS` gates risks the
  overfitting the file already warns about for the third-party equation. Ask Young-Kyung
  before trying.

### 2. One input signature -- DONE (83dd36f8)
`fit_swing(cells, returns, ...)`; no swing special case in `run_race()`. compare_runs.R: 0.

Original item:
`fit_swing()` takes `cell_counts()`; every other model takes returns. Make it
`fit_swing(cells, returns, demo, interval, ...)` like the rest, and drop the special case
(and the `state =` / `returns =` arguments) in `run_race()` (`R/run.R`). No estimate change.

### 3. One precinct schema -- DONE (aa2a3b4a)
`PREC_COLS`, `prec_rows()`, `votes_precincts()` in `R/results.R`; `to_prec_data()` has no branches. medsl_sur's rows go through `prec_rows()` as they are (item 1 can return them in that shape directly). compare_runs.R: 0, tables identical().

Original item:
Each model's `cells` slot has a different shape: swing gives one row per cell with dem /
rep / tot, ratio_strata one row per precinct, pois_bayes and lm_bayes `draws_precincts()`'s
columns, medsl_sur its own. `to_prec_data()` (`R/output.R`) has one branch per shape. Have
every model return `PREC_COLS`-shaped precinct rows (one helper in `R/results.R`), and
delete the branches. No estimate change; the precinct file's columns must stay as they are
(CONTRACT.md section 15.3).

### 4. One rule for splitting votes among candidates -- DONE (8066387d)
`cand_split()` (`R/results.R`) used by `cand_from_party()`, `to_pred_data()` and `state_pred_data()`; `past_share()` uses Cory's equal split. Only ratio_strata moves: tx_base gov 5e-10 votes (noise); nc_base sen up to +1,040 votes per candidate, Dem share of all votes -6e-5 (the old rule gave a race with no write-in line only 85% of the third-party baseline). `add_party_votes()` (`R/virtual.R`) still splits third-party votes by their predictions.

Original item:
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

### 5. Statewide intervals for conf_seq and conformal -- DONE (f152ccf1)
`project_rest_bounds()` (`R/counties.R`), used by `state_pred_data()` and `add_virtual()`. Defaults unchanged; the 6 bound variants now have statewide intervals (`..._plus_rest`); NC virtual units' scraped bounds in those variants move up to 665 votes at 21:15.

Original item:
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

### 7. One place for draws and seeds -- DONE (e32227c8)
`DRAWS`, `SEED`, `cap_draws()` in `R/results.R`. Left: swing's `boot = 100` (500 would change its intervals and cost 5x); `MEDSL_DRAWS`, `MEDSL_MAX_PRECINCT_DRAWS` and medsl's `seed =` are in `R/model_medsl.R`, outside this item's files: still to switch to `DRAWS`, `SEED`, `cap_draws()` (DRAWS matches model_turnout.R's 500, so no estimate change expected). compare_runs.R: 0.

Original item:
Today: `POIS_BAYES_DRAWS`, `LM_BAYES_DRAWS` (+ `LM_BAYES_MAX_CELL_DRAWS`), `MEDSL_DRAWS`
(+ `MEDSL_MAX_PRECINCT_DRAWS`), swing's `boot`, and a `seed` argument on each model. Make
one `DRAWS` and one `SEED` (in `R/results.R` or `R/run.R`), keeping the memory caps. No
estimate change if the values stay the same.

### 8. Cory's style of run list -- DONE (46465590)
`RUN` (what runs) and `MODELS` (implemented, validated against) in `R/run.R`. `RUN` is a plain vector: its entries are the published labels, so names would add nothing. compare_runs.R: 0.

Original item:
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
