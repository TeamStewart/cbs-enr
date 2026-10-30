# Election-night models (`scripts/model/live/`)

One place for every model that runs on election night, each callable the same way on any
race. Run everything from the `cbs-enr` folder.

```r
source("scripts/model/live/load.R")

cells   = build_cells("TX")                      # precinct x method frame + expected votes
returns = returns_from_live(live, candidates, state = "TX", election = "2026-11-03")
                                                  # or returns_from_sim(...) for a test night
run_race("swing", cells, filter(returns, race_id == "TX_gov"))   # one race, one model
run_all(cells, returns)                           # every race x every model, one row each
```

## On election night

```
Rscript scripts/model/live/run_night.R --every=5
```
reads the scrapers' clean files, `26_general/{ST}/clean/*_latest.parquet` (`returns_from_clean()` in `R/returns.R`, which matches their precinct ids to our inputs). `--returns=<file> --candidates=<file>` reads a CONTRACT.md section 8 file instead.
Every 5 minutes: pulls fresh CBS county results for each race in `priors.csv`, runs every
model switched on in `26_general/model_config.csv` on every race, and writes `26_general/model_results/predictions_latest.csv` (scraped
counties, `geo = "scraped"`, and statewide, `geo = "all"`), `turnout_latest.csv` and `status_latest.csv`. Drop
`--every` for a single refresh. To try the whole path on a simulated night with a simulated
CBS feed (writes to `model_tests/run_night_sim/`, never to `model_results/`):
```
Rscript scripts/model/live/run_night.R --sim=tx_base --at="2026-11-03 22:00"
```

**Redistricting charts.** After every refresh, `run_night.R` also starts
`scripts/model/reports/redistricting_compare.R --live` in the background (`--no-redistricting`
turns it off): tonight's House vote, counted so far, by new district (actual) and old district
(estimated), for NC, FL, TX, OH and CA. It runs separately, so it never holds up or breaks the
models. **The dashboard reads the charts from `26_general/model_results/redistricting/`:**
`redistricting_dumbbell_{ST}_house_latest.png` and `redistricting_seats_{ST}_house_latest.png` (one each per state),
`redistricting_districts_house_latest.csv`, and the old / new district maps
`redistricting_map_{ST}_old_house_latest.png` / `_new_house_latest.png` (with
`redistricting_map_{ST}_house_latest.html` to flip between them), replaced at every run; time-stamped copies go to
`history/`, and the run's log to `live.log`.

**Before go time (once, so the first results chart straight away):**
1. Install fourthirtyfive: `Rscript -e 'remotes::install_github("kuriwaki/fourthirtyfive")'`
   (sf, ggplot2 and cbsapi with the CBS key are already needed for the models).
2. CA's 2024 precinct files from the Statewide Database in `26_general/input_data/swdb_g24/`
   (`srprec_state_g24_v01_shp.zip`, `state_g24_sov_data_by_g24_srprec.zip`; in place as of 2026-10-30).
   OH's official 2024 precinct returns from MEDSL at `26_general/input_data/medsl24/oh24.zip`
   (github.com/MEDSL/2024-elections-official; in place as of 2026-10-30) -- VoteHub's OH results
   don't match its OH precinct map.
3. `Rscript scripts/model/reports/redistricting_compare.R` -- all five states, ~15 minutes.
   Writes the county x district weights and map geometry the night run needs to
   `26_general/input_data/redistricting/`, and the 2024 baseline charts and maps (`*_2024pres.*`).
   Without the weights `run_night.R` prints a warning and skips the charts.
4. Dry run on CBS's test feed: `Rscript scripts/model/reports/redistricting_compare.R --live --env=test`,
   then check that the `*_house_latest` files appear in `model_results/redistricting/`.

## Output for the dashboard

`predict_all(cells, returns)` runs every race x model and returns `$pred`, in enightmodels'
tidy prediction format (the columns in `enightmodels::COLS_PRED`: one row per race x model
x candidate x `pred_type`, with `pred_type` "vote" and "share", `rep`, `pred`, `pred_min`,
`pred_max`, `pct_rep`, plus `p_win`), `$turnout`, the race total with its interval (one row
per race x model x geo; CONTRACT.md section 15.4), and `$status`, one row per race x model saying "ok",
"no_votes" or "error: ...". `write_predictions(res)` writes all three to `26_general/model_results/` in the shared
Dropbox, as timestamped CSVs plus `predictions_latest.csv` / `turnout_latest.csv` / `status_latest.csv`. Test
runs should pass another `dir` so they never mix with the night's results. Details at the top of `R/output.R`.
Every prediction row passes `enightmodels::check_pred_data()`.

**Statewide.** Pass `cbs = list(<race_id> = read_cbs_latest(...))` and
`base = county_baseline(state)` to `predict_all()` and each race x model also gets rows with
`geo = "all"`: the scraped counties from the model (precinct detail kept) plus every other
county from CBS's county results (`scripts/model/live/R/cbs_county_pull.R` writes them to
`26_general/cbs_api/`), with each county's uncounted vote projected from CBS's share of
expected vote, its 2022 turnout and 2024 lean, and tonight's swing. The model-only rows have
`geo = "scraped"`. Method and the interval terms are at the top of `R/counties.R`.

CBS county results for every race in `priors.csv`, from the `cbs-enr` folder:
`Rscript scripts/model/live/R/cbs_county_pull.R --all` (test feed; add `--live` on the night). It
prints which races CBS returned and which it didn't. On the night the pull happens inside each
model run: `cbs_for_races(races)` pulls every race fresh and falls back to the last saved
snapshot for any race whose pull fails.

## Layout

| file | what it holds |
|---|---|
| `load.R` | loads packages and the files in `R/`; nothing runs at load time |
| `run_night.R` | the election-night run: CBS pull + every model on every race, scraped and statewide, written for the dashboard |
| `R/cbs_county_pull.R` | `pull_cbs_race()`: pulls CBS county results for a race into `26_general/cbs_api/` (called by `cbs_for_races()` at every model run); also runs from the command line for one race, several, or `--all` |
| `R/paths.R` | finds `26_general` on your machine; parquet read/write; `cred()` interval |
| `R/cells.R` | `build_cells()`: one row per precinct x method with `dem2`, `oth`, `exp_votes` |
| `R/returns.R` | the standard returns shape; `returns_from_sim()`, `returns_from_live()`, `cell_counts()` |
| `R/demos.R` | the demographic specs a model can take: Cory's `model_specs` from tx-prim-26 in our column names, `DEMO_DEFAULT`, and the shared precinct covariates (`demo_covs()`) |
| `R/results.R` | the result list every model returns (`model_result()`), and the intervals: `MODEL_INTERVALS`, conformal (`conformal_party()`), confidence sequence (`conf_seq_bounds()`) |
| `R/model_swing.R` | **swing**: `fit_swing()` (math written out at the top of the file) |
| `R/model_ratio_strata.R` | **ratio_strata**: `fit_ratio_strata()`, Cory's `enightmodels::model_legacy()` on precinct x method cells |
| `R/model_pois_bayes.R` | **pois_bayes**: `fit_pois_bayes()`, a precinct Poisson model on `enightmodels::pois_bayes()` (turnout on counted-out cells, party shares on reporting cells, posterior predictive draws) |
| `R/model_lm_bayes.R` | **lm_bayes**: `fit_lm_bayes()`, Cory's `enightmodels::forecast_prec()` set up as in his NYC run (`../modeling-cory/nyc/`); out of the default run on cost |
| `R/model_medsl.R` | **medsl_sur**: `fit_medsl_sur()`; builds the model's inputs and loads `medsl/` once per state (`medsl_setup()`) |
| `medsl/model_turnout.R` | medsl_sur's engine: the precinct x method turnout / third-party / two-party model (`fit_stage()`, `prior_totals()`) |
| `medsl/model_hybrid.R` | re-centres model_turnout.R's priors on tonight's returns and CBS county totals (`fit_stage_hybrid()`) |
| `medsl/model_core.R` | fitting primitives model_turnout.R uses |
| `R/run.R` | `run_race()`, `run_all()`; `RUN`, what runs on the night, and `MODELS`, every implemented model |
| `R/config.R` | `26_general/model_config.csv`: every model / spec / interval name, and `run` = TRUE for the ones `run_night.R` runs (and so the targets pipeline, `model_status` in `_targets.R`; `MODEL_SIM` in `scripts/util/globals.R` points it at a sim). Rebuild with `make_model_config.R`, which keeps each row's `run` and `note` |
| `R/output.R` | `predict_all()` and `write_predictions()`: results in enightmodels' prediction format for the dashboard |
| `R/counties.R` | statewide rows: scraped counties from the models + CBS county results for the rest |
| `R/virtual.R` | NC's county-level "virtual precinct" batches for every model but medsl_sur: `fit_model()`, `virtual_split()`, `add_virtual()` |
| `R/races.R` | the races we cover, from `26_general/input_data/priors.csv`: `races_from_priors()`, `cbs_for_races()` |
| `tests/sim_night.R` | runs the models on one of Ethan's simulated nights, every checkpoint x race x model in parallel (`--cores=`, default half the cores minus one) |
| `tests/time_models.R` | how long each model takes per race on this computer, one at a time and all at once in parallel (as `run_night.R` runs them) |
| `tests/sim_statewide.R` | a simulated night at state level: Ethan's sim for the scraped counties + a simulated CBS feed for the rest, scored against the statewide truth |
| `tests/compare_runs.R` | checks a refactor changes no estimates: `save` before and after, then `compare` |
| `tests/check_outputs.R`, `tests/time_refresh.R`, `tests/coverage_summary.R` | output checks, refresh timing, and the interval coverage report (`tests/coverage_report.md`) |

## Model names, demographic specs and intervals

A model name is `<model>[/<spec>][/<interval>]`, and that name is also the published `model`
label: `--models=swing,pois_bayes/strat_d/conformal`, or
`fit_model("lm_bayes/conf_seq", cells, returns)`. A plain name runs the model's default spec
and interval.

| model | what it is | specs | intervals (first = default) |
|---|---|---|---|
| `swing` | uniform swing, county x method fixed effects | all, as slopes / dummies (geostratum absorbed by county) | bootstrap, conformal |
| `ratio_strata` | Cory's legacy ratio model, ratios pooled within strata | `ratio`, `strat_g` (default), `strat_d`, `strat_dg` | none, conformal |
| `pois_bayes` | Bayesian Poisson precinct model (`pois_bayes()`) | all, as fixed effects; default `strat_dg_cov` | credible, conf_seq, conformal |
| `lm_bayes` | Cory's NYC forecaster (`forecast_prec()`) | all, added to NYC's `simple_pres` spec; default `ratio` | credible, conf_seq, conformal |
| `medsl_sur` | precinct x method SUR model (`medsl/`) | all: covariates after `log_n` in place of its own, strata as its random-effect grouping in place of county; default `medsl_cov` (its own design) | credible, conf_seq, conformal |

**Specs** (`R/demos.R`, Cory's tx-prim-26 `model_specs` plus `medsl_cov`):

| spec | terms |
|---|---|
| `ratio` | none |
| `strat_g` | (1 \| geostratum) |
| `strat_d` | (1 \| stratum_demo) |
| `strat_dg` | both strata |
| `strat_d_cov` | strat_d + dem2, dem2², ages 18-29 / 65+, white college / non-college |
| `strat_dg_cov` | strat_dg + the same |
| `tpsob` | 43-column thin-plate basis of those covariates plus Black and Hispanic (`bases::b_tpsob`, CRAN) |
| `tpsob_g` | tpsob + geostratum |
| `medsl_cov` | medsl_sur's own covariates: dem2, 2022 and 2024 turnout, ages 65+, male, YouGov Black / Hispanic (not one of Cory's) |

**Intervals** (`R/results.R`; `model_conf` in the output):

| interval | how | `model_conf` |
|---|---|---|
| bootstrap | swing's county bootstrap draws | `county_bootstrap_95` |
| credible | posterior draws, equal-tailed | `posterior_95` |
| conf_seq | `enightmodels::conf_seq()`: anytime-valid confidence sequence from prior vs posterior draws of the race totals | `conf_seq_95` |
| conformal | Cory's approximate split-conformal: residuals of counted vs fitted cell shares by party x method, applied to each cell's remaining votes and summed (conservative; turnout a point) | `approx_conform_95` |
| none | point only | NA |

conf_seq and conformal give bounds, not draws: no `p_win`. Their statewide rows carry the bounds to the rest of the state: each end of a party's interval projects the other counties at the scraped interval ends worst for that party, plus the rest-of-state noise quantiles (`project_rest_bounds()`, `R/counties.R`; NC virtual units the same way).
On tx_base (TX governor, 61-80% counted) the 95% two-party intervals were about: swing
bootstrap 0.4-0.7 points and pois_bayes credible 0.4-0.7 (both missed the truth at 22:00 or
later), lm_bayes credible 1.4; pois_bayes conf_seq 2-3, lm_bayes conf_seq 8; conformal 2-4
for swing and pois_bayes, 3-5.5 for ratio_strata and lm_bayes. All conf_seq and conformal
intervals held the truth.

## The interface every model follows

**In:** `cells` (from `build_cells()`) and standard returns for one race (from
`returns.R`: one row per precinct x method x candidate with `votes`, `stage`, `complete`).

**Out:** a list with `model`, `status`, `settings`, and

- `race`: one row: `est_twop`, `lo_twop`, `hi_twop`, `p_dem_win`, `est_turnout`,
  `lo_turnout`, `hi_turnout`, `raw_twop`, `counted_votes`, `expected_votes`, `obs_cells`,
  `complete_cells`
- `cells`: projections below the race level
- `draws`: simulation or bootstrap draws, where the model has them

`run_all()` records a model that errors on a race as `status = "error: ..."` and carries
on, so one bad race cannot stop the rest.

## Status of each model

| model | file | status |
|---|---|---|
| swing | `R/model_swing.R` | county x method fixed-effect swing, county bootstrap interval |
| ratio_strata | `R/model_ratio_strata.R` | `model_legacy()`'s two stages called directly so the precinct frame comes from the same fit; past = expected votes x baseline share |
| pois_bayes | `R/model_pois_bayes.R` | precinct Poisson on `enightmodels::pois_bayes()`, posterior predictive draws |
| lm_bayes | `R/model_lm_bayes.R` | NYC's `simple_pres` spec plus 0/1 method columns; draws from `forecast_summarize()`; out of the default run on cost |
| medsl_sur | `R/model_medsl.R` | `medsl/model_turnout.R` and `medsl/model_hybrid.R` sourced into one environment per state; `fit_stage_hybrid()` fits. The CBS county snapshot is passed on the night (county-constraint arm), none in the sims; arms switch off with `HY_LEVEL` / `HY_COMP` / `HY_SHARE` / `HY_COUNTY=0`. Mode offsets default to zero (`MODE_OFFSET_SOURCE=zero`); the model's other switches are environment variables. Blank YouGov race predictions (some MI and CA precincts) are filled with the county mean, noted in `settings`. |

Speed, one race per refresh on a slow 2-core test server: swing 4-7 s, medsl_sur 3-15 s, ratio_strata 1-18 s in AK, FL, MI, NC and TX; California (23,685 precincts) takes ratio_strata about 58 s; medsl_sur's draws are cut there to fit in memory (`MEDSL_MAX_PRECINCT_DRAWS`). `run_night.R` runs every race x model at once on all cores but one (`--cores=`), so a refresh takes about as long as its slowest fit. Time it on your machine with `tests/time_models.R`.

## Before this runs live

- **Live ids.** The default path, `returns_from_clean()`, matches the scrapers' geo ids to
  our precinct inputs. `--returns=` (`returns_from_live()`, `../docs/CONTRACT.md` section 8)
  expects `geo` already reconciled to the precinct spine and `method` already canonical
  (`../lookups/vote_mode_map.csv`).
- **"Complete" is never set live.** A live feed gives only the precinct's `stage`, and
  `get_data()` (`scripts/functions.R`) rewrites stage 6 to 5 unless `trust_stage`, which is
  FALSE everywhere, so `complete` is never TRUE on election night. Every model's turnout fit
  therefore uses a fallback once the flag is empty: a cell counts as counted out when its
  votes reach `COMPLETE_FRAC` (default 0.6) x its expected votes (`turnout_done()` in
  `R/returns.R` for swing, pois_bayes and ratio_strata's district adjustment;
  `complete_usable()` in `medsl/model_turnout.R` for medsl_sur). The feed's flag is used
  instead as soon as it is TRUE for any cell, or always with `COMPLETE_TRUST=1`. A cell is
  pinned at its count only on the real flag. The sims return feed-like rows by default
  (stage 6 -> 5, `complete` FALSE, `returns_from_sim()`), so sim runs test the fallback;
  `SIM_TRUST_COMPLETE=1` gives the sim's exact completion instead.
- **Splitting votes among candidates.** ratio_strata's `past` uses Cory's equal split
  (tx-prim-26 `run_model_cty()`): each party's baseline share goes equally to its candidates,
  and the third-party share equally to every third-party candidate, the write-in line
  included (`past_share()`). The other models split their party totals with `cand_split()`
  (`R/results.R`): equally within a party, third-party votes in proportion to their counted
  votes (equally until any are counted).
- **Races with no vote-method split.** No method split is ever assumed. If a state's vote-method history has no votes for one of the methods (MI: no early in-person in any L2 year), `build_cells()` gives one cell per precinct and the state is modeled without method; likewise if any of a race's returns lack a usable method (precinct totals only, `adv`, or missing), the race is modeled without method: swing on precinct totals (county fixed effects only), ratio_strata with `model_legacy_config(by_method = FALSE)`, lm_bayes on precinct totals (as in NYC), and medsl_sur is skipped (status says why), since its equations are by method. `abs` is read as mail and `prov` as Election Day. When only some counties' results lack a method split (a county scraper with precinct totals only, e.g. TX Hardin, MI Wayne, most of CA), medsl_sur is fit on the counties that do split and the others come from CBS's county results in the statewide rows (`METHOD_ONLY_MODELS`, `predict_one()`); `settings` says how many counties. Decided per race at every refresh (`shape_for_method()` in `R/run.R`); `settings` in the status file says when a fit was without method.
- **Virtual precincts (NC).** Votes a county reports in county-level batches instead of by precinct (NC's early-voting sites and absentee piles) match no precinct. For every model but medsl_sur, a county x method that was mostly reported this way in 2024 (`{ST}_virtual_history.parquet`) is taken out of the precinct fit and added back as one county-level unit: counted votes once the batch is in, projected from 2024 at tonight's swing until then (`R/virtual.R`). medsl_sur uses the same rows as county x mode observations (`bucket_counts()` in `R/returns.R`).
- **NC vote-method split.** `{ST}_precinct_mode_results.parquet` (`scripts/model/prep/make_mode_results.R`) gives every model the same split: each county's 2022 split (official county returns) with the 2024 precinct pattern within the county. Rerun it after rerunning `prep_model_inputs.R NC`.
- **Counties the feed doesn't cover.** The models fit only the counties with at least one precinct in the race's returns (`feed_cells()` in `R/output.R`); every other county, including counties in our precinct inputs whose live ids we don't have, comes from CBS's county results in the statewide rows. As of 2026-10-22 the crosswalks have live ids for FL 18 of 31 counties, MI 10 of 31, TX 4 of 32 and CA none.
- **Turnout (Ansolabehere et al. 2024, "Forecasting Turnout").** Two changes from that paper, 2026-10-29. (1) Every model's expected vote (`est_turnout` in `{ST}_precinct.parquet`) is the registration model: registrants x their 2022 turnout rate, calibrated to each county's actual 2022 vote (`prep/prep_model_inputs.R`; `EST_TURNOUT_SOURCE=vendor` restores the vendor-score version, kept as `est_turnout_vendor`). (2) In the statewide rows, a county CBS hasn't finished counting is projected with its congressional district's turnout so far, not one statewide ratio (`cd_turnout_offsets()` in `R/counties.R`), using `{ST}_county_cd.csv` from `prep/make_county_cd.R`; without that file the offsets are 0.
- **County history for CA and NY.** votehub has no county results for CA or NY, so their statewide rows read `26_general/input_data/county_history/{ST}_county_history.csv` (2024 presidential by county), built by `scripts/model/prep/make_county_history.R` (outside live/; run it again only to rebuild those files or add a state).
- **Statewide rows** come from `R/counties.R` (scraped counties from each model + CBS county
  results for the rest).

## Open items

- **Default intervals.** swing's bootstrap and pois_bayes's credible intervals were 0.4-0.7
  points wide on tx_base and missed the truth from 22:00 on; every conf_seq and conformal
  interval held it (`tests/coverage_report.md`). `MODEL_INTERVALS` defaults (`R/results.R`)
  are unchanged pending a decision.
- **Simulated CBS feed has no method split.** `tests/sim_statewide.R` writes the simulated
  CBS county feed without a `method` column, so medsl_sur's county-constraint arm
  (`county_targets()` in `medsl/model_hybrid.R`) is untested on simulated nights.
- **medsl_sur draws.** `MEDSL_DRAWS`, `MEDSL_MAX_PRECINCT_DRAWS` and its `seed` could move to
  `DRAWS`, `SEED` and `cap_draws()` (`R/results.R`); same values, so no estimate change.
