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
Rscript scripts/model/live/run_night.R --returns=<scraped returns file> --candidates=<candidates file> --every=5
```
Every 5 minutes: pulls fresh CBS county results for each race in `priors.csv`, runs every
model on every race, and writes `26_general/model_results/predictions_latest.csv` (scraped
counties, `geo = "scraped"`, and statewide, `geo = "all"`) and `status_latest.csv`. Drop
`--every` for a single refresh. To try the whole path on a simulated night with a simulated
CBS feed (writes to `model_tests/run_night_sim/`, never to `model_results/`):
```
Rscript scripts/model/live/run_night.R --sim=tx_base --at="2026-11-03 22:00"
```

## Output for the dashboard

`predict_all(cells, returns)` runs every race x model and returns `$pred`, in enightmodels'
tidy prediction format (the columns in `enightmodels::COLS_PRED`: one row per race x model
x candidate x `pred_type`, with `pred_type` "vote" and "share", `rep`, `pred`, `pred_min`,
`pred_max`, `pct_rep`, plus `p_win`), and `$status`, one row per race x model saying "ok",
"no_votes" or "error: ...". `write_predictions(res)` writes both to `26_general/model_results/` in the shared
Dropbox, as timestamped CSVs plus `predictions_latest.csv` / `status_latest.csv`. Test
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
| `R/model_swing.R` | **model_simpleswing**: `fit_swing()` (math written out at the top of the file) |
| `R/model_cory.R` | **model_cory**: `fit_cory()`, a wrapper on `enightmodels::model_legacy()` |
| `R/model_medsl.R` | **model_medsl**: `fit_medsl()`, which runs Young-Kyung's `scripts/model/model_turnout.R` from its own file (see below) |
| `R/run.R` | `run_race()`, `run_all()`, and the list of wired models |
| `R/output.R` | `predict_all()` and `write_predictions()`: results in enightmodels' prediction format for the dashboard |
| `R/counties.R` | statewide rows: scraped counties from the models + CBS county results for the rest |
| `R/races.R` | the races we cover, from `26_general/input_data/priors.csv`: `races_from_priors()`, `cbs_for_races()` |
| `tests/sim_night.R` | runs the models on one of Ethan's simulated nights through `run_all()` |
| `tests/check_against_original.R` | confirms the moved models reproduce `scripts/model/tests/test_sim_night.R` |
| `tests/time_models.R` | how long each model takes per race on this computer, one at a time and all at once in parallel (as `run_night.R` runs them) |
| `tests/sim_statewide.R` | a simulated night at state level: Ethan's sim for the scraped counties + a simulated CBS feed for the rest, scored against the statewide truth |

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
| swing | `R/model_swing.R` | moved from `tests/test_sim_night.R` section C; output checked identical |
| cory | `R/model_cory.R` | moved from `tests/test_sim_night.R` section D; output checked identical at the 7 calibration times on tx_base |
| medsl | `R/model_medsl.R` | runs `scripts/model/model_turnout.R` itself, not a copy: its sections 1-2 and its functions are evaluated from that file each time the models load for a state (once per state per session), the way `tests/test_sim_night.R` section A runs it, so Young-Kyung's changes there are picked up with no edit here. Its two file reads are replaced by frames built from `{ST}_precinct.parquet` and `{ST}_precinct_mode_history.parquet`; mode offsets default to zero (`MODE_OFFSET_SOURCE=zero`). Its other switches still work as environment variables. Blank YouGov race predictions (some MI and CA precincts) are filled with the county mean, noted in `settings`. If `model_turnout.R`'s section markers or its `covs_long` / `mode_hist` lines change, `fit_medsl()` stops with a message saying so. |

The originals in `scripts/model/` and `scripts/model/tests/` are untouched. Speed, one race per refresh on a slow 2-core test server: model_simpleswing 4-7 s, model_medsl 3-15 s, model_cory 1-18 s in AK, FL, MI, NC and TX; California (23,685 precincts) takes model_cory about 58 s and needs `MEDSL_DRAWS=200` or less to fit model_medsl in memory. `run_night.R` runs every race x model at once on all cores but one (`--cores=`), so a refresh takes about as long as its slowest fit. Time it on your machine with `tests/time_models.R`.

## Before this runs live

- **Live ids.** `returns_from_live()` follows CONTRACT.md section 8 but has not met a real
  feed: `geo` must already be reconciled to the precinct spine (TX live ids joined at 0.8%
  as of 2026-10-18) and `method` already canonical (`../lookups/vote_mode_map.csv`).
- **"Complete" means different things in the sims and live.** The sims say when each
  precinct x method cell is fully counted; a live feed only gives the precinct's `stage`,
  and stage 6 can take days (CONTRACT.md section 8). `returns_from_live()` marks a cell
  complete only at stage 6, so on election night the swing model's turnout equation
  (fit on complete cells only) will have few or no cells and fall back to expected
  turnout. model_medsl uses turnout only from complete cells too, so the same applies.
  Sim results therefore overstate how much these models learn about turnout live. Tested: with the completeness flags made equal, the live and sim paths give
  identical estimates.
- **Cory's third-party split.** `past` gives write-ins 15% and every other third-party
  candidate 85% of the third-party baseline, as in Ethan's sims. Two minor candidates
  would each get 85%. The write-in candidate must be named exactly `write-in`.
- **Races with no vote-method split.** No method split is ever assumed. If a state's vote-method history has no votes for one of the methods (MI: no early in-person in any L2 year), `build_cells()` gives one cell per precinct and the state is modeled without method; likewise if any of a race's returns lack a usable method (precinct totals only, `adv`, or missing), the race is modeled without method: model_simpleswing on precinct totals (county fixed effects only), model_cory with `model_legacy_config(by_method = FALSE)`, and model_medsl is skipped (status says why), since its equations are by method. `abs` is read as mail and `prov` as Election Day. Decided per race at every refresh (`shape_for_method()` in `R/run.R`); `settings` in the status file says when a fit was without method.
- **Counties the feed doesn't cover.** The models fit only the counties with at least one precinct in the race's returns (`feed_cells()` in `R/output.R`); every other county, including counties in our precinct inputs whose live ids we don't have, comes from CBS's county results in the statewide rows. As of 2026-10-22 the crosswalks have live ids for FL 18 of 31 counties, MI 10 of 31, TX 4 of 32 and CA none.
- **County history for CA and NY.** votehub has no county results for CA or NY, so their statewide rows read `26_general/input_data/county_history/{ST}_county_history.csv` (2024 presidential by county), built by `scripts/model/make_county_history.R` (outside live/; run it again only to rebuild those files or add a state).
- **Statewide rows** come from `R/counties.R` (scraped counties from each model + CBS county
  results for the rest), not from `scripts/model/tests/statewide_tx.R`, which is not moved.
