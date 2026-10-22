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
| `R/cbs_county_pull.R` | `pull_cbs_race()`: pulls CBS county results for a race into `26_general/cbs_api/` (called by `cbs_for_races()` at every model run); also runs from the command line for one race, several, or `--all` |
| `R/paths.R` | finds `26_general` on your machine; parquet read/write; `cred()` interval |
| `R/cells.R` | `build_cells()`: one row per precinct x method with `dem2`, `oth`, `exp_votes` |
| `R/returns.R` | the standard returns shape; `returns_from_sim()`, `returns_from_live()`, `cell_counts()` |
| `R/model_swing.R` | **model_simpleswing**: `fit_swing()` (math written out at the top of the file) |
| `R/model_cory.R` | **model_cory**: `fit_cory()`, a wrapper on `enightmodels::model_legacy()` |
| `R/model_share.R` | **model_y**: an untouched copy of `model_turnout.R`, not wired in yet (see below) |
| `R/run.R` | `run_race()`, `run_all()`, and the list of wired models |
| `R/output.R` | `predict_all()` and `write_predictions()`: results in enightmodels' prediction format for the dashboard |
| `R/counties.R` | statewide rows: scraped counties from the models + CBS county results for the rest |
| `R/races.R` | the races we cover, from `26_general/input_data/priors.csv`: `races_from_priors()`, `cbs_for_races()` |
| `tests/sim_night.R` | runs the models on one of Ethan's simulated nights through `run_all()` |
| `tests/check_against_original.R` | confirms the moved models reproduce `scripts/model/tests/test_sim_night.R` |

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
| y | `R/model_share.R` | copy of `model_turnout.R` at commit `dc7a4d4`. Young-Kyung is still working on the original, which stays the working version. Once it settles, re-copy it and wrap it as `fit_y(cells, returns)`. |

The originals in `scripts/model/` and `scripts/model/tests/` are untouched.

## Before this runs live

- **Live ids.** `returns_from_live()` follows CONTRACT.md section 8 but has not met a real
  feed: `geo` must already be reconciled to the precinct spine (TX live ids joined at 0.8%
  as of 2026-10-18) and `method` already canonical (`../lookups/vote_mode_map.csv`).
- **"Complete" means different things in the sims and live.** The sims say when each
  precinct x method cell is fully counted; a live feed only gives the precinct's `stage`,
  and stage 6 can take days (CONTRACT.md section 8). `returns_from_live()` marks a cell
  complete only at stage 6, so on election night the swing model's turnout equation
  (fit on complete cells only) will have few or no cells and fall back to expected
  turnout. Sim results therefore overstate how much the swing model learns about turnout
  live. Tested: with the completeness flags made equal, the live and sim paths give
  identical estimates.
- **Cory's third-party split.** `past` gives write-ins 15% and every other third-party
  candidate 85% of the third-party baseline, as in Ethan's sims. Two minor candidates
  would each get 85%. The write-in candidate must be named exactly `write-in`.
- **Statewide extension.** `scripts/model/tests/statewide_tx.R` (32 counties to the full
  state) is not moved; the models here estimate the precincts in `cells` only.
