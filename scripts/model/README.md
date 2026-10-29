# Election-night models (`scripts/model/`)

| folder | what it holds | when it runs |
|---|---|---|
| `live/` | the election-night models: `run_night.R`, the model code in `R/`, medsl_sur's engine in `medsl/`, tests in `tests/`. Start with `live/README.md`. | on the night (and the targets pipeline's `model_status`) |
| `prep/` | builders for the models' inputs in `26_general/input_data/model_inputs/` and for `lookups/`: `prep_model_inputs.R <ST>` first, then the `make_*.R` scripts that need it (each header says what it reads and writes) | before the election, when inputs change |
| `lookups/` | committed reference tables: vote-mode and party maps, mode scope per state, reporting levels, precinct and county identity tables (`lookups/README.md`) | read by `live/` and `prep/` |
| `docs/` | `CONTRACT.md` (the input data contract) and the model specs (`.tex` / `.pdf`) | reference |
| `modeling-cory/` | Cory McCartan's tx-prim-26 and NYC code, kept as reference for the models built on `enightmodels` | reference only; not sourced |

Run every script from the `cbs-enr` folder.
