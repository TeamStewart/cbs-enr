# MEDSL pipeline

A second pipeline on the `medsl` branch: scrape and process only, for MEDSL rather than CBS.
No upload, no modeling, no Quarto render.

| | CBS | MEDSL |
|---|---|---|
| script / store | `_targets.R` / `_targets/` | `_targets_medsl.R` / `_targets_medsl/` |
| driver | `scripts/automation.R`, 5 min | `scripts/automation_medsl.R`, 15 min |
| scope | `build & updated & for_cbs` — 84 jurisdictions, 13 states | `build & updated` — 214, 32 states |
| output | `.../CBS-MIT Election Data/26_general/` | `.../medsl-enr/26_general/` |
| extra data | — | `networkenr`: 10 news networks, contest level |
| targets | ~336 | 663 |

## Running it

```bash
Rscript scripts/automation_medsl.R >> logs/medsl_console.log 2>&1
```

Google auth must be cached once first, since a non-interactive `Rscript` cannot open a browser:

```bash
Rscript -e 'options(rlang_interactive = TRUE); googledrive::drive_auth()'
```

For a one-off `tar_make()` by hand, set the same variables the driver sets:

```bash
TAR_PROJECT=medsl \
PATH_DROPBOX="$HOME/MIT Dropbox/Joseph Loffredo/medsl-enr" \
PATH_ENIGHTDATA="$HOME/Desktop/GitHub/enightdata" \
ENIGHTDATA_CONFIG="$HOME/.cache/medsl-enr-config" \
RETICULATE_PYTHON=".../renv/python/virtualenvs/renv-python-3.9/bin/python3" \
Rscript -e 'targets::tar_make()'
```

## On medsl.mit.edu

Every host-specific value in `automation_medsl.R` is an env var, so the server only sets them:

| Variable | Default (laptop) |
|---|---|
| `MEDSL_DROPBOX` | `~/MIT Dropbox/Joseph Loffredo/medsl-enr` |
| `MEDSL_ENIGHTDATA` | `~/Desktop/GitHub/enightdata` |
| `MEDSL_PYTHON` | the renv 3.9 venv; empty skips it |
| `MEDSL_CONFIG` | `~/.cache/medsl-enr-config` |
| `MEDSL_WORKERS` | `max(12, cores - 2)` |
| `MEDSL_ENIGHTDATA_BRANCH` | `medsl` |
| `MEDSL_NETWORKENR_BRANCH` | `main` |

`MEDSL_WORKERS` is unset by default and scales to the host, so the server needs it only to
override. The work is network-bound, so it can run well past the core count.

## Three things that will bite

**`PATH_DROPBOX` and `PATH_ENIGHTDATA` must be set together.** `scripts/util/globals.R` only
skips its username map when both are set; with one alone the map silently reassigns
`PATH_DROPBOX` to the CBS folder. `_targets_medsl.R` aborts rather than let that run.

**Python is load-bearing.** Clarity is the only platform needing reticulate and it is 54 of the
214 rows. With `RETICULATE_PYTHON` unset, discovery dies on whatever stale python is first on
`PATH` and never reaches `py_require()`.

**`write_local()` writes parquet only when the path ends in `.csv`.** A `.parquet` path with
`csv = FALSE` writes nothing and returns normally. `medsl_write()` in `scripts/medsl/lib.R`
handles this; follow it for any new write.

## Checks

```r
enightdata::get_all()   # path_dropbox must be the MEDSL folder, election_folder 26_general
targets::tar_read(net_report)        # one row per network: entries, errors, contests, candidates
targets::tar_meta(fields = "error") |> dplyr::filter(!is.na(error))
```

After a cycle, the CBS tree must show nothing from this run:

```bash
find "$HOME/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data/26_general" \
  -newermt "$(date -v-20M +%Y-%m-%dT%H:%M:%S)" -type f
```

## Notes

- `ELECTION_FOLDER` stays `26_general`. `enightdata/R/format_wide.R` slices positions 3–4 of it
  for the two-digit year.
- `enightdata` and `networkenr` are both in renv's `ignored.packages`; each host installs its
  own. Their Imports are pinned via `scripts/util/renv_*_deps.R`.
- `ENIGHTDATA_CONFIG` (added on enightdata's `medsl` branch) gives this pipeline its own config
  file. Without it, `set_all()` would write the MEDSL path into the single per-user config that
  the CBS pipeline's crew workers read.
- ABC and NBC return the current time as their timestamp, so those two network targets never
  cache and refetch every cycle. Expected, not a bug.
- The Hill's 2026 URLs are unconfirmed, so `net_data_hill` may return 0 contests until its pages
  publish. `error = "null"` keeps that from failing the run.
