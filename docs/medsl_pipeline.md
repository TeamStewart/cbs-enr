# MEDSL pipeline

A second pipeline on the `medsl` branch: scrape and process only, for MEDSL rather than CBS.
No upload, no modeling, no Quarto render.

| | CBS | MEDSL |
|---|---|---|
| script / store | `_targets.R` / `_targets/` | `_targets_medsl.R` / `_targets_medsl/` |
| driver | `scripts/automation.R`, 5 min | `scripts/automation_medsl.R`, 10 min |
| scope | `build & updated & for_cbs` — 84 jurisdictions, 13 states | `build & updated` — 218, 32 states |
| output | `.../CBS-MIT Election Data/26_general/` | `.../medsl-enr/26_general/` |
| extra data | — | `networkenr`: 10 news networks, contest level |
| targets | ~336 | 676 |

## Running it

```bash
cd /d/research/cbs-enr && source /home/agent/medsl-env.sh
nohup scripts/medsl/run_medsl.sh > /dev/null 2>&1 &
echo $! > /home/agent/medsl.pid
```

Stop it with `touch "$MEDSL_LOGDIR/STOP"` (finishes the cycle in flight) or
`kill $(cat /home/agent/medsl.pid)`, which also kills the driver via the supervisor's trap.

### Picking up code changes

`automation_medsl.R` runs `git pull --ff-only` as the first step of every cycle, so all three
files self-update, but they land differently:

| changed file | takes effect |
|---|---|
| `_targets_medsl.R`, `scripts/medsl/lib.R` | that same cycle — `tar_make()` re-reads them |
| `scripts/automation_medsl.R` | the driver exits 7 and `run_medsl.sh` starts the new one, before sleeping |
| `enightdata`, `networkenr` | their own GitHub step, within one cycle |

So a pushed fix is live within one cycle with nobody touching the server. `--ff-only` means a
checkout carrying local commits stops rather than merging blind; the step is wrapped, so a
failed pull leaves the cycle running on the current checkout.

`run_medsl.sh` restarts on exit 7 immediately and on any other exit after 60 seconds, so a
driver that fails at startup backs off instead of spinning.

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

On the server that is just:

```bash
cd /d/research/cbs-enr && source /home/agent/medsl-env.sh
export TAR_PROJECT=medsl CBS_AUTOMATION=1 RETICULATE_PYTHON=$MEDSL_PYTHON
export PATH_DROPBOX=$MEDSL_DROPBOX PATH_ENIGHTDATA=$MEDSL_ENIGHTDATA ENIGHTDATA_CONFIG=$MEDSL_CONFIG
Rscript -e 'targets::tar_make()'
```

`tar_make(names = )` uses tidyselect, which cannot see a variable from the calling scope — pass
the target names as a literal vector (`any_of(c("data_AK_NA", ...))`), not via a variable.

## On medsl.mit.edu

Every host-specific value in `automation_medsl.R` is an env var, so the server only sets them:

| Variable | Default (laptop) | On the server |
|---|---|---|
| `MEDSL_DROPBOX` | `~/MIT Dropbox/Joseph Loffredo/medsl-enr` | `/d/research/medsl-enr` |
| `MEDSL_ENIGHTDATA` | `~/Desktop/GitHub/enightdata` | `~/.cache/medsl-enr-enightdata` |
| `MEDSL_PYTHON` | the renv 3.9 venv; empty skips it | `~/medsl-venv/bin/python3` |
| `MEDSL_CONFIG` | `~/.cache/medsl-enr-config` | same |
| `MEDSL_LOGDIR` | `logs` | `/d/research/medsl-enr/26_general/logs` |
| `MEDSL_WORKERS` | `max(12, cores - 2)` | unset; 12 cores → 12 |
| `MEDSL_ENIGHTDATA_BRANCH` | `medsl` | same |
| `MEDSL_NETWORKENR_BRANCH` | `main` | same |

Live at `/home/agent/medsl-env.sh`, which also puts `/opt/R/4.6.1/bin` first on `PATH`. Run as
**`agent`**, which owns the other `/d/research/2026-*-scraper` dirs. `agent` cannot `sudo`, so
creating directories under `/d/research` and installing system packages needs someone else.

`MEDSL_LOGDIR` points inside `26_general` on purpose: the per-cycle logs then travel with the
data, so anyone who downloads the election folder can see what failed without asking.

### Server build notes

`renv.lock` pins R 4.6.1 and **Posit Package Manager has no binaries for R 4.6 on focal**, so
everything compiles from source. Install the subset this pipeline needs, never a bare
`renv::restore()` — the full lockfile is 328 packages including arrow, sf, terra and V8, none of
which this pipeline touches. The 35-package seed in `/home/agent/restore.R` resolves to 114.

Two system-level gotchas, both solved without root:

- **`fs` 2.1.0 needs libuv** and no longer bundles it. Build with `USE_BUNDLED_LIBUV=1` for a
  static copy. Without it `fs` fails and takes gargle, googledrive, googlesheets4, pkgload,
  roxygen2 and tarchetypes with it.
- **`elections.alaska.gov` serves an incomplete certificate chain**, omitting its
  "GlobalSign GCC R46 OV TLS CA 2025" intermediate. macOS fetches it via the cert's AIA
  extension; OpenSSL does not, so AK fails on Linux with "unable to get local issuer
  certificate". `CURL_CA_BUNDLE` (set in `~/.Renviron`) points at system roots plus that
  intermediate; verification stays on. Rebuild after a `ca-certificates` update with
  `/home/agent/ca/rebuild.sh`.

`libcurl4-openssl-dev` and `libpoppler-cpp-dev` do need root — the first for curl/httr2, the
second because enightdata *Imports* `pdftools` even though no PDF rows are in scope. The
`V8`/`libnode-dev` warning renv prints is noise: it lists requirements for the whole lockfile,
and `V8` is reachable only through `gt` and `rmapshaper`.

Selenium needs no `chromedriver` package — Selenium Manager fetches a matching driver at
runtime, verified against the installed `google-chrome`.

## Three things that will bite

**`PATH_DROPBOX` and `PATH_ENIGHTDATA` must be set together.** `scripts/util/globals.R` only
skips its username map when both are set; with one alone the map silently reassigns
`PATH_DROPBOX` to the CBS folder. `_targets_medsl.R` aborts rather than let that run.

**Python is load-bearing.** Clarity is the only platform needing reticulate and it is 57 of the
218 rows. With `RETICULATE_PYTHON` unset, discovery dies on whatever stale python is first on
`PATH` and never reaches `py_require()`.

**`write_local()` writes parquet only when the path ends in `.csv`.** A `.parquet` path with
`csv = FALSE` writes nothing and returns normally. `medsl_write()` in `scripts/medsl/lib.R`
handles this; follow it for any new write.

## Network file formats

Raw responses are archived verbatim, one `networks/{net}/raw/{net}_{ts}.tar.gz` per network per
cycle holding each response's bytes exactly as fetched plus a `manifest.jsonl` of url, status,
content type, `last_modified`, etag and the filename. Nothing is re-encoded, so the archive is
what the server sent. A body escaped into a table cell was unusable and huge: NBC alone was
146 MB as CSV and 25 MB as parquet against ~10 MB here, and a cycle's raw is 12 MB in ~10
files rather than 41 MB.

**Filenames come from sniffing the body, not the content type**, which lies: AP serves
`.json.gz` and reports `application/x-gzip`, and a `cbsapi` body arrives already parsed. A
first byte of `{`/`[` gives `.json`, `<?xml` gives `.xml`, `<` gives `.html`, else `.txt`.

Parsed results stay tabular — `{net}_contests_*` and `{net}_candidates_*` as parquet, plus a
`_latest.csv` to eyeball. A network returning 0 contests writes raw but no clean file, so
`abc`, `hill` and `nbc` have an empty `clean/` until their feeds publish.

`net_report` carries a `problem` column holding `verify_enr()`'s message, because verification
runs *after* the files are written: Fox once served a duplicate candidate mid-update, and
aborting first cost the whole cycle's Fox data.

## Checks

```r
enightdata::get_all()   # path_dropbox must be the MEDSL folder, election_folder 26_general
targets::tar_read(net_report)   # per network: entries, errors, contests, candidates, problem
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
- ABC and NBC return the current time as their timestamp, and Hill falls back to it while its
  pages 404, so those targets never cache and write an archive every cycle. Expected, not a bug.
- The Hill's 2026 URLs are unconfirmed, so `net_data_hill` may return 0 contests until its pages
  publish. `error = "null"` keeps that from failing the run.
