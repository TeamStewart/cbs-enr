# Upstream items for `enightmodels` (and `cbsapi`)

Things we have measured in Cory's packages that we work around locally rather than patch,
because `renv.lock` pins `enightmodels` by SHA (`c99797a66`) and the installed copy ships no
`.R` sources — only `.rdb`/`.rdx` — so a local patch is undone by the next
`renv::restore()`, i.e. precisely on a fresh checkout on election night.

Each item below was verified against the installed build, not read off the source. Status is
ours, not Cory's.

---

## 1. `check_pred_data()` cannot return, for any input — **blocks CONTRACT §15's acceptance test**

Its last block is

```r
if (isTRUE(pred)) {
  assert(identical(names(d), COLS_PRED), ...)
}
```

and `pred` is not one of its arguments: `formals()` is just `d`, and
`exists("pred", asNamespace("enightmodels"), inherits = FALSE)` is `FALSE`. So every call
ends `Error: object 'pred' not found`.

It is a copy-paste from the sibling. `check_elec_data(d, full = TRUE, strict = FALSE)` ends
with the **identical** block guarded by `isTRUE(strict)` — a real argument, defaulting
`FALSE`. So `strict` was renamed to `pred` on the way across and never added to the
signature.

The block is **last**, so all ten substantive checks run and pass first; only the
unreachable `invisible(TRUE)` is missing.

**The fix is two tokens:**

```r
check_pred_data = function(d, strict = FALSE)   # was function(d)
...
  if (isTRUE(strict)) {                          # was if (isTRUE(pred))
```

**`strict` must default to `FALSE`.** If it defaulted `TRUE`, that block asserts
`identical(names(d), COLS_PRED)` over *all* names and would reject output carrying permitted
extras (ours adds `p_win`, `geo`, `cbs_as_of` per CONTRACT §15). That default is almost
certainly why it is off in the sibling.

**Our workaround:** `check_pred()` in `scripts/model/live/R/output.R` swallows that one
message by exact string match and re-raises everything else, then applies the column-order
assertion itself, scoped to the first 16 columns. It retires itself silently once this is
fixed — the `tryCatch` simply stops firing. Verified narrow: 9 real violations still abort.

---

## 2. Two functions call unqualified symbols their namespace does not import

`enightmodels` imports **one** symbol from `dplyr` and does not import `stringr` or
`tibble` at all. Measured by resolving each name in an environment whose parent is the
package namespace:

| function | unqualified call | resolves inside the namespace? |
|---|---|---|
| `rename_demo_cbs()` | `str_replace()` | **no** |
| `forecast_intervals()` | `tibble()` | **no** |

So both depend on the **caller's** search path. They work inside our pipeline because
`live/load.R` attaches `dplyr` (which re-exports `tibble`), and they work inside
`create_frame.R` because it attaches `tidyverse`. They fail for a bare
`Rscript -e 'enightmodels::forecast_package(...)'` call, which is how we first hit it.

**Fix:** namespace them (`stringr::str_replace`, `tibble::tibble`) or add the imports.

---

## 3. `forecast_prec()`'s `obs_turn` is required but undocumented

Omitting it fails with `object '' not found`, which gives a caller nothing to go on. Worth
either a default or a named error. (`scripts/model/live/R/model_cprec.R` asserts it
up front for this reason.)

---

## 4. Our local divergence from `prec_model.R` — **send this one to Cory, it is ours**

`scripts/model/modeling-cory/tx-prim-26/R/prec_model.R` carries three additive lines of ours
that return `prec`, the pre-`summarize` frame, so the precinct dump can read per-precinct
output. It is a local divergence from his upstream copy and will be clobbered by his next
pull unless he takes it.

---

## 5. `cbsapi`: `exp_method` is floored to `rep_method`, so "complete" is indistinguishable from "no expectation"

When CBS files no expectation for a method, `cbs_county_pull.R` records that `exp_method` is
set to `rep_method`, making the implied counted fraction exactly `1.0`. `prov` has no
expectation field at all (`expected` covers `adv`/`eday`/`mail`/`early` only), so
provisionals read as permanently counted out.

Consequence for us: a per-mode counted fraction of `1.0` cannot be read as "counted out",
which is why `model_hybrid`'s county arm does not use per-mode gross-ups. A distinguishing
value (`NA`, or a flag) would make the fraction usable.

---

## Not an upstream item, recorded so it is not re-litigated

`ei_run()` is not a race forecaster and cannot run here — `seine` is not installed and
`ei_run()` calls `seine` functions unqualified. Excluded deliberately, not blocked.
