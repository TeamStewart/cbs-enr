# Vote-mode lookups

`vote_mode_map.csv` maps the **raw** `vote_mode` string a county reports to the canonical
`method` value used by the election-night contract.

## Canonical vocabulary (authoritative)

Per the contract, `method` is one of: `eday`, `early`, `mail`, `abs`, `adv` (a combination
of early, mail or absentee), `prov`, `all`. `enightmodels::VOTE_METHODS` maps these
abbreviations to full names. Note this is finer-grained than
`enightdata::format_wide`, which collapses to `early`/`eday`/`mail`/`provisional`.

`all` is an aggregate and must **never** be summed alongside the others.
`DROP` is not a method: it marks rows that are not votes (registration counts, under/over
votes) or composites that cannot be assigned.

## Keyed on (state, year), deliberately

Labels drift between cycles and differ by county within a state. TX 2024 alone uses **26
distinct raw labels across 28 counties**, including case variants (`Early Voting` vs
`EARLY VOTING`) and county-specific coinages (`EV In-person`, `PROV/EV/ED`).

## Matching rules

Match on the exact raw string where possible. If falling back to patterns, the order must
be **mail → prov → early → eday**:
 - `EV Mail` must reach `mail` before a loose `/ev|early/` rule claims it;
 - `Early Voting Provisional` and `Election Day Provisional` must reach `prov` before
   `early` or `eday` claim them.

## State-specific determinations

**TX: `absentee` means by-mail, so it maps to `mail`, not `abs`.** Established empirically
— every TX county's absentee-labelled column is 2.1–4.7% of that county's vote, matching
Texas's restricted by-mail share (~4%), whereas early voting runs 61–82%. Do not assume
this transfers: in other states "absentee" can mean in-person absentee, which would be
`early` or `abs`.

## Provenance

Built 2026-10-18 from `24_general/TX/raw/*.csv` (249 snapshot files, 28 counties).
**These lookups are only needed where the cleaning step has not run.** As of 2026-10-19,
`24_general/{AZ,GA,MI,NC,PA}/clean/` carry an already-normalized schema — `vote_mode` in
the canonical vocabulary (`Early Voting` / `Election Day` / `Absentee/Mail` /
`Provisional` / `Overvote/Undervote`) and `candidate_party` spelled out (`Democrat`) — so
those states need no mapping at all. The states still on the raw county shape, and so
dependent on these files, are AR, CO, SC, TX and WV. `scripts/model/returns_panel.R`
branches on which shape a state has.

Note `24_general/TX/clean/` is empty — the TX 2024 cleaning step was never run, which is
why no normalized TX 2024 data existed and this map had to be reconstructed from raw.

---

# `party_map.csv`

Maps the raw `candidate_party` string to `dem` / `rep` / `oth`.

Needed for the same reason as the mode map: the raw strings are county-specific. **Travis
parenthesises the party** — `(D)`, `(R)`, `(L)` — in both `candidate_party` *and*
`candidate_name` (`"(D) Colin Allred"`). A naive `substr(upper(party),1,3) == "DEM"` test
sends Travis's entire vote to `oth`: 3.83M Allred and 1.46M Cruz across snapshots, which
inflated the panel's third-party share to 17.7% against a true ~2%.

Other observed forms: `DEM`/`Dem`, `REP`/`Rep`, `LIB`/`Lib` (Dallas uses title case,
Tarrant upper), and `NON` for write-in rows.

**Write-ins (`NON`) stay in `oth`** — they are real votes and dropping them would short the
turnout denominator and bias every share upward.

**Separately, exclude rows with a blank `candidate_name`.** Tarrant carries 75,712 votes
under an empty candidate name; these are not attributable and must not be summed.

---

# `reporting_levels.csv`

**What it answers:** for a given `(state, county, method)`, will those votes arrive on
precinct geos, or in a county-level / early-voting-site bucket?

Built by `scripts/model/make_reporting_levels.R` (from the cbs-enr folder:
`Rscript scripts/model/make_reporting_levels.R [STATE ...]`). Keyed on `(state, year)`
like the other lookups in this folder.

## Why it exists

`cell_counts()` in `scripts/model/live/R/returns.R` joins **from** the precinct spine, so
any returns row whose `geo` is not a precinct is dropped. That is not a small leak:

| source | votes on non-precinct geos | share |
|---|---|---|
| NC 2024 Governor (`virtual_precinct == TRUE`) | 1,743,117 of 5,591,547 | **31.2%** |
| AK Aug-2026 primary (`NN-EARLY` / `NN-ABSENTEE` / `NN-QUESTION`) | 153,478 of 493,117 | **31.1%** |

`returns_dropped()` / `report_dropped()` catch this at runtime. **This table is the
pre-election expectation they are checked against**, so a county reporting differently
from expectation is a logged deviation rather than a discovery on air.

## Grain and columns

One row per `state × year × county × method`.

| column | meaning |
|---|---|
| `county_fips`, `county_cbs`, `county_name` | from `{ST}_counties.csv`. `county_cbs`, zero-padded to 3, is the `geo` prefix — verified against `{ST}_precinct.parquet` for all six states. `county_name` is upper-cased. For AK the "county" is the **election district** (`ED01 …`), which is the unit `county_cbs` indexes, not the borough. |
| `method` | `eday` / `early` / `mail` / `prov` / `all`, plus two values that are not vote methods — see **`transfer` and `*`** below |
| `level` | **the operational expectation: `precinct`, `county_site`, or `unknown`.** Resolved by the UNION rule below |
| `level_priority` | the same question resolved by strict source priority instead of union |
| `provenance` | `zero_2026` / `actual_2024` / `state_modal` / `unknown` — the highest-priority source that had evidence |
| `sources` | every source that had evidence, e.g. `zero_2026+actual_2024` |
| `level_zero_2026`, `level_actual_2024` | the per-source verdicts, blank where that source is silent. **The disagreement is meant to be visible in the table** |
| `disagree` | both sources spoke and they differ |
| `n_geo_precinct`, `n_geo_bucket` | 2026 zero-file geo counts for this county × method |
| `votes_precinct_2026`, `votes_bucket_2026` | 2026 vote mass, **summed over every contest in the file**. Evidence that a bucket is live, not a vote total. Blank where the file is still all zeros |
| `votes_precinct_2024`, `votes_bucket_2024`, `share_bucket_2024` | 2024 vote mass by `virtual_precinct`, and the bucket share that decided `level_actual_2024` |
| `in_spine` | is this county in `{ST}_precinct.parquet`? A county we do not model is an `unknown` that does not matter |
| `state_evidence_coverage` | share of the state's counties with direct evidence — the number that gates `state_modal`, repeated on every row of that state |
| `bucket_labels` | the distinct bucket labels seen, truncated to 300 chars. This is what a human reads to recognise `nc_032_EV DUKE` on the night |

## Provenance, in priority order

1. **`zero_2026`** — `26_general/{ST}/clean/{ST}*_latest.parquet` (schema: `state,
   election, jurisdiction, contest, district, candidate, party, time, stage, method, geo,
   votes`). A county × method is `county_site` when the county carries at least one bucket
   geo of that class. The file is a full `geo × method` cross, so the bucket's *own* method
   comes from its label, not from which methods appear on its rows.
2. **`actual_2024`** — `24_general/{ST}/clean/{ST}*_latest.parquet`. **A different schema**,
   carrying `virtual_precinct` (logical) plus `vote_mode` / `precinct_total` /
   `precinct_id` / `jurisdiction`. `virtual_precinct` is the ground truth for
   site-vs-precinct and the 2026 normalization **dropped it**, which is the whole reason
   2024 is still needed. Here there is vote mass, so the rule is measured: `county_site`
   when more than half that county × method's votes sit on virtual precincts. On NC that
   threshold does no work it could get wrong — 67 counties at 0.0%, 33 at 100.0%, nothing
   in between.
3. **`state_modal`** — for a county with no direct evidence, the modal pattern among the
   counties that *did* file, but **only if the state's filers are numerous enough to speak
   for it**: `state_evidence_coverage ≥ 1/3`, the method carried by ≥ 2/3 of evidence
   counties, and ≥ 2/3 of them agreeing on its level.
4. **`unknown`** — nothing. One row, `method = "*"`, `level = "unknown"`.

**`unknown` is a normal outcome, not an edge case.** Zero-file coverage is partial and
will stay partial: CA has 4 files against 58 counties, MI 13 against 83, TX 6 against 254,
FL 34 against 67, while AK and NC are single statewide files. So CA, MI and TX fall below
the coverage gate and their unfiled counties are `unknown` rather than being assigned a
level inferred from 2.4–20% of the state.

Per the plan, **unknown counties route to the `all`-only / mode-free path**, and the
asymmetry is the reason: assume three modes wrongly and the composition equations sit at
prior and return the turnout echo *silently*; assume `all`-only wrongly and the first real
returns reveal it and upgrade.

## `level` is the UNION of the sources, not the newest

`level_priority` follows the strict order above. **`level` does not** — it is
`county_site` if *either* source says `county_site`. Absence of a bucket in a zero file
does not mean precinct allocation: zero files are filed before early voting opens and a
county may not have created its site geos yet. Expecting a bucket that never arrives costs
nothing; not expecting one that does costs 31% of the vote.

NC is why this matters: the two sources disagree in **both** directions. Measured —
32 counties carry a 2026 early bucket, 33 were site-reporting for Early Voting in 2024,
and the union is 38.

- `county_site` in 2026, `precinct` in 2024: **Henderson, Nash, Polk, Rockingham, Wayne**
- `county_site` in 2024, no 2026 early bucket: **Bladen, Camden, Hoke, Martin, Perquimans,
  Watauga**

(Two of those 2026 flags are the classifier's known false positives against 2024's
`virtual_precinct` — Caswell `PROVI` and Henderson `EV`. Both *look* like buckets, so
flagging them errs in the safe direction.)

## How a bucket geo is recognised

`BUCKET_RX` in `make_reporting_levels.R`, matched against the geo's precinct-id suffix in
the order this README already fixes for `vote_mode_map.csv` — **mail → prov → early →
eday** — so `EV MAIL` reaches `mail` and `EARLY VOTING PROVISIONAL` reaches `prov`.

**It is scored, not asserted.** Against `virtual_precinct` on all 2,908 distinct
`(county, precinct_id)` labels in NC 2024 Governor it finds **249 of 250** virtual labels
with **2** false positives. The one miss is Hoke's `ROCKFISH`, an early-voting site named
after a place — unrecoverable by pattern, and the reason this is a *validated heuristic*
rather than ground truth. `make_reporting_levels.R` re-scores it on every run that
includes NC; re-read that output after any edit to `BUCKET_RX`.

Observed label families: `EARLY VOTING`, `EARLY VOTING n`, `EARLYVOTE`, `EV <site>`,
`EV-<site>`, `EV[A-Z]{2,4}` (Wake's compact codes), `<place> EV`, `BOE`, `CURBSIDE`,
`ONE STOP`; `ABSENTEE`, `ABSENTEE BY MAIL`, `ABS-SUPPLEMENTAL`, `SUPP ABSENTEE`;
`PROVISIONAL`, `PROVISONAL`, `PROVI`, AK's `QUESTION`; `TRANSFER`, `TRANS`.

**Do not use the spine join to detect buckets.** Off-spine does not mean bucket: CA, FL,
MI and TX zero files use a completely different precinct-id vocabulary from
`{ST}_precinct.parquet` (`ca_037_0001-100000-SAN PASQUAL` vs `ca_037_456100`,
`mi_011_City of Benton Harbor, Ward 1, Precinct 1` vs
`mi_063_ROCHESTER HILLS CITY_W01_P006`), so a join-based test reports those states as
100% bucket. That is unreconciled ids, a different problem.

## `transfer` and `*` are not vote methods

Two `method` values are deliberately outside `enightmodels::VOTE_METHODS`:

- **`transfer`** — a county's transfer pile (`nc_003_TRANSFER`), present in 33 NC counties.
  Its votes land in `eday`, but it must **not** make `eday` look site-level: NC 2024
  measures `eday` at 100% precinct-allocated, the transfer pile being 254 votes statewide.
  Carrying it as its own row keeps `level` meaning exactly one thing while still
  enumerating every geo `cell_counts()` will drop. Emitted from 2026 only — the 2024 rule
  is a per-`vote_mode` vote-mass share, and a TRANSFER precinct's `vote_mode` is
  `Election Day`.
- **`*`** — "we do not know this county's method vocabulary either", on `unknown` rows.
  One such row per unknown county, so the unknown row count equals the unknown county
  count.

## What the table says today (2026-10-26)

| state | `zero_2026` | `actual_2024` | `state_modal` | `unknown` | counties `unknown` (of them modelled) |
|---|---|---|---|---|---|
| AK | 160 | — | — | — | 0 |
| CA | 7 | — | — | 54 | 54 (13) |
| FL | 115 | — | 99 | — | 0 |
| MI | 33 | 12 | — | 66 | 66 (16) |
| NC | 433 | — | — | — | 0 |
| TX | 17 | — | — | 248 | 248 (27) |

`county_site` rows: **AK** early 40, mail 40, prov 40 — all 40 election districts, i.e.
AK's entire early, mail and provisional vote reports at district level and only `eday` is
precinct-level. **NC** early 38, mail 41, prov 41, transfer 33. **CA, FL, MI, TX** none
yet — and for CA/MI/TX that is a statement about 4/13/6 filed counties, not about the
state.

`24_general/{ST}/clean/` exists only for **MI** (Eaton, Ingham, Macomb, Oakland) and
**NC** among the six; AK/CA/FL/TX have no normalized 2024 actuals (TX's `clean/` is empty —
see the `vote_mode_map.csv` provenance note above), so for them source 2 is silent.

NY is out of scope: statewide-only, no precinct inputs.

## What the model does with it: the structural-absence mask

`model_precincts.csv` carries one `level_{mode}` column per method, joined down from this
table by `make_model_reference.R`. `model_turnout.R` reads those columns at setup
(`USE_MODE_EXISTS`, default on) and builds an `EXISTS` matrix of precinct x mode. A cell
marked `county_site` **does not exist**: its composition weight is set to exactly 0 before
the per-precinct renormalisation, it contributes no row to that mode's equation, and it is
excluded from the all-modes-counted test. `""` / NA **keeps** the cell — unknown must never
remove one, because wrongly dropping a mode throws its returns away while wrongly keeping
one just reproduces the old behaviour.

Only AK and NC carry any `county_site`, so CA/FL/MI/TX are bit-identical with the mask on
or off (verified per draw on the precinct frame and both statewide draw matrices).

What it fixes, measured at 50% of precincts reporting with every reporting precinct counted
out, where the right answer for a reported precinct is exactly its counted vote:

| | AK before | AK after | NC before | NC after |
|---|---|---|---|---|
| max `q_share` reached | 0.917 | **1.000** | 1.000 | 1.000 |
| reported precincts that can pin | **0 of 201** | 201 of 201 | 799 of 1,316 | **1,316 of 1,316** |
| est / counted on reported precincts | **1.558** | 1.000 | **1.266** | 1.000 |

So a fully-counted AK precinct was predicted 56% above its own final count, and it could
never pin because `q_share` could not reach `PIN_MIN_SHARE` — it was still waiting for mail
and early cells that report at district level and never arrive. The statewide total hid
this almost perfectly (100.0% of prior either way), which is why it had to be measured per
precinct.

**A mode with NO precinct support anywhere is a `mode_scope.csv` error and aborts.** `M =
length(MODES)` is baked into Sigma, the residual array and the softmax before `fit_stage`
is called, so it cannot be dropped later. AK is the live case and its `modes` is now `eday`
alone; its district-level mail and early have to come back through the county add-on.

---

# `precinct_geo_crosswalk.csv` and `precinct_geo_match_rates.csv`

Maps our precinct key `geo` to the votehub precinct-geometry key `unit_id`, so a
precinct-level prediction can be drawn on a map. Built by
`scripts/model/make_precinct_topo.R`, which also emits the simplified geometry to
`scripts/model/topo/{st}_precincts.topojson`.

**Geometry is static for the whole night.** Only predictions change, so the crosswalk and
the topojson are built ONCE, offline, and the dashboard joins data to them at render time.
Nothing here belongs in the refresh loop.

## Why it exists

There is one nationwide GeoPackage —
`26_general/input_data/votehub-shapefiles/26_pct_bdrys.gpkg`, 525 MB, layer `precincts`,
38,892 features, EPSG:4326 — keyed `unit_id = {fips5}-:-{local_precinct}`. That is the
same key as `estimated_past_results.csv` in the same directory (identical 38,892 rows), so
one crosswalk serves geometry and past results both.

**No single column joins it.** The key that works differs by state, the normalization that
works differs by county, and in six FL counties votehub keys by polling-place NAME rather
than by precinct number. A persisted, reviewable crosswalk with a per-row provenance
column is the only version of this that can be audited after the fact.

## Scope is AK / FL / NC / TX — a data limit, not a choice

| state | our precincts | in gpkg | status |
|---|---|---|---|
| AK | 402 | 402 | **ready** |
| FL | 3,764 | 5,708 | **ready** (31 of 67 counties modelled) |
| NC | 2,633 | 2,625 | **ready** |
| TX | 6,134 | 9,530 | **ready** (32 of 254 counties modelled) |
| MI | 2,730 | 3,895 | 0% on every column we carry — needs a name-construction crosswalk |
| CA | 23,685 | **absent** | no CA geometry in Dropbox at any depth |
| NY | statewide only | — | nothing to map |

GA, IA, NE, OH and SC are in the GeoPackage; we model none of them.

## Grain and columns

`precinct_geo_crosswalk.csv` — one row per **our** precinct, matched or not. Keyed on
`(state, year)` like `vote_mode_map.csv`.

| column | meaning |
|---|---|
| `state`, `year` | the lookup key; `year` is the geometry vintage, 2026 |
| `geo` | our precinct key, `{st}_{county_cbs}_{precinct_id}` |
| `unit_id` | votehub's key, `{fips5}-:-{local_precinct}`. **Blank when unmatched** |
| `rule` | **provenance** — which rule made this pair; blank when unmatched |
| `county_fips`, `county_name`, `precinct_raw` | carried for review |

`precinct_geo_match_rates.csv` — one row per `(state, county_fips)`, the join unit.

## Two denominators, and only one of them measures the key

- **`pct_ours`** = `matched / our_n`. What the map actually covers.
- **`pct_attainable`** = `matched / min(our_n, vh_n)`. Key quality.

The second exists because several counties simply have **fewer votehub polygons than we
have precincts**, and no normalization can conjure a polygon that is not in the file. TX is
the whole story: `TX26P` is **March-primary vintage**, and primary-consolidated counties
are renumbered wholesale — Bell carries votehub `101..120 / 201..` against our `0003..`,
El Paso a sequential `001..236` against our real precinct numbers, Wood 14 polygons for
our 68 precincts. Those are different precinct universes, not different spellings.

So the **gate is on `pct_attainable`** (95% state, 80% county), which is the part we
control. `pct_ours` is reported beside it and blocks a state's topojson below 90%. A county
with low `pct_ours` and high `pct_attainable` has a geometry-vintage problem the script
cannot fix and must not pretend to.

## Achieved rates (built 2026-10-26)

| state | our precincts | matched | `pct_ours` | `pct_attainable` | join units |
|---|---|---|---|---|---|
| AK | 402 | 402 | **100.0%** | 100.0% | 40 |
| FL | 3,764 | 3,755 | **99.8%** | 99.9% | 31 |
| NC | 2,633 | 2,618 | **99.4%** | 99.7% | 100 |
| TX | 6,134 | 5,675 | **92.5%** | 98.2% | 32 |

Every county clears both gates. The worst `pct_attainable` is NC Halifax at 88.2% and TX
Bexar at 92.3%; the worst `pct_ours` is TX Wood at 20.6% (14 polygons for 68 precincts)
and TX Bell at 51.5% — both pure vintage, both 97–100% of attainable.

These **beat the figures the plan was written against** (AK 99.5%, FL 98.3%, NC 99.4%,
TX 92.5%), and the gains came from the three rules below, not from a better single key.

## The rule cascade

One ordered cascade serves all four states; the per-state winner simply fires first. Each
rule is tried only on rows still unmatched, **only within a county**, and contributes a
pair **only if its key is unique on both sides** within that county — an ambiguous key is
dropped, never guessed.

| rule | our side | votehub side | who needs it |
|---|---|---|---|
| `enr_exact` | `precinct_enr` literal | `unit_id` tail | AK 400, NC 2,617, FL 1,004 |
| `raw_exact` | `precinct_raw` literal | `unit_id` tail | TX 4,813, FL 2,615 |
| `enr_code` / `raw_code` / `l2_code` | normalized code | normalized code | TX 858, FL 108 |
| `enr_code_lead` | `precinct_enr` code | **first token** of `unit_id` tail | AK 2 |
| `raw_code_sos` / `raw_code_sostail` | `precinct_raw` code | `precinct_sos`, whole or **last token** | FL 18, TX 4 |
| `enr_name` | `precinct_enr` as a NAME | `precinct_sos` as a NAME | FL 10 |
| `l2_name*` | `precinct_l2`, whole or suffix | `unit_id` tail / `precinct_sos` | NC 1 |

**Literal equality runs first, and that is load-bearing.** The code normalizer is lossy and
can collide two genuinely different precincts: NC Buncombe carries both `68.1` and `681`,
votehub carries both as separate polygons, and both normalize to `0681` on both sides — so
the uniqueness guard (correctly) drops all four and Buncombe silently loses two precincts.
Literal matching resolves them, and lossy normalization only ever sees what literal
matching could not place.

### Normalization

`code4` is the verified recipe from `tx_backtest_panel.R` / `backtest_panel.R` (98.9% on
TX 2022 L2↔votehub): upcase → take a **trailing parenthetical** if present → strip
punctuation → **zero-pad to 4**.

- The parenthetical rule is for our `precinct_l2`, which stores several states as
  `NAME (ABBREV)` while votehub stores the bare abbreviation (AK `WALES (39-950)`,
  NC `01 PATTERSON (01)`). votehub never uses parentheses, so applying it to both sides
  is safe.
- Padding both sides to a common width is what makes `01` and `1` meet. Stripping leading
  zeros instead would be equivalent for numeric keys but **breaks alphanumerics** like
  NC's `03C`.
- **A trailing `.0` is stripped, and this one is a correctness fix, not a coverage one.**
  FL Levy's `unit_id` carries spreadsheet decimal artifacts (`1.0`, `10.0`). Without the
  strip, `1.0` → `10` → `0010` and silently matches **our precinct 10**. That is a *wrong*
  join, not a missing one, and it is why Levy previously read 7.7% rather than 0%.

## State-specific determinations

**FL: six counties are keyed by precinct NAME, not number** (Baker, Gadsden, Hendry, Levy,
Okeechobee, Taylor — all six are in our 31 modelled counties). votehub gives
`12003-:-Baxter`. Only three of the six actually needed a name rule; the cascade reaches
all six and the per-county record of which rule fired is in `rule`:
- *Gadsden* — `precinct_sos` is the polling-place name and our `precinct_enr` **is** that
  same name (`FAMU Research Center`). `enr_name` handles it; the normalizer must strip
  punctuation because votehub uses a curly apostrophe in `Everybody's Church`.
- *Levy* — same shape, but the `.0` strip alone already recovers it on `raw_code`, and
  `enr_name` independently agrees. Two rules reaching the same pair is the cross-check.
- *Baker* — `precinct_enr` is NA there. `precinct_sos` is `Baxter 4A`, so the **last
  token** carries the code; `precinct_l2` is `4A-BAXTER`, so its suffix carries the name.
- *Hendry, Okeechobee, Taylor* — reached by plain `raw_code`/`enr_code` after
  normalization; Okeechobee is a pure zero-pad case. All three are 100%.
- *Walton* — not a name county, but `unit_id` is `Walton 210` while `precinct_sos` is the
  bare `210`. `raw_code_sos` handles it.

**AK: `county_fips` is a HOUSE DISTRICT, not a county.** `02001` covers Ketchikan Gateway,
Prince of Wales-Hyder *and* Wrangell, and one district reaches 7 borough names. The
match-rate table therefore groups on `county_fips` alone and labels rows with votehub's own
`county` (`HD-01`); grouping on the borough name too would split one join unit into several
rows carrying the same `vh_n` and double-count the attainable denominator. Separately,
votehub appends place names to two Bethel codes (`38-824 Kipnuk`) — `enr_code_lead`.

**NC: the 15 unmatched are not spelling failures.** They are 2:1 merges and genuine
differences: Halifax has our `ENF1`/`ENF 2` against votehub's single `ENF`, and our
`RR1-2`/`RR3-4` against `RR1-4`; Lenoir has our `PH1`/`PH2` against `PH`. A 1:1 crosswalk
cannot express a merge. Johnston (`PR24/25/27A/27B` vs `PR39/PR40`), Vance (`NH` vs
`Townsville`) and Cabarrus (`02-05/02-07` vs `02-10`) are different precincts outright.

**TX: the 459 unmatched are overwhelmingly geometry vintage, not keys.** **17** of our 32
TX counties have fewer votehub polygons than we have precincts, accounting for **356** of
the 459 (Wood 68→14, Bell 136→72, Kaufman 96→37, El Paso 279→236, …). Bexar is the one real
key gap: 97 of our 841 are unmatched against a polygon shortfall of only 35, and the extra
62 sit in a `0001`–`004B` series with no counterpart in votehub's 4-digit `1001`-style
universe.

## The topojson, and three traps that each fail silently

`scripts/model/topo/{st}_precincts.topojson` — all votehub features in our modelled
counties, `geo` attached as a feature property, `county_fips`/`county_name` carried.
**Unmatched polygons are kept with `geo` unset**, deliberately: a dropped polygon is a hole
in the state outline that reads as "no votes here", whereas a null-`geo` polygon renders as
no-data and is honest about what it is.

| state | features | geojson in | topojson out | quantization |
|---|---|---|---|---|
| AK | 402 | 105.2 MB | 1.59 MB | 1e5 |
| FL | 3,829 | 104.4 MB | 1.40 MB | 1e5 |
| NC | 2,625 | 114.9 MB | 1.29 MB | 1e5 |
| TX | 5,919 | 93.7 MB | 1.93 MB | 1e6 |

Simplification is a **resolution in metres** (100 m), not a percentage: a percentage is a
quota on removable vertices, so the same setting means a different ground resolution in
every state and AK's coastline would come out far coarser than TX.

Three failure modes, all of which exit 0 and produce a valid file:

1. **`ogr2ogr -sql` with an explicit column list DROPS GEOMETRY.** `geom` must be named in
   the SELECT. Without it every feature is `"geometry": null`, and mapshaper turns that
   into a perfectly valid, perfectly empty topojson.
2. **`-clean` after `-simplify` DISCARDS the simplification.** It rebuilds topology from
   the full-precision arcs. Measured on AK: 0.25 MB with `-clean -simplify`, 12.62 MB with
   the two swapped, and mapshaper reports success either way. `-clean` must come first.
3. **Coarse quantization DELETES SMALL POLYGONS.** It is applied over the layer's bounding
   box, so `quantization=1e4` nulled 75 of 5,919 TX features and 15 of 402 AK features for
   ~10% off the file size — and small-area precincts are the dense urban ones, so the holes
   land exactly where the votes are. The script escalates `1e5 → 1e6 → 1e7 → none` until the
   written file has **zero** nulls, and aborts if none does.

The script therefore reads each written file back with `mapshaper -info` and asserts it is
a polygon layer, has the expected feature count, and has zero null geometries. Checking the
exit code would have passed all three traps.

**AK crosses the antimeridian.** Its Aleutian tail reaches +179.8, so its bounding box reads
359° wide for a state ~55° wide. The geometry is correct; what breaks is any renderer that
fits a viewport to the bbox, which draws AK as a sliver. The fix belongs at render time (an
Albers projection — Observable Plot's `projection: "albers-usa"`), not in the data, so the
script warns rather than rewriting coordinates.

**FL's three largest unmatched polygons are placeholders, not precincts** — `12087-:-XX`
(Monroe, 2,577 km²), `12099-:-NP` (Palm Beach, 2,215 km²) and `12011-:-XXX` (Broward,
2,056 km²) cover Everglades water-conservation land. They will render as large no-data
blobs, which is correct: there are no voters in them.

## Provenance

Built 2026-10-26 by `scripts/model/make_precinct_topo.R` from
`26_general/input_data/votehub-shapefiles/26_pct_bdrys.gpkg` (2026-10-02) and
`26_general/input_data/model_inputs/{ST}_precinct.parquet` (2026-10-22). Rebuild with
`Rscript scripts/model/make_precinct_topo.R`; `SKIP_TOPO=1` stops after the crosswalk and
`STATES=FL` does one state. Requires GDAL (`ogr2ogr`) and mapshaper
(`npm i -g mapshaper`) — no R spatial dependency, and `nanoparquet` rather than `arrow`,
which is in `renv.lock` but not installed.
