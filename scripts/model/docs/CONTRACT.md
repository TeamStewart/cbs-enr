# Data contract — race-by-race election-night model (2026)

Consumers: the election-night models in `scripts/model/live/`. §1, §2, §3, §6 and §9 are
built for every state by `scripts/model/prep/prep_model_inputs.R`, which reads
`26_general/input_data` and writes `26_general/input_data/model_inputs/{ST}_*`. See
`tx_model_spec.tex` for the medsl_sur model these inputs feed.

This specifies what the model needs, at what grain, in what units. It is written so a
database can be mapped to it field by field. Sections marked **BLOCKED** are inputs we do
not yet have; they are specified anyway so the schema can carry typed placeholders rather
than be reshaped later.

---

## 0. Conventions that apply everywhere

**Canonical precinct id.** One string, `geo`, formatted `{st}_{county_cbs}_{precinct_id}`
with `st` lowercase two-letter and `county_cbs` zero-padded to 3 digits —
e.g. `tx_257_1`. This is the live-scraper format and the spine of every join.

**`precinct_id` is normalized the same way for every source** before `geo` is built:
trim, upper-case, and drop leading zeros (`0007` → `7`, `001A` → `1A`). Live feeds write
ids WITHOUT leading zeros: Kaufman's live id for YouGov precinct `0001` is `1`, and
Harris live ids matched the frame at 15.8% before stripping zeros and 99.6% after. The
county's own label is kept unchanged in `precinct_raw` (§1). Where the crosswalk has a
`precinct_enr`, `geo` is built from that id instead (same normalization).

**ID reconciliation is the main failure mode, not a detail.** Measured on TX as of
2026-10-18, matching each source to the covariate frame:

| source | match rate | note |
|---|---|---|
| L2 (`precinct_l2` via crosswalk) | 90.3% of precincts (5,537 / 6,134), 98.2% of registrants | usable; measured 2026-10-19 on the 2026-10-11 L2 file |
| L2 (direct `county` + `precinct`) | 62.4% | **do not join directly** |
| live ENR feed (`precinct_enr`) | **0.8%** | hard blocker, see §8 |
| votehub past results | 88% / 38% / 0% | varies by county (BEXAR / KAUFMAN / BELL) |

**Do not backfill crosswalk gaps by normalizing ids.** For 92 TX precincts the crosswalk
leaves `precinct_l2` blank although a normalized-id match exists in L2. Those pairs are
different places that share a number: their registrant counts differ by a median of 35%
(e.g. El Paso `0054`: 13,275 registrants in the frame vs 1,989 in L2), against a median
gap of about 1% for crosswalk matches. The blanks are correct.

Every table below therefore carries `geo` as its key, and **any source whose native ids
differ must be reconciled upstream**, not at query time. The crosswalk
(`crosswalks/{st}.csv`) is the place for that, with an explicit rule column.

**Units are a required part of every numeric field.** Mixing rates and counts has bitten
repeatedly: `turnout20/22/24` in the YouGov frame are **rates in [0,1]**, while L2's
`vote22_*` are **counts of voters**. Every field below states which.

**Boundary vintage must be stamped.** Precinct boundaries change between 2022, 2024 and
2026. Any table carrying historical quantities needs `boundary_vintage` naming the map its
`geo` values are on. Votehub's `estimated_past_results.csv` appears to be on an older
vintage for some counties (KAUFMAN has 37 precincts there vs 97 in the 2026 frame), which
is why this is not optional.

**Vote-mode taxonomy.** `method` is one of `eday`, `early`, `mail`, `abs`,
`adv` (a combination of early, mail or absentee), `prov`, `all` —
`enightmodels::VOTE_METHODS` maps these abbreviations to full names. The model currently
uses `{mail, early, eday}`. Note this vocabulary is finer-grained than
`enightdata::format_wide`, which collapses to `early`/`eday`/`mail`/`provisional`; the
contract vocabulary above is the authoritative one.

`all` is an aggregate and must never be summed alongside the others.

**The raw-label mapping lives in `scripts/model/lookups/vote_mode_map.csv`**, keyed on
`(state, year, raw_label)`. TX 2024 alone needs **26 rows for 28 counties**, including
case variants (`Early Voting` / `EARLY VOTING`) and county coinages (`EV In-person`,
`PROV/EV/ED`). Two things that mapping records and any new state must too:

- **Where provisional, limited and composite rows went.** `Limited` folds into `prov`;
  Dallas's `PROV/EV/ED` is a composite of three modes and is dropped (1,370 votes, 0.2% of
  Dallas); `regVotersCounty`, `Undervotes` and `Overvotes` are not votes and are dropped.
- **State-specific semantics.** In **TX, `absentee` means by-mail and maps to `mail`, not
  `abs`** — established empirically, since every TX county's absentee column is 2.1–4.7% of
  its vote, matching Texas's restricted by-mail share, against 61–82% for early voting.
  This does **not** transfer: elsewhere "absentee" can mean in-person absentee.

**The mapping must be keyed on `(state, year)`, not state alone.** Labels drift between
cycles even within one source: in the MEDSL official returns, NC's early-vote mode is
`ONE STOP` in 2022 and `EARLY VOTING` in 2024, and NY 2022 carries both `ELECTION_DAY`
and `ELECTION DAY` as distinct values.

---

## 1. `precinct` — the spine

Grain: one row per precinct per cycle.

| field | type | null | unit | notes |
|---|---|---|---|---|
| `geo` | text | no | — | **PK**. Format per §0 |
| `state` | char(2) | no | — | |
| `county_name` | text | no | — | upper-case, squished |
| `county_fips` | char(5) | no | — | |
| `county_cbs` | char(3) | no | — | the id used inside `geo` |
| `precinct_raw` | text | no | — | the county's own label, pre-normalization |
| `boundary_vintage` | text | no | — | e.g. `2026_general` |
| `geostratum` | text | yes | — | geographic pooling key |
| `stratum_demo` | text | yes | — | demographic pooling key. See §11 note |

---

## 2. `precinct_covariate` — the design matrix

Grain: one row per `geo`. Source: L2 precinct frame, except `dem2` / `oth_base`.

| field | type | null | unit | source |
|---|---|---|---|---|
| `geo` | text | no | — | **PK/FK** |
| `as_of` | date | no | — | voter-file vintage |
| `n` | int | no | count | registrants, from the YouGov frame (complete for every precinct); L2's count is carried as `l2_n` |
| `active` | int | yes | count | |
| `new` | int | yes | count | new registrants |
| `p_male` | double | yes | share of `n` | |
| `p_age_18_29`, `p_age_30_44`, `p_age_45_64`, `p_age_65_up` | double | yes | share of `n` | from L2 `age1`–`age4` |
| `p_white`, `p_black`, `p_hisp`, `p_other` | double | yes | share of `n` | L2 `race1`–`race4` |
| `p_col` | double | yes | share of **known** | `col_known/(col_known+noncol_known)` |
| `p_white_col`, `p_white_noncol`, `p_black_col`, `p_black_noncol`, `p_hisp_col`, `p_hisp_noncol` | double | yes | share of `n` | race × education |
| `turnout18`, `turnout20`, `turnout22`, `turnout24` | double | yes | **rate** in [0,1] | L2 `voteNN / n` |
| `est_turnout` | double | no | **count** | expected total votes. See §11 |
| `dem2` | double | no | share in (0,1) | **two-party** Dem baseline. See §11 |
| `oth_base` | double | no | share of all in [0,1) | third-party baseline |

**Validation:** `p_*` families each sum to ≤ 1 within tolerance; `turnout*` strictly in
[0,1]; `est_turnout > 0`; `dem2` and `oth_base` strictly interior.

**As built for TX (2026-10-19), `TX_precinct.parquet`:**
- `p_*` and `turnout*` use L2's own registrant count (`l2_n`) as the denominator.
  `p_white`…`p_other` exclude L2's `race_unk`, so they sum to less than 1.
- Precincts with no L2 match (TX: 610 precincts, 207,777 registrants; mostly Travis) get
  the county's registrant-weighted mean for every L2 field, flagged `l2_imputed = TRUE`.
  The design matrix therefore has no blanks; `lm_bayes` misaligns rows when covariates
  are NA.
- `dem2` / `oth_base`: from `pr_harris` / `pr_trump` until the frame carries `prec_*`
  (§11.1); the source is recorded per row in `dem2_source`, and the model version is
  kept as `dem2_model` / `oth_model`. `vote_imputed` flags the 231 precincts filled from
  the county average.
- `est_turnout` = vendor `turnoutPred × n`, capped at `n` (§11.3 still open).
- Extra columns: the join ids (`precinct26_code`, `precinct_l2`, `precinct_enr`,
  `l2_rule`), YouGov values (`yg_*`), `cd_main`, and the flags above.

---

## 3. `precinct_mode_history` — mode turnout composition

Grain: one row per `geo` × `election` × `method`. Source: L2
`vote{YY}_{mail,early,eday}`. **Available now:** TX built as
`TX_precinct_mode_history.parquet` (2016–2024 generals, 5,537 precincts with an L2 match).
In the 32 TX scope counties the 2022 split is mail 4.4% / early 65.0% / election day 30.5%,
and the mode columns cover 99.8% of 2022 voters (93.6% in 2016).

| field | type | null | unit | notes |
|---|---|---|---|---|
| `geo` | text | no | — | **PK** part |
| `election` | text | no | — | **PK** part, e.g. `2022_general` |
| `method` | text | no | — | **PK** part, §0 taxonomy |
| `voters` | int | no | count | voters of that mode in that precinct |
| `is_midterm` | bool | no | — | selection key; 2026 needs midterm analogues |

**Why `election` is in the key and not collapsed:** the mode split differs sharply by
election type. Measured for TX (election-day share): 2018 27.3%, 2020 13.2%, 2022 32.0%,
2024 20.2%. A "most recent general" composite reads like a presidential year and
understates election day by ~10pp for a midterm. **Do not pre-aggregate across elections.**

**Caveat to record in the table comment:** these count *current* registrants who voted
that way, so they are survivorship-biased — voters since deceased or moved are absent.

---

## 4. `race` — dimension

| field | type | null | notes |
|---|---|---|---|
| `race_id` | text | no | **PK**, e.g. `TX_gov`, `TX_sen`, `TX_house_07` |
| `state` | char(2) | no | |
| `office` | text | no | `gov` / `sen` / `house` / `sldl` / `sldu` / … |
| `district` | text | yes | null for statewide |
| `contest_date` | date | no | |
| `is_runoff_eligible` | bool | no | affects whether a 50% threshold matters |

---

## 5. `candidate` — dimension

Grain: one row per `race_id` × `candidate_id`.

| field | type | null | notes |
|---|---|---|---|
| `race_id` | text | no | **PK** part |
| `candidate_id` | text | no | **PK** part |
| `name` | text | no | as the feed reports it |
| `party` | text | no | `dem` / `rep` / other |
| `role` | text | no | `major` / `minor` / `writein` |

**The model groups by `role`, not by `name`.** It models the Dem share of the two-party
vote plus a third-party share, so it needs exactly two `major` candidates and any number
of `minor`/`writein`. Write-ins must appear here rather than being dropped, or total
turnout comes up short and every share is biased high.

**If a `minor` candidate is polling above roughly 10%**, flag it — the model needs to
promote them to their own outcome rather than pooling them into third party.

---

## 6. `race_prior` — statewide targets

Grain: one row per `race_id`. Source: `priors.csv`. **Available now for TX.**

| field | type | null | unit | notes |
|---|---|---|---|---|
| `race_id` | text | no | — | **PK** |
| `prior_dem` | double | yes | share of **all** votes | |
| `prior_rep` | double | yes | share of all votes | |
| `prior_oth` | double | yes | share of all votes | see below |
| `prior_turnout` | double | yes | count | statewide expected votes |
| `as_of` | date | no | — | |
| `source` | text | no | — | polling average, internal forecast, … |

**`prior_dem + prior_rep` does not sum to 1, and the residual is not third party.** For
`TX_gov` the current values are 0.463 / 0.485, leaving 5.2% — but third party actually
took ~1.4% in 2022. The gap is undecideds. **Do not feed the residual into `oth_base`;**
keep `prior_oth` as its own nullable field.

---

## 7. `race_mode_prior` — the mode partisan differential — **UNBLOCKED for TX 2024**

**Status 2026-10-19.** No longer blocked in principle: the 2024 TX scrape yields real
precinct × mode × party results, and δ_m is now measured rather than borrowed or assumed.
Built once from the raw county scrapes for a backtest (not in the pipeline path, see §10). Measured on TX 2024:

| method | `delta_a` | `delta_b` | two-party Dem of that mode |
|---|---|---|---|
| mail | +0.769 | −0.088 | 67.3% |
| early | +0.087 | −0.039 | 53.3% |
| eday | +0.082 | −0.288 | 55.9% |

**TX election day runs 2.6pp MORE Democratic than early voting** — the opposite of NC
(mail +0.660 / early −0.029 / eday −0.025) and the opposite of the red-mirage pattern this
field exists to capture. So `delta_source` must be recorded per state and **NC's values
must not be borrowed for TX**: on the 2024 backtest the NC borrow moves mean |err| from
0.60pp to 0.59pp, i.e. nothing, and even an oracle δ_m measured on the target election
reaches only 0.41pp. δ_m is a smaller lever than this section originally assumed.

Still wanted: the same measurement for the other in-scope states, which needs §10 for each.

Grain: one row per `race_id` × `method`. This is $\delta_m$ in the model.

| field | type | null | unit | notes |
|---|---|---|---|---|
| `race_id` | text | no | — | **PK** part |
| `method` | text | no | — | **PK** part |
| `delta_a` | double | yes | logit points | intercept of the offset |
| `delta_b` | double | yes | logit per logit | **slope on the precinct's own lean** |
| `delta_source` | text | no | — | `results` / `ei` / `assumed_zero` |
| `delta_se` | double | yes | logit points | needed if `delta_source = 'ei'` |

**It must be lean-dependent, not a constant.** Measured on NC 2024 (1,118 precincts,
MEDSL official returns — the only state with complete precinct x mode x party):

| mode | mean delta | slope on lean | t |
|---|---|---|---|
| mail | **+0.660** | **-0.297** | **-20.8** |
| early | -0.029 | +0.041 | 17.5 |
| eday | -0.025 | -0.089 | -11.8 |

So `delta_im = delta_a[m] + delta_b[m] * logit(dem2_i)`. Mail's Democratic skew is largest
in Republican-leaning precincts and shrinks as the precinct gets more Democratic; a single
number per mode is misspecified, not approximately right. Supplying a correct
lean-dependent offset cut the trial's error at 10% reported from 1.07pp to 0.17pp.

**Why blocked.** Every partisan source available is mode-blind, and every mode-resolved
source is partisan-blind — voter files record *how* people voted, never *for whom*. L2's
`vote22_{mode}_{dem,rep}` looks like a bridge but the "party" there is modeled affiliation
and fails validation: it puts TX 2022 voters at 53.5% two-party Dem against an actual
44.5%, and reads election day as *more* Democratic than early voting in both 2022 and
2024, which inverts the documented pattern.

**Unblocked by** §10, or by ecological inference crossing §3 with precinct partisan
outcomes.

**DECISION 2026-10-18: downgraded to nice-to-have.** No historical vote-share x mode source
exists without substantial cleaning. Verified by sweep: votehub `estimated_past_results.csv`
has `gov22_dem/rep/oth/total` but **no mode column**; the only TX-relevant files carrying
both party and mode are the L2 frame (modeled affiliation, not vote choice), the YouGov
method frame (codebook forbids crossing `pr_harris` with `method`), and the live summaries
(tonight's returns, not history). OpenElections covers 16 of the 32 scope counties at 97%
mode completeness but is missing Harris, Tarrant and Travis — a calibration sample, not a
production input. MEDSL has TX as VTD-level and mode-free. Proceeding with
`delta_source = 'assumed_zero'`.

**Partial substitute that needs no new data:** put each cell's modeled-Dem share,
`vote22_{mode}_dem / (vote22_{mode}_dem + vote22_{mode}_rep)`, into the share equation as a
COVARIATE rather than a prior. The level bias then does not matter — the regression learns
the modeled-party-to-vote-choice mapping from reported precincts and only needs the
covariate to be informative. Pooling that coefficient across modes lets mail and early
returns identify it and transfer it to unreported election-day cells, partially filling
this gap from data. Assumes the mapping is stable across modes, which the 2022-vs-2024
instability warns against; testable once returns arrive, and strictly better than zero.

**Default until then:** `delta_logit = 0`, `delta_source = 'assumed_zero'`. The model runs,
but carries the full red-mirage bias for as long as election day has not reported — which
is most of the night, since the prior for a mode's equations decays only as that mode
reports.

---

## 8. `returns_live` — the observations

Grain: one row per `geo` × `race_id` × `candidate_id` × `method` × `observed_at`.
This is the only table that changes during the night.

| field | type | null | unit | notes |
|---|---|---|---|---|
| `geo` | text | no | — | **must be reconciled to §1, see below** |
| `race_id` | text | no | — | |
| `candidate_id` | text | no | — | |
| `method` | text | no | — | §0 taxonomy, never `all` alongside others |
| `votes` | int | no | count | |
| `stage` | int | no | — | reporting completeness, see below |
| `observed_at` | timestamptz | no | — | when the scrape ran |

**Append-only, keyed by `observed_at`.** The model refits from scratch each run, and
`pct_rep` has been observed to go *backwards* in production (0.997 → 0.269 → 0.364 in the
TX primary). Do not overwrite; keep the history so regressions are visible.

**`stage` semantics must be documented per state**, because the model keys off it. In the
TX primary feed the emitted values were 0, 2 and 5 only — and code filtering on
`stage >= 6` therefore never fired, silently falling back to a hardcoded constant for
every run. **Required:** a `stage_meaning` lookup table giving, per state, which values
mean "nothing yet", "partially counted", and "complete".

**UPDATE 2026-10-19 — the practical vocabulary is 0 / 4 / 6, and 6 can take DAYS.**
Per the user, the best we can expect from a stage field is 0, 4 or 6, with 6 meaning
complete — and reaching 6 routinely takes days, well past election night. Three other
facts in this repo point the same way:
- `scripts/functions.R` actively DOWNGRADES `stage == 6` to 5 unless `trust_stage=TRUE`,
  i.e. the pipeline already treats a reported 6 as unreliable.
- the TX primary emitted 0/2/5, a different vocabulary again.
- `pct_rep` has been observed going BACKWARDS in production (0.997 → 0.269 → 0.364).

**Design consequence, and it is a hard one: no model component may GATE on completeness.**
A completeness signal that arrives days late, varies by state, is distrusted by our own
loader and is non-monotonic cannot carry any weight on election night. Measured
confirmation from the TX 2024 backtest: a plateau-based completeness proxy un-flagged 556
cells at 23:59 — the count of "complete" cells FELL — reproducing the same backwards
movement the real `pct_rep` shows.

What this means concretely for the model:
- the AGGREGATE is already safe: partial counts enter as a mixture
  (`dem = counted_dem + rem*(1-tp)*s`, `rem = tau - counted`), which needs no flag at all.
  A counted-out cell pins itself because `rem -> 0`.
- the TURNOUT EQUATION is NOT safe: it is still fitted on cells flagged counted-out, and
  that flag's instability is what makes the election-day turnout sigma oscillate between
  0.054 (prior floor) and 1.315 across adjacent stages. **Fix direction: treat counted as
  a RIGHT-censored observation of final turnout for every reporting cell, so no
  completeness signal is needed.** (Right-censored: we learn the true value lies ABOVE
  what we see, as with a subject still alive at study end. I first wrote "left" here,
  which is the opposite case. Conjugacy can be kept by data augmentation (impute the
  censored tau from the truncated normal, refit in closed form, iterate).**
- `STAGE_DONE = 2` in `model_turnout.R` is written against yet another vocabulary and must
  be replaced by the `stage_meaning` lookup once it exists.

**Blocker.** `precinct_enr` in the TX crosswalk is populated for 92 of 11,563 rows (0.8%),
covering two counties. Until that is filled, returns cannot be joined to covariates for
30 of 32 counties and the model cannot ingest them. This is independent of everything
about vote mode and is, in my read, the highest-priority item in this document.

---

## 9. `precinct_race` — allocation for sub-state races

Grain: one row per `geo` × `race_id`. **Required only if House or legislative races are in
scope**; statewide-only can omit it.

| field | type | null | unit | notes |
|---|---|---|---|---|
| `geo` | text | no | — | **PK** part |
| `race_id` | text | no | — | **PK** part |
| `share` | double | no | share in (0,1] | precinct's registrants in this district |

**Validation:** `share` sums to 1 per `geo` per office level.

---

## 10. `results_historical` — final canvass by mode and party — **PARTIALLY UNBLOCKED**

**Status 2026-10-19.** TX 2024 now exists, built from the raw county scrapes rather than
supplied (a one-off backtest file), 16 counties, 4 modes, in this
table's schema. It is deliberately **not** copied to
`26_general/input_data/TX_mode_history.parquet`, which is where the live model looks —
dropping it there would silently switch production off `delta_m = 0`, and that should be a
deliberate choice rather than a side effect. Activating it is a one-line copy.

Two caveats on what was built. It is the last election-night scrape per county, not the
certified canvass: measured against the canvass it is 99.28% of votes, with Dallas at
97.87%, so `is_final` is **false** for it. And it covers 16 of 254 TX counties — the ones
with election-night scrapes — not the state.

What is still wanted, in priority order: the certified canvass for TX 2024; and the same
table for the other in-scope states, which is what §13's backtest work needs and what would
let §7 be measured outside Texas.

Grain: one row per `geo` × `election` × `office` × `method` × `party`.

| field | type | null | unit | notes |
|---|---|---|---|---|
| `geo` | text | no | — | **PK** part |
| `election` | text | no | — | **PK** part |
| `office` | text | no | — | **PK** part. Prefer `gov`/`sen` over `president` |
| `method` | text | no | — | **PK** part |
| `party` | text | no | — | **PK** part: `dem` / `rep` / `oth` |
| `votes` | int | no | count | |
| `is_final` | bool | no | — | **must be true**; see below |
| `boundary_vintage` | text | no | — | §0 |

**This single table unblocks both §7 and the model's Σ.** It was the highest-value
acquisition in this contract, and for TX 2024 it has now been obtained — which immediately
showed that Σ's cross-mode blocks, borrowed from NC, are wrong for Texas where it matters
most: on share residuals TX gives early↔eday **+0.417** (t=22.3) against NC's +0.039, while
mail↔early is +0.049 against NC's +0.232. The pair NC says is negligible is the strongest
in TX. Getting this table for a second state is now the way to learn which pattern
generalises.

**`is_final` must be the certified canvass, not an election-night snapshot**, because
$\Sigma$ is estimated as the complete-case residual covariance across all modes. Partial
data biases it.

**Measured source availability (MEDSL official precinct returns, `Work/gh/*-elections-official`),
for the states currently in `26_general`:**

| state | 2022 | 2024 |
|---|---|---|
| NC | 4 modes, 100% of rows | 4 modes, 100% |
| AK | 5 modes, 100% | 5 modes, 100% |
| CA | absentee / not-absentee only | TOTAL only |
| MI | 2 modes, 44% of rows | TOTAL only |
| NY | 8 labels, 97% — inconsistent | 12 labels, **17%** — compound categories |
| FL | TOTAL only | TOTAL only |
| **TX** | **TOTAL only** | **TOTAL only** |

So this table is fillable **today for NC and AK** from official final returns, with no
vendor dependency.

**TX is not, for two reasons** (MEDSL 2022 README, Texas section). Its source is the Texas
Legislative Council **VTD-level** file: (a) that file carries no mode, so this is not
something MEDSL dropped; and (b) VTDs are census-equivalents of precincts and are
*distinct from the county-defined voting precincts* that counties actually report in — and
that our live feed, L2 and YouGov frame all use. So MEDSL TX will not join to `geo` even
for a `dem2` baseline or a backtest, without a VTD -> precinct crosswalk. MEDSL points to
OpenElections for county-precinct TX results:
`openelections/openelections-data-tx` issue 442. County canvasses are also where the mode
breakdowns live, so that is the lead to follow.

**CA and MI mode values are heuristic, not reported.** MEDSL reconstructs CA's
`ABSENTEE`/`NOT ABSENTEE` from an `A` suffix on precinct IDs and says of it: "imperfect
checks on a band-aid solution to an ambiguity which is baked into the original dataset.
Users would do well to double-check the face validity of any mode values they use from
this state." MI 2022's 44% coverage is the same kind of partial recovery. Treat both as
unusable for calibration.

**NY needs hand-built per-year rules** and a decision about its unlabeled majority; MEDSL
also flags unexplained precinct-vs-total vote discrepancies there.

**Data-quality flag:** CA 2022 has exactly 1,048,575 rows = 2^20 - 1, the Excel row limit,
and its last row's final field is empty where the preceding row's is not. All 58 counties
are present and it ends at YUBA (alphabetically last), so any loss is confined to YUBA's
tail — but verify against the upstream source before use.

**Mode values outside the `VOTE_METHODS` taxonomy need explicit mapping decisions.** MEDSL
emits `UNSPECIFIED`, `NON-FEDERAL`, `QUESTION`, `FAILSAFE`, `FAILSAFE PROVISIONAL`,
`LATE EARLY`, `SAME DAY REGISTRATION`, `UNSCANNED/MANUAL ENTRY` and compound labels such
as `ABSENTEE/EARLY VOTING BY MAIL PRE ELECTION DAY`. Each must be mapped or dropped
deliberately, and the choice recorded. Per MEDSL's README: splitting modes apart *while*
also retaining a `TOTAL` row double-counts.

**Recommended first step:** calibrate on NC, where both years are complete, to test whether
the mode differential and the cross-mode blocks of Sigma behave as the model assumes —
before committing to acquiring TX.

**Preference order: 2022 over 2024.** 2026 is a midterm; the 2022 governor's race is the
right analogue for both mode composition and the partisan differential. 2024 has better
coverage but a presidential electorate.

**Why $\Sigma$ cannot be estimated live instead:** modes report in sequence, so the number
of precincts with every mode counted is near zero until counting is essentially over. The
mail↔election-day block — the one that lets early returns inform the election-day
prediction — is never identified from the current night's data.

---

## 11. Decisions still open

1. **`dem2` source.** Options: `pr_harris`/`pr_trump` (2024 presidential, individual
   probabilities calibrated to precinct returns, averaged over 2024 voters);
   `prec_harris`/`prec_trump` (actual 2024 returns, already on `precinct26_code`, so no
   crosswalk needed); votehub `gov22_dem/rep` (2022 governor — right electorate, wrong
   ids). Recommend `prec_*` for now, `gov22` once reconciled.
   **Status 2026-10-19:** `prec_*` (and `prec_harris2p`) are in the raw voter file
   (`vf_frame_current_202609`) but NOT in `yougov_precinct_frame.parquet`; `create_frame.R`
   has to average them in (over 2024 voters, like `pr_harris`). Once it does,
   `prep_model_inputs.R` switches to them automatically and reports
   corr(logit returns, logit model) per §14.4.
2. **`oth_base` source.** `pr_other` exists as a column and should replace the
   `1 − pr_harris − pr_trump` residual. Better still is `gov22_oth/gov22_total`
   (mean 1.43%, p95 2.69%) once ids are reconciled.
   **Status 2026-10-19:** `pr_other` is also not in the frame parquet. Because each
   voter's `pr_harris + pr_trump + pr_other = 1`, the residual `1 − pr_harris − pr_trump`
   of the precinct averages equals the precinct's mean `pr_other`, so the current
   `oth_base` is already that quantity.
3. **`est_turnout`.** Either the vendor's `turnoutPred × n`, or built from L2 `vote18`/
   `vote22` history. The latter is more grounded but needs a midterm-trend assumption.
4. **`stratum_demo` definition.** The TX primary's version is a hard majority-threshold
   cascade with a 21% "other" catch-all and an order dependence (white tested first). Worth
   redesigning as continuous demographic covariates plus a coarser geographic effect before
   adopting it.
5. **Race scope** — statewide only, through congressional, or down to legislative. This
   determines whether §9 is needed and whether cross-race partial pooling is mandatory.

---

## 12. Minimum viable set

To run the model at all, in dependency order:

1. §1 `precinct` and §2 `precinct_covariate` — with `dem2` from any of the §11 options
2. §4 `race`, §5 `candidate`, §6 `race_prior`
3. §3 `precinct_mode_history` for a midterm election
4. §8 `returns_live` **with `precinct_enr` reconciled** — currently the binding blocker
5. §9 only if sub-state races are in scope

With §7 and §10 absent the model runs and converges, but early-night estimates carry a
known bias toward whichever party leads the early-reporting modes, and cross-mode
conditioning is inert for vote share. Both are acceptable for a trial; neither is
acceptable on air.

---

## 13. Backtest inputs — what a past election must provide

**Why this section exists.** Everything in §1-§12 describes what the model needs on
election night. This section describes what it needs to *validate* the model beforehand,
which is a different and in places stricter list. It is written from the TX 2024 backtest
(built 2026-10-18/19) and from a Georgia dataset that passed every obvious check and was
still unusable — see the acceptance tests, which exist because of that.

Grain: one row per `geo` x `method` x `observed_at`, per state per past election.

| field | type | null | notes |
|---|---|---|---|
| `geo` | text | no | **must reconcile to §1** — see the join test below |
| `method` | text | no | §0 taxonomy |
| `observed_at` | timestamptz | no | scrape time, **local or with an explicit offset** |
| `votes_dem` / `votes_rep` / `votes_oth` | int | no | candidate-specific, not precinct totals |
| `stage` | int | yes | if present, document the per-state vocabulary (§8) |

Plus, once per state per election, the **final canvass** at the same grain (`geo` x
`method` x party). That is the truth the backtest scores against, and it is not the same
as the last scrape: measured on TX 2024, the last election-night snapshot is **99.28%** of
the canvass, and Dallas alone lands at 97.87%.

### Semantics that must hold (and that bit us)

1. **Snapshots are cumulative within a county and counties are scraped independently.**
   A timestamp is therefore only comparable to other timestamps of the *same* county. Any
   statewide aggregate needs an as-of join (each county's latest snapshot at or before
   `t`). Summing per-county snapshots as if they were statewide totals produces a
   plausible-looking and wrong reporting curve.
2. **Election-night coverage is the thing that makes a state usable.** A scrape that only
   has pre-election or post-canvass snapshots is worthless for cadence work. On TX 2024,
   3 of 19 counties had *only* 2026-02 scrapes carrying zero votes.
3. **`votes_*` must be the candidate's votes**, not the precinct's total repeated on each
   candidate row.

### ACCEPTANCE TESTS — please run these and send the output with the data

These are cheap and each one caught a real defect:

- **T1 Turnout against official.** Sum the final canvass and compare to the published
  statewide turnout for that contest. *Georgia failed only here*: its two-party share was
  48.9% against an actual 48.9% — correct — while its vote total was 10.5M against an
  actual 5.27M. **A share-only sanity check passes this data.** Report both.
- **T2 Mode replication, at a SINGLE snapshot.** For each `(geo, candidate)` in one
  snapshot, are all of that cell's mode values identical and non-zero? In Georgia this
  was true for **64 of 159 counties**, which repeat the precinct total on every mode row
  and inflate any sum across modes by 4x. Report the per-county fraction.
  *This must be evaluated on ONE snapshot* — pooled across snapshots the test silently
  returns zero, because the four mode rows do not all appear in every snapshot.
- **T3 Night coverage.** Per county: number of snapshots between poll close and 08:00 the
  next morning, and the first/last of them.
- **T4 Join rate to §1**, per county, as `matched / total`. This is the binding
  constraint, not a formality — measured against our current frame, NC joins at **17%**
  and OH at **20%**, because L2 stores precinct NAMES (`BETHLEHEM 1`) where votehub stores
  ABBREVIATIONS (`B1`). 81 of 100 NC counties join at 0%.
- **T5 Party mapping.** List every distinct raw party string and its mapping. TX 2024
  needed this because Travis parenthesises the party (`(D)`, `(R)`) in *both* the party
  and candidate-name fields; a naive prefix test sent Travis's entire vote to `oth` and
  put panel third-party at 17.7% against a true 2.54%.
- **T6 Mode mapping.** List every distinct raw mode label and its mapping, with **zero
  unmapped**. Note state-specific semantics: in TX, "Absentee" means BY MAIL (2.1-4.7% of
  each county's vote, matching TX's restricted mail share), which is *not* transferable to
  states where absentee means in-person early.

### Priority states for backtesting

In descending order of what we would learn. The model has only ever been validated on one
state-election, so breadth matters more than depth here.

| state | why | known blocker |
|---|---|---|
| **NC** | full-state L2 coverage, 100 counties, has mode x party via MEDSL | the 17% join — needs a precinct name/abbreviation crosswalk |
| **GA** | full state, all four modes, 2024 scrape exists | the T2 replication problem in 64 counties |
| **MI** | dense election-night sampling (141 night snapshots) | only 4 counties in the scrape |
| **FL** | 97.6% join already | confirm night coverage |
| **TX** | **done** — the reference implementation | — |

---

## 14. 2026 prep datasets — per state, before election night

Scope is the 12 races in `26_general/input_data/priors.csv`: **AK, CA, FL, MI, NC, NY, TX**
(Senate / Governor / House as listed). Crosswalks exist for all but **NY**.

The intent of this section is that we do no data wrangling during the run-up. Each item
below is a deliverable with an acceptance test.

### 14.1 The precinct spine and its crosswalk — **highest priority**

One row per 2026 precinct, carrying **every id we will need to join on**:

| field | notes |
|---|---|
| `geo` | our canonical key |
| `precinct_l2` | L2 frame id |
| `precinct_votehub` | votehub `unit_id`, for historical results |
| `precinct_enr` | **the id the live feed will emit** |
| `precinct_shp` | shapefile id, if mapping |
| `county_fips` | 5-digit |

**UPDATE 2026-10-19 — largely mitigated, and not the way this section expected.**
`precinct_enr` is still populated for only 92 of 11,563 TX rows (0.8%), but the §0
normalization rule removes most of the dependency on it: stripping leading zeros takes
Harris live ids from **15.8% to 99.6%** matched against the frame. So the live feed can be
joined without a filled `precinct_enr` wherever that rule works. What remains worth
having is `precinct_enr` for the counties where it does not.

**And a correction to the instinct this section encouraged.** Do NOT close crosswalk gaps
by normalizing ids. For 92 TX precincts a normalized-id match exists in L2 and is WRONG —
the pairs are different places sharing a number, with registrant counts differing by a
median of 35% (El Paso `0054`: 13,275 in the frame vs 1,989 in L2) against ~1% for genuine
crosswalk matches. The blanks are correct. A high match rate achieved by loosening the key
is not the same as a good join, and the registrant-weighted rate (98.2% for L2, against
90.3% of precincts) is the figure to judge by.

*Acceptance:* per county, the fill rate of each id column; a round-trip test showing a
sample of live-format ids joining to `geo`; and the join rate reported BOTH per precinct
and registrant-weighted.

### 14.2 Registrants per precinct

`n` = registered voters. Used as a **ceiling**: the model now bounds precinct turnout at
`KAPPA * n` (default 1.25), which is what stopped turnout estimates reaching 184% of
truth.

As of 2026-10-19 `n` is the YouGov frame's registrant count (complete for every
precinct), with L2's carried as `l2_n` and L2 gaps filled by the county
registrant-weighted mean flagged `l2_imputed` — so the design matrix has no blanks.

*Acceptance:* the distribution of `historical turnout / n`. On TX 2024 this is median
0.654, p99 1.385, and **2.4% of precincts exceed 1.0** — which turned out to be a *join
artifact detector*: the violations cluster in FORT BEND (15.0% of its precincts, max ratio
4.50) and BELL (9.4%), the two counties with the worst join rates. Please report the
per-county exceedance rate; it is a free data-quality signal.

### 14.3 Mode turnout history (feeds §3)

Per precinct, votes by mode for **each** of the last several comparable elections — not
just one analogue year, and please label them rather than pre-selecting.

*Why:* the analogue-year choice is a real modeling decision and the obvious pick is often
wrong. TX mode composition tracks *turnout type*, not calendar — midterms run 27-32%
election-day, presidentials 13-20%. For the 2024 target: 2020 gives 7.15/81.71/11.15,
2022 gives 4.03/65.23/30.74, and the actual was 3.70/77.21/19.10. Neither analogue is
close, because 2020 was anomalous. We need the series to choose from, per state.

### 14.4 Partisan baseline (`dem2`, feeds §2)

Prior-cycle two-party Dem share per precinct, from **results** rather than modeled
partisanship, plus the third-party share from the same contest.

*Do not* substitute L2 modeled party: measured on TX, registrants read 64.1% two-party
Dem against an actual 2024 presidential 43.1%, and its mode differential is directionally
wrong (election day reads *more* Democratic than early in both 2022 and 2024, with the
ordering flipping between cycles).

*Acceptance:* `corr(logit(dem2), logit(actual))` at precinct level against the most recent
completed contest. TX 2024 gives **0.958**; anything far below that needs investigating
before we rely on it.

### 14.5 `stage` / completeness semantics, per state

Document which emitted values mean "nothing yet", "partially counted", and "complete", per
state and per feed.

*Why this is not bookkeeping:* the model is now built so that **nothing gates on
completeness** — partial counts enter as a vote-weighted mixture and a counted-out cell
pins itself — precisely because this field cannot be relied on. Realistic values are
0 / 4 / 6 with 6 meaning complete, and 6 can take **days**. The TX primary emitted only
0/2/5, so code filtering `stage >= 6` never fired. Our own loader downgrades a reported 6
to 5 unless `trust_stage=TRUE`. And `pct_rep` has been observed moving *backwards* in
production (0.997 -> 0.269 -> 0.364). We still want the semantics documented so the few
places that do read it are correct — but please do not spend effort making the field
reliable, because the model no longer depends on it.

### 14.6 Delivery order

1. §14.1 `precinct_enr` for the 7 in-scope states — nothing else matters until this lands
2. §14.2 registrants + the exceedance report (cheap, and it validates §14.1)
3. §14.4 `dem2`
4. §14.3 mode history series
5. §13 backtest data for NC and GA, with T1-T6

---

## 15. Model outputs — what the model emits

**Why this section exists.** §1-§14 run one way: they specify data the model must be
*given*. Nothing in them says what it *produces*, so the output spec has lived in three
unversioned places — `enightmodels::COLS_PRED`, `live/README.md`, and the header of
`live/R/output.R` — and a dashboard author or a new model author has had nothing to build
against. This section is that document. Consumer: the dashboard (`index.qmd`) and anyone
reading the night's files afterwards. Producer: `live/R/output.R`.

Four artifacts, written to `26_general/model_results/` in the shared Dropbox:

| artifact | schema | written by |
|---|---|---|
| `predictions_{stamp}.csv` + `predictions_latest.csv` | `enightmodels::COLS_PRED` + the extras in §15.1 | `write_predictions()` |
| `status_{stamp}.csv` + `status_latest.csv` | §15.2 | `write_predictions()` |
| `turnout_{stamp}.csv` + `turnout_latest.csv` | §15.4 — **new** | `write_predictions()` |
| `precincts_{stamp}.parquet` + `precincts_latest.parquet` | §15.3 — **new** | `write_precincts()` |

`{stamp}` is `max(status$time)` as `%Y%m%d_%H%M%S` in `America/New_York`. The stamped file
is never overwritten, so the night's history is kept; the `_latest` copy is written to a
hidden `.tmp` and renamed, so a reader never sees half a file. Both forms exist for all
four artifacts — a consumer polling `_latest` and a consumer reconstructing the night
from the stamped series must see the same thing. Test runs point `dir` elsewhere
(`tempdir()`), never at `model_results/`.

### 15.1 `predictions` — the prediction table

**The schema is `enightmodels::COLS_PRED`: exactly 16 columns, in that order.** The
package is the single source of truth. The list below is for readability only; where it
and `COLS_PRED` disagree, the package wins, and nothing in this section may be read as
re-specifying it.

| column | meaning |
|---|---|
| `state`, `election`, `jurisdiction`, `contest`, `district` | the race (enightmodels `RACE_ID`) |
| `candidate`, `party` | one row per candidate |
| `pred_type` | `"vote"` = predicted final votes; `"share"` = predicted final share of ALL votes cast in the race, as `pred_convert(to = "share")` defines it |
| `time` | the returns snapshot the predictions are from |
| `model` | `swing`, `ratio_strata`, `pois_bayes`, `medsl_sur` by default; `lm_bayes` only when asked for (see below). A variant run with a demographic spec or another interval is labelled `<model>[/<spec>][/<interval>]`, e.g. `pois_bayes/strat_d/conformal` (`live/README.md`). Renamed 2026-10-27 from `model_simpleswing`, `model_cory`, `model_cory_pbayes`, `model_cory_prec`, `model_hybrid`; `model_medsl` and `model_cory_pois` are no longer run |
| `model_conf` | how the interval was made and at what level — see below |
| `pct_rep` | share of the race's precincts with any votes counted. **The same definition for every model**, so the column lines up across them |
| `rep` | reported so far (votes, or share of the votes counted so far) |
| `pred` | predicted final value |
| `pred_min`, `pred_max` | interval ends; NA for a model with no interval |

Every row must pass `enightmodels::check_pred_data()`. That function, not this document,
defines the legal `pred_type` values, the `[0, 1]` bound on `pct_rep`, and the
non-negativity of `rep`/`pred`/`pred_min`/`pred_max`. Do not duplicate those constraints
here or anywhere else; a second copy is a second thing to get out of date.

**Call it through `check_pred()` in `live/R/output.R`, not directly**, for two measured
reasons that the wrapper documents in full:
1. `check_pred_data()` cannot return for ANY input — its last block reads
   `if (isTRUE(pred))` and `pred` is not one of its arguments. All ten substantive checks
   run and pass first, so the wrapper swallows that one message and re-raises everything
   else. Reported upstream; not patched locally, because `renv.lock` pins the package by
   SHA and the installed copy has no R sources, so a patch would evaporate on a fresh
   checkout — which is the night.
2. `district` is NA for every statewide race, and an all-NA column carries no type in a
   CSV, so `readr` reads it back as `logical` and the package's `is.integer` assertion
   fails. The wrapper coerces exactly that case. **Writing it as integer does not help:**
   `NA_integer_` still writes as an empty field.

**Emit `pred_type` `"vote"` and `"share"` only.** `check_pred_data()` also accepts
`"margin_vote"` and `"margin_share"`, but it requires `pred >= 0`, so a margin row for the
trailing party fails validation. **Margins are the dashboard's job** — it differences the
`dem` and `rep` rows itself. Do not emit margin rows and do not clamp one to zero to make
it pass.

**Extras beyond the 16 are permitted by the schema; exactly four are sanctioned.**

| extra | meaning |
|---|---|
| `p_win` | probability the candidate finishes ahead of the other major-party candidate. NA without draws, and on third-party rows |
| `geo` | which counties the row covers — see below |
| `cbs_as_of` | the time of the CBS county snapshot used, on statewide rows only |
| `race_id` | `priors.csv`'s key for the race, so §15.2 joins without reconstruction |

A fifth extra requires this section to be updated first. The point of naming them is that
a consumer can reject an unexpected column rather than silently ignore it.

**`race_id` is carried, not reconstructed, and that is deliberate.** It is
`<STATE>_<contest>[_<district>]`, and `COLS_PRED` has no slot for it, so `parse_race_id()`
(`live/R/races.R`) decomposes it into `state` / `contest` / `district`. **That decomposition
is not invertible:** it SYNTHESISES `district = 1` for house races in at-large states, so
`AK_house` arrives as contest `house` with district `1` and no district in its id. Measured
on the current 12 races, `paste(state, contest, sep = "_")` recovers `race_id` 12 of 12
while `paste(state, contest, district, sep = "_")` recovers 11 of 12 — and a districted id
such as `NY_house_12` breaks the first rule. No paste rule covers both shapes, and picking
the wrong one fails *silently*: an anti-join reports every row as missing rather than
erroring. One column removes the class.

**`geo` takes two literals and nothing else: `"scraped"` and `"all"`.** `"scraped"` is the
counties we scrape precinct by precinct — the models themselves. `"all"` is statewide: the
scraped counties from the model plus every other county from CBS's county results
(`counties.R`). **`geo` is not a precinct key**, despite being the name §0 gives the
canonical precinct id everywhere else in this contract. Nothing in this table is ever at
precinct grain; that is precisely why §15.3 exists as a separate artifact. Anyone joining
`predictions.geo` to a precinct spine has made a mistake, and it will join at 0%.

**`model_conf` is `{method}_{level}`, and `{method}_{level}_plus_rest` on statewide rows**
(`geo = "all"`), because a statewide interval is the model's interval composed with the
CBS remainder and is not the same object.

| model | `model_conf` |
|---|---|
| `swing` | `county_bootstrap_95` (`/conformal`: `approx_conform_95`) |
| `ratio_strata` | NA: no interval by default (`/conformal`: `approx_conform_95`) |
| `pois_bayes`, `lm_bayes` | `posterior_95` (`/conf_seq`: `conf_seq_95`; `/conformal`: `approx_conform_95`) |
| `medsl_sur` | `posterior_95` |

The methods are defined in `live/R/results.R`. `conf_seq_95` and `approx_conform_95` rows
have no `p_win`, and their statewide rows carry a point with NA `model_conf`.

**`lm_bayes` is implemented but NOT in the default run**, on cost rather than correctness.
Measured on `tx_base` at 22:00 with 2 races x 5 models, the refresh took 425s and lm_bayes
(then `model_cory_prec`) was **423s** of it. Its runtime is flat in draws and linear in
cells, against a 300s cadence. `MODELS` in `live/R/run.R` is the implemented set;
`RUN` is what runs. `--models=...,lm_bayes` still reaches it.

*Acceptance:* `check_pred(read_csv("predictions_latest.csv"))` passes — it folds in the
column-order assertion below, so this is one call, and it is the call to use rather than
`check_pred_data()` directly (see above);
`setdiff(names(d), c(COLS_PRED, "p_win", "geo", "cbs_as_of", "race_id"))` is empty;
`unique(d$pred_type)` is a subset of `{vote, share}`; `unique(d$geo)` is a subset of
`{scraped, all}`; and every row with `geo == "all"` has a non-NA `cbs_as_of` and a
`model_conf` ending `_plus_rest` or NA.

### 15.2 `status` — one row per race x model, always

| column | meaning |
|---|---|
| `race_id` | the race |
| `model` | the model |
| `time` | the returns snapshot attempted |
| `status` | `"ok"`, `"ok (partial prior: ...)"`, `"prior_only: ..."`, `"no_votes"`, or `"error: ..."` with the message |
| `statewide` | whether `geo = "all"` rows were added, or why not |
| `seconds` | wall-clock for that model on that race |
| `settings` | the model's resolved settings, enough to reproduce the fit |
| `cbs` | provenance of the county snapshot used, e.g. `"not pulled; using the snapshot saved 2026-11-04 03:00:00 UTC"` |

`race_id` here is `priors.csv`'s key (`TX_gov`), **not** §15.1's `contest` (`gov`). §15.1
carries `race_id` as a sanctioned extra precisely so the two tables join on it directly —
do not reconstruct it from `state` and `contest`, for the reason given there.

**`prior_only` is not a flavour of `ok`.** A fit whose equations have all fallen back to
their prior returns the same columns, the same point estimates and the same intervals as a
real one — nothing in §15.1 distinguishes them. So the degradation is reported here instead,
in three levels:

| status | meaning |
|---|---|
| `ok` | every equation was fitted on tonight's returns |
| `ok (partial prior: N of M equations[; modes gated: ...])` | some equations fell below `MIN_FIT_OBS` or a mode was held at prior by the support gate. Normal early in the night — in TX the reference mode's turnout equation sits at prior until mail reports, because it needs `q_share >= MIN_MODE_SHARE`. |
| `prior_only: N of M equations at prior, K observed cells of C` | **the output carries no information from tonight, and NO PREDICTION ROWS ARE EMITTED.** The race x model appears here with the reason and nowhere in §15.1, which is "no prediction and we say why" rather than a silent gap. |

A consumer should test `fit$prior_only$all_prior` rather than parse the string; `fit_medsl()`
returns the structured ledger alongside the status.

**The invariant this table exists for: a race x model with no prediction appears here,
never silently vanishes.** A model that errors, or that has no votes to work with, produces
no rows in §15.1 — the only way to tell that apart from "we never tried" is a status row.
So `status` is written on every run, for every race x model in the run, including the
successful ones, and it is written even when `predictions` is empty.

*Acceptance:* for one run, every race in `priors.csv` x every model in `RUN` (or `--models=`) has exactly
one `status` row; `nrow(status) == n_races * length(RUN)`; and every race x model absent
from `predictions_latest.csv` has a `status` that is not `"ok"`.

### 15.3 `precincts` — the per-precinct dump

**New artifact.** A precinct-level prediction frame already exists in memory every refresh
(`fit$cells`) and `output.R` drops it on the floor. This writes it.

Grain: one row per `geo` x `race_id` x `model` x `time`.

| column | meaning |
|---|---|
| `geo` | canonical precinct id (§0). **Here it is a precinct key** — unlike §15.1's `geo` |
| `county` | county, as the spine carries it |
| `model` | which model produced the row (§15.1's values) |
| `race_id` | the race |
| `time` | the returns snapshot |
| `turn_est` | predicted final turnout, in votes |
| `turn_sd`, `turn_lo`, `turn_hi` | its sd and interval ends |
| `dem_est` | predicted Dem share of **ALL** votes cast |
| `dem_sd`, `dem_lo`, `dem_hi` | its sd and interval ends |
| `est_dem2` | predicted **two-party** Dem share |
| `cnt_prec` | votes counted so far in that precinct |
| `q_share` | share of that precinct's expected votes already counted |
| `n_obs_modes` | number of modes with any observation in that precinct |

`dem_est` and `est_dem2` are both required and are on **different scales**. `dem_est` is
share of all votes; every headline figure and every coverage check is two-party. Carrying
only one of them is how an estimate gets compared against truth on the wrong scale.

Interval ends come from the `level` threaded through the call, not a hardcoded
`0.025`/`0.975`, and they are at the same level as §15.1's `model_conf`.

**Parquet, not CSV, and this is not a preference.** Precincts x races is about 52,800 rows
per model per refresh (CA alone is 23,685 x 1), so ~211k rows per refresh across four
models and ~30M rows over a 12-hour night at a 5-minute cadence. Partition by state and
stamp.

A model that cannot supply a precinct frame emits **no rows** and a note in §15.2 — not
NA-filled rows. A consumer must be able to tell "this model has no precinct view" from
"this precinct has no estimate".

**Residuals are deliberately not in this round.** No truth column, no residual, no error.
The point of this pass is to look at the dump before deciding what to measure on it;
adding a residual now fixes a comparison scale before we know which one is right. Same for
a per-precinct `p_win`: either it is computed from retained draws or this section says we
do not have it — for this round, we do not.

*Acceptance:* one `run_night.R --sim` pass writes `precincts_latest.parquet` with exactly
one row per `geo` x `race_id` x `model` for every model that supplies a frame; no NA in
`geo`, `county`, `model`, `race_id`, `time` or `turn_est`; the interval columns non-NA for
every model with draws; `est_dem2` in `[0, 1]`; the count of precincts with
`cnt_prec > turn_est` reported rather than left silent, since a precinct counting past its
own prediction is a signal, not a rounding artifact; and `n_obs_modes <=` the number of modes in that state's returns — asserted
explicitly, because the existing flag-gated dump gets this wrong, reading 10,956 on a
227-registrant precinct.

### 15.4 `turnout` — the race total and its interval

**New artifact (2026-10-29).** Every fit already computes `est_turnout`, `lo_turnout` and
`hi_turnout` (`model_result()`, `live/R/results.R`), and §15.1 has no place for them: it has
one row per candidate, and the candidates' `pred_min` / `pred_max` do not add up to the
total's interval (a sum of quantiles is not the quantile of the sum). So the total goes in
its own table, `turnout_<stamp>.csv` + `turnout_latest.csv`, written by
`write_predictions()` beside the other two.

Grain: one row per `race_id` x `model` x `geo` x `time`, for exactly the race x model x geo
combinations that have §15.1 rows. The columns are named as in §15.1 so a consumer can treat
a turnout row as a prediction row.

| column | meaning |
|---|---|
| `race_id`, `model`, `time`, `geo`, `model_conf`, `pct_rep` | as §15.1 |
| `rep` | votes counted so far, all candidates |
| `pred` | predicted final votes: the race's §15.1 `"vote"` rows summed, so it matches them exactly |
| `pred_min`, `pred_max` | the total's interval ends, at §15.1's level; NA when the model gives none |

Where the interval comes from:

| model / interval | `geo = "scraped"` | `geo = "all"` |
|---|---|---|
| bootstrap, credible (draws) | quantiles of the draws of the total | quantiles of scraped + rest-of-state draws |
| `conf_seq` | `conf_seq()` on the log total | scraped bounds + the rest projected at the turnout-ratio ends, plus the rest's noise quantiles (`rest_turnout_bounds()`, `counties.R`) |
| `conformal`, `none` | NA: conformal shifts shares only, so turnout is a point | NA |

*Acceptance:* every race x model x geo in `predictions_latest.csv` has exactly one row here;
`pred` equals the sum of its `"vote"` rows; `pred_min <= pred_max` where both are non-NA.

### 15.5 Delivery order

1. **§15.1 and §15.2** — these describe what `output.R` already emits, so the work is the
   assertions, not the writer: the sanctioned-extras check, the `geo`-literals check, and
   the race x model completeness check. Cheap, and they are what catches a schema drift
   introduced by a later model.
2. **§15.3** — new writer (`write_precincts()`), plus the `prec_out` columns it needs that
   do not exist yet: `turn_sd`/`turn_lo`/`turn_hi`, `est_dem2`, `cnt_prec`, and `level`
   threaded instead of hardcoded. Per-model adapters, because the `cells` slot is not
   uniform across models today.
3. ~~`model_cory_prec`'s `model_conf`~~ — settled 2026-10-27: `posterior_95` by default,
   `conf_seq_95` as the `lm_bayes/conf_seq` variant (§15.1).

Deferred: residuals and per-precinct `p_win` in §15.3; margin rows anywhere (§15.1 — the
dashboard's job, permanently).
