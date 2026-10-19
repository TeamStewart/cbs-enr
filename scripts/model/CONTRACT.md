# Data contract — race-by-race election-night model (2026)

**Status:** draft 1, 2026-10-18. Owner: Young-Kyung Kim. Consumer: `scripts/model/tx_gov_trial.R`
and its successors. See `tx_gov_model.tex` for the model these inputs feed and
`nyc_tx_comparison.tex` for why it is shaped this way.

This specifies what the model needs, at what grain, in what units. It is written so a
database can be mapped to it field by field. Sections marked **BLOCKED** are inputs we do
not yet have; they are specified anyway so the schema can carry typed placeholders rather
than be reshaped later.

---

## 0. Conventions that apply everywhere

**Canonical precinct id.** One string, `geo`, formatted `{st}_{county_cbs}_{precinct_id}`
with `st` lowercase two-letter and `county_cbs` zero-padded to 3 digits —
e.g. `tx_257_0001`. This is the live-scraper format and the spine of every join.

**ID reconciliation is the main failure mode, not a detail.** Measured on TX as of
2026-10-18, matching each source to the covariate frame:

| source | match rate | note |
|---|---|---|
| L2 (`precinct_l2` via crosswalk) | 93.5% | usable |
| L2 (direct `county` + `precinct`) | 62.4% | **do not join directly** |
| live ENR feed (`precinct_enr`) | **0.8%** | hard blocker, see §8 |
| votehub past results | 88% / 38% / 0% | varies by county (BEXAR / KAUFMAN / BELL) |

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
| `precinct_raw` | text | no | — | the county's own label, pre-normalisation |
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
| `n` | int | no | count | registrants |
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

---

## 3. `precinct_mode_history` — mode turnout composition

Grain: one row per `geo` × `election` × `method`. Source: L2
`vote{YY}_{mail,early,eday}`. **Available now.**

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

## 7. `race_mode_prior` — the mode partisan differential — **BLOCKED**

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

## 10. `results_historical` — final canvass by mode and party — **BLOCKED**

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

**This single table unblocks both §7 and the model's $\Sigma$.** It is the highest-value
acquisition in this contract.

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

**NY needs hand-built per-year rules** and a decision about its unlabelled majority; MEDSL
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
2. **`oth_base` source.** `pr_other` exists as a column and should replace the
   `1 − pr_harris − pr_trump` residual. Better still is `gov22_oth/gov22_total`
   (mean 1.43%, p95 2.69%) once ids are reconciled.
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
