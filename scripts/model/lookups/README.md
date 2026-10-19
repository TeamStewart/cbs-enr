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
