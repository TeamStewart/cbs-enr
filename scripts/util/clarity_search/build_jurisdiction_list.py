"""
build_jurisdiction_list.py

Builds data/jurisdiction_list.csv — one row per (state, county) combination
across the whole US, with a flag for every place ever seen on Clarity Elections
via Common Crawl historical index.

Columns
-------
state            2-letter abbreviation
county           Plain county name, spaces not underscores (blank = statewide)
seen_on_clarity  1 if this combo has appeared in CC CDX data, else 0
search           (empty) — fill in manually to flag places for deeper scanning

Sources
-------
1. US Census Bureau national county list (2020 vintage) — canonical jurisdiction list
2. Common Crawl CDX API — all indexes from 2018 onward, paginated

Usage
-----
python scripts/build_jurisdiction_list.py
"""

import csv
import io
import json
import os
import re
import time

import requests

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:42.0) Gecko/20100101 Firefox/42.0",
    "Accept": "application/json, text/html, */*",
}
REQUEST_DELAY = 0.25

CENSUS_URL = (
    "https://www2.census.gov/geo/docs/reference/codes2020/national_county2020.txt"
)
CC_COLLINFO_URL = "https://index.commoncrawl.org/collinfo.json"
CDX_BASE = "https://index.commoncrawl.org/{index}-index"
CLARITY_DOMAIN = "results.enr.clarityelections.com"

# Minimum CC index year to query (2018 = first major Clarity expansion)
MIN_YEAR = 2018
# Max CDX pages per index (5 000 records each)
CDX_LIMIT = 5000
CDX_MAX_PAGES = 20  # up to 100 000 records per index

OUTPUT_CSV = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "jurisdiction_list.csv"
)

# County-suffix words to strip from Census names
# e.g. "Autauga County" → "Autauga"
_SUFFIX_RE = re.compile(
    r"\s+(County|Parish|Borough|Census Area|Municipality|city|City"
    r"|Borough and Census Area|Island|District|Municipio)$",
    re.IGNORECASE,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def safe_get(url, params=None, timeout=30):
    try:
        resp = requests.get(url, headers=HEADERS, params=params, timeout=timeout)
        time.sleep(REQUEST_DELAY)
        return resp
    except Exception as exc:
        print(f"    [warn] {url}: {exc}")
        return None


def strip_county_suffix(name):
    """'El Paso County' → 'El Paso'"""
    return _SUFFIX_RE.sub("", name).strip()


def clarity_to_plain(clarity_county):
    """'El_Paso' → 'El Paso'"""
    return clarity_county.replace("_", " ").strip()


# ---------------------------------------------------------------------------
# Step 1: Canonical county list from Census
# ---------------------------------------------------------------------------

def get_census_counties():
    """
    Returns list of (state_abbrev, plain_county_name) tuples for all
    ~3 100 US counties/county-equivalents, sorted by state then county.
    """
    print("Fetching Census county list …")
    resp = safe_get(CENSUS_URL, timeout=30)
    if resp is None or resp.status_code != 200:
        raise RuntimeError(f"Census fetch failed: {getattr(resp, 'status_code', 'N/A')}")

    reader = csv.DictReader(io.StringIO(resp.text), delimiter="|")
    rows = []
    for row in reader:
        state = row["STATE"].strip().upper()
        raw_name = row["COUNTYNAME"].strip()
        plain = strip_county_suffix(raw_name)
        if state and plain:
            rows.append((state, plain))

    rows.sort(key=lambda r: (r[0], r[1]))
    print(f"  {len(rows)} county/equivalents from Census")
    return rows


# ---------------------------------------------------------------------------
# Step 2: CC CDX — all indexes from MIN_YEAR onward, paginated
# ---------------------------------------------------------------------------

def get_cc_indexes():
    """Return list of CC index IDs from MIN_YEAR onward, newest first."""
    resp = safe_get(CC_COLLINFO_URL)
    if resp is None or resp.status_code != 200:
        raise RuntimeError("Could not fetch CC index list")
    all_indexes = resp.json()
    filtered = []
    for ix in all_indexes:
        idx_id = ix.get("id", "")
        # Parse year from CC-MAIN-YYYY-WW
        m = re.match(r"CC-MAIN-(\d{4})", idx_id)
        if m and int(m.group(1)) >= MIN_YEAR:
            filtered.append(idx_id)
    print(f"  {len(filtered)} CC indexes from {MIN_YEAR}+")
    return filtered


def parse_clarity_url(url):
    """
    Returns (state, county_or_None) from a Clarity Elections URL.
    county is the raw Clarity string (underscores); state is uppercase.
    Returns None if unrecognised.
    """
    m = re.search(
        r"results\.enr\.clarityelections\.com"
        r"/([A-Z]{2})"            # state
        r"(?:/([^/\d][^/]*))?",   # optional county (doesn't start with digit)
        url,
        re.IGNORECASE,
    )
    if not m:
        return None
    state = m.group(1).upper()
    county_raw = m.group(2)  # None for statewide
    return (state, county_raw)


def cdx_scan_all(indexes):
    """
    Query CDX for every index, paginated.
    Returns set of (state, plain_county_or_None) tuples seen on Clarity.
    """
    seen = set()  # (state, plain_county or None)

    for idx_num, index in enumerate(indexes, 1):
        page_hits = 0
        for page in range(CDX_MAX_PAGES):
            params = {
                "url": f"{CLARITY_DOMAIN}/*",
                "output": "json",
                "limit": CDX_LIMIT,
                "offset": page * CDX_LIMIT,
            }
            url = CDX_BASE.format(index=index)
            resp = safe_get(url, params=params)
            if resp is None or resp.status_code != 200:
                break

            lines = [l.strip() for l in resp.text.splitlines() if l.strip()]
            if not lines:
                break

            for line in lines:
                try:
                    record = json.loads(line)
                except json.JSONDecodeError:
                    continue
                parsed = parse_clarity_url(record.get("url", ""))
                if parsed:
                    state, county_raw = parsed
                    plain = clarity_to_plain(county_raw) if county_raw else None
                    seen.add((state, plain))

            page_hits += len(lines)
            if len(lines) < CDX_LIMIT:
                break  # last page

        print(f"  [{idx_num:3d}/{len(indexes)}] {index}: {page_hits} records, "
              f"{len(seen)} unique jurisdictions so far")

    return seen


# ---------------------------------------------------------------------------
# Step 3: Build and write the output CSV
# ---------------------------------------------------------------------------

def build_csv(census_counties, clarity_seen):
    """
    Merge Census county list with Clarity sightings.
    One row per (state, county); blank county = statewide.
    clarity_seen: set of (state, plain_county or None)
    """
    # Collect all states from Census
    all_states = sorted({s for s, _ in census_counties})

    rows = []

    for state in all_states:
        # Statewide row (county blank)
        statewide_seen = 1 if (state, None) in clarity_seen else 0
        rows.append({
            "state": state,
            "county": "",
            "seen_on_clarity": statewide_seen,
            "search": "",
        })

        # County rows for this state
        state_counties = [(s, c) for s, c in census_counties if s == state]
        for _, county in state_counties:
            seen = 1 if (state, county) in clarity_seen else 0
            rows.append({
                "state": state,
                "county": county,
                "seen_on_clarity": seen,
                "search": "",
            })

    return rows


def write_csv(rows):
    os.makedirs(os.path.dirname(OUTPUT_CSV), exist_ok=True)
    fieldnames = ["state", "county", "seen_on_clarity", "search"]
    with open(OUTPUT_CSV, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)
    print(f"\nWrote {len(rows)} rows to {OUTPUT_CSV}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print("=== Step 1: Census county list ===")
    census_counties = get_census_counties()

    print("\n=== Step 2: Common Crawl CDX scan ===")
    indexes = get_cc_indexes()
    clarity_seen = cdx_scan_all(indexes)

    # Summary of Clarity sightings
    county_seen = {(s, c) for s, c in clarity_seen if c is not None}
    state_seen = {s for s, c in clarity_seen if c is None}
    print(f"\n  Clarity sightings: {len(county_seen)} county-level, "
          f"{len(state_seen)} statewide")

    print("\n=== Step 3: Building output CSV ===")
    rows = build_csv(census_counties, clarity_seen)

    seen_count = sum(1 for r in rows if r["seen_on_clarity"] == 1)
    print(f"  {seen_count} / {len(rows)} rows marked seen_on_clarity=1")

    write_csv(rows)

    print("\nDone. Open data/jurisdiction_list.csv and fill in the 'search' column.")
    print("  search=1  → include in deeper scan")
    print("  (blank)   → skip")


if __name__ == "__main__":
    main()
