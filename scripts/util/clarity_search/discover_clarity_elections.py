"""
discover_clarity_elections.py

3-layer discovery of Clarity Elections sites with election ID > MIN_ID.

Layer 1: Common Crawl CDX API — historical URL baseline
Layer 2: /{STATE}/elections.json + participatingcounties — comprehensive state/county coverage
Layer 3: /{STATE}/{COUNTY}/ listing pages — catch county-only elections

Output: data/discovered_elections.csv
"""

import csv
import json
import os
import re
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import requests
from bs4 import BeautifulSoup

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

MIN_ID = 125162
BASE_URL = "https://results.enr.clarityelections.com"
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:42.0) Gecko/20100101 Firefox/42.0",
    "Accept": "application/json, text/html, */*",
}
REQUEST_DELAY = 0.3  # seconds between requests

ALL_STATES = [
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC",
]

CC_INDEXES = [
    "CC-MAIN-2026-04",
    "CC-MAIN-2025-51",
    "CC-MAIN-2025-47",
    "CC-MAIN-2025-43",
    "CC-MAIN-2025-38",
    "CC-MAIN-2025-33",
    "CC-MAIN-2025-26",
    "CC-MAIN-2025-18",
    "CC-MAIN-2025-13",
]

OUTPUT_CSV = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "discovered_elections.csv"
)

# Regex: state/county/eid  OR  state/eid
_URL_RE = re.compile(
    r"results\.enr\.clarityelections\.com"
    r"/([A-Z]{2})"           # state
    r"(?:/([^/\d][^/]*))?"   # optional county (must not start with digit)
    r"/(\d+)/",              # eid
    re.IGNORECASE,
)

# ---------------------------------------------------------------------------
# HTTP helper
# ---------------------------------------------------------------------------

def safe_get(url, params=None, timeout=15):
    """GET with browser headers; returns Response or None on any error."""
    try:
        resp = requests.get(url, headers=HEADERS, params=params, timeout=timeout)
        time.sleep(REQUEST_DELAY)
        return resp
    except Exception as exc:
        print(f"  [warn] GET failed for {url}: {exc}")
        return None


# ---------------------------------------------------------------------------
# URL parsing
# ---------------------------------------------------------------------------

def parse_clarity_url(url):
    """
    Extract (state, county_or_None, eid) from a Clarity Elections URL.
    Returns None if the URL doesn't match.
    """
    m = _URL_RE.search(url)
    if not m:
        return None
    state = m.group(1).upper()
    county = m.group(2)  # None for statewide
    try:
        eid = int(m.group(3))
    except (TypeError, ValueError):
        return None
    return (state, county, eid)


def make_url(state, county, eid):
    if county:
        return f"{BASE_URL}/{state}/{county}/{eid}/"
    return f"{BASE_URL}/{state}/{eid}/"


# ---------------------------------------------------------------------------
# Layer 1: Common Crawl CDX scan
# ---------------------------------------------------------------------------

def layer1_cdx_scan():
    """
    Query Common Crawl CDX API for all indexed Clarity URLs.

    Returns:
        known_urls: set of canonical URL strings with EID > MIN_ID
        jurisdictions: dict  state -> set of county strings (all counties seen)
    """
    print("\n=== Layer 1: Common Crawl CDX scan ===")
    known_urls = set()
    jurisdictions = {}  # state -> set(county)

    cdx_base = "https://index.commoncrawl.org/{index}-index"
    params = {
        "url": "results.enr.clarityelections.com/*",
        "output": "json",
        "limit": 5000,
        "filter": "statuscode:200",
    }

    for index in CC_INDEXES:
        url = cdx_base.format(index=index)
        print(f"  Querying {index} …", end=" ", flush=True)
        resp = safe_get(url, params=params, timeout=30)
        if resp is None or resp.status_code != 200:
            print("no response / error")
            continue

        count = 0
        for line in resp.text.splitlines():
            line = line.strip()
            if not line:
                continue
            try:
                record = json.loads(line)
            except json.JSONDecodeError:
                continue
            raw_url = record.get("url", "")
            parsed = parse_clarity_url(raw_url)
            if parsed is None:
                continue
            state, county, eid = parsed

            # Track all (state, county) pairs regardless of EID
            if county:
                jurisdictions.setdefault(state, set()).add(county)

            if eid > MIN_ID:
                known_urls.add(make_url(state, county, eid))
                count += 1

        print(f"{count} new URLs with EID > {MIN_ID}")

    print(f"  Layer 1 total: {len(known_urls)} URLs, "
          f"{sum(len(v) for v in jurisdictions.values())} county/state pairs")
    return known_urls, jurisdictions


# ---------------------------------------------------------------------------
# Layer 2 helpers: elections.json + participatingcounties
# ---------------------------------------------------------------------------

def get_state_elections(state):
    """
    Fetch /{state}/elections.json.

    Returns list of dicts; each dict has at least 'eid' (int).
    Returns [] on failure or unexpected format.
    """
    url = f"{BASE_URL}/{state}/elections.json"
    resp = safe_get(url)
    if resp is None or resp.status_code != 200:
        return []

    try:
        data = resp.json()
    except Exception:
        return []

    elections = []

    # The API may return a list or a dict with a nested list
    if isinstance(data, list):
        items = data
    elif isinstance(data, dict):
        # Try common key names
        for key in ("elections", "Elections", "items"):
            if key in data and isinstance(data[key], list):
                items = data[key]
                break
        else:
            items = [data]
    else:
        return []

    for item in items:
        if not isinstance(item, dict):
            continue
        # EID may be stored under 'eid', 'id', 'ID', or 'eID'
        eid = None
        for k in ("eid", "id", "ID", "eID", "ElectionID"):
            if k in item:
                try:
                    eid = int(item[k])
                    break
                except (ValueError, TypeError):
                    pass
        if eid is not None:
            elections.append({"eid": eid, "raw": item})

    return elections


def get_election_version(state, eid, county=None):
    """
    GET current_ver.txt for a statewide or county election.
    Returns version string or None.
    """
    if county:
        url = f"{BASE_URL}/{state}/{county}/{eid}/current_ver.txt"
    else:
        url = f"{BASE_URL}/{state}/{eid}/current_ver.txt"

    resp = safe_get(url)
    if resp is None or resp.status_code != 200:
        return None
    version = resp.text.strip()
    return version if version else None


def get_participating_counties(state, eid, version):
    """
    Fetch electionsettings.json for a statewide election and extract
    the participatingcounties field.

    Returns list of (county_name, county_eid) tuples.
    """
    url = (f"{BASE_URL}/{state}/{eid}/{version}"
           f"/json/en/electionsettings.json")
    resp = safe_get(url)
    if resp is None or resp.status_code != 200:
        return []

    try:
        data = resp.json()
    except Exception:
        return []

    # Navigate settings.electiondetails.participatingcounties
    try:
        counties_raw = (
            data["settings"]["electiondetails"]["participatingcounties"]
        )
    except (KeyError, TypeError):
        return []

    if not isinstance(counties_raw, list):
        return []

    result = []
    for entry in counties_raw:
        if not isinstance(entry, str):
            continue
        parts = entry.split("|")
        # Format: county|sitenum|version|timestamp|unknown
        if len(parts) < 2:
            continue
        county_name = parts[0].strip()
        try:
            county_eid = int(parts[1].strip())
        except (ValueError, TypeError):
            continue
        if county_name and county_eid:
            result.append((county_name, county_eid))

    return result


# ---------------------------------------------------------------------------
# Layer 2: state elections.json scan
# ---------------------------------------------------------------------------

def layer2_state_elections():
    """
    For all 51 state codes, fetch elections.json.
    For each election with EID > MIN_ID, add statewide URL and discover
    participating counties.

    Returns:
        found_urls: set of URL strings
        found_jurisdictions: dict state -> set(county)  (augments Layer 1)
    """
    print("\n=== Layer 2: State elections.json + participatingcounties ===")
    found_urls = set()
    found_jurisdictions = {}

    for state in ALL_STATES:
        print(f"  {state} … ", end="", flush=True)
        elections = get_state_elections(state)

        recent = [e for e in elections if e["eid"] > MIN_ID]
        if not recent:
            print(f"0 recent elections (of {len(elections)} total)")
            continue

        print(f"{len(recent)} recent elections (of {len(elections)} total)")

        for elec in recent:
            eid = elec["eid"]
            # Statewide URL
            found_urls.add(make_url(state, None, eid))

            # Discover participating counties
            version = get_election_version(state, eid)
            if version is None:
                continue

            counties = get_participating_counties(state, eid, version)
            for county_name, county_eid in counties:
                # Track jurisdiction
                found_jurisdictions.setdefault(state, set()).add(county_name)
                if county_eid > MIN_ID:
                    found_urls.add(make_url(state, county_name, county_eid))

    print(f"  Layer 2 total: {len(found_urls)} URLs")
    return found_urls, found_jurisdictions


# ---------------------------------------------------------------------------
# Layer 3: county listing pages
# ---------------------------------------------------------------------------

def get_county_elections_from_listing(state, county):
    """
    Scrape /{state}/{county}/ for election links matching /{state}/{county}/{eid}/.

    Returns list of EIDs (ints).
    """
    url = f"{BASE_URL}/{state}/{county}/"
    resp = safe_get(url)
    if resp is None or resp.status_code not in (200, 301, 302):
        return []

    try:
        soup = BeautifulSoup(resp.text, "html.parser")
    except Exception:
        return []

    pattern = re.compile(
        rf"/{re.escape(state)}/{re.escape(county)}/(\d+)/",
        re.IGNORECASE,
    )
    eids = set()
    for tag in soup.find_all("a", href=True):
        m = pattern.search(tag["href"])
        if m:
            try:
                eids.add(int(m.group(1)))
            except ValueError:
                pass

    return list(eids)


def layer3_county_listings(jurisdictions):
    """
    For all (state, county) pairs collected in Layers 1+2, scrape the
    county listing page for election IDs > MIN_ID.

    jurisdictions: dict state -> set(county)
    Returns set of URL strings.
    """
    print("\n=== Layer 3: County listing pages ===")
    found_urls = set()

    pairs = [(s, c) for s, counties in jurisdictions.items() for c in counties]
    print(f"  Scanning {len(pairs)} state/county pairs …")

    for i, (state, county) in enumerate(pairs):
        if (i + 1) % 50 == 0:
            print(f"  … {i+1}/{len(pairs)}")
        eids = get_county_elections_from_listing(state, county)
        for eid in eids:
            if eid > MIN_ID:
                found_urls.add(make_url(state, county, eid))

    print(f"  Layer 3 total: {len(found_urls)} URLs")
    return found_urls


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

def validate_url(url):
    """HEAD request; returns True if status is 200, 301, or 302."""
    try:
        resp = requests.head(url, headers=HEADERS, timeout=10, allow_redirects=True)
        return resp.status_code in (200, 301, 302)
    except Exception:
        return False


def validate_all(urls, workers=10):
    """Parallel validation of all URLs. Returns dict url -> bool."""
    print(f"\n=== Validating {len(urls)} candidate URLs ({workers} workers) ===")
    results = {}
    with ThreadPoolExecutor(max_workers=workers) as executor:
        future_to_url = {executor.submit(validate_url, u): u for u in urls}
        done = 0
        for future in as_completed(future_to_url):
            url = future_to_url[future]
            try:
                results[url] = future.result()
            except Exception:
                results[url] = False
            done += 1
            if done % 25 == 0:
                print(f"  … validated {done}/{len(urls)}")
    valid_count = sum(1 for v in results.values() if v)
    print(f"  {valid_count} valid / {len(urls)} total")
    return results


# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

def write_csv(url_validity):
    """
    Write data/discovered_elections.csv with columns:
        url, state, county, eid, valid
    """
    os.makedirs(os.path.dirname(OUTPUT_CSV), exist_ok=True)
    rows = []
    for url, valid in sorted(url_validity.items()):
        parsed = parse_clarity_url(url)
        if parsed is None:
            continue
        state, county, eid = parsed
        rows.append({
            "url": url,
            "state": state,
            "county": county or "",
            "eid": eid,
            "valid": valid,
        })

    rows.sort(key=lambda r: (r["state"], r["county"], r["eid"]))

    with open(OUTPUT_CSV, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["url", "state", "county", "eid", "valid"])
        writer.writeheader()
        writer.writerows(rows)

    print(f"\nWrote {len(rows)} rows to {OUTPUT_CSV}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print(f"Discovering Clarity Elections sites with EID > {MIN_ID}")
    print(f"Base URL: {BASE_URL}")

    # Layer 1
    l1_urls, l1_jurisdictions = layer1_cdx_scan()

    # Layer 2
    l2_urls, l2_jurisdictions = layer2_state_elections()

    # Merge jurisdictions from layers 1 and 2 for layer 3
    all_jurisdictions = {}
    for src in (l1_jurisdictions, l2_jurisdictions):
        for state, counties in src.items():
            all_jurisdictions.setdefault(state, set()).update(counties)

    # Layer 3
    l3_urls = layer3_county_listings(all_jurisdictions)

    # Combine all candidates
    all_urls = l1_urls | l2_urls | l3_urls
    print(f"\nTotal candidate URLs (before validation): {len(all_urls)}")

    # Validate
    validity = validate_all(all_urls)

    # Write output
    write_csv(validity)

    # Summary
    valid_urls = [u for u, v in validity.items() if v]
    print("\n=== Summary ===")
    print(f"  Candidate URLs : {len(all_urls)}")
    print(f"  Valid URLs     : {len(valid_urls)}")
    print(f"  Output file    : {OUTPUT_CSV}")

    # Spot-check known examples
    known = [
        f"{BASE_URL}/TX/El_Paso/125659/",
        f"{BASE_URL}/NJ/Monmouth/125221/",
        f"{BASE_URL}/NJ/Ocean/125152/",
        f"{BASE_URL}/PA/Susquehanna/125147/",
    ]
    print("\n=== Spot-checks (known examples) ===")
    for k in known:
        status = validity.get(k, "not discovered")
        print(f"  {k} → {status}")


if __name__ == "__main__":
    main()
