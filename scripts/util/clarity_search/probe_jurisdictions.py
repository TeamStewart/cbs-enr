"""
probe_jurisdictions.py

For every row in data/jurisdiction_list.csv with search=1, fetch the county's
elections.json and look for elections matching a target date (default: 3/3/2026).

Works by calling:
  GET https://results.enr.clarityelections.com/{STATE}/{COUNTY}/elections.json

Each response is a list of elections with EID, ElectionName, Date.
Statewide rows (county="") use the state-level elections.json instead.

Output: data/probe_results.csv
  state, county, eid, election_name, date, url, valid

Usage
-----
python scripts/probe_jurisdictions.py [--date 3/3/2026]
"""

import argparse
import csv
import os
import re
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import requests

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

BASE_URL = "https://results.enr.clarityelections.com"
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:42.0) Gecko/20100101 Firefox/42.0",
    "Accept": "application/json, text/html, */*",
}
REQUEST_DELAY = 0.25

JURISDICTION_CSV = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "jurisdiction_list.csv"
)
OUTPUT_CSV = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "probe_results.csv"
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def safe_get(url, timeout=15):
    try:
        resp = requests.get(url, headers=HEADERS, timeout=timeout)
        time.sleep(REQUEST_DELAY)
        return resp
    except Exception as exc:
        return None


def county_to_url_slug(county_plain):
    """'El Paso' → 'El_Paso'"""
    return county_plain.replace(" ", "_")


def validate_url(url):
    try:
        resp = requests.head(url, headers=HEADERS, timeout=10, allow_redirects=True)
        return resp.status_code in (200, 301, 302)
    except Exception:
        return False


# ---------------------------------------------------------------------------
# Core fetch
# ---------------------------------------------------------------------------

def get_elections_for_county(state, county_plain):
    """
    Fetch elections.json for a county (or state if county_plain is "").
    Returns (slug_used, list_of_election_dicts).
    """
    if county_plain:
        slug = county_to_url_slug(county_plain)
        url = f"{BASE_URL}/{state}/{slug}/elections.json"
    else:
        slug = None
        url = f"{BASE_URL}/{state}/elections.json"

    resp = safe_get(url)
    if resp is None or resp.status_code != 200:
        return slug, []

    try:
        data = resp.json()
        if isinstance(data, list):
            return slug, data
        # Some states wrap in a dict
        for key in ("elections", "Elections"):
            if key in data and isinstance(data[key], list):
                return slug, data[key]
        return slug, []
    except Exception:
        return slug, []


def filter_elections(elections, target_date):
    """
    Keep elections whose Date matches target_date (e.g. '3/3/2026').
    Date field format is '3/3/2026 12:00:00 AM' or similar.
    """
    matches = []
    for e in elections:
        date_str = e.get("Date", "") or e.get("date", "") or e.get("ElectionDate", "")
        eid_raw = e.get("EID") or e.get("eid") or e.get("id") or e.get("ID")
        name = e.get("ElectionName") or e.get("electionName") or e.get("name") or ""
        if not eid_raw:
            continue
        try:
            eid = int(eid_raw)
        except (ValueError, TypeError):
            continue
        if target_date and target_date in date_str:
            matches.append({"eid": eid, "election_name": name, "date": date_str})
    return matches


# ---------------------------------------------------------------------------
# Main probe loop
# ---------------------------------------------------------------------------

def load_search_jurisdictions():
    rows = []
    with open(JURISDICTION_CSV) as f:
        for r in csv.DictReader(f):
            if r.get("search", "").strip():
                rows.append((r["state"], r["county"]))
    return rows


def probe_all(jurisdictions, target_date):
    """
    For each (state, county) pair, fetch elections.json and collect matches.
    Returns list of result dicts.
    """
    results = []
    total = len(jurisdictions)

    for i, (state, county) in enumerate(jurisdictions, 1):
        label = f"{state}/{county}" if county else f"{state} (statewide)"
        slug, elections = get_elections_for_county(state, county)
        matches = filter_elections(elections, target_date)
        
        if matches:
            for m in matches:
                url = (
                    f"{BASE_URL}/{state}/{slug}/{m['eid']}/"
                    if slug else
                    f"{BASE_URL}/{state}/{m['eid']}/"
                )
                results.append({
                    "state": state,
                    "county": county,
                    "eid": m["eid"],
                    "election_name": m["election_name"],
                    "date": m["date"],
                    "url": url,
                    "valid": None,  # filled in later
                })
                print(f"  [{i:4d}/{total}] {label}: EID {m['eid']}  \"{m['election_name']}\"")

    return results


def validate_all(results, workers=10):
    print(f"\nValidating {len(results)} URLs …")
    urls = [r["url"] for r in results]
    validity = {}
    with ThreadPoolExecutor(max_workers=workers) as ex:
        future_map = {ex.submit(validate_url, u): u for u in urls}
        for fut in as_completed(future_map):
            validity[future_map[fut]] = fut.result()
    for r in results:
        r["valid"] = validity.get(r["url"], False)
    valid_n = sum(1 for r in results if r["valid"])
    print(f"  {valid_n} / {len(results)} valid")
    return results


def write_csv(results):
    os.makedirs(os.path.dirname(OUTPUT_CSV), exist_ok=True)
    fields = ["state", "county", "eid", "election_name", "date", "url", "valid"]
    with open(OUTPUT_CSV, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(results)
    print(f"Wrote {len(results)} rows to {OUTPUT_CSV}")


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--date", default="3/3/2026",
                        help="Target election date (default: 3/3/2026)")
    args = parser.parse_args()

    print(f"Target date : {args.date}")
    print(f"Reading     : {JURISDICTION_CSV}")

    jurisdictions = load_search_jurisdictions()
    print(f"search=1 rows: {len(jurisdictions)}  "
          f"({sum(1 for s,c in jurisdictions if c)} county, "
          f"{sum(1 for s,c in jurisdictions if not c)} statewide)\n")

    print("=== Probing elections.json for each jurisdiction ===")
    results = probe_all(jurisdictions, args.date)

    if not results:
        print("\nNo elections found for that date.")
        return

    results = validate_all(results)
    write_csv(results)

    print(f"\n=== Summary ===")
    print(f"  Jurisdictions probed : {len(jurisdictions)}")
    print(f"  Elections found      : {len(results)}")
    print(f"  Valid URLs           : {sum(1 for r in results if r['valid'])}")
    print(f"  Output               : {OUTPUT_CSV}")


if __name__ == "__main__":
    main()
