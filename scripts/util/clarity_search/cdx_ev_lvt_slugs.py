"""
cdx_ev_lvt_slugs.py

Collect Enhanced Voting and LiveVoterTurnout jurisdiction slugs from the
Common Crawl CDX API (indexes 2022+).

  EV:  app.enhancedvoting.com/{cdn/results|results/public}/{slug}/...
  LVT: livevoterturnout.com/ENR/{slug}/{id}/...  (also liveresults.boe.ohio.gov)

Output: cdx_ev_lvt_slugs.csv  (platform, host, slug, max_id, n_urls)

Usage
-----
python cdx_ev_lvt_slugs.py
"""

import csv
import json
import os
import re
import time
from collections import defaultdict

import requests

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "cdx_ev_lvt_slugs.csv")
QUERIES = [
    ("ev", "app.enhancedvoting.com/*"),
    ("lvt", "livevoterturnout.com/*"),
    ("lvt", "www.livevoterturnout.com/*"),
    ("lvt", "liveresults.boe.ohio.gov/*"),
]
EV_RE = re.compile(r"enhancedvoting\.com/(?:cdn/results|results/public(?:/api/jurisdictions)?)/([^/?#]+)", re.I)
LVT_RE = re.compile(r"^(?:https?://)?([^/]+)/ENR/([^/?#]+)/(\d+)/", re.I)


def indexes():
    cols = requests.get("https://index.commoncrawl.org/collinfo.json", timeout=60).json()
    return [c["id"] for c in cols if int(c["id"].split("-")[2]) >= 2022]


def fetch(url, params, tries=5):
    for attempt in range(tries):
        try:
            r = requests.get(url, params=params, timeout=120)
            if r.status_code in (200, 404):
                return r
        except requests.RequestException:
            pass
        time.sleep(10 * (attempt + 1))
    return None


def cdx(index, pattern):
    url = f"https://index.commoncrawl.org/{index}-index"
    r = fetch(url, {"url": pattern, "showNumPages": "true", "output": "json"})
    try:
        pages = r.json().get("pages", 1)
    except (AttributeError, ValueError):
        print(f"  {index} {pattern}: page count failed")
        return []
    out = []
    for page in range(pages):
        r = fetch(url, {"url": pattern, "output": "json", "fl": "url", "page": page})
        if r is None:
            print(f"  {index} {pattern}: page {page} failed")
            continue
        if r.status_code == 404:  # no captures
            break
        out += [json.loads(l)["url"] for l in r.text.splitlines() if l.strip()]
        time.sleep(1)
    return out


def main():
    seen = defaultdict(lambda: {"max_id": 0, "n": 0})
    for ix in indexes():
        for plat, pat in QUERIES:
            urls = cdx(ix, pat)
            print(f"{ix} {pat}: {len(urls)} urls")
            for u in urls:
                if plat == "ev":
                    m = EV_RE.search(u)
                    if m:
                        k = ("ev", "app.enhancedvoting.com", m.group(1).lower())
                        seen[k]["n"] += 1
                else:
                    m = LVT_RE.search(u)
                    if m:
                        k = ("lvt", m.group(1).lower().removeprefix("www."), m.group(2).lower())
                        seen[k]["n"] += 1
                        seen[k]["max_id"] = max(seen[k]["max_id"], int(m.group(3)))
            time.sleep(1)
    with open(OUT, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["platform", "host", "slug", "max_id", "n_urls"])
        for (p, h, s), v in sorted(seen.items()):
            w.writerow([p, h, s, v["max_id"] or "", v["n"]])
    print(f"wrote {len(seen)} slugs to {OUT}")


if __name__ == "__main__":
    main()
