"""
discover_new_jurisdictions.py

Look for Clarity, Enhanced Voting (EV) and LiveVoterTurnout (LVT) results sites
for jurisdictions not yet in metadata.csv, and check whether each has posted the
target election (default 2026-11-03).

Candidates
  clarity: jurisdiction_list.csv rows with seen_on_clarity=1
  ev:      {county}-county-{st} (or -parish-la) for every county in
           jurisdiction_list.csv, plus slugs in cdx_ev_lvt_slugs.csv
  lvt:     {county}{st}enr for every county, plus slugs in cdx_ev_lvt_slugs.csv
Rows already in metadata.csv, and every county in a state with a statewide
metadata row (county blank), are skipped.

Probes (same logic as scripts/util/enr_cycle/cycle.R)
  clarity: /{ST}/{County}/elections.json, one request at a time at 2/s; stops
           all Clarity requests at the first HTTP 405 (WAF block)
  ev:      /results/public/api/jurisdictions/{slug}, election with electionDate
  lvt:     /ENR/{slug}/ is 403 if the site exists and 404 if not; for existing
           sites, scan Index_{n}.html for the target date

Output: discovery_results.csv
  platform, state, county, slug, exists, status, path, note
  status: found (target election posted), not_posted (site exists, no target
  election yet), missing (no site), error

Usage
-----
python discover_new_jurisdictions.py --metadata PATH [--platforms clarity ev lvt]
"""

import argparse
import csv
import os
import re
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from datetime import date, datetime

import requests

HERE = os.path.dirname(os.path.abspath(__file__))
JURIS = os.path.join(HERE, "jurisdiction_list.csv")
CDX = os.path.join(HERE, "cdx_ev_lvt_slugs.csv")
OUT = os.path.join(HERE, "discovery_results.csv")

UA = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:42.0) Gecko/20100101 Firefox/42.0"
CLARITY = "https://results.enr.clarityelections.com"
EV = "https://app.enhancedvoting.com"
LVT_HOSTS = {"OH": "https://liveresults.boe.ohio.gov"}
LVT_DEFAULT = "https://www.livevoterturnout.com"
CLARITY_RATE = 2  # requests/s, see cycle.R
OTHER_RATE = 4
LVT_SCAN = 40  # ids to scan when the site's current id is unknown

session = requests.Session()
session.headers["User-Agent"] = UA


class Throttle:
    def __init__(self, rate):
        self.gap, self.next, self.lock = 1 / rate, 0.0, threading.Lock()

    def wait(self):
        with self.lock:
            now = time.monotonic()
            t = max(now, self.next)
            self.next = t + self.gap
        time.sleep(max(0, t - now))


def get(url, throttle):
    throttle.wait()
    try:
        r = session.get(url, timeout=20)
        return r.status_code, r.text
    except requests.RequestException as e:
        return None, str(e)


def norm(x):
    return re.sub(r"[^a-z0-9]", "", x.lower().replace("saint ", "st "))


def read_meta(path):
    rows = list(csv.DictReader(open(path)))
    statewide = {r["state"] for r in rows if not r["county"].strip()}
    have = {(r["state"], norm(r["county"])) for r in rows if r["county"].strip()}
    return statewide, have


def counties():
    return [(r["state"], r["county"], r["seen_on_clarity"] == "1") for r in csv.DictReader(open(JURIS)) if r["county"]]


# ---------------------------------------------------------------------------
# Clarity
# ---------------------------------------------------------------------------

def probe_clarity(st, cty, target, throttle, state):
    t = cty.title() if cty.islower() else cty
    code = None
    for slug in dict.fromkeys([t.replace(" ", "_"), t.replace(".", "").replace(" ", "_")]):
        if state["blocked"]:
            return None
        code, body = get(f"{CLARITY}/{st}/{slug}/elections.json", throttle)
        if code == 405:
            state["blocked"] = True
            print("Clarity HTTP 405 (WAF block); no more Clarity requests this run")
            return None
        if code != 200:
            continue
        try:
            el = __import__("json").loads(body)
        except ValueError:
            continue
        if el == []:
            return dict(slug=slug, exists=1, status="not_posted", path="", note="elections.json empty; web search by hand")
        hits = [e for e in el if parse_mdy(e.get("Date", "")) == target]
        if hits:
            return dict(slug=slug, exists=1, status="found", path=f"{CLARITY}/{st}/{slug}/{hits[0]['EID']}/", note=hits[0]["ElectionName"])
        newest = el[0] if el else {}
        return dict(slug=slug, exists=1, status="not_posted", path="", note=f"newest {newest.get('ElectionName')} ({newest.get('Date')})")
    return dict(slug=t.replace(" ", "_"), exists=0, status="missing", path="", note=f"elections.json HTTP {code}")


def parse_mdy(s):
    try:
        return datetime.strptime(s.split(" ")[0], "%m/%d/%Y").date()
    except ValueError:
        return None


# ---------------------------------------------------------------------------
# Enhanced Voting
# ---------------------------------------------------------------------------

def ev_slug(st, cty):
    kind = "parish" if st == "LA" else "county"
    return f"{cty.lower().replace(' ', '-')}-{kind}-{st.lower()}"


def probe_ev(slug, target, throttle):
    code, body = get(f"{EV}/results/public/api/jurisdictions/{slug}", throttle)
    if code != 200:
        return dict(exists=0, status="missing" if code == 404 else "error", path="", note=f"API HTTP {code}")
    try:
        d = __import__("json").loads(body)
    except ValueError:
        return dict(exists=0, status="missing", path="", note="API not JSON")
    if not isinstance(d, dict) or not d.get("id"):
        return dict(exists=0, status="missing", path="", note="no jurisdiction")
    el = d.get("elections") or []
    name = d.get("name") or ""
    hits = [e for e in el if e.get("electionDate", "")[:10] == target.isoformat()]
    if hits:
        p = f"{EV}/cdn/results/{d.get('shortName') or slug}/export-{hits[0]['publicElectionId']}.json"
        code, _ = get(p, throttle)
        return dict(exists=1, status="found", path=p, note=f"{name}; export HTTP {code}")
    if not el:
        return dict(exists=1, status="not_posted", path="", note=f"{name}; no elections listed")
    last = max(el, key=lambda e: e.get("electionDate", ""))
    return dict(exists=1, status="not_posted", path="", note=f"{name}; newest {last.get('publicElectionId')} ({last.get('electionDate', '')[:10]})")


# ---------------------------------------------------------------------------
# LiveVoterTurnout
# ---------------------------------------------------------------------------

def lvt_slug(st, cty):
    return f"{norm(cty)}{st.lower()}enr"


def lvt_pattern(target):
    # pages name the election as "November 3, 2026", "11/3/2026", or only
    # "November 2026 General Election" / "2026 General Election" / "November General 2026"
    y, mon = target.year, target.strftime("%B")
    return re.compile(
        rf"{mon}\s+{target.day},?\s+{y}|{target.month}/0?{target.day}/{y}"
        rf"|{mon}\s+(?:General\s+)?{y}|{y}\s+General(?!\s+Primary)|{mon}\s+General\s+{y}",
        re.I,
    )


def probe_lvt(host, slug, max_id, target, throttle):
    code, _ = get(f"{host}/ENR/{slug}/", throttle)
    if code == 404:
        return dict(exists=0, status="missing", path="", note="site 404")
    if code != 403:
        return dict(exists=0, status="error", path="", note=f"site HTTP {code}")
    pat = lvt_pattern(target)
    ids = range(max(1, max_id - 2), max_id + 10) if max_id else range(1, LVT_SCAN + 1)
    last = None
    for n in ids:
        u = f"{host}/ENR/{slug}/{n}/en/Index_{n}.html"
        code, body = get(u, throttle)
        if code != 200:
            continue
        last = n
        if pat.search(body.replace("&nbsp;", " ")):
            return dict(exists=1, status="found", path=u, note=f"id {n}")
    return dict(exists=1, status="not_posted", path="", note=f"ids {ids.start}-{ids.stop - 1}: newest id {last}, no target date")


# ---------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metadata", required=True)
    ap.add_argument("--date", default="2026-11-03")
    ap.add_argument("--platforms", nargs="+", default=["clarity", "ev", "lvt"])
    ap.add_argument("--workers", type=int, default=4)
    ap.add_argument("--out", default=OUT)
    ap.add_argument("--states", nargs="+", help="only these states")
    ap.add_argument("--rate", type=float, default=OTHER_RATE, help="EV/LVT requests per second")
    args = ap.parse_args()
    target = date.fromisoformat(args.date)
    statewide, have = read_meta(args.metadata)
    keep = lambda st, cty: st not in statewide and (st, norm(cty)) not in have
    cs = [c for c in counties() if keep(c[0], c[1]) and (not args.states or c[0] in args.states)]
    cdx = list(csv.DictReader(open(CDX))) if os.path.exists(CDX) and not args.states else []
    results = []

    def add(plat, st, cty, r, slug=None):
        if r is None:
            return
        results.append(dict(platform=plat, state=st, county=cty, slug=r.get("slug", slug), **{k: r[k] for k in ("exists", "status", "path", "note")}))

    if "clarity" in args.platforms:
        thr, state = Throttle(CLARITY_RATE), {"blocked": False}
        todo = [c for c in cs if c[2]]
        print(f"clarity: {len(todo)} candidates")
        for st, cty, _ in todo:
            add("clarity", st, cty, probe_clarity(st, cty, target, thr, state))

    thr = Throttle(args.rate)
    if "ev" in args.platforms:
        cands = {ev_slug(st, cty): (st, cty) for st, cty, _ in cs}
        for r in cdx:
            if r["platform"] == "ev" and r["slug"] not in cands:
                m = re.match(r"(.+)-(?:county|parish)-([a-z]{2})$", r["slug"])
                st, cty = (m.group(2).upper(), m.group(1).replace("-", " ").title()) if m else ("", r["slug"])
                if st and not keep(st, cty):
                    continue
                cands[r["slug"]] = (st, cty)
        print(f"ev: {len(cands)} candidates")
        with ThreadPoolExecutor(args.workers) as ex:
            for slug, r in zip(cands, ex.map(lambda s: probe_ev(s, target, thr), cands)):
                add("ev", *cands[slug], r, slug)

    if "lvt" in args.platforms:
        cands = {}
        for st, cty, _ in cs:
            cands[(LVT_HOSTS.get(st, LVT_DEFAULT), lvt_slug(st, cty))] = (st, cty, 0)
        for r in cdx:
            if r["platform"] != "lvt":
                continue
            host = "https://" + ("www." + r["host"] if r["host"] == "livevoterturnout.com" else r["host"])
            m = re.match(r"(.+?)([a-z]{2})enr$", r["slug"])
            st = m.group(2).upper() if m else ""
            k = (host, r["slug"])
            prev = cands.get(k)
            if prev is None and st and st in statewide:
                continue
            if prev is None and m and (st, norm(m.group(1))) in have:
                continue
            mx = int(r["max_id"] or 0)
            cands[k] = (prev[0], prev[1], mx) if prev else (st, m.group(1) if m else r["slug"], mx)
        print(f"lvt: {len(cands)} candidates")
        with ThreadPoolExecutor(args.workers) as ex:
            out = ex.map(lambda k: probe_lvt(k[0], k[1], cands[k][2], target, thr), cands)
            for k, r in zip(cands, out):
                add("lvt", cands[k][0], cands[k][1], r, k[1])

    fields = ["platform", "state", "county", "slug", "exists", "status", "path", "note"]
    with open(args.out, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        w.writerows(r for r in results if r["exists"] or r["status"] == "error")
    n = lambda s: sum(r["status"] == s for r in results)
    print(f"wrote {args.out}: found {n('found')}, not_posted {n('not_posted')}, error {n('error')} (missing {n('missing')} dropped)")


if __name__ == "__main__":
    main()
