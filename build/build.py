#!/usr/bin/env python3
"""Rebuild index.html from the NYC Daily Inmates in Custody file.

    python3 build/build.py              # fetch today's data, rebuild
    python3 build/build.py --offline    # rebuild from build/gamedata.json

Fetches every record, maps each top charge through charges.py to an offense
label and a category bucket, writes build/gamedata.json, and inlines it into
build/template.html at the __DATA__ placeholder to produce index.html.
"""
import collections
import datetime
import json
import os
import sys
import urllib.request

import charges

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
SRC = "https://data.cityofnewyork.us/resource/7479-ugqb.json?$limit=50000"
FELONY = ("A-I", "A-II", "B", "C", "D", "E")
PEOPLE_PER_SPRITE = 50  # must match PER in template.html

# inmate_status_code / custody_level -> the small ints the page expects
STATUS = {"DE": 0, "CS": 1, "DEP": 2, "SSR": 3, "DNS": 4, "CSP": 5, "DPV": 6}
CUSTODY = {"MIN": 0, "MED": 1, "MAX": 2}


def fetch():
    print(f"fetching {SRC}")
    with urllib.request.urlopen(SRC, timeout=120) as r:
        rows = json.load(r)
    print(f"  {len(rows)} records")
    return rows


def days_held(rec, today):
    try:
        adm = datetime.datetime.strptime(rec["admitted_dt"][:10], "%Y-%m-%d").date()
        return (today - adm).days
    except (KeyError, TypeError, ValueError):
        return -1


def bucket(label, cls, cat, violent, attempted, code):
    """Assign one mutually exclusive category. Every person lands in exactly one."""
    if cat == "NONE":
        return "nocharge"
    if cls in ("warrant", "VOP", "other"):
        return "admin"
    if cls not in FELONY:
        if cat in ("ASL", "DV", "KID") or label.startswith(
            ("Forcible", "Sexual abuse", "Public lewdness", "Reckless endangerment")
        ):
            return "misd_viol"
        return "misd"
    if cat == "HOM":
        if "Murder" in label:
            return "attmurder" if attempted else "murder"
        return "mansl"
    if cat == "ROB":
        return "rob1" if "1st" in label else "rob2" if "2nd" in label else "rob3"
    if cat == "ASL" and violent:
        hard = ("1st", "Gang", "police", "peace officer", "judge")
        return "asl1" if any(h in label for h in hard) else "asl2"
    if cat == "WEA" and violent:
        return "gun"
    if cat == "BUR":
        return "burg_res" if violent else "burg3"
    if cat == "SEX":
        if not violent:
            return "nvfel_other"
        if code.startswith("263") or label.startswith("Predatory") or "child" in label.lower():
            return "child"
        return "sex"
    if cat == "DV":
        return "dv_viol" if violent else "contempt"
    if cat == "ARS":
        return "arson" if violent else "nvfel_other"
    if cat == "KID":
        return "kidnap" if violent else "nvfel_other"
    if violent:
        return "viol_other"
    if cat == "DRG":
        return "drug"
    if cat in ("PRP", "FRD"):
        return "theft"
    if cat == "CON":
        return "consp"
    if cat == "ESC":
        return "esc"
    return "nvfel_other"


def build(rows, today):
    labels, people = {}, []
    for rec in rows:
        raw = (rec.get("top_charge") or "").strip().upper()
        label, cls, cat, violent, attempted = charges.lookup(rec.get("top_charge"))
        shown = ("Attempted " + label[0].lower() + label[1:]) if attempted else label
        code = raw[4:] if raw.startswith("110-") else raw
        key = labels.setdefault((shown, cls), len(labels))
        age = rec.get("age")
        people.append([
            key,
            bucket(label, cls, cat, violent, attempted, code),
            int(age) if age and str(age).isdigit() else -1,
            1 if rec.get("gender") == "F" else 0,
            CUSTODY.get(rec.get("custody_level"), 1),
            STATUS.get(rec.get("inmate_status_code"), 0),
            1 if rec.get("srg_flg") == "Y" else 0,
            1 if rec.get("bradh") == "Y" else 0,
            1 if rec.get("infraction") == "Y" else 0,
            days_held(rec, today),
        ])

    counts = collections.Counter(p[1] for p in people)
    names = sorted(counts)
    index = {name: i for i, name in enumerate(names)}
    for p in people:
        p[1] = index[p[1]]

    flat = [None] * len(labels)
    for (text, cls), i in labels.items():
        flat[i] = [text, cls]

    return {
        "people": people,
        "labels": flat,
        "buckets": names,
        "asof": today.isoformat(),
    }, counts


VIOLENT_BUCKETS = {
    "murder", "attmurder", "mansl", "rob1", "rob2", "rob3", "asl1", "asl2", "gun",
    "burg_res", "sex", "child", "dv_viol", "arson", "kidnap", "viol_other",
}


def report(counts):
    total = sum(counts.values())
    violent = sum(n for b, n in counts.items() if b in VIOLENT_BUCKETS)
    homicide = sum(counts[b] for b in ("murder", "attmurder", "mansl") if b in counts)
    print(f"\n  in custody          {total:>6,}")
    print(f"  violent felony      {violent:>6,}  ({violent / total:.1%})")
    print(f"  homicide charge     {homicide:>6,}  ({homicide / total:.1%})")
    print(f"  nonviolent          {total - violent:>6,}")
    print(f"  floor if all the nonviolent go free: {violent:,} "
          f"({violent - 3300:+,} against 3,300 beds)")
    print(f"  chibi figures       {sum(max(1, round(n / PEOPLE_PER_SPRITE)) for n in counts.values()):>6,}")
    missing = set(counts) - set(VIOLENT_BUCKETS) - {
        "misd", "misd_viol", "admin", "nocharge", "esc", "nvfel_other",
        "drug", "theft", "burg3", "consp", "contempt",
    }
    if missing:
        print(f"  WARNING: buckets the page has no lever for: {sorted(missing)}")


def main():
    offline = "--offline" in sys.argv
    data_path = os.path.join(HERE, "gamedata.json")

    if offline:
        print(f"reading {data_path}")
        data = json.load(open(data_path))
        counts = collections.Counter(data["buckets"][p[1]] for p in data["people"])
    else:
        today = datetime.date.today()
        data, counts = build(fetch(), today)
        with open(data_path, "w") as fh:
            json.dump(data, fh, separators=(",", ":"))
        print(f"wrote {data_path}")

    report(counts)

    template = open(os.path.join(HERE, "template.html"), encoding="utf-8").read()
    if "__DATA__" not in template:
        sys.exit("template.html has no __DATA__ placeholder")
    page = template.replace("__DATA__", json.dumps(data, separators=(",", ":")))
    out = os.path.join(ROOT, "index.html")
    with open(out, "w", encoding="utf-8") as fh:
        fh.write(page)
    print(f"\nwrote {out}  ({len(page) / 1024:.0f} KB, data as of {data['asof']})")


if __name__ == "__main__":
    main()
