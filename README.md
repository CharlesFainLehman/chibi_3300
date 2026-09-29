# Only 3,300 Beds?!

A one-screen browser game about New York City's jail capacity. You get the 3,300 beds the
four borough-based jails will hold, the roughly 6,600 people actually in custody today, and a
yard full of chibi pixel-art detainees. Check a category to release everyone in it and watch
them walk out the gate. The point is that you cannot get under 3,300 without releasing people
charged with violent felonies — release every single nonviolent case and you are still about
1,100 beds short on opening day.

Built on the argument in Charles Fain Lehman, [*Is 3,300 Enough? Why the Borough-Based Jails
Are Too Small to Keep NYC Safe*](https://manhattan.institute/article/is-3300-enough-why-the-borough-based-jails-are-too-small-to-keep-nyc-safe),
Manhattan Institute.

## Running it

`index.html` is entirely self-contained — no build step, no network, no dependencies. Open it
in a browser, or publish it anywhere that serves a static file.

To preview locally:

```bash
python3 -m http.server 8760
```

## Refreshing the data

The page bakes in a snapshot of the city's
[Daily Inmates in Custody](https://data.cityofnewyork.us/Public-Safety/Daily-Inmates-In-Custody/7479-ugqb)
file, which the city updates daily. To pull the current file and rebuild:

```bash
python3 build/build.py
```

That fetches every record, re-maps the charges, rewrites `build/gamedata.json`, and regenerates
`index.html`. It prints a summary so you can see what moved:

```
  in custody           6,574
  violent felony       4,439  (67.5%)
  homicide charge      1,486  (22.6%)
  nonviolent           2,135
  floor if all the nonviolent go free: 4,439 (+1,139 against 3,300 beds)
  chibi figures          133
```

To rebuild from the saved snapshot without hitting the network:

```bash
python3 build/build.py --offline
```

Every figure the copy quotes is derived from the loaded data at runtime — the population, the
capacity marker's position, the category counts, the share of the jail on the mental-health
caseload, the number of people with no charge on file. Nothing needs editing by hand after a
refresh. The only hardcoded number is 3,300, which is the statutory plan, not data.

## Files

| Path | What it is |
|---|---|
| `index.html` | The finished game. Self-contained, ~210 KB, data inlined. |
| `build/build.py` | Fetch, classify, and render. The only command you need. |
| `build/charges.py` | New York Penal Law code table: ~250 codes mapped to an offense name, felony class, category, and a violent/nonviolent flag, with article-level fallbacks for anything unmapped. |
| `build/template.html` | The page with a `__DATA__` placeholder where the records get inlined. Edit this, never `index.html`. |
| `build/gamedata.json` | The current snapshot: one compact array per person. |

## How the categories work

Each person is placed in exactly one of 27 mutually exclusive buckets, assigned from their
**top charge** only. The buckets are grouped into the four columns you see on screen. Releases
are computed as a set union over individual records, not by adding up bucket sizes, so
overlapping levers (a murder defendant who is also on the mental-health caseload) never
double-count. Each unchecked lever shows how many *additional* people it would free given what
you have already released.

`build.py` warns if a refresh ever produces a bucket the page has no lever for.

### What counts as violent

Felonies listed in New York Penal Law §70.02, plus all homicide, robbery, felony sex offenses,
arson, kidnapping, and felony domestic-violence assault. Robbery in the third degree and
manslaughter in the second degree are counted as violent although §70.02 does not list them.

This runs a few points below the 79% in the 2022 report — different source, different year, and
a stricter classification here. The page states its own definition in the Lore panel.

### Caveats the page discloses

- Categories come from the top charge only, so someone charged with six crimes appears under
  the most serious one. Every category is therefore *more* sympathetic than the real case file.
- A few hundred people have no top charge in the published file. They are counted as nonviolent
  throughout, which is a generous assumption.
- The BRADH mental-health flag covers about 60% of the jail and says nothing about severity.
- The court-appearance and reoffending projections (15.6% and 12.2% of everyone released) are
  averages for defendants at the *margin* of a release decision, from the quasi-experimental
  literature cited in the report, applied here to everyone you let out. They are rough.
- The D→SSS letter ranks are this page's own invention, assigned by offense severity. New York
  grades felonies A-I through E.

## Published artifact

<https://claude.ai/artifact/64yxiz19VpZH6JtaEqzt2y> — private until shared from the page's
Share menu.
