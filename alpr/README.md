# ALPRs per US police agency (OSM / DeFlock)

Estimates automated license plate readers (ALPRs) per local police department and sheriff's office,
from OpenStreetMap nodes tagged `man_made=surveillance` + `surveillance:type=ALPR` (the layer DeFlock
maps), crosswalked to FBI/BJS agency identifiers.

## Run order (from this folder)

```
Rscript R/01_download_census.R   # TIGER/Line 2025 states, counties, places, county subdivisions; ACS 2020-24 B01003
Rscript R/02_pull_osm.R          # Overpass, state by state -> data/raw/osm/<date>/ + osm_alpr_raw_<date>.csv
Rscript R/03_agencies.R          # FBI CDE agency directory + FBI employee file (population) + LEAIC FIPS
Rscript R/04_method_a.R          # operator-tag name match
Rscript R/05_method_b.R          # spatial join (needs LEAIC)
Rscript R/06_combine_outputs.R   # cameras.csv, agencies.csv, summary_stats.csv, density plot
Rscript R/07_validate.R          # comparison with N&O Flock transparency-portal counts
```

R packages: sf, dplyr, readr, tidyr, stringr, purrr, jsonlite, httr2, stringdist, ggplot2.

## Manual download: LEAIC (required for method B)

ICPSR needs a login (and blocks scripted access), so download by hand:

- Study: **Law Enforcement Agency Identifiers Crosswalk, United States, 2012 (ICPSR 35158)**,
  https://www.icpsr.umich.edu/web/NACJD/studies/35158
- Dataset: **DS1** (the only dataset). Format: **Delimited** (tab) or **R**.
- Put `35158-0001-Data.tsv` (or `35158-0001-Data.rda`) anywhere under `data/raw/leaic/`.

Then re-run from `03_agencies.R`.

## Sources and access notes

| Data | Source | Note |
|---|---|---|
| OSM ALPR nodes | Overpass API, `maps.mail.ru` mirror | `overpass-api.de` and Geofabrik were unreachable from the build machine (connection reset by network proxy). The mirror runs the same Overpass software on live OSM data; the `osm_base` timestamp is in `query_log.csv`. |
| Agency directory | FBI Crime Data Explorer, `/LATEST/agency/byStateAbbr/<ST>` | ORI, name, type, county |
| Population served | FBI CDE "Law Enforcement Employees" file `lee_1960_2025.csv` | latest year per ORI (2025 for most) |
| FIPS crosswalk | BJS LEAIC 2012 (ICPSR 35158) | manual download, see above |
| Boundaries | Census TIGER/Line 2025 | |
| Population (places/counties) | ACS 2020-2024 5-year, table B01003, summary-file download | Census API now requires a key |
| Validation | github.com/mcclatchy-southeast/private_eyes `data/latest_usage04262023.csv` | public (MIT); scraped from Flock transparency portals 2024-04-26 |
