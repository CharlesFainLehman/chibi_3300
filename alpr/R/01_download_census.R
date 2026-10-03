# Download Census TIGER/Line boundaries (states, counties, places) and ACS 5-year population.
source("R/00_setup.R")

tdir <- file.path(RAW, "census", paste0("tiger", TIGER_YEAR))
base <- sprintf("https://www2.census.gov/geo/tiger/TIGER%d", TIGER_YEAR)

invisible(fetch(sprintf("%s/STATE/tl_%d_us_state.zip", base, TIGER_YEAR),  file.path(tdir, sprintf("tl_%d_us_state.zip", TIGER_YEAR))))
invisible(fetch(sprintf("%s/COUNTY/tl_%d_us_county.zip", base, TIGER_YEAR), file.path(tdir, sprintf("tl_%d_us_county.zip", TIGER_YEAR))))
for (f in STATES$fips) {
  fn <- sprintf("tl_%d_%s_place.zip", TIGER_YEAR, f)
  fetch(sprintf("%s/PLACE/%s", base, fn), file.path(tdir, "place", fn))
  fn <- sprintf("tl_%d_%s_cousub.zip", TIGER_YEAR, f)     # county subdivisions (towns/townships)
  fetch(sprintf("%s/COUSUB/%s", base, fn), file.path(tdir, "cousub", fn))
}

# ACS 5-year table B01003 (total population), all summary levels, from the table-based summary file.
# (The Census API now requires a key; this file is the same estimates without one.)
acs_url <- sprintf("https://www2.census.gov/programs-surveys/acs/summary_file/%d/table-based-SF/data/5YRData/acsdt5y%d-b01003.dat", ACS_YEAR, ACS_YEAR)
invisible(fetch(acs_url, file.path(RAW, "census", sprintf("acsdt5y%d-b01003.dat", ACS_YEAR))))

# Read, combine, and save processed layers ------------------------------------------------------
rd <- function(zip) {
  ex <- file.path(tempdir(), tools::file_path_sans_ext(basename(zip)))
  unzip(zip, exdir = ex)
  st_read(list.files(ex, pattern = "\\.shp$", full.names = TRUE), quiet = TRUE)
}

states <- rd(file.path(tdir, sprintf("tl_%d_us_state.zip", TIGER_YEAR))) |>
  filter(STATEFP %in% STATES$fips) |> select(STATEFP, STUSPS, NAME)
n_log(states, "TIGER states (50 + DC)")

counties <- rd(file.path(tdir, sprintf("tl_%d_us_county.zip", TIGER_YEAR))) |>
  filter(STATEFP %in% STATES$fips) |> select(STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD, CLASSFP)
n_log(counties, "TIGER counties (50 + DC)")

places <- map(list.files(file.path(tdir, "place"), full.names = TRUE), rd) |>
  bind_rows() |> select(STATEFP, PLACEFP, GEOID, NAME, NAMELSAD, LSAD, CLASSFP, FUNCSTAT)
n_log(places, "TIGER places (incorporated + CDPs)")

cousubs <- map(list.files(file.path(tdir, "cousub"), full.names = TRUE), rd) |>
  bind_rows() |> select(STATEFP, COUNTYFP, COUSUBFP, GEOID, NAME, NAMELSAD, LSAD, CLASSFP, FUNCSTAT)
n_log(cousubs, "TIGER county subdivisions")

acs <- read_delim(file.path(RAW, "census", sprintf("acsdt5y%d-b01003.dat", ACS_YEAR)),
                  delim = "|", col_types = cols(.default = "c"))
acs <- acs |> transmute(GEO_ID, pop_acs = as.numeric(B01003_E001))
n_log(acs, "ACS B01003 rows (all summary levels)")

places <- places |> mutate(GEO_ID = paste0("1600000US", GEOID)) |> left_join(acs, by = "GEO_ID")
n_log(places, "places after ACS join")
message("  places missing ACS pop: ", sum(is.na(places$pop_acs)))
counties <- counties |> mutate(GEO_ID = paste0("0500000US", GEOID)) |> left_join(acs, by = "GEO_ID")
n_log(counties, "counties after ACS join")
message("  counties missing ACS pop: ", sum(is.na(counties$pop_acs)))

saveRDS(states,   file.path(PROC, "states.rds"))
saveRDS(counties, file.path(PROC, "counties.rds"))
saveRDS(places,   file.path(PROC, "places.rds"))
saveRDS(cousubs,  file.path(PROC, "cousubs.rds"))
