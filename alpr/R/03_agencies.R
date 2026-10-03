# Build the agency list: local police departments and sheriff's offices.
#
# Sources
#   FBI Crime Data Explorer (CDE) agency directory, one JSON per state  -> ORI, name, type, county
#   FBI CDE "Law Enforcement Employees" file (lee_1960_2025.csv)       -> population served
#   BJS LEAIC 2012 (ICPSR 35158), downloaded by hand                    -> place / county FIPS
source("R/00_setup.R")

# 1. FBI CDE agency directory ---------------------------------------------------------------------
cdir <- file.path(RAW, "fbi", "cde_agency_by_state")
for (s in STATES$abbr)
  fetch(sprintf("https://cde.ucr.cjis.gov/LATEST/agency/byStateAbbr/%s", s), file.path(cdir, paste0(s, ".json")))

cde <- map_dfr(STATES$abbr, \(s) {
  js <- fromJSON(file.path(cdir, paste0(s, ".json")), simplifyVector = FALSE)
  map_dfr(unlist(js, recursive = FALSE), \(a) tibble(
    ori = a$ori, cde_name = a$agency_name, cde_type = a$agency_type_name,
    state = a$state_abbr, cde_counties = a$counties %||% NA_character_,
    cde_lat = a$latitude %||% NA_real_, cde_lon = a$longitude %||% NA_real_))
})
n_log(cde, "CDE agency directory, all types, 50 states + DC")
cde <- distinct(cde, ori, .keep_all = TRUE)
n_log(cde, "CDE after de-duplicating ORI")
print(count(cde, cde_type, sort = TRUE))

# 2. Population served: most recent year each ORI appears in the LEE file ---------------------------
fetch_lee <- function(dest) {
  if (file.exists(dest)) return(dest)
  key <- paste0("additional-datasets/law-enforcement/", LEE_FILE)
  signed <- fromJSON(sprintf("https://cde.ucr.cjis.gov/LATEST/s3/signedurl?key=%s", key))[[1]]
  fetch(signed, dest)
}
lee_path <- fetch_lee(file.path(RAW, "fbi", LEE_FILE))
lee <- read_csv(lee_path, col_types = cols(.default = "c"))
n_log(lee, "LEE file, all years")
lee <- lee |> mutate(data_year = as.integer(data_year), population = as.numeric(na_if(population, "NULL")))
lee_latest <- lee |> filter(data_year >= 2020) |> arrange(ori, desc(data_year)) |> distinct(ori, .keep_all = TRUE) |>
  transmute(ori, lee_name = pub_agency_name, lee_unit = na_if(pub_agency_unit, "NULL"), lee_type = agency_type_name,
            lee_state = state_abbr, lee_county = county_name, pop_year = data_year, population_fbi = population,
            officers = as.numeric(officer_ct))
n_log(lee_latest, "LEE, latest record per ORI since 2020")
message("  LEE latest-year distribution:"); print(count(lee_latest, pop_year))

# 3. Combine and keep local police + sheriffs ------------------------------------------------------
ag <- full_join(cde, lee_latest, by = "ori")
n_log(ag, "CDE full-join LEE on ORI")
message("  in CDE only: ", sum(is.na(ag$lee_name)), "; in LEE only: ", sum(is.na(ag$cde_name)))
ag <- ag |> mutate(
  state = coalesce(state, lee_state),
  type_fbi = coalesce(cde_type, lee_type),
  # LEE names are the place name; LEE unit carries "Police Department" etc. where present.
  name = coalesce(cde_name, str_squish(paste(lee_name, coalesce(lee_unit, ""))))
)
ag <- ag |> filter(state %in% STATES$abbr)
n_log(ag, "agencies in 50 states + DC")
ag <- ag |> filter(type_fbi %in% c("City", "County"))
n_log(ag, "agencies with FBI type City or County")

ag <- ag |> mutate(agency_type = case_when(
  type_fbi == "City" ~ "Municipal police",
  str_detect(name, regex("sheriff", ignore_case = TRUE)) ~ "Sheriff",
  str_detect(name, regex("police|public safety", ignore_case = TRUE)) ~ "County police",
  TRUE ~ "County (type unclear)"))   # LEE-only rows named just "<County>"
print(count(ag, agency_type))

# 4. LEAIC FIPS codes ------------------------------------------------------------------------------
# ICPSR requires a login, so this file must be downloaded by hand (see README.md). Accepts the
# ICPSR tab-delimited (35158-0001-Data.tsv) or R (35158-0001-Data.rda) versions.
leaic_files <- list.files(file.path(RAW, "leaic"), pattern = "35158-0001-Data\\.(tsv|rda)$",
                          recursive = TRUE, full.names = TRUE)
saveRDS(ag, file.path(PROC, "agencies_fbi.rds"))   # usable by method A without LEAIC
write_csv(ag, file.path(PROC, "agencies_fbi.csv"))

if (!length(leaic_files)) {
  stop("LEAIC not found. Download ICPSR 35158 DS1 (Delimited or R format) and place ",
       "35158-0001-Data.tsv or .rda under data/raw/leaic/. See README.md.")
}
f <- leaic_files[1]
if (grepl("\\.rda$", f)) { e <- new.env(); load(f, envir = e); leaic <- get(ls(e)[1], envir = e) } else
  leaic <- read_tsv(f, col_types = cols(.default = "c"))
leaic <- as_tibble(leaic) |> mutate(across(everything(), as.character))
names(leaic) <- toupper(names(leaic))
n_log(leaic, "LEAIC 2012 rows")
need <- c("ORI9", "FIPS_ST", "FIPS_COUNTY", "FPLACE")
if (!all(need %in% names(leaic)))
  stop("LEAIC columns not as expected. Found: ", paste(names(leaic), collapse = ", "))

leaic <- leaic |> filter(!is.na(ORI9), ORI9 != "-1", nchar(ORI9) == 9) |>
  transmute(ori = ORI9, leaic_name = NAME,
            county_fips = paste0(str_pad(FIPS_ST, 2, pad = "0"), str_pad(FIPS_COUNTY, 3, pad = "0")),
            place_fips = ifelse(is.na(FPLACE) | FPLACE %in% c("99999", "-1", ""), NA,
                                paste0(str_pad(FIPS_ST, 2, pad = "0"), str_pad(FPLACE, 5, pad = "0")))) |>
  mutate(county_fips = ifelse(grepl("^-|999$", county_fips), NA, county_fips))
dups <- leaic |> count(ori) |> filter(n > 1)
message("  LEAIC ORIs appearing more than once (first kept): ", nrow(dups))
leaic <- distinct(leaic, ori, .keep_all = TRUE)
n_log(leaic, "LEAIC unique ORI9")

ag <- left_join(ag, leaic, by = "ori")
n_log(ag, "agencies after LEAIC join")
message("  agencies with no LEAIC record (ORI new since 2012 or changed): ", sum(is.na(ag$leaic_name)))
message("  Municipal PDs with no place FIPS: ", sum(ag$agency_type == "Municipal police" & is.na(ag$place_fips)))
message("  County agencies with no county FIPS: ", sum(ag$agency_type != "Municipal police" & is.na(ag$county_fips)))

saveRDS(ag, file.path(PROC, "agencies.rds"))
