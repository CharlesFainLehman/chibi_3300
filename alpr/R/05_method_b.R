# Method B: spatial join. Camera -> incorporated place -> municipal PD;
#           otherwise -> county -> sheriff (or county police where it serves the unincorporated area).
#
# Order of assignment for each (non-private) camera:
#   1. Inside an active incorporated place (TIGER CLASSFP C*, FUNCSTAT A/B/F) whose FIPS code is
#      the LEAIC place code of a municipal PD                                   -> that PD
#   2. Inside an active county subdivision (towns/townships, FUNCSTAT A) whose FIPS code is the
#      LEAIC place code of a municipal PD (NJ/PA/MI/NY... township police)      -> that PD
#      (Not in the original spec. Without it, cameras in township-policed areas fall to the sheriff.
#       Kept separate in column b_level so the strict place/county version can be recovered.)
#   3. Otherwise                                                                -> county agency
#      (the Sheriff or County police with the largest FBI population served in that county).
# Census places that are CDPs, military, or inactive count as unincorporated.
source("R/00_setup.R")

cams     <- readRDS(file.path(PROC, "osm_alpr.rds"))
ag       <- readRDS(file.path(PROC, "agencies.rds"))       # needs LEAIC (03_agencies.R)
places   <- readRDS(file.path(PROC, "places.rds"))
cousubs  <- readRDS(file.path(PROC, "cousubs.rds"))
counties <- readRDS(file.path(PROC, "counties.rds"))
n_log(cams, "cameras in")

# 1. Exclude private cameras ------------------------------------------------------------------------
PRIVATE_OP_TYPES <- c("private", "business", "university", "school", "religious", "community", "hoa")
PRIVATE_OP_RX <- regex(paste0(
  "\\b(hoa|homeowners?|home owners?|property owners|owners association|condominium|condo|apartments?|",
  "residences|community association|poa|llc|inc|corp|corporation|company|plaza|mall|shopping|",
  "retail|store|stores|lowe'?s|home depot|walmart|wal-mart|target|kroger|costco|sam'?s club|",
  "simon property|casino|casinos|resort|hotel|fedex|ups|amazon|school|schools|academy|isd|",
  "school district|university|college|campus|hospital|medical|health|church|parish church)\\b"),
  ignore_case = TRUE)

cams <- cams |> mutate(
  excl_surveillance_private = coalesce(str_to_lower(surveillance) == "private", FALSE),
  excl_operator_type = coalesce(str_to_lower(operator_type) %in% PRIVATE_OP_TYPES, FALSE),
  excl_operator_text = coalesce(str_detect(operator, PRIVATE_OP_RX), FALSE) &
                       !coalesce(str_detect(operator, regex("police|sheriff", ignore_case = TRUE)), FALSE),
  b_excluded = excl_surveillance_private | excl_operator_type | excl_operator_text)
message("Excluded from method B as private: ", sum(cams$b_excluded), " of ", nrow(cams))
message("  surveillance=private: ", sum(cams$excl_surveillance_private),
        "; operator:type private/business/school etc.: ", sum(cams$excl_operator_type),
        "; operator name looks private: ", sum(cams$excl_operator_text))
write_csv(cams |> filter(b_excluded) |> count(operator, operator_type, surveillance, sort = TRUE),
          file.path(OUT, "method_b_excluded_private_operators.csv"))

# 2. Point-in-polygon ---------------------------------------------------------------------------------
pts <- st_as_sf(cams |> select(osm_id, lon, lat), coords = c("lon", "lat"), crs = 4326) |> st_transform(st_crs(places))
first_hit <- function(pts, poly, col) {
  idx <- st_intersects(pts, poly)
  nh <- lengths(idx)
  if (any(nh > 1)) message(sprintf("  %d cameras fall in >1 %s polygon (first kept)", sum(nh > 1), col))
  vapply(idx, \(j) if (length(j)) poly[[col]][j[1]] else NA_character_, "")
}
inc_places <- places |> filter(str_starts(CLASSFP, "C"), FUNCSTAT %in% c("A", "B", "F"))
act_cousub <- cousubs |> filter(FUNCSTAT == "A")
cams$place_geoid  <- first_hit(pts, inc_places, "GEOID")
cams$cousub_geoid <- first_hit(pts, act_cousub, "GEOID")
cams$county_geoid <- first_hit(pts, counties, "GEOID")
message("  in an incorporated place: ", sum(!is.na(cams$place_geoid)),
        "; in an active county subdivision: ", sum(!is.na(cams$cousub_geoid)),
        "; in no county polygon: ", sum(is.na(cams$county_geoid)))

# 3. Geography -> agency keys ------------------------------------------------------------------------
muni <- ag |> filter(agency_type == "Municipal police", !is.na(place_fips))
dup_place <- muni |> count(place_fips) |> filter(n > 1)
message("Place FIPS codes with >1 municipal PD in LEAIC/FBI (largest population kept): ", nrow(dup_place),
        " (", sum(dup_place$n), " agencies)")
place_to_pd <- muni |> arrange(place_fips, desc(coalesce(population_fbi, -1))) |> distinct(place_fips, .keep_all = TRUE) |>
  select(geo = place_fips, pd_ori = ori)

cty <- ag |> filter(agency_type != "Municipal police", !is.na(county_fips))
multi_cty <- cty |> count(county_fips) |> filter(n > 1)
message("Counties with >1 sheriff/county police agency (largest FBI population kept): ", nrow(multi_cty))
write_csv(cty |> semi_join(multi_cty, by = "county_fips") |> select(county_fips, ori, name, agency_type, population_fbi) |>
            arrange(county_fips, desc(population_fbi)), file.path(OUT, "method_b_counties_with_multiple_county_agencies.csv"))
county_to_ag <- cty |> arrange(county_fips, desc(coalesce(population_fbi, -1))) |> distinct(county_fips, .keep_all = TRUE) |>
  select(geo = county_fips, cty_ori = ori)

# 4. Assign -------------------------------------------------------------------------------------------
b <- cams |> select(osm_id, b_excluded, place_geoid, cousub_geoid, county_geoid)
b <- b |> left_join(place_to_pd, by = c("place_geoid" = "geo")); n_log(b, "after place -> PD join")
# county subdivision GEOID is 10 digits (state+county+cousub); LEAIC place code is state + 5 digits
b <- b |> mutate(cousub_key = if_else(is.na(cousub_geoid), NA, paste0(substr(cousub_geoid, 1, 2), substr(cousub_geoid, 6, 10)))) |>
  left_join(place_to_pd |> rename(cs_pd_ori = pd_ori), by = c("cousub_key" = "geo")); n_log(b, "after county subdivision -> PD join")
b <- b |> left_join(county_to_ag, by = c("county_geoid" = "geo")); n_log(b, "after county -> sheriff join")
stopifnot(nrow(b) == nrow(cams))

b <- b |> mutate(
  b_level = case_when(
    b_excluded ~ "excluded_private",
    !is.na(pd_ori) ~ "place",
    !is.na(cs_pd_ori) ~ "county_subdivision",
    !is.na(place_geoid) & !is.na(cty_ori) ~ "place_without_pd_to_county",
    !is.na(cty_ori) ~ "county",
    TRUE ~ "unassigned"),
  b_ori = case_when(b_level == "place" ~ pd_ori, b_level == "county_subdivision" ~ cs_pd_ori,
                    b_level %in% c("county", "place_without_pd_to_county") ~ cty_ori, TRUE ~ NA))
message("\nMethod B assignment level:"); print(count(b, b_level))
saveRDS(b |> select(osm_id, b_excluded, place_geoid, cousub_geoid, county_geoid, b_level, b_ori), file.path(PROC, "method_b.rds"))
