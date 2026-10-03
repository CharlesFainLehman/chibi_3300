# Method A: match the OSM operator tag to an agency name within the same state.
#
# Steps (on unique state + operator strings, then joined back to cameras):
#   1. Normalize text; detect agency kind (police / sheriff / none) and the jurisdiction "core".
#   2. Exact match on (state, kind, core).
#   3. Exact match on (state, kind, core with "city/town/township/village/borough" dropped), if unique.
#   4. Operators with no kind word ("City of X", "County of Y", bare place name): match the
#      jurisdiction to its municipal PD or sheriff. Flagged as basis = "jurisdiction_only".
#   5. Fuzzy (Jaro-Winkler) on core within state and kind. Accepted only if distance <= FUZZY_MAX and
#      the runner-up is at least FUZZY_GAP worse. Every fuzzy candidate is written for review.
#   Never across states.
source("R/00_setup.R")
library(stringdist)

FUZZY_MAX <- 0.05
FUZZY_GAP <- 0.05

cams <- readRDS(file.path(PROC, "osm_alpr.rds"))
ag   <- readRDS(file.path(PROC, "agencies_fbi.rds"))
n_log(cams, "cameras in"); n_log(ag, "agencies in")

basic <- function(x) {
  x |> str_to_lower() |> str_replace_all("[’`]", "'") |> str_replace_all("'s\\b", "s") |>
    str_replace_all("&", " and ") |> str_replace_all("\\bst\\.\\s", "saint ") |>
    str_replace_all("[^a-z0-9 ]", " ") |> str_squish() |>
    str_replace_all("\\bco\\b", "county") |> str_replace_all("\\btwp\\b", "township") |>
    # unambiguous big-city abbreviations
    str_replace_all(c("\\bnypd\\b" = "new york city police department", "\\blapd\\b" = "los angeles police department",
                      "\\bsfpd\\b" = "san francisco police department", "\\bnopd\\b" = "new orleans police department",
                      "\\blvmpd\\b" = "las vegas metropolitan police department"))
}

MUNI_WORDS <- "\\b(city|town|township|twp|village|borough|boro)\\b"
ORG_WORDS  <- paste0("\\b(police|pd|dept|department|dep|sheriffs|sheriff|office|so|public safety|dps|",
                     "division|bureau|agency|the|of|law enforcement|services|service|force)\\b")

parse_name <- function(x) {
  b <- basic(x)
  kind <- case_when(
    str_detect(b, "\\bsheriffs?\\b") | str_detect(b, "\\bso$") ~ "sheriff",
    str_detect(b, "\\b(police|pd|public safety|dps)\\b") ~ "police",
    TRUE ~ "none")
  prefix <- case_when(
    str_detect(b, "^(the )?(city and county of|consolidated city of)\\b") ~ "city",
    str_detect(b, "^(the )?county of\\b") | str_detect(b, "\\b(county|parish|borough)$") ~ "county",
    str_detect(b, "^(the )?(city|town|village|township|borough) of\\b") ~ "muni",
    TRUE ~ "")
  core <- b |>
    str_replace("^(the )?(city and county of|consolidated city of|city of|town of|village of|township of|borough of|county of)\\s+", "") |>
    str_replace_all("\\bpolice department\\b|\\bpolice dept\\b|\\bpolice dep\\b", " ") |>
    str_replace_all(ORG_WORDS, " ") |> str_squish()
  core2 <- core |> str_replace_all(MUNI_WORDS, " ") |> str_squish()
  tibble(norm = b, kind = kind, prefix = prefix, core = core, core2 = core2)
}

# Agency side: kind from FBI type, not from the name.
ag_p <- bind_cols(ag |> select(ori, name, state, agency_type), parse_name(ag$name)) |>
  mutate(kind = if_else(agency_type == "Municipal police", "police",
                        if_else(agency_type == "Sheriff", "sheriff", "police")),
         is_county = agency_type != "Municipal police",
         core2 = if_else(is_county, core, core2))   # keep "county" etc. for county agencies

uniq_key <- function(df, key) df |> group_by(state, kind, .data[[key]]) |>
  summarise(n_ag = n(), ori = first(ori), .groups = "drop") |> rename(k = all_of(key))
k1 <- uniq_key(ag_p, "core"); k2 <- uniq_key(ag_p, "core2")

# Operator side -------------------------------------------------------------------------------------
ops <- cams |> filter(!is.na(operator), str_squish(operator) != "") |> count(state, operator, name = "n_cameras")
n_log(ops, "unique (state, operator) strings")
ops <- bind_cols(ops, parse_name(ops$operator))

# Step 2: exact on core
m <- ops |> left_join(k1, by = c("state", "kind", "core" = "k"))
n_log(m, "operators after exact join (core)")
m <- m |> mutate(a_ori = if_else(n_ag == 1, ori, NA), a_basis = if_else(n_ag == 1, "exact", NA),
                 ambiguous = !is.na(n_ag) & n_ag > 1) |> select(-ori, -n_ag)
message("  exact (core): ", sum(!is.na(m$a_ori)), "; ambiguous: ", sum(m$ambiguous))

# Step 3: exact on core2
m <- m |> left_join(k2, by = c("state", "kind", "core2" = "k"))
n_log(m, "operators after exact join (core2)")
m <- m |> mutate(take = is.na(a_ori) & !is.na(n_ag) & n_ag == 1 & kind != "none",
                 a_basis = if_else(take, "exact_no_muni_word", a_basis),
                 a_ori = if_else(take, ori, a_ori),
                 ambiguous = ambiguous | (is.na(a_ori) & !is.na(n_ag) & n_ag > 1)) |>
  select(-ori, -n_ag, -take)
message("  + exact (core2): ", sum(m$a_basis == "exact_no_muni_word", na.rm = TRUE))

# Step 4: jurisdiction only ("City of X", "County of Y", "X County", bare "X")
juris <- m |> filter(is.na(a_ori), kind == "none") |>
  mutate(target_kind = if_else(prefix == "county", "sheriff", "police"),
         target_core = if_else(prefix == "county", str_replace(core, "^(.*?)( county| parish)?$", "\\1 county"), core2))
ag_j <- ag_p |> mutate(jk = if_else(is_county, "sheriff", "police"),
                       jcore = if_else(is_county, str_replace(core, "^(.*?)( county| parish)?$", "\\1 county"), core2)) |>
  group_by(state, jk, jcore) |> summarise(n_ag = n(), ori_j = first(ori), .groups = "drop")
juris <- juris |> left_join(ag_j, by = c("state", "target_kind" = "jk", "target_core" = "jcore")) |>
  filter(n_ag == 1) |> select(state, operator, ori_j)
m <- m |> left_join(juris, by = c("state", "operator")) |>
  mutate(a_basis = if_else(is.na(a_ori) & !is.na(ori_j), "jurisdiction_only", a_basis),
         a_ori = coalesce(a_ori, ori_j)) |> select(-ori_j)
message("  + jurisdiction only: ", sum(m$a_basis == "jurisdiction_only", na.rm = TRUE))

# Step 5: fuzzy within state and kind (police/sheriff operators only)
fz <- m |> filter(is.na(a_ori), kind != "none", core2 != "")
fuzzy <- pmap_dfr(fz |> select(state, operator, kind, core2, n_cameras), \(state, operator, kind, core2, n_cameras) {
  cand <- ag_p[ag_p$state == state & ag_p$kind == kind, ]
  if (!nrow(cand)) return(NULL)
  d <- stringdist(core2, cand$core2, method = "jw", p = 0.1)
  o <- order(d)
  tibble(state = state, operator = operator, n_cameras = n_cameras, op_core = core2,
         best_ori = cand$ori[o[1]], best_name = cand$name[o[1]], best_dist = d[o[1]],
         second_name = if (length(o) > 1) cand$name[o[2]] else NA, second_dist = if (length(o) > 1) d[o[2]] else NA)
})
fuzzy <- fuzzy |> mutate(accepted = best_dist <= FUZZY_MAX & (is.na(second_dist) | second_dist - best_dist >= FUZZY_GAP)) |>
  arrange(desc(accepted), best_dist)
write_csv(fuzzy |> filter(best_dist <= 0.15), file.path(OUT, "method_a_fuzzy_matches_for_review.csv"))
message("  fuzzy candidates (dist <= 0.15) written for review: ", sum(fuzzy$best_dist <= 0.15),
        "; accepted: ", sum(fuzzy$accepted))
m <- m |> left_join(fuzzy |> filter(accepted) |> select(state, operator, f_ori = best_ori, f_dist = best_dist),
                    by = c("state", "operator")) |>
  mutate(a_basis = if_else(is.na(a_ori) & !is.na(f_ori), "fuzzy", a_basis), a_ori = coalesce(a_ori, f_ori)) |>
  select(-f_ori)

# Results ------------------------------------------------------------------------------------------
m <- m |> left_join(ag |> select(a_ori = ori, a_name = name, a_type = agency_type), by = "a_ori")
write_csv(m |> arrange(state, desc(n_cameras)), file.path(OUT, "method_a_operator_crosswalk.csv"))
message("\nMethod A summary (unique state+operator strings / cameras):")
print(m |> mutate(a_basis = coalesce(a_basis, if_else(ambiguous, "unmatched_ambiguous", "unmatched"))) |>
        group_by(a_basis) |> summarise(strings = n(), cameras = sum(n_cameras)) |> arrange(desc(cameras)))

cams_a <- cams |> select(osm_id, state, operator) |>
  left_join(m |> select(state, operator, a_ori, a_basis), by = c("state", "operator"))
n_log(cams_a, "cameras after joining method A result")
stopifnot(nrow(cams_a) == nrow(cams))
saveRDS(cams_a |> select(osm_id, a_ori, a_basis), file.path(PROC, "method_a.rds"))
