# Combine methods A and B; write cameras.csv, agencies.csv, summary tables, density plot.
source("R/00_setup.R")
library(ggplot2)

cams <- readRDS(file.path(PROC, "osm_alpr.rds"))
a    <- readRDS(file.path(PROC, "method_a.rds"))
ag_path <- file.path(PROC, "agencies.rds")
HAS_B <- file.exists(file.path(PROC, "method_b.rds")) && file.exists(ag_path)
ag <- if (HAS_B) readRDS(ag_path) else readRDS(file.path(PROC, "agencies_fbi.rds"))
if (!HAS_B) {
  warning("Method B not available (LEAIC missing). b_* columns are NA.")
  b <- tibble(osm_id = cams$osm_id, b_excluded = NA, place_geoid = NA_character_, cousub_geoid = NA_character_,
              county_geoid = NA_character_, b_level = NA_character_, b_ori = NA_character_)
} else b <- readRDS(file.path(PROC, "method_b.rds"))

n_log(cams, "cameras")
x <- cams |> left_join(a, by = "osm_id"); n_log(x, "cameras + method A")
x <- x |> left_join(b, by = "osm_id");    n_log(x, "cameras + method B")
stopifnot(nrow(x) == nrow(cams))
nm <- ag |> select(ori, name, agency_type)
x <- x |> left_join(nm |> rename(a_ori = ori, a_name = name, a_type = agency_type), by = "a_ori") |>
  left_join(nm |> rename(b_ori = ori, b_name = name, b_type = agency_type), by = "b_ori")
n_log(x, "cameras + agency names")
x <- x |> mutate(
  is_flock = coalesce(str_detect(str_to_lower(paste(manufacturer, brand)), "flock"), FALSE),
  a_b_compare = case_when(
    is.na(a_ori) & is.na(b_ori) ~ "neither",
    is.na(a_ori) ~ "B only",
    is.na(b_ori) ~ "A only",
    a_ori == b_ori ~ "agree",
    TRUE ~ "disagree"))
saveRDS(x, file.path(PROC, "cameras_assigned.rds"))
message("A vs B comparison:"); print(count(x, a_b_compare))
write_csv(x |> select(osm_id, lat, lon, state, manufacturer, brand, operator, operator_type, surveillance,
                      surveillance_zone, last_edit, is_flock,
                      a_ori, a_name, a_type, a_basis,
                      b_excluded, place_geoid, cousub_geoid, county_geoid,
                      b_level, b_ori, b_name, b_type, a_b_compare),
          file.path(OUT, "cameras.csv"))

# Agency table ----------------------------------------------------------------------------------------
cnt <- function(col, flock_only = FALSE) {
  d <- if (flock_only) filter(x, is_flock) else x
  d |> filter(!is.na(.data[[col]])) |> count(ori = .data[[col]])
}
agt <- ag |> select(ori, name, agency_type, state, any_of(c("place_fips", "county_fips")), population_fbi, pop_year) |>
  left_join(cnt("a_ori") |> rename(cameras_a = n), by = "ori") |>
  left_join(cnt("b_ori") |> rename(cameras_b = n), by = "ori") |>
  left_join(cnt("a_ori", TRUE) |> rename(flock_a = n), by = "ori") |>
  left_join(cnt("b_ori", TRUE) |> rename(flock_b = n), by = "ori") |>
  mutate(across(c(cameras_a, cameras_b, flock_a, flock_b), \(v) coalesce(v, 0L)),
         across(c(cameras_b, flock_b), \(v) if (HAS_B) v else NA_integer_),
         per10k_a = if_else(population_fbi > 0, 1e4 * cameras_a / population_fbi, NA),
         per10k_b = if_else(population_fbi > 0, 1e4 * cameras_b / population_fbi, NA))
n_log(agt, "agency table (all local PDs + sheriffs, incl. zero cameras)")
stopifnot(sum(agt$cameras_a) == sum(!is.na(x$a_ori)))
write_csv(agt, file.path(OUT, "agencies.csv"))

# Summary statistics ------------------------------------------------------------------------------------
popbin <- function(p) cut(p, c(-Inf, 2500, 10000, 25000, 50000, 100000, 250000, Inf), right = FALSE,
                          labels = c("<2.5k", "2.5k-10k", "10k-25k", "25k-50k", "50k-100k", "100k-250k", "250k+"))
agt <- agt |> mutate(pop_bin = coalesce(as.character(popbin(population_fbi)), "unknown"))
stats <- function(d, col) d |> summarise(
  agencies = n(), cameras = sum(.data[[col]]), mean = mean(.data[[col]]), median = median(.data[[col]]),
  p10 = quantile(.data[[col]], .10), p25 = quantile(.data[[col]], .25), p75 = quantile(.data[[col]], .75),
  p90 = quantile(.data[[col]], .90), p99 = quantile(.data[[col]], .99), max = max(.data[[col]]), .groups = "drop")
summ <- list()
for (m in c("a", "b")) {
  if (m == "b" && !HAS_B) next
  col <- paste0("cameras_", m)
  for (set in c("all agencies", "agencies with >=1 camera")) {
    d <- if (set == "all agencies") agt else filter(agt, .data[[col]] >= 1)
    summ[[length(summ) + 1]] <- bind_rows(
      stats(d, col) |> mutate(group_var = "overall", group = "all"),
      stats(group_by(d, group = agency_type), col) |> mutate(group_var = "agency_type"),
      stats(group_by(d, group = pop_bin), col) |> mutate(group_var = "population_bin")) |>
      mutate(method = toupper(m), set = set, .before = 1)
  }
}
summ <- bind_rows(summ) |> relocate(group_var, group, .after = set)
write_csv(summ, file.path(OUT, "summary_stats.csv"))
print(summ |> filter(group_var == "overall"), width = Inf)

# Density plot: cameras per agency, agencies with >=1 camera, log x -------------------------------------
dd <- bind_rows(agt |> transmute(method = "Method A: operator tag", n = cameras_a),
                if (HAS_B) agt |> transmute(method = "Method B: spatial join", n = cameras_b)) |> filter(n >= 1)
p <- ggplot(dd, aes(n, colour = method)) +
  geom_density(linewidth = 0.8, adjust = 1.2) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000)) +
  scale_colour_manual(values = c("Method A: operator tag" = "#2a78d6", "Method B: spatial join" = "#eb6834"), name = NULL) +
  labs(x = "Cameras per agency (log scale)", y = "Density",
       title = "Distribution of OSM ALPR cameras per agency",
       subtitle = sprintf("Agencies with at least one camera. OSM pull %s.", readLines(file.path(PROC, "osm_pull_date.txt")))) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", legend.justification = "left", panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e0", linewidth = 0.3),
        plot.background = element_rect(fill = "#fcfcfb", colour = NA), plot.subtitle = element_text(colour = "#52514e"))
ggsave(file.path(OUT, "density_cameras_per_agency.png"), p, width = 8, height = 4.5, dpi = 150)
