# Validate against agency-reported Flock camera counts (N&O "Private Eyes", Flock transparency
# portals scraped 2024-04-26; the file name says 2023 but the `updated` field is 2024-04-26).
source("R/00_setup.R")
library(ggplot2)
library(stringdist)

repo <- file.path(RAW, "private_eyes", "repo")
if (!dir.exists(repo))
  system2("git", c("clone", "--depth", "1", "https://github.com/mcclatchy-southeast/private_eyes", repo))
writeLines(system2("git", c("-C", repo, "log", "-1", "--format=%H %cd"), stdout = TRUE),
           file.path(RAW, "private_eyes", "commit.txt"))

pe <- read_csv(file.path(repo, "data", "latest_usage04262023.csv"), col_types = cols(.default = "c"))
n_log(pe, "N&O latest usage file")
pe <- pe |> filter(!str_detect(agency, "Flock|Training|DNU"))
n_log(pe, "after dropping Flock / Training / DNU rows")
pe <- pe |> mutate(reported = suppressWarnings(as.numeric(number_of_owned_cameras)))
message("  rows with missing camera count (dropped): ", sum(is.na(pe$reported)))
pe <- pe |> filter(!is.na(reported))
n_log(pe, "after dropping missing camera counts")

# Parse "<Name> <ST> <PD|SO>" style names ---------------------------------------------------------
STATE_OVERRIDE <- c("Yuba County Sheriffs Office" = "CA")   # only US Yuba County is in California
st_tok <- function(x) {
  toks <- str_split(str_replace_all(x, "[^A-Za-z ]", " "), "\\s+")[[1]]
  hits <- toks[toks %in% STATES$abbr & !toks %in% c("PD", "SO")]
  if (length(hits)) tail(hits, 1) else NA_character_
}
pe <- pe |> mutate(
  state = coalesce(unname(STATE_OVERRIDE[agency]), vapply(agency, st_tok, "")),
  name_wo_state = str_squish(str_replace(agency, paste0("\\b", state, "\\b"), " ")),
  name_wo_state = str_replace(name_wo_state, "\\bTwp\\b", "Township"))
message("  rows with no parseable state: ", sum(is.na(pe$state)))

# Reuse method A's parser.
src <- readLines("R/04_method_a.R")
eval(parse(text = src[grep("^basic <- function", src):(grep("^# Agency side", src) - 1)]))
ag <- readRDS(file.path(PROC, "agencies_fbi.rds"))
ag_p <- bind_cols(ag |> select(ori, name, state, agency_type), parse_name(ag$name)) |>
  mutate(kind = if_else(agency_type == "Sheriff", "sheriff", "police"),
         core2 = if_else(agency_type != "Municipal police", core, core2))
pe <- bind_cols(pe, parse_name(pe$name_wo_state) |> select(kind, core, core2)) |>
  mutate(core2 = if_else(str_detect(core, "\\b(county|parish)\\b"), core, core2))

match_one <- function(state, kind, core, core2) {
  cand <- ag_p[ag_p$state %in% state & (kind == "none" | ag_p$kind == kind), ]
  if (!nrow(cand)) return(tibble(ori = NA, basis = "no_candidates", dist = NA, cand_name = NA))
  e1 <- cand[cand$core == core, ]
  if (nrow(e1) == 1) return(tibble(ori = e1$ori, basis = "exact", dist = 0, cand_name = e1$name))
  e2 <- cand[cand$core2 == core2, ]
  if (nrow(e2) == 1) return(tibble(ori = e2$ori, basis = "exact_no_muni_word", dist = 0, cand_name = e2$name))
  if (nrow(e1) > 1 || nrow(e2) > 1) return(tibble(ori = NA, basis = "ambiguous", dist = 0, cand_name = paste(e2$name, collapse = " | ")))
  d <- stringdist(core2, cand$core2, method = "jw", p = 0.1); o <- order(d)
  ok <- d[o[1]] <= 0.05 && (length(o) == 1 || d[o[2]] - d[o[1]] >= 0.05)
  tibble(ori = if (ok) cand$ori[o[1]] else NA, basis = if (ok) "fuzzy" else "unmatched",
         dist = d[o[1]], cand_name = cand$name[o[1]])
}
mm <- pmap_dfr(pe |> select(state, kind, core, core2), match_one)
pe <- bind_cols(pe, mm)
n_log(pe, "validation agencies after matching")
print(count(pe, basis))
write_csv(pe |> select(id, agency, state, kind, reported, basis, dist, matched_ori = ori, cand_name),
          file.path(OUT, "validation_agency_matches.csv"))

# Camera counts by method (Flock only) ----------------------------------------------------------------
cams <- readRDS(file.path(PROC, "cameras_assigned.rds"))
flock <- cams |> filter(is_flock)
cnt_a <- flock |> filter(!is.na(a_ori)) |> count(ori = a_ori, name = "osm_a")
cnt_b <- flock |> filter(!is.na(b_ori)) |> count(ori = b_ori, name = "osm_b")
v <- pe |> filter(!is.na(ori)) |> select(id, agency, state, ori, reported, basis)
n_log(v, "matched validation agencies")
dup <- v |> count(ori) |> filter(n > 1)
if (nrow(dup)) message("  ORIs matched by >1 validation row (all kept): ", nrow(dup))
v <- v |> left_join(cnt_a, by = "ori") |> left_join(cnt_b, by = "ori") |>
  mutate(osm_a = coalesce(osm_a, 0L), osm_b = coalesce(osm_b, 0L))
n_log(v, "after joining OSM counts")
write_csv(v, file.path(OUT, "validation_comparison.csv"))

metrics <- function(osm, rep) {
  pos <- rep > 0
  tibble(n = length(rep),
         pearson_r = cor(osm, rep), spearman_rho = cor(osm, rep, method = "spearman"),
         pearson_r_log1p = cor(log1p(osm), log1p(rep)),
         median_ratio_osm_over_reported = median(osm[pos] / rep[pos]),
         median_abs_error = median(abs(osm - rep)),
         share_osm_zero = mean(osm == 0), share_osm_above_reported = mean(osm > rep))
}
HAS_B <- any(!is.na(cams$b_ori))
res <- bind_rows(A = metrics(v$osm_a, v$reported), B = if (HAS_B) metrics(v$osm_b, v$reported), .id = "method")
print(res, width = Inf)
write_csv(res, file.path(OUT, "validation_metrics.csv"))

long <- bind_rows(
  v |> transmute(method = "Method A: operator tag", reported, osm = osm_a),
  if (HAS_B) v |> transmute(method = "Method B: spatial join", reported, osm = osm_b))
lab <- res |> mutate(method = c("Method A: operator tag", "Method B: spatial join")[seq_len(n())],
                     txt = sprintf("n = %d   r(log) = %.2f   median ratio = %.2f   MAE = %.0f",
                                   n, pearson_r_log1p, median_ratio_osm_over_reported, median_abs_error))
brks <- c(0, 1, 3, 10, 30, 100, 300, 1000)
p <- ggplot(long, aes(reported, osm)) +
  geom_abline(slope = 1, intercept = 0, colour = "#52514e", linewidth = 0.4, linetype = "dashed") +
  geom_point(colour = "#2a78d6", alpha = 0.6, size = 2.2, stroke = 0) +
  geom_text(data = lab, aes(x = 0, y = Inf, label = txt), hjust = 0, vjust = 1.5, size = 3, colour = "#52514e", inherit.aes = FALSE) +
  scale_x_continuous(trans = "log1p", breaks = brks) + scale_y_continuous(trans = "log1p", breaks = brks) +
  facet_wrap(~method) +
  labs(x = "Agency-reported Flock cameras, April 2024 (log scale, +1)",
       y = "OSM Flock cameras assigned, 2026 (log scale, +1)",
       title = "OSM camera counts vs. agency-reported Flock counts",
       subtitle = "Dashed line = equal counts. OSM data are newer, so points above the line are expected.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#e6e5e0", linewidth = 0.3),
        plot.background = element_rect(fill = "#fcfcfb", colour = NA), text = element_text(colour = "#0b0b0b"),
        plot.subtitle = element_text(colour = "#52514e"))
ggsave(file.path(OUT, "validation_scatter.png"), p, width = 10, height = 5.2, dpi = 150)
