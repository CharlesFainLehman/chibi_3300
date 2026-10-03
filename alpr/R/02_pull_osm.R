# Pull all ALPR nodes (man_made=surveillance + surveillance:type=ALPR) from Overpass, state by state.
#
# Each state is queried by its TIGER bounding box (split if the server times out), then every node
# is assigned to a state by point-in-polygon and de-duplicated by OSM id (bboxes overlap).
# overpass-api.de is tried first; the VK mirror (maps.mail.ru) is the fallback. Both run the
# same Overpass API software against the live OSM database.
source("R/00_setup.R")
library(httr2)

ENDPOINTS <- c("https://overpass-api.de/api/interpreter",
               "https://maps.mail.ru/osm/tools/overpass/api/interpreter")
PULL_DATE <- format(Sys.Date())
odir <- file.path(RAW, "osm", PULL_DATE)
dir.create(odir, recursive = TRUE, showWarnings = FALSE)

states <- readRDS(file.path(PROC, "states.rds"))

# Drop endpoints that cannot be reached at all from this machine (logged, not silent).
reachable <- vapply(ENDPOINTS, \(ep) {
  r <- tryCatch(request(ep) |> req_body_form(data = "[out:json];node(1);out;") |> req_timeout(90) |>
                  req_error(is_error = \(r) FALSE) |> req_perform(), error = function(e) NULL)
  !is.null(r)
}, logical(1))
message("Overpass endpoints reachable: ", paste(names(reachable), reachable, collapse = "; "))
ENDPOINTS <- ENDPOINTS[reachable]
stopifnot(length(ENDPOINTS) > 0)

overpass <- function(bbox) {   # bbox = c(s, w, n, e)
  q <- sprintf('[out:json][timeout:300];node["man_made"="surveillance"]["surveillance:type"="ALPR"](%s);out meta;',
               paste(sprintf("%.6f", bbox), collapse = ","))
  for (ep in ENDPOINTS) for (i in 1:3) {
    res <- tryCatch(
      request(ep) |> req_body_form(data = q) |> req_timeout(330) |>
        req_error(is_error = \(r) FALSE) |> req_perform(),
      error = function(e) NULL)
    if (!is.null(res) && resp_status(res) == 200) {
      txt <- resp_body_string(res)
      js <- tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
      # Overpass reports server-side timeouts in "remark" with a 200 status: treat as failure.
      if (!is.null(js) && is.null(js$remark)) return(list(json = js, text = txt, endpoint = ep))
    }
    Sys.sleep(5 * i)
  }
  NULL
}

# Query a bbox; on failure split into quadrants (max depth 3).
pull_bbox <- function(bbox, depth = 0) {
  r <- overpass(bbox)
  if (!is.null(r)) return(list(r))
  if (depth >= 3) stop("Overpass failed for bbox ", paste(bbox, collapse = ","))
  message("    splitting bbox (depth ", depth + 1, ")")
  mid_lat <- mean(bbox[c(1, 3)]); mid_lon <- mean(bbox[c(2, 4)])
  quads <- list(c(bbox[1], bbox[2], mid_lat, mid_lon), c(bbox[1], mid_lon, mid_lat, bbox[4]),
                c(mid_lat, bbox[2], bbox[3], mid_lon), c(mid_lat, mid_lon, bbox[3], bbox[4]))
  unlist(lapply(quads, pull_bbox, depth = depth + 1), recursive = FALSE)
}

state_bboxes <- function(st) {
  bb <- st_bbox(st)
  if (st$STUSPS == "AK")   # Alaska crosses the antimeridian
    return(list(c(51, -180, 72, -129), c(51, 172, 54, 180)))
  list(c(bb["ymin"], bb["xmin"], bb["ymax"], bb["xmax"]) + c(-0.01, -0.01, 0.01, 0.01))
}

tag <- function(tags, k) { v <- tags[[k]]; if (is.null(v)) NA_character_ else as.character(v) }

log_rows <- list()
all_nodes <- list()
for (i in seq_len(nrow(states))) {
  st <- states[i, ]
  f <- file.path(odir, sprintf("overpass_%s.json", st$STUSPS))
  parts <- list()
  if (file.exists(f)) {
    parts <- readRDS(sub("\\.json$", ".rds", f))
  } else {
    for (bb in state_bboxes(st)) parts <- c(parts, pull_bbox(unname(bb)))
    writeLines(vapply(parts, `[[`, "", "text"), f)     # raw response(s), one per line-block
    saveRDS(parts, sub("\\.json$", ".rds", f))
  }
  els <- unlist(lapply(parts, \(p) p$json$elements), recursive = FALSE)
  df <- map_dfr(els, \(e) tibble(
    osm_id = as.character(e$id), lat = e$lat, lon = e$lon,
    manufacturer = tag(e$tags, "manufacturer"), operator = tag(e$tags, "operator"),
    operator_type = tag(e$tags, "operator:type"), brand = tag(e$tags, "brand"),
    surveillance = tag(e$tags, "surveillance"), surveillance_zone = tag(e$tags, "surveillance:zone"),
    last_edit = e$timestamp, osm_version = e$version,
    all_tags = as.character(toJSON(e$tags, auto_unbox = TRUE))))
  df$query_state <- st$STUSPS
  all_nodes[[st$STUSPS]] <- df
  log_rows[[st$STUSPS]] <- tibble(state = st$STUSPS, nodes_in_bbox = nrow(df),
                                  endpoint = paste(unique(vapply(parts, `[[`, "", "endpoint")), collapse = ";"),
                                  osm_base = parts[[1]]$json$osm3s$timestamp_osm_base)
  message(sprintf("  %s: %d nodes in bbox (%s)", st$STUSPS, nrow(df), log_rows[[st$STUSPS]]$endpoint))
}

raw <- bind_rows(all_nodes)
n_log(raw, "nodes returned across all state bboxes (with overlap)")
cams <- raw |> distinct(osm_id, .keep_all = TRUE) |> select(-query_state)
n_log(cams, "unique nodes after de-duplicating by OSM id")

# Assign state by point-in-polygon; drop nodes outside the 50 states + DC (Canada, Mexico, sea).
pts <- st_as_sf(cams, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |> st_transform(st_crs(states))
idx <- st_intersects(pts, states)
cams$state <- vapply(idx, \(j) if (length(j)) states$STUSPS[j[1]] else NA_character_, "")
message("  nodes outside US states (dropped): ", sum(is.na(cams$state)))
cams <- cams |> filter(!is.na(state))
n_log(cams, "US ALPR nodes (inside a state polygon)")

write_csv(bind_rows(log_rows), file.path(odir, "query_log.csv"))
write_csv(cams, file.path(RAW, "osm", sprintf("osm_alpr_raw_%s.csv", PULL_DATE)))
saveRDS(cams, file.path(PROC, "osm_alpr.rds"))
writeLines(PULL_DATE, file.path(PROC, "osm_pull_date.txt"))

# Report -------------------------------------------------------------------------------------------
message("\nTotal US ALPR nodes: ", nrow(cams), "   (OSM data as of ", bind_rows(log_rows)$osm_base[1], ")")
mf <- cams |> mutate(manufacturer = coalesce(manufacturer, "(none)")) |> count(manufacturer, sort = TRUE)
print(mf, n = 30)
message(sprintf("Share with non-empty operator tag: %.1f%% (%d of %d)",
                100 * mean(!is.na(cams$operator) & cams$operator != ""),
                sum(!is.na(cams$operator) & cams$operator != ""), nrow(cams)))
write_csv(mf, file.path(OUT, "osm_counts_by_manufacturer.csv"))
