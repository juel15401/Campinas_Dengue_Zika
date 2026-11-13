###############################################################################
#  geomask_cases.R  —  put line-list cases into randomized locations for maps
#
#  Committee action item (2026-09-24): "code for putting line-list data into a
#  randomized polygon". Two tools, both run INSIDE DHS on the confidential
#  extract; only the masked output is meant to leave:
#
#  1. random_point_in_area()  - cases known only by area (county or tract):
#     each case gets a random point inside its area. Uniform by default, or
#     population-weighted (point lands in a populated 2020 census block, so
#     dots don't fall in lakes/forests) with pop_weighted = TRUE.
#     -> dot maps that look like point data but carry only area-level info.
#
#  2. donut_mask()            - cases with a geocoded residence (lat/lon):
#     each point is moved a random distance between r_min and r_max in a
#     random direction, staying inside the same county. r_max adapts to
#     population density so every case is hidden among >= ~k_min residents
#     (k-anonymity), i.e. short moves in Milwaukee, long moves up north.
#     -> report the achieved k with kanon_summary().
#
#  Neither replaces DHS's own release rules; they are the standard technical
#  methods (Hampton et al. 2010 Int J Health Geogr 9:3 for donut masking;
#  Allshouse et al. 2010 for population-weighted displacement) to propose.
#
#  Needs: sf, dplyr (CRAN). Boundaries download once from census.gov into
#  ./cache (tracts 1 MB, counties 12 MB; optional blocks 290 MB).
###############################################################################
suppressPackageStartupMessages({ library(sf); library(dplyr) })
GM <- list(state_fips = "55", cache_dir = "cache", crs_m = 3071)   # 3071 = NAD83(HARN) Wisconsin TM (metres)
dir.create(GM$cache_dir, showWarnings = FALSE)
sf_use_s2(FALSE)

load_areas <- function(level = c("tract", "county", "block")) {
  level <- match.arg(level)
  url <- switch(level,
    tract  = sprintf("https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_%s_tract_500k.zip", GM$state_fips),
    county = "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip",
    block  = sprintf("https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_%s_tabblock20.zip", GM$state_fips))
  z <- file.path(GM$cache_dir, basename(url))
  if (!file.exists(z)) { options(timeout = 1200); download.file(url, z, mode = "wb", quiet = TRUE) }
  d <- file.path(GM$cache_dir, sub("\\.zip$", "", basename(url)))
  if (!dir.exists(d)) unzip(z, exdir = d)
  a <- st_read(list.files(d, "\\.shp$", full.names = TRUE)[1], quiet = TRUE)
  sf_col <- intersect(c("STATEFP", "STATEFP20"), names(a))[1]
  a <- a[a[[sf_col]] == GM$state_fips, ] %>% st_transform(GM$crs_m)
  if (level == "block") a <- a %>% transmute(geoid = GEOID20, tract_geoid = substr(GEOID20, 1, 11),
                                             county_fips = substr(GEOID20, 1, 5), pop = POP20)
  else a <- a %>% transmute(geoid = GEOID, county_fips = substr(GEOID, 1, 5))
  a
}

# cases: data.frame with an id column and an area code column (5-digit county
# FIPS or 11-digit tract GEOID). Returns sf POINTS (WGS84) with id + area code.
random_point_in_area <- function(cases, id_col = "case_id", area_col = "tract_geoid",
                                 level = c("tract", "county"), pop_weighted = FALSE, seed = NULL) {
  level <- match.arg(level)
  if (!is.null(seed)) set.seed(seed)
  codes <- as.character(cases[[area_col]])
  if (pop_weighted) {
    blk <- load_areas("block") %>% filter(pop > 0)
    blk$area <- if (level == "tract") blk$tract_geoid else blk$county_fips
    pts <- lapply(seq_along(codes), function(i) {
      b <- blk[blk$area == codes[i], ]
      if (!nrow(b)) return(st_sfc(st_point(), crs = GM$crs_m))
      st_sample(b[sample.int(nrow(b), 1, prob = b$pop), ], 1, exact = TRUE)
    })
  } else {
    ar <- load_areas(level)
    pts <- lapply(codes, function(cd) {
      p <- ar[ar$geoid == cd, ]
      if (!nrow(p)) return(st_sfc(st_point(), crs = GM$crs_m))
      st_sample(p, 1, exact = TRUE)
    })
  }
  miss <- vapply(pts, function(g) length(g) == 0 || st_is_empty(g[[1]]), logical(1))
  if (any(miss)) warning(sum(miss), " case(s) had an area code not found in the boundary file -> empty point")
  geom <- do.call(c, lapply(pts, function(g) if (length(g)) st_geometry(g) else st_sfc(st_point(), crs = GM$crs_m)))
  st_sf(setNames(data.frame(cases[[id_col]], codes), c(id_col, area_col)), geometry = st_set_crs(geom, GM$crs_m)) %>%
    st_transform(4326)
}

# Donut masking for geocoded points. cases needs id, lon, lat (WGS84).
# k_min / k_max = number of residents inside the inner / outer displacement
# circle (from tract population density), so every masked point is hidden
# among at least ~k_min people; r_floor_m / r_cap_m bound the distances.
donut_mask <- function(cases, id_col = "case_id", lon_col = "lon", lat_col = "lat",
                       k_min = 50, k_max = 250, r_floor_m = 100, r_cap_m = 25000,
                       max_tries = 200, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  tr  <- load_areas("tract")
  cty <- load_areas("county")
  # tract population density (people / m^2) from the PEP-free 2020 centres-of-pop file
  cpf <- file.path(GM$cache_dir, "cenpop2020_tract.csv")
  if (!file.exists(cpf)) download.file(sprintf("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR%s.txt",
                                               GM$state_fips), cpf, mode = "wb", quiet = TRUE)
  cp <- read.csv(cpf, colClasses = "character", check.names = FALSE)
  names(cp)[1] <- "STATEFP"                                   # first header carries a byte-order mark
  cp$geoid <- paste0(cp$STATEFP, cp$COUNTYFP, cp$TRACTCE)
  tr <- tr %>% left_join(data.frame(geoid = cp$geoid, pop = as.numeric(cp$POPULATION)), by = "geoid") %>%
    mutate(dens = pop / as.numeric(st_area(geometry)))
  p <- st_as_sf(cases, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE) %>% st_transform(GM$crs_m)
  j <- st_join(p, tr %>% select(dens), join = st_within)
  j$dens[is.na(j$dens) | j$dens <= 0] <- stats::median(tr$dens[tr$dens > 0], na.rm = TRUE)
  r_min <- pmin(pmax(sqrt(k_min / (pi * j$dens)), r_floor_m), r_cap_m / 2)
  r_max <- pmin(pmax(sqrt(k_max / (pi * j$dens)), 2 * r_min), r_cap_m)
  home_cty <- cty$county_fips[as.integer(st_within(p, cty))]
  xy <- st_coordinates(p); out <- xy; moved <- numeric(nrow(p))
  for (i in seq_len(nrow(p))) {
    for (t in seq_len(max_tries)) {
      d <- sqrt(runif(1, r_min[i]^2, r_max[i]^2))                    # uniform over the annulus area
      a <- runif(1, 0, 2 * pi)
      cand <- xy[i, ] + d * c(cos(a), sin(a))
      ok <- is.na(home_cty[i]) ||
        isTRUE(cty$county_fips[as.integer(st_within(st_sfc(st_point(cand), crs = GM$crs_m), cty))] == home_cty[i])
      if (ok) break
    }
    out[i, ] <- cand; moved[i] <- d
  }
  m <- st_as_sf(data.frame(id = cases[[id_col]], displacement_m = round(moved),
                           r_max_m = round(r_max), est_k = round(j$dens * pi * moved^2),
                           x = out[, 1], y = out[, 2]), coords = c("x", "y"), crs = GM$crs_m) %>% st_transform(4326)
  names(m)[1] <- id_col
  m
}

# Summary of achieved anonymity (aggregate only - safe to report in methods)
kanon_summary <- function(masked) {
  q <- function(v) round(stats::quantile(v, c(0, .1, .5, .9, 1), na.rm = TRUE))
  cat("displacement (m) min/p10/median/p90/max:", q(masked$displacement_m), "\n")
  cat("estimated k      min/p10/median/p90/max:", q(masked$est_k), "\n")
  invisible(NULL)
}

# Write masked points without any original coordinates
write_masked <- function(masked, path = "cases_masked.csv") {
  xy <- st_coordinates(masked)
  df <- st_drop_geometry(masked)
  df$lon_masked <- round(xy[, 1], 5); df$lat_masked <- round(xy[, 2], 5)
  df <- df[, setdiff(names(df), c("est_k", "r_max_m", "displacement_m")), drop = FALSE]
  write.csv(df, path, row.names = FALSE)
  invisible(path)
}

# ---- example (synthetic) ----------------------------------------------------
# fake <- data.frame(case_id = sprintf("FAKE%03d", 1:20),
#                    tract_geoid = sample(load_areas("tract")$geoid, 20))
# pts <- random_point_in_area(fake, level = "tract", seed = 1)
# plot(st_geometry(load_areas("county") |> st_transform(4326))); plot(pts, add = TRUE, col = "red")
