###############################################################################
#  legionella_external_data.R
#
#  Pulls PUBLIC external covariates relevant to Legionnaires' disease onset /
#  exposure risk for Wisconsin, at county and census-tract level, and provides
#  a function that joins them to a confidential case line list that NEVER
#  leaves the machine it is run on.
#
#  Design principle: everything in STEP 1 is public data keyed by geography
#  (county FIPS / tract GEOID) and date. Nothing about cases is sent anywhere.
#  STEP 2 (build_case_covariates) runs entirely locally against the WEDS
#  extract and only ever reads it.
#
#  Sources (all tested 2026-11-03, no login required unless noted):
#    * Census Gazetteer 2023 ........ county/tract centroids, land & water area
#    * Census ACS 5-yr 2023 ......... demographics, race/ethnicity, income,
#                                     housing age, plumbing  (needs FREE key)
#    * CDC/ATSDR SVI 2022 ........... social vulnerability (tract + county);
#                                     also carries ACS-derived % 65+, % poverty,
#                                     % minority, % Hispanic etc. -> works even
#                                     without a Census key
#    * CDC PLACES 2024 release ...... smoking, COPD, diabetes, CKD, cancer,
#                                     CHD, obesity, uninsured, checkups
#    * USDA ERS RUCC 2023 ........... rural-urban continuum (county)
#    * Open-Meteo ERA5 archive ...... daily temp / RH / dew point / precip /
#                                     VPD / wind / solar / ET0 by county
#    * NOAA NCEI Storm Events ....... floods, heavy rain, heat, etc. by county
#    * EPA SDWIS .................... public water systems: source (GW/SW),
#                                     type, population served, county served
#    * EPA FRS (WI state file) ...... facilities by NAICS -> proxies for
#                                     cooling towers / large building water
#                                     systems (hospitals, nursing homes,
#                                     hotels, power plants, paper mills, ...)
#
#    * Census Pop. Estimates (PEP) .. county x year x 5-yr age group, 2020-2025
#                                     (denominators for age-adjusted rates)
#    * Census 2020 centers of pop ... tract population-weighted centroids
#    * CMS Hospital General Info .... WI hospitals: acute vs critical access,
#                                     emergency services (healthcare access)
#    * HRSA HPSA (primary care) ..... shortage-area designation and score
#
#  Dependencies (CRAN): jsonlite, readr, dplyr, tidyr, purrr, stringr
#  No sf / rgdal / GIS stack required. Works on Windows, Mac, Linux.
#
#  VERSION 2 (2026-11-13) - changes from v1 (2026-11-03):
#    * Study window now 2023-01-01 .. 2025-12-31 to match the 635 confirmed
#      2023-2025 cases. v1 started 2024-01-01, so every 2023 case got NO weather.
#    * Weather cache files are now named by date range + counties, so changing
#      the window can never silently re-use a partial chunk from an old run.
#    * Lag summaries are NA (not NaN / -Inf / a fake 0) when weather or storm
#      data do not cover a case's window; storm counts are NA outside coverage.
#    * Robust date parsing (YYYY-MM-DD, M/D/YYYY, Excel serials) and a
#      date fallback (onset -> diagnosis -> specimen -> admit -> report),
#      recorded in `anchor_date` / `anchor_source`.
#    * County may be given as FIPS (55xxx or xxx) OR as a county name.
#    * check_case_input(): duplicate IDs, unmatched counties/tracts, dates
#      outside coverage - run it before trusting any join.
#    * New layers: PEP age-specific population (no key needed), healthcare
#      access (CMS hospitals, HPSA, distance to nearest general hospital).
#    * county_rates(): crude, age-adjusted (US 2000 std) and indirectly
#      standardised (SIR) rates per county - the "population-adjusted map".
#    * make_analysis_file(): strips direct identifiers, turns dates into
#      day-intervals, top-codes age, so a de-identified file can be shared.
#    * MMWR (epi) week instead of %U week.
#
#  USAGE
#    1. Edit the CONFIG block below (study dates, Census key if you have one).
#    2. source("legionella_external_data.R")        # builds ./output/*.csv
#    3. Inside DHS, with your WEDS line list in a data.frame `cases`:
#         check_case_input(cases)                   # aggregate QA report, no PII
#         cov <- build_case_covariates(cases)       # local join, no network
#         share <- make_analysis_file(cov)          # de-identified analysis file
#         rates <- county_rates(cases, age_col = "age")  # map table, aggregate only
#       `cases` needs: case_id, a date (onset_date, or diagnosis/specimen/admit/report), county
#       (FIPS or name); optional: tract_geoid (11-digit, 2020 tracts), age, admit/discharge/death dates.
#
#  Behind a state proxy? Before sourcing:
#    Sys.setenv(https_proxy = "http://proxy.host:port", http_proxy = "...")
###############################################################################

# ============================= CONFIG ========================================
CONFIG <- list(
  state_fips   = "55",
  state_abbr   = "WI",
  state_name   = "WISCONSIN",
  # Study window = earliest to latest onset date you care about. The weather
  # pull adds `lag_buffer_days` BEFORE study_start so lagged exposures exist
  # for the earliest cases.
  # v2: 2023-01-01 (was 2024-01-01) - the cohort is 2023-2025 confirmed cases.
  study_start  = as.Date("2023-01-01"),
  study_end    = as.Date("2025-12-31"),
  lag_buffer_days = 45,
  # Census Population Estimates vintage (cc-est<vintage>-agesex). Falls back
  # to vintage - 1 automatically if the newer file is not published yet.
  pep_vintage  = 2025,
  # Free key from https://api.census.gov/data/key_signup.html (instant).
  # Leave "" to skip the ACS pull; SVI still gives core SES/demographics.
  census_key   = Sys.getenv("CENSUS_API_KEY", unset = ""),
  acs_year     = 2023,
  svi_year     = 2022,
  gaz_year     = 2023,
  out_dir      = "output",
  cache_dir    = "cache",
  # Set FALSE to skip the slowest/largest pulls
  do_weather   = TRUE,
  do_frs       = TRUE,     # ~25 MB zip
  do_storms    = TRUE,
  do_places_tract = TRUE,  # ~61k rows via paged API
  do_pep       = TRUE,     # age-specific county population (no key needed)
  do_access    = TRUE,     # CMS hospitals, HPSA (~50 MB national file), distance
  # Cells below this are flagged/suppressed in county_rates() and in the
  # small-cell report from make_analysis_file(). Confirm DHS's rule (often <5).
  small_cell   = 5
)
# =============================================================================

# ----------------------------- setup -----------------------------------------
req_pkgs <- c("jsonlite", "readr", "dplyr", "tidyr", "purrr", "stringr")
missing  <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(jsonlite); library(readr); library(dplyr); library(tidyr)
  library(purrr);    library(stringr)
})
options(timeout = 600, readr.show_col_types = FALSE, readr.show_progress = FALSE)
dir.create(CONFIG$out_dir,   showWarnings = FALSE, recursive = TRUE)
dir.create(CONFIG$cache_dir, showWarnings = FALSE, recursive = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a
log_msg <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

# NA-safe summaries: return NA when every value is missing instead of
# NaN (mean), -Inf (max) or a misleading 0 (sum). v1 produced those silently.
safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
safe_sum  <- function(x) if (all(is.na(x))) NA_real_ else sum(x,  na.rm = TRUE)
safe_max  <- function(x) if (all(is.na(x))) NA_real_ else max(x,  na.rm = TRUE)

# Parse the date formats that come out of WEDSS / Excel / SAS exports.
parse_date_any <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x))         return(as.Date(x, origin = "1899-12-30"))  # Excel serial
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", ".", "Unknown", "UNK")] <- NA
  x <- sub("[ T].*$", "", x)                                   # drop any time part
  out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  try_fmt <- function(out, fmt) {
    i <- is.na(out) & !is.na(x)
    if (any(i)) out[i] <- suppressWarnings(as.Date(x[i], format = fmt))
    out[!is.na(out) & as.integer(format(out, "%Y")) < 1900] <- NA  # "3/5/24" read as year 24
    out
  }
  out <- try_fmt(out, "%m/%d/%Y")
  out <- try_fmt(out, "%m/%d/%y")
  out <- try_fmt(out, "%d%b%Y")                                 # SAS DATE9. e.g. 05MAR2024
  i <- is.na(out) & !is.na(x) & grepl("^[0-9]{5}$", x)
  if (any(i)) out[i] <- as.Date(as.numeric(x[i]), origin = "1899-12-30")
  bad <- sum(is.na(out) & !is.na(x))
  if (bad) warning(bad, " date value(s) could not be parsed, e.g. '",
                   x[is.na(out) & !is.na(x)][1], "' -> set to NA")
  out
}

# County as FIPS ("55025", 55025, "025", 25) or name ("Dane", "Dane County",
# "ST. CROIX") -> 5-character FIPS. Needs the gazetteer county table.
standardize_county <- function(x, geo_county = NULL) {
  x <- trimws(as.character(x))
  out <- rep(NA_character_, length(x))
  num <- grepl("^[0-9]+$", x)
  out[num & nchar(x) <= 3] <- paste0(CONFIG$state_fips, str_pad(x[num & nchar(x) <= 3], 3, pad = "0"))
  out[num & nchar(x) %in% 4:5] <- str_pad(x[num & nchar(x) %in% 4:5], 5, pad = "0")
  if (any(!num & !is.na(x))) {
    if (is.null(geo_county)) geo_county <- read_csv(file.path(CONFIG$out_dir, "geo_county.csv"),
                                                    col_types = cols(.default = "c"))
    key <- function(s) gsub("[^a-z]", "", sub(" county$", "", tolower(s)))
    lk  <- setNames(geo_county$county_fips, key(geo_county$county_name))
    out[!num] <- unname(lk[key(x[!num])])
  }
  out
}

# MMWR / epi week (Sunday start; week 1 contains the first Wednesday... i.e.
# the first week with >= 4 days in the new year). Returns year and week.
mmwr_week <- function(d) {
  d <- as.Date(d)
  wk_start <- d - as.integer(format(d, "%w"))                   # Sunday of that week
  yr <- as.integer(format(wk_start + 3, "%Y"))                  # Wednesday decides the year
  jan1 <- as.Date(paste0(yr, "-01-01"))
  first_sun <- jan1 - as.integer(format(jan1, "%w"))
  first_sun[as.integer(format(jan1, "%w")) > 3] <- first_sun[as.integer(format(jan1, "%w")) > 3] + 7
  data.frame(mmwr_year = yr, mmwr_week = as.integer((wk_start - first_sun) / 7) + 1L)
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  12742 * asin(pmin(1, sqrt(a)))
}

# Download with caching; returns local path. Retries once.
dl <- function(url, dest = NULL, force = FALSE) {
  dest <- dest %||% file.path(CONFIG$cache_dir, basename(sub("\\?.*$", "", url)))
  if (!force && file.exists(dest) && file.size(dest) > 0) return(dest)
  ok <- tryCatch({
    download.file(url, dest, mode = "wb", quiet = TRUE,
                  headers = c("User-Agent" = "WI-DHS-legionella-research-R"))
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) {
    Sys.sleep(3)
    download.file(url, dest, mode = "wb", quiet = TRUE,
                  headers = c("User-Agent" = "WI-DHS-legionella-research-R"))
  }
  dest
}

get_json <- function(url) {
  jsonlite::fromJSON(url, simplifyVector = TRUE)
}

# Socrata (data.cdc.gov) paged pull -> data.frame
socrata_all <- function(base, where, select = NULL, page = 50000) {
  out <- list(); off <- 0
  repeat {
    q <- paste0(base, "?", where, "&$limit=", page, "&$offset=", off,
                if (!is.null(select)) paste0("&$select=", select) else "")
    chunk <- get_json(URLencode(q))
    if (length(chunk) == 0 || nrow(chunk) == 0) break
    out[[length(out) + 1]] <- chunk
    if (nrow(chunk) < page) break
    off <- off + page
  }
  bind_rows(out)
}

save_out <- function(df, name) {
  p <- file.path(CONFIG$out_dir, paste0(name, ".csv"))
  write_csv(df, p)
  log_msg("wrote ", p, "  (", nrow(df), " rows x ", ncol(df), " cols)")
  invisible(df)
}

###############################################################################
#  STEP 1 — PUBLIC EXTERNAL DATA  (keyed by geography and/or date)
###############################################################################

# ---- 1a. Geography: Census Gazetteer (centroids, land/water area) -----------
pull_gazetteer <- function() {
  log_msg("Gazetteer: counties")
  yr <- CONFIG$gaz_year
  cz <- dl(sprintf("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/%s_Gazetteer/%s_Gaz_counties_national.zip", yr, yr))
  cty <- read_tsv(cz, col_types = cols(.default = "c")) %>%
    rename_with(str_trim) %>%
    filter(USPS == CONFIG$state_abbr) %>%
    transmute(county_fips = GEOID, county_name = NAME,
              aland_km2 = as.numeric(ALAND) / 1e6, awater_km2 = as.numeric(AWATER) / 1e6,
              water_frac = awater_km2 / (aland_km2 + awater_km2),
              lat = as.numeric(INTPTLAT), lon = as.numeric(INTPTLONG))
  log_msg("Gazetteer: tracts")
  tz <- dl(sprintf("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/%s_Gazetteer/%s_Gaz_tracts_national.zip", yr, yr))
  tr <- read_tsv(tz, col_types = cols(.default = "c")) %>%
    rename_with(str_trim) %>%
    filter(USPS == CONFIG$state_abbr) %>%
    transmute(tract_geoid = GEOID, county_fips = substr(GEOID, 1, 5),
              aland_km2 = as.numeric(ALAND) / 1e6, awater_km2 = as.numeric(AWATER) / 1e6,
              water_frac = awater_km2 / (aland_km2 + awater_km2),
              lat = as.numeric(INTPTLAT), lon = as.numeric(INTPTLONG))
  list(county = save_out(cty, "geo_county"), tract = save_out(tr, "geo_tract"))
}

# ---- 1b. ACS 5-year (requires free key) -------------------------------------
ACS_VARS <- c(
  pop_total          = "B01003_001E",
  median_age         = "B01002_001E",
  pop_65plus_m       = "B01001_020E", # males 65-66 ... summed below
  median_hh_income   = "B19013_001E",
  per_capita_income  = "B19301_001E",
  pov_universe       = "B17001_001E",
  pov_below          = "B17001_002E",
  race_universe      = "B03002_001E",
  race_nh_white      = "B03002_003E",
  race_nh_black      = "B03002_004E",
  race_nh_aian       = "B03002_005E",
  race_nh_asian      = "B03002_006E",
  race_hispanic      = "B03002_012E",
  hu_total           = "B25001_001E",
  hu_occupied        = "B25002_002E",
  hu_vacant          = "B25002_003E",
  hu_owner           = "B25003_002E",
  hu_renter          = "B25003_003E",
  hu_median_yr_built = "B25035_001E",
  hu_built_pre1940   = "B25034_011E",
  hu_built_1940_59   = "B25034_010E",  # 1940-49 + 1950-59 handled below
  hu_built_1950_59   = "B25034_009E",
  hu_built_1960_69   = "B25034_008E",
  hu_built_1970_79   = "B25034_007E",
  hu_plumb_universe  = "B25047_001E",
  hu_lack_plumbing   = "B25047_003E",
  hu_units_universe  = "B25024_001E",
  hu_units_20plus    = "B25024_008E",  # 20-49 units
  hu_units_50plus    = "B25024_009E",
  gq_total           = "B26001_001E",
  edu_universe       = "B15003_001E",
  edu_bachelors      = "B15003_022E",
  edu_masters        = "B15003_023E",
  edu_prof           = "B15003_024E",
  edu_doctorate      = "B15003_025E",
  ins_universe       = "B27010_001E",
  workers_universe   = "B08301_001E"
)
# Age 65+ = sum of B01001 male 020-025 and female 044-049
AGE65_VARS <- c(sprintf("B01001_%03dE", 20:25), sprintf("B01001_%03dE", 44:49))

acs_fetch <- function(geo_for, geo_in) {
  vars <- unique(c(unname(ACS_VARS), AGE65_VARS))
  # API caps at 50 vars per call
  chunks <- split(vars, ceiling(seq_along(vars) / 45))
  res <- NULL
  for (v in chunks) {
    url <- sprintf("https://api.census.gov/data/%d/acs/acs5?get=NAME,%s&for=%s&in=%s&key=%s",
                   CONFIG$acs_year, paste(v, collapse = ","), geo_for, geo_in, CONFIG$census_key)
    m <- get_json(URLencode(url))
    df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE)
    names(df) <- m[1, ]
    res <- if (is.null(res)) df else left_join(res, df %>% select(-NAME), by = intersect(c("state","county","tract"), names(df)))
  }
  res
}

acs_tidy <- function(df) {
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[x < -1e8] <- NA; x }
  d <- df %>% mutate(across(all_of(unique(c(unname(ACS_VARS), AGE65_VARS))), num))
  d$pop_65plus <- rowSums(d[AGE65_VARS], na.rm = TRUE)
  rn <- setNames(names(ACS_VARS), ACS_VARS)
  d <- d %>% rename(any_of(rn)) %>% select(-all_of(setdiff(AGE65_VARS, names(ACS_VARS))))
  d %>% mutate(
    pct_65plus      = 100 * pop_65plus / pop_total,
    pct_poverty     = 100 * pov_below / pov_universe,
    pct_nh_white    = 100 * race_nh_white / race_universe,
    pct_nh_black    = 100 * race_nh_black / race_universe,
    pct_nh_aian     = 100 * race_nh_aian / race_universe,
    pct_nh_asian    = 100 * race_nh_asian / race_universe,
    pct_hispanic    = 100 * race_hispanic / race_universe,
    pct_renter      = 100 * hu_renter / (hu_owner + hu_renter),
    pct_vacant      = 100 * hu_vacant / hu_total,
    pct_built_pre1960 = 100 * (hu_built_pre1940 + hu_built_1940_59 + hu_built_1950_59) / hu_total,
    pct_built_pre1980 = 100 * (hu_built_pre1940 + hu_built_1940_59 + hu_built_1950_59 +
                                 hu_built_1960_69 + hu_built_1970_79) / hu_total,
    pct_lack_plumbing = 100 * hu_lack_plumbing / hu_plumb_universe,
    pct_units_20plus  = 100 * (hu_units_20plus + hu_units_50plus) / hu_units_universe,
    pct_bachelors_plus = 100 * (edu_bachelors + edu_masters + edu_prof + edu_doctorate) / edu_universe,
    pct_group_quarters = 100 * gq_total / pop_total
  )
}

pull_acs <- function() {
  if (!nzchar(CONFIG$census_key)) {
    log_msg("ACS: no CENSUS_API_KEY set -> skipping (SVI covers core SES vars). ",
            "Get a free key at https://api.census.gov/data/key_signup.html")
    return(NULL)
  }
  log_msg("ACS ", CONFIG$acs_year, ": counties")
  cty <- acs_fetch("county:*", paste0("state:", CONFIG$state_fips)) %>%
    acs_tidy() %>% mutate(county_fips = paste0(state, county)) %>%
    select(county_fips, NAME, everything(), -state, -county)
  log_msg("ACS ", CONFIG$acs_year, ": tracts")
  tr <- acs_fetch("tract:*", paste0("state:", CONFIG$state_fips)) %>%
    acs_tidy() %>% mutate(tract_geoid = paste0(state, county, tract), county_fips = paste0(state, county)) %>%
    select(tract_geoid, county_fips, NAME, everything(), -state, -county, -tract)
  list(county = save_out(cty, "acs_county"), tract = save_out(tr, "acs_tract"))
}

# ---- 1c. CDC/ATSDR Social Vulnerability Index --------------------------------
pull_svi <- function() {
  yr <- CONFIG$svi_year; st <- str_to_title(tolower(CONFIG$state_name))
  log_msg("SVI ", yr, ": tracts + counties")
  tr  <- read_csv(dl(sprintf("https://svi.cdc.gov/Documents/Data/%d/csv/states/%s.csv", yr, st),
                     file.path(CONFIG$cache_dir, sprintf("svi_%d_tract.csv", yr))),
                  col_types = cols(.default = "c"))
  cty <- read_csv(dl(sprintf("https://svi.cdc.gov/Documents/Data/%d/csv/states_counties/%s_county.csv", yr, st),
                     file.path(CONFIG$cache_dir, sprintf("svi_%d_county.csv", yr))),
                  col_types = cols(.default = "c"))
  keep <- c("E_TOTPOP","E_HU","E_HH","EP_POV150","EP_UNEMP","EP_HBURD","EP_NOHSDP","EP_UNINSUR",
            "EP_AGE65","EP_AGE17","EP_DISABL","EP_SNGPNT","EP_LIMENG","EP_MINRTY","EP_MUNIT",
            "EP_MOBILE","EP_CROWD","EP_NOVEH","EP_GROUPQ","EP_NOINT","EP_AFAM","EP_HISP","EP_ASIAN",
            "EP_AIAN","EP_NHPI","EP_TWOMORE","EP_OTHERRACE",
            "RPL_THEME1","RPL_THEME2","RPL_THEME3","RPL_THEME4","RPL_THEMES","E_DAYPOP","AREA_SQMI")
  tidy <- function(d, idcol, idname) {
    names(d) <- sub("^﻿", "", names(d))
    d %>% select(all_of(c(idcol, intersect(keep, names(d))))) %>%
      rename(!!idname := all_of(idcol)) %>%
      mutate(across(-all_of(idname), ~ { x <- as.numeric(.x); x[x == -999] <- NA; x })) %>%
      rename_with(~ paste0("svi_", tolower(.x)), -all_of(idname))
  }
  tr_t  <- tidy(tr,  "FIPS",   "tract_geoid") %>% mutate(county_fips = substr(tract_geoid, 1, 5), .after = 1)
  cty_t <- tidy(cty, "FIPS",   "county_fips")
  list(county = save_out(cty_t, "svi_county"), tract = save_out(tr_t, "svi_tract"))
}

# ---- 1d. CDC PLACES (behavioral risk + chronic disease prevalence) ----------
PLACES_MEASURES <- c("CSMOKING","COPD","DIABETES","KIDNEY","CANCER","CHD","OBESITY","BINGE",
                     "CHECKUP","ACCESS2","BPHIGH","DEPRESSION","GHLTH","PHLTH","STROKE",
                     "CASTHMA","DISABILITY","MHLTH","ISOLATION","HOUSINSECU","FOODINSECU")
pull_places <- function() {
  wh <- sprintf("stateabbr=%s&data_value_type=Crude prevalence", CONFIG$state_abbr)
  sel <- "year,locationid,measureid,data_value,low_confidence_limit,high_confidence_limit,totalpopulation"
  wide <- function(d, idname) {
    d %>% filter(measureid %in% PLACES_MEASURES) %>%
      mutate(data_value = as.numeric(data_value)) %>%
      select(locationid, year, measureid, data_value) %>%
      distinct(locationid, measureid, .keep_all = TRUE) %>%
      pivot_wider(names_from = measureid, values_from = data_value, names_prefix = "places_") %>%
      rename(!!idname := locationid, places_year = year) %>%
      rename_with(tolower)
  }
  log_msg("PLACES: counties")
  cty <- socrata_all("https://data.cdc.gov/resource/swc5-untb.json", wh, sel) %>% wide("county_fips")
  res <- list(county = save_out(cty, "places_county"))
  if (CONFIG$do_places_tract) {
    log_msg("PLACES: tracts (~61k rows, paged)")
    tr <- socrata_all("https://data.cdc.gov/resource/cwsq-ngmh.json", wh, sel) %>% wide("tract_geoid") %>%
      mutate(county_fips = substr(tract_geoid, 1, 5), .after = 1)
    res$tract <- save_out(tr, "places_tract")
  }
  res
}

# ---- 1e. USDA ERS Rural-Urban Continuum Codes ------------------------------
pull_rucc <- function() {
  log_msg("RUCC 2023")
  p <- dl("https://ers.usda.gov/sites/default/files/_laserfiche/DataFiles/53251/Ruralurbancontinuumcodes2023.csv",
          file.path(CONFIG$cache_dir, "rucc2023.csv"))
  d <- read_csv(p, col_types = cols(.default = "c"))
  names(d) <- tolower(names(d))
  d <- d %>% filter(state == CONFIG$state_abbr)
  # file is long: Attribute / Value
  if ("attribute" %in% names(d)) {
    d <- d %>% select(fips, attribute, value) %>%
      pivot_wider(names_from = attribute, values_from = value) %>%
      rename_with(tolower)
  }
  d <- d %>% rename(county_fips = fips) %>%
    mutate(across(any_of(c("rucc_2023", "population_2020")), as.numeric)) %>%
    mutate(rucc_metro = if_else(rucc_2023 <= 3, 1L, 0L))
  save_out(d, "rucc_county")
}

# ---- 1f. Daily weather by county centroid (Open-Meteo ERA5-Land archive) ----
WEATHER_VARS <- c("temperature_2m_mean","temperature_2m_max","temperature_2m_min",
                  "relative_humidity_2m_mean","relative_humidity_2m_max","dew_point_2m_mean",
                  "precipitation_sum","rain_sum","precipitation_hours",
                  "vapour_pressure_deficit_max","wind_speed_10m_max","wind_direction_10m_dominant",
                  "shortwave_radiation_sum","et0_fao_evapotranspiration")
pull_weather <- function(geo_county) {
  start <- CONFIG$study_start - CONFIG$lag_buffer_days
  end   <- min(CONFIG$study_end, Sys.Date() - 7)   # archive lags ~5 days
  log_msg("Weather: ", nrow(geo_county), " counties, ", start, " to ", end,
          "  (~1 min per 6 counties; chunks are cached so reruns resume)")
  # Open-Meteo's free tier rate-limits by (days x variables x locations), so
  # pull 6 counties x 1 calendar year per call, cache each raw JSON chunk,
  # and back off on HTTP 429. Each chunk is cached, so reruns resume.
  chunks <- split(geo_county, ceiling(seq_len(nrow(geo_county)) / 6))
  years  <- seq(as.integer(format(start, "%Y")), as.integer(format(end, "%Y")))
  fetch_chunk <- function(g, k, y) {
    s <- max(start, as.Date(sprintf("%d-01-01", y))); e <- min(end, as.Date(sprintf("%d-12-31", y)))
    url <- sprintf("https://archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s&start_date=%s&end_date=%s&daily=%s&timezone=America%%2FChicago",
                   paste(g$lat, collapse = ","), paste(g$lon, collapse = ","),
                   s, e, paste(WEATHER_VARS, collapse = ","))
    # v2: name includes exact date range + first/last county, so a cache file
    # from a run with a different window can never be re-used by mistake.
    f <- file.path(CONFIG$cache_dir, sprintf("wx_%s_%s_%s-%s.json", s, e,
                                             g$county_fips[1], g$county_fips[nrow(g)]))
    # Open-Meteo free tier has minutely, hourly and daily quotas and answers
    # HTTP 429 for all three. Escalate: 1 min -> next hour -> next hour -> give up.
    n429 <- 0
    for (attempt in 1:8) {
      reason <- ""
      ok <- tryCatch({
        if (!file.exists(f) || file.size(f) == 0) dl(url, f)
        js <- jsonlite::fromJSON(f, simplifyVector = FALSE)
        if (isTRUE(js$error)) stop(js$reason %||% "API error")
        TRUE
      }, error = function(err) { reason <<- conditionMessage(err); FALSE },
         warning = function(w)  { reason <<- conditionMessage(w);   FALSE })
      if (ok) break
      unlink(f)
      # download.file() does not expose the HTTP status; a 429 surfaces as
      # "downloaded length 0 != reported length 95" (the tiny JSON error body).
      # Treat every failure as a probable rate limit and escalate.
      n429 <- n429 + 1
      if (n429 == 1) {
        wait <- 61                                   # minutely quota
      } else if (n429 <= 3) {                        # hourly quota resets on the hour
        now  <- Sys.time()
        wait <- as.numeric(difftime(trunc(now, "hours") + 3600 + 45, now, units = "secs"))
      } else {
        stop("Open-Meteo still failing after waiting two hours - probably the DAILY quota ",
             "(or no network). Chunks already fetched are kept in ./cache; re-run tomorrow (or set ",
             "do_weather = FALSE for now) and the weather pull will resume where it stopped. ",
             "Last error: ", reason)
      }
      log_msg("  open-meteo fetch failed (", substr(reason, 1, 60), ") chunk ", k, "/", y,
              " - waiting ", round(wait / 60, 1), " min then retrying")
      Sys.sleep(wait)
    }
    if (!ok) stop("Open-Meteo failed after retries for chunk ", k, " year ", y, ": ", reason)
    if (!is.null(js$daily)) js <- list(js)      # single-location shape
    map2_dfr(js, seq_along(js), function(loc, i) {   # results come back in request order
      d <- loc$daily
      d <- as.data.frame(lapply(d, function(v) unlist(lapply(v, function(x) if (is.null(x)) NA else x))),
                         stringsAsFactors = FALSE)
      d$county_fips <- g$county_fips[i]
      d
    })
  }
  out <- map_dfr(years, function(y) imap_dfr(chunks, function(g, k) { r <- fetch_chunk(g, k, y); Sys.sleep(2); r }))
  out <- out %>% rename(date = time) %>% mutate(date = as.Date(date)) %>%
    rename_with(~ paste0("wx_", .x), -c(county_fips, date)) %>%
    select(county_fips, date, everything())
  rng <- range(out$date)
  if (rng[1] > start || rng[2] < end)
    warning("Weather covers ", rng[1], " to ", rng[2], " but ", start, " to ", end, " was requested")
  save_out(out, "weather_county_daily")
}

# ---- 1g. NOAA Storm Events (floods, heavy rain, heat, etc.) -----------------
pull_storms <- function() {
  yrs <- seq(as.integer(format(CONFIG$study_start - CONFIG$lag_buffer_days, "%Y")),
             as.integer(format(CONFIG$study_end, "%Y")))
  idx <- "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
  listing <- paste(readLines(idx, warn = FALSE), collapse = "\n")
  out <- map(yrs, function(y) {
    fn <- str_extract_all(listing, sprintf("StormEvents_details-ftp_v1.0_d%d_c\\d+\\.csv\\.gz", y))[[1]]
    if (!length(fn)) { log_msg("Storm events: no file for ", y); return(NULL) }
    fn <- sort(fn, decreasing = TRUE)[1]
    log_msg("Storm events: ", fn)
    read_csv(dl(paste0(idx, fn)), col_types = cols(.default = "c")) %>%
      filter(STATE == CONFIG$state_name)
  }) %>% bind_rows()
  out <- out %>%
    transmute(event_id = EVENT_ID, episode_id = EPISODE_ID, event_type = EVENT_TYPE,
              cz_type = CZ_TYPE,  # C = county, Z = NWS forecast zone
              county_fips = if_else(CZ_TYPE == "C", paste0(CONFIG$state_fips, str_pad(CZ_FIPS, 3, pad = "0")), NA_character_),
              cz_name = CZ_NAME,
              begin_date = as.Date(BEGIN_DATE_TIME, format = "%d-%b-%y %H:%M:%S"),
              end_date   = as.Date(END_DATE_TIME,   format = "%d-%b-%y %H:%M:%S"),
              magnitude = as.numeric(MAGNITUDE), magnitude_type = MAGNITUDE_TYPE,
              flood_cause = FLOOD_CAUSE, damage_property = DAMAGE_PROPERTY,
              begin_lat = as.numeric(BEGIN_LAT), begin_lon = as.numeric(BEGIN_LON),
              narrative = EVENT_NARRATIVE)
  # record which period the files cover, so cases outside it get NA, not 0
  got <- yrs[yrs %in% as.integer(format(out$begin_date, "%Y"))]
  write_csv(tibble(cover_start = as.Date(sprintf("%d-01-01", min(got))),
                   cover_end   = as.Date(sprintf("%d-12-31", max(got)))),
            file.path(CONFIG$out_dir, "storm_coverage.csv"))
  save_out(out, "storm_events_wi")
}

# ---- 1h. EPA SDWIS public water systems ------------------------------------
pull_sdwis <- function(geo_county) {
  log_msg("SDWIS: water systems (WI primacy)")
  base <- "https://data.epa.gov/efservice"
  page <- function(tbl, filt, n = 10000) {
    out <- list(); i <- 0
    repeat {
      u <- sprintf("%s/%s/%s/ROWS/%d:%d/JSON", base, tbl, filt, i, i + n - 1)
      d <- tryCatch(get_json(u), error = function(e) NULL)
      if (is.null(d) || !length(d) || !nrow(d)) break
      out[[length(out) + 1]] <- d
      if (nrow(d) < n) break
      i <- i + n
    }
    bind_rows(out)
  }
  ws <- page("WATER_SYSTEM", sprintf("PRIMACY_AGENCY_CODE/=/%s", CONFIG$state_abbr))
  ws <- ws %>% select(any_of(c("pwsid","pws_name","pws_activity_code","pws_type_code","gw_sw_code",
                               "primary_source_code","owner_type_code","population_served_count",
                               "service_connections_count","is_school_or_daycare_ind","is_wholesaler_ind",
                               "pws_deactivation_date","city_name","zip_code")))
  log_msg("SDWIS: geographic areas served")
  ga <- page("GEOGRAPHIC_AREA", sprintf("PRIMACY_AGENCY_CODE/=/%s", CONFIG$state_abbr)) %>%
    select(any_of(c("pwsid","area_type_code","county_served","city_served","zip_code_served")))
  cnty_lookup <- geo_county %>% transmute(county_fips, key = tolower(str_remove(county_name, " County$")))
  ga <- ga %>% filter(!is.na(county_served)) %>%
    mutate(key = tolower(str_trim(county_served))) %>%
    left_join(cnty_lookup, by = "key") %>% select(pwsid, county_fips) %>% distinct()
  ws <- ws %>% left_join(ga, by = "pwsid", relationship = "many-to-many")
  ws <- ws %>% mutate(population_served_count = as.numeric(population_served_count),
                      service_connections_count = as.numeric(service_connections_count))
  save_out(ws, "sdwis_systems_wi")
  # county aggregation (active systems only)
  agg <- ws %>% filter(pws_activity_code == "A", !is.na(county_fips)) %>%
    group_by(county_fips) %>%
    summarise(
      pws_n_active        = n_distinct(pwsid),
      pws_n_cws           = n_distinct(pwsid[pws_type_code == "CWS"]),
      pws_n_ntncws        = n_distinct(pwsid[pws_type_code == "NTNCWS"]),
      pws_n_tncws         = n_distinct(pwsid[pws_type_code == "TNCWS"]),
      pws_pop_served_cws  = sum(population_served_count[pws_type_code == "CWS"], na.rm = TRUE),
      pws_pop_served_sw   = sum(population_served_count[pws_type_code == "CWS" & gw_sw_code == "SW"], na.rm = TRUE),
      pws_pct_cws_pop_surfacewater = 100 * pws_pop_served_sw / pmax(pws_pop_served_cws, 1),
      pws_n_cws_surfacewater = n_distinct(pwsid[pws_type_code == "CWS" & gw_sw_code == "SW"]),
      pws_n_purchased     = n_distinct(pwsid[primary_source_code %in% c("GWP","SWP")]),
      .groups = "drop")
  # Share of county pop NOT on a CWS ~ private well reliance (needs pop; filled in merge step)
  save_out(agg, "sdwis_county")
}

# ---- 1i. EPA FRS facilities -> cooling-tower / building-water-system proxies -
# NAICS prefixes chosen for buildings that commonly run cooling towers,
# evaporative condensers, large hot-water recirculating systems, or spas.
FRS_NAICS <- tribble(
  ~grp,                 ~prefix,
  "hospital",           "622",
  "nursing_resid_care", "623",
  "hotel_lodging",      "7211",
  "rv_camp",            "7212",
  "power_generation",   "2211",
  "paper_mill",         "3221",
  "chemical_mfg",       "325",
  "food_mfg",           "311",
  "beverage_mfg",       "3121",
  "metal_foundry",      "3315",
  "plastics_rubber",    "326",
  "data_center",        "518210",
  "college_univ",       "6113",
  "correctional",       "92214",
  "wastewater_tx",      "22132",
  "water_supply",       "22131",
  "fitness_rec",        "71394",
  "car_wash",           "811192",
  "grocery_misting",    "4451"
)
pull_frs <- function() {
  log_msg("FRS: WI state file (~25 MB)")
  z <- dl(sprintf("https://ordsext.epa.gov/FLA/www3/state_files/state_combined_%s.zip", tolower(CONFIG$state_abbr)),
          file.path(CONFIG$cache_dir, "frs_wi.zip"))
  fac_f  <- sprintf("%s_FACILITY_FILE.CSV", CONFIG$state_abbr)
  naic_f <- sprintf("%s_NAICS_FILE.CSV", CONFIG$state_abbr)
  unzip(z, files = c(fac_f, naic_f), exdir = CONFIG$cache_dir, overwrite = TRUE)
  fac <- read_csv(file.path(CONFIG$cache_dir, fac_f), col_types = cols(.default = "c")) %>%
    select(REGISTRY_ID, PRIMARY_NAME, CITY_NAME, COUNTY_NAME, FIPS_CODE, POSTAL_CODE,
           CENSUS_BLOCK_CODE, SITE_TYPE_NAME, LATITUDE83, LONGITUDE83, PGM_SYS_ACRNMS)
  nai <- read_csv(file.path(CONFIG$cache_dir, naic_f), col_types = cols(.default = "c")) %>%
    select(REGISTRY_ID, NAICS_CODE, CODE_DESCRIPTION) %>% distinct()
  # classify
  nai$grp <- NA_character_
  for (i in seq_len(nrow(FRS_NAICS))) {
    hit <- is.na(nai$grp) & startsWith(nai$NAICS_CODE, FRS_NAICS$prefix[i])
    nai$grp[hit] <- FRS_NAICS$grp[i]
  }
  nai <- nai %>% filter(!is.na(grp)) %>% distinct(REGISTRY_ID, grp, .keep_all = TRUE)
  fac <- fac %>% inner_join(nai, by = "REGISTRY_ID") %>%
    transmute(registry_id = REGISTRY_ID, name = PRIMARY_NAME, city = CITY_NAME,
              county_fips = if_else(nchar(FIPS_CODE) == 5 & startsWith(FIPS_CODE, CONFIG$state_fips), FIPS_CODE, NA_character_),
              tract_geoid = if_else(nchar(CENSUS_BLOCK_CODE) >= 11 & startsWith(CENSUS_BLOCK_CODE, CONFIG$state_fips),
                                    substr(CENSUS_BLOCK_CODE, 1, 11), NA_character_),
              zip = substr(POSTAL_CODE, 1, 5), naics = NAICS_CODE, naics_desc = CODE_DESCRIPTION,
              facility_group = grp, lat = as.numeric(LATITUDE83), lon = as.numeric(LONGITUDE83),
              programs = PGM_SYS_ACRNMS)
  save_out(fac, "frs_facilities_wi")
  wide <- function(d, id) d %>% filter(!is.na(.data[[id]])) %>%
    count(.data[[id]], facility_group) %>%
    pivot_wider(names_from = facility_group, values_from = n, values_fill = 0, names_prefix = "frs_n_") %>%
    mutate(frs_n_any = rowSums(across(starts_with("frs_n_"))))
  list(facilities = fac,
       county = save_out(wide(fac, "county_fips"), "frs_county"),
       tract  = save_out(wide(fac, "tract_geoid"), "frs_tract"))
}

# ---- 1k. Census Population Estimates: county x year x age group (no key) ----
# Denominators for crude and age-adjusted rates and for the SIR map. LD
# incidence rises steeply with age, so an unadjusted county map mostly shows
# where older people live.
PEP_AGE_GROUPS <- c("AGE04","AGE59","AGE1014","AGE1519","AGE2024","AGE2529","AGE3034",
                    "AGE3539","AGE4044","AGE4549","AGE5054","AGE5559","AGE6064","AGE6569",
                    "AGE7074","AGE7579","AGE8084","AGE85PLUS")
PEP_AGE_LO <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
# US 2000 standard population (NCHS Statistical Note 20), per million, in the
# same 18 groups (<1 and 1-4 combined into 0-4).
US2000_STD <- c(69135, 72533, 73032, 72169, 66478, 64529, 71044, 80762, 81851, 72118,
                62716, 48454, 38793, 34264, 31773, 26999, 17842, 15508)
pull_pep <- function() {
  for (v in c(CONFIG$pep_vintage, CONFIG$pep_vintage - 1)) {
    url <- sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%d/counties/asrh/cc-est%d-agesex-%s.csv",
                   v, v, CONFIG$state_fips)
    p <- tryCatch(dl(url, file.path(CONFIG$cache_dir, sprintf("pep_v%d.csv", v))), error = function(e) NULL)
    if (!is.null(p) && file.size(p) > 1000) break
    log_msg("PEP vintage ", v, " not available, trying ", v - 1)
  }
  log_msg("PEP vintage ", v, ": county x year x age")
  d <- read_csv(p, col_types = cols(.default = "c"), locale = locale(encoding = "latin1"))
  # YEAR code 1 = 4/1/2020 census base; 2 = 7/1/2020; 3 = 2021 ... -> calendar year
  d <- d %>% mutate(year_code = as.integer(YEAR)) %>% filter(year_code >= 2) %>%
    transmute(county_fips = paste0(STATE, COUNTY), year = 2018L + year_code,
              pop_total = as.numeric(POPESTIMATE),
              across(all_of(paste0(PEP_AGE_GROUPS, "_TOT")), as.numeric),
              pop_65plus = as.numeric(AGE65PLUS_TOT))
  long <- d %>% select(county_fips, year, all_of(paste0(PEP_AGE_GROUPS, "_TOT"))) %>%
    pivot_longer(-c(county_fips, year), names_to = "age_group", values_to = "pop") %>%
    mutate(age_group = sub("_TOT$", "", age_group),
           age_lo = PEP_AGE_LO[match(age_group, PEP_AGE_GROUPS)])
  save_out(long, "pep_county_year_age")
  wide <- d %>% filter(year >= as.integer(format(CONFIG$study_start, "%Y")),
                       year <= as.integer(format(CONFIG$study_end, "%Y"))) %>%
    group_by(county_fips) %>%
    summarise(pep_pop_mean = mean(pop_total), pep_pct_65plus = 100 * sum(pop_65plus) / sum(pop_total),
              pep_pct_75plus = 100 * sum(AGE7579_TOT + AGE8084_TOT + AGE85PLUS_TOT) / sum(pop_total),
              pep_person_years = sum(pop_total), .groups = "drop")
  list(long = long, county = save_out(wide, "pep_county"))
}

# ---- 1l. Healthcare access: CMS hospitals, HRSA HPSA, distance to hospital --
# Severity (death, length of stay) depends on where people can be treated:
# critical-access vs larger acute-care hospitals, primary-care shortage
# (later diagnosis), and travel distance.
pull_access <- function(geo, frs) {
  key <- function(s) gsub("[^a-z]", "", sub(" county$", "", tolower(s)))
  lk  <- setNames(geo$county$county_fips, key(geo$county$county_name))
  # CMS Hospital General Information (one row per Medicare-certified hospital)
  log_msg("CMS hospitals (", CONFIG$state_abbr, ")")
  u <- sprintf(paste0("https://data.cms.gov/provider-data/api/1/datastore/query/xubh-q36u/0?",
                      "conditions[0][property]=state&conditions[0][value]=%s&conditions[0][operator]=%%3D&limit=1000"),
               CONFIG$state_abbr)
  hosp <- get_json(u)$results %>%
    transmute(cms_id = facility_id, name = facility_name, city = citytown,
              county_fips = unname(lk[key(countyparish)]), zip = zip_code,
              hospital_type, emergency_services, overall_rating = suppressWarnings(as.numeric(hospital_overall_rating)))
  save_out(hosp, "cms_hospitals_wi")
  hosp_cty <- hosp %>% filter(!is.na(county_fips)) %>% group_by(county_fips) %>%
    summarise(hosp_n_acute    = sum(hospital_type == "Acute Care Hospitals"),
              hosp_n_cah      = sum(hospital_type == "Critical Access Hospitals"),
              hosp_n_with_ed  = sum(emergency_services == "Yes" &
                                    hospital_type %in% c("Acute Care Hospitals","Critical Access Hospitals")),
              .groups = "drop")
  # HRSA primary-care HPSAs (national file ~50 MB)
  log_msg("HRSA HPSA primary care (~50 MB)")
  hp <- read_csv(dl("https://data.hrsa.gov/DataDownload/DD_Files/BCD_HPSA_FCT_DET_PC.csv",
                    file.path(CONFIG$cache_dir, "hpsa_pc.csv")),
                 col_types = cols(.default = "c"), name_repair = "minimal")
  hp <- hp[, nzchar(names(hp))]
  hp <- hp %>% filter(`Primary State Abbreviation` == CONFIG$state_abbr,
                      `HPSA Status` %in% c("Designated", "Proposed For Withdrawal"),
                      `Designation Type` %in% c("Geographic HPSA", "High Needs Geographic HPSA", "HPSA Population")) %>%
    transmute(county_fips = `State and County Federal Information Processing Standard Code`,
              comp_type = `HPSA Component Type Description`, score = as.numeric(`HPSA Score`),
              desig = `Designation Type`)
  hp_cty <- hp %>% group_by(county_fips) %>%
    summarise(hpsa_pc_any = 1L, hpsa_pc_max_score = max(score, na.rm = TRUE),
              hpsa_pc_whole_county = as.integer(any(comp_type == "Single County")), .groups = "drop")
  # Distance from each tract's population-weighted centre (2020) to the
  # nearest general medical/surgical hospital (FRS NAICS 622110).
  log_msg("Distance: tract centre of population -> nearest general hospital")
  cp <- read_csv(dl(sprintf("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR%s.txt",
                            CONFIG$state_fips), file.path(CONFIG$cache_dir, "cenpop2020_tract.csv")),
                 col_types = cols(.default = "c"))
  names(cp) <- sub("^﻿", "", names(cp))
  cp <- cp %>% transmute(tract_geoid = paste0(STATEFP, COUNTYFP, TRACTCE), county_fips = paste0(STATEFP, COUNTYFP),
                         pop2020 = as.numeric(POPULATION), plat = as.numeric(LATITUDE), plon = as.numeric(LONGITUDE))
  hx <- if (!is.null(frs)) frs$facilities %>% filter(startsWith(naics, "622110"), !is.na(lat), !is.na(lon)) %>%
    distinct(registry_id, .keep_all = TRUE) else NULL
  if (!is.null(hx) && nrow(hx)) {
    cp$dist_km_nearest_hospital <- vapply(seq_len(nrow(cp)), function(i)
      min(haversine_km(cp$plat[i], cp$plon[i], hx$lat, hx$lon)), numeric(1))
  } else cp$dist_km_nearest_hospital <- NA_real_
  tr_acc <- cp %>% select(tract_geoid, pop2020, dist_km_nearest_hospital)
  save_out(tr_acc, "access_tract")
  cty <- cp %>% group_by(county_fips) %>%
    summarise(dist_km_nearest_hospital_popwt = weighted.mean(dist_km_nearest_hospital, pop2020, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(hosp_cty, by = "county_fips") %>% left_join(hp_cty, by = "county_fips") %>%
    mutate(across(c(hosp_n_acute, hosp_n_cah, hosp_n_with_ed, hpsa_pc_any, hpsa_pc_whole_county), ~ replace_na(.x, 0L)))
  list(county = save_out(cty, "access_county"), tract = tr_acc)
}

# ---- 1j. Merge static covariates into one county table and one tract table -
merge_static <- function(geo, acs, svi, places, rucc, sdwis, frs, pep = NULL, access = NULL) {
  cty <- geo$county
  if (!is.null(acs))   cty <- left_join(cty, acs$county %>% select(-NAME), by = "county_fips")
  cty <- left_join(cty, svi$county, by = "county_fips") %>%
    left_join(places$county, by = "county_fips") %>%
    left_join(rucc, by = "county_fips")
  if (!is.null(sdwis)) cty <- left_join(cty, sdwis, by = "county_fips")
  if (!is.null(frs))   cty <- left_join(cty, frs$county, by = "county_fips")
  if (!is.null(pep))   cty <- left_join(cty, pep$county, by = "county_fips")
  if (!is.null(access)) cty <- left_join(cty, access$county, by = "county_fips")
  cty <- cty %>% mutate(
    pop_density_km2 = svi_e_totpop / aland_km2,
    pct_pop_on_cws  = if ("pws_pop_served_cws" %in% names(.)) pmin(100, 100 * pws_pop_served_cws / svi_e_totpop) else NA_real_,
    across(starts_with("frs_n_"), ~ replace_na(.x, 0)))
  save_out(cty, "covariates_county")

  tr <- geo$tract
  if (!is.null(acs))   tr <- left_join(tr, acs$tract %>% select(-NAME, -county_fips), by = "tract_geoid")
  tr <- left_join(tr, svi$tract %>% select(-county_fips), by = "tract_geoid")
  if (!is.null(places$tract)) tr <- left_join(tr, places$tract %>% select(-county_fips), by = "tract_geoid")
  tr <- left_join(tr, rucc, by = "county_fips")
  if (!is.null(frs))   tr <- left_join(tr, frs$tract, by = "tract_geoid")
  if (!is.null(access)) tr <- left_join(tr, access$tract, by = "tract_geoid")
  tr <- tr %>% mutate(pop_density_km2 = svi_e_totpop / aland_km2,
                      across(starts_with("frs_n_"), ~ replace_na(.x, 0)))
  save_out(tr, "covariates_tract")
  list(county = cty, tract = tr)
}

# ---- run STEP 1 -------------------------------------------------------------
run_step1 <- function() {
  geo    <- pull_gazetteer()
  acs    <- tryCatch(pull_acs(),    error = function(e) { log_msg("ACS failed: ", conditionMessage(e)); NULL })
  svi    <- pull_svi()
  places <- pull_places()
  rucc   <- pull_rucc()
  sdwis  <- tryCatch(pull_sdwis(geo$county), error = function(e) { log_msg("SDWIS failed: ", conditionMessage(e)); NULL })
  frs    <- if (CONFIG$do_frs) tryCatch(pull_frs(), error = function(e) { log_msg("FRS failed: ", conditionMessage(e)); NULL }) else NULL
  if (CONFIG$do_weather) tryCatch(pull_weather(geo$county), error = function(e) log_msg("Weather failed: ", conditionMessage(e)))
  if (CONFIG$do_storms)  tryCatch(pull_storms(),            error = function(e) log_msg("Storms failed: ", conditionMessage(e)))
  pep    <- if (CONFIG$do_pep) tryCatch(pull_pep(), error = function(e) { log_msg("PEP failed: ", conditionMessage(e)); NULL }) else NULL
  access <- if (CONFIG$do_access) tryCatch(pull_access(geo, frs), error = function(e) { log_msg("Access failed: ", conditionMessage(e)); NULL }) else NULL
  merge_static(geo, acs, svi, places, rucc, sdwis, frs, pep, access)
  writeLines(c(paste("run_date:", Sys.Date()), paste("study_window:", CONFIG$study_start, "to", CONFIG$study_end),
               paste("acs_year:", CONFIG$acs_year), paste("svi_year:", CONFIG$svi_year),
               paste("pep_vintage_requested:", CONFIG$pep_vintage), paste("R:", R.version.string)),
             file.path(CONFIG$out_dir, "_run_metadata.txt"))
  log_msg("STEP 1 complete. Files in ./", CONFIG$out_dir)
}

###############################################################################
#  STEP 2 — LOCAL JOIN TO CONFIDENTIAL LINE LIST  (no network access)
#
#  cases: data.frame, one row per case, with
#    * case_id
#    * a county column: county_fips ("55025", 55025, "025") OR county / county_name
#    * at least one date column, in priority order:
#        onset_date > diagnosis_date > specimen_collection_date > admit_date > report_date
#      The first non-missing one becomes `anchor_date` (the date exposure lags
#      are counted back from); `anchor_source` records which one was used.
#      When the fallback is not onset, it is shifted back by the median
#      onset-to-that-date gap observed in cases that have both (anchor_shift_days).
#    * optional tract_geoid (11-digit, 2020 vintage)
#  Any other columns pass through untouched (strip them with make_analysis_file()).
#
#  Returns one row per case with:
#    * county-level static covariates (prefix c_)
#    * tract-level static covariates if tract_geoid supplied (prefix t_)
#    * weather summarised over lag windows before anchor_date (incubation is
#      2-14 days, so default windows are 0-2, 3-6, 7-10, 11-14, 0-14, 15-28);
#      NA where the window is not covered (never NaN / -Inf)
#    * storm event counts (events overlapping the window); NA outside coverage
#    * calendar: year, month, MMWR week, season
###############################################################################
ANCHOR_DATE_COLS <- c("onset_date", "diagnosis_date", "specimen_collection_date",
                      "admit_date", "report_date")
DEFAULT_LAGS <- list(`0_2` = 0:2, `3_6` = 3:6, `7_10` = 7:10, `11_14` = 11:14,
                     `0_14` = 0:14, `15_28` = 15:28)

read_out <- function(n, out_dir = CONFIG$out_dir) {
  p   <- file.path(out_dir, paste0(n, ".csv"))
  hdr <- names(read_csv(p, n_max = 0, col_types = cols(.default = "c")))
  ids <- intersect(c("county_fips", "tract_geoid"), hdr)          # keep FIPS codes as text
  spec <- do.call(cols, c(setNames(as.list(rep("c", length(ids))), ids), list(.default = col_guess())))
  read_csv(p, col_types = spec, guess_max = 5000)
}

prep_cases <- function(cases, out_dir = CONFIG$out_dir, shift_fallback = TRUE) {
  cases <- as.data.frame(cases, stringsAsFactors = FALSE)
  if (!"case_id" %in% names(cases)) stop("cases needs a case_id column")
  cty_col <- intersect(c("county_fips", "county", "county_name", "county_of_residence"), names(cases))[1]
  if (is.na(cty_col)) stop("cases needs a county column: county_fips (55xxx) or county / county_name")
  geo_c <- read_csv(file.path(out_dir, "geo_county.csv"), col_types = cols(.default = "c"))
  cases$county_raw  <- as.character(cases[[cty_col]])
  cases$county_fips <- standardize_county(cases[[cty_col]], geo_c)
  dcols <- intersect(ANCHOR_DATE_COLS, names(cases))
  if (!length(dcols)) stop("cases needs at least one date column: ", paste(ANCHOR_DATE_COLS, collapse = ", "))
  other_dates <- intersect(c("discharge_date", "death_date", "dob", "date_of_birth", "interview_date"), names(cases))
  for (dc in c(dcols, other_dates)) cases[[dc]] <- parse_date_any(cases[[dc]])
  anchor <- rep(as.Date(NA), nrow(cases)); src <- rep(NA_character_, nrow(cases)); shift <- rep(0, nrow(cases))
  for (dc in dcols) {
    i <- is.na(anchor) & !is.na(cases[[dc]])
    if (!any(i)) next
    gap <- 0
    if (shift_fallback && dc != "onset_date" && "onset_date" %in% dcols) {
      g <- as.numeric(cases[[dc]] - cases$onset_date)
      g <- g[!is.na(g) & g >= 0 & g <= 60]
      if (length(g) >= 10) gap <- stats::median(g)
    }
    anchor[i] <- cases[[dc]][i] - gap; src[i] <- dc; shift[i] <- gap
  }
  cases$anchor_date <- anchor; cases$anchor_source <- src; cases$anchor_shift_days <- shift
  if ("tract_geoid" %in% names(cases)) {
    t <- gsub("\\D", "", as.character(cases$tract_geoid))
    cases$tract_geoid <- ifelse(nzchar(t) & !is.na(cases$tract_geoid), str_pad(t, 11, pad = "0"), NA_character_)
  }
  cases
}

# Run this FIRST on the real extract. Prints only aggregate counts (no PII).
check_case_input <- function(cases, out_dir = CONFIG$out_dir) {
  x <- prep_cases(cases, out_dir)
  cat("\n== check_case_input ==========================================\n")
  cat("rows:", nrow(x), " unique case_id:", n_distinct(x$case_id), "\n")
  if (anyDuplicated(x$case_id)) cat("  !! duplicated case_id:", sum(duplicated(x$case_id)),
                                    "rows (one row per case expected - de-duplicate first)\n")
  bad_c <- unique(x$county_raw[is.na(x$county_fips)])
  cat("county unmatched:", sum(is.na(x$county_fips)), if (length(bad_c)) paste0(" values: ", paste(head(bad_c, 10), collapse = " | ")), "\n")
  if ("tract_geoid" %in% names(x)) {
    tr <- read_out("covariates_tract", out_dir)$tract_geoid
    cat("tract given:", sum(!is.na(x$tract_geoid)), " matched to 2020 tracts:", sum(x$tract_geoid %in% tr),
        " (unmatched usually = 2010-vintage GEOIDs or typos)\n")
  }
  cat("anchor date source:\n"); print(table(x$anchor_source, useNA = "ifany"))
  if (any(x$anchor_shift_days != 0)) cat("  fallback dates shifted back by (days):",
                                         paste(unique(x$anchor_shift_days[x$anchor_shift_days != 0]), collapse = ", "), "\n")
  out_win <- sum(x$anchor_date < CONFIG$study_start | x$anchor_date > CONFIG$study_end, na.rm = TRUE)
  cat("anchor outside study window (", format(CONFIG$study_start), "-", format(CONFIG$study_end), "):", out_win, "\n")
  wxf <- file.path(out_dir, "weather_county_daily.csv")
  if (file.exists(wxf)) {
    rng <- range(read_csv(wxf, col_select = "date", col_types = cols(date = col_date()))$date)
    nc <- sum(x$anchor_date - 28 < rng[1] | x$anchor_date > rng[2], na.rm = TRUE)
    cat("weather file covers", format(rng[1]), "to", format(rng[2]), "-> cases with incomplete 0-28 d windows:", nc,
        if (nc) "  !! re-run STEP 1 with a wider study window" else "", "\n")
  }
  if ("onset_date" %in% names(x) && "admit_date" %in% names(x)) {
    g <- as.numeric(x$admit_date - x$onset_date)
    cat("onset -> admit (days): median", stats::median(g, na.rm = TRUE), " negative:", sum(g < 0, na.rm = TRUE),
        " > 30:", sum(g > 30, na.rm = TRUE), "\n")
  }
  if (all(c("admit_date", "discharge_date") %in% names(x))) {
    los <- as.numeric(x$discharge_date - x$admit_date)
    cat("length of stay: missing", sum(is.na(los)), " negative", sum(los < 0, na.rm = TRUE),
        " median", stats::median(los, na.rm = TRUE), "\n")
  }
  cat("==============================================================\n\n")
  invisible(x)
}

build_case_covariates <- function(cases,
                                  out_dir = CONFIG$out_dir,
                                  lag_windows = DEFAULT_LAGS,
                                  storm_types = c("Flood","Flash Flood","Heavy Rain","Excessive Heat",
                                                  "Heat","Thunderstorm Wind","Tornado","Hail"),
                                  prepared = FALSE) {
  if (!prepared) cases <- prep_cases(cases, out_dir)
  if (anyDuplicated(cases$case_id)) stop("case_id is not unique - de-duplicate first (see check_case_input())")

  # -- static county
  cty <- read_out("covariates_county", out_dir)
  out <- cases %>% left_join(cty %>% rename_with(~ paste0("c_", .x), -county_fips), by = "county_fips")

  # -- static tract
  if ("tract_geoid" %in% names(cases)) {
    tr <- read_out("covariates_tract", out_dir) %>% select(-county_fips)
    out <- out %>% left_join(tr %>% rename_with(~ paste0("t_", .x), -tract_geoid), by = "tract_geoid")
  }

  key <- cases %>% select(case_id, county_fips, anchor_date)

  # -- weather lag windows (NA-safe)
  wxf <- file.path(out_dir, "weather_county_daily.csv")
  if (file.exists(wxf)) {
    wx <- read_csv(wxf, col_types = cols(county_fips = "c", date = col_date(), .default = "d"))
    sum_vars  <- intersect(c("wx_precipitation_sum","wx_rain_sum","wx_precipitation_hours",
                             "wx_shortwave_radiation_sum","wx_et0_fao_evapotranspiration"), names(wx))
    mean_vars <- setdiff(names(wx), c("county_fips","date", sum_vars, "wx_wind_direction_10m_dominant"))
    for (w in names(lag_windows)) {
      grid <- key %>% tidyr::crossing(lag = lag_windows[[w]]) %>% mutate(date = anchor_date - lag) %>%
        left_join(wx, by = c("county_fips","date"))
      agg <- grid %>% group_by(case_id) %>%
        summarise(across(all_of(mean_vars), safe_mean, .names = "{.col}_mean_lag{w}"),
                  across(all_of(sum_vars),  safe_sum,  .names = "{.col}_sum_lag{w}"),
                  !!paste0("wx_temperature_2m_max_max_lag", w) := safe_max(wx_temperature_2m_max),
                  !!paste0("wx_precip_days_gt1mm_lag", w)       := if (all(is.na(wx_precipitation_sum))) NA_real_
                                                                    else sum(wx_precipitation_sum > 1, na.rm = TRUE),
                  !!paste0("wx_ndays_missing_lag", w)           := sum(is.na(wx_temperature_2m_mean)),
                  .groups = "drop")
      out <- left_join(out, agg, by = "case_id")
    }
  } else message("weather_county_daily.csv not found - skipping weather lags")

  # -- storm events overlapping each window; NA if the window is outside coverage
  stf <- file.path(out_dir, "storm_events_wi.csv")
  if (file.exists(stf)) {
    st <- read_csv(stf, col_types = cols(.default = "c")) %>%
      filter(!is.na(county_fips), event_type %in% storm_types) %>%
      mutate(begin_date = as.Date(begin_date), end_date = coalesce(as.Date(end_date), begin_date))
    cvf <- file.path(out_dir, "storm_coverage.csv")
    cov <- if (file.exists(cvf)) read_csv(cvf, col_types = cols(.default = col_date())) else
      tibble(cover_start = as.Date(paste0(format(min(st$begin_date), "%Y"), "-01-01")),
             cover_end   = as.Date(paste0(format(max(st$begin_date), "%Y"), "-12-31")))
    for (w in intersect(c("0_14", "15_28"), names(lag_windows))) {
      lags <- lag_windows[[w]]
      agg <- key %>% mutate(w_start = anchor_date - max(lags), w_end = anchor_date - min(lags)) %>%
        left_join(st, by = "county_fips", relationship = "many-to-many") %>%
        mutate(hit = !is.na(begin_date) & begin_date <= w_end & end_date >= w_start) %>%
        group_by(case_id, w_start, w_end) %>%
        summarise(n_all   = sum(hit),
                  n_flood = sum(hit & event_type %in% c("Flood","Flash Flood","Heavy Rain")),
                  n_heat  = sum(hit & event_type %in% c("Excessive Heat","Heat")), .groups = "drop") %>%
        mutate(ok = !is.na(w_start) & w_start >= cov$cover_start & w_end <= cov$cover_end,
               across(c(n_all, n_flood, n_heat), ~ if_else(ok, as.numeric(.x), NA_real_))) %>%
        transmute(case_id, !!paste0("storm_n_events_lag", w) := n_all,
                  !!paste0("storm_n_flood_lag", w) := n_flood, !!paste0("storm_n_heat_lag", w) := n_heat)
      out <- left_join(out, agg, by = "case_id")
    }
  }

  # -- calendar features (from anchor_date)
  mw <- mmwr_week(out$anchor_date)
  out %>% mutate(anchor_year = as.integer(format(anchor_date, "%Y")),
                 anchor_month = as.integer(format(anchor_date, "%m")),
                 mmwr_year = mw$mmwr_year, mmwr_week = mw$mmwr_week,
                 anchor_season = case_when(is.na(anchor_month) ~ NA_character_,
                                           anchor_month %in% 6:8 ~ "summer", anchor_month %in% 9:11 ~ "fall",
                                           anchor_month %in% c(12, 1, 2) ~ "winter", TRUE ~ "spring"))
}

# County x day exposure table over the whole study window with the same lagged
# weather summaries - for a case-crossover / time-series design. Join daily
# case counts (by county_fips, anchor_date) to it inside DHS.
build_county_day_exposures <- function(out_dir = CONFIG$out_dir,
                                       lag_windows = DEFAULT_LAGS[c("0_2","3_6","7_10","11_14","0_14")]) {
  geo_c <- read_csv(file.path(out_dir, "geo_county.csv"), col_types = cols(.default = "c"))
  days  <- seq(CONFIG$study_start, CONFIG$study_end, by = "day")
  grid  <- tidyr::expand_grid(county_fips = geo_c$county_fips, onset_date = days) %>%
    mutate(case_id = paste(county_fips, onset_date, sep = "_"))
  build_case_covariates(grid, out_dir, lag_windows) %>%
    select(-case_id, -county_raw, -anchor_source, -anchor_shift_days)
}

###############################################################################
#  RATES FOR THE MAP — crude, age-adjusted (direct, US 2000 std) and indirectly
#  standardised (SIR, expected from statewide age-specific rates), plus an
#  empirical-Bayes smoothed SIR for small counties. Output is aggregate only.
#  cases needs: case_id, county, a date column (for year), and age (years).
###############################################################################
county_rates <- function(cases, age_col = "age", out_dir = CONFIG$out_dir, per = 1e5) {
  x <- prep_cases(cases, out_dir)
  if (!age_col %in% names(x)) stop("need an age column: ", age_col)
  x$age  <- suppressWarnings(as.numeric(x[[age_col]]))
  x$year <- as.integer(format(x$anchor_date, "%Y"))
  pep <- read_out("pep_county_year_age", out_dir)
  yrs <- intersect(sort(unique(x$year)), unique(pep$year))
  miss_yr <- setdiff(unique(na.omit(x$year)), unique(pep$year))
  if (length(miss_yr)) message("No PEP population for year(s) ", paste(miss_yr, collapse = ", "),
                               " - using the latest available year as denominator for them")
  x$year[x$year %in% miss_yr] <- max(pep$year)
  dropped <- sum(is.na(x$age) | is.na(x$county_fips) | is.na(x$year))
  if (dropped) message(dropped, " case(s) dropped from rates (missing age, county or date)")
  x <- x %>% filter(!is.na(age), !is.na(county_fips), !is.na(year)) %>%
    mutate(age_group = PEP_AGE_GROUPS[findInterval(age, PEP_AGE_LO)])
  py <- pep %>% filter(year %in% unique(x$year)) %>% group_by(county_fips, age_group) %>%
    summarise(py = sum(pop), .groups = "drop")
  obs <- x %>% count(county_fips, age_group, name = "o")
  ca  <- py %>% left_join(obs, by = c("county_fips", "age_group")) %>% mutate(o = replace_na(o, 0L))
  state_r <- ca %>% group_by(age_group) %>% summarise(r = sum(o) / sum(py), .groups = "drop")
  std <- tibble(age_group = PEP_AGE_GROUPS, w = US2000_STD / sum(US2000_STD))
  res <- ca %>% left_join(state_r, by = "age_group") %>% left_join(std, by = "age_group") %>%
    group_by(county_fips) %>%
    summarise(cases = sum(o), person_years = sum(py), expected = sum(py * r),
              crude_rate = per * cases / person_years,
              adj_rate = per * sum(w * o / py),
              adj_var  = per^2 * sum(w^2 * o / py^2),
              wmax     = max(w / py), .groups = "drop") %>%
    mutate(
      # Fay-Feuer gamma CI for the directly adjusted rate (as used by NCHS/SEER)
      adj_lcl = ifelse(cases == 0, 0, qgamma(0.025, shape = adj_rate^2 / adj_var, scale = adj_var / adj_rate)),
      adj_ucl = qgamma(0.975, shape = (adj_rate + per * wmax)^2 / (adj_var + (per * wmax)^2),
                       scale = (adj_var + (per * wmax)^2) / (adj_rate + per * wmax)),
      sir = cases / expected,
      sir_lcl = ifelse(cases == 0, 0, qchisq(0.025, 2 * cases) / 2 / expected),
      sir_ucl = qchisq(0.975, 2 * (cases + 1)) / 2 / expected)
  # Marshall (1991) global empirical-Bayes shrinkage of SIR toward 1
  m  <- sum(res$cases) / sum(res$expected)
  s2 <- sum(res$expected * (res$sir - m)^2) / sum(res$expected) - m / mean(res$expected)
  s2 <- max(s2, 0)
  res <- res %>% mutate(sir_eb = m + (s2 / (s2 + m / expected)) * (sir - m),
                        small_cell = cases > 0 & cases < CONFIG$small_cell) %>%
    select(-adj_var, -wmax)
  attr(res, "years") <- sort(unique(x$year))
  res
}

###############################################################################
#  DE-IDENTIFIED ANALYSIS FILE
#  Run INSIDE DHS on the output of build_case_covariates(). Produces a file
#  with no direct identifiers and no calendar dates finer than `date_detail`,
#  which is what you would ask DHS to release (confirm their rules first -
#  this is a technical aid, not a data-sharing determination).
#    * drops columns whose names look like direct identifiers (printed so you
#      can check), plus any you list in `drop_cols`
#    * age from DOB (if present), top-coded at 90
#    * every date -> whole days relative to anchor_date (admit_day, discharge_day,
#      death_day ...) + length of stay; then the dates themselves are removed
#    * anchor year (and month if date_detail = "month") kept for season/trend
#    * random study_id replaces case_id; the crosswalk is returned as an
#      attribute and must stay inside DHS
#    * geography: keep_geo = "county" keeps county_fips and c_* covariates;
#      "none" drops county_fips (c_* still attached); tract IDs are always
#      dropped, and t_* covariates are converted to statewide quintiles unless
#      keep_tract_values = TRUE (exact tract values are unique to a tract, so
#      they re-identify the tract just as well as the GEOID would)
#    * prints a small-cell report for categorical variables
###############################################################################
PII_PATTERNS <- c("(^|_)name$", "^name", "first_?name", "last_?name", "middle", "address", "street",
                  "(^|_)addr", "^city$", "phone", "email", "(^|_)mrn", "medical_record", "ssn",
                  "social_sec", "(^|_)dob$", "birth", "^zip", "postal", "(^|_)(lat|latitude)($|_)",
                  "(^|_)(lon|lng|longitude)($|_)", "^x$", "^y$", "geocode", "contact_(name|phone|info)",
                  "guardian", "next_of_kin", "physician", "provider_name", "insurance_id", "medicaid_id",
                  "(^|_)notes?($|_)", "comment", "narrative", "free_?text", "county_raw")
make_analysis_file <- function(x, keep_geo = c("county", "none"), date_detail = c("year", "month"),
                               drop_cols = NULL, keep_cols = NULL, keep_tract_values = FALSE,
                               out_dir = CONFIG$out_dir, seed = NULL) {
  keep_geo <- match.arg(keep_geo); date_detail <- match.arg(date_detail)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  # age
  dob <- intersect(c("dob", "date_of_birth", "birth_date"), names(x))[1]
  if (!is.na(dob) && !"age" %in% names(x))
    x$age <- floor(as.numeric(parse_date_any(x[[dob]]) %--% x$anchor_date))
  if ("age" %in% names(x)) x$age <- pmin(suppressWarnings(as.numeric(x$age)), 90)
  # dates -> days relative to anchor
  for (dc in names(x)[vapply(x, inherits, logical(1), "Date")]) {
    if (dc %in% c("anchor_date", dob)) next
    x[[sub("_date$", "_day", dc)]] <- as.numeric(x[[dc]] - x$anchor_date)
  }
  if (all(c("admit_day", "discharge_day") %in% names(x))) x$los_days <- x$discharge_day - x$admit_day
  x$anchor_year <- as.integer(format(x$anchor_date, "%Y"))
  if (date_detail == "month") x$anchor_month <- as.integer(format(x$anchor_date, "%m")) else
    x <- x[, setdiff(names(x), c("anchor_month", "mmwr_week")), drop = FALSE]
  date_cols <- names(x)[vapply(x, inherits, logical(1), c("Date", "POSIXt"))]
  # identifiers
  low <- tolower(names(x))
  pii <- names(x)[Reduce(`|`, lapply(PII_PATTERNS, grepl, x = low))]
  pii <- setdiff(pii, c(grep("^(c|t)_", names(x), value = TRUE), keep_cols))   # covariates are not PII
  drop <- unique(c(pii, date_cols, drop_cols, "tract_geoid", if (keep_geo == "none") "county_fips"))
  drop <- setdiff(intersect(drop, names(x)), keep_cols)
  message("make_analysis_file(): dropping ", length(drop), " column(s): ", paste(drop, collapse = ", "))
  # tract covariates -> quintiles of the statewide tract distribution
  tcols <- grep("^t_", names(x), value = TRUE)
  if (length(tcols) && !keep_tract_values) {
    ref <- read_out("covariates_tract", out_dir)
    for (tc in tcols) {
      v <- suppressWarnings(as.numeric(x[[tc]])); r <- suppressWarnings(as.numeric(ref[[sub("^t_", "", tc)]]))
      if (all(is.na(v)) || sum(!is.na(r)) < 50 || length(unique(na.omit(r))) < 6) { x[[tc]] <- NULL; next }
      br <- unique(stats::quantile(r, probs = seq(0, 1, 0.2), na.rm = TRUE))
      x[[paste0(tc, "_q5")]] <- findInterval(v, br, rightmost.closed = TRUE, all.inside = TRUE)
      x[[tc]] <- NULL
    }
  }
  # study id
  if (!is.null(seed)) set.seed(seed)
  sid <- sprintf("LD%04d", sample.int(nrow(x)))
  crosswalk <- data.frame(case_id = x$case_id, study_id = sid)
  x$study_id <- sid
  x <- x[, c("study_id", setdiff(names(x), c("study_id", "case_id", drop))), drop = FALSE]
  x <- x[order(x$study_id), , drop = FALSE]
  # small-cell report
  cat_cols <- names(x)[vapply(x, function(v) (is.character(v) || is.factor(v) || is.logical(v)) &&
                                length(unique(v)) <= 20, logical(1))]
  sc <- do.call(rbind, lapply(cat_cols, function(cc) {
    t <- table(x[[cc]], useNA = "no"); t <- t[t > 0 & t < CONFIG$small_cell]
    if (length(t)) data.frame(variable = cc, level = names(t), n = as.integer(t)) else NULL
  }))
  if (!is.null(sc) && nrow(sc)) { message("Small cells (< ", CONFIG$small_cell, "):"); print(sc, row.names = FALSE) }
  attr(x, "crosswalk") <- crosswalk          # KEEP INSIDE DHS
  attr(x, "small_cells") <- sc
  x
}
`%--%` <- function(d1, d2) as.numeric(difftime(d2, d1, units = "days")) / 365.25
###############################################################################
# Running from the command line (Rscript legionella_external_data.R) executes
# STEP 1. From RStudio: source() this file, then call run_step1() yourself,
# then build_case_covariates(cases).
if (!interactive() && !isTRUE(getOption("legionella.no_autorun"))) run_step1()
