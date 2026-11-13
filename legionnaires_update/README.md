# Legionnaires' disease in Wisconsin: external covariates, de-identification and severity analysis (v2, 2026-11-13)

Code for Rachel Erickson's M.S. thesis (UW–Madison Population Health Sciences, with WI DHS).
**The primary aim** is severity among the 635 confirmed cases from 2023–2025. **The secondary aim** is spatial/temporal description.
See `CHANGES_v2.md` for what changed since the 2026-11-03 version and why.

| File | Where it runs | What it does |
|---|---|---|
| `legionella_external_data.R` | STEP 1 anywhere; STEP 2 **inside DHS** | Pulls public covariates; joins them to the line list; QA; de-identified export; county rates |
| `severity_analysis_template.R` | anywhere (synthetic) / on the de-identified file | Table 1, power, death model, competing risks, hospital-free days, multiple imputation |
| `geomask_cases.R` | inside DHS | Randomized point-in-polygon (county/tract) and donut masking for maps |
| `test_synthetic_cases_v2.R` | anywhere | Messy synthetic cases → exercises every STEP 2 function |
| `output/` | — | Public covariate tables from the 2026-11-13 run (no case data) |
| `deck/` | — | Slide deck for Rachel + the script that builds it |

## Workflow

```r
# --- STEP 1: public data (no case data involved). Takes ~1-2 h the first time
#     because the Open-Meteo free tier rate-limits; it waits and resumes on its own.
Sys.setenv(CENSUS_API_KEY = "...")          # optional: adds ACS housing age/plumbing
source("legionella_external_data.R")        # Rscript runs step 1 automatically;
run_step1()                                 # in RStudio call it yourself
# (or just copy the ready-made ./output folder from Drive into the working directory)

# --- STEP 2: inside DHS, on the WEDSS extract (no network needed)
options(legionella.no_autorun = TRUE); source("legionella_external_data.R")
cases <- read.csv("weds_extract.csv")       # case_id, county (FIPS or name), onset_date, ...
check_case_input(cases)                     # aggregate QA printout - safe to share
cov   <- build_case_covariates(cases)       # + c_* county, t_* tract, wx_* lags, storm_*
share <- make_analysis_file(cov, keep_geo = "county", date_detail = "year")
write.csv(share, "LD_analysis_deidentified.csv", row.names = FALSE)
write.csv(attr(share, "crosswalk"), "crosswalk_KEEP_IN_DHS.csv", row.names = FALSE)
rates <- county_rates(cases, age_col = "age")   # map table (aggregate)
```

### Columns STEP 2 understands
* **Required:** `case_id`; county as `county_fips` / `county` / `county_name` (55xxx, xxx or a name); at least one of `onset_date`, `diagnosis_date`, `specimen_collection_date`, `admit_date`, `report_date`.
* **Optional:** `tract_geoid` (11 digits, 2020 tracts), `age` or `dob`, `discharge_date`, `death_date`. Anything else passes through; `make_analysis_file()` removes columns that look like identifiers and prints the list.
* Dates may be `YYYY-MM-DD`, `M/D/YYYY`, `M/D/YY`, SAS `05MAR2024` or Excel serial numbers.

## What gets pulled and why

| Output | Source | Level | Relevance |
|---|---|---|---|
| `weather_county_daily.csv` | Open-Meteo ERA5-Land | county × day, 2022-11-17 → 2025-12-31 | Rain/humidity/heat 0–14 d before onset (secondary aim) |
| `storm_events_wi.csv` | NOAA Storm Events | event | Floods, heavy rain, heat |
| `svi_*` | CDC/ATSDR SVI 2022 | tract, county | SES, race/ethnicity, % 65+, uninsured, housing |
| `places_*` | CDC PLACES 2024 | tract, county | Smoking, COPD, diabetes, CKD prevalence |
| `acs_*` (key) | ACS 5-yr 2023 | tract, county | Housing age, plumbing, large buildings |
| `pep_*` **new** | Census PEP vintage 2025 | county × year × age | Age-adjusted rates, SIR |
| `access_*`, `cms_hospitals_wi.csv` **new** | CMS, HRSA HPSA, FRS + 2020 centres of population | tract, county | Critical-access vs acute hospitals, shortage areas, km to nearest hospital |
| `rucc_county.csv` | USDA ERS RUCC 2023 | county | Rurality |
| `sdwis_*` | EPA SDWIS | system, county | Surface vs ground water, large systems |
| `frs_*` | EPA FRS (NAICS) | point, tract, county | Cooling-tower / building-water proxies |
| `covariates_county.csv`, `covariates_tract.csv` | merged | | Single join tables |

## Notes
* Menominee County has no SDWIS row (EPA tribal primacy).
* ERA5-Land is ~9 km resolution, sampled at the county centroid; tract-level weather would need `pull_weather()` on the tract table (~4 h) or gridded PRISM/gridMET.
* Record `output/_run_metadata.txt` in the thesis methods section (run date and data vintages).
* `make_analysis_file()` is a technical aid. What may leave DHS (date detail, geography, small-cell threshold) is DHS's decision.
