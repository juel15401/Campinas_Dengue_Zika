# legionella_external_data.R — v2 (2026-11-13)

Changes since v1 (2026-11-03, the version in `Rachel Erickson/Legionnaires/legionella_external_data/`).

## Bugs fixed (these would have bitten us)

| # | v1 behaviour | Consequence | v2 |
|---|---|---|---|
| 1 | `study_start = 2024-01-01` | The cohort is the 635 confirmed cases from **2023–2025**. All 2023 cases (~1/3) got NO weather and incomplete storm lags. | `study_start = 2023-01-01`; weather starts 2022-11-17 (45-day lag buffer). |
| 2 | Weather cache files named `wx_<year>_chunkNN.json` | v1's 2023 cache file only covers 2023-11-17 → 12-31. Re-running with a wider window would **silently reuse it**, so Jan–Nov 2023 would still be missing. | Cache file named by exact start/end date + first/last county FIPS. A warning fires if the final weather file doesn't cover the requested window. |
| 3 | `mean(..., na.rm=TRUE)`, `max(...)`, `sum(...)` over windows with no data | Gave `NaN`, `-Inf` (with a warning) and **0**. A 0 for rainfall or storms looks like real data ("dry, no storms"). | `safe_mean/safe_sum/safe_max` return `NA`. Storm counts are `NA` when the window falls outside the downloaded storm years (`storm_coverage.csv`). |
| 4 | `as.Date(onset_date)` only | WEDSS/Excel exports such as `3/5/2024`, Excel serials or SAS `05MAR2024` fail or go NA. | `parse_date_any()` handles ISO, M/D/YYYY, M/D/YY, SAS DATE9 and Excel serials, and warns on anything it can't parse. |
| 5 | `onset_date` required | Cases without onset would drop out of the lag analysis. | Fallback order: onset → diagnosis → specimen collection → admit → report. The fallback date is shifted back by the median onset-to-that-date gap among cases with both dates. Recorded in `anchor_date`, `anchor_source`, `anchor_shift_days` for sensitivity analysis. |
| 6 | `county_fips` required as 5-digit text | A 3-digit code (`"025"`) became `"00025"` (wrong county). County names failed. | `standardize_county()` accepts 55xxx, xxx, numbers and names ("St. Croix", "FOND DU LAC COUNTY"). |
| 7 | No input checks | Duplicate case rows, 2010-vintage tract IDs or typos joined silently. | `check_case_input(cases)` prints aggregate-only QA: duplicates, unmatched counties/tracts, anchor-date sources, dates outside the study or weather window, onset→admit gaps, LOS sanity. `build_case_covariates()` stops on duplicate IDs. |
| 8 | Week = `%U` (Sunday-start, week 0 exists) | Doesn't match the MMWR weeks DHS/CDC report. | `mmwr_week()` gives `mmwr_year`, `mmwr_week`. |
| 9 | Output passed every column through | Names, addresses and DOB would ride along into any file written from `cov`. | `make_analysis_file()` (below). |
| 10 | Storm events counted by begin date only | Multi-day floods/heat waves that started before the window were missed. | Counts events whose [begin, end] **overlaps** the window. |

## New functions

* **`check_case_input(cases)`** — run first on the real extract. Prints only counts, so the printout can be shared.
* **`make_analysis_file(cov, keep_geo = "county", date_detail = "year")`** — de-identified analysis file:
  * drops columns that look like direct identifiers (names, address, phone, MRN, DOB, ZIP, lat/lon, free text) and prints what it dropped so you can check;
  * age from DOB, top-coded at 90;
  * every date becomes days relative to the anchor (onset) date (`admit_day`, `discharge_day`, `death_day`, …) plus `los_days`; calendar dates are removed and only `anchor_year` (and optionally month) is kept;
  * random `study_id`; the case_id↔study_id crosswalk is returned as `attr(x, "crosswalk")` and **must stay in DHS**;
  * tract IDs are always dropped. Tract covariates become statewide quintiles (`t_*_q5`), because an exact tract value is unique to a tract and so identifies it;
  * small-cell report for categorical variables (threshold `CONFIG$small_cell`, default 5; confirm with DHS).
* **`county_rates(cases, age_col = "age")`** — the population-adjusted map table (aggregate only): cases, person-years, crude rate, **age-adjusted rate** (US 2000 standard, Fay–Feuer gamma CI), **SIR** (observed/expected from statewide age-specific rates, exact Poisson CI), **empirical-Bayes smoothed SIR** (Marshall 1991), and a small-cell flag.

## New public layers (STEP 1)

| Output | Source | Use |
|---|---|---|
| `pep_county_year_age.csv`, `pep_county.csv` | Census Population Estimates, vintage 2025 (falls back to 2024), county × year × 18 age groups, 2020–2025, no key needed | Denominators for age-adjusted rates and SIR; `% 65+`, `% 75+` |
| `cms_hospitals_wi.csv` | CMS Hospital General Information (143 WI hospitals) | Counts of acute-care vs **critical-access** hospitals and EDs by county |
| HPSA in `access_county.csv` | HRSA primary-care Health Professional Shortage Areas | `hpsa_pc_any`, `hpsa_pc_max_score`, `hpsa_pc_whole_county`: access to care → diagnostic delay → severity |
| `access_tract.csv`, `access_county.csv` | 2020 tract centres of population + EPA FRS general hospitals (NAICS 622110) | `dist_km_nearest_hospital` (tract), population-weighted mean (county) |
| `storm_coverage.csv` | — | Years the storm files cover (for NA vs 0) |
| `_run_metadata.txt` | — | Run date and data vintages, for the methods section |

## Still not included (worth considering)

* **Area Deprivation Index** (UW Neighborhood Atlas, block group). It is a UW–Madison product and widely used in health-disparities work, but downloading it needs a free login and data-use agreement, so it can't be scripted. Add it by hand if the committee wants an SES index besides SVI.
* **Tract-level rurality (RUCA 2020).** Only county RUCC is included now.
* **Daily PM2.5 / wildfire smoke** (EPA AQS). June 2023 Canadian smoke over Wisconsin could matter for respiratory severity; this is only relevant if a temporal analysis is done.
* **Hospital capacity / respiratory-virus season.** HHS hospital capacity reporting ended in 2024; CDC FluView/NREVSS give state-level weekly flu, RSV and COVID activity if hospital strain is a concern.
* **Cooling-tower registry.** Wisconsin has none; FRS counts remain proxies.
