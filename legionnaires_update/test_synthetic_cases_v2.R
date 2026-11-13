# Test STEP 2 of legionella_external_data.R (v2) on synthetic, deliberately
# messy cases: 2023 dates, county names, M/D/YYYY + Excel serial dates,
# missing onset dates, a duplicate ID, a bad county, DOB + identifiers.
options(legionella.no_autorun = TRUE)
source("legionella_external_data.R")
set.seed(11)
n <- 60
geo <- read_csv("output/geo_county.csv", col_types = cols(.default = "c"))
trt <- read_csv("output/covariates_tract.csv", col_types = cols(.default = "c"))$tract_geoid
onset <- sample(seq(as.Date("2023-01-01"), as.Date("2025-12-20"), by = "day"), n, TRUE)
tg <- sample(trt, n, TRUE)
cases <- data.frame(
  case_id = sprintf("FAKE%03d", 1:n),
  first_name = "Jane", last_name = "Doe", street_address = "1 Main St", phone = "555-0100",
  dob = format(onset - round(runif(n, 30, 95) * 365.25), "%m/%d/%Y"),
  county = ifelse(seq_len(n) %% 3 == 0, sub(" County$", "", geo$county_name[match(substr(tg, 1, 5), geo$county_fips)]),
                  substr(tg, 3, 5)),                                   # names and 3-digit codes mixed
  tract_geoid = tg,
  onset_date = format(onset, "%m/%d/%Y"),
  specimen_collection_date = as.numeric(onset + 4 - as.Date("1899-12-30")),   # Excel serial
  admit_date = as.character(onset + 3),
  discharge_date = as.character(onset + 3 + rpois(n, 6)),
  death_date = NA_character_,
  sex = sample(c("F", "M"), n, TRUE), exposure = sample(c("Healthcare", "Travel", "None"), n, TRUE),
  stringsAsFactors = FALSE)
cases$onset_date[1:6] <- NA                                            # -> fallback to specimen date
cases$county[7] <- "Not A County"
cases$death_date[8] <- as.character(onset[8] + 5); cases$discharge_date[8] <- NA
cases$age <- floor(as.numeric(onset - parse_date_any(cases$dob)) / 365.25)

chk <- check_case_input(rbind(cases, cases[2, ]))                      # duplicate shows up in the report
cov <- build_case_covariates(cases)
cat("rows:", nrow(cov), " cols:", ncol(cov), "\n")
stopifnot(nrow(cov) == n)
cat("2023 cases with full 0-14 d weather:",
    sum(cov$anchor_year == 2023 & cov$wx_ndays_missing_lag0_14 == 0, na.rm = TRUE), "of", sum(cov$anchor_year == 2023, na.rm = TRUE), "\n")
num <- cov[vapply(cov, is.numeric, logical(1))]
cat("NaN cells:", sum(vapply(num, function(v) sum(is.nan(v)), 1)), " Inf cells:", sum(vapply(num, function(v) sum(is.infinite(v)), 1)), "\n")
cat("anchor sources:\n"); print(table(cov$anchor_source))
cat("bad-county case has county covariates?", !is.na(cov$c_svi_rpl_themes[7]), "\n")
print(head(cov[, c("case_id", "county_fips", "anchor_date", "anchor_source", "c_pep_pct_65plus",
                   "c_hosp_n_cah", "c_hpsa_pc_any", "t_dist_km_nearest_hospital",
                   "wx_precipitation_sum_sum_lag0_14", "storm_n_flood_lag0_14", "mmwr_week")], 8))

share <- make_analysis_file(cov, seed = 1)
cat("shared file:", nrow(share), "rows x", ncol(share), "cols\n")
left <- intersect(names(share), c("first_name", "last_name", "street_address", "phone", "dob", "tract_geoid",
                                  "case_id", "onset_date", "admit_date", "anchor_date"))
cat("identifier/date columns remaining:", if (length(left)) left else "none", "\n")
stopifnot(!length(left))
print(head(share[, c("study_id", "age", "sex", "county_fips", "anchor_year", "admit_day", "discharge_day",
                     "death_day", "los_days", grep("_q5$", names(share), value = TRUE)[1:2])], 5))

rt <- county_rates(cases, age_col = "age")
cat("county_rates rows:", nrow(rt), " sum cases:", sum(rt$cases), " sum expected:", round(sum(rt$expected), 2), "\n")
print(head(rt[order(-rt$cases), ], 5))

ex <- build_county_day_exposures()
cat("county-day grid:", nrow(ex), "rows; NA share in 0-14 d precip:", round(mean(is.na(ex$wx_precipitation_sum_sum_lag0_14)), 4), "\n")
write_csv(cov[, c("case_id", grep("^(c_|wx_|storm_)", names(cov), value = TRUE))][1:25, ], "output/_synthetic_case_covariates_example.csv")
cat("ALL TESTS PASSED\n")
