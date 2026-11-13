###############################################################################
#  severity_analysis_template.R  —  starter analysis for the PRIMARY aim
#
#  "Are reported exposures (healthcare, senior living, recreational water,
#   travel) associated with more severe Legionnaires' disease among the
#   confirmed Wisconsin cases, 2023-2025?"
#
#  Runs end-to-end on SYNTHETIC data (simulate_cases()) so the code can be
#  checked before it touches WEDSS. To use real data, replace `d` with the
#  output of make_analysis_file(build_case_covariates(cases)) and rename
#  columns to match the CODEBOOK below.
#
#  Addresses the open questions from the first committee meeting:
#    * outcome choice: death (few events) vs length of stay vs a composite
#    * patients who die early "look like" short stays -> competing risks and
#      hospital-free days, not naive LOS
#    * missing discharge dates (~33%) -> describe, then multiple imputation
#      (or backfill) - and show how much the answer moves
#    * lower interview completeness among deaths (61% vs 85%) -> missing
#      exposure is NOT "no exposure"; keep it as its own level / impute
#    * power calculation for 38-42 deaths
#
#  Packages: survival, MASS, splines (ship with R); mice (optional).
###############################################################################
suppressPackageStartupMessages({ library(survival); library(MASS); library(splines) })

# ---- CODEBOOK (rename real columns to these) ---------------------------------
# study_id          character
# age               years (top-coded 90)
# sex               "F"/"M"
# race_eth          "NH White","NH Black","Hispanic","NH Asian","NH AIAN","Other/Multi","Unknown"
# smoking           "Current","Former","Never","Unknown"
# immunocomp        0/1  (transplant, chemo, steroids >=2 wk, biologics, HIV with low CD4)
# chronic_lung, diabetes, ckd, cancer, cvd, liver, alcohol_use   0/1
# exposure          "Healthcare","Senior living","Recreational water","Travel",
#                   "Community/none reported","Not interviewed"   (one primary category)
# interviewed       0/1
# outbreak_assoc    0/1
# dx_method         "Urine antigen only","PCR","Culture","Multiple"
# onset_to_admit    days (care-seeking delay)
# icu, mech_vent    0/1
# admit_day, discharge_day, death_day   days relative to anchor (onset)
# died              0/1  (death attributed to / with LD within 30 d of onset)
# discharge_dispo   "Home","SNF/LTC","Transfer","Died","Unknown"
# anchor_year       2023-2025
# c_svi_rpl_themes, c_rucc_metro, c_dist_km_nearest_hospital_popwt ... area covariates
# ------------------------------------------------------------------------------

simulate_cases <- function(n = 635, seed = 42) {
  set.seed(seed)
  age <- pmin(90, round(rnorm(n, 66, 13)))
  exposure <- sample(c("Healthcare", "Senior living", "Recreational water", "Travel",
                       "Community/none reported", "Not interviewed"), n, TRUE,
                     prob = c(.08, .07, .05, .12, .50, .18))
  immunocomp <- rbinom(n, 1, .18); chronic_lung <- rbinom(n, 1, .25)
  diabetes <- rbinom(n, 1, .30); ckd <- rbinom(n, 1, .15); cancer <- rbinom(n, 1, .12)
  smoking <- sample(c("Current", "Former", "Never", "Unknown"), n, TRUE, c(.35, .25, .30, .10))
  lp <- -7.0 + 0.045 * age + 0.8 * immunocomp + 0.5 * chronic_lung + 0.4 * ckd +
        0.6 * (exposure == "Healthcare") + 0.5 * (exposure == "Senior living") +
        0.7 * (exposure == "Not interviewed")
  died <- rbinom(n, 1, plogis(lp))
  icu  <- rbinom(n, 1, plogis(-1.2 + 1.5 * died + 0.02 * (age - 65) + 0.5 * immunocomp))
  onset_to_admit <- rpois(n, 3)
  admit_day <- onset_to_admit
  los <- ifelse(died == 1, rgeom(n, 0.15) + 1, round(rlnorm(n, log(5) + 0.6 * icu + 0.01 * (age - 65), 0.6)))
  death_day <- ifelse(died == 1, admit_day + los, NA)
  discharge_day <- ifelse(died == 1, NA, admit_day + los)
  # discharge dates missing more often for long stays and 2023-24 (as described)
  anchor_year <- sample(2023:2025, n, TRUE)
  miss <- rbinom(n, 1, plogis(-1.4 + 0.08 * (los - 5) + 0.8 * (anchor_year < 2025))) == 1 & died == 0
  discharge_day[miss] <- NA
  interviewed <- as.integer(exposure != "Not interviewed")
  data.frame(study_id = sprintf("LD%04d", seq_len(n)), age,
             sex = sample(c("F", "M"), n, TRUE, c(.38, .62)),
             race_eth = sample(c("NH White", "NH Black", "Hispanic", "NH Asian", "NH AIAN", "Other/Multi", "Unknown"),
                               n, TRUE, c(.74, .12, .05, .02, .01, .02, .04)),
             smoking, immunocomp, chronic_lung, diabetes, ckd, cancer,
             exposure, interviewed, outbreak_assoc = rbinom(n, 1, .05),
             dx_method = sample(c("Urine antigen only", "PCR", "Culture", "Multiple"), n, TRUE, c(.7, .15, .05, .1)),
             onset_to_admit, icu, mech_vent = rbinom(n, 1, .5) * icu,
             admit_day, discharge_day, death_day, died, anchor_year,
             c_svi_rpl_themes = runif(n), c_rucc_metro = rbinom(n, 1, .75))
}

# ---- 0. data -------------------------------------------------------------------
d <- simulate_cases()
d$exposure <- relevel(factor(d$exposure), ref = "Community/none reported")
d$los <- with(d, ifelse(died == 1, death_day - admit_day, discharge_day - admit_day))
d$los_known <- !is.na(d$los)

# ---- 1. Table 1 by outcome (median [IQR] / n (%), with tests) -----------------
table1 <- function(d, by = "died", vars) {
  g <- factor(d[[by]])
  rows <- lapply(vars, function(v) {
    x <- d[[v]]
    if (is.numeric(x) && length(unique(na.omit(x))) > 2) {
      f <- function(z) sprintf("%.0f [%.0f-%.0f]", median(z, na.rm = TRUE), quantile(z, .25, na.rm = TRUE), quantile(z, .75, na.rm = TRUE))
      p <- tryCatch(wilcox.test(x ~ g)$p.value, error = function(e) NA)
      data.frame(variable = v, level = "median [IQR]", all = f(x), t(tapply(x, g, f)), p = signif(p, 2),
                 n_missing = sum(is.na(x)), check.names = FALSE)
    } else {
      tb <- table(x, g, useNA = "no")
      p <- tryCatch(if (any(suppressWarnings(chisq.test(tb))$expected < 5)) fisher.test(tb, simulate.p.value = TRUE)$p.value
                    else chisq.test(tb)$p.value, error = function(e) NA)
      cell <- function(k, tot) sprintf("%d (%.0f%%)", k, 100 * k / tot)
      out <- data.frame(variable = v, level = rownames(tb), all = cell(rowSums(tb), sum(tb)),
                        sapply(colnames(tb), function(cn) cell(tb[, cn], sum(tb[, cn]))),
                        p = c(signif(p, 2), rep(NA, nrow(tb) - 1)), n_missing = c(sum(is.na(x)), rep(NA, nrow(tb) - 1)),
                        check.names = FALSE)
      out
    }
  })
  do.call(rbind, rows)
}
t1 <- table1(d, "died", c("age", "sex", "race_eth", "smoking", "immunocomp", "chronic_lung", "diabetes",
                          "ckd", "cancer", "exposure", "dx_method", "onset_to_admit", "icu", "los"))
print(t1, row.names = FALSE)

# ---- 2. Power / events-per-variable -------------------------------------------
# With ~38-42 deaths, ~10 events per parameter allows ~4 parameters: age
# (1-2 df), the exposure of interest, and 1-2 confounders. Firth-penalised
# logistic regression (logistf::logistf) is the usual fix for sparse events.
power_logistic <- function(n = 635, p_death = 0.06, p_exp = 0.10, or = 2, nsim = 1000, alpha = 0.05, seed = 1) {
  set.seed(seed)
  b1 <- log(or)
  b0 <- uniroot(function(b) mean(plogis(b + b1 * rbinom(2e5, 1, p_exp))) - p_death, c(-10, 0))$root
  mean(replicate(nsim, {
    x <- rbinom(n, 1, p_exp); y <- rbinom(n, 1, plogis(b0 + b1 * x))
    if (sum(y) < 2 || length(unique(x[y == 1])) < 2) return(FALSE)
    summary(glm(y ~ x, family = binomial))$coefficients[2, 4] < alpha
  }))
}
pw <- expand.grid(p_exp = c(0.05, 0.10, 0.20), or = c(1.5, 2, 3))
pw$power <- mapply(function(p, o) power_logistic(p_exp = p, or = o, nsim = 500), pw$p_exp, pw$or)
cat("\nPower (unadjusted, n = 635, 6% deaths):\n"); print(reshape(pw, idvar = "p_exp", timevar = "or", direction = "wide"))

# ---- 3. Outcome A: death (logistic; Firth if available) ------------------------
fit_death <- if (requireNamespace("logistf", quietly = TRUE)) {
  logistf::logistf(died ~ exposure + ns(age, 3) + immunocomp + chronic_lung, data = d)
} else glm(died ~ exposure + ns(age, 3) + immunocomp + chronic_lung, family = binomial, data = d)
cat("\nDeath model (OR; exposure terms):\n")
print(round(exp(cbind(OR = coef(fit_death), confint.default(fit_death)))[grep("^exposure", names(coef(fit_death))), ], 2))

# ---- 4. Outcome B: competing risks — discharge alive vs in-hospital death ---
# Early deaths are short stays; naive LOS calls them "mild". Treat time to
# discharge-alive with death as a competing event.
d$cr_time   <- d$los
d$cr_status <- factor(ifelse(is.na(d$los), NA, ifelse(d$died == 1, "death", "discharged")),
                      levels = c("censor", "discharged", "death"))
ci <- survfit(Surv(cr_time, cr_status) ~ 1, data = d[!is.na(d$cr_time), ])
cat("\nCumulative incidence at day 7/14/30:\n")
ps <- summary(ci, times = c(7, 14, 30))$pstate
dimnames(ps) <- list(paste0("day ", c(7, 14, 30)), c("still in hospital", "discharged alive", "died"))
print(round(ps, 3))
fg <- finegray(Surv(cr_time, cr_status) ~ ., data = d[!is.na(d$cr_time), c("cr_time", "cr_status", "exposure", "age", "immunocomp")],
               etype = "discharged")
fit_fg <- coxph(Surv(fgstart, fgstop, fgstatus) ~ exposure + ns(age, 3) + immunocomp, weights = fgwt, data = fg)
cat("\nFine-Gray subdistribution HR for DISCHARGE ALIVE (HR < 1 = slower recovery / worse):\n")
print(round(exp(cbind(HR = coef(fit_fg), confint(fit_fg)))[1:5, ], 2))

# ---- 5. Outcome C: hospital-free days to day 30 (deaths = worst) --------------
# One ordinal outcome that uses deaths AND length of stay: 30 - days in
# hospital, with death within 30 d scored -1 (worse than any survivor).
d$hfd30 <- with(d, ifelse(died == 1 & !is.na(death_day) & death_day <= 30, -1,
                          ifelse(is.na(discharge_day), NA, pmax(0, 30 - pmax(0, pmin(discharge_day, 30) - admit_day)))))
d$hfd30_cat <- cut(d$hfd30, c(-Inf, -0.5, 10, 20, 24, Inf), labels = c("Died", "0-10", "11-20", "21-24", "25-30"), ordered_result = TRUE)
fit_po <- polr(hfd30_cat ~ exposure + ns(age, 3) + immunocomp, data = d, Hess = TRUE)
cat("\nProportional-odds model for hospital-free days (OR > 1 = more hospital-free days = better):\n")
print(round(exp(cbind(OR = coef(fit_po), confint.default(fit_po)))[1:5, ], 2))

# ---- 6. Missing discharge dates: how bad, and does it matter? ----------------
cat("\nMissing LOS by year / ICU / death:\n")
print(round(prop.table(table(year = d$anchor_year, los_missing = is.na(d$los)), 1), 2))
print(round(prop.table(table(icu = d$icu, los_missing = is.na(d$los)), 1), 2))
if (requireNamespace("mice", quietly = TRUE)) {
  imp_dat <- d[, c("hfd30", "exposure", "age", "immunocomp", "icu", "mech_vent", "died", "anchor_year", "chronic_lung", "ckd")]
  imp <- mice::mice(imp_dat, m = 20, method = "pmm", printFlag = FALSE, seed = 7)
  fit_mi <- with(imp, lm(hfd30 ~ exposure + ns(age, 3) + immunocomp))
  cat("\nHospital-free days, linear model after multiple imputation (m = 20):\n")
  print(summary(mice::pool(fit_mi))[2:6, c("term", "estimate", "std.error", "p.value")])
}

# ---- 7. Interview completeness as a source of bias --------------------------
cat("\nInterviewed by outcome:\n"); print(round(prop.table(table(died = d$died, interviewed = d$interviewed), 1), 2))
# Keep "Not interviewed" as its own exposure level (done above). Sensitivity
# analyses: (a) restrict to interviewed cases; (b) multiple imputation of
# exposure; (c) quantitative bias analysis assuming exposure prevalence in
# non-interviewed deaths equals that in interviewed deaths.
