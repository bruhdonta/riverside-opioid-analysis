source("R/04_Normalized_Severity_Scores.R")
library(dplyr)
library(tidyr)
library(stringr)
library(MASS)
library(car)
library(broom)

base_dir <- here::here("data_raw", "HPI")

# ---- Build HPI master table ----
housing_domain      <- read_indicator("housing.csv",                    "housing_domain")
healthcare_access   <- read_indicator("healthcare_access_domain.csv",   "healthcare_access")
insured             <- read_indicator("insured.csv",                    "insured")
above_poverty       <- read_indicator("abovepoverty.csv",               "above_poverty")
economic_domain     <- read_indicator("economic_domain.csv",            "economic_domain")
education_domain    <- read_indicator("education.csv",                  "education_domain")
homeownership       <- read_indicator("homeownership.csv",              "homeownership")
neighborhood_domain <- read_indicator("neighborhood_domain.csv",        "neighborhood_domain")
ownsevere           <- read_indicator("ownsevere.csv",                  "ownsevere")
social_domain       <- read_indicator("social.csv",                     "social_domain")
transportation      <- read_indicator("transportation.csv",             "transportation")
bachelorsed         <- read_indicator("bachelorsed.csv",                "bachelorsed")
percapita_income    <- read_indicator("percapitaincome.csv",            "percapita_income")
employed            <- read_indicator("employed.csv",                   "employed")

hpi <- housing_domain %>%
  left_join(healthcare_access,   by = "ZIP") %>%
  left_join(insured,             by = "ZIP") %>%
  left_join(above_poverty,       by = "ZIP") %>%
  left_join(economic_domain,     by = "ZIP") %>%
  left_join(education_domain,    by = "ZIP") %>%
  left_join(homeownership,       by = "ZIP") %>%
  left_join(neighborhood_domain, by = "ZIP") %>%
  left_join(ownsevere,           by = "ZIP") %>%
  left_join(social_domain,       by = "ZIP") %>%
  left_join(transportation,      by = "ZIP") %>%
  left_join(bachelorsed,         by = "ZIP") %>%
  left_join(percapita_income,    by = "ZIP") %>%
  left_join(employed,            by = "ZIP")

# Outcome table (ZIP + normalized severity)
severity_outcome <- severity_by_zip %>%
  dplyr::mutate(ZIP = zipify(ZIP)) %>%
  dplyr::select(ZIP, Normalized_Severity)

# ==========================================
# (A) DOMAINS-ONLY model
# ==========================================
preds <- c(
  "housing_domain",
  "healthcare_access",
  "economic_domain",
  "education_domain",
  "neighborhood_domain",
  "social_domain",
  "transportation"
)

cat("\n=========================\n")
cat("OLS: Domains-only model\n")
cat("=========================\n")

sev_domains_ols <- run_ols_block(
  outcome_df  = severity_outcome,
  outcome_col = "Normalized_Severity",
  out_prefix  = "severity_domains_only"
)

cat("\n=== Domains-only: Full model ===\n")
print(summary(sev_domains_ols$model_full))

cat("\n=== Domains-only: StepAIC final model ===\n")
print(summary(sev_domains_ols$model_step))

cat("\n=== Domains-only: VIF (step model) ===\n")
print(sev_domains_ols$vif_tab)

# ==========================================
# (B) INDICATORS-ONLY model
# ==========================================
preds <- c(
  "insured",
  "above_poverty",
  "homeownership",
  "ownsevere",
  "bachelorsed",
  "percapita_income",
  "employed"
)

cat("\n=========================\n")
cat("OLS: Indicators-only model\n")
cat("=========================\n")

sev_indicators_ols <- run_ols_block(
  outcome_df  = severity_outcome,
  outcome_col = "Normalized_Severity",
  out_prefix  = "severity_indicators_only"
)

cat("\n=== Indicators-only: Full model ===\n")
print(summary(sev_indicators_ols$model_full))

cat("\n=== Indicators-only: StepAIC final model ===\n")
print(summary(sev_indicators_ols$model_step))

cat("\n=== Indicators-only: VIF (step model) ===\n")
print(sev_indicators_ols$vif_tab)

cat("\nDone. CSV outputs written to ./data_interim with prefixes:\n")
cat("  - severity_domains_only_*\n")
cat("  - severity_indicators_only_*\n")