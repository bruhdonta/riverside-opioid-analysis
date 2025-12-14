source("R/04_Normalized_Severity_Scores.R")
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(MASS)   
library(car)
library(broom)


base_dir <- here::here("data_raw", "HPI")


# Expected to already be in memory:
#   make sure to run 04_Normalized_Severity_Scores.R
#   severity_by_zip (ZIP + Normalized_Severity or severity_norm_0_1)
#   ed_df (ZIP + ed_coef), hosp_df (ZIP + hosp_coef), death_df (ZIP + death_coef)

preds <- c("housing","healthcare_access","insured","above_poverty",
           "economic_domain","education_domain","neighborhood_domain",
           "homeownership","ownsevere","social_domain","transportation")

housing             <- read_indicator("housing.csv",                    "housing")
healthcare_access   <- read_indicator("healthcare_access_domain.csv",   "healthcare_access")
insured             <- read_indicator("insured.csv",                    "insured")
above_poverty       <- read_indicator("abovepoverty.csv",               "above_poverty")
economic_domain     <- read_indicator("economic_domain.csv",            "economic_domain")
education_domain    <- read_indicator("education.csv",                  "education_domain")
homeownership       <- read_indicator("homeownership.csv",              "homeownership")
neighborhood_domain <- read_indicator("neighborhood_domain.csv",        "neighborhood_domain")
ownsevere           <- read_indicator("ownsevere.csv",                  "ownsevere")
social_domain       <- read_indicator("social.csv",              "social_domain")
transportation      <- read_indicator("transportation.csv",             "transportation")

hpi <- housing %>%
  left_join(healthcare_access,   by = "ZIP") %>%
  left_join(insured,             by = "ZIP") %>%
  left_join(above_poverty,       by = "ZIP") %>%
  left_join(economic_domain,     by = "ZIP") %>%
  left_join(education_domain,    by = "ZIP") %>%
  left_join(homeownership,       by = "ZIP") %>%
  left_join(neighborhood_domain, by = "ZIP") %>%
  left_join(ownsevere,           by = "ZIP") %>%
  left_join(social_domain,       by = "ZIP") %>%
  left_join(transportation,      by = "ZIP")

severity_by_zip <- severity_by_zip %>%
  dplyr::mutate(ZIP = zipify(ZIP))

# Run models (severity, ED, hosp, death)
sev_models   <- run_ols_block(severity_by_zip %>% dplyr::select(ZIP, Normalized_Severity),
                              "Normalized_Severity", "severity")

cat("\n=== Full model ===\n")
summary(sev_models$model_full)

cat("\n=== Final models (stepAIC) ===\n")
print(summary(sev_models$model_step))

cat("\nCSV outputs written to ./data_interim\n")
