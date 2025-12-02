source("R/04_Chloropleth_Map.R")
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(MASS)   
library(car)
library(broom)


base_dir <- here::here("data_raw", "HPI")   # <- change this folder name if needed


# Expected to already be in memory:
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

fix_zip_char <- function(df) {
  stopifnot("ZIP" %in% names(df))
  df %>% mutate(
    ZIP = as.character(ZIP),
    ZIP = ifelse(nchar(ZIP) < 5, stringr::str_pad(ZIP, 5, pad = "0"), ZIP)
  )
}
ed_df   <- ed_df   |> dplyr::rename(ZIP = `ZIP Code`)
hosp_df <- hosp_df |> dplyr::rename(ZIP = `ZIP Code`)
death_df<- death_df|> dplyr::rename(ZIP = `ZIP Code`)

severity_by_zip <- fix_zip_char(severity_by_zip)
ed_df           <- fix_zip_char(ed_df)     %>% dplyr::select(ZIP, ed_coef)
hosp_df         <- fix_zip_char(hosp_df)   %>% dplyr::select(ZIP, hosp_coef)
death_df        <- fix_zip_char(death_df)  %>% dplyr::select(ZIP, death_coef)



# Run models (severity, ED, hosp, death)
sev_models   <- run_ols_block(severity_by_zip %>% dplyr::select(ZIP, Normalized_Severity),
                              "Normalized_Severity", "severity")
ed_models    <- run_ols_block(ed_df,   "ed_coef",   "ed")
hosp_models  <- run_ols_block(hosp_df, "hosp_coef", "hosp")
death_models <- run_ols_block(death_df,"death_coef","death")

cat("\n=== Final models (stepAIC) ===\n")
print(summary(sev_models$model_step))
print(summary(ed_models$model_step))
print(summary(hosp_models$model_step))
print(summary(death_models$model_step))

cat("\nCSV outputs written to ./data_interim\n")
